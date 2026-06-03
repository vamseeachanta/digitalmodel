#!/usr/bin/env python3
# ABOUTME: Sweep Config loader for demo_05 — validates + resolves inputs/demo_05_jumper.yml.
# ABOUTME: ADR-0005: the yaml is the COMPLETE config source of truth; this loader resolves it.
"""demo_05 Sweep Config loader (sibling of sweep_config_demo03.py, NOT an import of it).

Loads + validates a Sweep Config yaml (the analyst-facing case matrix + ALL config) against
an inline jsonschema, mirroring the validation idiom of ``sweep_config_demo03.load_demo03_config``
(parse YAML -> jsonschema validate -> resolve). Resolves the validated yaml into a
``ResolvedDemo05Config`` exposing the axis lists typed as the engine consumes them:

  - ``vessels`` items are the vessel ``name`` labels (e.g. "Large CSV"),
  - ``lengths_m`` items are ``float`` (match the catalog ``length_m``),
  - ``depths`` items stay ``int`` (never coerced to float; ``water_depth_m`` is int),
  - ``hs`` items are ``float``.

In addition to the demo_03 constant set, this resolver carries the two NEW span MODELS that fix
the Phase-2 (in-air lift bending) and Phase-5 (tie-in deflection) physics defects — the
spreader-bar lift span and the inter-bend tie-in free span, both derived from the jumper's M/W
geometry (``horizontal_span_m`` / ``number_of_bends``), with reviewable boundary-condition
coefficients and the connector-mating tolerance.

Circular-import avoidance: the demo module is heavy (numpy/pandas/plotly). It is imported
LAZILY only where needed; this loader does NOT need the engineering stack to resolve, so the
full config + the paths-only resolver are both usable without importing the demo.
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List

try:
    import jsonschema
except ImportError as exc:  # pragma: no cover — real dep required.
    raise RuntimeError(
        "sweep_config_demo05 requires jsonschema. Install with "
        "`uv pip install jsonschema>=4.26`."
    ) from exc

try:
    import yaml
except ImportError as exc:  # pragma: no cover
    raise RuntimeError(
        "sweep_config_demo05 requires PyYAML. Install with `uv pip install pyyaml`."
    ) from exc


class SweepConfigError(ValueError):
    """Raised when a Sweep Config yaml fails schema or resolution checks."""


# ---------------------------------------------------------------------------
# jsonschema — describes the demo_05 Sweep Config surface (ADR-0005: ALL config)
# ---------------------------------------------------------------------------

_SWEEP_SCHEMA = {
    "type": "object",
    "required": ["vessels", "lengths_m", "depths", "hs"],
    "additionalProperties": False,
    "properties": {
        "vessels": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "string"},
        },
        "lengths_m": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "number", "exclusiveMinimum": 0},  # lengths are floats
        },
        "depths": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "integer"},  # depths are integers
        },
        "hs": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "number"},  # Hs are floats
        },
    },
}

_CONSTANTS_SCHEMA = {
    "type": "object",
    "required": [
        "daf_liftoff",
        "daf_splash",
        "rigging_mass_kg",
        "cs_slamming",
        "cd_cylinder",
        "ca_cylinder",
        "v_lowering",
        "v_current",
        "splash_submerged_length_m",
        "wire_allowable_factor",
        "bending_allowable",
        "cable_unit_weight_sub_n_per_m",
        "tie_in_tolerance_mm",
        "reference_hs",
        "tp_coefficient",
        "go_marginal_threshold",
        "nogo_utilisation",
    ],
    "additionalProperties": False,
    "properties": {
        "daf_liftoff": {"type": "number"},
        "daf_splash": {"type": "number"},
        "rigging_mass_kg": {"type": "number"},
        "cs_slamming": {"type": "number"},
        "cd_cylinder": {"type": "number"},
        "ca_cylinder": {"type": "number"},
        # Velocities feed v_rel^2 / drag^2 terms; a non-positive value is a config error.
        "v_lowering": {"type": "number", "exclusiveMinimum": 0},
        "v_current": {"type": "number", "exclusiveMinimum": 0},
        "splash_submerged_length_m": {"type": "number"},
        "wire_allowable_factor": {"type": "number"},
        "bending_allowable": {"type": "number"},
        "cable_unit_weight_sub_n_per_m": {"type": "number"},
        # The tie-in connector tolerance is the divisor of the Phase-5 utilisation; a zero or
        # negative tolerance is a config error (would div-by-zero / invert the check).
        "tie_in_tolerance_mm": {"type": "number", "exclusiveMinimum": 0},
        "reference_hs": {"type": "number"},
        "tp_coefficient": {"type": "number"},
        "go_marginal_threshold": {"type": "number"},
        "nogo_utilisation": {"type": "number"},
    },
}

_PHYSICAL_CONSTANTS_SCHEMA = {
    "type": "object",
    "required": [
        "seawater_density_kg_m3",
        "gravity_m_s2",
    ],
    "additionalProperties": False,
    "properties": {
        "seawater_density_kg_m3": {"type": "number"},
        "gravity_m_s2": {"type": "number"},
    },
}

# Phase-2 in-air lift bending span model (the fix). The lift span is an EXPLICIT length-scaling
# fraction of horizontal_span_m (lift_span = lift_span_fraction * horizontal_span_m), with an
# absolute override (lift_span_m, wins when > 0). M = w_eff * lift_span^2 / moment_coeff,
# w_eff = air_n_per_m * daf_liftoff.
_LIFT_SPAN_SCHEMA = {
    "type": "object",
    "required": ["lift_span_fraction", "moment_coeff"],
    "additionalProperties": False,
    "properties": {
        # Fraction of horizontal_span_m (0 < f <= 1); the unsupported bay cannot exceed the run.
        "lift_span_fraction": {"type": "number", "exclusiveMinimum": 0, "maximum": 1.0},
        # Absolute override (m). null => use the fraction; when a number, must be > 0.
        "lift_span_m": {"type": ["number", "null"], "exclusiveMinimum": 0},
        # moment_coeff (8 ss / 12 ff) is the divisor of the bending moment — zero/negative is a
        # config error.
        "moment_coeff": {"type": "number", "exclusiveMinimum": 0},
    },
}

# Phase-5 tie-in free-span deflection model (the fix). The tie-in free span is an EXPLICIT
# length-scaling fraction of horizontal_span_m (tiein_span = tiein_unsupported_span_fraction *
# horizontal_span_m), with an absolute override (tiein_unsupported_span_m, wins when > 0).
# delta = w_res * tiein_span^4 / (deflection_coeff * E * I).
_TIEIN_SPAN_SCHEMA = {
    "type": "object",
    "required": [
        "tiein_unsupported_span_fraction",
        "include_self_weight",
        "deflection_coeff",
    ],
    "additionalProperties": False,
    "properties": {
        # Fraction of horizontal_span_m (0 < f <= 1).
        "tiein_unsupported_span_fraction": {
            "type": "number", "exclusiveMinimum": 0, "maximum": 1.0,
        },
        # Absolute override (m). null => use the fraction; when a number, must be > 0.
        "tiein_unsupported_span_m": {"type": ["number", "null"], "exclusiveMinimum": 0},
        "include_self_weight": {"type": "boolean"},
        # deflection_coeff (76.8 ss / 384 ff) divides E*I*span^4; zero/negative is a config error.
        "deflection_coeff": {"type": "number", "exclusiveMinimum": 0},
    },
}

_CATALOGS_SCHEMA = {
    "type": "object",
    "required": ["vessels", "jumpers"],
    "additionalProperties": False,
    "properties": {
        "vessels": {"type": "string"},
        "jumpers": {"type": "string"},
    },
}

_ARTIFACTS_SCHEMA = {
    "type": "object",
    "required": ["results_root", "output_root"],
    "additionalProperties": False,
    "properties": {
        "results_root": {"type": "string"},
        "output_root": {"type": "string"},
    },
}

DEMO05_CONFIG_SCHEMA = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "additionalProperties": False,
    "required": [
        "meta",
        "sweep",
        "constants",
        "physical_constants",
        "lift_span_model",
        "tiein_span_model",
        "catalogs",
        "artifacts",
    ],
    "properties": {
        "meta": {
            "type": "object",
            "additionalProperties": False,
            "required": ["demo_id"],
            "properties": {
                "demo_id": {"type": "string"},
                "code_ref": {"type": "string"},
            },
        },
        "sweep": _SWEEP_SCHEMA,
        "constants": _CONSTANTS_SCHEMA,
        "physical_constants": _PHYSICAL_CONSTANTS_SCHEMA,
        "lift_span_model": _LIFT_SPAN_SCHEMA,
        "tiein_span_model": _TIEIN_SPAN_SCHEMA,
        "catalogs": _CATALOGS_SCHEMA,
        "artifacts": _ARTIFACTS_SCHEMA,
    },
}


# ---------------------------------------------------------------------------
# Resolved config dataclasses
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class ResolvedDemo05Config:
    """The resolved demo_05 axes + constants + span models, typed as the engine consumes them.

    Axis ordering for the cross-product is fixed: vessel (outer) -> jumper length -> depth ->
    hs (inner), matching the frozen 300-case golden.

    ``depths`` is ``List[int]`` (kept int, never coerced); ``lengths_m`` and ``hs`` are
    ``List[float]``.
    """

    demo_id: str
    code_ref: str
    # Sweep axes.
    vessels: List[str]
    lengths_m: List[float]
    depths: List[int]                # integers, preserved
    hs: List[float]                  # floats
    # Engineering constants.
    daf_liftoff: float
    daf_splash: float
    rigging_mass_kg: float
    cs_slamming: float
    cd_cylinder: float
    ca_cylinder: float
    v_lowering: float
    v_current: float
    splash_submerged_length_m: float
    wire_allowable_factor: float
    bending_allowable: float
    cable_unit_weight_sub_n_per_m: float
    tie_in_tolerance_mm: float
    reference_hs: float
    tp_coefficient: float
    go_marginal_threshold: float
    nogo_utilisation: float
    # Physical constants.
    seawater_density_kg_m3: float
    gravity_m_s2: float
    # Phase-2 lift-span model (the fix). Span scales with horizontal_span_m via the fraction;
    # the absolute override (lift_span_m) wins when set (> 0), else None.
    lift_span_fraction: float
    lift_span_m: float | None
    lift_moment_coeff: float
    # Phase-5 tie-in span model (the fix). Span scales with horizontal_span_m via the fraction;
    # the absolute override (tiein_unsupported_span_m) wins when set (> 0), else None.
    tiein_unsupported_span_fraction: float
    tiein_unsupported_span_m: float | None
    tiein_include_self_weight: bool
    tiein_deflection_coeff: float
    # Catalog file locations — resolved to ABSOLUTE paths (relative to the yaml dir).
    vessels_path: Path
    jumpers_path: Path
    # Artifact roots — resolved to ABSOLUTE paths (relative to the yaml dir).
    results_root: Path
    output_root: Path
    source_path: Path


@dataclass(frozen=True)
class ResolvedDemo05Paths:
    """The catalog + artifact paths only, resolved ABSOLUTE relative to the yaml dir.

    Lightweight sibling of ``ResolvedDemo05Config`` for the engineering-free ``--from-cache``
    path. Validated against the same schema as the full loader.
    """

    vessels_path: Path
    jumpers_path: Path
    results_root: Path
    output_root: Path
    source_path: Path


# ---------------------------------------------------------------------------
# Loader
# ---------------------------------------------------------------------------


def _load_schema() -> dict:
    """Return the demo_05 Sweep Config jsonschema (inline)."""
    return DEMO05_CONFIG_SCHEMA


def _parse_and_validate(path: Path) -> dict:
    """Parse the yaml and validate it against the schema; return the raw mapping."""
    if not path.exists():
        raise SweepConfigError(f"sweep config not found: {path}")
    try:
        with path.open("r", encoding="utf-8") as fh:
            raw = yaml.safe_load(fh)
    except yaml.YAMLError as exc:
        raise SweepConfigError(f"malformed YAML in {path}: {exc}") from exc
    if not isinstance(raw, dict):
        raise SweepConfigError(
            f"sweep config {path} must be a YAML mapping at top level, "
            f"got {type(raw).__name__}"
        )
    try:
        jsonschema.validate(
            instance=raw, schema=_load_schema(), cls=jsonschema.Draft7Validator
        )
    except jsonschema.ValidationError as exc:
        raise SweepConfigError(
            f"schema validation failed for {path}: {exc.message}"
        ) from exc
    return raw


def load_demo05_paths(path: str | Path) -> ResolvedDemo05Paths:
    """Parse + validate the yaml and resolve ONLY the catalog/artifact paths.

    No engineering imports — usable on the ``--from-cache`` path. Paths are resolved relative
    to the yaml file's directory. Raises ``SweepConfigError`` on parse/schema failure exactly
    like ``load_demo05_config``.
    """
    path = Path(path)
    raw = _parse_and_validate(path)
    base = path.resolve().parent
    catalogs = raw["catalogs"]
    artifacts = raw["artifacts"]
    return ResolvedDemo05Paths(
        vessels_path=(base / catalogs["vessels"]).resolve(),
        jumpers_path=(base / catalogs["jumpers"]).resolve(),
        results_root=(base / artifacts["results_root"]).resolve(),
        output_root=(base / artifacts["output_root"]).resolve(),
        source_path=path,
    )


def load_demo05_config(path: str | Path) -> ResolvedDemo05Config:
    """Load, validate, and resolve a demo_05 Sweep Config yaml.

    Validation mirrors ``sweep_config_demo03.load_demo03_config``: parse YAML -> jsonschema
    validate -> resolve. Resolution applies the type rules:

      - ``depths`` items asserted ``int`` (rejecting ``bool``), kept as ``int``,
      - ``lengths_m`` and ``hs`` items resolved to ``float``.

    No engineering import is needed: the axes are plain string/number labels the demo loop
    resolves against the catalogs at run time.
    """
    path = Path(path)
    raw = _parse_and_validate(path)

    meta = raw["meta"]
    sweep = raw["sweep"]
    constants = raw["constants"]
    physical = raw["physical_constants"]
    lift = raw["lift_span_model"]
    tiein = raw["tiein_span_model"]
    catalogs = raw["catalogs"]
    artifacts = raw["artifacts"]

    # depths are schema-int; assert the runtime type so a loosened schema can't regress it,
    # and so a bool (a subclass of int) is rejected. Kept as int — DO NOT coerce to float.
    depths: List[int] = []
    for d in sweep["depths"]:
        if not isinstance(d, int) or isinstance(d, bool):
            raise SweepConfigError(
                f"sweep.depths item {d!r} is not an int (depths must stay integer)."
            )
        depths.append(d)

    # lengths_m and hs are floats — resolve each to float.
    lengths_m: List[float] = [float(v) for v in sweep["lengths_m"]]
    hs: List[float] = [float(v) for v in sweep["hs"]]

    # Absolute span overrides: null/absent => None (use the fraction); a present value is a
    # positive number (schema-guarded). Normalise to Optional[float].
    def _opt_span(section: dict, key: str) -> "float | None":
        val = section.get(key)
        return None if val is None else float(val)

    lift_span_m = _opt_span(lift, "lift_span_m")
    tiein_span_m = _opt_span(tiein, "tiein_unsupported_span_m")

    # Catalog + artifact paths are resolved RELATIVE TO THE YAML FILE'S DIRECTORY.
    base = path.resolve().parent
    vessels_path = (base / catalogs["vessels"]).resolve()
    jumpers_path = (base / catalogs["jumpers"]).resolve()
    results_root = (base / artifacts["results_root"]).resolve()
    output_root = (base / artifacts["output_root"]).resolve()

    return ResolvedDemo05Config(
        demo_id=meta["demo_id"],
        code_ref=meta.get("code_ref", ""),
        vessels=list(sweep["vessels"]),
        lengths_m=lengths_m,
        depths=depths,
        hs=hs,
        daf_liftoff=float(constants["daf_liftoff"]),
        daf_splash=float(constants["daf_splash"]),
        rigging_mass_kg=float(constants["rigging_mass_kg"]),
        cs_slamming=float(constants["cs_slamming"]),
        cd_cylinder=float(constants["cd_cylinder"]),
        ca_cylinder=float(constants["ca_cylinder"]),
        v_lowering=float(constants["v_lowering"]),
        v_current=float(constants["v_current"]),
        splash_submerged_length_m=float(constants["splash_submerged_length_m"]),
        wire_allowable_factor=float(constants["wire_allowable_factor"]),
        bending_allowable=float(constants["bending_allowable"]),
        cable_unit_weight_sub_n_per_m=float(constants["cable_unit_weight_sub_n_per_m"]),
        tie_in_tolerance_mm=float(constants["tie_in_tolerance_mm"]),
        reference_hs=float(constants["reference_hs"]),
        tp_coefficient=float(constants["tp_coefficient"]),
        go_marginal_threshold=float(constants["go_marginal_threshold"]),
        nogo_utilisation=float(constants["nogo_utilisation"]),
        seawater_density_kg_m3=float(physical["seawater_density_kg_m3"]),
        gravity_m_s2=float(physical["gravity_m_s2"]),
        lift_span_fraction=float(lift["lift_span_fraction"]),
        lift_span_m=lift_span_m,
        lift_moment_coeff=float(lift["moment_coeff"]),
        tiein_unsupported_span_fraction=float(tiein["tiein_unsupported_span_fraction"]),
        tiein_unsupported_span_m=tiein_span_m,
        tiein_include_self_weight=bool(tiein["include_self_weight"]),
        tiein_deflection_coeff=float(tiein["deflection_coeff"]),
        vessels_path=vessels_path,
        jumpers_path=jumpers_path,
        results_root=results_root,
        output_root=output_root,
        source_path=path,
    )


__all__ = [
    "SweepConfigError",
    "ResolvedDemo05Config",
    "ResolvedDemo05Paths",
    "DEMO05_CONFIG_SCHEMA",
    "load_demo05_config",
    "load_demo05_paths",
]
