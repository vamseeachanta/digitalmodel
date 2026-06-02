#!/usr/bin/env python3
# ABOUTME: Sweep Config loader for demo_03 — validates + resolves inputs/demo_03_mudmat.yml.
# ABOUTME: ADR-0005: the yaml is the COMPLETE config source of truth; this loader resolves it.
"""demo_03 Sweep Config loader (sibling of sweep_config_demo02.py, NOT an import of it).

Loads + validates a Sweep Config yaml (the analyst-facing case matrix + all config) against
an inline jsonschema, mirroring the validation idiom of ``sweep_config_demo02.load_demo02_config``
(parse YAML -> jsonschema validate -> resolve). Resolves the validated yaml into a
``ResolvedDemo03Config`` exposing the axis lists typed as the engine consumes them:

  - ``vessels`` items are the vessel ``name`` labels (e.g. "Large CSV"),
  - ``depths`` items stay ``int`` (B1 — never coerced to float; ``water_depth_m`` is int),
  - ``mudmats`` items are the structure ``name`` labels,
  - ``hs`` items are ``float`` (B1).

Circular-import avoidance: the demo module is heavy (numpy/pandas/plotly). It is imported
LAZILY only where needed; this loader does NOT need the engineering stack to resolve, so the
full config + the paths-only resolver are both usable without importing the demo.
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional

try:
    import jsonschema
except ImportError as exc:  # pragma: no cover — real dep required.
    raise RuntimeError(
        "sweep_config_demo03 requires jsonschema. Install with "
        "`uv pip install jsonschema>=4.26`."
    ) from exc

try:
    import yaml
except ImportError as exc:  # pragma: no cover
    raise RuntimeError(
        "sweep_config_demo03 requires PyYAML. Install with `uv pip install pyyaml`."
    ) from exc


class SweepConfigError(ValueError):
    """Raised when a Sweep Config yaml fails schema or resolution checks."""


# ---------------------------------------------------------------------------
# jsonschema — describes the demo_03 Sweep Config surface (ADR-0005: ALL config)
# ---------------------------------------------------------------------------

_SWEEP_SCHEMA = {
    "type": "object",
    "required": ["vessels", "depths", "mudmats", "hs"],
    "additionalProperties": False,
    "properties": {
        "vessels": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "string"},
        },
        "depths": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "integer"},  # B1: depths are integers
        },
        "mudmats": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "string"},
        },
        "hs": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "number"},  # B1: Hs are floats
        },
    },
}

_CONSTANTS_SCHEMA = {
    "type": "object",
    "required": [
        "daf_liftoff",
        "daf_splash",
        "wire_mbl_sf",
        "tilt_limit_deg",
        "operating_radius_m",
        "reference_hs",
        "tp_coefficient",
        "go_marginal_threshold",
        "nogo_utilisation",
    ],
    "additionalProperties": False,
    "properties": {
        "daf_liftoff": {"type": "number"},
        "daf_splash": {"type": "number"},
        "wire_mbl_sf": {"type": "number"},
        "tilt_limit_deg": {"type": "number"},
        "operating_radius_m": {"type": "number"},
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
        "steel_density_kg_m3",
    ],
    "additionalProperties": False,
    "properties": {
        "seawater_density_kg_m3": {"type": "number"},
        "gravity_m_s2": {"type": "number"},
        "steel_density_kg_m3": {"type": "number"},
    },
}

# Soil bearing-capacity parameters (§4). The landing phase now derives the undrained bearing
# CAPACITY q_ult = su * Nc * sc * dc (Brinch Hansen, phi=0) and checks the applied bearing
# pressure against q_allow = q_ult / FS. These parameters drive the live compute.
_SOIL_SCHEMA = {
    "type": "object",
    "required": [
        "undrained_shear_strength_su_kpa",
        "bearing_capacity_factor_nc",
        "apply_shape_factor",
        "apply_depth_factor",
        "factor_of_safety",
    ],
    "additionalProperties": False,
    "properties": {
        # su, Nc and FS are strictly positive physical quantities: a zero or
        # negative value is a config error, not a valid sweep point. Reject it
        # at load time so it can never reach calc_landing and silently mask as a
        # PASS via the q_allow>0 fail-open guard (review finding §4, LOW).
        "undrained_shear_strength_su_kpa": {"type": "number", "exclusiveMinimum": 0},
        "bearing_capacity_factor_nc": {"type": "number", "exclusiveMinimum": 0},
        "apply_shape_factor": {"type": "boolean"},
        "apply_depth_factor": {"type": "boolean"},
        "factor_of_safety": {"type": "number", "exclusiveMinimum": 0},
    },
}

_CATALOGS_SCHEMA = {
    "type": "object",
    "required": ["vessels", "mudmats"],
    "additionalProperties": False,
    "properties": {
        "vessels": {"type": "string"},
        "mudmats": {"type": "string"},
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

DEMO03_CONFIG_SCHEMA = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "required": [
        "meta",
        "sweep",
        "constants",
        "physical_constants",
        "soil",
        "catalogs",
        "artifacts",
    ],
    "properties": {
        "meta": {
            "type": "object",
            "required": ["demo_id"],
            "properties": {
                "demo_id": {"type": "string"},
                "code_ref": {"type": "string"},
            },
        },
        "sweep": _SWEEP_SCHEMA,
        "constants": _CONSTANTS_SCHEMA,
        "physical_constants": _PHYSICAL_CONSTANTS_SCHEMA,
        "soil": _SOIL_SCHEMA,
        "catalogs": _CATALOGS_SCHEMA,
        "artifacts": _ARTIFACTS_SCHEMA,
    },
}


# ---------------------------------------------------------------------------
# Resolved config dataclasses
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class ResolvedDemo03Config:
    """The resolved demo_03 axes + constants, typed as the engine consumes them.

    Axis ordering for the cross-product is fixed: vessel (outer) -> depth -> structure ->
    hs (inner), matching the frozen 180-case golden.

    B1: ``depths`` is ``List[int]`` (kept int, never coerced); ``hs`` is ``List[float]``.
    """

    demo_id: str
    code_ref: str
    # Sweep axes.
    vessels: List[str]
    depths: List[int]                # B1: integers, preserved
    mudmats: List[str]
    hs: List[float]                  # B1: floats
    # Engineering constants.
    daf_liftoff: float
    daf_splash: float
    wire_mbl_sf: float
    tilt_limit_deg: float
    operating_radius_m: float
    reference_hs: float
    tp_coefficient: float
    go_marginal_threshold: float
    nogo_utilisation: float
    # Physical constants.
    seawater_density_kg_m3: float
    gravity_m_s2: float
    steel_density_kg_m3: float
    # Soil bearing-capacity parameters (§4: q_ult = su * Nc * sc * dc, Brinch Hansen phi=0).
    undrained_shear_strength_su_kpa: float
    bearing_capacity_factor_nc: float
    apply_shape_factor: bool
    apply_depth_factor: bool
    factor_of_safety: float
    # Catalog file locations — resolved to ABSOLUTE paths (relative to the yaml dir).
    vessels_path: Path
    mudmats_path: Path
    # Artifact roots — resolved to ABSOLUTE paths (relative to the yaml dir).
    results_root: Path
    output_root: Path
    source_path: Path


@dataclass(frozen=True)
class ResolvedDemo03Paths:
    """The catalog + artifact paths only, resolved ABSOLUTE relative to the yaml dir.

    Lightweight sibling of ``ResolvedDemo03Config`` for the engineering-free ``--from-cache``
    path. Validated against the same schema as the full loader.
    """

    vessels_path: Path
    mudmats_path: Path
    results_root: Path
    output_root: Path
    source_path: Path


# ---------------------------------------------------------------------------
# Loader
# ---------------------------------------------------------------------------


def _load_schema() -> dict:
    """Return the demo_03 Sweep Config jsonschema (inline)."""
    return DEMO03_CONFIG_SCHEMA


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


def load_demo03_paths(path: str | Path) -> ResolvedDemo03Paths:
    """Parse + validate the yaml and resolve ONLY the catalog/artifact paths.

    No engineering imports — usable on the ``--from-cache`` path. Paths are resolved relative
    to the yaml file's directory. Raises ``SweepConfigError`` on parse/schema failure exactly
    like ``load_demo03_config``.
    """
    path = Path(path)
    raw = _parse_and_validate(path)
    base = path.resolve().parent
    catalogs = raw["catalogs"]
    artifacts = raw["artifacts"]
    return ResolvedDemo03Paths(
        vessels_path=(base / catalogs["vessels"]).resolve(),
        mudmats_path=(base / catalogs["mudmats"]).resolve(),
        results_root=(base / artifacts["results_root"]).resolve(),
        output_root=(base / artifacts["output_root"]).resolve(),
        source_path=path,
    )


def load_demo03_config(path: str | Path) -> ResolvedDemo03Config:
    """Load, validate, and resolve a demo_03 Sweep Config yaml.

    Validation mirrors ``sweep_config_demo02.load_demo02_config``: parse YAML -> jsonschema
    validate -> resolve. Resolution applies the type rules (B1):

      - ``depths`` items asserted ``int`` (rejecting ``bool``), kept as ``int``,
      - ``hs`` items resolved to ``float``.

    No engineering import is needed: the axes are plain string/number labels the demo loop
    resolves against the catalogs at run time.
    """
    path = Path(path)
    raw = _parse_and_validate(path)

    meta = raw["meta"]
    sweep = raw["sweep"]
    constants = raw["constants"]
    physical = raw["physical_constants"]
    soil = raw["soil"]
    catalogs = raw["catalogs"]
    artifacts = raw["artifacts"]

    # B1: depths are schema-int; assert the runtime type so a loosened schema can't regress it,
    # and so a bool (a subclass of int) is rejected. Kept as int — DO NOT coerce to float.
    depths: List[int] = []
    for d in sweep["depths"]:
        if not isinstance(d, int) or isinstance(d, bool):
            raise SweepConfigError(
                f"sweep.depths item {d!r} is not an int (B1: depths must stay integer)."
            )
        depths.append(d)

    # B1: hs are floats — resolve each to float.
    hs: List[float] = [float(v) for v in sweep["hs"]]

    # Catalog + artifact paths are resolved RELATIVE TO THE YAML FILE'S DIRECTORY.
    base = path.resolve().parent
    vessels_path = (base / catalogs["vessels"]).resolve()
    mudmats_path = (base / catalogs["mudmats"]).resolve()
    results_root = (base / artifacts["results_root"]).resolve()
    output_root = (base / artifacts["output_root"]).resolve()

    return ResolvedDemo03Config(
        demo_id=meta["demo_id"],
        code_ref=meta.get("code_ref", ""),
        vessels=list(sweep["vessels"]),
        depths=depths,
        mudmats=list(sweep["mudmats"]),
        hs=hs,
        daf_liftoff=float(constants["daf_liftoff"]),
        daf_splash=float(constants["daf_splash"]),
        wire_mbl_sf=float(constants["wire_mbl_sf"]),
        tilt_limit_deg=float(constants["tilt_limit_deg"]),
        operating_radius_m=float(constants["operating_radius_m"]),
        reference_hs=float(constants["reference_hs"]),
        tp_coefficient=float(constants["tp_coefficient"]),
        go_marginal_threshold=float(constants["go_marginal_threshold"]),
        nogo_utilisation=float(constants["nogo_utilisation"]),
        seawater_density_kg_m3=float(physical["seawater_density_kg_m3"]),
        gravity_m_s2=float(physical["gravity_m_s2"]),
        steel_density_kg_m3=float(physical["steel_density_kg_m3"]),
        undrained_shear_strength_su_kpa=float(soil["undrained_shear_strength_su_kpa"]),
        bearing_capacity_factor_nc=float(soil["bearing_capacity_factor_nc"]),
        apply_shape_factor=bool(soil["apply_shape_factor"]),
        apply_depth_factor=bool(soil["apply_depth_factor"]),
        factor_of_safety=float(soil["factor_of_safety"]),
        vessels_path=vessels_path,
        mudmats_path=mudmats_path,
        results_root=results_root,
        output_root=output_root,
        source_path=path,
    )


__all__ = [
    "SweepConfigError",
    "ResolvedDemo03Config",
    "ResolvedDemo03Paths",
    "DEMO03_CONFIG_SCHEMA",
    "load_demo03_config",
    "load_demo03_paths",
]
