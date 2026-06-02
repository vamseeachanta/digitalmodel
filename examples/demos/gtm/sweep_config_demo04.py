#!/usr/bin/env python3
# ABOUTME: Sweep Config loader for demo_04 — validates + resolves inputs/demo_04_pipelay.yml.
# ABOUTME: ADR-0002/0005: the yaml is the COMPLETE config source of truth; this loader resolves it.
"""demo_04 Sweep Config loader (sibling of sweep_config_demo03.py, NOT an import of it).

Loads + validates a Sweep Config yaml (the analyst-facing case matrix + ALL config) against an
inline jsonschema, mirroring the validation idiom of ``sweep_config_demo03.load_demo03_config``
(parse YAML -> jsonschema validate -> resolve). Resolves the validated yaml into a
``ResolvedDemo04Config`` exposing the axis lists typed as the engine consumes them:

  - ``vessels`` items are the vessel ``id`` labels (e.g. "PLV-001"),
  - ``pipe_sizes`` is an ordered list of (nominal_size, target_wt_mm) the loop resolves against
    the pipelines catalog (WT is ON the compute path — drives A_steel/axial and OD/WT/bending),
  - ``water_depths`` items stay ``int`` (B1 — never coerced to float),
  - the material ``grade`` selects ``smys_pa`` / ``smts_pa`` from the grades map.

Circular-import avoidance: the demo module is heavy (numpy/pandas/plotly). This loader does NOT
need the engineering stack to resolve, so the full config + the paths-only resolver are both
usable without importing the demo (e.g. the ``--from-cache`` path).
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Tuple

try:
    import jsonschema
except ImportError as exc:  # pragma: no cover — real dep required.
    raise RuntimeError(
        "sweep_config_demo04 requires jsonschema. Install with "
        "`uv pip install jsonschema>=4.26`."
    ) from exc

try:
    import yaml
except ImportError as exc:  # pragma: no cover
    raise RuntimeError(
        "sweep_config_demo04 requires PyYAML. Install with `uv pip install pyyaml`."
    ) from exc


class SweepConfigError(ValueError):
    """Raised when a Sweep Config yaml fails schema or resolution checks."""


# ---------------------------------------------------------------------------
# jsonschema — describes the demo_04 Sweep Config surface (ADR-0002/0005: ALL config)
# ---------------------------------------------------------------------------

_SWEEP_SCHEMA = {
    "type": "object",
    "required": ["vessels", "pipe_sizes", "water_depths_m"],
    "additionalProperties": False,
    "properties": {
        "vessels": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "string"},
        },
        # pipe_sizes maps nominal_size -> target wall thickness (mm). WT is strictly positive
        # (a zero/negative WT is a config error, not a valid sweep point).
        "pipe_sizes": {
            "type": "object",
            "minProperties": 1,
            "additionalProperties": {"type": "number", "exclusiveMinimum": 0},
        },
        "water_depths_m": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "integer"},  # B1: depths are integers
        },
    },
}

_GRADE_ENTRY_SCHEMA = {
    "type": "object",
    "required": ["smys_pa", "smts_pa"],
    "additionalProperties": False,
    "properties": {
        # SMYS / SMTS are strictly positive physical quantities.
        "smys_pa": {"type": "number", "exclusiveMinimum": 0},
        "smts_pa": {"type": "number", "exclusiveMinimum": 0},
    },
}

_MATERIAL_SCHEMA = {
    "type": "object",
    "required": ["grade", "grades"],
    "additionalProperties": False,
    "properties": {
        "grade": {"type": "string"},
        "grades": {
            "type": "object",
            "minProperties": 1,
            "additionalProperties": _GRADE_ENTRY_SCHEMA,
        },
    },
}

_PHYSICAL_CONSTANTS_SCHEMA = {
    "type": "object",
    "required": [
        "seawater_density_kg_m3",
        "gravity_m_s2",
        "steel_density_kg_m3",
        "youngs_modulus_pa",
    ],
    "additionalProperties": False,
    "properties": {
        "seawater_density_kg_m3": {"type": "number", "exclusiveMinimum": 0},
        "gravity_m_s2": {"type": "number", "exclusiveMinimum": 0},
        "steel_density_kg_m3": {"type": "number", "exclusiveMinimum": 0},
        # youngs_modulus is a strictly positive physical scalar on the compute path.
        "youngs_modulus_pa": {"type": "number", "exclusiveMinimum": 0},
    },
}

_CRITERIA_SCHEMA = {
    "type": "object",
    "required": [
        "stress_limit_factor",
        "tension_margin",
        "go_threshold",
        "nogo_utilisation",
        "sagbend_stress_basis",
    ],
    "additionalProperties": False,
    "properties": {
        # stress_limit_factor and tension_margin are strictly positive (a zero/negative value
        # silently rubber-stamps everything as a PASS / collapses the tension margin).
        "stress_limit_factor": {"type": "number", "exclusiveMinimum": 0},
        "tension_margin": {"type": "number", "exclusiveMinimum": 0},
        "go_threshold": {"type": "number"},
        "nogo_utilisation": {"type": "number"},
        "sagbend_stress_basis": {"type": "string", "enum": ["combined", "bending_only"]},
    },
}

_DISPLAY_SCHEMA = {
    "type": "object",
    "required": ["util_color_green_below", "util_color_amber_below"],
    "additionalProperties": False,
    "properties": {
        "util_color_green_below": {"type": "number"},
        "util_color_amber_below": {"type": "number"},
    },
}

_CATALOGS_SCHEMA = {
    "type": "object",
    "required": ["vessels", "pipelines"],
    "additionalProperties": False,
    "properties": {
        "vessels": {"type": "string"},
        "pipelines": {"type": "string"},
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

DEMO04_CONFIG_SCHEMA = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "required": [
        "meta",
        "sweep",
        "material",
        "physical_constants",
        "criteria",
        "display",
        "catalogs",
        "artifacts",
    ],
    # Reject unknown top-level sections: a misspelled section (e.g. "criterion"
    # instead of "criteria") must fail loudly at load, not silently fall back to
    # the module-default constants for the whole block.
    "additionalProperties": False,
    "properties": {
        "meta": {
            "type": "object",
            "required": ["demo_id"],
            "additionalProperties": False,
            "properties": {
                "demo_id": {"type": "string"},
                "code_ref": {"type": "string"},
            },
        },
        "sweep": _SWEEP_SCHEMA,
        "material": _MATERIAL_SCHEMA,
        "physical_constants": _PHYSICAL_CONSTANTS_SCHEMA,
        "criteria": _CRITERIA_SCHEMA,
        "display": _DISPLAY_SCHEMA,
        "catalogs": _CATALOGS_SCHEMA,
        "artifacts": _ARTIFACTS_SCHEMA,
    },
}


# ---------------------------------------------------------------------------
# Resolved config dataclasses
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class ResolvedDemo04Config:
    """The resolved demo_04 axes + constants, typed as the engine consumes them.

    Axis ordering for the cross-product is fixed: vessel (outer) -> pipe_size -> depth (inner),
    matching the frozen 60-case golden.

    B1: ``water_depths`` is ``List[int]`` (kept int, never coerced).
    PS: ``pipe_sizes`` is an ordered ``List[(nominal_size, target_wt_mm)]`` — WT is on the
        compute path (drives A_steel/axial and OD/WT/bending).
    """

    demo_id: str
    code_ref: str
    # Sweep axes.
    vessels: List[str]                       # vessel ids
    pipe_sizes: List[Tuple[str, float]]      # (nominal_size, target_wt_mm) — PS
    water_depths: List[int]                  # B1: integers, preserved
    # Material grade -> SMYS / SMTS (Pa).
    grade: str
    smys_pa: float
    smts_pa: float
    # Physical constants.
    seawater_density_kg_m3: float
    gravity_m_s2: float
    steel_density_kg_m3: float
    youngs_modulus_pa: float
    # Acceptance / criteria constants.
    stress_limit_factor: float
    tension_margin: float
    go_threshold: float
    nogo_utilisation: float
    sagbend_stress_basis: str                # 'combined' (default) | 'bending_only'
    # Display-only colour thresholds (SEPARATE from the go/no-go logic bands).
    util_color_green_below: float
    util_color_amber_below: float
    # Catalog file locations — resolved to ABSOLUTE paths (relative to the yaml dir).
    vessels_path: Path
    pipelines_path: Path
    # Artifact roots — resolved to ABSOLUTE paths (relative to the yaml dir).
    results_root: Path
    output_root: Path
    source_path: Path

    @property
    def pipe_selection(self) -> dict:
        """The PIPE_SELECTION map {nominal_size: target_wt_mm} (compute-path WT)."""
        return {nom: wt for nom, wt in self.pipe_sizes}


@dataclass(frozen=True)
class ResolvedDemo04Paths:
    """The catalog + artifact paths only, resolved ABSOLUTE relative to the yaml dir.

    Lightweight sibling of ``ResolvedDemo04Config`` for the engineering-free ``--from-cache``
    path. Validated against the same schema as the full loader.
    """

    vessels_path: Path
    pipelines_path: Path
    results_root: Path
    output_root: Path
    source_path: Path


# ---------------------------------------------------------------------------
# Loader
# ---------------------------------------------------------------------------


def _load_schema() -> dict:
    """Return the demo_04 Sweep Config jsonschema (inline)."""
    return DEMO04_CONFIG_SCHEMA


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


def load_demo04_paths(path: str | Path) -> ResolvedDemo04Paths:
    """Parse + validate the yaml and resolve ONLY the catalog/artifact paths.

    No engineering imports — usable on the ``--from-cache`` path. Paths are resolved relative
    to the yaml file's directory. Raises ``SweepConfigError`` on parse/schema failure exactly
    like ``load_demo04_config``.
    """
    path = Path(path)
    raw = _parse_and_validate(path)
    base = path.resolve().parent
    catalogs = raw["catalogs"]
    artifacts = raw["artifacts"]
    return ResolvedDemo04Paths(
        vessels_path=(base / catalogs["vessels"]).resolve(),
        pipelines_path=(base / catalogs["pipelines"]).resolve(),
        results_root=(base / artifacts["results_root"]).resolve(),
        output_root=(base / artifacts["output_root"]).resolve(),
        source_path=path,
    )


def load_demo04_config(path: str | Path) -> ResolvedDemo04Config:
    """Load, validate, and resolve a demo_04 Sweep Config yaml.

    Validation mirrors ``sweep_config_demo03.load_demo03_config``: parse YAML -> jsonschema
    validate -> resolve. Resolution applies the type rules:

      - ``water_depths`` items asserted ``int`` (rejecting ``bool``), kept as ``int`` (B1),
      - ``pipe_sizes`` resolved to an ordered list of (nominal_size, target_wt_mm) floats (PS),
      - the material ``grade`` selects ``smys_pa`` / ``smts_pa`` from the grades map.
    """
    path = Path(path)
    raw = _parse_and_validate(path)

    meta = raw["meta"]
    sweep = raw["sweep"]
    material = raw["material"]
    physical = raw["physical_constants"]
    criteria = raw["criteria"]
    display = raw["display"]
    catalogs = raw["catalogs"]
    artifacts = raw["artifacts"]

    # B1: depths are schema-int; assert the runtime type so a loosened schema can't regress it,
    # and so a bool (a subclass of int) is rejected. Kept as int — DO NOT coerce to float.
    water_depths: List[int] = []
    for d in sweep["water_depths_m"]:
        if not isinstance(d, int) or isinstance(d, bool):
            raise SweepConfigError(
                f"sweep.water_depths_m item {d!r} is not an int "
                "(B1: water depths must stay integer)."
            )
        water_depths.append(d)

    # PS: pipe_sizes map -> ordered (nominal, wt_mm) list, preserving yaml insertion order.
    pipe_sizes: List[Tuple[str, float]] = [
        (nom, float(wt)) for nom, wt in sweep["pipe_sizes"].items()
    ]

    # Material grade -> SMYS / SMTS.
    grade = material["grade"]
    grades = material["grades"]
    if grade not in grades:
        raise SweepConfigError(
            f"material.grade {grade!r} is not defined in material.grades "
            f"(available: {sorted(grades)})."
        )
    grade_props = grades[grade]
    smys_pa = float(grade_props["smys_pa"])
    smts_pa = float(grade_props["smts_pa"])

    # Catalog + artifact paths are resolved RELATIVE TO THE YAML FILE'S DIRECTORY.
    base = path.resolve().parent
    vessels_path = (base / catalogs["vessels"]).resolve()
    pipelines_path = (base / catalogs["pipelines"]).resolve()
    results_root = (base / artifacts["results_root"]).resolve()
    output_root = (base / artifacts["output_root"]).resolve()

    return ResolvedDemo04Config(
        demo_id=meta["demo_id"],
        code_ref=meta.get("code_ref", ""),
        vessels=list(sweep["vessels"]),
        pipe_sizes=pipe_sizes,
        water_depths=water_depths,
        grade=grade,
        smys_pa=smys_pa,
        smts_pa=smts_pa,
        seawater_density_kg_m3=float(physical["seawater_density_kg_m3"]),
        gravity_m_s2=float(physical["gravity_m_s2"]),
        steel_density_kg_m3=float(physical["steel_density_kg_m3"]),
        youngs_modulus_pa=float(physical["youngs_modulus_pa"]),
        stress_limit_factor=float(criteria["stress_limit_factor"]),
        tension_margin=float(criteria["tension_margin"]),
        go_threshold=float(criteria["go_threshold"]),
        nogo_utilisation=float(criteria["nogo_utilisation"]),
        sagbend_stress_basis=str(criteria["sagbend_stress_basis"]),
        util_color_green_below=float(display["util_color_green_below"]),
        util_color_amber_below=float(display["util_color_amber_below"]),
        vessels_path=vessels_path,
        pipelines_path=pipelines_path,
        results_root=results_root,
        output_root=output_root,
        source_path=path,
    )


__all__ = [
    "SweepConfigError",
    "ResolvedDemo04Config",
    "ResolvedDemo04Paths",
    "DEMO04_CONFIG_SCHEMA",
    "load_demo04_config",
    "load_demo04_paths",
]
