#!/usr/bin/env python3
# ABOUTME: Sweep Config loader for demo_02 — validates + resolves inputs/demo_02_wall_thickness.yml.
# ABOUTME: Phase-1 scope: drives only the sweep loop; locked constants stay in the demo module.
"""demo_02 Sweep Config loader (sibling of sweep_config.py, NOT an import of it).

Loads + validates a Sweep Config yaml (the analyst-facing case matrix) against an inline
jsonschema, mirroring the validation idiom of ``sweep_config.load_sweep_config``
(parse YAML -> jsonschema validate -> resolve). Resolves the validated yaml into a
``ResolvedDemo02Config`` exposing the axis lists typed as the engine consumes them:

  - ``sizes`` items are the PIPE_SIZES ``name`` labels (e.g. ``6"`` .. ``20"``),
  - ``codes`` items are the SPACED DISPLAY strings ("DNV-ST-F101" / "API RP 1111" /
    "PD 8010-2"), each resolved to a ``DesignCode`` via the demo's inverse map (BD-3 —
    NEVER ``DesignCode(<display string>)``, which would raise because the enum .value
    forms are the HYPHENATED "API-RP-1111" / "PD-8010-2"),
  - ``internal_pressures_mpa`` items stay ``int``,
  - ``safety_class`` LABEL ("MEDIUM") is mapped to ``SafetyClass.MEDIUM``.

Circular-import avoidance: the heavy demo / engineering symbols are imported LAZILY inside
``load_demo02_config`` (or may be supplied via the ``code_name_map`` / ``safety_class_enum``
parameters), never at module top.
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple

try:
    import jsonschema
except ImportError as exc:  # pragma: no cover — real dep required.
    raise RuntimeError(
        "sweep_config_demo02 requires jsonschema. Install with "
        "`uv pip install jsonschema>=4.26`."
    ) from exc

try:
    import yaml
except ImportError as exc:  # pragma: no cover
    raise RuntimeError(
        "sweep_config_demo02 requires PyYAML. Install with `uv pip install pyyaml`."
    ) from exc


class SweepConfigError(ValueError):
    """Raised when a Sweep Config yaml fails schema or resolution checks."""


# ---------------------------------------------------------------------------
# jsonschema — describes the demo_02 Sweep Config surface (Phase-1: axes + locked scalars)
# ---------------------------------------------------------------------------

_SWEEP_SCHEMA = {
    "type": "object",
    "required": ["sizes", "codes", "internal_pressures_mpa"],
    "additionalProperties": False,
    "properties": {
        "sizes": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "string"},
        },
        "codes": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "string"},
        },
        "internal_pressures_mpa": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "integer"},  # pressures are integers
        },
    },
}

_CONSTANTS_SCHEMA = {
    "type": "object",
    "required": [
        "water_depth_m",
        "grade",
        "smys_pa",
        "smts_pa",
        "corrosion_allowance_m",
        "safety_class",
        "find_min_bounds_m",
        "find_min_tol_m",
    ],
    "additionalProperties": False,
    "properties": {
        "water_depth_m": {"type": "number"},
        "grade": {"type": "string"},
        "smys_pa": {"type": "number"},
        "smts_pa": {"type": "number"},
        "corrosion_allowance_m": {"type": "number"},
        "safety_class": {"type": "string"},
        "find_min_bounds_m": {
            "type": "array",
            "minItems": 2,
            "maxItems": 2,
            "items": {"type": "number"},
        },
        "find_min_tol_m": {"type": "number"},
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

_REPORT_SCHEMA = {
    "type": "object",
    "required": ["design_pressure_mpa"],
    "additionalProperties": False,
    "properties": {
        "design_pressure_mpa": {"type": "integer"},
    },
}

_CATALOGS_SCHEMA = {
    "type": "object",
    "required": ["pipelines", "design_codes"],
    "additionalProperties": False,
    "properties": {
        "pipelines": {"type": "string"},
        "design_codes": {"type": "string"},
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

DEMO02_CONFIG_SCHEMA = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "required": [
        "meta",
        "sweep",
        "constants",
        "physical_constants",
        "report",
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
        "report": _REPORT_SCHEMA,
        "catalogs": _CATALOGS_SCHEMA,
        "artifacts": _ARTIFACTS_SCHEMA,
    },
}


# ---------------------------------------------------------------------------
# Resolved config dataclasses
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class ResolvedDemo02Config:
    """The resolved demo_02 axes + locked scalars, typed as the engine consumes them.

    Axis ordering for the cross-product is fixed: size (outer) -> code (mid) ->
    pressure (inner), matching the frozen 72-case golden.

    ``codes`` holds the original SPACED DISPLAY strings (the byte-for-byte value emitted
    into each case record); ``design_codes`` holds the 1:1-resolved ``DesignCode`` enum
    members the analyzer consumes. ``safety_class`` is the resolved ``SafetyClass`` member.
    """

    demo_id: str
    code_ref: str
    sizes: List[str]
    codes: List[str]                 # SPACED display strings (emitted verbatim)
    design_codes: List[object]       # resolved DesignCode enum members (1:1 with codes)
    internal_pressures_mpa: List[int]
    water_depth_m: float
    grade: str
    smys_pa: float
    smts_pa: float
    corrosion_allowance_m: float
    safety_class: object             # resolved SafetyClass enum member
    safety_class_label: str
    find_min_bounds_m: Tuple[float, float]
    find_min_tol_m: float
    # Physical constants (hydrostatic + steel weight).
    seawater_density_kg_m3: float
    gravity_m_s2: float
    steel_density_kg_m3: float
    # Report scalars (fixed design pressure used by charts/summary, in MPa and Pa).
    design_pressure_mpa: int
    design_pressure_pa: float
    # Catalog file locations — resolved to ABSOLUTE paths (relative to the yaml dir).
    pipelines_path: Path
    design_codes_path: Path
    # Artifact roots — resolved to ABSOLUTE paths (relative to the yaml dir).
    results_root: Path
    output_root: Path
    source_path: Path


# ---------------------------------------------------------------------------
# Lazy demo/engineering symbol access (circular-import avoidance)
# ---------------------------------------------------------------------------


def _build_display_to_design_code() -> Dict[str, object]:
    """Build the inverse map {display_string: DesignCode} from the demo's CODE_NAMES.

    BD-3: resolution must go through the demo's CODE_NAMES map (DesignCode -> spaced
    display string), inverted — NEVER ``DesignCode(<display string>)`` (whose .value forms
    are the HYPHENATED strings and would raise). Imported lazily to avoid a circular import
    at module load and to keep this loader usable without the engineering stack at import
    time.
    """
    # The demo module exposes CODE_NAMES only after _init_code_constants() runs, so call the
    # demo's loaders here rather than reading a possibly-empty module global.
    try:
        import demo_02_wall_thickness_multicode as demo
    except ImportError:  # pragma: no cover — packaged import path fallback.
        from examples.demos.gtm import demo_02_wall_thickness_multicode as demo

    demo._load_engineering_modules()
    demo._init_code_constants()
    # CODE_NAMES maps DesignCode -> spaced display string; invert it.
    inverse: Dict[str, object] = {display: code for code, display in demo.CODE_NAMES.items()}
    if not inverse:  # pragma: no cover — defensive; _init_code_constants should populate it.
        raise SweepConfigError(
            "demo CODE_NAMES is empty after _init_code_constants(); cannot resolve codes."
        )
    return inverse


def _resolve_safety_class(label: str) -> object:
    """Map the safety_class LABEL ("MEDIUM") to a SafetyClass enum member."""
    try:
        from digitalmodel.structural.analysis.wall_thickness import SafetyClass
    except ImportError:  # pragma: no cover
        from examples.demos.gtm.demo_02_wall_thickness_multicode import SafetyClass  # type: ignore
    try:
        return SafetyClass[label]
    except KeyError as exc:
        valid = sorted(s.name for s in SafetyClass)
        raise SweepConfigError(
            f"constants.safety_class {label!r} is not a known SafetyClass; expected one of {valid}"
        ) from exc


# ---------------------------------------------------------------------------
# Loader
# ---------------------------------------------------------------------------


def _load_schema() -> dict:
    """Return the demo_02 Sweep Config jsonschema (inline, Phase-1 scoped)."""
    return DEMO02_CONFIG_SCHEMA


def _resolve_codes(
    raw_codes: list, code_name_map: Dict[str, object]
) -> Tuple[List[str], List[object]]:
    """Resolve each display-string code via the inverse map (BD-3).

    Returns (display_strings, design_codes) as 1:1 parallel lists. An unknown code string
    is rejected with a clear, actionable error.
    """
    display_strings: List[str] = []
    design_codes: List[object] = []
    for entry in raw_codes:
        if entry not in code_name_map:
            raise SweepConfigError(
                f"sweep.codes entry {entry!r} is not a known code display string; "
                f"expected one of {sorted(code_name_map)} (BD-3: use the SPACED display "
                "string, never the hyphenated enum .value form)."
            )
        display_strings.append(entry)
        design_codes.append(code_name_map[entry])
    return display_strings, design_codes


@dataclass(frozen=True)
class ResolvedDemo02Paths:
    """The catalog + artifact paths only, resolved ABSOLUTE relative to the yaml dir.

    Lightweight sibling of ``ResolvedDemo02Config`` that needs NO engineering stack — so
    the demo can resolve where to read catalogs / write artifacts even on the engineering-
    free ``--from-cache`` path. Validated against the same schema as the full loader.
    """

    pipelines_path: Path
    design_codes_path: Path
    results_root: Path
    output_root: Path
    source_path: Path


def load_demo02_paths(path: str | Path) -> ResolvedDemo02Paths:
    """Parse + validate the yaml and resolve ONLY the catalog/artifact paths.

    No engineering imports — usable on the ``--from-cache`` path. Paths are resolved
    relative to the yaml file's directory. Raises ``SweepConfigError`` on parse/schema
    failure exactly like ``load_demo02_config``.
    """
    path = Path(path)
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

    base = path.resolve().parent
    catalogs = raw["catalogs"]
    artifacts = raw["artifacts"]
    return ResolvedDemo02Paths(
        pipelines_path=(base / catalogs["pipelines"]).resolve(),
        design_codes_path=(base / catalogs["design_codes"]).resolve(),
        results_root=(base / artifacts["results_root"]).resolve(),
        output_root=(base / artifacts["output_root"]).resolve(),
        source_path=path,
    )


def load_demo02_config(
    path: str | Path,
    *,
    code_name_map: Optional[Dict[str, object]] = None,
    safety_class_enum: Optional[object] = None,
) -> ResolvedDemo02Config:
    """Load, validate, and resolve a demo_02 Sweep Config yaml.

    Validation mirrors ``sweep_config.load_sweep_config``: parse YAML -> jsonschema
    validate -> resolve. Resolution applies the type/label rules:

      - ``codes`` resolved via the demo's {display -> DesignCode} inverse map (BD-3),
      - ``internal_pressures_mpa`` asserted ``int``,
      - ``safety_class`` mapped to a ``SafetyClass`` member.

    ``code_name_map`` / ``safety_class_enum`` may be supplied (e.g. by the demo, which has
    already loaded the engineering stack) to avoid the lazy import. When omitted they are
    built lazily here — keeping the import off the module top to avoid a circular import.
    """
    path = Path(path)
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
            instance=raw,
            schema=_load_schema(),
            cls=jsonschema.Draft7Validator,
        )
    except jsonschema.ValidationError as exc:
        raise SweepConfigError(
            f"schema validation failed for {path}: {exc.message}"
        ) from exc

    meta = raw["meta"]
    sweep = raw["sweep"]
    constants = raw["constants"]
    physical = raw["physical_constants"]
    report = raw["report"]
    catalogs = raw["catalogs"]
    artifacts = raw["artifacts"]

    # design_pressure_mpa is schema-int; assert at runtime so a loosened schema can't regress it.
    design_pressure_mpa = report["design_pressure_mpa"]
    if not isinstance(design_pressure_mpa, int) or isinstance(design_pressure_mpa, bool):
        raise SweepConfigError(
            f"report.design_pressure_mpa {design_pressure_mpa!r} is not an int."
        )

    # Catalog + artifact paths are resolved RELATIVE TO THE YAML FILE'S DIRECTORY.
    base = path.resolve().parent
    pipelines_path = (base / catalogs["pipelines"]).resolve()
    design_codes_path = (base / catalogs["design_codes"]).resolve()
    results_root = (base / artifacts["results_root"]).resolve()
    output_root = (base / artifacts["output_root"]).resolve()

    # Pressures: schema enforced integer items; assert the runtime type so it cannot
    # silently regress if the schema is ever loosened.
    pressures = list(sweep["internal_pressures_mpa"])
    for p in pressures:
        if not isinstance(p, int) or isinstance(p, bool):
            raise SweepConfigError(
                f"sweep.internal_pressures_mpa item {p!r} is not an int."
            )

    if code_name_map is None:
        code_name_map = _build_display_to_design_code()
    display_strings, design_codes = _resolve_codes(sweep["codes"], code_name_map)
    assert len(display_strings) == len(design_codes), (
        "demo_02: display_strings must be 1:1 with design_codes"
    )

    if safety_class_enum is not None:
        try:
            safety_class = safety_class_enum[constants["safety_class"]]
        except KeyError as exc:
            valid = sorted(s.name for s in safety_class_enum)
            raise SweepConfigError(
                f"constants.safety_class {constants['safety_class']!r} is not a known "
                f"SafetyClass; expected one of {valid}"
            ) from exc
    else:
        safety_class = _resolve_safety_class(constants["safety_class"])

    bounds = list(constants["find_min_bounds_m"])

    return ResolvedDemo02Config(
        demo_id=meta["demo_id"],
        code_ref=meta.get("code_ref", ""),
        sizes=list(sweep["sizes"]),
        codes=display_strings,
        design_codes=design_codes,
        internal_pressures_mpa=pressures,
        water_depth_m=float(constants["water_depth_m"]),
        grade=str(constants["grade"]),
        smys_pa=float(constants["smys_pa"]),
        smts_pa=float(constants["smts_pa"]),
        corrosion_allowance_m=float(constants["corrosion_allowance_m"]),
        safety_class=safety_class,
        safety_class_label=str(constants["safety_class"]),
        find_min_bounds_m=(float(bounds[0]), float(bounds[1])),
        find_min_tol_m=float(constants["find_min_tol_m"]),
        seawater_density_kg_m3=float(physical["seawater_density_kg_m3"]),
        gravity_m_s2=float(physical["gravity_m_s2"]),
        steel_density_kg_m3=float(physical["steel_density_kg_m3"]),
        design_pressure_mpa=int(design_pressure_mpa),
        design_pressure_pa=float(design_pressure_mpa) * 1e6,
        pipelines_path=pipelines_path,
        design_codes_path=design_codes_path,
        results_root=results_root,
        output_root=output_root,
        source_path=path,
    )


__all__ = [
    "SweepConfigError",
    "ResolvedDemo02Config",
    "ResolvedDemo02Paths",
    "DEMO02_CONFIG_SCHEMA",
    "load_demo02_config",
    "load_demo02_paths",
]
