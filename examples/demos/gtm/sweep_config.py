#!/usr/bin/env python3
# ABOUTME: Sweep Config loader for demo_01 — validates + resolves inputs/demo_01_freespan.yml.
# ABOUTME: Phase-1 scope: drives only the sweep loop; locked constants stay in the demo module.
"""demo_01 Sweep Config loader.

Loads + validates a Sweep Config yaml (the analyst-facing case matrix, per CONTEXT.md)
against an inline jsonschema, mirroring the validation idiom of ``prospect_adapter``
(jsonschema via ``_load_schema``; ADR-0001). Resolves the validated yaml into a
``ResolvedSweepConfig`` exposing, per sub-sweep, the axis lists with the correct types:

  - ``span_lengths_m`` items stay ``int`` (BD-2),
  - finite gap-ratio items are ``float``, the jumper mid-water gap is ``float('inf')`` (BD-3),
  - the ``boundary_condition`` LABEL is mapped to the exact beam coefficient
    (``pinned -> C_N_PINNED == 3.5596``, ``fixed -> C_N_FIXED``) imported from the demo
    module so there is ONE definition of the coefficient (BD-5).

Phase-1 boundary (ADR-0002): this module drives ONLY the sweep loop. The locked physical
constants and screening thresholds remain module constants in the demo; migrating them into
the yaml is an explicit Phase-2 follow-up.
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import List

try:
    import jsonschema
except ImportError as exc:  # pragma: no cover — real dep required.
    raise RuntimeError(
        "sweep_config requires jsonschema. Install with "
        "`uv pip install jsonschema>=4.26`."
    ) from exc

try:
    import yaml
except ImportError as exc:  # pragma: no cover
    raise RuntimeError(
        "sweep_config requires PyYAML. Install with `uv pip install pyyaml`."
    ) from exc

# Import the beam boundary-condition coefficients from the demo module so there is exactly
# ONE definition of each coefficient (BD-5: never hardcode 3.56 here). The demo module is
# importable as `demo_01_dnv_freespan_viv` when PYTHONPATH includes examples/demos/gtm.
try:
    from demo_01_dnv_freespan_viv import C_N_FIXED, C_N_PINNED
except ImportError:  # pragma: no cover — packaged import path fallback.
    from examples.demos.gtm.demo_01_dnv_freespan_viv import C_N_FIXED, C_N_PINNED


# ---------------------------------------------------------------------------
# Boundary-condition LABEL -> coefficient (single source: the demo module)
# ---------------------------------------------------------------------------
BOUNDARY_CONDITION_TO_C_N = {
    "pinned": C_N_PINNED,
    "fixed": C_N_FIXED,
}


class SweepConfigError(ValueError):
    """Raised when a Sweep Config yaml fails schema or resolution checks."""


# ---------------------------------------------------------------------------
# jsonschema — describes the Sweep Config surface (Phase-1: sweep axes only)
# ---------------------------------------------------------------------------

# A gap-ratio item is a number (PyYAML resolves the YAML float `.inf` to a Python float,
# validated here as "number"). We also accept "string" at the SCHEMA layer purely so that a
# bare `inf` (which PyYAML loads as the STRING "inf") reaches the resolution layer, where it
# is rejected with a clear, actionable message (see _resolve_gap_ratios / BD-3) rather than a
# generic schema type error.
_GAP_ITEM_SCHEMA = {"type": ["number", "string"]}

_SUB_SWEEP_SCHEMA = {
    "type": "object",
    "required": [
        "sizes",
        "span_lengths_m",
        "current_velocities_ms",
        "gap_ratios",
        "content_density_kg_m3",
        "boundary_condition",
        "wt_selection",
    ],
    "additionalProperties": False,
    "properties": {
        "sizes": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "string"},
        },
        "span_lengths_m": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "integer"},  # BD-2: spans are integers
        },
        "current_velocities_ms": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "number"},
        },
        "gap_ratios": {
            "type": "array",
            "minItems": 1,
            "items": _GAP_ITEM_SCHEMA,
        },
        "content_density_kg_m3": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "number"},
        },
        "boundary_condition": {
            "type": "array",
            "minItems": 1,
            "items": {"type": "string", "enum": ["pinned", "fixed"]},
        },
        "wt_selection": {
            "type": "array",
            "minItems": 1,
            "items": {
                "type": "string",
                "enum": ["thinnest", "thickest", "explicit"],
            },
        },
    },
}

SWEEP_CONFIG_SCHEMA = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "required": ["meta", "catalogs", "sweeps"],
    "properties": {
        "meta": {
            "type": "object",
            "required": ["demo_id"],
            "properties": {
                "demo_id": {"type": "string"},
                "code_ref": {"type": "string"},
            },
        },
        "catalogs": {
            "type": "object",
            "required": ["pipelines", "jumpers"],
            "properties": {
                "pipelines": {"type": "string"},
                "jumpers": {"type": "string"},
            },
        },
        "sweeps": {
            "type": "object",
            "required": ["pipelines", "jumpers"],
            "properties": {
                "pipelines": _SUB_SWEEP_SCHEMA,
                "jumpers": _SUB_SWEEP_SCHEMA,
            },
        },
    },
}


# ---------------------------------------------------------------------------
# Resolved config dataclasses
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class ResolvedSubSweep:
    """One sub-sweep's resolved axes, typed as the engine consumes them.

    Axis ordering for the cross-product is fixed (ADR-0004 / BD-4):
        size -> span -> current -> gap
    with the three promoted axes (content_density, boundary_condition coefficient,
    wt_selection) iterated OUTSIDE/length-1 so that — being length-1 in the Baseline —
    they add zero iterations and do not perturb case numbering.
    """

    sizes: List[str]
    span_lengths_m: List[int]
    current_velocities_ms: List[float]
    gap_ratios: List[float]
    content_density_kg_m3: List[float]
    c_n_values: List[float]              # resolved coefficients (BD-5)
    boundary_condition_labels: List[str]  # original labels, for the record
    wt_selection: List[str]


@dataclass(frozen=True)
class ResolvedSweepConfig:
    demo_id: str
    code_ref: str
    pipelines_catalog: str
    jumpers_catalog: str
    pipelines: ResolvedSubSweep
    jumpers: ResolvedSubSweep
    source_path: Path


# ---------------------------------------------------------------------------
# Loader
# ---------------------------------------------------------------------------


def _load_schema() -> dict:
    """Return the Sweep Config jsonschema.

    Mirrors prospect_adapter._load_schema's role (a single validation idiom, ADR-0001).
    The schema is held inline rather than on disk because it is small and Phase-1 scoped;
    Phase-2 (constants:/thresholds: blocks) may promote it to a sibling file alongside
    prospect-schema.json.
    """
    return SWEEP_CONFIG_SCHEMA


def _resolve_gap_ratios(raw_gaps: list, *, sub_sweep_name: str) -> List[float]:
    """Resolve a gap-ratio axis: finite gaps -> float, mid-water `.inf` -> float('inf').

    Rejects bare `inf` (which PyYAML loads as the string "inf") with a clear error (BD-3).
    """
    resolved: List[float] = []
    for item in raw_gaps:
        if isinstance(item, str):
            if item.strip().lower() in ("inf", "infinity"):
                # inf-like string (bare `inf`/`infinity`): point at the `.inf` YAML float (BD-3).
                raise SweepConfigError(
                    f"{sub_sweep_name}.gap_ratios contains the string {item!r}; the mid-water "
                    "gap must be written as the YAML float `.inf` (not bare `inf`, which loads "
                    "as a string)."
                )
            # Genuinely garbage (non-numeric, non-inf) string: generic, actionable message.
            raise SweepConfigError(
                f"{sub_sweep_name}.gap_ratios entries must be numbers or the YAML float "
                f".inf; got {item!r}"
            )
        if isinstance(item, bool):  # bool is an int subclass — exclude explicitly.
            raise SweepConfigError(
                f"{sub_sweep_name}.gap_ratios contains a boolean {item!r}; expected a number."
            )
        resolved.append(float(item))
    return resolved


def _resolve_boundary_conditions(labels: list, *, sub_sweep_name: str) -> List[float]:
    """Map each boundary_condition LABEL to its beam coefficient (BD-5)."""
    c_n_values: List[float] = []
    for label in labels:
        try:
            c_n_values.append(BOUNDARY_CONDITION_TO_C_N[label])
        except KeyError as exc:
            raise SweepConfigError(
                f"{sub_sweep_name}.boundary_condition {label!r} is not a known label; "
                f"expected one of {sorted(BOUNDARY_CONDITION_TO_C_N)}"
            ) from exc
    return c_n_values


def _resolve_sub_sweep(raw: dict, *, sub_sweep_name: str) -> ResolvedSubSweep:
    spans = list(raw["span_lengths_m"])
    # Schema already enforced integer items; assert the runtime type so BD-2 cannot
    # silently regress if the schema is ever loosened.
    for s in spans:
        if not isinstance(s, int) or isinstance(s, bool):
            raise SweepConfigError(
                f"{sub_sweep_name}.span_lengths_m item {s!r} is not an int (BD-2)."
            )

    boundary_condition_labels = list(raw["boundary_condition"])
    c_n_values = _resolve_boundary_conditions(
        boundary_condition_labels, sub_sweep_name=sub_sweep_name
    )
    # c_n_values is derived 1:1 from boundary_condition_labels (one coefficient per label).
    # The demo iterates `zip(c_n_values, boundary_condition_labels)`; a length divergence would
    # make zip() silently under-run while the printed total (computed from len(c_n_values))
    # over-reports. Assert parity so that divergence is caught here, not silently dropped (D4).
    assert len(c_n_values) == len(boundary_condition_labels), (
        f"{sub_sweep_name}: c_n_values ({len(c_n_values)}) must be 1:1 with "
        f"boundary_condition_labels ({len(boundary_condition_labels)})"
    )

    return ResolvedSubSweep(
        sizes=list(raw["sizes"]),
        span_lengths_m=spans,
        current_velocities_ms=[float(v) for v in raw["current_velocities_ms"]],
        gap_ratios=_resolve_gap_ratios(raw["gap_ratios"], sub_sweep_name=sub_sweep_name),
        content_density_kg_m3=[float(d) for d in raw["content_density_kg_m3"]],
        c_n_values=c_n_values,
        boundary_condition_labels=boundary_condition_labels,
        wt_selection=list(raw["wt_selection"]),
    )


def load_sweep_config(path: str | Path) -> ResolvedSweepConfig:
    """Load, validate, and resolve a demo_01 Sweep Config yaml.

    Validation mirrors prospect_adapter.load_and_validate: parse YAML -> jsonschema
    validate -> resolve. Resolution applies the type/label rules (BD-2/3/5).
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
    catalogs = raw["catalogs"]
    sweeps = raw["sweeps"]

    return ResolvedSweepConfig(
        demo_id=meta["demo_id"],
        code_ref=meta.get("code_ref", ""),
        pipelines_catalog=catalogs["pipelines"],
        jumpers_catalog=catalogs["jumpers"],
        pipelines=_resolve_sub_sweep(sweeps["pipelines"], sub_sweep_name="pipelines"),
        jumpers=_resolve_sub_sweep(sweeps["jumpers"], sub_sweep_name="jumpers"),
        source_path=path,
    )


# Re-export math for callers that want to test isinf on resolved gaps without re-importing.
__all__ = [
    "SweepConfigError",
    "ResolvedSubSweep",
    "ResolvedSweepConfig",
    "SWEEP_CONFIG_SCHEMA",
    "BOUNDARY_CONDITION_TO_C_N",
    "load_sweep_config",
    "math",
]
