"""Worn-casing inspection intake; capacity qualification remains NOT_EVALUATED.

The nominal API ratings in casing.py are deliberately not called: substituting
a measured minimum wall does not establish applicability to damaged casing.
No standard-derived coefficient, material default or acceptance limit is used.
"""

from __future__ import annotations

import copy
from datetime import datetime
import hashlib
import json
import math

WORKFLOW_ID = "casing-wear-remaining-capacity"

# Source locators identify evidence gaps, not an implemented standard method.
SOURCES = {
    "worn_casing_study_2019": {
        "title": "Elastoplastic numerical evaluation on the collapse pressure of a worn casing tube",
        "publication": "Revista de Engenharia e Tecnologia, volume 11, number 3 (2019)",
        "sections": "Abstract; sections 2.1, 2.2 and 5",
        "url": "https://revistas.uepg.br/index.php/ret/article/download/13194/209209211470/209209220074",
        "checked_on": "2026-10-20",
        "use": "Nominal collapse and worn-section methods are distinguished; validation is for the unworn model.",
    },
    "api_tr_5c3": {
        "title": "API TR 5C3, Calculating Performance Properties of Pipe Used as Casing or Tubing",
        "publication": "First edition (2008), as cited by the public study",
        "sections": "Performance-property method; original qualification remains required",
        "use": "Referenced only; no licensed formula, example value or coefficient is reproduced.",
    },
    "iso_tr_10400": {
        "title": "ISO/TR 10400, Equations and calculations for the properties of casing, tubing, drill pipe and line pipe used as casing or tubing",
        "publication": "Edition requires confirmation for the assessment basis",
        "sections": "Applicable damaged-casing method and limits remain unverified",
        "use": "Referenced only; no calculation is claimed against this document.",
    },
}

_ROOT_FIELDS = {
    "source_ref",
    "observed_at",
    "log_kind",
    "damage",
    "od_mm",
    "nominal_wall_mm",
    "yield_strength_mpa",
    "measurement_uncertainty_mm",
    "measured_ovality",
    "samples",
    "load_cases",
}
_CASE_FIELDS = {
    "name",
    "internal_pressure_mpa",
    "external_pressure_mpa",
    "axial_force_kn",
    "temperature_c",
    "burst_design_factor",
    "collapse_design_factor",
}


def _fields(value: dict, expected: set[str], label: str) -> None:
    if not isinstance(value, dict):
        raise ValueError(f"{label} must be an object")
    missing, extra = expected - value.keys(), value.keys() - expected
    if missing or extra:
        raise ValueError(f"{label}: missing {sorted(missing)}, unknown {sorted(extra)}")


def _number(value, label: str, *, positive=False, nonnegative=False) -> float:
    if isinstance(value, bool) or not isinstance(value, (float, int)):
        raise ValueError(f"{label} must be a finite number")
    try:
        finite = math.isfinite(value)
    except OverflowError:
        finite = False
    if not finite or (positive and value <= 0) or (nonnegative and value < 0):
        raise ValueError(f"{label} is outside its numeric domain")
    return value


def _text(value, label: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{label} must be nonempty text")


def _validate_samples(data: dict) -> None:
    samples = data["samples"]
    if not isinstance(samples, list) or not samples:
        raise ValueError("samples must be a nonempty list")
    for sample in samples:
        _fields(sample, {"depth_m", "azimuth_deg", "value_mm"}, "sample")
        _number(sample["depth_m"], "depth_m", nonnegative=True)
        angle = _number(sample["azimuth_deg"], "azimuth_deg", nonnegative=True)
        if angle >= 360:  # Geometric convention: one revolution, degrees [0,360).
            raise ValueError("azimuth_deg must be below one revolution")
        value = _number(sample["value_mm"], "value_mm", positive=True)
        # Geometric bound, not a damage-acceptance criterion: wall/radius < D/2.
        if value >= data["od_mm"] / 2:
            raise ValueError("value_mm must be below the outside radius")


def _validate_cases(cases: list) -> None:
    if not isinstance(cases, list) or not cases:
        raise ValueError("load_cases must be a nonempty list")
    names = set()
    for case in cases:
        _fields(case, _CASE_FIELDS, "load case")
        _text(case["name"], "name")
        if case["name"] in names:
            raise ValueError("load case names must be unique")
        names.add(case["name"])
        for field in ("internal_pressure_mpa", "external_pressure_mpa"):
            _number(case[field], field, nonnegative=True)
        for field in ("burst_design_factor", "collapse_design_factor"):
            _number(case[field], field, positive=True)
        for field in ("axial_force_kn", "temperature_c"):
            _number(case[field], field)


def _validate(data: dict) -> None:
    _fields(data, _ROOT_FIELDS, "inspection")
    _text(data["source_ref"], "source_ref")
    _text(data["observed_at"], "observed_at")
    try:
        timestamp = datetime.fromisoformat(data["observed_at"].replace("Z", "+00:00"))
        if timestamp.tzinfo is None:
            raise ValueError("timezone required")
    except ValueError as exc:
        raise ValueError("observed_at must be an ISO timestamp with timezone") from exc
    if data["log_kind"] not in ("ultrasonic_wall", "caliper_internal_radius"):
        raise ValueError("unsupported log_kind")
    if data["damage"] not in ("uniform_corrosion", "wear_groove"):
        raise ValueError("unsupported damage")
    for field in ("od_mm", "nominal_wall_mm", "yield_strength_mpa"):
        _number(data[field], field, positive=True)
    if data["nominal_wall_mm"] >= data["od_mm"] / 2:  # Hollow-section geometry.
        raise ValueError("nominal_wall_mm must be below the outside radius")
    for field in ("measurement_uncertainty_mm", "measured_ovality"):
        _number(data[field], field, nonnegative=True)
    _validate_samples(data)
    _validate_cases(data["load_cases"])


def _inspection_summary(data: dict) -> dict:
    wall = lower = None
    limits = [
        "Inspection coverage, calibration and uncertainty basis require qualification.",
        "A measured minimum or uncertainty bound is not an equivalent uniform wall.",
    ]
    if data["log_kind"] == "ultrasonic_wall":
        wall = min(sample["value_mm"] for sample in data["samples"])
        # Bookkeeping definition: deterministic lower bound for supplied absolute
        # error magnitude. No confidence level or engineering equivalence is inferred.
        lower = wall - data["measurement_uncertainty_mm"]
        if lower <= 0:
            limits.append(
                "The supplied uncertainty does not establish a positive wall."
            )
    else:
        limits.insert(
            0,
            "Caliper radius needs an independently qualified outer-surface geometry to establish wall.",
        )
    return {
        "sample_count": len(data["samples"]),
        "minimum_observed_wall_mm": wall,
        "lower_wall_bound_mm": lower,
        "equivalent_wall_mm": None,
        "limitations": limits,
    }


def _checks() -> list[dict]:
    gaps = {
        "wall_equivalence": "Qualified conversion of inspection geometry and uncertainty to equivalent wall for the specified damage is missing.",
        "collapse": "Damaged-wall applicability, D/t regime, ovality correction, axial-load and temperature methods and validation are missing.",
        "burst": "A qualified damaged-section internal-yield or rupture method, wall basis and combined-load applicability are missing.",
        "combined_loads": "Temperature-dependent material properties and the axial-pressure interaction criterion are missing.",
        "design_factors": "No qualified remaining burst or collapse capacity exists for comparison with the supplied design factors.",
    }
    return [
        {
            "id": name,
            "status": "NOT_EVALUATED",
            "value": None,
            "missing_evidence": reason,
            "sources": list(SOURCES),
        }
        for name, reason in gaps.items()
    ]


def assess_casing_wear(data: dict) -> dict:
    """Validate inspection/load inputs and return a deterministic gap assessment.

    Pressure inputs use the same absolute or gauge reference, and axial force is
    tension-positive. Measured ovality is retained as supplied, with its definition
    requiring qualification. Design factors are required inputs, never defaults.
    No operating-pressure or remaining-strength verdict is established.
    """
    _validate(data)
    canonical = json.dumps(data, sort_keys=True, separators=(",", ":"), allow_nan=False)
    cases = []
    for case in data["load_cases"]:
        # Differential-pressure definition; positive inside-minus-outside drives
        # burst, positive outside-minus-inside drives collapse. No capacity equation.
        dp = case["internal_pressure_mpa"] - case["external_pressure_mpa"]
        cases.append(
            {
                "name": case["name"],
                "burst_differential_mpa": max(dp, 0),
                "collapse_differential_mpa": max(-dp, 0),
                "checks": _checks(),
            }
        )
    return {
        "workflow_id": WORKFLOW_ID,
        "schema_version": 1,
        "status": "NOT_EVALUATED",
        "input_sha256": hashlib.sha256(canonical.encode("utf-8")).hexdigest(),
        "inputs": copy.deepcopy(data),
        "inspection_summary": _inspection_summary(data),
        "load_cases": cases,
        "sources": copy.deepcopy(SOURCES),
        "validation": {
            "api_example_properties": "NOT_EVALUATED",
            "damaged_casing_capacity": "NOT_EVALUATED",
            "basis": "Software tests establish intake and bookkeeping only; no physical capacity benchmark is qualified.",
        },
        "next_action": "Obtain an applicable damaged-casing method, inspection qualification, material data and independent capacity benchmark before evaluating capacity or operating pressure.",
    }
