# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# ABOUTME: Regulatory compliance engine — IMO/ABS/DNV automated checks
# ABOUTME: Preliminary checks with explicit missing-data status
"""Legacy compliance screens with PASS, FAIL and NOT_EVALUATED results."""

import math
from dataclasses import dataclass, field
from numbers import Real
from typing import Literal, Sequence

from digitalmodel.naval_architecture.damage_stability import (
    check_imo_intact_stability,
)

_FREEBOARD_COEFF = 0.025  # ICLL 1966 simplified Type B
_IMO_INTACT_NAME = "IMO Intact Stability (Part A 2.2, 6 criteria)"
_POSITIVE_INPUTS = {
    "lwl_m", "beam_m", "draft_m", "depth_m", "prop_diameter_m",
    "displacement_tonnes", "min_ratio", "max_ratio", "max_trim_fraction",
}
CheckStatus = Literal["PASS", "FAIL", "NOT_EVALUATED"]


@dataclass(frozen=True)
class CheckResult:
    """Typed evidence envelope; the legacy dictionary API uses ``to_dict``.

    ``missing_inputs`` includes absent and invalid data. A null clause reference
    explicitly records an unqualified legacy screen rather than inventing a
    standard clause. PASS describes the screen only, not statutory approval.
    """

    name: str
    code: str
    status: CheckStatus
    missing_inputs: tuple[str, ...] = ()
    clause_reference: str | None = None
    values: dict = field(default_factory=dict)

    def __post_init__(self) -> None:
        if self.status not in ("PASS", "FAIL", "NOT_EVALUATED"):
            raise ValueError("Invalid check status")
        if self.missing_inputs and self.status != "NOT_EVALUATED":
            raise ValueError("An evaluated check cannot have missing inputs")

    def to_dict(self) -> dict:
        """Serialize evidence using native status strings, lists and booleans."""
        return {
            **self.values,
            "name": self.name, "code": self.code, "status": self.status,
            "pass": self.status == "PASS",
            "missing_inputs": list(self.missing_inputs),
            "clause_reference": self.clause_reference,
            "reference_status": "RECORDED" if self.clause_reference else "UNVERIFIED",
        }

    @classmethod
    def from_dict(cls, row: dict) -> "CheckResult":
        """Reconstruct a typed row and reject contradictory serialized status."""
        if row["pass"] is not (row["status"] == "PASS"):
            raise ValueError("Serialized pass flag contradicts status")
        fields = {"name", "code", "status", "pass", "missing_inputs",
                  "clause_reference", "reference_status"}
        return cls(row["name"], row["code"], row["status"],
                   tuple(row["missing_inputs"]), row["clause_reference"],
                   {key: value for key, value in row.items() if key not in fields})


@dataclass(frozen=True)
class ComplianceResult:
    """Typed aggregate; empty or incomplete evidence never has PASS status."""

    checks: tuple[CheckResult, ...]

    @property
    def status(self) -> CheckStatus:
        if any(row.status == "FAIL" for row in self.checks):
            return "FAIL"
        if not self.checks or any(row.status == "NOT_EVALUATED" for row in self.checks):
            return "NOT_EVALUATED"
        return "PASS"

    def to_dict(self) -> dict:
        """Expose the legacy report keys alongside structured missing evidence."""
        return {
            "name": "Preliminary compliance screens (IMO Part A 2.2 only)",
            "overall_pass": self.status == "PASS", "status": self.status,
            "checks": [row.to_dict() for row in self.checks],
            "total": len(self.checks),
            "passed": sum(row.status == "PASS" for row in self.checks),
            "failed": sum(row.status == "FAIL" for row in self.checks),
            "not_evaluated": sum(row.status == "NOT_EVALUATED" for row in self.checks),
            "missing_inputs": list(dict.fromkeys(
                key for row in self.checks for key in row.missing_inputs
            )) if self.checks else ["check_ids"],
        }


def _clause_reference(code: str) -> str | None:
    # Source: damage_stability.py, MSC.267(85) (2008), Part A 2.2.
    # Other legacy code labels lack a qualified clause: do not infer one.
    return "MSC.267(85), 2008, Part A 2.2" if code == "IMO-IS-2008" else None


def _not_evaluated(name, code, reason, unit=None, missing_inputs=()) -> dict:
    """Issue #2597: absence of evidence must never produce a passing check."""
    return CheckResult(
        name, code, "NOT_EVALUATED", tuple(missing_inputs), _clause_reference(code),
        {"reason": reason, "value": None, "required": None, "unit": unit},
    ).to_dict()


def _finite(value) -> bool:
    if not isinstance(value, Real) or isinstance(value, bool):
        return False
    try:
        return math.isfinite(value)
    except OverflowError:
        return False


def _validate_inputs(name, code, unit=None, **inputs):
    """Apply the same input contract to direct and aggregate calls."""
    invalid = []
    for key, value in inputs.items():
        if key in ("angles_deg", "gz_m"):
            valid = (isinstance(value, (list, tuple)) and len(value) >= 2
                     and all(_finite(v) for v in value))
        else:
            valid = _finite(value)
            if valid and key in _POSITIVE_INPUTS:
                valid = value > 0
            elif valid and key == "cb":
                valid = 0 < value <= 1
            elif valid and key == "actual_z_cm3":
                valid = value >= 0
        if not valid:
            invalid.append(key)
    if invalid:
        return _not_evaluated(
            name, code, "Missing or invalid inputs: " + ", ".join(invalid), unit, invalid)
    if "min_ratio" in inputs and inputs["min_ratio"] > inputs["max_ratio"]:
        return _not_evaluated(name, code, "min_ratio must not exceed max_ratio", unit,
                              ("min_ratio", "max_ratio"))
    if "angles_deg" in inputs:
        angles, gz = inputs["angles_deg"], inputs["gz_m"]
        if (len(angles) != len(gz)
                or any(b <= a for a, b in zip(angles, angles[1:]))):
            return _not_evaluated(
                name, code, "angles_deg and gz_m require equal lengths and increasing angles",
                missing_inputs=("angles_deg", "gz_m"))
        if angles[0] != 0 or angles[-1] < 40:
            return _not_evaluated(
                name, code, "angles_deg must start at 0 and cover at least 40 degrees",
                missing_inputs=("angles_deg",))
    return None


def _evaluated(name, code, passed, **values) -> dict:
    """Return a consistent status and a native boolean for every public check."""
    passed = bool(passed)
    return CheckResult(
        name, code, "PASS" if passed else "FAIL",
        clause_reference=_clause_reference(code), values=values,
    ).to_dict()


def check_imo_intact(angles_deg, gz_m, gm_m) -> dict:
    """Screen the six Part A 2.2 criteria; the weather criterion is excluded."""
    invalid = _validate_inputs(_IMO_INTACT_NAME, "IMO-IS-2008",
                               angles_deg=angles_deg, gz_m=gz_m, gm_m=gm_m)
    if invalid is not None:
        return invalid
    r = check_imo_intact_stability(angles_deg, gz_m, gm_m)
    return _evaluated(_IMO_INTACT_NAME, "IMO-IS-2008", r["overall_pass"],
                      details=r["criteria"])


def check_min_freeboard(freeboard_m: float, lwl_m: float) -> dict:
    """ICLL 1966 minimum freeboard check (simplified Type B)."""
    name, code = "Minimum freeboard (Type B)", "ICLL-1966"
    invalid = _validate_inputs(name, code, "m", freeboard_m=freeboard_m, lwl_m=lwl_m)
    if invalid is not None:
        return invalid
    req = _FREEBOARD_COEFF * lwl_m
    return _evaluated(name, code, freeboard_m >= req,
                      value=round(freeboard_m, 2), required=round(req, 2), unit="m")


def check_beam_draft_ratio(beam_m, draft_m, max_ratio=5.0, min_ratio=1.8) -> dict:
    """DNV beam/draft ratio check for conventional ships."""
    name, code = "Beam/draft ratio", "DNV-RU-SHIP"
    invalid = _validate_inputs(name, code, "dimensionless", beam_m=beam_m,
                               draft_m=draft_m, max_ratio=max_ratio, min_ratio=min_ratio)
    if invalid is not None:
        return invalid
    r = beam_m / draft_m
    return _evaluated(name, code, min_ratio <= r <= max_ratio,
                      value=round(r, 2), required=f"{min_ratio}-{max_ratio}",
                      unit="dimensionless")


def check_length_beam_ratio(lwl_m, beam_m, max_ratio=11.0, min_ratio=4.0) -> dict:
    """DNV length/beam ratio check."""
    name, code = "Length/beam ratio", "DNV-RU-SHIP"
    invalid = _validate_inputs(name, code, "dimensionless", lwl_m=lwl_m,
                               beam_m=beam_m, max_ratio=max_ratio, min_ratio=min_ratio)
    if invalid is not None:
        return invalid
    r = lwl_m / beam_m
    return _evaluated(name, code, min_ratio <= r <= max_ratio,
                      value=round(r, 2), required=f"{min_ratio}-{max_ratio}",
                      unit="dimensionless")


def check_draft_depth_ratio(draft_m, depth_m=None, max_ratio=0.85) -> dict:
    """DNV draft/depth ratio for adequate reserve buoyancy."""
    name, code = "Draft/depth ratio", "DNV-RU-SHIP"
    invalid = _validate_inputs(name, code, "dimensionless", draft_m=draft_m,
                               depth_m=depth_m, max_ratio=max_ratio)
    if invalid is not None:
        return invalid
    r = draft_m / depth_m
    return _evaluated(name, code, r <= max_ratio,
                      value=round(r, 2), required=f"≤{max_ratio}", unit="dimensionless")


def check_section_modulus(lwl_m, beam_m, draft_m, cb, actual_z_cm3) -> dict:
    """Legacy strength screen; coefficient and unit qualification remain open."""
    name, code = "Hull girder section modulus", "ABS-SVR"
    invalid = _validate_inputs(name, code, "cm³", lwl_m=lwl_m, beam_m=beam_m,
                               draft_m=draft_m, cb=cb, actual_z_cm3=actual_z_cm3)
    if invalid is not None:
        return invalid
    z_req = 0.01 * lwl_m**2 * beam_m * (cb + 0.7)
    return _evaluated(name, code, actual_z_cm3 >= z_req,
                      value=round(actual_z_cm3, 0), required=round(z_req, 0), unit="cm³")


def check_displacement_lwl_ratio(displacement_tonnes, lwl_m) -> dict:
    """Displacement/length ratio (slenderness) — general range check."""
    name, code = "Displacement/length ratio", "GENERAL"
    unit = "tonnes/((0.01·L)³)"
    invalid = _validate_inputs(name, code, unit,
                               displacement_tonnes=displacement_tonnes, lwl_m=lwl_m)
    if invalid is not None:
        return invalid
    r = displacement_tonnes / (0.01 * lwl_m) ** 3
    return _evaluated(name, code, 30.0 <= r <= 600.0,
                      value=round(r, 1), required="30-600", unit=unit)


def check_propeller_immersion(draft_m, prop_diameter_m=None) -> dict:
    """Minimum propeller immersion for cavitation avoidance."""
    name, code = "Propeller tip immersion", "DNV-RU-SHIP"
    invalid = _validate_inputs(name, code, "m", draft_m=draft_m,
                               prop_diameter_m=prop_diameter_m)
    if invalid is not None:
        return invalid
    tip = draft_m - prop_diameter_m
    req = 0.1 * prop_diameter_m
    return _evaluated(name, code, tip >= req,
                      value=round(tip, 2), required=round(req, 2), unit="m")


def check_max_trim(trim_m, lwl_m, max_trim_fraction=0.015) -> dict:
    """Maximum trim check (general practice: trim < 1.5% LWL)."""
    name, code = "Maximum trim", "GENERAL"
    invalid = _validate_inputs(name, code, "m", trim_m=trim_m, lwl_m=lwl_m,
                               max_trim_fraction=max_trim_fraction)
    if invalid is not None:
        return invalid
    mx = max_trim_fraction * lwl_m
    return _evaluated(name, code, abs(trim_m) <= mx,
                      value=round(abs(trim_m), 2), required=round(mx, 2), unit="m")


def evaluate_compliance_checks(
    vessel: dict, *, check_ids: Sequence[str] | None = None,
) -> ComplianceResult:
    """Evaluate the complete check set, exposing missing or invalid inputs.

    Each check requires every named input; no geometry or measured strength is
    inferred. Missing, None, nonfinite or malformed data is NOT_EVALUATED.
    FAIL takes precedence over NOT_EVALUATED in the aggregate. ``overall_pass``
    is true only when every intended check was evaluated and passed. ``failed``
    excludes unevaluated checks, counted separately as ``not_evaluated``.

    ``check_ids=None`` selects the complete legacy set. An explicit sequence
    selects only those screens in that order; an empty sequence is
    NOT_EVALUATED. Unknown or duplicate identifiers raise ValueError.

    IMO stability covers Part A 2.2 only, excluding the Part A 2.3 weather
    criterion. These are legacy preliminary screens; their approximate rule coefficients
    are unchanged and do not establish class or statutory compliance.
    """
    specifications = {
        "imo_intact": (check_imo_intact, ("angles_deg", "gz_m", "gm_m")),
        "min_freeboard": (check_min_freeboard, ("freeboard_m", "lwl_m")),
        "beam_draft_ratio": (check_beam_draft_ratio, ("beam_m", "draft_m")),
        "length_beam_ratio": (check_length_beam_ratio, ("lwl_m", "beam_m")),
        "draft_depth_ratio": (check_draft_depth_ratio, ("draft_m", "depth_m")),
        "displacement_lwl_ratio": (check_displacement_lwl_ratio,
                                    ("displacement_tonnes", "lwl_m")),
        "section_modulus": (check_section_modulus,
                             ("lwl_m", "beam_m", "draft_m", "cb", "actual_z_cm3")),
        "propeller_immersion": (check_propeller_immersion, ("draft_m", "prop_diameter_m")),
        "max_trim": (check_max_trim, ("trim_m", "lwl_m")),
    }
    selected = list(specifications) if check_ids is None else list(check_ids)
    if len(set(selected)) != len(selected):
        raise ValueError("Duplicate compliance check identifiers")
    if any(key not in specifications for key in selected):
        raise ValueError("Unknown compliance check identifier")
    checks = []
    for key in selected:
        function, keys = specifications[key]
        row = function(*(vessel.get(name) for name in keys))
        checks.append(CheckResult.from_dict({**row, "check_id": key}))
    return ComplianceResult(tuple(checks))


def run_compliance_checks(vessel: dict, *, check_ids: Sequence[str] | None = None) -> dict:
    """Serialize :func:`evaluate_compliance_checks` with legacy dictionary keys."""
    return evaluate_compliance_checks(vessel, check_ids=check_ids).to_dict()
