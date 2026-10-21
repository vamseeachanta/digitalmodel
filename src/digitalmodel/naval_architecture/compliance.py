# ABOUTME: Regulatory compliance engine — IMO/ABS/DNV automated checks
# ABOUTME: Preliminary checks with explicit missing-data status
"""Legacy compliance screens with PASS, FAIL and NOT_EVALUATED results."""

import math

from digitalmodel.naval_architecture.damage_stability import (
    check_imo_intact_stability,
)

_FREEBOARD_COEFF = 0.025  # ICLL 1966 simplified Type B


def _not_evaluated(name, code, reason, unit=None) -> dict:
    """Issue #2597: absence of evidence must never produce a passing check."""
    return {"name": name, "code": code, "pass": False,
            "status": "NOT_EVALUATED", "reason": reason,
            "value": None, "required": None, "unit": unit}


def _finite(value) -> bool:
    return (isinstance(value, (int, float))
            and not isinstance(value, bool) and math.isfinite(value))


def check_imo_intact(angles_deg, gz_m, gm_m) -> dict:
    """IMO IS Code 2008 intact stability — all 6 criteria."""
    r = check_imo_intact_stability(angles_deg, gz_m, gm_m)
    return {"name": "IMO Intact Stability (6 criteria)", "code": "IMO-IS-2008",
            "pass": r["overall_pass"], "details": r["criteria"]}


def check_min_freeboard(freeboard_m: float, lwl_m: float) -> dict:
    """ICLL 1966 minimum freeboard check (simplified Type B)."""
    req = _FREEBOARD_COEFF * lwl_m
    return {"name": "Minimum freeboard (Type B)", "code": "ICLL-1966",
            "pass": freeboard_m >= req,
            "value": round(freeboard_m, 2), "required": round(req, 2), "unit": "m"}


def check_beam_draft_ratio(beam_m, draft_m, max_ratio=5.0, min_ratio=1.8) -> dict:
    """DNV beam/draft ratio check for conventional ships."""
    r = beam_m / draft_m
    return {"name": "Beam/draft ratio", "code": "DNV-RU-SHIP",
            "pass": min_ratio <= r <= max_ratio,
            "value": round(r, 2), "required": f"{min_ratio}-{max_ratio}",
            "unit": "dimensionless"}


def check_length_beam_ratio(lwl_m, beam_m, max_ratio=11.0, min_ratio=4.0) -> dict:
    """DNV length/beam ratio check."""
    r = lwl_m / beam_m
    return {"name": "Length/beam ratio", "code": "DNV-RU-SHIP",
            "pass": min_ratio <= r <= max_ratio,
            "value": round(r, 2), "required": f"{min_ratio}-{max_ratio}",
            "unit": "dimensionless"}


def check_draft_depth_ratio(draft_m, depth_m=None, max_ratio=0.85) -> dict:
    """DNV draft/depth ratio for adequate reserve buoyancy."""
    if not _finite(depth_m) or depth_m <= 0:
        return _not_evaluated("Draft/depth ratio", "DNV-RU-SHIP",
                              "depth_m must be supplied, finite and positive",
                              "dimensionless")
    r = draft_m / depth_m
    return {"name": "Draft/depth ratio", "code": "DNV-RU-SHIP",
            "pass": r <= max_ratio, "status": "PASS" if r <= max_ratio else "FAIL",
            "value": round(r, 2),
            "required": f"≤{max_ratio}", "unit": "dimensionless"}


def check_section_modulus(lwl_m, beam_m, draft_m, cb, actual_z_cm3) -> dict:
    """ABS SVR — min hull girder section modulus: Z = C1*L²*B*(Cb+0.7)."""
    if not _finite(actual_z_cm3):
        return _not_evaluated("Hull girder section modulus", "ABS-SVR",
                              "actual_z_cm3 must be supplied and finite", "cm³")
    z_req = 0.01 * lwl_m**2 * beam_m * (cb + 0.7)
    return {"name": "Hull girder section modulus", "code": "ABS-SVR",
            "pass": actual_z_cm3 >= z_req,
            "value": round(actual_z_cm3, 0), "required": round(z_req, 0), "unit": "cm³"}


def check_displacement_lwl_ratio(displacement_tonnes, lwl_m) -> dict:
    """Displacement/length ratio (slenderness) — general range check."""
    r = displacement_tonnes / (0.01 * lwl_m) ** 3
    return {"name": "Displacement/length ratio", "code": "GENERAL",
            "pass": 30.0 <= r <= 600.0,
            "value": round(r, 1), "required": "30-600", "unit": "tonnes/((0.01·L)³)"}


def check_propeller_immersion(draft_m, prop_diameter_m=None) -> dict:
    """Minimum propeller immersion for cavitation avoidance."""
    if not _finite(prop_diameter_m) or prop_diameter_m <= 0:
        return _not_evaluated("Propeller tip immersion", "DNV-RU-SHIP",
                              "prop_diameter_m must be supplied, finite and positive", "m")
    tip = draft_m - prop_diameter_m
    req = 0.1 * prop_diameter_m
    return {"name": "Propeller tip immersion", "code": "DNV-RU-SHIP",
            "pass": tip >= req, "value": round(tip, 2), "required": round(req, 2), "unit": "m"}


def check_max_trim(trim_m, lwl_m, max_trim_fraction=0.015) -> dict:
    """Maximum trim check (general practice: trim < 1.5% LWL)."""
    mx = max_trim_fraction * lwl_m
    return {"name": "Maximum trim", "code": "GENERAL",
            "pass": abs(trim_m) <= mx,
            "value": round(abs(trim_m), 2), "required": round(mx, 2), "unit": "m"}


def run_compliance_checks(vessel: dict) -> dict:
    """Evaluate the complete check set, exposing missing or invalid inputs.

    Each check requires every named input; no geometry or measured strength is
    inferred. Missing, None, nonfinite or malformed data is NOT_EVALUATED.
    FAIL takes precedence over NOT_EVALUATED in the aggregate. ``overall_pass``
    is true only when every intended check was evaluated and passed. ``failed``
    excludes unevaluated checks, counted separately as ``not_evaluated``.

    These are legacy preliminary screens; their approximate rule coefficients
    are unchanged and do not establish class or statutory compliance.
    """
    specifications = (
        (check_imo_intact, "IMO Intact Stability (6 criteria)", "IMO-IS-2008",
         ("angles_deg", "gz_m", "gm_m")),
        (check_min_freeboard, "Minimum freeboard (Type B)", "ICLL-1966",
         ("freeboard_m", "lwl_m")),
        (check_beam_draft_ratio, "Beam/draft ratio", "DNV-RU-SHIP",
         ("beam_m", "draft_m")),
        (check_length_beam_ratio, "Length/beam ratio", "DNV-RU-SHIP",
         ("lwl_m", "beam_m")),
        (check_draft_depth_ratio, "Draft/depth ratio", "DNV-RU-SHIP",
         ("draft_m", "depth_m")),
        (check_displacement_lwl_ratio, "Displacement/length ratio", "GENERAL",
         ("displacement_tonnes", "lwl_m")),
        (check_section_modulus, "Hull girder section modulus", "ABS-SVR",
         ("lwl_m", "beam_m", "draft_m", "cb", "actual_z_cm3")),
        (check_propeller_immersion, "Propeller tip immersion", "DNV-RU-SHIP",
         ("draft_m", "prop_diameter_m")),
        (check_max_trim, "Maximum trim", "GENERAL", ("trim_m", "lwl_m")),
    )
    positive_dimensions = {"lwl_m", "beam_m", "draft_m", "depth_m", "prop_diameter_m"}
    checks = []
    for function, name, code, keys in specifications:
        invalid = []
        for key in keys:
            value = vessel.get(key)
            if key in ("angles_deg", "gz_m"):
                # Linear interpolation requires a segment, hence two points.
                valid = (isinstance(value, (list, tuple)) and len(value) >= 2
                         and all(_finite(v) for v in value))
            else:
                valid = _finite(value)
                if valid and key in positive_dimensions:
                    valid = value > 0
            if not valid:
                invalid.append(key)
        if invalid:
            checks.append(_not_evaluated(
                name, code, "Missing or invalid inputs: " + ", ".join(invalid)))
            continue
        if function is check_imo_intact:
            angles, gz = vessel["angles_deg"], vessel["gz_m"]
            if (len(angles) != len(gz)
                    or any(b <= a for a, b in zip(angles, angles[1:]))):
                checks.append(_not_evaluated(
                    name, code, "angles_deg and gz_m require equal lengths and increasing angles"))
                continue
        result = function(*(vessel[key] for key in keys))
        result.setdefault("status", "PASS" if result["pass"] else "FAIL")
        checks.append(result)

    passed = sum(c["status"] == "PASS" for c in checks)
    failed = sum(c["status"] == "FAIL" for c in checks)
    not_evaluated = sum(c["status"] == "NOT_EVALUATED" for c in checks)
    if failed:
        status = "FAIL"
    elif not checks or not_evaluated:
        status = "NOT_EVALUATED"
    else:
        status = "PASS"
    return {
        "overall_pass": status == "PASS",
        "status": status,
        "checks": checks,
        "total": len(checks),
        "passed": passed,
        "failed": failed,
        "not_evaluated": not_evaluated,
    }
