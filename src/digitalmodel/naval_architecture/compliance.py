# ABOUTME: Regulatory compliance engine — IMO/ABS/DNV automated checks
# ABOUTME: Binary pass/fail criteria with code references
"""Automated regulatory compliance checks — IMO/ICLL/DNV/ABS binary criteria."""

from digitalmodel.naval_architecture.damage_stability import (
    check_imo_intact_stability,
)

_FREEBOARD_COEFF = 0.025  # ICLL 1966 simplified Type B


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
    if depth_m is None:
        return {"name": "Draft/depth ratio", "code": "DNV-RU-SHIP",
                "pass": True, "value": "N/A", "required": f"≤{max_ratio}",
                "unit": "dimensionless"}
    r = draft_m / depth_m
    return {"name": "Draft/depth ratio", "code": "DNV-RU-SHIP",
            "pass": r <= max_ratio, "value": round(r, 2),
            "required": f"≤{max_ratio}", "unit": "dimensionless"}


def check_section_modulus(lwl_m, beam_m, draft_m, cb, actual_z_cm3) -> dict:
    """ABS SVR — min hull girder section modulus: Z = C1*L²*B*(Cb+0.7)."""
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
    if prop_diameter_m is None:
        prop_diameter_m = 0.65 * draft_m
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
    """Run all applicable compliance checks on a vessel data dict.

    Expected vessel keys: angles_deg, gz_m, gm_m, freeboard_m,
    lwl_m, beam_m, draft_m, displacement_tonnes.
    Optional: depth_m, cb, actual_z_cm3, trim_m, prop_diameter_m.
    """
    checks = []

    # 1-6: IMO intact stability (6 sub-criteria)
    if "angles_deg" in vessel and "gz_m" in vessel:
        checks.append(check_imo_intact(
            vessel["angles_deg"], vessel["gz_m"],
            vessel.get("gm_m", 0.0),
        ))

    # 7: Freeboard
    if "freeboard_m" in vessel and "lwl_m" in vessel:
        checks.append(check_min_freeboard(
            vessel["freeboard_m"], vessel["lwl_m"],
        ))

    # 8: Beam/draft ratio
    if "beam_m" in vessel and "draft_m" in vessel:
        checks.append(check_beam_draft_ratio(
            vessel["beam_m"], vessel["draft_m"],
        ))

    # 9: Length/beam ratio
    if "lwl_m" in vessel and "beam_m" in vessel:
        checks.append(check_length_beam_ratio(
            vessel["lwl_m"], vessel["beam_m"],
        ))

    # 10: Draft/depth ratio
    if "draft_m" in vessel:
        checks.append(check_draft_depth_ratio(
            vessel["draft_m"], vessel.get("depth_m"),
        ))

    # 11: Displacement/length
    if "displacement_tonnes" in vessel and "lwl_m" in vessel:
        checks.append(check_displacement_lwl_ratio(
            vessel["displacement_tonnes"], vessel["lwl_m"],
        ))

    # 12: Section modulus (if data provided)
    if all(k in vessel for k in ("lwl_m", "beam_m", "draft_m", "cb")):
        z = vessel.get("actual_z_cm3", 999999.0)
        checks.append(check_section_modulus(
            vessel["lwl_m"], vessel["beam_m"],
            vessel["draft_m"], vessel["cb"], z,
        ))

    # 13: Propeller immersion
    if "draft_m" in vessel:
        checks.append(check_propeller_immersion(
            vessel["draft_m"], vessel.get("prop_diameter_m"),
        ))

    # 14: Trim
    if "trim_m" in vessel and "lwl_m" in vessel:
        checks.append(check_max_trim(
            vessel["trim_m"], vessel["lwl_m"],
        ))

    return {
        "overall_pass": all(c["pass"] for c in checks),
        "checks": checks,
        "total": len(checks),
        "passed": sum(1 for c in checks if c["pass"]),
        "failed": sum(1 for c in checks if not c["pass"]),
    }
