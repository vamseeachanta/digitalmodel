# ABOUTME: Hull hydrostatic properties — capacity plans, stability curves, weight estimates
# ABOUTME: Wraps ship_data.estimate_vessel_hydrostatics with higher-level engineering outputs
"""
Hull properties module — hydrostatic curves, capacity estimates, weight groups.

Provides higher-level hull property calculations for registered fleet vessels
and arbitrary principal dimensions.  Builds on the hydrostatic estimates in
``ship_data.estimate_vessel_hydrostatics`` and the curves of form interpolator
in ``curves_of_form``.

Issue #1973 — integrate worldenergydata vessel fleet hull models.

Usage
-----
>>> from digitalmodel.naval_architecture.hull_properties import hull_hydrostatics
>>> props = hull_hydrostatics("THIALF")
>>> print(f"GM = {props['gm_ft']:.1f} ft, Cb = {props['cb']:.3f}")
"""

from __future__ import annotations

import math
from typing import Any, Optional

_GAMMA_SW_LB_FT3 = 64.0  # seawater specific weight
_LT_TO_LB = 2240.0
_FT_TO_M = 0.3048


def hull_hydrostatics(hull_id: str) -> Optional[dict[str, Any]]:
    """Return hydrostatic properties for a registered vessel.

    Looks up the vessel in the ship registry, then estimates hydrostatic
    coefficients.  Returns ``None`` if the vessel is not registered or
    lacks sufficient dimensions.

    Keys returned:
        hull_id, cb, kb_ft, bm_ft, kg_ft, gm_ft, waterplane_area_ft2,
        displacement_lt (if available), submerged_volume_ft3 (if displacement known).
    """
    from digitalmodel.naval_architecture.ship_data import (
        get_ship,
        estimate_vessel_hydrostatics,
    )

    ship = get_ship(hull_id)
    if ship is None:
        return None

    hydro = estimate_vessel_hydrostatics(ship)
    if hydro is None:
        return None

    result: dict[str, Any] = {"hull_id": hull_id, **hydro}

    disp = ship.get("displacement_lt")
    if disp is not None and float(disp) > 0:
        result["displacement_lt"] = float(disp)
        result["submerged_volume_ft3"] = float(disp) * _LT_TO_LB / _GAMMA_SW_LB_FT3

    return result


def capacity_plan(
    loa_ft: float,
    beam_ft: float,
    draft_ft: float,
    cb: float = 0.65,
    *,
    cw: float = 0.72,
    cm: float = 0.97,
    frame_spacing_ft: float = 2.5,
) -> dict[str, float]:
    """Estimate hull capacity plan (tank volumes, structural weight).

    Provides order-of-magnitude estimates suitable for concept-level
    field development screening.  Not a substitute for a detailed
    capacity plan from a shipyard.

    Parameters
    ----------
    loa_ft : float
        Length overall in feet.
    beam_ft : float
        Beam (breadth) in feet.
    draft_ft : float
        Design draft in feet.
    cb : float
        Block coefficient (default 0.65).
    cw : float
        Waterplane coefficient (default 0.72).
    cm : float
        Midship section coefficient (default 0.97).
    frame_spacing_ft : float
        Frame spacing in feet (default 2.5 ft).

    Returns
    -------
    dict with keys:
        moulded_volume_ft3, waterplane_area_ft2, midship_area_ft2,
        cargo_volume_ft3 (estimated usable), steel_weight_lt (estimated).
    """
    if loa_ft <= 0 or beam_ft <= 0 or draft_ft <= 0:
        raise ValueError("All dimensions must be positive")
    if not (0.0 < cb <= 1.0):
        raise ValueError(f"Block coefficient cb={cb} must be in (0, 1]")

    moulded_vol = cb * loa_ft * beam_ft * draft_ft
    awp = cw * loa_ft * beam_ft
    am = cm * beam_ft * draft_ft

    # Cargo volume is roughly 60-70% of moulded volume after structure/machinery
    cargo_vol = moulded_vol * 0.65

    # Lightweight steel estimate: Watson & Gilfillan empirical method
    # Wst ≈ K * (Cb * L * B * D)^(1.36) / 2240 for long tons
    # K depends on ship type (~0.031 for tankers, 0.036 for bulkers)
    # Simplified: steel weight ≈ 20-25% of displacement
    displacement_lb = moulded_vol * _GAMMA_SW_LB_FT3
    steel_weight_lt = (displacement_lb * 0.22) / _LT_TO_LB

    return {
        "moulded_volume_ft3": moulded_vol,
        "waterplane_area_ft2": awp,
        "midship_area_ft2": am,
        "cargo_volume_ft3": cargo_vol,
        "steel_weight_lt": steel_weight_lt,
    }


def stability_curve_estimate(
    gm_ft: float,
    beam_ft: float,
    *,
    max_angle_deg: float = 80.0,
    step_deg: float = 5.0,
) -> list[dict[str, float]]:
    """Estimate a simplified GZ curve from GM and beam.

    Uses the sine approximation: GZ = GM * sin(theta) for small angles
    and GM * sin(theta) * (1 - 0.5 * sin^2(theta)) correction for
    larger angles (approximate wall-sided formula).

    Returns a list of {angle_deg, gz_ft} pairs.
    """
    if gm_ft <= 0:
        raise ValueError(f"GM must be positive, got {gm_ft}")
    if beam_ft <= 0:
        raise ValueError(f"Beam must be positive, got {beam_ft}")

    curve: list[dict[str, float]] = []
    angle = 0.0
    while angle <= max_angle_deg + 0.001:
        theta_rad = math.radians(angle)
        sin_t = math.sin(theta_rad)
        # Wall-sided formula: GZ = sin(theta) * (GM + 0.5 * BM * tan^2(theta))
        # Simplified: GZ = GM * sin(theta) * (1 - 0.5 * sin^2(theta))
        # This captures the roll-off at large angles
        gz = gm_ft * sin_t * (1.0 - 0.5 * sin_t * sin_t)
        curve.append({"angle_deg": round(angle, 1), "gz_ft": round(gz, 4)})
        angle += step_deg

    return curve


def hull_weight_groups(
    displacement_lt: float,
    *,
    vessel_type: str = "general",
) -> dict[str, float]:
    """Estimate weight group breakdown from displacement.

    Returns approximate weight in long tons for each group per
    SWBS (Ship Work Breakdown Structure) categories.

    This is a rough empirical split suitable for early-stage studies.
    """
    if displacement_lt <= 0:
        raise ValueError("Displacement must be positive")

    # Typical weight fractions (vary by vessel type)
    fractions = {
        "crane_vessel":        {"hull": 0.35, "outfit": 0.10, "machinery": 0.08, "payload": 0.40, "margin": 0.07},
        "fpso":                {"hull": 0.25, "outfit": 0.12, "machinery": 0.10, "payload": 0.45, "margin": 0.08},
        "drillship":           {"hull": 0.30, "outfit": 0.12, "machinery": 0.12, "payload": 0.38, "margin": 0.08},
        "semi_submersible":    {"hull": 0.40, "outfit": 0.08, "machinery": 0.07, "payload": 0.38, "margin": 0.07},
        "general":             {"hull": 0.30, "outfit": 0.10, "machinery": 0.10, "payload": 0.42, "margin": 0.08},
    }

    vtype = vessel_type.lower()
    splits = fractions.get(vtype, fractions["general"])

    return {
        f"{group}_lt": round(displacement_lt * frac, 1)
        for group, frac in splits.items()
    }
