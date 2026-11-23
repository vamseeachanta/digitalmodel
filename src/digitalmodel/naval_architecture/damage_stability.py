# ABOUTME: Damage stability and IMO intact stability criteria checks
# ABOUTME: Lost buoyancy method, GZ curve analysis, IS Code 2008
"""
Damage stability calculations and IMO intact stability criteria.

Covers:
- IMO IS Code 2008 (MSC.267(85)) intact stability checks
- Lost buoyancy method for flooding
- GZ curve area integration and interpolation

References:
- IMO Resolution MSC.267(85) — International Code on Intact Stability
- USNA EN400 Chapter 4 — Ship Stability
"""

import math

# IMO IS Code 2008 intact stability thresholds
IMO_AREA_30_MIN = 0.055  # m·rad — area under GZ to 30°
IMO_AREA_40_MIN = 0.09  # m·rad — area under GZ to 40°
IMO_AREA_30_40_MIN = 0.03  # m·rad — area between 30° and 40°
IMO_GZ_30_MIN = 0.20  # m — minimum GZ at 30°
IMO_MAX_GZ_ANGLE_MIN = 25.0  # deg — angle of max GZ must exceed
IMO_GM_MIN = 0.15  # m — minimum initial GM


def interpolate_gz(
    angles_deg: list[float],
    gz_values: list[float],
    target_deg: float,
) -> float:
    """Linearly interpolate GZ at a target angle from tabulated data."""
    if target_deg <= angles_deg[0]:
        return gz_values[0]
    if target_deg >= angles_deg[-1]:
        return gz_values[-1]

    for i in range(len(angles_deg) - 1):
        if angles_deg[i] <= target_deg <= angles_deg[i + 1]:
            frac = (target_deg - angles_deg[i]) / (
                angles_deg[i + 1] - angles_deg[i]
            )
            return gz_values[i] + frac * (gz_values[i + 1] - gz_values[i])

    return gz_values[-1]


def gz_area_under_curve(
    angles_deg: list[float],
    gz_values: list[float],
    limit_deg: float,
) -> float:
    """Integrate GZ curve from 0 to limit_deg using trapezoidal rule.

    Returns area in m·rad (angles converted to radians for integration).
    """
    area = 0.0
    for i in range(len(angles_deg) - 1):
        a0 = angles_deg[i]
        a1 = angles_deg[i + 1]

        if a0 >= limit_deg:
            break

        if a1 > limit_deg:
            gz1 = interpolate_gz(angles_deg, gz_values, limit_deg)
            a1 = limit_deg
        else:
            gz1 = gz_values[i + 1]

        gz0 = gz_values[i]
        da_rad = math.radians(a1 - a0)
        area += 0.5 * (gz0 + gz1) * da_rad

    return area


def angle_of_max_gz(
    angles_deg: list[float],
    gz_values: list[float],
) -> float:
    """Return the angle (degrees) at which GZ is maximum."""
    max_idx = gz_values.index(max(gz_values))
    return angles_deg[max_idx]


def check_imo_intact_stability(
    angles_deg: list[float],
    gz_values: list[float],
    gm_m: float,
) -> dict:
    """Check all 6 IMO IS Code 2008 intact stability criteria.

    Returns dict with overall_pass and per-criterion results.
    """
    area_30 = gz_area_under_curve(angles_deg, gz_values, 30.0)
    area_40 = gz_area_under_curve(angles_deg, gz_values, 40.0)
    gz_at_30 = interpolate_gz(angles_deg, gz_values, 30.0)
    max_gz_angle = angle_of_max_gz(angles_deg, gz_values)

    criteria = [
        {
            "name": "Area under GZ to 30°",
            "value": round(area_30, 4),
            "required": IMO_AREA_30_MIN,
            "unit": "m·rad",
            "pass": area_30 >= IMO_AREA_30_MIN,
        },
        {
            "name": "Area under GZ to 40°",
            "value": round(area_40, 4),
            "required": IMO_AREA_40_MIN,
            "unit": "m·rad",
            "pass": area_40 >= IMO_AREA_40_MIN,
        },
        {
            "name": "Area 30° to 40°",
            "value": round(area_40 - area_30, 4),
            "required": IMO_AREA_30_40_MIN,
            "unit": "m·rad",
            "pass": (area_40 - area_30) >= IMO_AREA_30_40_MIN,
        },
        {
            "name": "GZ at 30°",
            "value": round(gz_at_30, 3),
            "required": IMO_GZ_30_MIN,
            "unit": "m",
            "pass": gz_at_30 >= IMO_GZ_30_MIN,
        },
        {
            "name": "Angle of max GZ",
            "value": max_gz_angle,
            "required": IMO_MAX_GZ_ANGLE_MIN,
            "unit": "deg",
            "pass": max_gz_angle >= IMO_MAX_GZ_ANGLE_MIN,
        },
        {
            "name": "Initial GM",
            "value": gm_m,
            "required": IMO_GM_MIN,
            "unit": "m",
            "pass": gm_m >= IMO_GM_MIN,
        },
    ]

    return {
        "overall_pass": all(c["pass"] for c in criteria),
        "criteria": criteria,
    }


def lost_buoyancy_sinkage(
    compartment_volume_m3: float,
    permeability: float,
    waterplane_area_m2: float,
) -> float:
    """Parallel sinkage from flooding using lost buoyancy method.

    sinkage = mu * v / Awp

    Args:
        compartment_volume_m3: volume of flooded compartment
        permeability: fraction of volume flooded (0.0-1.0)
        waterplane_area_m2: intact waterplane area
    """
    if not 0.0 <= permeability <= 1.0:
        raise ValueError(
            f"permeability must be 0.0-1.0, got {permeability}"
        )
    return permeability * compartment_volume_m3 / waterplane_area_m2


def flooded_gm(
    intact_gm_m: float,
    lost_waterplane_inertia_m4: float,
    displacement_tonnes: float,
) -> float:
    """GM after flooding — accounts for lost waterplane inertia.

    GM_damaged = GM_intact - I_lost / displacement
    """
    return intact_gm_m - lost_waterplane_inertia_m4 / displacement_tonnes
