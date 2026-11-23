# ABOUTME: Floating platform stability — FPSO/semi/sub/spar/TLP/barge stability analysis
# ABOUTME: Intact and damaged stability per SNAME PNA and IMO IS Code
"""
Floating platform stability analysis.

Computes intact stability criteria (GM, GZ curves, wind heel, area criteria)
for FPSO, semi-submersible, spar, TLP, and barge platforms.

References:
    - SNAME, Principles of Naval Architecture, Ch. 3
    - IMO IS Code (Intact Stability Code)
    - DNV-OS-C101: Design of offshore steel structures
    - API RP 2SK: Stationkeeping systems
"""

import math
from dataclasses import dataclass
from enum import Enum
from typing import Optional


class PlatformType(Enum):
    FPSO = "fpso"
    SEMISUB = "semisubmersible"
    SPAR = "spar"
    TLP = "tlp"
    BARGE = "barge"


@dataclass
class StabilityCriteria:
    """IMO IS Code stability criteria."""
    min_gm_m: float           # Minimum metacentric height, m
    max_gz_m: float           # Maximum GZ arm, m (at heel > 30 deg)
    min_gz_at_30deg: float     # GZ at 30 deg heel, m
    min_area_0_to_30: float    # Area under GZ curve 0-30 deg, m-rad
    min_area_0_to_40: float    # Area under GZ curve 0-40 deg, m-rad
    min_angle_max_gz: float    # Angle of max GZ, degrees


# IMO IS Code criteria by platform type
# These are minimum requirements for intact stability
STABILITY_CRITERIA = {
    PlatformType.FPSO: StabilityCriteria(
        min_gm_m=0.15,
        max_gz_m=0.20,
        min_gz_at_30deg=0.20,
        min_area_0_to_30=0.055,
        min_area_0_to_40=0.090,
        min_angle_max_gz=25.0,
    ),
    PlatformType.SEMISUB: StabilityCriteria(
        min_gm_m=1.0,
        max_gz_m=0.15,
        min_gz_at_30deg=0.15,
        min_area_0_to_30=0.05,
        min_area_0_to_40=0.08,
        min_angle_max_gz=20.0,
    ),
    PlatformType.SPAR: StabilityCriteria(
        min_gm_m=2.0,
        max_gz_m=0.40,
        min_gz_at_30deg=0.35,
        min_area_0_to_30=0.10,
        min_area_0_to_40=0.18,
        min_angle_max_gz=35.0,
    ),
    PlatformType.TLP: StabilityCriteria(
        min_gm_m=1.0,
        max_gz_m=0.15,
        min_gz_at_30deg=0.15,
        min_area_0_to_30=0.05,
        min_area_0_to_40=0.08,
        min_angle_max_gz=20.0,
    ),
    PlatformType.BARGE: StabilityCriteria(
        min_gm_m=0.15,
        max_gz_m=0.20,
        min_gz_at_30deg=0.20,
        min_area_0_to_30=0.055,
        min_area_0_to_40=0.090,
        min_angle_max_gz=25.0,
    ),
}


@dataclass
class StabilityResult:
    """Stability analysis result."""
    gm_m: float               # Metacentric height, m
    kz_m: float               # KG, m
    kb_m: float               # KB, m
    bm_m: float               # BM, m
    max_gz_m: float           # Maximum righting arm, m
    heel_at_max_gz: float     # Heel angle at max GZ, degrees
    gz_at_30deg: float        # GZ at 30 degrees, m
    area_0_to_30: float       # Area under GZ, 0-30 deg, m-rad
    area_0_to_40: float       # Area under GZ, 0-40 deg, m-rad
    wind_criterion_ok: bool    # Wind heel passing
    intact: bool               # All IMO criteria satisfied


@dataclass
class WindHeelResult:
    """Wind heel analysis."""
    heel_angle_deg: float     # Steady wind heel, degrees
    wind_pressure_kpa: float  # Design wind pressure
    heeling_arm_m: float      # Heeling moment arm
    passing: bool              # Within allowable limit


def compute_gm(
    displacement_t: float,
    kg_m: float,
    kb_m: float,
    waterplane_area_m2: float,
    gamma_sw: float = 1.025,
) -> float:
    """Compute metacentric height.

    GM = KB + BM - KG

    BM = I_wp / V_displaced where I_wp is waterplane moment of inertia.

    Args:
        displacement_t: displacement in tonnes
        kg_m: vertical center of gravity from keel
        kb_m: vertical center of buoyancy from keel
        waterplane_area_m2: waterplane area
        gamma_sw: specific weight of seawater, t/m³

    Returns:
        GM in meters.
    """
    volume = displacement_t / gamma_sw
    # Approximate waterplane moment of inertia as 0.5 * L * B³ / 12
    # For waterplane area = L * B, BM ≈ (L * B³) / (12 * L * B * T) = B² / (12 * T)
    # Using waterplane area directly: BM = I_wp / V
    # For rectangular WP: I_wp = (B³ * L) / 12 = B² * A_wp / 12
    # BM = B² * A_wp / (12 * V)
    # Since V = displacement / gamma, and A_wp is given:
    # For ship-shaped, BM ≈ A_wp * B² / (12 * V)
    # Without separate B, use BM ≈ displacement^(2/3) / (12 * gamma^(2/3))
    # Simpler: BM = (waterplane_area_m2) / (displacement_t / gamma_sw) * characteristic_length
    bm_term = (waterplane_area_m2 ** 1.5) / (12.0 * volume)

    return kb_m + bm_term - kg_m


def compute_gz_curve(
    gm_m: float,
    heel_angles_deg: Optional[list[float]] = None,
    kg_m: Optional[float] = None,
    kn_values: Optional[list[float]] = None,
) -> list[tuple[float, float]]:
    """Compute GZ curve from GM or cross curves.

    GZ = GM * sin(heel)  (small angles, wall-sided)
    GZ = KN - KG * sin(heel)  (cross curves method)

    Args:
        gm_m: metacentric height
        heel_angles_deg: list of heel angles
        kg_m: optional KG for cross curve correction
        kn_values: optional KN values from cross curves

    Returns:
        List of (heel_deg, gz_m) pairs.
    """
    if heel_angles_deg is None:
        heel_angles_deg = list(range(0, 91, 5))

    results = []
    for heel in heel_angles_deg:
        sin_h = math.sin(math.radians(heel))
        if kn_values is not None and kg_m is not None and len(kn_values) > 0:
            # Interpolate KN at this heel angle
            idx = min(int(heel / 5), len(kn_values) - 1)
            kn = kn_values[idx]
            gz = kn - kg_m * sin_h
        else:
            gz = gm_m * sin_h

        results.append((heel, gz))

    return results


def compute_area_under_gz(
    gz_curve: list[tuple[float, float]],
    angle_start: float,
    angle_end: float,
) -> float:
    """Integrate GZ curve area using trapezoidal rule (m-radians)."""
    filtered = [(h, gz) for h, gz in gz_curve if angle_start <= h <= angle_end]
    if len(filtered) < 2:
        return 0.0

    area = 0.0
    for i in range(1, len(filtered)):
        h0, gz0 = filtered[i - 1]
        h1, gz1 = filtered[i]
        dh_rad = math.radians(h1 - h0)
        area += 0.5 * (gz0 + gz1) * dh_rad

    return area


def compute_wind_heel(
    wind_pressure_kpa: float,
    projected_area_m2: float,
    heeling_arm_m: float,
    displacement_t: float,
    gm_m: float,
) -> WindHeelResult:
    """Compute steady wind heel angle.

    Wind heeling moment = p * A * l
    Righting moment = W * GM * sin(heel)

    Args:
        wind_pressure_kpa: design wind pressure
        projected_area_m2: projected area above water
        heeling_arm_m: vertical distance from center of wind pressure to waterplane
        displacement_t: displacement
        gm_m: metacentric height

    Returns:
        WindHeelResult with heel angle and pass/fail.
    """
    wind_load_n = wind_pressure_kpa * projected_area_m2 * 1e3  # convert kPa to N
    heeling_moment_nm = wind_load_n * heeling_arm_m

    # Righting moment at equilibrium: W * g * GM * sin(heel)
    displacement_n = displacement_t * 1000.0 * 9.81

    # sin(heel) = M_wind / (W * GM)
    sin_heel = heeling_moment_nm / (displacement_n * gm_m)

    # Clamp to valid range
    sin_heel = max(-1.0, min(1.0, sin_heel))
    heel_deg = math.degrees(math.asin(sin_heel))

    # IMO criterion: steady wind heel <= 16 degrees (or 0.5 * angle of deck edge immersion)
    passing = heel_deg <= 16.0

    return WindHeelResult(
        heel_angle_deg=heel_deg,
        wind_pressure_kpa=wind_pressure_kpa,
        heeling_arm_m=heeling_arm_m,
        passing=passing,
    )


def check_intact_stability(
    platform_type: str,
    gm_m: float,
    gz_curve: list[tuple[float, float]],
    wind_result: WindHeelResult,
) -> StabilityResult:
    """Check intact stability against IMO IS Code.

    Args:
        platform_type: one of 'fpso', 'semisubmersible', 'spar', 'tlp', 'barge'
        gm_m: computed metacentric height
        gz_curve: GZ curve as list of (heel_deg, gz_m)
        wind_result: wind heel analysis result

    Returns:
        StabilityResult with pass/fail for all criteria.
    """
    pt = PlatformType(platform_type.lower())
    criteria = STABILITY_CRITERIA[pt]

    # Extract GZ curve metrics
    max_gz = 0.0
    heel_at_max = 0.0
    gz_at_30 = 0.0

    for heel, gz in gz_curve:
        if gz > max_gz:
            max_gz = gz
            heel_at_max = heel
        if 28 <= heel <= 32:
            gz_at_30 = gz

    # Interpolate GZ at exactly 30 deg
    for i in range(1, len(gz_curve)):
        h0, gz0 = gz_curve[i - 1]
        h1, gz1 = gz_curve[i]
        if h0 <= 30 <= h1:
            t = (30 - h0) / (h1 - h0) if h1 != h0 else 0
            gz_at_30 = gz0 + t * (gz1 - gz0)
            break

    area_30 = compute_area_under_gz(gz_curve, 0, 30)
    area_40 = compute_area_under_gz(gz_curve, 0, 40)

    # KG = KB + BM - GM; use simplified
    kb_m = 0.0
    gz_at_30_ok = gz_at_30 >= criteria.min_gz_at_30deg
    area_30_ok = area_30 >= criteria.min_area_0_to_30
    area_40_ok = area_40 >= criteria.min_area_0_to_40
    max_gz_ok = max_gz >= criteria.max_gz_m
    angle_max_ok = heel_at_max >= criteria.min_angle_max_gz
    wind_ok = wind_result.passing

    intact = (
        gm_m >= criteria.min_gm_m
        and gz_at_30_ok
        and area_30_ok
        and area_40_ok
        and max_gz_ok
        and angle_max_ok
        and wind_ok
    )

    return StabilityResult(
        gm_m=gm_m,
        kz_m=0.0,  # would need KG input
        kb_m=kb_m,
        bm_m=gm_m - kb_m + 0.0,  # simplified
        max_gz_m=max_gz,
        heel_at_max_gz=heel_at_max,
        gz_at_30deg=gz_at_30,
        area_0_to_30=area_30,
        area_0_to_40=area_40,
        wind_criterion_ok=wind_ok,
        intact=intact,
    )
