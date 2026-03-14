# ABOUTME: Scour depth prediction per DNV-RP-F107.
# ABOUTME: Pipeline scour, monopile scour, and rock armour scour protection.
"""Scour depth prediction — DNV-RP-F107.

Implements:
- pipeline_scour_depth: equilibrium scour for pipelines (Sec 5)
- monopile_scour_depth: equilibrium scour for monopiles (Breusers/Sumer)
- rock_armour_thickness: minimum rock armour protection layer

References:
    DNV-RP-F107 "Risk Assessment of Pipeline Protection" (2019)
    Sumer & Fredsoe "The Mechanics of Scour in the Marine Environment" (2002)
"""
from dataclasses import dataclass
import math

STANDARD = "DNV-RP-F107"
KINEMATIC_VISCOSITY_WATER = 1.0e-6  # m^2/s at ~20C
GRAVITY = 9.81  # m/s^2
DEFAULT_WAVE_PERIOD_S = 8.0  # typical wave period for KC calculation


@dataclass
class PipelineScourResult:
    """Result of pipeline scour depth calculation."""

    scour_depth_m: float
    pipe_od_m: float
    keulegan_carpenter: float
    current_wave_ratio: float
    standard: str = STANDARD


@dataclass
class MonopileScourResult:
    """Result of monopile scour depth calculation."""

    scour_depth_m: float
    pile_diameter_m: float
    scour_to_diameter_ratio: float
    standard: str = STANDARD


@dataclass
class RockArmourResult:
    """Result of rock armour thickness calculation."""

    thickness_m: float
    stability_number: float
    standard: str = STANDARD


def pipeline_scour_depth(
    pipe_od_m: float,
    current_velocity_ms: float,
    wave_orbital_velocity_ms: float,
    sediment_d50_mm: float,
    burial_ratio: float = 0.0,
    wave_period_s: float = DEFAULT_WAVE_PERIOD_S,
) -> PipelineScourResult:
    """Equilibrium scour depth for a pipeline on the seabed.

    Per DNV-RP-F107 Section 5. Uses the empirical relationship
    S/D = f(KC, theta_cw) with burial reduction.

    Args:
        pipe_od_m: Pipeline outer diameter in metres.
        current_velocity_ms: Steady current velocity in m/s.
        wave_orbital_velocity_ms: Near-bed wave orbital velocity in m/s.
        sediment_d50_mm: Median grain size in mm.
        burial_ratio: Fraction of diameter buried (0.0 = exposed, 1.0 = fully buried).
        wave_period_s: Wave period in seconds (default 8 s).

    Returns:
        PipelineScourResult with scour depth and intermediate parameters.
    """
    if pipe_od_m <= 0:
        raise ValueError(f"pipe_od_m must be positive, got {pipe_od_m}")
    if not 0.0 <= burial_ratio <= 1.0:
        raise ValueError(f"burial_ratio must be in [0, 1], got {burial_ratio}")

    total_velocity = current_velocity_ms + wave_orbital_velocity_ms
    if total_velocity == 0.0:
        return PipelineScourResult(
            scour_depth_m=0.0,
            pipe_od_m=pipe_od_m,
            keulegan_carpenter=0.0,
            current_wave_ratio=0.0,
        )

    # Keulegan-Carpenter number: KC = U_w * T / D
    kc = wave_orbital_velocity_ms * wave_period_s / pipe_od_m

    # Current-to-wave velocity ratio: theta_cw = Uc / (Uc + Uw)
    theta_cw = current_velocity_ms / total_velocity

    # Base scour-to-diameter ratio per DNV-RP-F107 empirical curves.
    # Current-dominated (theta_cw > 0.5): S/D ~ 0.6
    # Wave-dominated (theta_cw < 0.5): S/D depends on KC
    if theta_cw >= 0.5:
        # Current-dominated regime — empirical plateau at ~0.6
        s_over_d = 0.6
    else:
        # Wave-dominated regime — increases with KC up to ~0.6
        # For KC < 6: onset region; for KC > 6: live-bed regime
        if kc < 1.0:
            s_over_d = 0.0
        elif kc < 6.0:
            s_over_d = 0.1 * kc
        else:
            s_over_d = 0.6

    # Burial reduction factor: linear reduction
    burial_factor = 1.0 - burial_ratio

    scour_depth = s_over_d * pipe_od_m * burial_factor

    return PipelineScourResult(
        scour_depth_m=scour_depth,
        pipe_od_m=pipe_od_m,
        keulegan_carpenter=kc,
        current_wave_ratio=theta_cw,
    )


def monopile_scour_depth(
    pile_diameter_m: float,
    current_velocity_ms: float,
    water_depth_m: float,
) -> MonopileScourResult:
    """Equilibrium scour depth around a monopile.

    Uses Breusers/Sumer empirical formula per DNV-RP-F107.
    For live-bed conditions: S/D = 1.3.
    Scales linearly with velocity ratio below critical velocity.

    Args:
        pile_diameter_m: Monopile diameter in metres.
        current_velocity_ms: Depth-averaged current velocity in m/s.
        water_depth_m: Water depth in metres.

    Returns:
        MonopileScourResult with scour depth and ratio.
    """
    if pile_diameter_m <= 0:
        raise ValueError(
            f"pile_diameter_m must be positive, got {pile_diameter_m}"
        )
    if water_depth_m <= 0:
        raise ValueError(
            f"water_depth_m must be positive, got {water_depth_m}"
        )

    if current_velocity_ms == 0.0:
        return MonopileScourResult(
            scour_depth_m=0.0,
            pile_diameter_m=pile_diameter_m,
            scour_to_diameter_ratio=0.0,
        )

    # Critical velocity for sediment motion (Shields-based estimate).
    # Simplified: U_cr ~ 0.3 m/s for medium sand is typical.
    # Using logarithmic profile approximation:
    u_cr = 0.3  # m/s — conservative for medium sand

    # Live-bed equilibrium: S/D = 1.3 (Breusers et al. 1977, Sumer 2002)
    s_over_d_max = 1.3

    if current_velocity_ms >= u_cr:
        # Live-bed scour — full empirical ratio
        s_over_d = s_over_d_max
    else:
        # Clear-water regime — linear ramp from 0 to 1.3
        s_over_d = s_over_d_max * (current_velocity_ms / u_cr)

    scour_depth = s_over_d * pile_diameter_m

    return MonopileScourResult(
        scour_depth_m=scour_depth,
        pile_diameter_m=pile_diameter_m,
        scour_to_diameter_ratio=s_over_d,
    )


def rock_armour_thickness(
    current_velocity_ms: float,
    rock_density_kg_m3: float,
    water_density_kg_m3: float,
    rock_d50_m: float,
) -> RockArmourResult:
    """Minimum rock armour layer thickness for scour protection.

    Per DNV-RP-F107 guidance. Stability assessed via Izbash-type
    stability number, minimum thickness = 2 * d50 (two stone layers).

    Args:
        current_velocity_ms: Design current velocity in m/s.
        rock_density_kg_m3: Rock density in kg/m3 (typically 2650).
        water_density_kg_m3: Seawater density in kg/m3 (typically 1025).
        rock_d50_m: Median rock diameter in metres.

    Returns:
        RockArmourResult with thickness and stability number.
    """
    if rock_d50_m <= 0:
        raise ValueError(f"rock_d50_m must be positive, got {rock_d50_m}")
    if rock_density_kg_m3 <= water_density_kg_m3:
        raise ValueError("rock_density must exceed water_density")

    # Relative submerged density
    delta = (rock_density_kg_m3 / water_density_kg_m3) - 1.0

    # Stability number: Ns = U / sqrt(g * d50 * delta)
    stability_number = current_velocity_ms / math.sqrt(
        GRAVITY * rock_d50_m * delta
    )

    # Minimum armour thickness: 2 stone layers
    thickness = 2.0 * rock_d50_m

    return RockArmourResult(
        thickness_m=thickness,
        stability_number=stability_number,
    )
