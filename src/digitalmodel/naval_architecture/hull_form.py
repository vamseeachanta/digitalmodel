# ABOUTME: Hull form parametric design — form coefficients and relationships
# ABOUTME: Block, prismatic, midship, waterplane coefficients + Series 60
"""
Hull form parametric design calculations.

Covers form coefficients (Cb, Cp, Cm, Cwp), their interrelationships,
and Series 60 hull form regression data.

References:
- USNA EN400 Chapter 2 — Hull form geometry
- PNA Vol I — Hull form coefficients
- Todd, Series 60 — Methodical Experiments (DTMB Report 1712)
"""

import math

# Seawater properties (15°C)
RHO_SW = 1025.0  # kg/m³


def block_coefficient(
    displacement_m3: float,
    lwl_m: float,
    beam_m: float,
    draft_m: float,
) -> float:
    """Block coefficient Cb = V / (L * B * T).

    Measures fullness of underwater hull relative to bounding box.
    Typical: 0.35 (destroyer) to 0.85 (tanker).
    """
    box_vol = lwl_m * beam_m * draft_m
    if box_vol <= 0:
        raise ValueError("Dimensions must be positive")
    return displacement_m3 / box_vol


def prismatic_coefficient(cb: float, cm: float) -> float:
    """Prismatic coefficient Cp = Cb / Cm.

    Measures longitudinal distribution of displacement.
    """
    if cm <= 0:
        raise ValueError("Midship coefficient must be positive")
    return cb / cm


def midship_coefficient(
    midship_area_m2: float,
    beam_m: float,
    draft_m: float,
) -> float:
    """Midship section coefficient Cm = Am / (B * T)."""
    if beam_m <= 0 or draft_m <= 0:
        raise ValueError("Beam and draft must be positive")
    return midship_area_m2 / (beam_m * draft_m)


def waterplane_coefficient(
    waterplane_area_m2: float,
    lwl_m: float,
    beam_m: float,
) -> float:
    """Waterplane area coefficient Cwp = Awp / (L * B)."""
    if lwl_m <= 0 or beam_m <= 0:
        raise ValueError("Length and beam must be positive")
    return waterplane_area_m2 / (lwl_m * beam_m)


def displacement_from_cb(
    cb: float,
    lwl_m: float,
    beam_m: float,
    draft_m: float,
    rho: float = RHO_SW,
) -> float:
    """Displacement in tonnes from block coefficient.

    Delta = Cb * L * B * T * rho / 1000
    """
    return cb * lwl_m * beam_m * draft_m * rho / 1000.0


def froude_number(speed_ms: float, lwl_m: float) -> float:
    """Froude number Fn = V / sqrt(g * L)."""
    return speed_ms / math.sqrt(9.81 * lwl_m)


def wetted_surface_denny_mumford(
    displacement_m3: float,
    lwl_m: float,
) -> float:
    """Wetted surface approximation (Denny-Mumford formula).

    S = 1.7 * L * T + V/T  (simplified)
    More practical: S ≈ 2.6 * sqrt(displacement * L)
    """
    return 2.6 * math.sqrt(displacement_m3 * lwl_m)


def series_60_cr(cb: float, fn: float) -> float:
    """Series 60 residuary resistance coefficient (simplified).

    Approximate regression for Series 60 parent forms.
    Valid for 0.60 ≤ Cb ≤ 0.80 and 0.15 ≤ Fn ≤ 0.32.

    Returns Cr × 1000 (dimensionless × 10³).
    """
    if not 0.55 <= cb <= 0.85:
        raise ValueError(f"Cb={cb} outside Series 60 range (0.55-0.85)")
    if not 0.10 <= fn <= 0.35:
        raise ValueError(f"Fn={fn} outside valid range (0.10-0.35)")

    # Simplified quadratic regression from Series 60 data
    # Cr increases with both Cb and Fn
    cr = (0.5 + 3.5 * (cb - 0.60) + 8.0 * fn**2
          + 12.0 * (cb - 0.60) * fn**2)
    return max(cr, 0.0)


def lcb_from_cb(cb: float) -> float:
    """Approximate LCB position (% LWL fwd of midship) from Cb.

    Empirical: LCB ≈ -13.5 + 19.4 * Cb (Harvald approximation).
    Negative = aft of midship.
    """
    return -13.5 + 19.4 * cb
