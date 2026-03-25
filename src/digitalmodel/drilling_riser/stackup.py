"""Drilling riser stackup calculations — tension, wall thickness, effective tension.

Implements tension requirements per 2H-TNE-0050-03 §3.1 and API RP 16Q §3.3,
wall thickness per Barlow's formula, and effective tension along riser string.
"""

from __future__ import annotations

# ---------------------------------------------------------------------------
# Tension safety factors (2H-TNE-0050-03 §3.1, API RP 16Q §3.3)
# ---------------------------------------------------------------------------
SAFETY_FACTOR_TENSION: float = 1.25  # [-] minimum top tension safety factor
F_WT_DEFAULT: float = 1.05  # [-] submerged weight tolerance factor (§3.1.2)
F_BT_DEFAULT: float = 0.96  # [-] buoyancy loss and tolerance factor (§3.1.2)

# ---------------------------------------------------------------------------
# Wall thickness (Barlow's formula)
# ---------------------------------------------------------------------------
SAFETY_FACTOR_BARLOW: float = 1.5  # [-] default wall thickness safety factor


def top_tension_required(
    submerged_weight_kn: float,
    dynamic_factor: float = SAFETY_FACTOR_TENSION,
) -> float:
    """Calculate minimum required top tension for drilling riser.

    Per 2H-TNE-0050-03 §3.1 and API RP 16Q §3.3.

    Parameters
    ----------
    submerged_weight_kn : float
        Effective submerged weight of riser string [kN].
    dynamic_factor : float, optional
        Dynamic amplification / safety factor [-]. Default 1.25.

    Returns
    -------
    float
        Required top tension [kN].
    """
    return submerged_weight_kn * dynamic_factor


def wall_thickness_required(
    od_mm: float,
    design_pressure_mpa: float,
    smys_mpa: float,
    safety_factor: float = SAFETY_FACTOR_BARLOW,
) -> float:
    """Calculate minimum wall thickness using Barlow's formula.

    t = (P * OD * SF) / (2 * SMYS)

    Safety factor SF >= 1 scales the required thickness upward (more conservative).
    Equivalent API design-factor form: t = p*D / (2*SMYS*DF) with DF = 1/SF.

    Parameters
    ----------
    od_mm : float
        Outer diameter [mm].
    design_pressure_mpa : float
        Design internal pressure [MPa].
    smys_mpa : float
        Specified minimum yield strength [MPa].
    safety_factor : float, optional
        Safety factor >= 1 [-]. Higher value → thicker wall. Default 1.5.

    Returns
    -------
    float
        Required wall thickness [mm].
    """
    return (design_pressure_mpa * od_mm * safety_factor) / (2.0 * smys_mpa)


def effective_tension(
    top_tension_kn: float,
    submerged_weight_kn: float,
    depth_factor: float,
) -> float:
    """Calculate effective tension at a given depth along the riser.

    T_eff = T_top - W_sub * depth_factor

    Parameters
    ----------
    top_tension_kn : float
        Applied top tension [kN].
    submerged_weight_kn : float
        Total submerged weight of riser string [kN].
    depth_factor : float
        Fraction of riser length from surface (0=top, 1=mudline) [-].

    Returns
    -------
    float
        Effective tension at depth [kN].
    """
    return top_tension_kn - submerged_weight_kn * depth_factor


def minimum_slip_ring_tension(
    submerged_weight_kn: float,
    buoyancy_uplift_kn: float,
    internal_area_m2: float,
    mud_density_kn_m3: float,
    mud_column_m: float,
    seawater_density_kn_m3: float,
    seawater_column_m: float,
    f_wt: float = F_WT_DEFAULT,
    f_bt: float = F_BT_DEFAULT,
) -> float:
    """Minimum tension at the slip ring per 2H-TNE-0050-03 Eq 3.2.

    T_SRmin = Ws * fwt - Bn * fbt + Ai * (dm * Hm - dw * Hw)

    Parameters
    ----------
    submerged_weight_kn : float
        Submerged riser weight above point of consideration [kN].
    buoyancy_uplift_kn : float
        Net lift of buoyancy material above point of consideration [kN].
    internal_area_m2 : float
        Internal cross-sectional area of riser [m2].
    mud_density_kn_m3 : float
        Drilling fluid weight density [kN/m3].
    mud_column_m : float
        Drilling fluid column height [m].
    seawater_density_kn_m3 : float
        Seawater weight density [kN/m3].
    seawater_column_m : float
        Seawater column height to point of consideration [m].
    f_wt : float, optional
        Submerged weight tolerance factor [-]. Default 1.05.
    f_bt : float, optional
        Buoyancy loss and tolerance factor [-]. Default 0.96.

    Returns
    -------
    float
        Minimum required slip ring tension [kN].
    """
    weight_term = submerged_weight_kn * f_wt
    buoyancy_term = buoyancy_uplift_kn * f_bt
    pressure_term = internal_area_m2 * (
        mud_density_kn_m3 * mud_column_m
        - seawater_density_kn_m3 * seawater_column_m
    )
    return weight_term - buoyancy_term + pressure_term
