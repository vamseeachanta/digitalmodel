"""DNV-RP-B401 — Cathodic Protection Design (2005/2017).

Implements sacrificial anode CP design for offshore structures including
current demand, coating breakdown, anode mass requirements, anode
resistance for slender stand-off anodes, anode current output, and
protected length estimation per DNV-RP-F103.
"""

from __future__ import annotations

import math

# ---------------------------------------------------------------------------
# Protection potentials vs Ag/AgCl (DNV-RP-B401 §5.4.1)
# ---------------------------------------------------------------------------
PROTECTION_POTENTIAL_AGAGCL: float = -0.800  # V vs Ag/AgCl
ANODE_CLOSED_CIRCUIT_POTENTIAL: float = -1.050  # V vs Ag/AgCl (Al-Zn-In)

# ---------------------------------------------------------------------------
# Design driving voltage (DNV-RP-B401 §5.4.1)
# ---------------------------------------------------------------------------
DESIGN_DRIVING_VOLTAGE: float = abs(
    PROTECTION_POTENTIAL_AGAGCL - ANODE_CLOSED_CIRCUIT_POTENTIAL
)  # 0.25 V

# ---------------------------------------------------------------------------
# Al-Zn-In anode properties (DNV-RP-B401 Table 10-6)
# ---------------------------------------------------------------------------
ANODE_CAPACITY_ALZNI: float = 2000.0  # A-h/kg electrochemical capacity
ANODE_DENSITY_ALZNI: float = 2750.0  # kg/m³

# ---------------------------------------------------------------------------
# Utilization factors (DNV-RP-B401 Table 10-8)
# ---------------------------------------------------------------------------
UTILIZATION_FACTOR_STANDOFF: float = 0.90  # stand-off structural anodes
UTILIZATION_FACTOR_FLUSH: float = 0.85  # flush-mount hull anodes

# ---------------------------------------------------------------------------
# Steel resistivity (DNV-RP-F103 §5.6.10)
# ---------------------------------------------------------------------------
STEEL_RESISTIVITY: float = 2.0e-7  # ohm-m


def current_demand(
    surface_area_m2: float,
    current_density_A_m2: float,
    breakdown_factor: float,
) -> float:
    """Current demand for a coated structure (DNV-RP-B401 §7.4.1, Eq 1).

    I_c = A_c * i_c * f_c

    Parameters
    ----------
    surface_area_m2 : float
        Individual surface area [m²].
    current_density_A_m2 : float
        Design current density [A/m²].
    breakdown_factor : float
        Coating breakdown factor (dimensionless, 0-1).

    Returns
    -------
    float
        Current demand [A].
    """
    return surface_area_m2 * current_density_A_m2 * breakdown_factor


def anode_mass_requirement(
    I_mean_A: float,
    T_design_years: float,
    E_capacity: float = ANODE_CAPACITY_ALZNI,
    u_f: float = UTILIZATION_FACTOR_STANDOFF,
) -> float:
    """Total net anode mass requirement (DNV-RP-B401 §7.7.1, Eq 2).

    M_a = (I_cm * t_f * 8760) / (u * epsilon)

    Parameters
    ----------
    I_mean_A : float
        Mean current demand over design life [A].
    T_design_years : float
        Design life [years].
    E_capacity : float
        Electrochemical capacity [A-h/kg].
    u_f : float
        Anode utilization factor (dimensionless).

    Returns
    -------
    float
        Required net anode mass [kg].
    """
    return (I_mean_A * T_design_years * 8760.0) / (E_capacity * u_f)


def coating_breakdown_factor(
    a: float, b: float, t_years: float
) -> float:
    """Coating breakdown factor at time t (DNV-RP-B401 Table 10-4).

    f_c = a + b * t

    Parameters
    ----------
    a : float
        Initial breakdown factor constant.
    b : float
        Annual degradation rate.
    t_years : float
        Elapsed time [years].

    Returns
    -------
    float
        Coating breakdown factor at time t (dimensionless).
    """
    return a + b * t_years


def anode_resistance_slender_standoff(
    rho: float,
    L_a: float,
    r_a: float,
    proximity_factor: float = 1.0,
) -> float:
    """Anode resistance for long slender stand-off anode (DNV-RP-B401 Table 10-7).

    R_a = (rho / (2 * pi * L_a)) * (ln(4 * L_a / r_a) - 1)

    Parameters
    ----------
    rho : float
        Seawater resistivity [ohm-m].
    L_a : float
        Anode length [m].
    r_a : float
        Anode equivalent radius [m].
    proximity_factor : float
        Proximity/shielding factor applied to R_a (e.g. 1.3 for
        anode-to-structure distances of 150-300mm per DNV-RP-B401).

    Returns
    -------
    float
        Anode-to-electrolyte resistance [ohm].
    """
    R_a = (rho / (2.0 * math.pi * L_a)) * (
        math.log(4.0 * L_a / r_a) - 1.0
    )
    return R_a * proximity_factor


def anode_current_output(
    rho: float,
    L_a: float,
    r_a: float,
    delta_E: float = DESIGN_DRIVING_VOLTAGE,
    proximity_factor: float = 1.0,
) -> float:
    """Anode current output (DNV-RP-B401 §5.3.2).

    I_a = delta_E / R_a

    Parameters
    ----------
    rho : float
        Seawater resistivity [ohm-m].
    L_a : float
        Anode length [m].
    r_a : float
        Anode equivalent radius [m].
    delta_E : float
        Design driving voltage [V].
    proximity_factor : float
        Proximity/shielding factor applied to R_a.

    Returns
    -------
    float
        Anode current output [A].
    """
    R_a = anode_resistance_slender_standoff(rho, L_a, r_a, proximity_factor)
    return delta_E / R_a


def equivalent_radius_from_mass(
    net_mass_kg: float,
    L_a: float,
    density: float = ANODE_DENSITY_ALZNI,
) -> float:
    """Equivalent cylindrical radius from anode mass and length.

    Used to compute equivalent radius for trapezoidal cross-section
    anodes approximated as cylinders (DNV-RP-B401 Table 10-7 note).

    r = sqrt(m / (pi * L * rho_material))

    Parameters
    ----------
    net_mass_kg : float
        Anode net alloy mass [kg].
    L_a : float
        Anode length [m].
    density : float
        Anode material density [kg/m³].

    Returns
    -------
    float
        Equivalent radius [m].
    """
    return math.sqrt(net_mass_kg / (math.pi * L_a * density))


def number_of_anodes(
    total_mass_kg: float,
    anode_net_mass_kg: float,
) -> int:
    """Number of anodes required (rounded up).

    Parameters
    ----------
    total_mass_kg : float
        Total net anode mass requirement [kg].
    anode_net_mass_kg : float
        Net mass of one anode [kg].

    Returns
    -------
    int
        Number of anodes (rounded up to nearest integer).
    """
    return math.ceil(total_mass_kg / anode_net_mass_kg)


def protected_length(
    delta_E_me: float,
    WT: float,
    D: float,
    rho_me: float,
    f_cf: float,
    i_cm: float,
) -> float:
    """Protected length of rigid riser line pipe (DNV-RP-F103 §5.6.7, Eq 14).

    PL = sqrt((delta_E_me * WT * (D - WT)) / (rho_me * D * f_cf * i_cm))

    Parameters
    ----------
    delta_E_me : float
        Metallic voltage drop [V] (typically 0.15 V per DNV-RP-F103 §5.6.3).
    WT : float
        Wall thickness of the pipe [m].
    D : float
        Outer diameter of the pipe [m].
    rho_me : float
        Resistivity of pipe steel [ohm-m].
    f_cf : float
        Final coating breakdown factor (dimensionless).
    i_cm : float
        Design mean current density [A/m²].

    Returns
    -------
    float
        Protected length [m].
    """
    return math.sqrt(
        (delta_E_me * WT * (D - WT)) / (rho_me * D * f_cf * i_cm)
    )


def flush_anode_resistance(
    rho_ohm_cm: float,
    L_a_in: float,
    W_in: float,
    H_in: float,
    r_eq_in: float,
) -> float:
    """Anode resistance for flush-mount hull anode (McCoy method).

    Uses the simplified slender-body formula with equivalent radius
    and dimensions in inches, resistivity in ohm-cm (Imperial units
    as used in standard CP design spreadsheets).

    R_a = (rho / (2 * pi * L_cm)) * (ln(2 * L_cm / r_cm) - 0.5)

    Parameters
    ----------
    rho_ohm_cm : float
        Seawater resistivity [ohm-cm].
    L_a_in : float
        Anode length [inches].
    W_in : float
        Anode width [inches].
    H_in : float
        Anode height [inches].
    r_eq_in : float
        Anode equivalent radius [inches].

    Returns
    -------
    float
        Anode-to-electrolyte resistance [ohm].
    """
    # Convert inches to cm for McCoy/Sunde half-space formula.
    # Flush-mount anodes radiate into a half-space (mounted on hull),
    # giving a factor of 1/(pi*L) instead of 1/(2*pi*L).
    L_cm = L_a_in * 2.54
    r_cm = r_eq_in * 2.54
    return (rho_ohm_cm / (math.pi * L_cm)) * (
        math.log(2.0 * L_cm / r_cm) - 0.5
    )
