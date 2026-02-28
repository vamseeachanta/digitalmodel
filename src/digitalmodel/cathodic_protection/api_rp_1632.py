"""API RP 1632 — Cathodic Protection of Underground Petroleum Storage Tanks
and Piping Systems (3rd Edition, 1996).

Implements galvanic anode CP design for underground steel structures
including protection criteria, Dwight resistance, current demand,
anode sizing, and life calculations.
"""

from __future__ import annotations

import math

# ---------------------------------------------------------------------------
# Protection criteria (API RP 1632 §4.2)
# ---------------------------------------------------------------------------
PROTECTION_POTENTIAL_CSE: float = -0.850  # V vs CSE (§4.2.1)
POLARIZATION_SHIFT_MIN: float = 0.100  # V minimum polarization shift (§4.2.2)

# ---------------------------------------------------------------------------
# Anode open-circuit potentials vs CSE (§6.3)
# ---------------------------------------------------------------------------
ANODE_OC_POTENTIAL: dict[str, float] = {
    "zinc": -1.10,  # ASTM B418
    "magnesium_h1": -1.55,  # H-1 alloy
    "aluminum": -1.10,  # seawater reference
}

# ---------------------------------------------------------------------------
# Electrochemical capacity [A-h/kg] (§6.4)
# ---------------------------------------------------------------------------
ANODE_CAPACITY: dict[str, float] = {
    "zinc": 780.0,  # ASTM B418 Type II
    "magnesium_h1": 500.0,  # practical efficiency ~45%
    "aluminum": 2000.0,  # theoretical
}

# ---------------------------------------------------------------------------
# Typical current density for underground steel [mA/m²] (§5)
# ---------------------------------------------------------------------------
CURRENT_DENSITY_BARE: float = 21.5  # midpoint of 10.76–32.3 mA/m²
CURRENT_DENSITY_COATED: float = 0.3  # well-coated


def anode_driving_voltage(anode_type: str) -> float:
    """Driving voltage between anode and protected structure (§6.3).

    E_drive = |E_anode_oc - E_protected|

    Parameters
    ----------
    anode_type : str
        Key into ANODE_OC_POTENTIAL (e.g. "zinc", "magnesium_h1").

    Returns
    -------
    float
        Driving voltage magnitude [V].
    """
    e_anode = ANODE_OC_POTENTIAL[anode_type]
    return abs(e_anode - PROTECTION_POTENTIAL_CSE)


def anode_resistance_vertical_rod(rho: float, L: float, d: float) -> float:
    """Dwight equation for a vertical rod anode (§6.3).

    R_a = (rho / (2 * pi * L)) * (ln(8*L/d) - 1)

    Parameters
    ----------
    rho : float
        Soil resistivity [ohm-m].
    L : float
        Anode length [m].
    d : float
        Anode diameter [m].

    Returns
    -------
    float
        Anode-to-electrolyte resistance [ohm].
    """
    return (rho / (2.0 * math.pi * L)) * (math.log(8.0 * L / d) - 1.0)


def current_demand(surface_area_m2: float, current_density_mA_m2: float) -> float:
    """Total protection current required (§5).

    I = surface_area * current_density

    Parameters
    ----------
    surface_area_m2 : float
        Bare or effective surface area to protect [m²].
    current_density_mA_m2 : float
        Design current density [mA/m²].

    Returns
    -------
    float
        Required current [A].
    """
    return surface_area_m2 * current_density_mA_m2 / 1000.0


def current_per_anode(
    anode_type: str, rho: float, L: float, d: float
) -> float:
    """Current output per single anode (§6.3).

    Assumes electrolyte resistance R_e approx R_a for a single remote anode:
    I_a = E_drive / (2 * R_a)

    Parameters
    ----------
    anode_type : str
        Anode material key.
    rho : float
        Soil resistivity [ohm-m].
    L : float
        Anode length [m].
    d : float
        Anode diameter [m].

    Returns
    -------
    float
        Output current per anode [A].
    """
    e_drive = anode_driving_voltage(anode_type)
    r_a = anode_resistance_vertical_rod(rho, L, d)
    return e_drive / (2.0 * r_a)


def number_of_anodes(
    surface_area_m2: float,
    current_density_mA_m2: float,
    anode_type: str,
    rho: float,
    L: float,
    d: float,
) -> int:
    """Number of anodes to meet protection current demand (§6.3).

    N = ceil(I_demand / I_per_anode)

    Parameters
    ----------
    surface_area_m2 : float
        Area to protect [m²].
    current_density_mA_m2 : float
        Design current density [mA/m²].
    anode_type : str
        Anode material key.
    rho : float
        Soil resistivity [ohm-m].
    L : float
        Anode length [m].
    d : float
        Anode diameter [m].

    Returns
    -------
    int
        Required number of anodes (rounded up).
    """
    i_demand = current_demand(surface_area_m2, current_density_mA_m2)
    i_anode = current_per_anode(anode_type, rho, L, d)
    return math.ceil(i_demand / i_anode)


def anode_life_years(
    W_anode_kg: float, anode_type: str, I_per_anode_A: float
) -> float:
    """Estimated anode service life (§6.4).

    Life = (W * E_capacity) / (I * 8760)

    Parameters
    ----------
    W_anode_kg : float
        Anode mass [kg].
    anode_type : str
        Anode material key.
    I_per_anode_A : float
        Current drawn from the anode [A].

    Returns
    -------
    float
        Anode life [years].
    """
    capacity = ANODE_CAPACITY[anode_type]
    return (W_anode_kg * capacity) / (I_per_anode_A * 8760.0)


def check_protection_potential(measured_potential_V_CSE: float) -> dict:
    """Evaluate measured potential against API RP 1632 §4.2 criteria.

    Protection is adequate when the measured (instant-off) potential is
    equal to or more negative than -0.850 V vs CSE.

    Parameters
    ----------
    measured_potential_V_CSE : float
        Measured structure-to-electrolyte potential [V vs CSE].

    Returns
    -------
    dict
        {"pass": bool, "potential": float, "criterion": float}
    """
    is_protected = measured_potential_V_CSE <= PROTECTION_POTENTIAL_CSE
    return {
        "pass": is_protected,
        "potential": measured_potential_V_CSE,
        "criterion": PROTECTION_POTENTIAL_CSE,
    }
