"""DNV-RP-B401 — Cathodic Protection Design (editions 2005 to 2021).

Sacrificial anode CP design for offshore structures: current demand,
coating breakdown, anode mass requirement, Table 10-7 anode resistance,
anode current output and anode count. Every formula is evaluated by
:mod:`digitalmodel.cathodic_protection._kernels`; the functions here keep
the public names, signatures and ``edition`` arguments as thin wrappers
(issue #2211). The DNV-RP-F103 protected length lives in
:mod:`digitalmodel.cathodic_protection.dnv_rp_f103`; ``protected_length``
here is a compatibility wrapper.
"""

from __future__ import annotations

import warnings
from typing import Final

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import (
    DEFAULT_EDITION,
    Edition,
    F103Edition,
    normalize_edition,
)
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    AnodeShape,
    anode_capacity,
    anode_closed_circuit_potential,
    design_driving_voltage,
    protection_potential,
    utilisation_factor,
)
from digitalmodel.cathodic_protection.dnv_rp_f103 import (
    STEEL_RESISTIVITY as _F103_STEEL_RESISTIVITY,
)
from digitalmodel.cathodic_protection.dnv_rp_f103 import (
    protected_length as _f103_protected_length,
)

# Module constants are derived from the cited B401 table lookups at import
# time (issue #2207). The values are identical across all supported editions,
# so the package default edition is used here without a warning; callers that
# need the citation call the ``b401_tables`` lookup with their own edition.
_TABLE_EDITION: Edition = DEFAULT_EDITION

# ---------------------------------------------------------------------------
# Protection potentials vs Ag/AgCl (DNV-RP-B401 Sec. 5 and Table 10-6)
# ---------------------------------------------------------------------------
PROTECTION_POTENTIAL_AGAGCL: float = protection_potential(_TABLE_EDITION).value
ANODE_CLOSED_CIRCUIT_POTENTIAL: float = anode_closed_circuit_potential(
    AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, _TABLE_EDITION
).value  # V vs Ag/AgCl, Al-based anode in seawater

# ---------------------------------------------------------------------------
# Design driving voltage E_c - E_a (Table 10-6 with Sec. 5): 0.25 V for Al
# ---------------------------------------------------------------------------
DESIGN_DRIVING_VOLTAGE: float = design_driving_voltage(
    AnodeMaterial.ALUMINIUM, _TABLE_EDITION
).value

# ---------------------------------------------------------------------------
# Al-based anode properties (DNV-RP-B401 Table 10-6)
# ---------------------------------------------------------------------------
ANODE_CAPACITY_ALZNI: float = anode_capacity(
    AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, _TABLE_EDITION
).value  # A-h/kg electrochemical capacity, seawater
# Alloy density is not tabulated in B401; typical Al-Zn-In value.
ANODE_DENSITY_ALZNI: float = kernel.ANODE_DENSITY_ALZNI  # kg/m3

# ---------------------------------------------------------------------------
# Utilisation factors (DNV-RP-B401 Table 10-8)
# ---------------------------------------------------------------------------
UTILIZATION_FACTOR_STANDOFF: float = utilisation_factor(
    AnodeShape.LONG_SLENDER_STANDOFF, _TABLE_EDITION
).value  # long slender stand-off, L >= 4r
UTILIZATION_FACTOR_FLUSH: float = utilisation_factor(
    AnodeShape.LONG_FLUSH, _TABLE_EDITION
).value  # long flush-mounted, L >= 4 width and thickness

# ---------------------------------------------------------------------------
# Steel resistivity (DNV-RP-F103 §5.6.10)
# ---------------------------------------------------------------------------
STEEL_RESISTIVITY: float = _F103_STEEL_RESISTIVITY  # ohm-m

# Companion DNV-RP-F103 edition for a B401 edition (inverse of the map in
# ``f103_tables``): the 2005 and 2010 B401 editions pair with F103 (2010),
# the DNVGL 2017 and DNV 2021 editions with DNVGL-RP-F103 (2016).
_F103_EDITION_FOR_B401: Final[dict[Edition, F103Edition]] = {
    "2005": "2010",
    "2010": "2010",
    "2017": "2016",
    "2021": "2016",
}

_INCH_M: Final = 0.0254
_OHM_CM_TO_OHM_M: Final = 0.01


def current_demand(
    surface_area_m2: float,
    current_density_A_m2: float,
    breakdown_factor: float,
    edition: Edition | None = None,
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
    _ = normalize_edition(edition, stacklevel=3)
    return kernel.current_demand(surface_area_m2, current_density_A_m2, breakdown_factor)


def anode_mass_requirement(
    I_mean_A: float,
    T_design_years: float,
    E_capacity: float = ANODE_CAPACITY_ALZNI,
    u_f: float = UTILIZATION_FACTOR_STANDOFF,
    edition: Edition | None = None,
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
    _ = normalize_edition(edition, stacklevel=3)
    return kernel.anode_mass(I_mean_A, T_design_years, E_capacity, u_f)


def coating_breakdown_factor(
    a: float,
    b: float,
    t_years: float,
    edition: Edition | None = None,
) -> float:
    """Coating breakdown factor at time t (DNV-RP-B401 Table 10-4).

    f_c = a + b * t, capped at 1.0 (bare steel).

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
    _ = normalize_edition(edition, stacklevel=3)
    return kernel.coating_breakdown_linear(a, b, t_years)


def anode_resistance_slender_standoff(
    rho: float,
    L_a: float,
    r_a: float,
    proximity_factor: float = 1.0,
    edition: Edition | None = None,
) -> float:
    """Anode resistance for a slender stand-off anode (DNV-RP-B401 Table 10-7).

    Long slender stand-off (L_a >= 4 r_a):
        R_a = (rho / (2 * pi * L_a)) * (ln(4 * L_a / r_a) - 1)
    Short slender stand-off (L_a < 4 r_a): the Table 10-7 short formula
    (``_kernels.short_slender_standoff``). The form is selected from the
    L_a / r_a ratio, so a stubby or depleted anode no longer receives the
    long formula outside its validity range.

    Parameters
    ----------
    rho : float
        Seawater resistivity [ohm-m].
    L_a : float
        Anode length [m].
    r_a : float
        Anode equivalent radius [m].
    proximity_factor : float
        Proximity/shielding factor applied to R_a (1.3 for anode-to-structure
        distances of 150-300 mm, Table 10-7 note 1; see
        ``_kernels.resistance_proximity_factor``).

    Returns
    -------
    float
        Anode-to-electrolyte resistance [ohm].
    """
    _ = normalize_edition(edition, stacklevel=3)
    if proximity_factor <= 0.0:
        raise ValueError(f"proximity_factor must be positive, got {proximity_factor!r}")
    return kernel.slender_standoff(rho, L_a, r_a) * proximity_factor


def anode_current_output(
    rho: float,
    L_a: float,
    r_a: float,
    delta_E: float = DESIGN_DRIVING_VOLTAGE,
    proximity_factor: float = 1.0,
    edition: Edition | None = None,
) -> float:
    """Anode current output of a slender stand-off anode (DNV-RP-B401 §7.8).

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
    ed = normalize_edition(edition, stacklevel=3)
    R_a = anode_resistance_slender_standoff(rho, L_a, r_a, proximity_factor, edition=ed)
    return kernel.anode_current_output(delta_E, R_a)


def equivalent_radius_from_mass(
    net_mass_kg: float,
    L_a: float,
    density: float = ANODE_DENSITY_ALZNI,
    edition: Edition | None = None,
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
    _ = normalize_edition(edition, stacklevel=3)
    return kernel.equivalent_radius_from_mass(net_mass_kg, L_a, density)


def number_of_anodes(
    total_mass_kg: float,
    anode_net_mass_kg: float,
    round_to_even: bool = False,
    edition: Edition | None = None,
) -> int:
    """Number of anodes required (rounded up).

    Parameters
    ----------
    total_mass_kg : float
        Total net anode mass requirement [kg].
    anode_net_mass_kg : float
        Net mass of one anode [kg]; must be positive.
    round_to_even : bool, optional
        Round up to the next even integer for symmetric placement.
        Default False (mathematical ceiling only).

    Returns
    -------
    int
        Number of anodes (rounded up to nearest integer or even integer).

    Raises
    ------
    ValueError
        If ``anode_net_mass_kg`` is not positive.
    """
    _ = normalize_edition(edition, stacklevel=3)
    return kernel.anode_count(total_mass_kg, anode_net_mass_kg, round_to_even)


def protected_length(
    delta_E_me: float,
    WT: float,
    D: float,
    rho_me: float,
    f_cf: float,
    i_cm: float,
    edition: Edition | None = None,
) -> float:
    """Protected length of rigid riser line pipe (DNV-RP-F103 §5.6.7, Eq 14).

    Compatibility wrapper for
    :func:`digitalmodel.cathodic_protection.dnv_rp_f103.protected_length`;
    the B401 ``edition`` is mapped to its companion F103 edition
    (2005/2010 -> 2010, 2017/2021 -> 2016).

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
    ed = normalize_edition(edition, stacklevel=3)
    return _f103_protected_length(
        delta_E_me, WT, D, rho_me, f_cf, i_cm, edition=_F103_EDITION_FOR_B401[ed]
    )


def flush_anode_resistance(
    rho_ohm_cm: float,
    L_a_in: float,
    W_in: float,
    H_in: float,
    r_eq_in: float,
    edition: Edition | None = None,
) -> float:
    """Flush-mounted anode resistance in spreadsheet units (deprecated).

    .. deprecated:: #2211
        Use ``_kernels.short_flush_or_bracelet`` (SI units) or
        ``anode_sizing.calculate_anode_resistance``. This wrapper converts
        inches and ohm-cm to SI and evaluates the DNV-RP-B401 Table 10-7
        short flush-mounted formula ``R_a = 0.315 rho / sqrt(A)`` with the
        exposed area ``A = L * W``. The earlier implementation ignored
        ``W_in`` and ``H_in`` and evaluated a half-space slender-body
        expression mis-attributed to McCoy; ``W_in`` is now used, ``H_in``
        and ``r_eq_in`` are accepted for signature compatibility only.

    Parameters
    ----------
    rho_ohm_cm : float
        Seawater resistivity [ohm-cm].
    L_a_in : float
        Anode length [inches].
    W_in : float
        Anode width [inches]; with the length it gives the exposed area.
    H_in : float
        Anode height [inches]; unused (the Table 10-7 short flush formula
        depends on the exposed area only).
    r_eq_in : float
        Anode equivalent radius [inches]; unused.

    Returns
    -------
    float
        Anode-to-electrolyte resistance [ohm].
    """
    warnings.warn(
        "flush_anode_resistance is deprecated: it now evaluates the DNV-RP-B401 "
        "Table 10-7 short flush-mounted formula 0.315 rho / sqrt(L * W) in SI; "
        "call _kernels.short_flush_or_bracelet or "
        "anode_sizing.calculate_anode_resistance instead.",
        DeprecationWarning,
        stacklevel=2,
    )
    _ = normalize_edition(edition, stacklevel=3)
    _ = H_in, r_eq_in  # accepted for signature compatibility, not used
    rho = rho_ohm_cm * _OHM_CM_TO_OHM_M
    exposed_area_m2 = (L_a_in * _INCH_M) * (W_in * _INCH_M)
    return kernel.short_flush_or_bracelet(rho, exposed_area_m2)
