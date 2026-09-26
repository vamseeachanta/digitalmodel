"""Single implementation of the sacrificial-anode CP design formulas.

Every module in :mod:`digitalmodel.cathodic_protection` that needs current
demand, anode mass, the linear coating-breakdown model, anode current output
or one of the DNV-RP-B401 Table 10-7 anode-resistance formulas calls the
function here (issue #2211). The public modules keep their names, signatures
and ``edition`` arguments as thin wrappers.

Formulas (DNV-RP-B401, Sec. 7 and Table 10-7; identical in ISO 15589-2 and
DNV-RP-F103):

- current demand, Eq. 1: ``I_c = A_c * i_c * f_c``
- anode mass, Eq. 2: ``M_a = I_cm * t_f * 8760 / (u * epsilon)``
- coating breakdown: ``f_c(t) = a + b * t``, mean ``a + b * t_f / 2``,
  final ``a + b * t_f``, both capped at 1.0
- anode current output: ``I_a = delta_E / R_a``
- Table 10-7 resistances:

  * long slender stand-off (L >= 4r):
    ``R_a = rho / (2 pi L) * (ln(4L/r) - 1)``
  * short slender stand-off (L < 4r):
    ``R_a = rho / (2 pi L) * [ln{(2L/r)(1 + sqrt(1 + (r/2L)^2))} + r/(2L)
    - sqrt(1 + (r/2L)^2)]``
  * long flush-mounted (L >= 4 width and L >= 4 thickness):
    ``R_a = rho / (2 S)``, S the arithmetic mean of length and width
  * short flush-mounted, bracelet and other types:
    ``R_a = 0.315 rho / sqrt(A)``, A the exposed surface area

  Note 1: for anode-to-structure distances of 0.15-0.30 m the resistance is
  increased by a factor 1.3 (``resistance_proximity_factor``). Note 2: for a
  non-cylindrical anode ``r = c / (2 pi)`` with ``c`` the cross-section
  periphery (``equivalent_radius_from_periphery``).

Every function validates its inputs and raises :class:`ValueError` naming
the offending parameter, so that no caller can obtain a negative resistance
or a bare ``ZeroDivisionError`` from a zero anode mass.
"""

from __future__ import annotations

import math
from typing import Final

HOURS_PER_YEAR: Final = 8760.0

#: Table 10-7: short flush-mounted, bracelet and other types.
SHORT_FLUSH_COEFFICIENT: Final = 0.315

#: Table 10-7 note 1: distance band (m) in which the 1.3 factor applies.
PROXIMITY_FACTOR_MIN_DISTANCE_M: Final = 0.15
PROXIMITY_FACTOR_FREE_DISTANCE_M: Final = 0.30
PROXIMITY_FACTOR_CLOSE: Final = 1.3

#: Maximum coating breakdown factor (bare steel).
MAX_BREAKDOWN_FACTOR: Final = 1.0

#: Long slender stand-off / long flush validity ratios (Table 10-7).
SLENDER_LENGTH_RATIO: Final = 4.0
FLUSH_LENGTH_RATIO: Final = 4.0

#: Typical Al-Zn-In alloy density [kg/m3]; not tabulated in B401.
ANODE_DENSITY_ALZNI: Final = 2750.0


# ---------------------------------------------------------------------------
# Validation helpers
# ---------------------------------------------------------------------------


def _require_positive(name: str, value: float) -> float:
    """Return ``value`` or raise ``ValueError`` when it is not > 0."""
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"{name} must be a positive finite number, got {value!r}")
    return value


def _require_non_negative(name: str, value: float) -> float:
    """Return ``value`` or raise ``ValueError`` when it is not >= 0."""
    if not math.isfinite(value) or value < 0.0:
        raise ValueError(
            f"{name} must be a non-negative finite number, got {value!r}"
        )
    return value


def _require_fraction(name: str, value: float) -> float:
    """Return ``value`` or raise ``ValueError`` when it is not in (0, 1]."""
    if not math.isfinite(value) or value <= 0.0 or value > 1.0:
        raise ValueError(f"{name} must be in (0, 1], got {value!r}")
    return value


# ---------------------------------------------------------------------------
# Demand, mass and coating breakdown
# ---------------------------------------------------------------------------


def current_demand(
    area_m2: float, current_density_A_m2: float, breakdown_factor: float
) -> float:
    """Current demand ``I_c = A_c * i_c * f_c`` (DNV-RP-B401 Sec. 7.4, Eq. 1).

    Parameters
    ----------
    area_m2 : float
        Surface area [m2], >= 0.
    current_density_A_m2 : float
        Design current density [A/m2], >= 0.
    breakdown_factor : float
        Coating breakdown factor, 0 (perfect coating) to 1 (bare steel).

    Returns
    -------
    float
        Current demand [A].
    """
    _require_non_negative("area_m2", area_m2)
    _require_non_negative("current_density_A_m2", current_density_A_m2)
    _require_non_negative("breakdown_factor", breakdown_factor)
    if breakdown_factor > MAX_BREAKDOWN_FACTOR:
        raise ValueError(
            f"breakdown_factor must be <= {MAX_BREAKDOWN_FACTOR}, got {breakdown_factor!r}"
        )
    return area_m2 * current_density_A_m2 * breakdown_factor


def anode_mass(
    I_mean_A: float,
    life_years: float,
    capacity_Ah_kg: float,
    utilisation: float,
) -> float:
    """Net anode mass ``M_a = I_cm * t_f * 8760 / (u * epsilon)`` (B401 Eq. 2).

    Parameters
    ----------
    I_mean_A : float
        Mean current demand over the design life [A], >= 0.
    life_years : float
        Design life [years], > 0.
    capacity_Ah_kg : float
        Electrochemical capacity [Ah/kg], > 0.
    utilisation : float
        Utilisation factor u, in (0, 1].

    Returns
    -------
    float
        Required net anode mass [kg].
    """
    _require_non_negative("I_mean_A", I_mean_A)
    _require_positive("life_years", life_years)
    _require_positive("capacity_Ah_kg", capacity_Ah_kg)
    _require_fraction("utilisation", utilisation)
    return (I_mean_A * life_years * HOURS_PER_YEAR) / (capacity_Ah_kg * utilisation)


def mass_consumed(I_mean_A: float, elapsed_years: float, capacity_Ah_kg: float) -> float:
    """Metal consumed ``I * t * 8760 / epsilon`` [kg] at a mean current.

    This is the alloy actually dissolved (Faraday), *not* the design mass
    requirement: the utilisation factor bounds the usable fraction
    ``M * u`` of the installed mass and must not divide the consumption.
    """
    _require_non_negative("I_mean_A", I_mean_A)
    _require_non_negative("elapsed_years", elapsed_years)
    _require_positive("capacity_Ah_kg", capacity_Ah_kg)
    return I_mean_A * elapsed_years * HOURS_PER_YEAR / capacity_Ah_kg


def coating_breakdown_linear(a: float, b: float, t_years: float) -> float:
    """Coating breakdown factor ``f_c(t) = a + b * t`` capped at 1.0.

    Parameters
    ----------
    a : float
        Initial breakdown constant (dimensionless), >= 0.
    b : float
        Yearly breakdown rate [1/yr], >= 0.
    t_years : float
        Elapsed time [years], >= 0.
    """
    _require_non_negative("a", a)
    _require_non_negative("b", b)
    _require_non_negative("t_years", t_years)
    return min(a + b * t_years, MAX_BREAKDOWN_FACTOR)


def coating_breakdown_mean(a: float, b: float, design_life_years: float) -> float:
    """Mean coating breakdown factor ``f_cm = a + b * t_f / 2`` (capped at 1.0)."""
    return coating_breakdown_linear(a, b, design_life_years / 2.0)


def coating_breakdown_final(a: float, b: float, design_life_years: float) -> float:
    """Final coating breakdown factor ``f_cf = a + b * t_f`` (capped at 1.0)."""
    return coating_breakdown_linear(a, b, design_life_years)


# ---------------------------------------------------------------------------
# Anode current output and counts
# ---------------------------------------------------------------------------


def anode_current_output(delta_E: float, R_a: float) -> float:
    """Anode current output ``I_a = delta_E / R_a`` (B401 Sec. 7.8).

    Parameters
    ----------
    delta_E : float
        Design driving voltage [V], > 0.
    R_a : float
        Anode-to-electrolyte resistance [ohm], > 0.
    """
    _require_positive("delta_E", delta_E)
    _require_positive("R_a", R_a)
    return delta_E / R_a


def anode_count(
    total_mass_kg: float, anode_net_mass_kg: float, round_to_even: bool = False
) -> int:
    """Anodes needed by mass: ``ceil(M / m_a)`` (optionally rounded to even).

    Raises
    ------
    ValueError
        If ``anode_net_mass_kg`` is not positive or ``total_mass_kg`` is
        negative.
    """
    _require_non_negative("total_mass_kg", total_mass_kg)
    _require_positive("anode_net_mass_kg", anode_net_mass_kg)
    n = math.ceil(total_mass_kg / anode_net_mass_kg)
    if round_to_even and n % 2 != 0:
        n += 1
    return n


def anodes_for_current(current_demand_A: float, anode_output_A: float) -> int:
    """Anodes needed by current: ``ceil(I_c / I_a)``.

    Raises
    ------
    ValueError
        If ``anode_output_A`` is not positive or ``current_demand_A`` is
        negative.
    """
    _require_non_negative("current_demand_A", current_demand_A)
    _require_positive("anode_output_A", anode_output_A)
    return math.ceil(current_demand_A / anode_output_A)


# ---------------------------------------------------------------------------
# DNV-RP-B401 Table 10-7 anode resistance
# ---------------------------------------------------------------------------


def long_slender_standoff(rho: float, L: float, r: float) -> float:
    """Long slender stand-off anode, L >= 4r (Table 10-7).

    ``R_a = rho / (2 pi L) * (ln(4 L / r) - 1)``

    Parameters
    ----------
    rho : float
        Electrolyte resistivity [ohm-m].
    L : float
        Anode length [m].
    r : float
        Equivalent cross-section radius [m].

    Raises
    ------
    ValueError
        If any input is not positive or ``L < 4 r`` (use
        ``short_slender_standoff`` or ``slender_standoff``).
    """
    _require_positive("rho", rho)
    _require_positive("L", L)
    _require_positive("r", r)
    if L < SLENDER_LENGTH_RATIO * r:
        raise ValueError(
            f"long_slender_standoff requires L >= 4 r (L={L!r}, r={r!r}); "
            "use short_slender_standoff"
        )
    return (rho / (2.0 * math.pi * L)) * (math.log(4.0 * L / r) - 1.0)


def short_slender_standoff(rho: float, L: float, r: float) -> float:
    """Short slender stand-off anode, L < 4r (Table 10-7).

    ``R_a = rho / (2 pi L) * [ln{(2L/r)(1 + sqrt(1 + (r/2L)^2))} + r/(2L)
    - sqrt(1 + (r/2L)^2)]``

    Raises
    ------
    ValueError
        If any input is not positive or ``L >= 4 r`` (use
        ``long_slender_standoff`` or ``slender_standoff``).
    """
    _require_positive("rho", rho)
    _require_positive("L", L)
    _require_positive("r", r)
    if L >= SLENDER_LENGTH_RATIO * r:
        raise ValueError(
            f"short_slender_standoff requires L < 4 r (L={L!r}, r={r!r}); "
            "use long_slender_standoff"
        )
    ratio = r / (2.0 * L)
    root = math.sqrt(1.0 + ratio * ratio)
    return (rho / (2.0 * math.pi * L)) * (
        math.log((2.0 * L / r) * (1.0 + root)) + ratio - root
    )


def slender_standoff(rho: float, L: float, r: float) -> float:
    """Slender stand-off anode: Table 10-7 long (L >= 4r) or short (L < 4r) form."""
    _require_positive("rho", rho)
    _require_positive("L", L)
    _require_positive("r", r)
    if L >= SLENDER_LENGTH_RATIO * r:
        return long_slender_standoff(rho, L, r)
    return short_slender_standoff(rho, L, r)


def long_flush(rho: float, length: float, width: float, thickness: float) -> float:
    """Long flush-mounted anode, L >= 4 width and L >= 4 thickness (Table 10-7).

    ``R_a = rho / (2 S)`` with ``S`` the arithmetic mean of length and width.

    Raises
    ------
    ValueError
        If any input is not positive or the length ratios are violated (use
        ``short_flush_or_bracelet``).
    """
    _require_positive("rho", rho)
    _require_positive("length", length)
    _require_positive("width", width)
    _require_positive("thickness", thickness)
    if length < FLUSH_LENGTH_RATIO * width or length < FLUSH_LENGTH_RATIO * thickness:
        raise ValueError(
            "long_flush requires length >= 4 width and length >= 4 thickness "
            f"(length={length!r}, width={width!r}, thickness={thickness!r}); "
            "use short_flush_or_bracelet"
        )
    S = (length + width) / 2.0
    return rho / (2.0 * S)


def short_flush_or_bracelet(rho: float, exposed_area_m2: float) -> float:
    """Short flush-mounted, bracelet and other anode types (Table 10-7).

    ``R_a = 0.315 rho / sqrt(A)`` with ``A`` the exposed surface area [m2].
    """
    _require_positive("rho", rho)
    _require_positive("exposed_area_m2", exposed_area_m2)
    return SHORT_FLUSH_COEFFICIENT * rho / math.sqrt(exposed_area_m2)


def resistance_proximity_factor(distance_m: float) -> float:
    """Table 10-7 note 1 factor on ``R_a`` for the anode-to-structure distance.

    1.0 for distances >= 0.30 m, 1.3 for 0.15-0.30 m; distances below
    0.15 m are outside the Table 10-7 stand-off formulas.
    """
    _require_non_negative("distance_m", distance_m)
    if distance_m >= PROXIMITY_FACTOR_FREE_DISTANCE_M:
        return 1.0
    if distance_m >= PROXIMITY_FACTOR_MIN_DISTANCE_M:
        return PROXIMITY_FACTOR_CLOSE
    raise ValueError(
        "distance_m below 0.15 m is outside the DNV-RP-B401 Table 10-7 "
        f"stand-off formulas, got {distance_m!r}"
    )


def equivalent_radius_from_periphery(periphery_m: float) -> float:
    """Equivalent radius ``r = c / (2 pi)`` of a non-cylindrical cross-section.

    Table 10-7 note 2: ``c`` is the cross-section periphery [m].
    """
    _require_positive("periphery_m", periphery_m)
    return periphery_m / (2.0 * math.pi)


def equivalent_radius_from_mass(
    net_mass_kg: float, L: float, density_kg_m3: float = ANODE_DENSITY_ALZNI
) -> float:
    """Equivalent cylinder radius ``r = sqrt(m / (pi L rho_alloy))``.

    Approximates a trapezoidal stand-off anode as a cylinder of the same
    net mass and length.
    """
    _require_positive("net_mass_kg", net_mass_kg)
    _require_positive("L", L)
    _require_positive("density_kg_m3", density_kg_m3)
    return math.sqrt(net_mass_kg / (math.pi * L * density_kg_m3))


__all__ = [
    "ANODE_DENSITY_ALZNI",
    "HOURS_PER_YEAR",
    "MAX_BREAKDOWN_FACTOR",
    "PROXIMITY_FACTOR_CLOSE",
    "PROXIMITY_FACTOR_FREE_DISTANCE_M",
    "PROXIMITY_FACTOR_MIN_DISTANCE_M",
    "SHORT_FLUSH_COEFFICIENT",
    "anode_count",
    "anode_current_output",
    "anode_mass",
    "anodes_for_current",
    "coating_breakdown_final",
    "coating_breakdown_linear",
    "coating_breakdown_mean",
    "current_demand",
    "equivalent_radius_from_mass",
    "equivalent_radius_from_periphery",
    "long_flush",
    "long_slender_standoff",
    "mass_consumed",
    "resistance_proximity_factor",
    "short_flush_or_bracelet",
    "short_slender_standoff",
    "slender_standoff",
]
