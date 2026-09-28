"""ISO 15589-2 — Cathodic Protection of Pipeline Systems —
Part 2: Offshore Pipelines (2004).

Galvanic anode CP design for offshore/subsea pipelines: temperature-
dependent current density, coating breakdown, pipeline current demand,
anode resistance, and mass requirement. The formulas are evaluated by
:mod:`digitalmodel.cathodic_protection._kernels`, so the demand, mass and
resistance agree with the DNV-RP-B401 module for the same inputs
(issue #2211).
"""

from __future__ import annotations

import math

from digitalmodel.cathodic_protection import _kernels as kernel

# ---------------------------------------------------------------------------
# Protection potentials vs Ag/AgCl (§5.2)
# ---------------------------------------------------------------------------
PROTECTION_POTENTIAL_AGAGCL: float = -0.800  # V vs Ag/AgCl
PROTECTION_POTENTIAL_MIN: float = -0.800  # V vs Ag/AgCl
PROTECTION_POTENTIAL_MAX: float = -1.100  # V — avoid H2 embrittlement

# ---------------------------------------------------------------------------
# Al-Zn-In anode properties (§8.4, Table 3)
# ---------------------------------------------------------------------------
ANODE_OC_POTENTIAL_ALZNI: float = -1.050  # V vs Ag/AgCl
ANODE_CAPACITY_ALZNI: float = 2000.0  # A-h/kg
ANODE_UTILIZATION_FACTOR: float = 0.90  # u_f (§8.5)

# ---------------------------------------------------------------------------
# Current density [mA/m²] — ISO 15589-2 Table 1
# ---------------------------------------------------------------------------
IC_WARM: float = 150.0  # initial, >15 degC
IC_COLD: float = 200.0  # initial, <7 degC
IM: float = 100.0  # mean
IF: float = 130.0  # final

_MA_PER_A: float = 1000.0


def initial_current_density(T_seawater_C: float) -> float:
    """Temperature-interpolated initial current density (§7.3, Table 1).

    ic = 150 + (200 - 150) * max(0, (15 - T) / (15 - 7))
    Clamped: ic = IC_WARM for T >= 15, ic = IC_COLD for T <= 7.

    Parameters
    ----------
    T_seawater_C : float
        Seawater temperature [deg C].

    Returns
    -------
    float
        Initial current density [mA/m²].
    """
    if T_seawater_C >= 15.0:
        return IC_WARM
    if T_seawater_C <= 7.0:
        return IC_COLD
    return IC_WARM + (IC_COLD - IC_WARM) * (15.0 - T_seawater_C) / (15.0 - 7.0)


def coating_breakdown_factor(
    fc_i: float, fc_f: float, t_years: float, T_design_years: float
) -> float:
    """Coating breakdown factor at time t (§8.2).

    fc(t) = fc_i + (fc_f - fc_i) * (t / T_design)

    The linear kernel ``a + b t`` with ``a = fc_i`` and
    ``b = (fc_f - fc_i) / T_design``.

    Parameters
    ----------
    fc_i : float
        Initial breakdown factor (e.g. 0.10 for FBE/3LPE).
    fc_f : float
        Final breakdown factor (e.g. 0.30); must not be below ``fc_i``.
    t_years : float
        Elapsed time [years].
    T_design_years : float
        Design life [years].

    Returns
    -------
    float
        Coating breakdown factor at time t (dimensionless).
    """
    if T_design_years <= 0.0:
        raise ValueError(f"T_design_years must be positive, got {T_design_years!r}")
    if fc_f < fc_i:
        raise ValueError(f"fc_f must not be below fc_i (fc_i={fc_i!r}, fc_f={fc_f!r})")
    return kernel.coating_breakdown_linear(fc_i, (fc_f - fc_i) / T_design_years, t_years)


def pipeline_current_demand(
    D: float, L: float, fc: float, ic_mA_m2: float
) -> float:
    """Current demand for a coated pipeline section (§8.3).

    I_c = pi * D * L * fc * ic

    Parameters
    ----------
    D : float
        Pipeline outer diameter [m].
    L : float
        Pipeline length to protect [m].
    fc : float
        Coating breakdown factor (dimensionless).
    ic_mA_m2 : float
        Design current density [mA/m²].

    Returns
    -------
    float
        Protection current demand [A].
    """
    return kernel.current_demand(math.pi * D * L, ic_mA_m2 / _MA_PER_A, fc)


def anode_resistance(rho: float, L_a: float, r_a: float) -> float:
    """Slender stand-off anode resistance (§8.4, Annex A).

    Long slender stand-off (L_a >= 4 r_a):
        R_a = (rho / (2 * pi * L_a)) * (ln(4 * L_a / r_a) - 1)
    Short slender stand-off (L_a < 4 r_a): the corresponding short formula.

    This is the DNV-RP-B401 Table 10-7 form that ISO 15589-2 Annex A also
    gives; the earlier ``ln(2 L / r) - 0.5`` expression was in neither
    standard (review 2026-09-24, issue #2211).

    Parameters
    ----------
    rho : float
        Seawater resistivity [ohm-m].
    L_a : float
        Anode length [m].
    r_a : float
        Anode equivalent radius [m].

    Returns
    -------
    float
        Anode-to-electrolyte resistance [ohm].
    """
    return kernel.slender_standoff(rho, L_a, r_a)


def anode_output_current(
    R_a: float,
    E_anode_V: float = ANODE_OC_POTENTIAL_ALZNI,
    E_struct_V: float = PROTECTION_POTENTIAL_AGAGCL,
) -> float:
    """Anode output current (§8.4).

    I_a = |E_anode - E_struct| / R_a

    Parameters
    ----------
    R_a : float
        Anode-to-electrolyte resistance [ohm].
    E_anode_V : float
        Anode open-circuit potential [V vs Ag/AgCl].
    E_struct_V : float
        Structure design protection potential [V vs Ag/AgCl].

    Returns
    -------
    float
        Anode output current [A].
    """
    return kernel.anode_current_output(abs(E_anode_V - E_struct_V), R_a)


def anode_mass_requirement(
    I_mean_A: float,
    T_design_years: float,
    E_capacity: float = ANODE_CAPACITY_ALZNI,
    u_f: float = ANODE_UTILIZATION_FACTOR,
) -> float:
    """Total anode mass requirement (§8.5).

    M_a = (I_mean * T_design * 8760) / (E_capacity * u_f)

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
        Required anode mass [kg].
    """
    return kernel.anode_mass(I_mean_A, T_design_years, E_capacity, u_f)


def check_protection_potential(measured_V_AgAgCl: float) -> dict:
    """Evaluate measured potential against ISO 15589-2 §5.2 criteria.

    Protection is adequate when the potential is between -0.800 V
    and -1.100 V vs Ag/AgCl. Potentials more negative than -1.100 V
    risk hydrogen embrittlement.

    Parameters
    ----------
    measured_V_AgAgCl : float
        Measured potential [V vs Ag/AgCl].

    Returns
    -------
    dict
        {"pass": bool, "potential": float, "criterion_min": float,
         "criterion_max": float}
    """
    is_protected = (
        measured_V_AgAgCl <= PROTECTION_POTENTIAL_MIN
        and measured_V_AgAgCl >= PROTECTION_POTENTIAL_MAX
    )
    return {
        "pass": is_protected,
        "potential": measured_V_AgAgCl,
        "criterion_min": PROTECTION_POTENTIAL_MIN,
        "criterion_max": PROTECTION_POTENTIAL_MAX,
    }
