# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Absolute static checks on the supplied F109 clause-map basis.

Locators refer to clauses 3.4 and 3.6 of the supplied reference basis.
The formerly claimed later edition is unqualified. No lookup data is supplied.
Factors, peak coefficients and figure-derived multipliers require explicit,
edition-matched user inputs. Generalised displacement and dynamic methods
remain OPEN. Instantaneous Morison helpers do not generate clause 3.6 peaks.
"""
from __future__ import annotations

import math
from typing import NamedTuple
import warnings


class StabilityResult(NamedTuple):
    """Governing utilisation, combined acceptance and intermediate evidence."""

    utilisation: float
    is_stable: bool
    details: dict


class AbsoluteStabilityMargin(NamedTuple):
    """Weight boundary from Eqs. (3.38)-(3.39), forces in N/m.

    Ratio is supplied / required, or None for zero demand. Both utilisations
    and the separate weight boundaries are retained. A positive reserve
    cannot qualify unsupported or invalid input. This is no-slip screening,
    not a displacement or structural assessment.
    """

    required_weight_N_m: float
    supplied_weight_N_m: float
    weight_margin_ratio: float | None
    weight_margin_N_m: float
    governing_check: str
    is_stable: bool
    lateral_utilisation: float
    vertical_utilisation: float
    required_lateral_weight_N_m: float
    required_vertical_weight_N_m: float


class AbsolutePeakLoads(NamedTuple):
    """Clause 3.6.4 extreme-cycle peaks and dimensionless lookup coordinates."""

    horizontal_N_m: float
    lift_N_m: float
    KC_extreme: float
    current_ratio: float
    dynamic_pressure_N_m: float


# Legacy exports for import compatibility only: unqualified instantaneous
# model values, not F109 peak coefficients or licensed table defaults.
C_D_SMOOTH: float = 0.9
C_L_SMOOTH: float = 0.9
C_M_SMOOTH: float = 3.29
C_D_ROUGH: float = 1.2
C_L_ROUGH: float = 1.0
C_M_ROUGH: float = 3.29
GAMMA_SC_NORMAL: float = 1.1  # Legacy export; never selected by a check.


def _finite(name: str, value: float, *, minimum: float | None = None,
            positive: bool = False) -> None:
    """Reject invalid numerical domains before evaluating an equation."""
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite")
    if positive and value <= 0:
        raise ValueError(f"{name} must be positive")
    if minimum is not None and value < minimum:
        raise ValueError(f"{name} must be at least {minimum}")


def hydrodynamic_force_per_meter(
    rho_w_kg_m3: float, D_outer_m: float, U_m_s: float, a_m_s2: float,
    C_D: float, C_M: float,
) -> float:
    """Evaluate signed instantaneous Morison drag plus inertia in N/m.

    This generic model is distinct from clause 3.6.4, Eqs. (3.40)-(3.41).
    U and acceleration are simultaneous signed samples in SI units. Density
    and coated diameter are positive; both model coefficients are explicit
    nonnegative inputs without a licensed-table attribution. No cycle peak,
    diameter-mean current or environmental combination is inferred here.
    """
    _finite("rho_w_kg_m3", rho_w_kg_m3, positive=True)
    _finite("D_outer_m", D_outer_m, positive=True)
    _finite("U_m_s", U_m_s)
    _finite("a_m_s2", a_m_s2)
    _finite("C_D", C_D, minimum=0)
    _finite("C_M", C_M, minimum=0)
    drag = 0.5 * rho_w_kg_m3 * C_D * D_outer_m * abs(U_m_s) * U_m_s
    inertia = rho_w_kg_m3 * C_M * (math.pi / 4) * D_outer_m**2 * a_m_s2
    return drag + inertia


def lift_force_per_meter(
    rho_w_kg_m3: float, D_outer_m: float, U_m_s: float, C_L: float,
) -> float:
    """Evaluate instantaneous lift in N/m with an explicit model coefficient.

    This generic quadratic helper is not clause 3.6.4 Eq. (3.41) peak selection.
    Inputs are SI; velocity may be signed, coefficient is nonnegative.
    """
    _finite("rho_w_kg_m3", rho_w_kg_m3, positive=True)
    _finite("D_outer_m", D_outer_m, positive=True)
    _finite("U_m_s", U_m_s)
    _finite("C_L", C_L, minimum=0)
    return 0.5 * rho_w_kg_m3 * C_L * D_outer_m * U_m_s**2


def transverse_current_at_height(
    V_ref_m_s: float, height_m: float, reference_height_m: float,
    roughness_m: float, heading_rad: float,
) -> float:
    """Clause 3.4.2 Eq. (3.2): signed transverse log-profile velocity [m/s].

    Heading is relative to the pipe axis in radians. Roughness (Table 3-1)
    and reference height are explicit positive user inputs in metres.
    """
    _validate_current(V_ref_m_s, reference_height_m, roughness_m, heading_rad)
    _finite("height_m", height_m, minimum=0)
    return (V_ref_m_s * math.sin(heading_rad) * math.log1p(height_m / roughness_m)
            / math.log1p(reference_height_m / roughness_m))


def _validate_current(speed: float, reference: float, roughness: float, heading: float) -> None:
    """Validate the shared domain of Eqs. (3.2)-(3.3)."""
    _finite("V_ref_m_s", speed, minimum=0)
    _finite("reference_height_m", reference, positive=True)
    _finite("roughness_m", roughness, positive=True)
    _finite("heading_rad", heading)


def diameter_mean_current(
    V_ref_m_s: float, D_outer_m: float, reference_height_m: float,
    roughness_m: float, heading_rad: float,
) -> float:
    """Clause 3.4.2 Eq. (3.3): diameter-mean transverse current [m/s].

    Coated diameter, reference height and Table 3-1 roughness are in metres;
    heading is in radians relative to the pipe. This returns signed current.
    Resolve the governing extreme-cycle direction before supplying a
    nonnegative associated current to :func:`absolute_peak_loads`.
    """
    _validate_current(V_ref_m_s, reference_height_m, roughness_m, heading_rad)
    _finite("D_outer_m", D_outer_m, positive=True)
    x = D_outer_m / roughness_m
    # Series avoids cancellation when diameter is small relative to roughness.
    mean_profile = (x / 2 - x**2 / 6 + x**3 / 12 if x < 1e-4
                    else (1 + 1 / x) * math.log1p(x) - 1)
    return (V_ref_m_s * math.sin(heading_rad) * mean_profile
            / math.log1p(reference_height_m / roughness_m))


def load_reduction_factor(permeability: float, penetration: float, trench: float) -> float:
    """Clause 3.4.5 Eq. (3.17): combine qualified direction-specific reductions.

    All multipliers are explicit inputs between zero and one. Evaluate this
    separately for horizontal and vertical loads. Figures 3-5 through 3-8
    and Eqs. (3.18)-(3.22) require source geometry, units and applicability
    evidence from the caller; no empirical fits are inferred here. Use unit
    multipliers when coefficients already include the same reductions.
    """
    for name, value in (("permeability", permeability), ("penetration", penetration),
                        ("trench", trench)):
        _finite(name, value, minimum=0)
        if value > 1:
            raise ValueError(f"{name} must not exceed one")
    return permeability * penetration * trench


def absolute_peak_loads(
    rho_w_kg_m3: float, D_outer_m: float, U_extreme_m_s: float,
    T_extreme_s: float, V_extreme_m_s: float, C_H: float, C_Z: float,
    reduction_horizontal: float, reduction_vertical: float,
) -> AbsolutePeakLoads:
    """Clause 3.6.4 Eqs. (3.40)-(3.41): horizontal/upward peaks [N/m].

    Supply the extreme oscillation amplitude, associated period and transverse
    diameter-mean current magnitude (SI), derived under clauses 3.4.2-3.4.4.
    KC = Ue*Te/D and M = Ve/Ue locate explicit C_H (Table 3-9) and C_Z
    (Table 3-10) user inputs; Figures 3-13/3-14 alternatives are also inputs.
    There is no interpolation or lookup here. Coefficients already contain
    the peak drag/inertia response: never add a separate inertia force.
    Direction-specific Eq. (3.17) reductions are applied once. Wave amplitude
    must be positive; the zero-wave limit remains OPEN.
    """
    for name, value in (("rho_w_kg_m3", rho_w_kg_m3), ("D_outer_m", D_outer_m),
                        ("U_extreme_m_s", U_extreme_m_s), ("T_extreme_s", T_extreme_s)):
        _finite(name, value, positive=True)
    for name, value in (("V_extreme_m_s", V_extreme_m_s), ("C_H", C_H), ("C_Z", C_Z)):
        _finite(name, value, minimum=0)
    rh = load_reduction_factor(reduction_horizontal, 1, 1)
    rz = load_reduction_factor(reduction_vertical, 1, 1)
    q = 0.5 * rho_w_kg_m3 * D_outer_m * (U_extreme_m_s + V_extreme_m_s)**2
    return AbsolutePeakLoads(rh * C_H * q, rz * C_Z * q,
                             U_extreme_m_s * T_extreme_s / D_outer_m,
                             V_extreme_m_s / U_extreme_m_s, q)


def low_kc_horizontal_coefficient(KC_extreme: float, KC_anchor: float, C_H_anchor: float) -> float:
    """Clause 3.6.4: inverse-KC adjustment below a supplied Table 3-9 anchor.

    The first-column KC coordinate and coefficient are explicit user inputs.
    Only 0 < KC <= anchor is supported. No zero-wave limit or other
    extrapolation is established; the result is not a separate inertia term.
    """
    _finite("KC_extreme", KC_extreme, positive=True)
    _finite("KC_anchor", KC_anchor, positive=True)
    _finite("C_H_anchor", C_H_anchor, minimum=0)
    if KC_extreme > KC_anchor:
        raise ValueError("KC_extreme must not exceed KC_anchor")
    return C_H_anchor * KC_anchor / KC_extreme


def submerged_weight_per_meter(
    D_outer_m: float, t_wall_m: float, rho_steel_kg_m3: float,
    rho_coat_kg_m3: float, t_coat_m: float, rho_contents_kg_m3: float,
    rho_w_kg_m3: float,
) -> float:
    """Clauses 1.5 and 2.3: single-coating net downward weight [N/m].

    D_outer_m is STEEL diameter; the hydrodynamic functions use COATED
    diameter. Thicknesses are radial, in metres; densities are kg/m3.
    Gravity follows clause 1.5. Supply phase-specific minimum contents
    density, including gas; zero represents an explicitly empty idealisation.
    The caller accounts for material loss and governing phase combinations.
    Multilayer geometry and separate flotation Eq. (3.1) remain OPEN.
    """
    for name, value in (("D_outer_m", D_outer_m), ("t_wall_m", t_wall_m),
                        ("rho_steel_kg_m3", rho_steel_kg_m3), ("rho_w_kg_m3", rho_w_kg_m3)):
        _finite(name, value, positive=True)
    for name, value in (("rho_coat_kg_m3", rho_coat_kg_m3), ("t_coat_m", t_coat_m),
                        ("rho_contents_kg_m3", rho_contents_kg_m3)):
        _finite(name, value, minimum=0)
    if 2 * t_wall_m >= D_outer_m:
        raise ValueError("t_wall_m must leave a positive bore")
    if t_coat_m > 0 and rho_coat_kg_m3 == 0:
        raise ValueError("rho_coat_kg_m3 must be positive for a coating annulus")
    inner = D_outer_m - 2 * t_wall_m
    coated = D_outer_m + 2 * t_coat_m
    area = math.pi / 4
    mass = area * (rho_steel_kg_m3 * (D_outer_m**2 - inner**2)
                   + rho_coat_kg_m3 * (coated**2 - D_outer_m**2)
                   + rho_contents_kg_m3 * inner**2 - rho_w_kg_m3 * coated**2)
    return mass * 9.81


def absolute_stability_check(
    W_s_N_m: float, F_H_N_m: float, F_L_N_m: float, mu_soil: float,
    gamma_SC: float, *, F_R_N_m: float = 0.0,
    passive_resistance_basis: str | None = None,
) -> StabilityResult:
    """Clauses 3.6.1-3.6.5, Eqs. (3.38)-(3.39): no-slip acceptance.

    eta_h = gamma*(h + mu*z)/(mu*w + p); eta_z = gamma*z/w.
    Both must be <= 1. Forces are N/m; h and z are finite nonnegative
    extreme-cycle PEAK demands, never unspecified signed time samples.
    Friction and gamma must be positive. Select gamma explicitly from
    clause 3.6.3 Tables 3-5 through 3-8 for the environment, region, soil
    and consequence class; no universal default is qualified.

    Nonzero p requires an explicit initial_penetration or prior_loading
    basis under clause 3.6.5. This records caller qualification, not a soil
    calculation: motion-generated resistance is ineligible under 3.6.1;
    rock passive resistance must be zero. Phase and soil evidence remain
    caller responsibilities. Passing means no lateral slip for the supplied
    extreme cycle only; no travel, structural or flotation claim follows.
    Nonpositive weight returns an unsupported failure. Invalid inputs raise.
    """
    _finite("W_s_N_m", W_s_N_m)
    for name, value in (("F_H_N_m", F_H_N_m), ("F_L_N_m", F_L_N_m), ("F_R_N_m", F_R_N_m)):
        _finite(name, value, minimum=0)
    _finite("mu_soil", mu_soil, positive=True)
    _finite("gamma_SC", gamma_SC, positive=True)
    if F_R_N_m > 0 and passive_resistance_basis not in {"initial_penetration", "prior_loading"}:
        raise ValueError("passive_resistance_basis must qualify initial_penetration or prior_loading")
    demand = gamma_SC * (F_H_N_m + mu_soil * F_L_N_m)
    capacity = mu_soil * W_s_N_m + F_R_N_m
    # Finite inputs can still overflow intermediate arithmetic; never accept it.
    _finite("demand", demand, minimum=0)
    _finite("capacity", capacity)
    vertical_demand = gamma_SC * F_L_N_m
    _finite("vertical_demand", vertical_demand, minimum=0)
    lateral = demand / capacity if W_s_N_m > 0 and capacity > 0 else math.inf
    vertical = vertical_demand / W_s_N_m if W_s_N_m > 0 else math.inf
    required_h = max(0.0, (demand - F_R_N_m) / mu_soil)
    required_z = vertical_demand
    governing = ("unsupported" if W_s_N_m <= 0 else
                 "no_load" if F_H_N_m == F_L_N_m == 0 else
                 "uplift" if vertical >= lateral else "absolute_lateral")
    utilisation = max(lateral, vertical)
    stable = utilisation <= 1
    return StabilityResult(utilisation, stable, {
        "W_s_N_m": W_s_N_m, "F_H_N_m": F_H_N_m, "F_L_N_m": F_L_N_m,
        "mu_soil": mu_soil, "gamma_SC": gamma_SC, "F_R_N_m": F_R_N_m,
        "passive_resistance_basis": passive_resistance_basis,
        "capacity": capacity, "demand": demand,
        "lateral_utilisation": lateral, "vertical_utilisation": vertical,
        "required_lateral_weight_N_m": required_h, "required_vertical_weight_N_m": required_z,
        "governing_check": governing, "no_lateral_slip": stable,
        "load_basis": "extreme_cycle_peaks",
        "acceptance_basis": "3.6.1; 3.6.2 Eqs. (3.38)-(3.39)",
    })


def absolute_stability_margin(
    W_s_N_m: float, F_H_N_m: float, F_L_N_m: float, mu_soil: float,
    gamma_SC: float, *, F_R_N_m: float = 0.0,
    passive_resistance_basis: str | None = None,
) -> AbsoluteStabilityMargin:
    """Invert clause 3.6.2 Eqs. (3.38)-(3.39) for required submerged weight.

    Required weight is max(0, (gamma*(h+mu*z)-p)/mu, gamma*z).
    The separate uplift boundary cannot be reduced by passive resistance.
    The margin governor identifies the larger required weight; the check's
    governor identifies the larger utilisation at the supplied weight.
    These may differ with nonzero passive resistance.
    Inputs and no-slip qualification follow :func:`absolute_stability_check`.
    Passive resistance is held fixed: if it depends on changing weight,
    penetration or soil contact, requalify it for the proposed weight.
    Figures 3-15/3-16 and Eq. (3.42) do not replace these factored checks.
    """
    check = absolute_stability_check(W_s_N_m, F_H_N_m, F_L_N_m, mu_soil, gamma_SC,
                                     F_R_N_m=F_R_N_m,
                                     passive_resistance_basis=passive_resistance_basis)
    d = check.details
    required = max(d["required_lateral_weight_N_m"], d["required_vertical_weight_N_m"])
    _finite("required_weight_N_m", required, minimum=0)
    governing = ("no_load" if F_H_N_m == F_L_N_m == 0 else
                 "uplift" if d["required_vertical_weight_N_m"] > 0
                 and d["required_vertical_weight_N_m"] >= d["required_lateral_weight_N_m"]
                 else "absolute_lateral")
    return AbsoluteStabilityMargin(
        required, W_s_N_m, W_s_N_m / required if required > 0 else None,
        W_s_N_m - required, governing, check.is_stable,
        d["lateral_utilisation"], d["vertical_utilisation"],
        d["required_lateral_weight_N_m"], d["required_vertical_weight_N_m"],
    )


def generalized_stability_check(
    W_s_N_m: float, F_H_N_m: float, F_L_N_m: float, mu_soil: float,
    F_R_N_m: float, gamma_SC: float, *, passive_resistance_basis: str | None = None,
) -> StabilityResult:
    """Deprecated STATIC compatibility alias for clause 3.6.2 checks.

    This name never implemented clause 3.5 Eq. (3.32) displacement response.
    It now uses Eqs. (3.38)-(3.39), including factored lift and independent
    uplift. Nonzero passive resistance requires initial-state qualification.
    The genuine generalised and dynamic methods remain OPEN.
    """
    warnings.warn("generalized_stability_check is a static compatibility alias; "
                  "use absolute_stability_check; clause 3.5 remains OPEN",
                  DeprecationWarning, stacklevel=2)
    return absolute_stability_check(W_s_N_m, F_H_N_m, F_L_N_m, mu_soil, gamma_SC,
                                     F_R_N_m=F_R_N_m,
                                     passive_resistance_basis=passive_resistance_basis)
