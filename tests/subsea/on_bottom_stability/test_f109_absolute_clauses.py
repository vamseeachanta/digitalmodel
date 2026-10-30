# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic clause regressions; no licensed lookup data or design benchmark."""
from copy import deepcopy
import math

import pytest

from digitalmodel.subsea.on_bottom_stability import dnv_rp_f109 as f109


@pytest.mark.parametrize("velocity", [1.5, -1.5])
@pytest.mark.parametrize("factor", [1.1, None])
def test_registered_workflow_rejects_unqualified_instantaneous_loads(velocity, factor):
    """A signed sample and an optional factor cannot qualify peak demands."""
    from digitalmodel.subsea.pipeline.integrity_workflows import run_on_bottom_stability

    obs = {
        "pipe": dict(od_steel_m=0.3239, wt_steel_m=0.0127,
                     coating_thickness_m=0.08, rho_steel=7850,
                     rho_coating=3040, rho_contents=0, rho_seawater=1025),
        "metocean": dict(water_velocity_m_s=velocity, water_acceleration_m_s2=0),
        "hydrodynamic": dict(cd=0.7, cm=3.29, cl=0.9),
        "soil": dict(friction_coefficient=0.5),
    }
    if factor is not None:
        obs["safety_factor"] = factor
    cfg = {"on_bottom_stability": obs}
    before = deepcopy(cfg)
    with pytest.raises(ValueError, match="unqualified.*extreme-cycle peak"):
        run_on_bottom_stability(cfg)
    assert cfg == before


@pytest.mark.parametrize("method", [f109.absolute_stability_check, f109.absolute_stability_margin])
def test_factor_is_explicit(method):
    """Clause 3.6.3: no universal table-row selection."""
    with pytest.raises(TypeError, match="gamma_SC"):
        method(1000, 100, 50, 0.5)


@pytest.mark.parametrize("method", [f109.absolute_stability_check, f109.absolute_stability_margin])
@pytest.mark.parametrize("field,value", [
    ("W_s_N_m", math.inf), ("W_s_N_m", math.nan),
    ("F_H_N_m", -1), ("F_H_N_m", math.inf), ("F_H_N_m", math.nan),
    ("F_L_N_m", -1), ("F_L_N_m", math.inf), ("F_L_N_m", math.nan),
    ("mu_soil", 0), ("mu_soil", -1), ("mu_soil", math.inf),
    ("gamma_SC", 0), ("gamma_SC", -1), ("gamma_SC", math.nan),
    ("F_R_N_m", -1), ("F_R_N_m", math.inf),
])
def test_peak_domain_rejection(method, field, value):
    """Clause 3.6.2 requires finite nonnegative peak demands."""
    kwargs = dict(W_s_N_m=1000, F_H_N_m=100, F_L_N_m=50,
                  mu_soil=0.5, gamma_SC=1.25, F_R_N_m=0)
    kwargs[field] = value
    with pytest.raises(ValueError, match=field):
        method(**kwargs)


def test_passive_resistance_does_not_mask_uplift():
    """Eqs. (3.38)-(3.39): independent uplift governs despite large p."""
    result = f109.absolute_stability_check(
        100, 10, 90, 0.5, 1.25, F_R_N_m=1000,
        passive_resistance_basis="initial_penetration",
    )
    assert result.details["lateral_utilisation"] == pytest.approx(68.75 / 1050)
    assert result.details["vertical_utilisation"] == pytest.approx(1.125)
    assert result.utilisation == pytest.approx(1.125)
    assert result.details["governing_check"] == "uplift"
    assert not result.is_stable
    assert result.details["no_lateral_slip"] is False


@pytest.mark.parametrize("basis", [None, "movement", "unverified"])
def test_no_motion_generated_resistance(basis):
    """Clauses 3.6.1 and 3.6.5: no credit for new motion resistance."""
    with pytest.raises(ValueError, match="passive_resistance_basis"):
        f109.absolute_stability_check(1000, 100, 50, 0.5, 1.25,
                                     F_R_N_m=100, passive_resistance_basis=basis)


@pytest.mark.parametrize("basis", ["initial_penetration", "prior_loading"])
def test_required_weight_inverts_both_criteria(basis):
    """Eqs. (3.38)-(3.39): h=200,z=100,p=50 gives w_req=525."""
    margin = f109.absolute_stability_margin(
        600, 200, 100, 0.5, 1.25, F_R_N_m=50, passive_resistance_basis=basis,
    )
    assert margin.required_lateral_weight_N_m == pytest.approx(525)
    assert margin.required_vertical_weight_N_m == pytest.approx(125)
    assert margin.required_weight_N_m == pytest.approx(525)
    assert margin.weight_margin_N_m == pytest.approx(75)
    assert margin.weight_margin_ratio == pytest.approx(600 / 525)
    assert margin.lateral_utilisation == pytest.approx(312.5 / 350)
    assert margin.vertical_utilisation == pytest.approx(125 / 600)
    assert margin.governing_check == "absolute_lateral"
    at_limit = f109.absolute_stability_check(
        525, 200, 100, 0.5, 1.25, F_R_N_m=50, passive_resistance_basis=basis,
    )
    assert at_limit.is_stable
    assert at_limit.utilisation == pytest.approx(1)
    assert not f109.absolute_stability_check(
        524, 200, 100, 0.5, 1.25, F_R_N_m=50, passive_resistance_basis=basis,
    ).is_stable


def test_vertical_weight_boundary_with_passive_resistance():
    """Eq. (3.39): p cannot reduce the vertical weight requirement."""
    margin = f109.absolute_stability_margin(
        100, 10, 90, 0.5, 1.25, F_R_N_m=1000,
        passive_resistance_basis="initial_penetration",
    )
    assert margin.required_lateral_weight_N_m == 0
    assert margin.required_weight_N_m == pytest.approx(112.5)
    assert margin.governing_check == "uplift"


@pytest.mark.parametrize("weight", [0, -10])
def test_unsupported_pipe_cannot_pass_with_passive_resistance(weight):
    result = f109.absolute_stability_check(
        weight, 0, 0, 0.5, 1.25, F_R_N_m=1000,
        passive_resistance_basis="prior_loading",
    )
    assert not result.is_stable
    assert math.isinf(result.utilisation)


def test_peak_loads_and_separate_reductions():
    """Eqs. (3.17), (3.40)-(3.41), synthetic coefficients and multipliers."""
    rh = f109.load_reduction_factor(0.8, 0.5, 1.0)
    rz = f109.load_reduction_factor(1.0, 0.75, 0.8)
    loads = f109.absolute_peak_loads(1000, 0.5, 2, 5, 1, 2.5, 1.5, rh, rz)
    assert loads.KC_extreme == pytest.approx(20)
    assert loads.current_ratio == pytest.approx(0.5)
    assert loads.dynamic_pressure_N_m == pytest.approx(2250)
    assert loads.horizontal_N_m == pytest.approx(2250)
    assert loads.lift_N_m == pytest.approx(2025)


@pytest.mark.parametrize("args", [
    (1000, 0.5, 0, 5, 1, 2.5, 1.5, 1, 1),
    (1000, 0.5, 2, -5, 1, 2.5, 1.5, 1, 1),
    (1000, 0.5, 2, 5, -1, 2.5, 1.5, 1, 1),
    (1000, 0.5, 2, 5, 1, -2.5, 1.5, 1, 1),
    (1000, 0.5, 2, 5, 1, 2.5, math.nan, 1, 1),
    (1000, 0.5, 2, 5, 1, 2.5, 1.5, 1.1, 1),
])
def test_invalid_peak_inputs(args):
    with pytest.raises(ValueError):
        f109.absolute_peak_loads(*args)


def test_low_kc_user_anchor():
    """Clause 3.6.4: explicit synthetic Table 3-9 anchor, no zero-wave limit."""
    assert f109.low_kc_horizontal_coefficient(2, 4, 3) == pytest.approx(6)
    assert f109.low_kc_horizontal_coefficient(4, 4, 3) == pytest.approx(3)
    for kc in [0, -1, 5, math.inf]:
        with pytest.raises(ValueError):
            f109.low_kc_horizontal_coefficient(kc, 4, 3)


def test_current_profile_and_diameter_mean():
    """Eqs. (3.2)-(3.3): compare mean with independent midpoint quadrature."""
    d, roughness, zref = 0.5, 0.01, 2
    n = 10000
    # Integrate V*sin(theta)*ln(1+z/z0)/ln(1+zref/z0) directly;
    # neither production helper contributes to the expected value.
    mean = sum(
        1.5 * math.sin(math.pi / 3)
        * math.log(1 + (i + 0.5) * d / n / roughness)
        / math.log(1 + zref / roughness)
        for i in range(n)
    ) / n
    actual = f109.diameter_mean_current(1.5, d, zref, roughness, math.pi / 3)
    assert actual == pytest.approx(mean, rel=1e-7)
    assert f109.diameter_mean_current(1.5, d, zref, roughness, 0) == 0
    assert f109.diameter_mean_current(1.5, d, zref, roughness, -math.pi / 3) == -actual
    assert f109.transverse_current_at_height(1.5, 0, zref, roughness, 1) == 0


@pytest.mark.parametrize("function", ["diameter_mean_current", "transverse_current_at_height"])
@pytest.mark.parametrize("index,value", [(0, -1), (1, -1), (2, 0), (3, 0), (4, math.nan)])
def test_invalid_current_inputs(function, index, value):
    args = [1.5, 0.5, 2, 0.01, 1]
    args[index] = value
    with pytest.raises(ValueError):
        getattr(f109, function)(*args)


def test_weight_geometry_and_reference_gravity():
    """Clauses 1.5 and 2.3: independently expanded concentric annuli."""
    expected = math.pi / 4 * (7800 * (0.5**2 - 0.46**2)
                              + 2400 * (0.6**2 - 0.5**2)
                              + 800 * 0.46**2 - 1000 * 0.6**2) * 9.81
    assert f109.submerged_weight_per_meter(0.5, 0.02, 7800, 2400, 0.05, 800, 1000) == pytest.approx(expected)


@pytest.mark.parametrize("field,value", [
    ("D_outer_m", 0), ("D_outer_m", math.inf), ("t_wall_m", 0),
    ("t_wall_m", 0.3), ("t_coat_m", -1), ("rho_steel_kg_m3", -1),
    ("rho_w_kg_m3", math.nan), ("rho_coat_kg_m3", -1),
    ("rho_coat_kg_m3", 0), ("rho_contents_kg_m3", -1),
])
def test_weight_rejects_invalid_geometry_and_density(field, value):
    kwargs = dict(D_outer_m=0.5, t_wall_m=0.02, rho_steel_kg_m3=7800,
                  rho_coat_kg_m3=2400, t_coat_m=0.05,
                  rho_contents_kg_m3=800, rho_w_kg_m3=1000)
    kwargs[field] = value
    with pytest.raises(ValueError, match=field):
        f109.submerged_weight_per_meter(**kwargs)


def test_utilisation_governor_can_differ_from_weight_governor():
    """Eqs. (3.38)-(3.39): fixed p makes distinct denominators matter."""
    kwargs = dict(W_s_N_m=1000, F_H_N_m=350, F_L_N_m=100, mu_soil=0.5,
                  gamma_SC=1.25, F_R_N_m=1000,
                  passive_resistance_basis="initial_penetration")
    check = f109.absolute_stability_check(**kwargs)
    margin = f109.absolute_stability_margin(**kwargs)
    assert check.details["governing_check"] == "absolute_lateral"
    assert check.utilisation == pytest.approx(500 / 1500)
    assert margin.governing_check == "uplift"
    assert margin.required_weight_N_m == pytest.approx(125)


@pytest.mark.parametrize("passive,lift", [(0, 10), (500, 90)])
def test_static_alias_enforces_same_checks_and_warns(passive, lift):
    """The old generalised name must not retain the unfactored-lift defect."""
    kwargs = dict(W_s_N_m=100, F_H_N_m=10, F_L_N_m=lift, mu_soil=0.5,
                  gamma_SC=1.25, F_R_N_m=passive,
                  passive_resistance_basis="prior_loading")
    with pytest.warns(DeprecationWarning, match="static compatibility"):
        alias = f109.generalized_stability_check(**kwargs)
    assert alias == f109.absolute_stability_check(**kwargs)


def test_eq_342_friction_only_unfactored_boundary():
    """Eq. (3.42) is the unreduced unfactored subset of the design checks."""
    loads = f109.absolute_peak_loads(1000, 0.5, 2, 5, 1, 2.5, 1.5, 1, 1)
    margin = f109.absolute_stability_margin(
        20000, loads.horizontal_N_m, loads.lift_N_m, 0.5, 1,
    )
    assert margin.required_weight_N_m / loads.dynamic_pressure_N_m == pytest.approx(6.5)


@pytest.mark.parametrize("function,args", [
    (f109.hydrodynamic_force_per_meter, (1000, 0.5, 1, 0)),
    (f109.lift_force_per_meter, (1000, 0.5, 1)),
])
def test_instantaneous_coefficients_are_explicit(function, args):
    with pytest.raises(TypeError):
        function(*args)


@pytest.mark.parametrize("index,value", [(0, 0), (0, math.inf), (1, -1),
                                        (2, math.nan), (3, math.inf), (4, -1), (5, -1)])
def test_instantaneous_horizontal_domain(index, value):
    args = [1000, 0.5, -1, 0, 0.7, 2]
    args[index] = value
    with pytest.raises(ValueError):
        f109.hydrodynamic_force_per_meter(*args)


@pytest.mark.parametrize("index,value", [(0, 0), (1, -1), (2, math.inf), (3, -1)])
def test_instantaneous_lift_domain(index, value):
    args = [1000, 0.5, -1, 0.7]
    args[index] = value
    with pytest.raises(ValueError):
        f109.lift_force_per_meter(*args)


@pytest.mark.parametrize("value", [-0.1, 1.1, math.nan, math.inf])
def test_reduction_domain(value):
    with pytest.raises(ValueError):
        f109.load_reduction_factor(1, value, 1)


def test_current_small_diameter_limit():
    """Eq. (3.3) must remain positive without subtractive cancellation."""
    d, roughness = 1e-12, 1
    result = f109.diameter_mean_current(1, d, 1, roughness, math.pi / 2)
    assert result == pytest.approx(d / (2 * roughness * math.log(2)), rel=1e-10)


def test_wrapper_factored_lift_and_explicit_factor():
    from digitalmodel.subsea.on_bottom_stability import (
        check_lateral_stability, check_vertical_stability, lateral_stability_check,
    )
    # At h=30 the former horizontal-only factor gives 0.9375;
    # Eq. (3.38) reaches 1.0 because lift is factored too.
    result = lateral_stability_check(100, 30, 20, 0.5, 1.25)
    assert result.utilization == pytest.approx(1)
    assert result.required_weight_n_per_m == pytest.approx(100)
    assert result.is_stable
    assert check_lateral_stability(0.1, 0.03, 0.02, 0.5, 1.25) == result
    # At h=32 the former balance passes at unity; the corrected check fails.
    overloaded = lateral_stability_check(100, 32, 20, 0.5, 1.25)
    assert overloaded.utilization == pytest.approx(1.05)
    assert not overloaded.is_stable
    for function, args in [(lateral_stability_check, (100, 30, 20, 0.5)),
                           (check_lateral_stability, (0.1, 0.03, 0.02, 0.5)),
                           (check_vertical_stability, (0.1, 0.02))]:
        with pytest.raises(TypeError):
            function(*args)
    with pytest.raises(ValueError, match="F_H_N_m"):
        lateral_stability_check(100, -30, 20, 0.5, 1.25)
    with pytest.raises(ValueError, match="F_L_N_m"):
        check_vertical_stability(0.1, -0.02, 1.25)


def test_overflow_cannot_produce_acceptance():
    with pytest.raises(ValueError, match="demand"):
        f109.absolute_stability_check(1000, 1e308, 0, 0.5, 2)


def test_passive_credit_can_remove_weight_demand_but_not_the_load():
    """Eq. (3.38): zero weight boundary is distinct from zero loading."""
    margin = f109.absolute_stability_margin(
        100, 10, 0, 0.5, 1.25, F_R_N_m=100,
        passive_resistance_basis="prior_loading",
    )
    assert margin.required_weight_N_m == 0
    assert margin.weight_margin_ratio is None
    assert margin.governing_check == "absolute_lateral"
    assert margin.is_stable


def test_wrapper_geometry_and_weight_match_canonical():
    from digitalmodel.subsea.on_bottom_stability import submerged_weight

    result = submerged_weight(0.5, 0.02, 0.05, 7800, 2400, 800, 1000)
    canonical = f109.submerged_weight_per_meter(0.5, 0.02, 7800, 2400, 0.05, 800, 1000)
    assert result.ws_n_per_m == canonical
    assert result.dry_weight_n_per_m - result.buoyancy_n_per_m == pytest.approx(canonical)
    with pytest.raises(ValueError, match="t_wall_m"):
        submerged_weight(0.5, 0.3, 0.05, 7800, 2400, 800, 1000)
