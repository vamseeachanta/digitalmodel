# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand-solvable operating points and explicit staged transient bounds."""

import math

import pytest


def case_inputs():
    """Construct a PI/vertical-lift/choke case entirely in SI units."""
    from digitalmodel.subsea.capping_stack.containment_flow import linear_inflow

    # Head=1000 Pa; p_res=11000 Pa; q=1e-4*(11000-p_bottom).
    # Choke q=0.01*sqrt(p_wh); q=0.5 gives p_wh=2500 Pa,
    # but q=0.5 implies p_bottom=6000 Pa. Actual root is evaluated below.
    return dict(inflow=lambda p: linear_inflow(p, 11000, 1e-4),
                reservoir_pressure=11000.0, column_density=1000.0,
                vertical_depth=1.0, gravity=1.0,
                bore_length=1.0, bore_diameter=1.0, friction_factor=0.0,
                outlet_pressure=0.0, choke_capacity=lambda p: 0.01*math.sqrt(p))


def test_linear_vogel_and_gas_inflow_numerical_behaviour():
    from digitalmodel.subsea.capping_stack.containment_flow import (
        linear_inflow, vogel_inflow, gas_backpressure_inflow,
    )

    assert linear_inflow(5e6, 10e6, 1e-8) == 0.05
    assert vogel_inflow(5e6, 10e6, 0.1) == pytest.approx(0.07)
    assert gas_backpressure_inflow(5e6, 10e6, 1e-15, 1.0) == pytest.approx(0.075)
    assert linear_inflow(11e6, 10e6, 1e-8) == 0
    assert vogel_inflow(10e6, 10e6, 0.1) == 0
    with pytest.raises(ValueError):
        gas_backpressure_inflow(5e6, 10e6, 1e-15, 0.1)


def test_ipr_vlp_choke_intersection_by_hand():
    from digitalmodel.subsea.capping_stack.containment_flow import solve_operating_point

    result = solve_operating_point(opening=1.0, **case_inputs())
    # q=1-1e-4*p_wh and p_wh=(q/0.01)^2 -> q^2+q-1=0.
    expected = (math.sqrt(5) - 1) / 2
    assert result.flow_rate_m3_s == pytest.approx(expected, abs=1e-10)
    assert result.wellhead_pressure_pa == pytest.approx((expected / 0.01)**2)
    assert result.bottomhole_pressure_pa == pytest.approx(result.wellhead_pressure_pa+1000)
    assert result.ipr_residual_m3_s == pytest.approx(0, abs=1e-10)
    assert result.choke_residual_m3_s == pytest.approx(0, abs=1e-10)


def test_bore_friction_reduces_rate_and_has_expected_darcy_drop():
    from digitalmodel.subsea.capping_stack.containment_flow import solve_operating_point

    inputs = case_inputs()
    free = solve_operating_point(opening=1.0, **inputs)
    inputs["friction_factor"] = 0.1
    resisted = solve_operating_point(opening=1.0, **inputs)
    area = math.pi / 4
    expected_drop = 0.1 * 1000 * (resisted.flow_rate_m3_s / area)**2 / 2
    assert resisted.flow_rate_m3_s < free.flow_rate_m3_s
    assert resisted.bottomhole_pressure_pa - resisted.wellhead_pressure_pa == pytest.approx(
        1000+expected_drop
    )


def test_choked_valve_relation_composes_with_operating_point():
    from digitalmodel.subsea.capping_stack.containment_flow import solve_operating_point
    from digitalmodel.subsea.hipps.valve_sizing import liquid_capacity

    inputs = case_inputs()
    inputs["choke_capacity"] = lambda p: liquid_capacity(
        coefficient=0.01, p_upstream=p, p_downstream=0,
        specific_gravity=1, unit_constant=1, piping_factor=1,
        recovery_factor=0.5, critical_pressure_ratio=1, vapor_pressure=0,
    ).rate
    result = solve_operating_point(opening=1.0, **inputs)
    # Choked q=0.005*sqrt(p_wh), so 4*q^2+q-1=0.
    expected = (math.sqrt(17)-1)/8
    assert result.flow_rate_m3_s == pytest.approx(expected, abs=1e-10)


def test_staged_peak_not_terminal_pressure_feeds_containment():
    from digitalmodel.subsea.capping_stack.containment_flow import staged_shut_in

    result = staged_shut_in(
        time_s=[0, 1, 2], openings=[1, 0.5, 0],
        transient_pressure_allowance_pa=[0, 20000, 0],
        rated_working_pressure=15000, **case_inputs(),
    )
    assert result.stages[-1].flow_rate_m3_s == 0
    assert result.stages[-1].wellhead_pressure_pa == pytest.approx(10000)
    assert result.terminal_containment.is_contained
    assert result.peak_pressure_pa > 20000
    assert result.peak_containment.siwhp == pytest.approx(result.peak_pressure_pa)
    assert not result.peak_containment.is_contained
    assert result.peak_time_s == 1


def test_staging_requires_actual_shut_in_and_ordered_history():
    from digitalmodel.subsea.capping_stack.containment_flow import staged_shut_in

    with pytest.raises(ValueError):
        staged_shut_in(time_s=[0, 1], openings=[1, 0.5],
                      transient_pressure_allowance_pa=[0, 0],
                      rated_working_pressure=15000, **case_inputs())
    with pytest.raises(ValueError):
        staged_shut_in(time_s=[1, 0], openings=[1, 0],
                      transient_pressure_allowance_pa=[0, 0],
                      rated_working_pressure=15000, **case_inputs())


@pytest.mark.parametrize("field,value", [("column_density", float("nan")),
                                         ("friction_factor", -1.0),
                                         ("opening", 1.1)])
def test_operating_point_rejects_invalid_inputs(field, value):
    from digitalmodel.subsea.capping_stack.containment_flow import solve_operating_point

    inputs = dict(case_inputs(), opening=1.0)
    inputs[field] = value
    with pytest.raises(ValueError):
        solve_operating_point(**inputs)


def test_outlet_above_static_wellhead_prevents_forward_flow():
    from digitalmodel.subsea.capping_stack.containment_flow import solve_operating_point

    inputs = case_inputs()
    inputs["outlet_pressure"] = 12000.0
    result = solve_operating_point(opening=1.0, **inputs)
    assert result.flow_rate_m3_s == 0
    assert result.wellhead_pressure_pa == 10000


def test_nonfinite_capacity_fails_closed():
    from digitalmodel.subsea.capping_stack.containment_flow import solve_operating_point

    inputs = case_inputs()
    inputs["choke_capacity"] = lambda p: float("nan")
    with pytest.raises(ValueError):
        solve_operating_point(opening=1.0, **inputs)


def transient_case():
    """Construct an elastic single-density well with a measured-input form."""
    from digitalmodel.subsea.capping_stack.containment_flow import linear_inflow
    from digitalmodel.subsea.capping_stack.containment_capacity import G

    reservoir = 10e6 + 1000 * G * 1000
    return dict(inflow=lambda p: linear_inflow(p, reservoir, 1e-6),
                reservoir_pressure=reservoir, column_density=1000.0,
                vertical_depth=1000.0, gravity=G,
                bore_length=1000.0, bore_diameter=0.2, friction_factor=0.0,
                outlet_pressure=1e6,
                choke_capacity=lambda p: 1e-5*math.sqrt(max(p-1e6, 0)))


def test_native_shut_in_computes_peak_and_feeds_pressure_check():
    from digitalmodel.subsea.capping_stack.containment_flow import transient_shut_in

    result = transient_shut_in(
        time_s=[0, 0.01], openings=[1, 0], acoustic_speed=1000,
        cells=20, duration=8, reverse_productivity=1e-6,
        rated_working_pressure=10.5e6, **transient_case(),
    )
    assert result.peak_pressure_pa > 10.5e6
    assert result.peak_containment.siwhp == pytest.approx(result.peak_pressure_pa)
    assert not result.peak_containment.is_contained
    assert result.terminal_containment.siwhp == pytest.approx(10e6)
    assert result.terminal_containment.is_contained
    assert result.flow_rate_m3_s[0] > 0
    assert result.flow_rate_m3_s[-1] == 0


def test_native_first_step_matches_joukowsky_at_valve():
    from digitalmodel.subsea.capping_stack.containment_flow import transient_shut_in

    inputs = transient_case()
    result = transient_shut_in(
        time_s=[0, 0.01], openings=[1, 0], acoustic_speed=1000,
        cells=20, duration=2, reverse_productivity=1e-6,
        rated_working_pressure=20e6, **inputs,
    )
    velocity = result.flow_rate_m3_s[0] / (math.pi * inputs["bore_diameter"]**2 / 4)
    expected_rise = inputs["column_density"] * 1000 * velocity
    assert result.wellhead_pressure_pa[1] - result.wellhead_pressure_pa[0] == pytest.approx(
        expected_rise, rel=0.05
    )


def test_native_finite_closure_and_grid_refinement():
    from digitalmodel.subsea.capping_stack.containment_flow import transient_shut_in

    common = dict(time_s=[0, 4, 8], openings=[1, 0.5, 0],
                  acoustic_speed=1000, duration=10, reverse_productivity=1e-6,
                  rated_working_pressure=20e6, **transient_case())
    coarse = transient_shut_in(cells=20, **common)
    fine = transient_shut_in(cells=40, **common)
    assert fine.peak_pressure_pa == pytest.approx(coarse.peak_pressure_pa, rel=0.02)
    assert coarse.flow_rate_m3_s[-1] == 0
    assert 9e6 < coarse.wellhead_pressure_pa[-1] < 11e6


def test_native_open_state_preserves_steady_friction_solution():
    from digitalmodel.subsea.capping_stack.containment_flow import transient_shut_in

    inputs = transient_case()
    inputs["friction_factor"] = 0.02
    result = transient_shut_in(
        time_s=[0, 2, 4], openings=[1, 1, 0], acoustic_speed=1000,
        cells=20, duration=6, reverse_productivity=1e-6,
        rated_working_pressure=20e6, **inputs,
    )
    # The first two seconds hold the valve fully open.
    assert result.wellhead_pressure_pa[:40] == pytest.approx(
        [result.wellhead_pressure_pa[0]]*40, rel=1e-8
    )
    assert result.flow_rate_m3_s[:40] == pytest.approx(
        [result.flow_rate_m3_s[0]]*40, rel=1e-8
    )


@pytest.mark.parametrize("key,value", [("cells", 1), ("reverse_productivity", -1),
                                      ("duration", 0.001)])
def test_native_transient_rejects_invalid_grid_or_boundary(key, value):
    from digitalmodel.subsea.capping_stack.containment_flow import transient_shut_in

    inputs = dict(time_s=[0, 0.01], openings=[1, 0], acoustic_speed=1000,
                  cells=20, duration=2, reverse_productivity=1e-6,
                  rated_working_pressure=20e6, **transient_case())
    inputs[key] = value
    with pytest.raises(ValueError):
        transient_shut_in(**inputs)
