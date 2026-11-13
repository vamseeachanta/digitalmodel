"""Exhaustive grid selection under equilibrium, clearance and scope constraints."""
import math

import pytest


def test_grid_selects_lowest_feasible_peak_tension():
    from digitalmodel.marine_ops.towing.config_optimizer import optimize_configuration

    result = optimize_configuration(
        horizontal_load_n=100000, bridle_angles_deg=[30, 60],
        line_lengths_m=[500, 600], line_diameters_m=[0.04, 0.05],
        reference_diameter_m=0.05, reference_weight_n_m=100,
        reference_mbl_n=600000, available_depth_m=100,
        minimum_clearance_m=10, scope_bounds=(1.001, 1.1),
        bridle_angle_bounds_deg=(20, 90), fairlead_capacity_n=500000,
        dynamic_amplification=1.3,
    )
    # 40 mm MBL = 384 kN < 3 * 131.65 kN dynamic demand: all four fail.
    assert result.feasible_count == 4
    assert result.line_length_m == 500
    assert result.line_diameter_m == 0.05
    assert result.bridle_angle_deg == 30
    assert result.peak_tension_n == pytest.approx(1.3 * math.hypot(100000, 100 * 500/2))
    assert result.bridle_leg_tension_n == pytest.approx(
        result.peak_tension_n / (2 * math.cos(math.radians(15))))
    assert all(check.passed for check in result.checks.values())


def test_infeasible_grid_is_explicit_and_never_returns_a_failed_candidate():
    from digitalmodel.marine_ops.towing.config_optimizer import optimize_configuration

    with pytest.raises(ValueError, match='no feasible'):
        optimize_configuration(
            horizontal_load_n=100000, bridle_angles_deg=[60],
            line_lengths_m=[600], line_diameters_m=[0.05],
            reference_diameter_m=0.05, reference_weight_n_m=100,
            reference_mbl_n=10000, available_depth_m=10,
            minimum_clearance_m=9, scope_bounds=(1, 1.1),
            bridle_angle_bounds_deg=(20, 90), fairlead_capacity_n=100,
            dynamic_amplification=1.3,
        )


@pytest.mark.parametrize('constraint', [
    {'minimum_clearance_m': 99}, {'scope_bounds': (1.05, 1.1)},
    {'fairlead_capacity_n': 100}, {'bridle_angle_bounds_deg': (70, 90)},
    {'reference_mbl_n': 1000},
])
def test_each_constraint_independently_rejects_grid(constraint):
    from digitalmodel.marine_ops.towing.config_optimizer import optimize_configuration

    args = dict(horizontal_load_n=100000, bridle_angles_deg=[60],
                line_lengths_m=[600], line_diameters_m=[0.05],
                reference_diameter_m=0.05, reference_weight_n_m=100,
                reference_mbl_n=600000, available_depth_m=100,
                minimum_clearance_m=10, scope_bounds=(1, 1.1),
                bridle_angle_bounds_deg=(20, 90), fairlead_capacity_n=500000,
                dynamic_amplification=1.3)
    assert optimize_configuration(**args).feasible_count == 1
    with pytest.raises(ValueError, match='no feasible'):
        optimize_configuration(**(args | constraint))


@pytest.mark.parametrize('overrides', [
    {'dynamic_amplification': 0.9}, {'scope_bounds': (0.9, 1.1)},
    {'reference_mbl_n': float('nan')}, {'line_lengths_m': []},
    {'line_diameters_m': [-0.1]}, {'bridle_angles_deg': [180]},
])
def test_invalid_grid_inputs_never_receive_a_verdict(overrides):
    from digitalmodel.marine_ops.towing.config_optimizer import optimize_configuration

    args = dict(horizontal_load_n=100000, bridle_angles_deg=[60],
                line_lengths_m=[600], line_diameters_m=[0.05],
                reference_diameter_m=0.05, reference_weight_n_m=100,
                reference_mbl_n=600000, available_depth_m=100,
                minimum_clearance_m=10, scope_bounds=(1, 1.1),
                bridle_angle_bounds_deg=(20, 90), fairlead_capacity_n=500000,
                dynamic_amplification=1.3)
    with pytest.raises(ValueError):
        optimize_configuration(**(args | overrides))
