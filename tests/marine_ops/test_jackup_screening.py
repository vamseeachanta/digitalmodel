# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent statics checks for an illustrative three-support unit."""

import math

import pytest

from digitalmodel.marine_ops.jackup_screening import (
    environmental_actions,
    screen_three_leg_unit,
    static_operating_envelope,
)


SUPPORTS = ((20.0, 0.0), (-10.0, 20.0), (-10.0, -20.0))


def screen(**changes):
    values = dict(vertical_load_kn=90000.0, supports_m=SUPPORTS,
                  moment_x_knm=0.0, moment_y_knm=0.0, horizontal_force_kn=0.0,
                  preload_per_leg_kn=50000.0, sliding_resistance_kn=10000.0)
    values.update(changes)
    return screen_three_leg_unit(**values)


def test_centred_load_has_equal_reactions():
    result = screen()
    assert result.reactions_kn == pytest.approx((30000, 30000, 30000))
    assert result.admissible


def test_biaxial_moments_satisfy_independent_equilibrium():
    result = screen(moment_x_knm=180000, moment_y_knm=300000)
    r = result.reactions_kn
    assert sum(r) == pytest.approx(90000)
    assert sum(x * load for (x, y), load in zip(SUPPORTS, r)) == pytest.approx(300000)
    assert sum(y * load for (x, y), load in zip(SUPPORTS, r)) == pytest.approx(-180000)


def test_overturning_preload_and_sliding_report_separately():
    assert not screen(moment_y_knm=-1800000).contact_ok
    assert not screen(moment_y_knm=1800000).preload_ok
    assert not screen(horizontal_force_kn=10001).sliding_ok
    assert screen(horizontal_force_kn=10000).sliding_ok
    assert not screen(moment_y_knm=-900000).contact_ok  # zero reaction is marginal


@pytest.mark.parametrize('changes', [
    {'vertical_load_kn': 0}, {'moment_x_knm': float('inf')},
    {'preload_per_leg_kn': float('nan')}, {'horizontal_force_kn': -1},
    {'supports_m': ((0, 0), (1, 1), (2, 2))},
    {'supports_m': ((0, 0), (0, 0), (1, 1))},
    {'supports_m': ((0, 0), (1, 1))}, {'sliding_resistance_kn': True},
    {'supports_m': ((0, 0), (1, float('nan')), (0, 1))},
])
def test_invalid_statics_rejected(changes):
    with pytest.raises(ValueError):
        screen(**changes)


def test_environmental_loads_reduce_to_drag_with_zero_wave():
    a = environmental_actions(
        hs_m=0, tp_s=10, wind_m_s=20, current_m_s=1,
        water_depth_m=60, air_gap_m=15, wind_area_m2=1000,
        submerged_area_m2=100, wind_drag_coefficient=1.2, water_drag_coefficient=1.0)
    wind = 0.5 * 1.225 * 1.2 * 1000 * 20**2 / 1000
    current = 0.5 * 1025 * 100 * 1**2 / 1000
    assert a.horizontal_force_kn == pytest.approx(wind + current)
    assert a.moment_y_knm == pytest.approx(wind * 75 + current * 30)


def test_wave_load_increases_for_same_period():
    args = dict(tp_s=10, wind_m_s=20, current_m_s=1, water_depth_m=60,
                air_gap_m=15, wind_area_m2=1000, submerged_area_m2=100,
                wind_drag_coefficient=1.2, water_drag_coefficient=1)
    assert environmental_actions(hs_m=2, **args).horizontal_force_kn > environmental_actions(hs_m=1, **args).horizontal_force_kn


def test_static_envelope_reports_sampled_bounds_and_no_false_safe_reentry():
    def evaluate(hs, tp):
        return screen(horizontal_force_kn=hs**2 * 1000)
    result = static_operating_envelope([6, 10], [0, 1, 2, 3, 4], evaluate)
    assert result[0].hs_limit_m == 3
    assert result[0].first_failing_hs_m == 4
    assert result[0].status == 'bounded_on_grid'
    assert result[0].governing_checks == ('sliding',)
    assert static_operating_envelope([10], [0, 1], evaluate)[0].status == 'grid_capped'
    assert static_operating_envelope([10], [4, 5], evaluate)[0].hs_limit_m is None
    def nonmonotone(hs, tp):
        return screen(horizontal_force_kn=20000 if hs == 1 else 0)
    with pytest.raises(ValueError, match='re-entry'):
        static_operating_envelope([10], [0, 1, 2], nonmonotone)


@pytest.mark.parametrize('periods,heights', [([], [0, 1]), ([0], [0, 1]), ([10], []),
                                         ([10], [2, 1]), ([10], [1, 1]), ([math.nan], [0, 1])])
def test_invalid_envelope_grids_rejected(periods, heights):
    with pytest.raises(ValueError):
        static_operating_envelope(periods, heights, lambda h, t: screen())


def test_existing_envelope_entrypoint_uses_static_criteria():
    from digitalmodel.marine_ops.operation_envelope import jackup_operating_envelope
    points = jackup_operating_envelope([8.0], [0.0, 1.0], lambda h, t: screen())
    assert points[0].status == 'grid_capped'
    assert points[0].hs_limit_m == 1.0
