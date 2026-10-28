# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent calculations for a constant-property, normally consolidated slab.

FHWA NHI-06-088 (2006), sections 5.5.6.1 and 7.5.3, equation 7-8:
https://www.fhwa.dot.gov/engineering/geotech/pubs/nhi06088.pdf
MIT 1.361 (2004), Part V-2, sections 1.1-1.2, pages 2-3:
https://ocw.mit.edu/courses/1-361-advanced-soil-mechanics-fall-2004/resources/part_v_2/
All dimensional expectations below are synthetic hand calculations, not outputs
saved from the production implementation. Numerical tolerances bound arithmetic.
"""

import math
from dataclasses import replace

import pytest

from digitalmodel.geotechnical.preload_consolidation import (
    ClayConsolidation,
    ConsolidationPreload,
    degree_of_consolidation,
    holding_response,
    recommend_minimum_hold,
)
from digitalmodel.marine_ops.preload_sequence import PreloadCriteria


def model():
    """Hand basis: delta sigma = 0.4*10000/100=40 kPa; s_inf=0.16 m.

    su0=0.2*100=20 kPa; su_inf=0.2*140=28 kPa. Uniform-clay
    circular reference factor 6.34 is the existing research model's equation 5,
    cited in geotechnical.spudcan, not a standard acceptance factor.
    """
    clay = ClayConsolidation(4, 2, 1, 0.001, 100, 0.2)
    return ConsolidationPreload(clay, 100, 10000, 0.4)


@pytest.mark.parametrize('tv,u', [(0, 0), (0.197, 0.5), (0.848, 0.9)])
def test_fhwa_selected_time_factors(tv, u):
    # Two selected values from FHWA Table 7-4; table precision is three places.
    assert degree_of_consolidation(tv) == pytest.approx(u, abs=0.0005)


def test_short_time_and_two_mode_solution():
    # At Tv=.001 the image correction is O(exp(-1000)); at Tv=1 the
    # third eigenmode contributes less than 1e-28. These independent truncated
    # analytical expressions bound their omitted terms without calling the model.
    assert degree_of_consolidation(0.001) == pytest.approx(
        2 * math.sqrt(0.001 / math.pi), abs=1e-14)
    u1 = 1 - 8 / math.pi**2 * (
        math.exp(-math.pi**2 / 4) + math.exp(-9 * math.pi**2 / 4) / 9)
    assert degree_of_consolidation(1) == pytest.approx(u1, abs=1e-14)
    assert degree_of_consolidation(10000) == 1


def test_rapid_preload_is_undrained_then_gains_strength_and_settles():
    rapid = holding_response(model(), 0)
    assert rapid.degree_of_consolidation == 0
    assert rapid.settlement_m == 0
    assert rapid.undrained_strength_kpa == 20
    assert rapid.capacity_kn == pytest.approx(12680)
    assert rapid.margin_ratio == pytest.approx(0.268)
    # t=4 h gives Tv=1. All multipliers are independently calculated above.
    u1 = 1 - 8 / math.pi**2 * math.exp(-math.pi**2 / 4)
    held = holding_response(model(), 4)
    assert held.settlement_m == pytest.approx(0.16 * u1, abs=1e-10)
    assert held.undrained_strength_kpa == pytest.approx(20 + 8 * u1, abs=1e-8)
    assert held.capacity_kn == pytest.approx(634 * (20 + 8 * u1), abs=1e-5)
    assert rapid.margin_ratio < held.margin_ratio
    final = holding_response(model(), 10000)
    assert final.settlement_m == pytest.approx(0.16)
    assert final.capacity_kn == pytest.approx(17752)


def test_linear_ramp_uses_load_superposition_including_early_time():
    # Ramp=4 h -> Tv_r=1. Analytic integral of each independent eigenmode:
    # Ubar=1-sum[32/(pi^4*j^4)*(1-exp(-j^2*pi^2/4))], odd j.
    # Tail beyond j=20001 is below 2e-14. No production helper is reused.
    expected = 1 - sum(32 / (math.pi**4 * j**4) *
                       (1 - math.exp(-j*j*math.pi**2/4))
                       for j in range(1, 20002, 2))
    slow = holding_response(model(), 0, ramp_duration_h=4)
    assert slow.degree_of_consolidation == pytest.approx(expected, abs=1e-12)
    assert slow.settlement_m == pytest.approx(0.16 * expected)
    # Integrate 2 sqrt(T/pi) over [0,.001]; mean is 4 sqrt(.001/pi)/3.
    early = holding_response(model(), 0, ramp_duration_h=0.004)
    assert early.degree_of_consolidation == pytest.approx(
        4 / 3 * math.sqrt(0.001 / math.pi), abs=1e-14)
    tiny = holding_response(model(), 4, ramp_duration_h=1e-12)
    step = holding_response(model(), 4)
    assert tiny.settlement_m == pytest.approx(step.settlement_m, abs=1e-13)
    assert slow.margin_ratio > holding_response(model(), 0).margin_ratio


def test_drainage_length_squared_scaling():
    # Single drainage doubles Hd, so four times the duration gives the same Tv.
    single = replace(model(), clay=replace(model().clay, drainage_path_m=4))
    assert holding_response(single, 16).settlement_m == pytest.approx(
        holding_response(model(), 4).settlement_m)


def test_tiny_ramp_crossing_short_time_boundary_has_no_cancellation():
    # Mean of 2 sqrt(T/pi) over a symmetric interval about .01 equals
    # its midpoint value to O((width/.01)^2), below 1e-25 here.
    result = holding_response(model(), 0.04 - 4e-16, ramp_duration_h=8e-16)
    assert result.degree_of_consolidation == pytest.approx(
        2 * math.sqrt(0.01 / math.pi), abs=1e-13)


def test_recommendation_matches_independent_late_time_root():
    criteria = PreloadCriteria(settlement_rate_limit_m_h=0.0001)
    # The first eigenmode is sufficient here: omitted next mode < 1e-23.
    # s(t)=.16*(1-8/pi² exp(-k*t)), k=pi²/16 per hour.
    # [s(t)-s(t-w)]/w = .16*8/pi²*(exp(k*w)-1)/w*exp(-k*t).
    k, window, limit = math.pi**2 / 16, 0.25, 0.0001
    expected = math.log(0.16 * 8 / math.pi**2 *
                        math.expm1(k * window) / window / limit) / k
    result = recommend_minimum_hold(model(), criteria=criteria, max_hold_h=100)
    assert result.holding_time_h == pytest.approx(expected, abs=1e-8)
    assert result.settlement_rate_m_h < limit
    assert result.criteria == criteria
    assert result.reason == 'criterion_reached'
    # This is a strict threshold. A slightly earlier complete window fails.
    t = result.holding_time_h - 1e-5
    independent_rate = 0.16 * 8 / math.pi**2 * math.expm1(k*window)/window*math.exp(-k*t)
    assert independent_rate > limit


def test_recommendation_has_floor_complete_window_and_finite_horizon():
    loose = PreloadCriteria(settlement_rate_limit_m_h=1)
    assert recommend_minimum_hold(model(), criteria=loose).holding_time_h == 1
    wide = replace(loose, settlement_window_h=2)
    assert recommend_minimum_hold(model(), criteria=wide).holding_time_h == 2
    result = recommend_minimum_hold(model(), max_hold_h=1)
    assert result.holding_time_h is None
    assert result.reason == 'criterion_not_reached_within_horizon'
    # Long drainage can have a low rate before sufficient strength gain.
    weak = replace(model(), target_load_kn=20000)
    assert holding_response(weak, 0).margin_ratio < 0


@pytest.mark.parametrize('field,value', [
    ('thickness_m', 0), ('drainage_path_m', 3), ('cv_m2_h', 0),
    ('mv_m2_kn', -1), ('initial_effective_stress_kpa', float('nan')),
    ('normalized_strength_ratio', True), ('normalized_strength_ratio', 0),
])
def test_invalid_clay_inputs(field, value):
    invalid = replace(model(), clay=replace(model().clay, **{field: value}))
    with pytest.raises(ValueError):
        holding_response(invalid, 1)


@pytest.mark.parametrize('field,value', [
    ('area_m2', 0), ('target_load_kn', float('inf')),
    ('stress_influence_factor', 1.1), ('stress_influence_factor', 0),
    ('initial_penetration_m', -1),
])
def test_invalid_load_inputs(field, value):
    with pytest.raises(ValueError):
        holding_response(replace(model(), **{field: value}), 1)


@pytest.mark.parametrize('time', [-1, True, float('inf'), float('nan')])
def test_invalid_times(time):
    with pytest.raises(ValueError):
        degree_of_consolidation(time)
    with pytest.raises(ValueError):
        holding_response(model(), time)
    with pytest.raises(ValueError):
        holding_response(model(), 1, ramp_duration_h=time)


def test_large_strain_and_invalid_recommendation_controls():
    with pytest.raises(ValueError, match='strain'):
        holding_response(replace(model(), clay=replace(model().clay, mv_m2_kn=1)), 1)
    with pytest.raises(ValueError):
        recommend_minimum_hold(model(), criteria=PreloadCriteria(minimum_hold_h=0))
    with pytest.raises(ValueError):
        recommend_minimum_hold(model(), max_hold_h=0)
