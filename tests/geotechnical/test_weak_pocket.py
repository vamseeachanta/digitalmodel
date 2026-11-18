# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic weak-pocket checks with independently calculated intersections."""

from dataclasses import replace

import pytest

from digitalmodel.geotechnical.weak_pocket import (
    CohesiveLayer, WeakPocket, weak_pocket_sensitivity,
)


LAYERS = (
    CohesiveLayer('upper', 0, 2, 0, 40),
    CohesiveLayer('lower', 2, 14, 60, 120),
)
POCKET = WeakPocket('lower', (1.0, 0.9, 0.8, 0.7, 0.6, 0.5), 2, 12, 75)


def screen(**changes):
    values = dict(layers=LAYERS, area_m2=160, pocket=POCKET,
                  preload_kn=45000, storm_kn=50000,
                  assumed_settlement_rate_m_s=0.1)
    values.update(changes)
    return weak_pocket_sensitivity(**values)


def test_mild_pocket_has_no_capacity_drop_and_retains_rationale():
    result = screen(pocket=replace(POCKET, strength_factors=(0.8,)))
    assert result.status == 'resolved'
    assert result.first_flag_factor is None
    assert result.pocket.nearest_survey_distance_m == 75
    case, = result.cases
    assert not case.punch_through_flag
    preload, storm = case.reactions
    assert preload.reduced_penetration_m == 2
    assert preload.penetration_change_m == 0
    # q = 6.34 su; lower su = 0.8 * [60 + 5 (z - 2)].
    expected = 2 + (50000 / (6.34 * 160 * 0.8) - 60) / 5
    assert storm.reduced_penetration_m == pytest.approx(expected)
    assert all(reaction.drop is None for reaction in case.reactions)


def test_sweep_reports_first_flag_and_independent_drop_distance_and_time():
    result = screen()
    assert result.first_flag_factor == 0.6
    assert [case.punch_through_flag for case in result.cases] == [False] * 4 + [True] * 2
    case = result.cases[4]
    for reaction, load in zip(case.reactions, (45000, 50000)):
        expected = 2 + (load / (6.34 * 160 * 0.6) - 60) / 5
        assert reaction.load_kn == load
        assert reaction.baseline_penetration_m == 2
        assert reaction.reduced_penetration_m == pytest.approx(expected)
        assert reaction.penetration_change_m == pytest.approx(expected - 2)
        assert reaction.load_reaches_drop
        drop = reaction.drop
        assert drop.peak_depth_m == 2
        assert drop.recovery_depth_m == pytest.approx(expected)
        assert drop.jump_m == pytest.approx(expected - 2)
        assert drop.duration_s == pytest.approx((expected - 2) / 0.1)
        assert drop.duration_order_s == 10
        assert drop.model == 'assumed_constant_settlement_rate'


def test_drop_distance_and_duration_grow_as_strength_is_reduced():
    result = screen(pocket=replace(POCKET, strength_factors=(0.6, 0.55, 0.5)))
    for index in (0, 1):
        drops = [case.reactions[index].drop for case in result.cases]
        assert all(b.jump_m > a.jump_m for a, b in zip(drops, drops[1:]))
        assert all(b.duration_s > a.duration_s for a, b in zip(drops, drops[1:]))


def test_warning_is_independent_of_load_reaching_the_peak():
    result = screen(preload_kn=20000, storm_kn=30000)
    reaction = result.cases[-1].reactions[0]
    assert result.cases[-1].punch_through_flag
    assert not reaction.load_reaches_drop
    assert reaction.drop is None
    assert reaction.penetration_change_m == 0


def test_recovery_at_pocket_base_uses_restored_strength_without_extrapolation():
    result = screen(pocket=replace(POCKET, strength_factors=(0.1,), bottom_m=5))
    reaction = result.cases[0].reactions[0]
    assert reaction.reduced_penetration_m == 5
    assert reaction.drop.jump_m == 3
    # Both sides of the pocket boundary are retained at the same depth.
    boundary = [p.load_kn for p in result.cases[0].curve if p.depth_m == 5]
    assert boundary == pytest.approx([6.34 * 160 * 7.5, 6.34 * 160 * 75])


def test_no_recovery_within_supplied_depth_is_unresolved():
    result = screen(storm_kn=200000)
    assert result.status == 'unresolved'
    reaction = result.cases[-1].reactions[1]
    assert reaction.status == 'unresolved'
    assert reaction.reduced_penetration_m is None
    assert reaction.penetration_change_m is None
    assert reaction.drop is None
    assert result.cases[-1].punch_through_flag


@pytest.mark.parametrize('key', [
    'layers', 'area_m2', 'pocket', 'preload_kn', 'storm_kn', 'assumed_settlement_rate_m_s',
])
def test_missing_case_inputs_are_unresolved(key):
    result = screen(**{key: None})
    assert result.status == 'unresolved'
    assert key in result.missing_inputs
    assert result.cases == ()
    assert result.first_flag_factor is None


@pytest.mark.parametrize('key', [
    'layer', 'strength_factors', 'top_m', 'bottom_m', 'nearest_survey_distance_m',
])
def test_missing_pocket_inputs_are_unresolved(key):
    result = screen(pocket=replace(POCKET, **{key: None}))
    assert result.status == 'unresolved'
    assert f'pocket.{key}' in result.missing_inputs
    assert result.cases == ()


@pytest.mark.parametrize('changes', [
    {'strength_factors': ()}, {'strength_factors': (0.5, 0.8)},
    {'strength_factors': (1.1,)}, {'strength_factors': (0,)},
    {'strength_factors': (True,)}, {'strength_factors': (float('nan'),)},
    {'strength_factors': (0.8, 0.8)}, {'top_m': 1}, {'bottom_m': 15},
    {'bottom_m': 2}, {'nearest_survey_distance_m': -1}, {'layer': 'absent'},
])
def test_invalid_pocket_inputs_rejected(changes):
    with pytest.raises(ValueError):
        screen(pocket=replace(POCKET, **changes))


@pytest.mark.parametrize('changes', [
    {'area_m2': 0}, {'preload_kn': -1}, {'storm_kn': float('inf')},
    {'assumed_settlement_rate_m_s': 0},
    {'layers': (LAYERS[0], replace(LAYERS[1], top_m=3))},
    {'layers': (replace(LAYERS[0], bottom_su_kpa=-1), LAYERS[1])},
])
def test_invalid_profile_and_reaction_inputs_rejected(changes):
    with pytest.raises(ValueError):
        screen(**changes)


def test_survey_distance_is_rationale_only_and_partial_layer_is_supported():
    pocket = replace(POCKET, top_m=4, bottom_m=6, strength_factors=(0.5,))
    result = screen(pocket=pocket)
    distant = screen(pocket=replace(pocket, nearest_survey_distance_m=500))
    assert result.cases == distant.cases
    depths = [point.depth_m for point in result.cases[0].curve]
    assert 4 in depths and 6 in depths
    assert result.cases[0].punch_through_flag


def test_load_equal_to_peak_follows_post_peak_recovery():
    result = screen(preload_kn=6.34 * 160 * 40)
    reaction = result.cases[-1].reactions[0]
    assert reaction.load_reaches_drop
    assert reaction.reduced_penetration_m == pytest.approx(6)
    assert reaction.drop.jump_m == pytest.approx(4)


def test_load_at_flat_peak_follows_drop_at_end_of_plateau():
    layers = (CohesiveLayer('upper', 0, 1, 0, 40),
              CohesiveLayer('plateau', 1, 2, 40, 40), LAYERS[1])
    result = screen(layers=layers, preload_kn=6.34 * 160 * 40)
    reaction = result.cases[-1].reactions[0]
    assert reaction.load_reaches_drop
    assert reaction.drop.peak_depth_m == 2
    assert reaction.drop.jump_m == pytest.approx(4)


def test_missing_layer_strength_is_unresolved():
    result = screen(layers=(LAYERS[0], replace(LAYERS[1], top_su_kpa=None)))
    assert result.status == 'unresolved'
    assert 'layers[1].top_su_kpa' in result.missing_inputs
    assert result.cases == ()
