"""Capacity-versus-demand checks using supplied pull rather than re-estimation."""
import pytest


def test_capacity_margin_and_storm_fail_without_reestimating_capacity():
    from digitalmodel.marine_ops.towing.bollard_pull import BollardPullCheck

    benign = BollardPullCheck(800000, 200000).check()
    assert benign.passed
    assert benign.required_pull_n == 200000
    assert benign.available_pull_n == 800000
    assert benign.margin_n == 600000
    storm = BollardPullCheck(800000, 900000).check()
    assert not storm.passed
    assert storm.margin_n == -100000


def test_load_result_can_be_supplied_with_explicit_derating_and_reserve():
    from digitalmodel.marine_ops.towing.bollard_pull import BollardPullCheck
    from digitalmodel.marine_ops.towing.drift_force import DriftForceResult

    loads = DriftForceResult(3000, 4000, 0, 0, 0)
    result = BollardPullCheck(10000, loads, efficiency=0.8, reserve_factor=1.5).check()
    assert result.required_pull_n == 7500
    assert result.available_pull_n == 8000
    assert result.margin_n == 500


@pytest.mark.parametrize('capacity,load', [(0, 1), (-1, 1), (1, -1),
    (float('nan'), 1), (1, float('inf'))])
def test_invalid_pull_inputs_rejected(capacity, load):
    from digitalmodel.marine_ops.towing.bollard_pull import BollardPullCheck

    with pytest.raises(ValueError):
        BollardPullCheck(capacity, load).check()
