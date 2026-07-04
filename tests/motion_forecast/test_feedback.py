"""Operability-feedback tests (digitalmodel #1360)."""

import pytest

from digitalmodel.marine_ops.installation.go_no_go import DecisionState
from digitalmodel.motion_forecast.feedback import operability_summary

GO, MARG, NOGO = DecisionState.GO, DecisionState.MARGINAL, DecisionState.NO_GO


def test_operable_fraction_and_longest_run_go_only():
    states = [GO, GO, NOGO, GO, MARG, GO]
    s = operability_summary(states)
    assert s.operable_fraction == pytest.approx(4 / 6)   # GO only
    assert s.downtime_fraction == pytest.approx(2 / 6)
    assert s.waiting_on_weather_fraction == pytest.approx(2 / 6)
    assert s.longest_operable_run == 2                   # GO,GO
    assert s.n == 6


def test_marginal_operable_flag():
    states = [GO, GO, NOGO, GO, MARG, GO]
    s = operability_summary(states, marginal_operable=True)
    assert s.operable_fraction == pytest.approx(5 / 6)   # GO + MARGINAL
    assert s.longest_operable_run == 3                   # GO,MARG,GO


def test_empty_raises():
    with pytest.raises(ValueError, match="non-empty"):
        operability_summary([])
