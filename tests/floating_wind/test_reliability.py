"""Tests for the reliability -> OPEX driver (issue #1222).

Anchor: the deck's operations sensitivity (slide 17) -- failure-rate reduction
of 25 / 50 / 100% takes the $184 base to $173 / $162 / $139 per MWh.
"""

import pytest

from digitalmodel.floating_wind.economics import base_case, compute_lcoe
from digitalmodel.floating_wind.reliability import (
    ReliabilityScenario,
    apply_reliability,
    lcoe_with_reliability,
)

# Deck slide 17 operations sensitivity.
_DECK = {0.25: 173.0, 0.50: 162.0, 1.00: 139.0}
_TOL = 0.02  # +/- 2%


@pytest.fixture
def econ():
    return base_case()


@pytest.mark.parametrize("reduction,expected", list(_DECK.items()))
def test_reproduces_deck_operations_sensitivity(econ, reduction, expected):
    scenario = ReliabilityScenario(failure_rate_reduction=reduction)
    lcoe = lcoe_with_reliability(econ, scenario).lcoe_usd_per_mwh
    assert lcoe == pytest.approx(expected, rel=_TOL)


def test_zero_reduction_is_base_case(econ):
    scenario = ReliabilityScenario(failure_rate_reduction=0.0)
    base = compute_lcoe(econ).lcoe_usd_per_mwh
    assert lcoe_with_reliability(econ, scenario).lcoe_usd_per_mwh == pytest.approx(
        base, rel=1e-9
    )


def test_reduction_is_monotonic(econ):
    lcoes = [
        lcoe_with_reliability(
            econ, ReliabilityScenario(failure_rate_reduction=r)
        ).lcoe_usd_per_mwh
        for r in (0.0, 0.25, 0.5, 0.75, 1.0)
    ]
    assert lcoes == sorted(lcoes, reverse=True)  # strictly decreasing


def test_apply_reliability_cuts_opex_and_lifts_cf(econ):
    scenario = ReliabilityScenario(failure_rate_reduction=1.0)
    adj = apply_reliability(econ, scenario)
    assert adj.opex_per_kw_year < econ.opex_per_kw_year
    assert adj.capacity_factor > econ.capacity_factor
    assert adj.capacity_factor <= 1.0


def test_availability_gain_capped_at_full_cf():
    econ = base_case().model_copy(update={"capacity_factor": 0.99})
    adj = apply_reliability(
        econ, ReliabilityScenario(failure_rate_reduction=1.0)
    )
    assert adj.capacity_factor == pytest.approx(1.0)


def test_multipliers():
    s = ReliabilityScenario(
        failure_rate_reduction=0.5,
        failure_related_opex_fraction=0.8,
        base_availability_loss=0.10,
    )
    assert s.opex_multiplier == pytest.approx(1.0 - 0.8 * 0.5)
    assert s.availability_gain == pytest.approx(0.10 * 0.5 / 0.90)


def test_reduction_out_of_range_rejected():
    from pydantic import ValidationError

    with pytest.raises(ValidationError):
        ReliabilityScenario(failure_rate_reduction=1.5)
