"""Tests for the subsea-manifold multi-tree flow-aggregation core (API 17P)."""

from __future__ import annotations

import math
from itertools import permutations

import pytest

from digitalmodel.subsea.manifolds.flow_aggregator import aggregate_flow


def test_aggregate_two_wells_hand_verified():
    # oil: 5000+3000=8000; gas: 5e6+3e6=8e6; water: 1000+2000=3000
    # liquid=11000; total=11000+8e6=8_011_000; wc=3000/11000; gor=8e6/8000=1000
    wells = [
        {"q_oil": 5000.0, "q_gas": 5_000_000.0, "q_water": 1000.0},
        {"q_oil": 3000.0, "q_gas": 3_000_000.0, "q_water": 2000.0},
    ]
    r = aggregate_flow(wells)
    assert r.n_wells == 2
    assert math.isclose(r.q_oil, 8000.0)
    assert math.isclose(r.q_gas, 8_000_000.0)
    assert math.isclose(r.q_water, 3000.0)
    assert math.isclose(r.q_liquid, 11000.0)
    assert math.isclose(r.q_total, 8_011_000.0)
    assert math.isclose(r.water_cut, 3000.0 / 11000.0)
    assert math.isclose(r.gor, 1000.0)
    assert r.within_capacity is None


def test_within_capacity_pass():
    wells = [{"q_oil": 5000.0, "q_water": 1000.0}]
    r = aggregate_flow(wells, q_capacity=10000.0)
    assert math.isclose(r.q_total, 6000.0)
    assert r.within_capacity is True


def test_over_capacity_fail():
    wells = [{"q_oil": 5000.0, "q_water": 1000.0}]
    r = aggregate_flow(wells, q_capacity=5000.0)
    assert r.within_capacity is False


def test_boundary_capacity_equal_is_within():
    wells = [{"q_oil": 5000.0, "q_water": 1000.0}]
    r = aggregate_flow(wells, q_capacity=6000.0)
    assert r.within_capacity is True


def test_zero_liquid_and_zero_oil_give_none_ratios():
    # gas-only well: liquid=0 -> water_cut None; oil=0 -> gor None
    wells = [{"q_gas": 1_000_000.0}]
    r = aggregate_flow(wells)
    assert r.water_cut is None
    assert r.gor is None
    assert math.isclose(r.q_total, 1_000_000.0)


def test_missing_phase_keys_default_zero():
    wells = [{"q_oil": 4000.0}]
    r = aggregate_flow(wells)
    assert math.isclose(r.q_gas, 0.0)
    assert math.isclose(r.q_water, 0.0)
    assert math.isclose(r.q_total, 4000.0)


def test_invalid_inputs_raise():
    with pytest.raises(ValueError):
        aggregate_flow([])
    with pytest.raises(ValueError):
        aggregate_flow([{"q_oil": -1.0}])
    with pytest.raises(ValueError):
        aggregate_flow([{"q_oil": 1.0}], q_capacity=-1.0)


@pytest.fixture
def synthetic_wells():
    """Supply invented volumetric rates on one common unit and reference basis.

    Source: independent hand calculation, not standard or field data.  The
    (oil, gas, water) triples are (12.5, 2, 0.5), (3, 6.5, 4.5), (0.5, 1.5, 1).
    Oil = 12.5 + 3 + 0.5 = 16; gas = 2 + 6.5 + 1.5 = 10;
    water = 0.5 + 4.5 + 1 = 6; liquid = 16 + 6 = 22.
    Per-well totals are 15, 14, 3, so total = 15 + 14 + 3 = 32.
    Water cut = 6 / (16 + 6) = 3 / 11; gas/oil ratio = 10 / 16 = 5 / 8.
    Binary-exact phase inputs permit exact rate assertions in every order.
    """
    return [
        {"q_oil": 12.5, "q_gas": 2.0, "q_water": 0.5},
        {"q_oil": 3.0, "q_gas": 6.5, "q_water": 4.5},
        {"q_oil": 0.5, "q_gas": 1.5, "q_water": 1.0},
    ]


@pytest.mark.parametrize(
    "order", permutations(range(3)), ids=["abc", "acb", "bac", "bca", "cab", "cba"]
)
def test_unequal_streams_conserve_phases_in_every_order(synthetic_wells, order):
    """Each ordering must match the independent balances in synthetic_wells.

    Oil/gas/water sums are 16/10/6; liquid = 22 and total = 32.
    Water cut = 6 / 22 = 3 / 11; gas/oil ratio = 10 / 16 = 5 / 8.
    Matching the same exact expectations also establishes order invariance.
    """
    r = aggregate_flow([synthetic_wells[index] for index in order])

    assert r.n_wells == 3
    assert r.q_oil == 16.0
    assert r.q_gas == 10.0
    assert r.q_water == 6.0
    assert r.q_liquid == 22.0
    assert r.q_total == 32.0
    assert r.water_cut == 3 / 11
    assert r.gor == 5 / 8
    assert r.within_capacity is None


@pytest.mark.parametrize("position", range(4), ids=["first", "second", "third", "last"])
def test_explicit_zero_rate_contributor_preserves_balance(synthetic_wells, position):
    """An explicit zero-rate dict is valid and counts as an additional well.

    Source: the synthetic_wells hand balance with zero added to every phase:
    oil = 16 + 0; gas = 10 + 0; water = 6 + 0; liquid = 22; total = 32.
    Water cut remains 6 / 22 = 3 / 11 and gas/oil remains 10 / 16 = 5 / 8.
    Three producing streams plus the zero-rate stream give four wells.
    """
    wells = synthetic_wells.copy()
    wells.insert(position, {"q_oil": 0.0, "q_gas": 0.0, "q_water": 0.0})
    r = aggregate_flow(wells)

    assert r.n_wells == 4
    assert r.q_oil == 16.0
    assert r.q_gas == 10.0
    assert r.q_water == 6.0
    assert r.q_liquid == 22.0
    assert r.q_total == 32.0
    assert r.water_cut == 3 / 11
    assert r.gor == 5 / 8
    assert r.within_capacity is None


def test_single_explicit_zero_rate_well_has_undefined_ratios():
    """The nonempty zero-rate input satisfies the existing per-phase contract.

    Source: hand balance, oil = gas = water = 0, liquid = 0 + 0 = 0,
    total = 0 + 0 + 0 = 0.  Both water cut = 0 / 0 and gas/oil = 0 / 0
    are undefined, represented by None.  The supplied well still counts as one.
    """
    r = aggregate_flow([{"q_oil": 0.0, "q_gas": 0.0, "q_water": 0.0}])

    assert r.n_wells == 1
    assert r.q_oil == 0.0
    assert r.q_gas == 0.0
    assert r.q_water == 0.0
    assert r.q_liquid == 0.0
    assert r.q_total == 0.0
    assert r.water_cut is None
    assert r.gor is None
    assert r.within_capacity is None
