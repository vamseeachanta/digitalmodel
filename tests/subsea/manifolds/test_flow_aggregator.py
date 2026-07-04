"""Tests for the subsea-manifold multi-tree flow-aggregation core (API 17P)."""

from __future__ import annotations

import math

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
