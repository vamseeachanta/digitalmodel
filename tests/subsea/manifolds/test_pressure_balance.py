"""Tests for the subsea-manifold well-to-header pressure-balance core (API 17P)."""

from __future__ import annotations

import math

import pytest

from digitalmodel.subsea.manifolds.pressure_balance import (
    manifold_balance,
    well_balance,
)


def test_well_within_limit_hand_verified():
    # p_arrival = 320 - 15 = 305; dp_choke = 305 - 250 = 55; <= 80 -> feasible
    r = well_balance("A", p_tree=320.0, p_header=250.0, dp_jumper=15.0, dp_choke_max=80.0)
    assert math.isclose(r.p_arrival, 305.0)
    assert math.isclose(r.dp_choke, 55.0)
    assert r.feasible is True
    assert r.back_pressured is False
    assert r.choke_over_ranged is False


def test_well_back_pressured_fails():
    # p_arrival = 240 - 10 = 230 < 250 -> back-pressured, infeasible
    r = well_balance("B", p_tree=240.0, p_header=250.0, dp_jumper=10.0)
    assert math.isclose(r.p_arrival, 230.0)
    assert math.isclose(r.dp_choke, -20.0)
    assert r.back_pressured is True
    assert r.feasible is False


def test_well_choke_over_ranged_fails():
    # p_arrival = 400; dp_choke = 150 > dp_choke_max 100 -> over-ranged, infeasible
    r = well_balance("C", p_tree=400.0, p_header=250.0, dp_jumper=0.0, dp_choke_max=100.0)
    assert math.isclose(r.dp_choke, 150.0)
    assert r.choke_over_ranged is True
    assert r.feasible is False
    assert r.back_pressured is False


def test_well_boundary_arrival_equals_header_is_feasible():
    # eq. 2 uses >= so p_arrival == p_header is feasible, dp_choke = 0
    r = well_balance("D", p_tree=250.0, p_header=250.0, dp_jumper=0.0)
    assert math.isclose(r.dp_choke, 0.0)
    assert r.back_pressured is False
    assert r.feasible is True


def test_well_boundary_choke_equals_max_is_feasible():
    # dp_choke = 80 exactly equals dp_choke_max 80 -> not over-ranged
    r = well_balance("E", p_tree=330.0, p_header=250.0, dp_jumper=0.0, dp_choke_max=80.0)
    assert math.isclose(r.dp_choke, 80.0)
    assert r.choke_over_ranged is False
    assert r.feasible is True


def test_unbounded_choke_authority_default():
    r = well_balance("F", p_tree=900.0, p_header=250.0)
    assert math.isclose(r.dp_choke, 650.0)
    assert r.choke_over_ranged is False
    assert r.feasible is True


def test_manifold_balance_all_feasible():
    wells = [
        {"well_id": "W1", "p_tree": 320.0, "dp_jumper": 15.0, "dp_choke_max": 80.0},
        {"well_id": "W2", "p_tree": 330.0, "dp_jumper": 0.0, "dp_choke_max": 80.0},
    ]
    res = manifold_balance(wells, p_header=250.0)
    assert res.balanced is True
    assert res.n_infeasible == 0
    assert len(res.wells) == 2


def test_manifold_balance_one_infeasible():
    wells = [
        {"well_id": "W1", "p_tree": 320.0, "dp_jumper": 15.0},
        {"well_id": "W2", "p_tree": 240.0, "dp_jumper": 10.0},  # back-pressured
    ]
    res = manifold_balance(wells, p_header=250.0)
    assert res.balanced is False
    assert res.n_infeasible == 1
    assert res.wells[1].back_pressured is True


def test_invalid_inputs_raise():
    with pytest.raises(ValueError):
        well_balance("X", p_tree=-1.0, p_header=250.0)
    with pytest.raises(ValueError):
        well_balance("X", p_tree=300.0, p_header=-1.0)
    with pytest.raises(ValueError):
        well_balance("X", p_tree=300.0, p_header=250.0, dp_jumper=-5.0)
    with pytest.raises(ValueError):
        well_balance("X", p_tree=300.0, p_header=250.0, dp_choke_max=-5.0)
    with pytest.raises(ValueError):
        manifold_balance([], p_header=250.0)
