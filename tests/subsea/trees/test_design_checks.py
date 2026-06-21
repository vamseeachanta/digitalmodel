"""Tests for the API 17D / API 6A subsea-tree P-T rating envelope check."""

from __future__ import annotations

import math

import pytest

from digitalmodel.subsea.trees.design_checks import (
    API_6A_RATED_WORKING_PRESSURES_PSI,
    API_6A_TEMPERATURE_CLASSES_F,
    select_rated_working_pressure,
    tree_rating_check,
)


# ---------------------------------------------------------------------------
# select_rated_working_pressure
# ---------------------------------------------------------------------------


def test_select_rwp_picks_smallest_containing_rating_hand_verified():
    # 8200 psi well -> smallest API 6A RWP >= 8200 is 10000 psi.
    assert select_rated_working_pressure(8200.0) == 10000


def test_select_rwp_exact_match_returns_that_rating():
    assert select_rated_working_pressure(5000.0) == 5000


def test_select_rwp_below_lowest_returns_2000():
    assert select_rated_working_pressure(100.0) == 2000


def test_select_rwp_above_highest_raises():
    with pytest.raises(ValueError, match="exceeds the highest API 6A rating"):
        select_rated_working_pressure(21000.0)


def test_select_rwp_negative_raises():
    with pytest.raises(ValueError, match="p_design must be >= 0"):
        select_rated_working_pressure(-1.0)


# ---------------------------------------------------------------------------
# tree_rating_check — pressure envelope
# ---------------------------------------------------------------------------


def test_within_limit_hand_verified():
    # 10,000 psi tree (10K), class U [0,250]F, well 8500 psi over 40..210 F.
    # pressure: 8500 <= 10000 -> ok; margin = 10000-8500 = 1500;
    # utilisation = 8500/10000 = 0.85; temp: 0<=40 and 210<=250 -> ok; passes.
    r = tree_rating_check(
        p_design=8500.0,
        rwp=10000,
        temperature_class="U",
        t_well_min=40.0,
        t_well_max=210.0,
    )
    assert r.pressure_ok is True
    assert r.temperature_ok is True
    assert math.isclose(r.margin, 1500.0)
    assert math.isclose(r.utilisation, 0.85)
    assert r.passed is True
    assert r.t_class_min == 0.0 and r.t_class_max == 250.0


def test_overpressure_fails():
    # 12,000 psi well on a 10K tree -> under-rated on pressure.
    r = tree_rating_check(
        p_design=12000.0,
        rwp=10000,
        temperature_class="U",
        t_well_min=40.0,
        t_well_max=210.0,
    )
    assert r.pressure_ok is False
    assert math.isclose(r.margin, -2000.0)
    assert math.isclose(r.utilisation, 1.2)
    assert r.passed is False


def test_overtemperature_fails():
    # Well max 260 F exceeds class U max 250 F -> temperature inadequate.
    r = tree_rating_check(
        p_design=8000.0,
        rwp=10000,
        temperature_class="U",
        t_well_min=40.0,
        t_well_max=260.0,
    )
    assert r.pressure_ok is True
    assert r.temperature_ok is False
    assert r.passed is False


def test_undertemperature_min_below_class_fails():
    # Well min -10 F below class U min 0 F -> temperature inadequate.
    r = tree_rating_check(
        p_design=8000.0,
        rwp=10000,
        temperature_class="U",
        t_well_min=-10.0,
        t_well_max=200.0,
    )
    assert r.temperature_ok is False
    assert r.passed is False


def test_boundary_pressure_equals_rwp_is_ok():
    # p_design == rwp -> still pressure_ok (eq. 3 uses <=), utilisation 1.0.
    r = tree_rating_check(
        p_design=15000.0,
        rwp=15000,
        temperature_class="X",
        t_well_min=50.0,
        t_well_max=300.0,
    )
    assert r.pressure_ok is True
    assert math.isclose(r.margin, 0.0)
    assert math.isclose(r.utilisation, 1.0)
    assert r.passed is True


def test_boundary_temperature_equals_class_limits_is_ok():
    # Well range exactly equals class X envelope [0, 350] -> contained (eq. 4).
    r = tree_rating_check(
        p_design=5000.0,
        rwp=5000,
        temperature_class="X",
        t_well_min=0.0,
        t_well_max=350.0,
    )
    assert r.temperature_ok is True
    assert r.passed is True


def test_temperature_class_is_case_insensitive():
    r_lower = tree_rating_check(8000.0, 10000, "u", 40.0, 210.0)
    r_upper = tree_rating_check(8000.0, 10000, "U", 40.0, 210.0)
    assert r_lower.temperature_class == "U"
    assert r_lower == r_upper


# ---------------------------------------------------------------------------
# tree_rating_check — input validation
# ---------------------------------------------------------------------------


def test_invalid_rwp_raises():
    with pytest.raises(ValueError, match="not an API 6A rated working pressure"):
        tree_rating_check(5000.0, 7500, "U", 40.0, 210.0)


def test_unknown_temperature_class_raises():
    with pytest.raises(ValueError, match="unknown API 6A temperature class"):
        tree_rating_check(5000.0, 10000, "Z", 40.0, 210.0)


def test_inverted_temperature_range_raises():
    with pytest.raises(ValueError, match="t_well_min .* must be <= t_well_max"):
        tree_rating_check(5000.0, 10000, "U", 210.0, 40.0)


def test_negative_p_design_raises():
    with pytest.raises(ValueError, match="p_design must be >= 0"):
        tree_rating_check(-1.0, 10000, "U", 40.0, 210.0)


# ---------------------------------------------------------------------------
# reference data integrity
# ---------------------------------------------------------------------------


def test_rated_working_pressures_are_sorted_and_canonical():
    assert API_6A_RATED_WORKING_PRESSURES_PSI == (2000, 3000, 5000, 10000, 15000, 20000)
    assert list(API_6A_RATED_WORKING_PRESSURES_PSI) == sorted(
        API_6A_RATED_WORKING_PRESSURES_PSI
    )


def test_every_temperature_class_has_min_le_max():
    for cls, (lo, hi) in API_6A_TEMPERATURE_CLASSES_F.items():
        assert lo <= hi, cls
