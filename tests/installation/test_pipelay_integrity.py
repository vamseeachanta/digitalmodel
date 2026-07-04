"""Tests for S-lay pipelay installation integrity calculations.

All expected values are computed by hand from generic round-number inputs and
the closed-form formulas (no client data used as an oracle).
"""

import math

import pytest

from digitalmodel.installation.pipelay import (
    bending_strain_from_radius,
    concrete_crushing_check,
    concrete_crushing_strain,
    displaced_weight_per_length,
    horizontal_lay_tension,
    is_weld_repair_acceptable,
    minimum_route_radius,
    route_radius_factor_of_safety,
    submerged_weight_per_length,
    top_lay_tension,
    weld_repair_allowable_length,
)
from digitalmodel.installation.pipelay.integrity import G_STANDARD


# --- 1. Concrete-coating crushing (Verley & Ness limiting strain) -----------


def test_bending_strain_from_radius():
    # eps = (OD/2)/R = (0.5/2)/250 = 0.25/250 = 0.001
    assert bending_strain_from_radius(0.5, 250.0) == pytest.approx(0.001)


def test_bending_strain_invalid_radius():
    with pytest.raises(ValueError):
        bending_strain_from_radius(0.5, 0.0)


def test_concrete_crushing_strain_scf():
    # SCF * eps = 2.0 * 0.001 = 0.002
    assert concrete_crushing_strain(0.001, 2.0) == pytest.approx(0.002)


def test_concrete_crushing_strain_invalid_scf():
    with pytest.raises(ValueError):
        concrete_crushing_strain(0.001, 0.9)


def test_concrete_crushing_check_passes():
    # OD=0.5, R=250 -> eps_bend=0.001; SCF=1 -> eps_conc=0.001
    # crush limit 0.002, FoS=1 -> allowable=0.002; util=0.5 -> passes
    res = concrete_crushing_check(
        outer_diameter=0.5,
        bend_radius=250.0,
        crushing_strain_limit=0.002,
        strain_concentration_factor=1.0,
        factor_of_safety=1.0,
    )
    assert res.bending_strain == pytest.approx(0.001)
    assert res.concrete_strain == pytest.approx(0.001)
    assert res.allowable_strain == pytest.approx(0.002)
    assert res.utilisation == pytest.approx(0.5)
    assert res.passes is True


def test_concrete_crushing_check_fails_with_fos():
    # Same geometry but FoS=2 -> allowable=0.001, util=1.0 -> NOT < 1 -> fails
    res = concrete_crushing_check(
        outer_diameter=0.5,
        bend_radius=250.0,
        crushing_strain_limit=0.002,
        strain_concentration_factor=1.0,
        factor_of_safety=2.0,
    )
    assert res.allowable_strain == pytest.approx(0.001)
    assert res.utilisation == pytest.approx(1.0)
    assert res.passes is False


def test_concrete_crushing_check_scf_drives_failure():
    # SCF=3 -> eps_conc=0.003 > allowable 0.002 -> util=1.5 -> fails
    res = concrete_crushing_check(
        outer_diameter=0.5,
        bend_radius=250.0,
        crushing_strain_limit=0.002,
        strain_concentration_factor=3.0,
    )
    assert res.concrete_strain == pytest.approx(0.003)
    assert res.utilisation == pytest.approx(1.5)
    assert res.passes is False


# --- 2. Minimum route / lay radius:  R = F / (S * W) ------------------------


def test_minimum_route_radius():
    # R = F/(S*W) = 100000/(0.5*2000) = 100000/1000 = 100
    assert minimum_route_radius(100_000.0, 0.5, 2000.0) == pytest.approx(100.0)


def test_minimum_route_radius_invalid():
    with pytest.raises(ValueError):
        minimum_route_radius(100_000.0, 0.0, 2000.0)
    with pytest.raises(ValueError):
        minimum_route_radius(100_000.0, 0.5, 0.0)


def test_route_radius_factor_of_safety():
    # planned 150 / min 100 = 1.5
    assert route_radius_factor_of_safety(150.0, 100.0) == pytest.approx(1.5)


# --- 3. Weld-repair allowable length (DNV-ST-F101 limits) -------------------


def test_weld_repair_fraction_governs():
    # total 1.0 m, partial 30% -> 0.30 m (> 50 mm minimum)
    assert weld_repair_allowable_length(1.0, 0.30) == pytest.approx(0.30)


def test_weld_repair_minimum_governs():
    # total 0.1 m, full 20% -> 0.02 m < 50 mm minimum -> floor at 0.05
    assert weld_repair_allowable_length(0.1, 0.20) == pytest.approx(0.05)


def test_weld_repair_invalid_fraction():
    with pytest.raises(ValueError):
        weld_repair_allowable_length(1.0, 0.0)
    with pytest.raises(ValueError):
        weld_repair_allowable_length(1.0, 1.5)


def test_is_weld_repair_acceptable():
    # total 1.0, fraction 0.30 -> limit 0.30, min 0.05
    assert is_weld_repair_acceptable(0.20, 1.0, 0.30) is True  # within band
    assert is_weld_repair_acceptable(0.40, 1.0, 0.30) is False  # over fractional
    assert is_weld_repair_acceptable(0.04, 1.0, 0.30) is False  # under minimum


# --- 4. S-lay submerged weight and lay tension ------------------------------


def test_displaced_weight_per_length():
    # b = rho*g*(pi/4)*OD^2 = 1025*9.80665*(pi/4)*0.5^2
    expected = 1025.0 * G_STANDARD * (math.pi / 4.0) * 0.5**2
    assert displaced_weight_per_length(0.5, 1025.0) == pytest.approx(expected)
    # sanity: ~1973.7 N/m
    assert displaced_weight_per_length(0.5, 1025.0) == pytest.approx(1973.67, abs=0.5)


def test_submerged_weight_per_length():
    # w_sub = w_dry - buoyancy = 3000 - rho*g*(pi/4)*OD^2
    buoyancy = 1025.0 * G_STANDARD * (math.pi / 4.0) * 0.5**2
    expected = 3000.0 - buoyancy
    assert submerged_weight_per_length(3000.0, 0.5, 1025.0) == pytest.approx(expected)


def test_horizontal_lay_tension():
    # H = w_sub * R = 1000 * 300 = 300000
    assert horizontal_lay_tension(1000.0, 300.0) == pytest.approx(300_000.0)


def test_top_lay_tension():
    # T_top = H + w_sub*d = 1000*300 + 1000*100 = 300000 + 100000 = 400000
    assert top_lay_tension(1000.0, 100.0, 300.0) == pytest.approx(400_000.0)


def test_top_lay_tension_equals_wsub_times_R_plus_d():
    w_sub, d, r = 1500.0, 80.0, 220.0
    assert top_lay_tension(w_sub, d, r) == pytest.approx(w_sub * (r + d))
