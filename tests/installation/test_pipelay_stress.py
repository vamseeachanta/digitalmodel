"""Tests for over/sagbend longitudinal-stress (%SMYS) acceptance checks.

All expected values are computed by hand from generic round-number inputs and
the closed-form formulas (no client data used as an oracle).
"""

import pytest

from digitalmodel.installation.pipelay import (
    SmysCheck,
    allowable_bend_radius,
    axial_stress,
    bending_stress_from_radius,
    smys_stress_check,
)
from digitalmodel.installation.pipelay.stress import (
    DEFAULT_OVERBEND_FRACTION,
    DEFAULT_SAGBEND_FRACTION,
)


# --- bending stress from radius ---------------------------------------------


def test_bending_stress_from_radius():
    # sigma = E * (OD/2) / R = 200000 * (0.5/2) / 250 = 200000 * 0.001 = 200.0
    assert bending_stress_from_radius(0.5, 250.0, 200000.0) == pytest.approx(200.0)


def test_bending_stress_invalid_radius():
    with pytest.raises(ValueError):
        bending_stress_from_radius(0.5, 0.0, 200000.0)


# --- axial stress -----------------------------------------------------------


def test_axial_stress():
    # sigma = N / A = 1000 / 0.01 = 100000
    assert axial_stress(1000.0, 0.01) == pytest.approx(100000.0)


def test_axial_stress_invalid_area():
    with pytest.raises(ValueError):
        axial_stress(1000.0, 0.0)


# --- %SMYS acceptance check -------------------------------------------------


def test_smys_check_pure_bending_passes():
    # sigma_bend = 200000 * (0.5/2) / 250 = 200.0
    # allowable = 0.85 * 450 = 382.5 ; 200 <= 382.5 -> pass
    res = smys_stress_check(
        outer_diameter=0.5,
        bend_radius=250.0,
        youngs_modulus=200000.0,
        smys=450.0,
        allowable_fraction=DEFAULT_OVERBEND_FRACTION,
    )
    assert isinstance(res, SmysCheck)
    assert res.longitudinal_stress == pytest.approx(200.0)
    assert res.allowable_stress == pytest.approx(382.5)
    assert res.utilisation == pytest.approx(200.0 / 382.5)
    assert res.percent_smys == pytest.approx(200.0 / 450.0 * 100.0)
    assert res.passes is True


def test_smys_check_with_axial_term():
    # sigma_bend = 200000 * (0.5/2) / 250 = 200.0
    # sigma_axial = 50000 / 0.01 = 5_000_000?  -> use consistent N, m^2, MPa? keep generic
    # Use stress units directly: axial_force in (stress*area) consistent set.
    # N = 3000 (force), A = 0.02 -> sigma_axial = 150000 ... too large.
    # Choose so total is round: sigma_axial = N/A = 1.0/0.02 = 50.0
    # total = 50 + 200 = 250.0 ; allowable = 0.72 * 450 = 324.0 ; pass
    res = smys_stress_check(
        outer_diameter=0.5,
        bend_radius=250.0,
        youngs_modulus=200000.0,
        smys=450.0,
        allowable_fraction=DEFAULT_SAGBEND_FRACTION,
        axial_force=1.0,
        steel_area=0.02,
    )
    assert res.longitudinal_stress == pytest.approx(250.0)
    assert res.allowable_stress == pytest.approx(324.0)
    assert res.passes is True


def test_smys_check_fails_when_overstressed():
    # sigma_bend = 200000 * (0.5/2) / 100 = 500.0 ; allowable = 0.85*450 = 382.5 -> fail
    res = smys_stress_check(
        outer_diameter=0.5,
        bend_radius=100.0,
        youngs_modulus=200000.0,
        smys=450.0,
        allowable_fraction=DEFAULT_OVERBEND_FRACTION,
    )
    assert res.longitudinal_stress == pytest.approx(500.0)
    assert res.utilisation > 1.0
    assert res.passes is False


def test_smys_check_axial_requires_area():
    with pytest.raises(ValueError):
        smys_stress_check(
            outer_diameter=0.5,
            bend_radius=250.0,
            youngs_modulus=200000.0,
            smys=450.0,
            allowable_fraction=0.85,
            axial_force=1.0,
        )


def test_smys_check_invalid_fraction():
    with pytest.raises(ValueError):
        smys_stress_check(0.5, 250.0, 200000.0, 450.0, 0.0)


def test_smys_check_invalid_smys():
    with pytest.raises(ValueError):
        smys_stress_check(0.5, 250.0, 200000.0, 0.0, 0.85)


def test_default_fractions():
    assert DEFAULT_OVERBEND_FRACTION == 0.85
    assert DEFAULT_SAGBEND_FRACTION == 0.72


# --- minimum admissible bend radius -----------------------------------------


def test_allowable_bend_radius():
    # R_min = E*(OD/2)/(f*SMYS) = 200000*(0.5/2)/(0.85*450)
    #       = 200000*0.25/382.5 = 50000/382.5
    expected = 200000.0 * 0.25 / (0.85 * 450.0)
    assert allowable_bend_radius(0.5, 200000.0, 450.0, 0.85) == pytest.approx(expected)


def test_allowable_bend_radius_consistent_with_check():
    # At exactly R_min, pure-bending stress equals allowable -> utilisation ~ 1.
    r_min = allowable_bend_radius(0.5, 200000.0, 450.0, 0.85)
    res = smys_stress_check(0.5, r_min, 200000.0, 450.0, 0.85)
    assert res.utilisation == pytest.approx(1.0)


def test_allowable_bend_radius_invalid():
    with pytest.raises(ValueError):
        allowable_bend_radius(0.5, 200000.0, 450.0, 1.5)
