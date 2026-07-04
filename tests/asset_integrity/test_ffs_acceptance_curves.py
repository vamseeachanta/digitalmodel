# ABOUTME: Tests for FFS acceptance curves — envelope monotonicity, the
# ABOUTME: safe=MAOP boundary, and general metal-loss curves.
"""Tests for digitalmodel.asset_integrity.ffs_acceptance_curves."""

import math

import pytest

from digitalmodel.asset_integrity.ffs_acceptance_curves import (
    _pipe_safe_pressure,
    general_metal_loss_curves,
    pipe_acceptance_curve,
    plate_acceptance_curve,
)
from digitalmodel.asset_integrity.ffs_lookup import barlow_maop_psi


# --- pipe acceptance envelope ---------------------------------------------
@pytest.mark.parametrize("method", ["b31g", "modified_b31g", "rstreng", "dnv_f101"])
def test_pipe_envelope_decreases_with_depth(method):
    c = pipe_acceptance_curve(20.0, 0.5, "X52", method=method)
    lens = c["max_acceptable_length_in"]
    # Deeper defects -> shorter acceptable length (monotonic non-increasing).
    assert all(lens[i] >= lens[i + 1] - 1e-6 for i in range(len(lens) - 1))
    assert c["maop_psi"] > 0


def test_pipe_envelope_boundary_meets_maop():
    # At the curve's (depth, max-length) the safe pressure should equal MAOP
    # (for interior points that are neither 0 nor clamped to the max).
    D, t, grade, method = 20.0, 0.5, "X52", "modified_b31g"
    maop = barlow_maop_psi(D, t, 52000.0)
    c = pipe_acceptance_curve(D, t, grade, method=method, max_length_in=40.0)
    checked = 0
    for d, L in zip(c["depth_in"], c["max_acceptable_length_in"]):
        if 0.01 < L < 39.9:
            p = _pipe_safe_pressure(method, D, t, d, L, grade)
            assert p == pytest.approx(maop, rel=2e-2)
            checked += 1
    assert checked >= 1


def test_pipe_envelope_custom_maop_is_stricter():
    base = pipe_acceptance_curve(20.0, 0.5, "X52", method="modified_b31g")
    strict = pipe_acceptance_curve(20.0, 0.5, "X52", method="modified_b31g",
                                   maop_psi=base["maop_psi"] * 1.5)
    # A higher MAOP demand -> shorter (or equal) acceptable lengths everywhere.
    assert all(s <= b + 1e-6 for s, b in zip(
        strict["max_acceptable_length_in"], base["max_acceptable_length_in"]))


# --- plate acceptance envelope --------------------------------------------
def test_plate_envelope_decreases_with_stress():
    c = plate_acceptance_curve("AH36", 2400.0, 800.0, 16.0)
    loss = c["max_acceptable_loss_mm"]
    assert all(loss[i] >= loss[i + 1] - 1e-6 for i in range(len(loss) - 1))
    assert all(0.0 <= x <= 16.0 for x in loss)


def test_plate_envelope_higher_grade_allows_more_loss():
    a = plate_acceptance_curve("Grade A", 2400.0, 800.0, 16.0,
                               sigma_x_values=[150.0])["max_acceptable_loss_mm"][0]
    e = plate_acceptance_curve("EH40", 2400.0, 800.0, 16.0,
                               sigma_x_values=[150.0])["max_acceptable_loss_mm"][0]
    assert e >= a


# --- general metal loss curves --------------------------------------------
def test_general_curves_mawp_decreases_with_fca():
    c = general_metal_loss_curves(20.0, 0.5, "X52", t_min_in=0.20)
    mawp = c["mawp_psi"]
    assert all(mawp[i] >= mawp[i + 1] for i in range(len(mawp) - 1))
    # MAWP at FCA=0 is the Barlow pressure 2*S*t/D.
    assert mawp[0] == pytest.approx(2 * 52000 * 0.72 * 0.5 / 20.0, rel=1e-6)


def test_general_curves_life_decreases_with_rate():
    c = general_metal_loss_curves(20.0, 0.5, "X52", t_min_in=0.20)
    life = c["remaining_life_yr"]
    assert all(life[i] >= life[i + 1] for i in range(len(life) - 1))
    assert all(x >= 0 for x in life)
