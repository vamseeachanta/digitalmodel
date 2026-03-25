"""Tests for S-lay installation bending strain checks -- API RP 1111 Section 6.

Benchmark: 12.75" OD (0.32385 m) X65 pipeline.
Allowable bending strain = 0.005 (0.5%).
"""
import pytest

from digitalmodel.subsea.pipeline.api_rp_1111_installation import (
    slay_sagbend_bending_strain,
    slay_overbend_bending_strain,
    slay_allowable_bending_strain,
    slay_bending_check,
)

# Benchmark pipe: 12.75" OD
_D = 12.75 * 0.0254  # 0.32385 m


def test_slay_sagbend_bending_strain_large_radius():
    """R_sagbend=200 m -> eps = 0.32385 / 400 = 0.000810."""
    eps = slay_sagbend_bending_strain(_D, R_sagbend=200.0)
    assert eps == pytest.approx(0.000810, rel=1e-2)


def test_slay_sagbend_bending_strain_small_radius():
    """R_sagbend=50 m -> eps = 0.32385 / 100 = 0.003239."""
    eps = slay_sagbend_bending_strain(_D, R_sagbend=50.0)
    assert eps == pytest.approx(0.003239, rel=1e-2)


def test_slay_overbend_bending_strain():
    """R_overbend=150 m -> eps = 0.32385 / 300 = 0.001080."""
    eps = slay_overbend_bending_strain(_D, R_overbend=150.0)
    assert eps == pytest.approx(0.001080, rel=1e-2)


def test_slay_allowable_bending_strain():
    """Conservative installation limit is 0.5% (0.005)."""
    assert slay_allowable_bending_strain() == 0.005


def test_slay_bending_check_pass():
    """R_sag=200, R_over=150 -> both utilizations well below 1.0."""
    res = slay_bending_check(D=_D, R_sagbend=200.0, R_overbend=150.0)
    assert res["pass"] is True
    assert res["sagbend_utilization"] == pytest.approx(0.162, rel=2e-2)
    assert res["overbend_utilization"] == pytest.approx(0.216, rel=2e-2)
    assert res["epsilon_b_allow"] == 0.005


def test_slay_bending_check_fail_sagbend():
    """R_sag=30 -> sagbend util=1.08 > 1.0 -> fail."""
    res = slay_bending_check(D=_D, R_sagbend=30.0, R_overbend=150.0)
    assert res["pass"] is False
    assert res["sagbend_utilization"] == pytest.approx(1.080, rel=2e-2)
    assert res["overbend_utilization"] < 1.0


def test_slay_sagbend_invalid_radius():
    """R_sagbend <= 0 raises ValueError."""
    with pytest.raises(ValueError, match="R_sagbend must be positive"):
        slay_sagbend_bending_strain(_D, R_sagbend=0.0)
    with pytest.raises(ValueError, match="R_sagbend must be positive"):
        slay_sagbend_bending_strain(_D, R_sagbend=-10.0)
