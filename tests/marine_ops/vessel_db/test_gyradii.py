"""Tests for radii-of-gyration estimation."""

from __future__ import annotations

import pytest

from digitalmodel.marine_ops.vessel_db.gyradii import (
    KXX_OVER_BEAM,
    KYY_OVER_LBP,
    KZZ_OVER_LBP,
    estimate_gyradii,
)


def test_ship_form_relations():
    est = estimate_gyradii(beam=40.0, length_bp=200.0, vessel_type="fpso")
    assert est["kxx_roll"].value == pytest.approx(KXX_OVER_BEAM * 40.0)
    assert est["kyy_pitch"].value == pytest.approx(KYY_OVER_LBP * 200.0)
    assert est["kzz_yaw"].value == pytest.approx(KZZ_OVER_LBP * 200.0)


def test_basis_string_is_machine_readable():
    est = estimate_gyradii(beam=30.0, length_bp=None)
    assert est["kxx_roll"].basis == f"estimated:kxx={KXX_OVER_BEAM}*beam"
    # No length -> no pitch/yaw.
    assert "kyy_pitch" not in est and "kzz_yaw" not in est


def test_missing_inputs_omit_axes():
    assert estimate_gyradii(beam=None, length_bp=None) == {}
    only_pitch = estimate_gyradii(beam=None, length_bp=180.0)
    assert set(only_pitch) == {"kyy_pitch", "kzz_yaw"}


def test_zero_or_negative_dimensions_ignored():
    assert estimate_gyradii(beam=0.0, length_bp=-5.0) == {}


def test_non_ship_form_carries_caveat():
    semi = estimate_gyradii(beam=80.0, length_bp=120.0, vessel_type="semisubmersible crane vessel")
    assert semi["kxx_roll"].caveat is not None
    assert "non-ship" in semi["kxx_roll"].caveat
    ship = estimate_gyradii(beam=40.0, length_bp=200.0, vessel_type="drillship")
    assert ship["kxx_roll"].caveat is None
