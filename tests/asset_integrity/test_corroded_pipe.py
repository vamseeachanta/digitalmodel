# ABOUTME: Tests for corroded-pipe remaining strength (B31G / Modified B31G /
# ABOUTME: RSTRENG) — golden hand-calcs, regimes, monotonicity, validation.
"""Tests for digitalmodel.asset_integrity.corroded_pipe."""

import math

import pytest

from digitalmodel.asset_integrity.corroded_pipe import (
    SMYS_PSI,
    CorrodedPipeResult,
    allowable_flaw_length,
    b31g_original,
    b31g_original_allowable_length,
    folias_modified,
    folias_original,
    modified_b31g,
    rstreng_effective_area,
)

# Reference defect: 30 in OD x 0.375 in WT, d=0.15 in (40% WT), L=8 in, X52.
D, T, DEPTH, LEN, SMYS = 30.0, 0.375, 0.15, 8.0, 52_000.0


# --- Folias factors --------------------------------------------------------
def test_folias_original_parabolic():
    # z = 8^2 / (30*0.375) = 5.6889 -> sqrt(1+0.8z) = 2.356
    assert folias_original(5.68889) == pytest.approx(2.3561, rel=1e-3)


def test_folias_original_infinite_regime_returns_none():
    assert folias_original(25.0) is None


def test_folias_modified_two_part():
    assert folias_modified(5.68889) == pytest.approx(2.1120, rel=1e-3)
    # z > 50 uses the linear branch
    assert folias_modified(60.0) == pytest.approx(0.032 * 60 + 3.3, rel=1e-6)


# --- Golden hand calculations ----------------------------------------------
def test_modified_b31g_golden():
    r = modified_b31g(D, T, DEPTH, LEN, SMYS)
    assert isinstance(r, CorrodedPipeResult)
    assert r.flow_stress_psi == pytest.approx(62_000.0)
    assert r.intact_pressure_psi == pytest.approx(1550.0, rel=1e-4)
    assert r.area_ratio == pytest.approx(0.34, rel=1e-6)
    assert r.failure_pressure_psi == pytest.approx(1219.3, rel=2e-3)
    assert r.rsf == pytest.approx(0.7866, rel=2e-3)


def test_original_b31g_golden():
    r = b31g_original(D, T, DEPTH, LEN, SMYS)
    assert r.flow_stress_psi == pytest.approx(57_200.0)
    assert r.intact_pressure_psi == pytest.approx(1430.0, rel=1e-4)
    assert r.failure_pressure_psi == pytest.approx(1182.6, rel=2e-3)
    assert r.details["regime"] == "parabolic"


def test_original_more_conservative_than_modified():
    orig = b31g_original(D, T, DEPTH, LEN, SMYS).failure_pressure_psi
    mod = modified_b31g(D, T, DEPTH, LEN, SMYS).failure_pressure_psi
    assert orig < mod


# --- Regimes & limits ------------------------------------------------------
def test_original_infinite_length_regime():
    r = b31g_original(D, T, DEPTH, 40.0, SMYS)   # z ~ 142 > 20
    assert r.details["regime"] == "infinite_length"
    assert math.isinf(r.folias_factor)
    # P_f = 2*flow*t/D*(1 - d/t) = 1430 * 0.6
    assert r.failure_pressure_psi == pytest.approx(1430.0 * 0.6, rel=1e-3)


def test_zero_defect_recovers_intact_pressure():
    r = modified_b31g(D, T, 0.0, LEN, SMYS)
    assert r.area_ratio == pytest.approx(0.0)
    assert r.failure_pressure_psi == pytest.approx(r.intact_pressure_psi, rel=1e-6)
    assert r.rsf == pytest.approx(1.0, rel=1e-6)


def test_deeper_defect_lowers_failure_pressure():
    shallow = modified_b31g(D, T, 0.075, LEN, SMYS).failure_pressure_psi
    deep = modified_b31g(D, T, 0.30, LEN, SMYS).failure_pressure_psi
    assert deep < shallow


def test_longer_defect_lowers_failure_pressure():
    short = modified_b31g(D, T, DEPTH, 3.0, SMYS).failure_pressure_psi
    long = modified_b31g(D, T, DEPTH, 16.0, SMYS).failure_pressure_psi
    assert long < short


# --- RSTRENG effective area ------------------------------------------------
def test_rstreng_rectangular_more_conservative_than_modified():
    # Uniform-depth profile -> effective A/A0 = d/t (0.40) > 0.85*d/t (0.34),
    # so RSTRENG predicts a lower failure pressure than Modified B31G.
    r = rstreng_effective_area(D, T, [0.0, LEN], [DEPTH, DEPTH], SMYS)
    mod = modified_b31g(D, T, DEPTH, LEN, SMYS)
    assert r.area_ratio == pytest.approx(0.40, rel=1e-6)
    assert r.failure_pressure_psi < mod.failure_pressure_psi


def test_rstreng_reports_critical_segment():
    # A deep notch in the middle of an otherwise shallow profile.
    xs = [0.0, 2.0, 4.0, 6.0, 8.0]
    ds = [0.02, 0.05, 0.25, 0.05, 0.02]
    r = rstreng_effective_area(D, T, xs, ds, SMYS)
    assert r.method == "RSTRENG"
    assert r.failure_pressure_psi > 0
    assert "critical_length_in" in r.details


def test_rstreng_validates_inputs():
    with pytest.raises(ValueError):
        rstreng_effective_area(D, T, [0.0, 1.0], [0.1], SMYS)        # mismatched
    with pytest.raises(ValueError):
        rstreng_effective_area(D, T, [1.0, 0.0], [0.1, 0.1], SMYS)   # not increasing
    with pytest.raises(ValueError):
        rstreng_effective_area(D, T, [0.0, 1.0], [0.1, 0.5], SMYS)   # depth > t


# --- acceptance vs MAOP ----------------------------------------------------
def test_acceptable_flag_vs_maop():
    r_lo = modified_b31g(D, T, DEPTH, LEN, SMYS, maop_psi=500.0)
    r_hi = modified_b31g(D, T, DEPTH, LEN, SMYS, maop_psi=1200.0)
    assert r_lo.acceptable is True       # safe ~877 psi >= 500
    assert r_hi.acceptable is False      # safe ~877 psi <  1200


# --- allowable flaw length (regenerated B31G table) ------------------------
def test_allowable_length_decreases_with_depth():
    shallow = allowable_flaw_length(D, T, 0.075, SMYS, 800.0)
    deep = allowable_flaw_length(D, T, 0.20, SMYS, 800.0)
    assert deep < shallow


def test_allowable_length_meets_design_pressure():
    L = allowable_flaw_length(D, T, DEPTH, SMYS, 900.0)
    safe = modified_b31g(D, T, DEPTH, L, SMYS).safe_pressure_psi
    assert safe == pytest.approx(900.0, rel=5e-3)


# --- material table & validation ------------------------------------------
def test_smys_table_grades():
    assert SMYS_PSI["X52"] == 52_000.0
    assert SMYS_PSI["X65"] == 65_000.0


def test_geometry_validation():
    with pytest.raises(ValueError):
        modified_b31g(0.0, T, DEPTH, LEN, SMYS)
    with pytest.raises(ValueError):
        modified_b31g(D, T, T + 0.1, LEN, SMYS)   # depth exceeds wall


# --- Original B31G allowable length vs the published ASME table -------------
# Golden values from ASME B31G-2012 page-26 allowable-defect-length table,
# OD = 20 in (d in., t in. -> allowable longitudinal length in.).
ASME_OD = 20.0


@pytest.mark.parametrize("d, t, expected_L", [
    (0.10, 0.500, 9.48),   # B-parameter regime
    (0.15, 0.500, 4.72),
    (0.25, 0.625, 3.76),
    (0.10, 0.219, 1.93),
    (0.05, 0.500, 14.17),  # shallow -> B capped at 4.0
    (0.04, 0.344, 11.75),  # shallow -> B capped at 4.0
    (0.50, 0.625, 1.78),   # d/t = 0.80 boundary (still acceptable)
])
def test_b31g_allowable_length_matches_asme_table(d, t, expected_L):
    L = b31g_original_allowable_length(ASME_OD, t, d)
    assert L == pytest.approx(expected_L, abs=0.02)


def test_b31g_allowable_length_rejects_above_80pct():
    # d/t = 0.18/0.219 = 0.822 > 0.80 -> not acceptable (table shows 0).
    assert b31g_original_allowable_length(ASME_OD, 0.219, 0.18) == 0.0


def test_b31g_allowable_length_decreases_with_depth():
    shallow = b31g_original_allowable_length(ASME_OD, 0.5, 0.10)
    deep = b31g_original_allowable_length(ASME_OD, 0.5, 0.30)
    assert deep < shallow
