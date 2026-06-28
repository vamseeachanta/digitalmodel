# ABOUTME: Consolidated FFS validation suite — cross-cutting golden checks
# ABOUTME: against published references (API 579 Folias, ASME B31G, hand calcs).
"""Cross-cutting FFS validation (issue #1061).

Complements the per-module golden tests with the references that span modules:
the API 579-1 Folias bulging factor (Table 4.4), the ASME B31G allowable-length
table, and cross-method ordering of the corroded-pipe failure pressures. The
documented record is `docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md`.
"""

import math

import pytest

from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
from digitalmodel.asset_integrity.corroded_pipe import (
    b31g_original,
    b31g_original_allowable_length,
    modified_b31g,
)
from digitalmodel.asset_integrity.dnv_rp_f101 import dnv_f101_single_defect


# --- API 579-1 Folias bulging factor (Table 4.4) ---------------------------
@pytest.mark.parametrize("lam, mt_expected", [
    (1.0, math.sqrt(1.0 + 0.48 * 1.0)),   # 1.2166
    (2.0, math.sqrt(1.0 + 0.48 * 4.0)),   # 1.7088
    (5.0, math.sqrt(1.0 + 0.48 * 25.0)),  # 3.6056
])
def test_folias_factor_matches_api579_table_4_4(lam, mt_expected):
    # Choose L so that lambda = 1.285 * L / sqrt(D*t) equals the target.
    D, t = 20.0, 0.5
    L = lam * math.sqrt(D * t) / 1.285
    mt = Level2Engine._folias_factor(l_a=L, nominal_id=D, t_c=t)
    assert mt == pytest.approx(mt_expected, rel=1e-3)


def test_folias_factor_floor_below_lambda_threshold():
    # Very short flaw -> lambda <= 0.354 -> M_t = 1.0.
    assert Level2Engine._folias_factor(l_a=0.1, nominal_id=20.0, t_c=0.5) == 1.0


# --- ASME B31G-2012 allowable-length table (page 26, OD 20 in) -------------
@pytest.mark.parametrize("d, t, expected_L", [
    (0.10, 0.500, 9.48),
    (0.15, 0.500, 4.72),
    (0.05, 0.500, 14.17),   # B capped at 4.0
    (0.50, 0.625, 1.78),    # d/t = 0.80 boundary
])
def test_b31g_allowable_length_matches_asme(d, t, expected_L):
    assert b31g_original_allowable_length(20.0, t, d) == pytest.approx(expected_L, abs=0.02)


# --- Corroded-pipe failure-pressure hand calcs -----------------------------
def test_b31g_failure_pressure_hand_calcs():
    # 30"x0.375", d=0.15", L=8", X52.
    assert modified_b31g(30.0, 0.375, 0.15, 8.0, 52_000.0).failure_pressure_psi \
        == pytest.approx(1219.3, rel=2e-3)
    assert b31g_original(30.0, 0.375, 0.15, 8.0, 52_000.0).failure_pressure_psi \
        == pytest.approx(1182.6, rel=2e-3)


def test_dnv_f101_capacity_hand_calc():
    # Same geometry, X52 SMTS 66700 psi -> Q=1.6624, P_cap=1334 psi.
    r = dnv_f101_single_defect(30.0, 0.375, 0.15, 8.0, 66_700.0)
    assert r.Q == pytest.approx(1.6624, rel=2e-3)
    assert r.capacity_pressure_psi == pytest.approx(1334.2, rel=3e-3)


# --- cross-method consistency ----------------------------------------------
def test_method_ordering_for_reference_defect():
    # For the reference defect, original B31G is the most conservative on
    # failure pressure; DNV (SMTS-based) the least. All positive, finite.
    args = (30.0, 0.375, 0.15, 8.0)
    p_orig = b31g_original(*args, 52_000.0).failure_pressure_psi
    p_mod = modified_b31g(*args, 52_000.0).failure_pressure_psi
    p_dnv = dnv_f101_single_defect(*args, 66_700.0).capacity_pressure_psi
    assert 0 < p_orig < p_mod < p_dnv
