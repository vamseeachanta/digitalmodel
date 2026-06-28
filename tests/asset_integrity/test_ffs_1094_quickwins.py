# ABOUTME: Regression tests for the #1094 FFS strength-method quick-win fixes:
# ABOUTME: unbiased area-average t_am, d/t applicability flags, frozen Folias cap.
import math

import numpy as np
import pandas as pd

from digitalmodel.asset_integrity import corroded_pipe as cp
from digitalmodel.asset_integrity import dnv_rp_f101 as dnv
from digitalmodel.asset_integrity.assessment.level2_engine import (
    Level2Engine,
    _LAMBDA_UPPER,
)


# --- #8: t_am is the true area-average, unbiased under unequal NaN counts -----
def test_gml_t_am_is_area_average_not_mean_of_column_means():
    # Column 1 has a missing cell, so the old df.mean().mean() (mean of the
    # per-column means) would give 0.525; the true area-average is 0.500.
    grid = pd.DataFrame({"c0": [0.40, 0.50], "c1": [0.60, np.nan]})
    engine = Level2Engine("GML", nominal_od_in=20.0, nominal_wt_in=0.5,
                          t_min_in=0.45)
    result = engine.evaluate(grid)

    valid = grid.to_numpy(dtype=float)
    expected = float(np.nanmean(valid))  # = 1.5 / 3 = 0.5
    assert math.isclose(result["t_am_in"], expected, rel_tol=1e-12)
    assert math.isclose(result["t_am_in"], 0.5, rel_tol=1e-12)
    # The biased mean-of-column-means would have been 0.525.
    assert not math.isclose(result["t_am_in"], 0.525, rel_tol=1e-6)


# --- #2: B31G / Modified / RSTRENG applicability flag (d/t <= 0.80) -----------
def test_b31g_flags_defect_beyond_applicability():
    deep = cp.b31g_original(D=20.0, t=0.5, d=0.45, L=4.0, smys_psi=52_000.0)
    assert deep.details["within_applicability"] is False
    assert deep.details["applicability_note"] is not None

    shallow = cp.b31g_original(D=20.0, t=0.5, d=0.20, L=4.0, smys_psi=52_000.0)
    assert shallow.details["within_applicability"] is True
    assert shallow.details["applicability_note"] is None


def test_modified_b31g_flags_defect_beyond_applicability():
    deep = cp.modified_b31g(D=20.0, t=0.5, d=0.45, L=4.0, smys_psi=52_000.0)
    assert deep.details["within_applicability"] is False


def test_rstreng_flags_profile_beyond_applicability():
    # Deepest profile point is 0.45 in of a 0.5 in wall -> d/t = 0.9 > 0.80.
    res = cp.rstreng_effective_area(
        D=20.0, t=0.5, positions_in=[0.0, 2.0, 4.0],
        depths_in=[0.10, 0.45, 0.10], smys_psi=52_000.0)
    assert res.details["within_applicability"] is False
    assert math.isclose(res.details["d_over_t"], 0.9, rel_tol=1e-9)


# --- #3: DNV-RP-F101 single-defect applicability flag (d/t <= 0.85) -----------
def test_dnv_f101_single_defect_flags_beyond_applicability():
    deep = dnv.dnv_f101_single_defect(D=20.0, t=0.5, d=0.45, L=4.0,
                                      smts_psi=66_700.0)
    assert deep.details["within_applicability"] is False
    assert deep.details["applicability_note"] is not None

    ok = dnv.dnv_f101_single_defect(D=20.0, t=0.5, d=0.40, L=4.0,
                                    smts_psi=66_700.0)
    assert ok.details["within_applicability"] is True
    assert ok.details["applicability_note"] is None


# --- #11: Folias factor is frozen at the lambda = 20 value beyond the range ---
def test_folias_factor_frozen_above_lambda_20():
    # Long flaw -> lambda >> 20; M_t must equal the lambda = 20 table value,
    # not extrapolate the simplified closed form.
    mt = Level2Engine._folias_factor(l_a=100.0, nominal_id=20.0, t_c=0.5)
    expected = math.sqrt(1.0 + 0.48 * _LAMBDA_UPPER ** 2)
    assert math.isclose(mt, expected, rel_tol=1e-12)
