# ABOUTME: Tests for 2D river-bottom effective-area projection (max / area-
# ABOUTME: weighted) onto the validated 1D RSTRENG engine — golden hand-calcs.
"""Tests for digitalmodel.asset_integrity.rstreng_2d.

Golden values (vetted, independently reproduced):
  D=24 in, t=0.5 in, X52 (SMYS=52000); x=[0,2,4,6,8] in; 3 circumferential
  columns; depths/in station1=[.10,.20,.05], station2=[.15,.30,.10],
  station3=[.05,.25,0] (zeros at x=0 and x=8).
  intact flow-stress pressure = 2*flow*t/D, flow=62000 -> 2583.3 psi.
  MAX  : d_eff=[0,.20,.30,.25,0] -> seg(1,4), L=6, A_eff=1.30, ar=0.4333,
         M=1.6888 -> pf=1969.2 psi (RSF=0.7623, safe@1.39=1416.7).
  AREA : d_eff=[0,.1167,.1833,.10,0] -> seg(0,4), L=8, ar=0.2000, z=5.333,
         M=2.0617 -> pf=2288.7 psi (RSF=0.8859, safe=1646.5) ~16% > MAX.
"""

import numpy as np
import pytest

from digitalmodel.asset_integrity.corroded_pipe import CorrodedPipeResult
from digitalmodel.asset_integrity.rstreng_2d import (
    DEFAULT_SAFETY_FACTOR,
    PROJECTION_AREA_WEIGHTED,
    PROJECTION_MAX,
    RiverBottom2DResult,
    project_grid,
    rstreng_2d_river_bottom,
)

D, T, SMYS = 24.0, 0.5, 52_000.0
X = [0.0, 2.0, 4.0, 6.0, 8.0]
# Grid d[a][c]: row = axial station, col = circumferential measurement.
GRID = [
    [0.00, 0.00, 0.00],  # x=0
    [0.10, 0.20, 0.05],  # x=2  (station1)
    [0.15, 0.30, 0.10],  # x=4  (station2)
    [0.05, 0.25, 0.00],  # x=6  (station3)
    [0.00, 0.00, 0.00],  # x=8
]
INTACT = 2583.3


# --- projection layer ------------------------------------------------------
def test_project_max_profile():
    d_eff = project_grid(GRID, T, projection=PROJECTION_MAX)
    assert list(d_eff) == pytest.approx([0.0, 0.20, 0.30, 0.25, 0.0])


def test_project_area_weighted_is_circ_mean():
    d_eff = project_grid(GRID, T, projection=PROJECTION_AREA_WEIGHTED)
    assert list(d_eff) == pytest.approx([0.0, 0.35 / 3, 0.55 / 3, 0.10, 0.0], rel=1e-6)


def test_area_weighted_equal_circ_weights_match_mean():
    # explicit equal arc widths must equal the implicit (None) mean
    d_default = project_grid(GRID, T, projection=PROJECTION_AREA_WEIGHTED)
    d_explicit = project_grid(
        GRID,
        T,
        projection=PROJECTION_AREA_WEIGHTED,
        circ_weights=[2.0, 2.0, 2.0],
    )
    assert list(d_explicit) == pytest.approx(list(d_default), rel=1e-12)


# --- golden: MAX projection ------------------------------------------------
def test_max_projection_golden():
    r = rstreng_2d_river_bottom(D, T, X, GRID, SMYS, projection=PROJECTION_MAX)
    assert isinstance(r, RiverBottom2DResult)
    assert isinstance(r.result, CorrodedPipeResult)
    assert r.projection_rule == "max"
    assert r.d_eff_in == pytest.approx([0.0, 0.20, 0.30, 0.25, 0.0])
    assert r.governing_segment == (1, 4)
    assert r.result.details["critical_length_in"] == pytest.approx(6.0)
    assert r.result.area_ratio == pytest.approx(0.4333, rel=1e-3)
    assert r.result.folias_factor == pytest.approx(1.6888, rel=1e-3)
    assert r.intact_pressure_psi == pytest.approx(INTACT, rel=1e-4)
    assert r.failure_pressure_psi == pytest.approx(1969.2, rel=2e-3)
    assert r.rsf == pytest.approx(0.7623, rel=2e-3)
    assert r.safe_pressure_psi == pytest.approx(1416.7, rel=2e-3)
    assert r.within_applicability is True


def test_max_is_default_projection():
    r_default = rstreng_2d_river_bottom(D, T, X, GRID, SMYS)
    assert r_default.projection_rule == "max"
    assert r_default.failure_pressure_psi == pytest.approx(1969.2, rel=2e-3)


# --- golden: AREA-WEIGHTED projection --------------------------------------
def test_area_weighted_projection_golden():
    r = rstreng_2d_river_bottom(
        D, T, X, GRID, SMYS, projection=PROJECTION_AREA_WEIGHTED
    )
    assert r.projection_rule == "area_weighted"
    assert r.d_eff_in == pytest.approx([0.0, 0.35 / 3, 0.55 / 3, 0.10, 0.0], rel=1e-6)
    assert r.governing_segment == (0, 4)
    assert r.result.details["critical_length_in"] == pytest.approx(8.0)
    assert r.result.area_ratio == pytest.approx(0.2000, rel=1e-3)
    # intermediate z = L^2/(D*t) = 64/12 = 5.333 (NOT 4.0)
    z = 8.0**2 / (D * T)
    assert z == pytest.approx(5.3333, rel=1e-4)
    assert r.result.folias_factor == pytest.approx(2.0617, rel=1e-3)
    assert r.failure_pressure_psi == pytest.approx(2288.7, rel=2e-3)
    assert r.rsf == pytest.approx(0.8859, rel=2e-3)
    assert r.safe_pressure_psi == pytest.approx(1646.5, rel=2e-3)
    assert r.details["affected_width"] == pytest.approx(3.0)


def test_area_weighted_less_conservative_than_max():
    r_max = rstreng_2d_river_bottom(D, T, X, GRID, SMYS, projection="max")
    r_area = rstreng_2d_river_bottom(D, T, X, GRID, SMYS, projection="area_weighted")
    # area-weighted predicts a HIGHER (less conservative) burst pressure
    assert r_area.failure_pressure_psi > r_max.failure_pressure_psi
    ratio = r_area.failure_pressure_psi / r_max.failure_pressure_psi
    assert ratio == pytest.approx(1.16, abs=0.02)


# --- result surfacing + applicability --------------------------------------
def test_intact_pressure_formula():
    r = rstreng_2d_river_bottom(D, T, X, GRID, SMYS)
    flow = SMYS + 10_000.0
    assert r.intact_pressure_psi == pytest.approx(2.0 * flow * T / D, rel=1e-9)
    assert r.result.flow_stress_psi == pytest.approx(62_000.0)


def test_maop_acceptance_flag_passthrough():
    r = rstreng_2d_river_bottom(D, T, X, GRID, SMYS, maop_psi=1000.0)
    # safe@1.39 = 1416.7 >= 1000 -> acceptable
    assert r.result.acceptable is True
    r2 = rstreng_2d_river_bottom(D, T, X, GRID, SMYS, maop_psi=2000.0)
    assert r2.result.acceptable is False


def test_deep_grid_flags_outside_applicability():
    deep = [
        [0.0, 0.0, 0.0],
        [0.30, 0.45, 0.20],  # 0.45/0.5 = 0.90 > 0.80
        [0.10, 0.20, 0.05],
        [0.0, 0.0, 0.0],
    ]
    r = rstreng_2d_river_bottom(D, T, [0.0, 2.0, 4.0, 6.0], deep, SMYS)
    assert r.within_applicability is False
    assert r.details["max_d_over_t"] == pytest.approx(0.90, rel=1e-6)


def test_default_safety_factor_value():
    assert DEFAULT_SAFETY_FACTOR == 1.39


# --- validation / errors ---------------------------------------------------
def test_unknown_projection_raises():
    with pytest.raises(ValueError, match="projection"):
        rstreng_2d_river_bottom(D, T, X, GRID, SMYS, projection="median")


def test_axial_length_mismatch_raises():
    with pytest.raises(ValueError, match="number of grid rows"):
        rstreng_2d_river_bottom(D, T, [0.0, 2.0, 4.0], GRID, SMYS)


def test_depth_exceeds_wall_raises():
    bad = [[0.0, 0.0, 0.0], [0.10, 0.60, 0.05], [0.0, 0.0, 0.0]]
    with pytest.raises(ValueError, match=r"\[0, t"):
        rstreng_2d_river_bottom(D, T, [0.0, 2.0, 4.0], bad, SMYS)


def test_bad_circ_weights_length_raises():
    with pytest.raises(ValueError, match="circ_weights"):
        project_grid(
            GRID,
            T,
            projection=PROJECTION_AREA_WEIGHTED,
            circ_weights=[1.0, 1.0],
        )


def test_no_pandas_pure_numpy():
    # guard: projection returns a numpy array, not a pandas object
    d_eff = project_grid(GRID, T)
    assert isinstance(d_eff, np.ndarray)
