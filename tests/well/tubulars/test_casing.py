# ABOUTME: Golden tests for the API 5CT casing catalog — API 5C3 collapse
# ABOUTME: (4-regime), burst, and body-yield validated vs published ratings.
import math

import pytest

from digitalmodel.well.tubulars import (
    Casing,
    CasingGrade,
    TubularGeometry,
    casing,
    casing_grade,
    list_sizes,
    wall_for_size,
)


# --- published API ratings: burst, body-yield, collapse (across regimes) -----
@pytest.mark.parametrize(
    "grade, od, ppf, burst, body_yield, collapse, regime",
    [
        # 7" 26# P110 — plastic collapse regime.
        ("P110", 7.0, 26.00, 9960.0, 830_000.0, 6230.0, "plastic"),
        # 9-5/8" 47# N80 — plastic.
        ("N80", 9.625, 47.00, 6870.0, 1_086_000.0, 4760.0, "plastic"),
        # 13-3/8" 68# K55 — transition collapse regime.
        ("K55", 13.375, 68.00, 3450.0, 1_070_000.0, 1950.0, "transition"),
    ],
)
def test_published_api_ratings(grade, od, ppf, burst, body_yield, collapse,
                               regime):
    c = casing(grade, od_in=od, weight_ppf=ppf)
    assert math.isclose(c.burst_psi, burst, rel_tol=3e-3)
    assert math.isclose(c.body_yield_lbf, body_yield, rel_tol=3e-3)
    assert math.isclose(c.collapse_psi, collapse, rel_tol=3e-3)
    assert c.collapse_regime == regime


# --- all four collapse regimes are reachable ---------------------------------
def test_yield_collapse_regime_low_dt():
    # 5" 23.2# P110: D/t = 10.46 -> yield regime.
    c = casing("P110", od_in=5.0, weight_ppf=23.20)
    assert c.collapse_regime == "yield"
    assert c.collapse_psi > 0


def test_elastic_collapse_regime_high_dt():
    # 20" 94# K55: D/t = 45.7 -> elastic regime.
    c = casing("K55", od_in=20.0, weight_ppf=94.00)
    assert c.collapse_regime == "elastic"


def test_collapse_decreases_with_d_over_t():
    # Same grade, increasing wall -> lower D/t -> higher collapse.
    thin = casing("P110", od_in=7.0, weight_ppf=20.00)   # D/t larger
    thick = casing("P110", od_in=7.0, weight_ppf=35.00)  # D/t smaller
    assert thick.d_over_t < thin.d_over_t
    assert thick.collapse_psi > thin.collapse_psi


# --- grades ------------------------------------------------------------------
def test_grade_min_max_yield():
    g = casing_grade("L80")
    assert isinstance(g, CasingGrade)
    assert g.min_yield_psi == 80_000 and g.max_yield_psi == 95_000

    assert casing_grade("p110").min_yield_psi == 110_000  # case-insensitive
    with pytest.raises(KeyError):
        casing_grade("ZZ99")


# --- geometry / query --------------------------------------------------------
def test_geometry_and_query():
    c = casing("P110", od_in=7.0, weight_ppf=26.00)
    assert c.wt_in == 0.362
    assert math.isclose(c.id_in, 7.0 - 2 * 0.362, rel_tol=1e-12)
    assert math.isclose(c.d_over_t, 7.0 / 0.362, rel_tol=1e-12)
    assert "P110" in c.reference and "7\"" in c.reference
    assert wall_for_size(7.0, 26.00) == 0.362
    assert (7.0, 26.00) in list_sizes()


def test_explicit_wall_path():
    c = casing("N80", od_in=9.625, wt_in=0.472)
    assert c.wt_in == 0.472
    assert c.weight_ppf is None


def test_errors():
    with pytest.raises(ValueError):
        casing("P110", od_in=7.0, weight_ppf=26.0, wt_in=0.362)  # both
    with pytest.raises(ValueError):
        casing("P110", od_in=7.0)                                # no wall
    with pytest.raises(KeyError):
        casing("P110", od_in=7.0, weight_ppf=99.0)               # bad size
    with pytest.raises(ValueError):
        Casing(od_in=7.0, wt_in=4.0, grade=casing_grade("P110"))  # wt>=od/2


# --- code reference + envelope bridge ----------------------------------------
def test_code_reference():
    assert casing("P110", od_in=7.0, weight_ppf=26.0).code_reference == \
        "API 5CT / API 5C3"


def test_to_tubular_geometry_feeds_design_envelope():
    c = casing("P110", od_in=7.0, weight_ppf=26.00)
    geom = c.to_tubular_geometry()
    assert isinstance(geom, TubularGeometry)
    assert geom.od_in == 7.0
    assert math.isclose(geom.burst_rating_psi, c.burst_psi, rel_tol=1e-12)
    assert math.isclose(geom.collapse_rating_psi, c.collapse_psi, rel_tol=1e-12)
    assert geom.smys_psi == 110_000
