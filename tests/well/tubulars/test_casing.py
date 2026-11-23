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


# Independent mechanics reference: OpenStax, University Physics Volume 1 (2016),
# section 12.3, equation 12.34 (stress = force / area):
# https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus
# Geometry and specified strengths below are synthetic inputs, not API catalog
# qualification. API TR 5C3 / ISO 10400 edition and clause text were unavailable:
# absolute burst wall-factor and collapse-coefficient checks are NOT_EVALUATED.
@pytest.mark.parametrize(
    "od, wall, expected_id, expected_ratio, expected_area",
    [
        (9.625, 0.472, 8.681, 20.39194915254237, 13.572358847521082),
        (7.0, 0.362, 6.276, 19.337016574585636, 7.549108916499515),
    ],
)
@pytest.mark.parametrize("strength", [80_000.0, 110_000.0])
def test_specified_casing_geometry_and_axial_yield_by_hand(
    od, wall, expected_id, expected_ratio, expected_area, strength,
):
    """Two sizes at each of two specified yield strengths (psi).

    Size A: ID = 9.625 - 0.944 = 8.681 in; D/t = 9.625/0.472.
    The annulus is circumference at mean diameter times wall:
    A = pi * 9.153 * 0.472 = 13.572358847521082 in2.
    F(80 ksi) = 1,085,788.707801687 lbf;
    F(110 ksi) = 1,492,959.473227319 lbf.
    Size B: ID = 7 - 0.724 = 6.276 in; D/t = 7/0.362.
    A = pi * 6.638 * 0.362 = 7.549108916499515 in2.
    F(80 ksi) = 603,928.713319961 lbf;
    F(110 ksi) = 830,401.980814947 lbf.
    Equation 12.34 gives F = specified stress * metal area; no API
    minimum-grade or damaged-wall qualification is inferred.
    """
    from digitalmodel.well.tubulars.casing import pipe_body_yield_strength

    forces = {
        (9.625, 80_000.0): 1_085_788.707801687,
        (9.625, 110_000.0): 1_492_959.473227319,
        (7.0, 80_000.0): 603_928.713319961,
        (7.0, 110_000.0): 830_401.980814947,
    }
    grade = CasingGrade("specified", strength, strength, strength)
    product = Casing(od_in=od, wt_in=wall, grade=grade)
    # Relative 1e-12 covers decimal-reference and binary arithmetic rounding;
    # it is a numerical comparison tolerance, not an engineering allowance.
    assert product.id_in == pytest.approx(expected_id, rel=1e-12)
    assert product.d_over_t == pytest.approx(expected_ratio, rel=1e-12)
    assert product.metal_area_in2 == pytest.approx(expected_area, rel=1e-12)
    expected_force = forces[od, strength]
    assert product.body_yield_lbf == pytest.approx(expected_force, rel=1e-12)
    assert pipe_body_yield_strength(od, wall, strength) == pytest.approx(
        expected_force, rel=1e-12,
    )


def test_burst_limiting_and_scaling_invariants_without_wall_factor_claim():
    """Zero wall/strength gives zero capacity; doubling strength doubles it.

    These invariants cannot establish the API wall factor. Its edition and
    clause remain NOT_EVALUATED; no absolute nonzero burst value is asserted.
    """
    from digitalmodel.well.tubulars.casing import internal_yield_pressure

    assert internal_yield_pressure(9.625, 0.472, 0.0) == 0.0
    assert internal_yield_pressure(9.625, 0.0, 80_000.0) == 0.0
    low = internal_yield_pressure(9.625, 0.472, 80_000.0)
    high = internal_yield_pressure(9.625, 0.472, 160_000.0)
    assert high == pytest.approx(2 * low, rel=1e-12)
    product = Casing(9.625, 0.472, CasingGrade("specified", 160_000, 160_000, 160_000))
    assert product.burst_psi == pytest.approx(2 * low, rel=1e-12)


@pytest.mark.parametrize("ratio", [10.0, 20.0, 30.0, 60.0])
def test_collapse_geometric_scale_invariance(ratio):
    """Equal D/t gives equal pressure and regime at either absolute size.

    D/t values are synthetic probes, not sourced API regime boundaries.
    Coefficients and absolute ratings remain NOT_EVALUATED. Relative 1e-12
    accommodates floating-point division of geometrically similar inputs.
    """
    from digitalmodel.well.tubulars.casing import collapse_pressure

    small_pressure, small_regime = collapse_pressure(ratio, 1.0, 80_000.0)
    large_pressure, large_regime = collapse_pressure(2 * ratio, 2.0, 80_000.0)
    product = Casing(ratio, 1.0, CasingGrade("specified", 80_000, 80_000, 80_000))
    assert large_pressure == pytest.approx(small_pressure, rel=1e-12)
    assert large_regime == small_regime == product.collapse_regime
    assert product.collapse_psi == pytest.approx(small_pressure, rel=1e-12)


@pytest.mark.parametrize(
    "left_regime, right_regime",
    [("yield", "plastic"), ("plastic", "transition"), ("transition", "elastic")],
)
def test_collapse_public_regime_interfaces_are_continuous(left_regime, right_regime):
    """Locate each implemented interface without importing private constants.

    Bisection of the public classifier brackets an interface to adjacent
    floating-point ratios. Continuity compares its two one-sided pressures;
    this is an invariant, not an independently established API boundary or
    absolute pressure. Standard boundary equations remain NOT_EVALUATED.
    Relative 1e-9 allows accumulated algebraic roundoff at the interface.
    """
    from digitalmodel.well.tubulars.casing import collapse_pressure

    order = {"yield": 0, "plastic": 1, "transition": 2, "elastic": 3}
    # Synthetic search interval; neither end is an acceptance limit.
    low, high = 5.0, 100.0
    assert collapse_pressure(low, 1.0, 80_000.0)[1] == "yield"
    assert collapse_pressure(high, 1.0, 80_000.0)[1] == "elastic"
    for _ in range(64):
        mid = (low + high) / 2
        if order[collapse_pressure(mid, 1.0, 80_000.0)[1]] <= order[left_regime]:
            low = mid
        else:
            high = mid
    left_pressure, left = collapse_pressure(low, 1.0, 80_000.0)
    right_pressure, right = collapse_pressure(high, 1.0, 80_000.0)
    assert (left, right) == (left_regime, right_regime)
    assert left_pressure == pytest.approx(right_pressure, rel=1e-9)
