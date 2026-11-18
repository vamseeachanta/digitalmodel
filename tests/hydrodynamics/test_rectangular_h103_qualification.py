"""Independent arithmetic and conservative status tests; no solver is imported.

All dimensions and flow inputs below are synthetic arithmetic examples, not
measured data or published material properties. Approximate comparisons use
pytest's default floating-point roundoff allowance, not engineering acceptance.
"""

import math

import pytest

from digitalmodel.hydrodynamics.rectangular_h103_qualification import (
    FiniteLookupDomain,
    flow_numbers,
    qualify_rectangular_h103,
)


def test_axis_dimensions_independent_hand_calculation():
    """For L=8, W=2, H=1 m, projected areas are 2, 8, 16 m².

    Sort each face: (a,b,c) = (1,2,8), (1,8,2), (2,8,1).
    Thus b/a = 2,8,4; c/sqrt(ab) = 8/sqrt(2),2/sqrt(8),1/4.
    Branch convention: existing rectangular calculator get_translational_cd;
    a ratio below unity selects plate, otherwise prism. This tests arithmetic
    consistency with that convention, not H103 physical validity.
    """
    result = qualify_rectangular_h103(length_m=8, width_m=2, height_m=1)
    for axis, area, ratio, slenderness, branch in (
        ("x", 2, 2, 8 / math.sqrt(2), "prism"),
        ("y", 8, 8, 2 / math.sqrt(8), "plate"),
        ("z", 16, 4, 0.25, "plate"),
    ):
        row = result["axes"][axis]
        assert row["projected_area_m2"] == pytest.approx(area)
        assert row["projected_aspect_ratio"] == pytest.approx(ratio)
        assert row["streamwise_aspect_ratio"] == pytest.approx(slenderness)
        assert row["drag_branch"] == branch
    assert result["axes"]["x"]["drag_geometry"] == "NON_SQUARE_ROD_APPROXIMATION"


def test_cube_does_not_establish_physical_qualification():
    """A synthetic unit cube has unity aspect ratios on every axis.

    Equality selects prism per get_translational_cd; square geometry alone
    supplies neither Reynolds applicability nor a horizontal added-mass basis.
    """
    result = qualify_rectangular_h103(length_m=1, width_m=1, height_m=1)
    assert result["status"] == "NOT_EVALUATED"
    for row in result["axes"].values():
        assert row["drag_branch"] == "prism"
        assert row["drag_geometry"] == "SQUARE_ROD_GEOMETRY_ONLY"
        assert row["drag_lookup_status"] == "NOT_EVALUATED"
        assert row["added_mass_lookup_status"] == "NOT_EVALUATED"
        assert row["added_mass_applicability"] == "NOT_EVALUATED"


def test_scale_and_axis_permutation_invariants():
    """Scaling all lengths by 2 gives 4 times area and unchanged ratios.

    Exchanging L and W exchanges x and y, with z unchanged. These follow
    directly from rectangular projection products, independent of any table.
    """
    base = qualify_rectangular_h103(length_m=8, width_m=2, height_m=1)["axes"]
    scaled = qualify_rectangular_h103(length_m=16, width_m=4, height_m=2)["axes"]
    swapped = qualify_rectangular_h103(length_m=2, width_m=8, height_m=1)["axes"]
    for axis in base:
        assert scaled[axis]["projected_area_m2"] == 4 * base[axis]["projected_area_m2"]
        assert (
            scaled[axis]["projected_aspect_ratio"]
            == base[axis]["projected_aspect_ratio"]
        )
        assert (
            scaled[axis]["streamwise_aspect_ratio"]
            == base[axis]["streamwise_aspect_ratio"]
        )
    assert swapped["x"] == base["y"]
    assert swapped["y"] == base["x"]
    assert swapped["z"] == base["z"]


def test_dimensionless_flow_independent_hand_calculation():
    """Synthetic U=2 m/s, D=0.5 m, nu=0.000002 m²/s, T=3 s.

    NASA Glenn, 'Reynolds Number', Re=UD/nu: 2*0.5/0.000002=500000.
    'Forces on Cylinders and Plates in an Oscillating Fluid' (1958), p.423,
    period parameter UT/D: 2*3/0.5=12. Neither value is an acceptance limit.
    """
    result = flow_numbers(
        speed_m_s=2,
        characteristic_length_m=0.5,
        kinematic_viscosity_m2_s=0.000002,
        period_s=3,
    )
    assert result["reynolds_number"] == pytest.approx(500000)
    assert result["keulegan_carpenter_number"] == pytest.approx(12)
    assert result["status"] == "NOT_EVALUATED"
    assert result["sources"]


def test_missing_flow_evidence_is_explicit():
    """No viscosity or period means no Re or KC, even at known velocity."""
    result = flow_numbers(speed_m_s=2, characteristic_length_m=0.5)
    assert result["reynolds_number"] is None
    assert result["keulegan_carpenter_number"] is None
    assert "kinematic_viscosity_m2_s" in result["missing_inputs"]
    assert "period_s" in result["missing_inputs"]


def test_partial_flow_evidence_is_not_inferred():
    """For synthetic U=D=T=1, KC=1; viscosity and hence Re remain unknown."""
    result = flow_numbers(speed_m_s=1, characteristic_length_m=1, period_s=1)
    assert result["keulegan_carpenter_number"] == 1
    assert result["reynolds_number"] is None
    assert result["missing_inputs"] == ["kinematic_viscosity_m2_s"]


@pytest.mark.parametrize("size", [1e-200, 1e200])
def test_unrepresentable_projection_is_rejected(size):
    """Synthetic extreme scales underflow/overflow area in float arithmetic.

    The diagnostic shall reject the projection instead of returning a true
    zero, infinite ratio, or an incidental division-by-zero error.
    """
    with pytest.raises(ValueError, match="derived geometry"):
        qualify_rectangular_h103(length_m=size, width_m=size, height_m=size)


def test_lookup_lower_bound_and_missing_selected_domain():
    """A synthetic cube ratio=1 is below supplied prism domain [2,3].

    A supplied plate domain cannot cover the selected prism branch. These
    endpoints are synthetic test policy, not values from a licensed table.
    """
    domain = FiniteLookupDomain(2, 3, "Synthetic arithmetic domain, this test")
    outside = qualify_rectangular_h103(
        length_m=1,
        width_m=1,
        height_m=1,
        prism_drag_domain=domain,
    )
    missing = qualify_rectangular_h103(
        length_m=1,
        width_m=1,
        height_m=1,
        plate_drag_domain=domain,
    )
    assert outside["axes"]["z"]["drag_lookup_status"] == "OUTSIDE_SUPPLIED_DOMAIN"
    assert missing["axes"]["z"]["drag_lookup_status"] == "NOT_EVALUATED"


def test_zero_speed_is_a_true_zero():
    """UD/nu and UT/D are both zero when U=0 and other inputs are known."""
    result = flow_numbers(
        speed_m_s=0, characteristic_length_m=1, kinematic_viscosity_m2_s=1, period_s=1
    )
    assert result["reynolds_number"] == 0
    assert result["keulegan_carpenter_number"] == 0


def test_finite_lookup_coverage_is_not_acceptance():
    """Synthetic domains [1,4] for plate, [1,2] for prism; not H103 values.

    For the 8x2x1 box, x prism ratio=8/sqrt(2)>2; y plate ratio=8>4;
    z plate ratio=4 is exactly the upper endpoint. Added-mass b/a ratios
    are 2,8,4. No extrapolation or infinity-sentinel interpolation is allowed.
    """
    plate = FiniteLookupDomain(1, 4, "Synthetic arithmetic domain, this test")
    prism = FiniteLookupDomain(1, 2, "Synthetic arithmetic domain, this test")
    result = qualify_rectangular_h103(
        length_m=8,
        width_m=2,
        height_m=1,
        plate_drag_domain=plate,
        prism_drag_domain=prism,
        added_mass_domain=plate,
    )
    axes = result["axes"]
    assert axes["x"]["drag_lookup_status"] == "OUTSIDE_SUPPLIED_DOMAIN"
    assert axes["y"]["drag_lookup_status"] == "OUTSIDE_SUPPLIED_DOMAIN"
    assert axes["z"]["drag_lookup_status"] == "WITHIN_SUPPLIED_DOMAIN"
    assert axes["x"]["added_mass_lookup_status"] == "WITHIN_SUPPLIED_DOMAIN"
    assert axes["y"]["added_mass_lookup_status"] == "OUTSIDE_SUPPLIED_DOMAIN"
    assert result["status"] == "NOT_EVALUATED"
    assert result["lookup_sources"]["plate_drag"] == plate.source


@pytest.mark.parametrize(
    "lower,upper,source",
    [
        (0, 2, "test"),
        (2, 1, "test"),
        (1, math.inf, "test"),
        (math.nan, 2, "test"),
        (1, 2, " "),
    ],
)
def test_invalid_or_unsourced_domain_is_rejected(lower, upper, source):
    """Finite bounds and a source are required; no numerical acceptance policy."""
    with pytest.raises(ValueError):
        FiniteLookupDomain(lower, upper, source)


@pytest.mark.parametrize("field", ["length_m", "width_m", "height_m"])
@pytest.mark.parametrize("invalid", [0, -1, math.inf, math.nan])
def test_invalid_geometry_is_rejected(field, invalid):
    """Lengths must be finite and positive for projection ratios to exist."""
    inputs = dict(length_m=1, width_m=1, height_m=1)
    inputs[field] = invalid
    with pytest.raises(ValueError, match=field):
        qualify_rectangular_h103(**inputs)


@pytest.mark.parametrize(
    "field,invalid",
    [
        ("speed_m_s", -1),
        ("speed_m_s", math.inf),
        ("speed_m_s", math.nan),
        ("characteristic_length_m", 0),
        ("characteristic_length_m", math.nan),
        ("kinematic_viscosity_m2_s", 0),
        ("kinematic_viscosity_m2_s", math.inf),
        ("period_s", 0),
        ("period_s", math.nan),
    ],
)
def test_invalid_flow_is_rejected(field, invalid):
    """Signed velocity is not a speed magnitude; divisors must be positive."""
    inputs = dict(
        speed_m_s=1, characteristic_length_m=1, kinematic_viscosity_m2_s=1, period_s=1
    )
    inputs[field] = invalid
    with pytest.raises(ValueError, match=field):
        flow_numbers(**inputs)
