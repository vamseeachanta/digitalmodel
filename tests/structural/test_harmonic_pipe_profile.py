"""Illustrative examples: independent identities for harmonic pipe geometry."""

import math
from dataclasses import FrozenInstanceError
from itertools import accumulate

import pytest

from digitalmodel.cad.units import resolve_units
from digitalmodel.drilling_riser.section import cross_sectional_area
from digitalmodel.structural.analysis.wall_thickness import PipeGeometry
from digitalmodel.structural.ovality.harmonic_profile import (
    HarmonicPipe,
    RoundnessLimit,
    capped_roundness_limit,
    diameter_thickness_ratio,
    sample_profile,
    summarize_profile,
)
from digitalmodel.structural.ovality.workflow import _out_of_roundness


CARDINALS = (0.0, 90.0, 180.0, 270.0)


def test_existing_scalar_ratio_for_illustrative_diameters():
    assert _out_of_roundness(0.8, 0.81, 0.79) == pytest.approx(0.02 / 0.8)


def test_ovality_adapter_matches_extreme_diameter_definition():
    assert HarmonicPipe(0.8, 0.02).ovality_fraction == pytest.approx(
        _out_of_roundness(0.8, 0.81, 0.79)
    )


def test_existing_circle_area_identity():
    assert cross_sectional_area(0.8, 0.0) == pytest.approx(math.pi * (0.8 / 2) ** 2)


def test_boundary_reuses_exact_repository_unit_factors():
    pipe = HarmonicPipe.from_inches_and_mm(20.0, 4.0)
    assert pipe.diameter_m == pytest.approx(0.508)
    assert pipe.out_of_roundness_m == pytest.approx(0.004)
    assert resolve_units("mm", None).metres_per_unit == 1 / 1000
    assert resolve_units("in", None).metres_per_unit == 254 / 10000


def test_inputs_are_frozen():
    pipe = HarmonicPipe(0.8, 0.02)
    with pytest.raises(FrozenInstanceError):
        pipe.diameter_m = 0.9


def test_small_diameter_difference_retains_scalar_precision():
    pipe = HarmonicPipe(0.8, 1e-20)
    assert pipe.ovality_fraction == pytest.approx(1e-20 / 0.8, rel=1e-14, abs=0)


def test_tiny_difference_retains_profile_deviations_and_summary():
    pipe = HarmonicPipe(0.8, 1e-20)
    points = sample_profile(pipe, CARDINALS)
    for point, sign in zip(points, (1, -1, 1, -1)):
        assert point.radial_difference_m == pytest.approx(sign * 1e-20 / 4, rel=1e-14, abs=0)
    summary = summarize_profile(pipe, CARDINALS, axis_offset_m=0.0)
    assert summary.diameter_difference_m == pytest.approx(1e-20, rel=1e-14, abs=0)


def test_circle_limit_at_cardinal_directions():
    points = sample_profile(HarmonicPipe(0.8, 0.0), CARDINALS)
    for point, expected in zip(points, ((0.4, 0), (0, 0.4), (-0.4, 0), (0, -0.4))):
        assert (point.x_m, point.y_m) == pytest.approx(expected, abs=1e-14)
        assert point.radius_m == pytest.approx(0.4)
        assert point.radial_difference_m == pytest.approx(0.0, abs=1e-14)
        assert (point.x_m, point.y_m) == (point.circle_x_m, point.circle_y_m)


def test_cardinal_diameter_difference_and_area_proxy():
    pipe = HarmonicPipe(0.8, 0.02)
    result = summarize_profile(pipe, CARDINALS, axis_offset_m=0.03)
    # Hand identities: axial radii are D/2 +/- q/4.
    a, b = 0.8 / 2 + 0.02 / 4, 0.8 / 2 - 0.02 / 4
    assert result.max_x_m == pytest.approx(a)
    assert result.max_y_m == pytest.approx(b)
    assert result.offset_max_x_m == pytest.approx(a + 0.03)
    assert result.offset_max_y_m == pytest.approx(b + 0.03)
    assert result.axis_product_area_m2 == pytest.approx(math.pi * a * b)
    assert result.circle_area_m2 == pytest.approx(math.pi * (0.8 / 2) ** 2)
    assert result.axis_product_area_m2 == pytest.approx(0.502576284)
    assert result.circle_area_m2 == pytest.approx(0.502654825)
    assert result.diameter_difference_m == pytest.approx(0.02)
    assert result.max_radial_difference_m == pytest.approx(0.02 / 4)
    assert result.min_radial_difference_m == pytest.approx(-0.02 / 4)


def test_oblique_point_by_double_angle_identity():
    pipe = HarmonicPipe(0.8, 0.02)
    angle = math.radians(37.0)
    point, = sample_profile(pipe, (37.0,))
    expected_r = 0.8 / 2 + 0.02 / 4 * (2 * math.cos(angle) ** 2 - 1)
    assert point.radius_m == pytest.approx(expected_r)
    assert point.radius_m == pytest.approx(0.4013781868)
    assert point.x_m == pytest.approx(expected_r * math.cos(angle))
    assert point.y_m == pytest.approx(expected_r * math.sin(angle))
    assert point.radial_difference_m == pytest.approx(expected_r - 0.8 / 2)


def test_periodicity_and_opposite_direction_symmetry():
    pipe = HarmonicPipe(0.8, 0.02)
    base, full_turn, negative_turn, opposite = sample_profile(
        pipe, (37.0, 397.0, -323.0, 217.0)
    )
    for point in (full_turn, negative_turn):
        assert point.radius_m == pytest.approx(base.radius_m)
        assert (point.x_m, point.y_m) == pytest.approx((base.x_m, base.y_m))
    assert (opposite.x_m, opposite.y_m) == pytest.approx((-base.x_m, -base.y_m))


def test_dimensional_scaling():
    small = summarize_profile(HarmonicPipe(0.8, 0.02), CARDINALS, axis_offset_m=0.03)
    large = summarize_profile(HarmonicPipe(1.6, 0.04), CARDINALS, axis_offset_m=0.06)
    assert large.max_x_m == pytest.approx(2 * small.max_x_m)
    assert large.diameter_difference_m == pytest.approx(2 * small.diameter_difference_m)
    assert large.axis_product_area_m2 == pytest.approx(4 * small.axis_product_area_m2)


def test_summary_uses_sampled_maxima_and_area_is_only_a_proxy():
    pipe = HarmonicPipe(0.8, 0.8)
    coarse = summarize_profile(pipe, CARDINALS, axis_offset_m=0.0)
    dense = summarize_profile(pipe, (*CARDINALS, 37.0, 53.0), axis_offset_m=0.0)
    assert dense.max_y_m > coarse.max_y_m
    # Exact polar integration: (1/2)*integral(r**2 dtheta).
    exact_enclosed_area = math.pi * (0.8 / 2) ** 2 * (1 + (0.8 / (2 * 0.8)) ** 2 / 2)
    assert coarse.axis_product_area_m2 != pytest.approx(exact_enclosed_area)


@pytest.mark.parametrize("angles", [
    [i * 0.1 for i in range(3600)],
    list(accumulate([0.1] * 3600, initial=0.0)),
    [angle + 5e-10 for angle in CARDINALS],
    [angle - 5e-10 for angle in CARDINALS],
], ids=["fractional-grid", "accumulated-grid", "positive-roundoff", "negative-roundoff"])
def test_summary_accepts_cardinal_roundoff(angles):
    result = summarize_profile(HarmonicPipe(0.8, 0.02), angles, axis_offset_m=0.0)
    assert result.diameter_difference_m == pytest.approx(0.02)
    assert result.axis_product_area_m2 == pytest.approx(0.502576284)


@pytest.mark.parametrize("fraction,cap,expected", [(0.02, 0.01, 0.01), (0.02, 0.03, 0.016), (0.0, 0.0, 0.0)])
def test_required_policy_inputs_apply_relative_limit_and_absolute_cap(fraction, cap, expected):
    result = capped_roundness_limit(RoundnessLimit(0.8, fraction, cap))
    assert result.relative_limit_m == pytest.approx(0.8 * fraction)
    assert result.capped_limit_m == pytest.approx(expected)


def test_existing_diameter_thickness_ratio():
    assert PipeGeometry(0.8, 0.02, fabrication_tolerance=0.0).d_over_t == 40.0


def test_diameter_thickness_adapter():
    assert diameter_thickness_ratio(0.8, 0.02) == 40.0


@pytest.mark.parametrize("bad", [-0.2, math.nan, math.inf, -math.inf, True, "0.8", None])
def test_invalid_diameter(bad):
    with pytest.raises(ValueError):
        HarmonicPipe(bad, 0.02)


@pytest.mark.parametrize("diameter,difference", [(0.0, 0.0), (0.8, -0.02), (0.8, 1.6), (0.8, 1.7), (0.8, math.nan), (0.8, True)])
def test_degenerate_or_invalid_profile(diameter, difference):
    with pytest.raises(ValueError):
        HarmonicPipe(diameter, difference)


@pytest.mark.parametrize("angle", [math.nan, math.inf, True, "37"])
def test_invalid_angle(angle):
    with pytest.raises(ValueError):
        sample_profile(HarmonicPipe(0.8, 0.02), (angle,))


@pytest.mark.parametrize("angles", [
    (), (37.0,), (180.0, 270.0),
    (0.0, 90.0 + 2e-9, 180.0, 270.0),
    (0.0, 90.0, 180.0, 270.0 + 2e-9),
    (360.0 - 2e-9, 90.0, 180.0, 270.0),
])
def test_summary_requires_all_cardinal_directions(angles):
    with pytest.raises(ValueError, match="cardinal"):
        summarize_profile(HarmonicPipe(0.8, 0.02), angles, axis_offset_m=0.0)


@pytest.mark.parametrize("offset", [-0.03, math.nan, math.inf, True])
def test_invalid_offset(offset):
    with pytest.raises(ValueError):
        summarize_profile(HarmonicPipe(0.8, 0.02), CARDINALS, axis_offset_m=offset)


@pytest.mark.parametrize("fraction,cap", [(-0.02, 0.02), (math.nan, 0.02), (0.02, -0.02), (0.02, math.inf), (True, 0.02)])
def test_invalid_limit_inputs(fraction, cap):
    with pytest.raises(ValueError):
        capped_roundness_limit(RoundnessLimit(0.8, fraction, cap))


@pytest.mark.parametrize("thickness", [0.0, -0.02, 0.4, math.nan, True])
def test_invalid_wall_geometry(thickness):
    with pytest.raises(ValueError):
        diameter_thickness_ratio(0.8, thickness)


@pytest.mark.parametrize("diameter", [0.0, -0.2, math.nan, math.inf, True, "0.8", None])
def test_invalid_ratio_diameter(diameter):
    with pytest.raises(ValueError):
        diameter_thickness_ratio(diameter, 0.02)


@pytest.mark.parametrize("diameter,difference", [(1e-320, 0.0), (0.8, 5e-324), (1.0, 1.5e-323)])
def test_unresolved_geometry_raises(diameter, difference):
    with pytest.raises(ValueError):
        summarize_profile(HarmonicPipe(diameter, difference), CARDINALS, axis_offset_m=0.0)


def test_finite_inputs_cannot_return_nonfinite_area_or_allowance():
    with pytest.raises(ValueError):
        summarize_profile(HarmonicPipe(1e200, 0.0), CARDINALS, axis_offset_m=0.0)
    with pytest.raises(ValueError):
        capped_roundness_limit(RoundnessLimit(1e308, 4.0, 0.02))


@pytest.mark.parametrize("diameter,difference", [(False, 4.0), (20.0, -4.0), (math.inf, 4.0)])
def test_boundary_rejects_invalid_units_values(diameter, difference):
    with pytest.raises(ValueError):
        HarmonicPipe.from_inches_and_mm(diameter, difference)
