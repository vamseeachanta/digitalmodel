"""Illustrative example inputs; expectations are independent hand calculations."""

from dataclasses import FrozenInstanceError, replace
import math

import pytest

from digitalmodel.geotechnical import piles
from digitalmodel.geotechnical.py_curves import SoftClay, soft_clay_py
from digitalmodel.geotechnical.pile_resistance import (
    AxialInputs, FrictionCurve, PileSection, ResistanceNode, SandPoint,
    axial_components, capped_sand_friction, cohesive_friction_from_curve,
    rigid_pile_response, sand_lateral_resistance,
)


def test_sand_shallow_and_deep_limits():
    point = SandPoint(3.0, 2.0, 30.0, 1.0, 2.0, 8.0)
    result = sand_lateral_resistance(point)
    assert result.shallow_kn_m == 210.0
    assert result.deep_kn_m == 480.0
    assert result.ultimate_kn_m == 210.0
    assert sand_lateral_resistance(replace(point, depth_m=30.0)).ultimate_kn_m == 480.0
    assert sand_lateral_resistance(replace(point, effective_stress_kpa=0.0)).ultimate_kn_m == 0
    with pytest.raises(FrozenInstanceError):
        point.depth_m = 4.0


def test_capped_friction_delegates_beta_method():
    expected = 0.6 * 100.0 * math.tan(math.radians(30.0))
    assert capped_sand_friction(100.0, 30.0, 0.6, 50.0) == pytest.approx(expected)
    assert capped_sand_friction(100.0, 30.0, 0.6, 25.0) == 25.0
    assert capped_sand_friction(100.0, 0.0, 0.6, 50.0) == 0


def test_capped_friction_rejects_right_angle_interface():
    with pytest.raises(ValueError, match='below 90'):
        capped_sand_friction(100.0, 90.0, 0.6, 50.0)


def test_supplied_curve_interpolation_and_flat_extension():
    curve = FrictionCurve(((0.0, 0.0), (40.0, 40.0), (80.0, 50.0), (160.0, 80.0)))
    assert cohesive_friction_from_curve(60.0, curve) == 45.0
    assert cohesive_friction_from_curve(200.0, curve) == 80.0
    assert cohesive_friction_from_curve(0.0, curve) == 0


def test_explicit_pivot_response_against_hand_sums():
    nodes = tuple(ResistanceNode(z, f) for z, f in ((0., 0.), (2., 10.), (4., 10.), (6., 10.)))
    result = rigid_pile_response(nodes, top_depth_m=0., load_depth_m=1., pivot_depth_m=4.)
    h = (10 * 2 + 10 * 2) / 3
    assert result.capacity_kn == pytest.approx(h)
    assert result.forward_moment_kn_m == (0., 10., 40., 90.)
    assert result.reverse_moment_kn_m == (0., 90., 80., 50.)
    assert result.reversal_flags == (0, 0, 0, 1)
    assert result.cumulative_force_kn == (0., 10., 20., 10.)
    assert result.centroid_depth_m == (None, 2., 3., 0.)
    assert result.shear_kn == pytest.approx((0., 10-h, 20-h, 10-h))
    assert result.bending_moment_kn_m == pytest.approx((0., -h, 20-3*h, 60-5*h))
    assert result.bending_moment_kn_m[2] == pytest.approx(-20.)
    assert result.force_residual_kn == pytest.approx(10-h)
    assert result.moment_residual_kn_m == -10.


def test_legacy_half_depth_response_is_explicit():
    nodes = tuple(ResistanceNode(z, f) for z, f in ((0., 0.), (2., 10.), (4., 10.), (6., 10.)))
    result = rigid_pile_response(nodes, top_depth_m=0., load_depth_m=1., pivot_depth_m=4.)
    h = 40 / 3
    assert result.legacy_centroid_depth_m == (None, 1., 2.5, -1.)
    assert result.legacy_bending_moment_kn_m == pytest.approx((0., 10-h, 30-3*h, 70-5*h))


def test_static_response_retains_force_at_first_node():
    nodes = tuple(ResistanceNode(z, f) for z, f in ((0., 5.), (2., 10.), (4., 10.), (6., 10.)))
    result = rigid_pile_response(nodes, top_depth_m=0., load_depth_m=1., pivot_depth_m=4.)
    assert result.capacity_kn == 20.
    assert result.centroid_depth_m == pytest.approx((0., 4/3, 2.4, 0.))
    assert result.shear_kn == (5., -5., 5., -5.)
    assert result.bending_moment_kn_m == (0., -10., -20., -10.)


@pytest.mark.parametrize('offset', [-1e-15, 1e-15])
def test_pivot_grid_accepts_roundoff(offset):
    nodes = tuple(ResistanceNode(z, f) for z, f in ((0., 0.), (2., 10.), (4., 10.), (6., 10.)))
    result = rigid_pile_response(nodes, top_depth_m=0., load_depth_m=1., pivot_depth_m=4.+offset)
    assert result.capacity_kn == pytest.approx(40/3)
    assert result.bending_moment_kn_m[2] == pytest.approx(-20.)


def test_reverse_moment_resets_at_roundoff_zero():
    nodes = tuple(ResistanceNode(z, f) for z, f in ((0., 1e-15), (2., 10.), (4., 10.), (6., 10.)))
    result = rigid_pile_response(nodes, top_depth_m=0., load_depth_m=1., pivot_depth_m=4.)
    assert result.reverse_moment_kn_m == (0., 90., 80., 50.)


def test_buried_top_and_cancelling_signed_reactions():
    nodes = tuple(ResistanceNode(z, 10.) for z in (0., 1., 2., 3.))
    result = rigid_pile_response(nodes, top_depth_m=1., load_depth_m=0., pivot_depth_m=2.)
    assert result.forward_moment_kn_m == (0., 10., 30., 60.)
    assert result.capacity_kn == 10.
    assert result.cumulative_force_kn == (0., 10., 20., 10.)
    assert result.centroid_depth_m == (None, 1., 1.5, 0.)
    assert result.bending_moment_kn_m == (0., -10., -10., 0.)
    cancellation = rigid_pile_response(
        (ResistanceNode(0., 10.), ResistanceNode(1., 10.)),
        top_depth_m=0., load_depth_m=-1., pivot_depth_m=1.,
    )
    assert cancellation.cumulative_force_kn[-1] == 0.
    assert cancellation.centroid_depth_m[-1] is None
    # A zero resultant still retains the moment of the cancelling forces.
    assert cancellation.bending_moment_kn_m[-1] == 0.


def test_axial_components_use_independent_area_formulas():
    section = PileSection(2., 0.05, 0.1, 1.)
    inputs = AxialInputs(100., 500., 250., 20., 80.)
    result = axial_components(section, inputs)
    full_area = math.pi
    inner_area = math.pi * 1.9**2 / 4
    annulus_area = full_area - inner_area + 0.1
    inner_friction = 100 * (1.9 / 2 + 1 / (2 * math.pi))
    assert result.internal_friction_kn == pytest.approx(inner_friction)
    assert result.full_base_bearing_kn == pytest.approx(500 * full_area)
    assert result.annulus_bearing_kn == pytest.approx(250 * annulus_area)
    assert result.open_end_bearing_kn == pytest.approx(inner_friction + 250*annulus_area)
    assert result.closed_compression_kn == pytest.approx(100 + 500*full_area - 20)
    assert result.uplift_kn == pytest.approx(100 + 20 + min(80, inner_friction))
    installation = 100 + inner_friction + 250*annulus_area
    assert result.installation_resistance_kn == pytest.approx(installation)
    assert result.suction_differential_kpa == pytest.approx((installation-20)/inner_area)
    heavy = axial_components(section, replace(inputs, pile_submerged_weight_kn=10000.))
    assert heavy.suction_differential_kpa < 0


def test_legacy_open_end_discontinuity_is_explicit():
    section = PileSection(2., 0.05, 0., 0.)
    inputs = AxialInputs(100., 100/math.pi, 100., 0., 0.)
    result = axial_components(section, inputs)
    assert result.internal_friction_kn == 95.
    assert result.open_end_bearing_kn <= result.full_base_bearing_kn
    assert result.open_end_bearing_kn == result.full_base_bearing_kn
    assert result.legacy_open_end_bearing_kn == pytest.approx(95 + 100*math.pi*0.0975)
    assert result.legacy_open_end_bearing_kn > result.full_base_bearing_kn
    assert result.open_compression_kn == pytest.approx(200.)
    plugged = axial_components(section, replace(inputs, outer_friction_kn=200.))
    assert plugged.open_end_bearing_kn == plugged.full_base_bearing_kn
    assert plugged.legacy_open_end_bearing_kn == plugged.full_base_bearing_kn


@pytest.mark.parametrize('su,stress', [(20., 80.), (80., 20.), (10., 200.)])
def test_existing_alpha_method_independent_power_law(su, stress):
    ratio = su/stress
    alpha = min(1., 0.5/math.sqrt(ratio) if ratio <= 1 else 0.5/ratio**0.25)
    result = piles.skin_friction_clay(su, stress)
    assert result.alpha == pytest.approx(alpha)
    assert result.unit_friction_kpa == pytest.approx(alpha*su)


def test_existing_clay_lateral_and_end_bearing_limits():
    soil = SoftClay(20., 8., 0.02, 0.4)
    for depth in (0., 2., 50.):
        result = soft_clay_py(soil, depth_m=depth, diameter_m=2., displacements_m=(0., 1.))
        assert result.ultimate_kn_m == pytest.approx(min((3*20+8*depth)*2+0.4*20*depth, 9*20*2))
    assert piles.end_bearing_clay(20., nc=8.).unit_bearing_kpa == 160.
    assert piles.end_bearing_sand(100., nq=25.).unit_bearing_kpa == 2500.
    with pytest.raises(ValueError):
        piles.validate_pile_inputs(pile_diameter_m=-1.)
    with pytest.raises(ValueError):
        piles.axial_capacity(1., 5., [])


@pytest.mark.parametrize('bad', [float('nan'), float('inf'), -1., True])
def test_numeric_input_validation(bad):
    with pytest.raises(ValueError):
        SandPoint(bad, 2., 30., 1., 2., 8.)
    with pytest.raises(ValueError):
        ResistanceNode(1., bad)
    with pytest.raises(ValueError):
        cohesive_friction_from_curve(bad, FrictionCurve(((0., 0.), (10., 5.))))
    with pytest.raises(ValueError):
        capped_sand_friction(100., bad, 0.6, 50.)
    with pytest.raises(ValueError):
        AxialInputs(bad, 10., 5., 1., 2.)


@pytest.mark.parametrize('points', [((0., 0.),), ((10., 5.), (5., 3.)),
                                    ((0., 1.), (10., 5.)), ((0., 0.), (10., 20.)),
                                    ((0., 0.), (10., 8.), (20., 5.))])
def test_invalid_friction_curves(points):
    with pytest.raises(ValueError):
        FrictionCurve(points)


@pytest.mark.parametrize('section', [(0., .05, 0., 0.), (2., 1., 0., 0.),
                                    (2., .05, 4., 0.), (float('inf'), .05, 0., 0.)])
def test_invalid_sections(section):
    with pytest.raises(ValueError):
        PileSection(*section)


@pytest.mark.parametrize('top,load,pivot', [(0., 1., 0.), (0., 1., 1.), (0., 0., 1.5),
                                           (0., float('nan'), 2.), (3., 0., 2.)])
def test_unsupported_pivot_and_geometry(top, load, pivot):
    with pytest.raises(ValueError):
        rigid_pile_response((ResistanceNode(0., 10.), ResistanceNode(2., 10.)),
                            top_depth_m=top, load_depth_m=load, pivot_depth_m=pivot)


def test_invalid_grid_and_zero_reaction():
    for nodes in ((), (ResistanceNode(1., 10.), ResistanceNode(0., 10.)),
                  (ResistanceNode(0., 0.), ResistanceNode(1., 0.))):
        with pytest.raises(ValueError):
            rigid_pile_response(nodes, top_depth_m=0., load_depth_m=0., pivot_depth_m=1.)


def test_curve_requires_origin_to_preserve_friction_bound_below_first_knot():
    with pytest.raises(ValueError, match='origin'):
        FrictionCurve(((10., 5.), (20., 10.)))
    curve = FrictionCurve(((0., 0.), (10., 5.), (20., 10.)))
    assert cohesive_friction_from_curve(0., curve) == 0.
    assert cohesive_friction_from_curve(1., curve) == 0.5
