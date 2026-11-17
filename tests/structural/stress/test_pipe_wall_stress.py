"""Illustrative example inputs; expected values follow independent identities."""
from dataclasses import FrozenInstanceError, replace
import math

import numpy
import pytest

from digitalmodel.structural.stress.pipe_wall_stress import (
    PipeWallStressInput, WallStress, pipe_wall_stress_at_fraction,
    sample_pipe_wall_stress, mean_wall_stress,
)
from digitalmodel.structural.stress.vm_stress import (
    PipeGeometry, MaterialProperties, PipeStressAnalyzer, calculate_vm_stress,
)
from digitalmodel.drilling_riser.postprocess.pressure import burst_pressure_mpa


def example(**changes):
    """Illustrative example: SI geometry and independently selected stresses."""
    values = dict(outer_radius_m=0.25, inner_radius_m=0.20,
                  pressure_radius_m=0.18, net_pressure_pa=8e6,
                  axial_force_stress_pa=2e6, bending_outer_stress_pa=5e6,
                  torsion_outer_stress_pa=3e6, lateral_shear_stress_pa=1e6)
    return PipeWallStressInput(**(values | changes))


def test_pressure_boundary_conditions_and_independent_axial_bore():
    inp = example(axial_force_stress_pa=0, bending_outer_stress_pa=0,
                  torsion_outer_stress_pa=0, lateral_shear_stress_pa=0)
    inner = pipe_wall_stress_at_fraction(inp, 0)
    outer = pipe_wall_stress_at_fraction(inp, 1)
    assert inner.radial_pa == pytest.approx(-inp.net_pressure_pa)
    assert outer.radial_pa == pytest.approx(0, abs=1e-8)
    # Force equilibrium: pressure end force / annular metal area.
    expected_axial = 8e6 * math.pi * 0.18**2 / (math.pi * (0.25**2 - 0.20**2))
    assert inner.axial_pressure_pa == pytest.approx(expected_axial)
    assert outer.axial_pressure_pa == pytest.approx(expected_axial)
    assert inner.hoop_pa == pytest.approx(8e6 * (0.25**2 + 0.20**2) /
                                               (0.25**2 - 0.20**2))


def test_zero_pressure_bore_suppresses_end_load_only():
    point = pipe_wall_stress_at_fraction(example(pressure_radius_m=0), 0.5)
    assert point.axial_pressure_pa == 0
    assert point.hoop_pa > 0
    assert point.axial_force_pa == 2e6


@pytest.mark.parametrize('fraction', [0, 0.3, 1])
def test_combined_stress_against_expanded_hand_formula(fraction):
    inp = example()
    point = pipe_wall_stress_at_fraction(inp, fraction)
    r = 0.20 + 0.05 * fraction
    den = 0.25**2 - 0.20**2
    radial = 8e6 * 0.20**2 / den * (1 - 0.25**2 / r**2)
    hoop = 8e6 * 0.20**2 / den * (1 + 0.25**2 / r**2)
    pressure_axial = 8e6 * 0.18**2 / den
    bending = 5e6 * r / 0.25
    torsion = 3e6 * r / 0.25
    assert point.radius_m == pytest.approx(r)
    assert point.radial_pa == pytest.approx(radial)
    assert point.hoop_pa == pytest.approx(hoop)
    assert point.bending_pa == pytest.approx(bending)
    assert point.torsional_shear_pa == pytest.approx(torsion)
    for longitudinal, actual in [
        (pressure_axial + 2e6 + bending, point.von_mises_plus_pa),
        (pressure_axial + 2e6 - bending, point.von_mises_minus_pa),
    ]:
        expected = math.sqrt(radial**2 + hoop**2 + longitudinal**2 -
                             radial*hoop - hoop*longitudinal - longitudinal*radial +
                             3*(torsion**2 + 1e6**2))
        assert actual == pytest.approx(expected)
    assert point.normal_differences_plus_pa == pytest.approx(
        (abs(radial-hoop), abs(hoop-point.axial_plus_pa),
         abs(point.axial_plus_pa-radial)))


def test_mechanical_only_sign_and_zero_load_identities():
    inp = example(net_pressure_pa=0)
    plus = pipe_wall_stress_at_fraction(inp, 1)
    reversed_bending = pipe_wall_stress_at_fraction(
        replace(inp, bending_outer_stress_pa=-5e6), 1)
    assert plus.von_mises_plus_pa == pytest.approx(reversed_bending.von_mises_minus_pa)
    empty = example(net_pressure_pa=0, axial_force_stress_pa=0,
                    bending_outer_stress_pa=0, torsion_outer_stress_pa=0,
                    lateral_shear_stress_pa=0)
    assert pipe_wall_stress_at_fraction(empty, 0.5).von_mises_plus_pa == 0


def test_external_pressure_reverses_pressure_components():
    positive = pipe_wall_stress_at_fraction(example(), 0.5)
    negative = pipe_wall_stress_at_fraction(example(net_pressure_pa=-8e6), 0.5)
    assert negative.radial_pa == pytest.approx(-positive.radial_pa)
    assert negative.hoop_pa == pytest.approx(-positive.hoop_pa)
    assert negative.axial_pressure_pa == pytest.approx(-positive.axial_pressure_pa)


def test_sampling_and_component_average_are_not_average_equivalent_stress():
    points = sample_pipe_wall_stress(example(), 4)
    assert len(points) == 5
    assert points[0].radius_m == 0.20
    assert points[-1].radius_m == 0.25
    mean = mean_wall_stress(points)
    assert mean.hoop_pa == pytest.approx(sum(p.hoop_pa for p in points)/5)
    assert mean.von_mises_plus_pa < sum(p.von_mises_plus_pa for p in points)/5
    # Average signed components before absolute value when a difference crosses zero.
    a = WallStress(0.2, 0, 6e6, 0, 0, 0, 0, 0)
    b = WallStress(0.3, 0, -6e6, 0, 0, 0, 0, 0)
    assert mean_wall_stress((a, b)).normal_differences_plus_pa == (0, 0, 0)


@pytest.mark.parametrize('field,value', [
    ('outer_radius_m', 0), ('inner_radius_m', 0),
    ('inner_radius_m', 0.25), ('inner_radius_m', 0.30),
    ('pressure_radius_m', -0.1), ('pressure_radius_m', 0.26),
])
def test_invalid_geometry(field, value):
    with pytest.raises(ValueError):
        example(**{field: value})


@pytest.mark.parametrize('field', list(PipeWallStressInput.__dataclass_fields__))
@pytest.mark.parametrize('value', [float('nan'), float('inf'), -float('inf'), True, '1'])
def test_nonfinite_or_nonnumeric_input(field, value):
    with pytest.raises(ValueError):
        example(**{field: value})


@pytest.mark.parametrize('value', [numpy.int64(8_000_000), numpy.float32(8e6)])
def test_numpy_real_pressure_input(value):
    inp = example(net_pressure_pa=value)
    assert inp.net_pressure_pa == 8e6


@pytest.mark.parametrize('value', [-0.01, 1.01, float('nan'), True, '0'])
def test_invalid_fraction(value):
    with pytest.raises(ValueError):
        pipe_wall_stress_at_fraction(example(), value)


@pytest.mark.parametrize('value', [0, -1, 1.5, True])
def test_invalid_sampling(value):
    with pytest.raises(ValueError):
        sample_pipe_wall_stress(example(), value)


def test_empty_average_and_immutable_input():
    with pytest.raises(ValueError):
        mean_wall_stress(())
    with pytest.raises(FrozenInstanceError):
        example().net_pressure_pa = 0


def test_reused_geometry_and_mechanical_stresses():
    # Illustrative example: round SI geometry, material, force and moment.
    geom = PipeGeometry(0.50, 0.05)
    material = MaterialProperties(300e6, 450e6, 200e9, 0.3)
    analyzer = PipeStressAnalyzer(geom, material)
    area = math.pi * (0.25**2 - 0.20**2)
    inertia = math.pi/4 * (0.25**4 - 0.20**4)
    assert geom.cross_sectional_area == pytest.approx(area)
    assert geom.moment_of_inertia == pytest.approx(inertia)
    assert analyzer.calculate_axial_stress(100e3) == pytest.approx(100e3/area)
    assert analyzer.calculate_bending_stress(50e3) == pytest.approx(50e3*0.25/inertia)
    assert analyzer.calculate_bending_stress(50e3, 'inner') == pytest.approx(50e3*0.20/inertia)
    with pytest.raises(ValueError):
        PipeGeometry(0.5, 0.3)
    with pytest.raises(ValueError):
        analyzer.calculate_bending_stress(50e3, 'middle')


@pytest.mark.parametrize('value', [float('nan'), float('inf')])
def test_reused_equivalent_stress_validation(value):
    with pytest.raises(ValueError):
        calculate_vm_stress(value, 0, 0)


def test_reused_logarithmic_burst_pressure_independent_identity():
    # Illustrative example chosen so the diameter ratio is exactly two.
    actual = burst_pressure_mpa(0.40, 0.10, 300, 500)
    assert actual == pytest.approx(0.45 * (300+500) * math.log(2))
    with pytest.raises(ValueError):
        burst_pressure_mpa(0.40, 0.20, 300, 500)


@pytest.mark.parametrize('field', ['torsion_outer_stress_pa', 'lateral_shear_stress_pa'])
def test_pure_shear_identity(field):
    inp = example(net_pressure_pa=0, axial_force_stress_pa=0,
                  bending_outer_stress_pa=0, torsion_outer_stress_pa=0,
                  lateral_shear_stress_pa=0, **{})
    inp = replace(inp, **{field: -4e6})
    point = pipe_wall_stress_at_fraction(inp, 1)
    assert point.von_mises_plus_pa == pytest.approx(math.sqrt(3)*4e6)
    assert point.normal_differences_plus_pa == (0, 0, 0)


def test_aligned_shears_are_not_summed():
    inp = example(net_pressure_pa=0, axial_force_stress_pa=0,
                  bending_outer_stress_pa=0, torsion_outer_stress_pa=4e6,
                  lateral_shear_stress_pa=4e6)
    point = pipe_wall_stress_at_fraction(inp, 1)
    assert point.von_mises_plus_pa == pytest.approx(math.sqrt(3*(4e6**2 + 4e6**2)))
    assert point.von_mises_plus_pa != pytest.approx(math.sqrt(3)*8e6)


def test_pressure_boundary_with_adjacent_float_radii():
    # Illustrative numerical limit: wall thickness is one floating-point step.
    inp = example(inner_radius_m=math.nextafter(0.25, 0.0))
    inner = pipe_wall_stress_at_fraction(inp, 0)
    outer = pipe_wall_stress_at_fraction(inp, 1)
    assert inner.radial_pa == pytest.approx(-inp.net_pressure_pa, rel=1e-14)
    assert outer.radial_pa == 0
