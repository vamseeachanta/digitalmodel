# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Illustrative example regressions against the existing rectangular solver.

All geometry and coefficient curves are synthetic. The legacy workbook names
DNV-RP-N103 (July 2017), Appendix B for drag, Appendix A for added mass,
4.6.3.3 for the three-dimensional correction and 4.6.4 for perforation.
The available references give no verified equation or table for the plate/prism
switch at unity, the splash multiplier and floor, the perforation breakpoints
and factors, or the rotational curve switch and rescaling. The numerical
examples below record unverified legacy behavior for those rules. Physical
applicability and agreement with the standard require separate evidence.
Uniform-body inertia follows OpenStax, University Physics Volume 1 (2016),
section 10.5, equation 10.19, integrated over a centred rectangular body.
"""
import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (
    DNVRPH103_hydrodynamics_rectangular,
)
from digitalmodel.units import Q_


def example_solver():
    """Illustrative example in SI: a rectangular body and synthetic curves."""
    ratios = [0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75]
    solver = DNVRPH103_hydrodynamics_rectangular()
    solver.cfg = {
        'rho_water': 1000.0,
        'inputs': {
            'l': 8.0, 'w': 4.0, 'h': 2.0,
            'm_air': 2400.0, 'm_water': 1600.0,
            'perforation_ratio': {'x': 0, 'y': 0, 'z': 0.2},
            'cog': {'x': 0, 'y': 0, 'z': 0},
        },
        'look_up': {
            'plate': {
                'drag': {'ratio': [0, 100], 'value': [0.4, 0.8]},
                'added_mass': {'ratio': [0, 100], 'value': [0.2, 0.6]},
            },
            'prism': {'drag': {'ratio': [0, 100], 'value': [0.6, 1.0]}},
            'rotational': {
                'ratio': ratios,
                'm55_upper': [0.4 + 0.2*r + 0.1*r*r for r in ratios],
                'm55_lower': [0.3 - 0.1*r + 0.04*r*r for r in ratios],
            },
        },
    }
    return solver


@pytest.mark.parametrize(('axis', 'a', 'b', 'c'), [
    ('x', 2, 4, 8), ('y', 2, 8, 4), ('z', 4, 8, 2),
])
def test_projected_dimensions_and_displaced_volume(axis, a, b, c):
    dimensions = getattr(example_solver(), f'get_{axis}_dimensions')()
    assert dimensions == {'a': a, 'b': b, 'c': c, 'volume': (2400-1600)/1000}


@pytest.mark.parametrize(('axis', 'area_m2', 'deep'), [
    ('x', 8, 0.611313708498985), ('y', 16, 0.604), ('z', 25.6, 0.408),
])
def test_translational_drag_with_independent_linear_curve(axis, area_m2, deep):
    # x: prism line 0.6 + 0.004*sqrt(8). At unity y uses 0.6 + 0.004*1.
    # z: plate line 0.4 + 0.004*2. Its open fraction leaves 32*0.8 = 25.6 m².
    area, cd = getattr(example_solver(), f'get_translational_{axis}_cd')()
    assert area == pytest.approx(area_m2, rel=1e-12)
    assert cd['deep'] == pytest.approx(deep, rel=1e-12)
    assert cd['splash'] == pytest.approx(2.5, rel=1e-12)


@pytest.mark.parametrize(('axis', 'expected_mass_kg'), [
    ('x', 4339.96602372825), ('y', 8402.07827483154), ('z', 21842.0318748240),
])
def test_translational_added_mass_and_normalization(axis, expected_mass_kg):
    # Before correction: (4π*208, 8π*216, 32π*208) kg from the fixture.
    # Multiply by 1 + sqrt((1-lambda²)/(2+2*lambda²)), using respectively
    # sqrt(8)/(8+sqrt(8)), 1/2 and sqrt(32)/(2+sqrt(32)) for lambda.
    # Only z takes the legacy factor 0.7 + 0.3*cos(15π/34).
    # Displaced water mass is 2400 - 1600 = 800 kg on all three axes.
    mass, ca = getattr(example_solver(), f'get_translational_{axis}_ca')()
    assert mass == pytest.approx(expected_mass_kg, rel=1e-12)
    assert ca == pytest.approx(expected_mass_kg / 800, rel=1e-12)


@pytest.mark.parametrize(('fraction', 'expected_kg'), [
    (0, 600), (0.05, 600), (0.051, 599.992316106606), (0.2, 453.074913206983),
    (0.339, 259.618825646094), (0.34, 254.623707406170), (0.5, 143.790621865065),
])
def test_perforation_piecewise_branches(fraction, expected_kg):
    # For 600 kg, the three cosine examples give 420 + 180*cos(theta)
    # at theta = π/340, 15π/34 and 0.85π. At 0.34 the result is
    # 600*exp(-6/7), and at 0.5 it is 600*exp(-10/7).
    mass = example_solver().get_mass_added_with_perforation(600, fraction)
    assert mass == pytest.approx(expected_kg, rel=1e-12)


@pytest.mark.parametrize(('deep', 'splash'), [
    (0.4, 2.5), (0.83, 2.5), (5/6, 2.5), (0.84, 2.52), (0.9, 2.7), (1.4, 4.2),
])
def test_splash_drag_clamp(deep, splash):
    # 3*0.83 = 2.49 reaches the legacy floor and 3*0.84 = 2.52 exceeds it.
    assert example_solver().get_splash_from_deep_drag(deep) == pytest.approx(splash, rel=1e-12)


def test_rotational_drag_moments_use_each_directional_coefficient():
    solver = example_solver()
    # Illustrative example: deliberately distinct axes expose accidental swaps.
    coeff = {'x': 0.8, 'y': 1.4, 'z': 1.8}
    expected = {
        'x': (1.4*8*2**4 + 1.8*8*4**4)/32,
        'y': (0.8*4*2**4 + 1.8*4*8**4)/32,
        'z': (0.8*2*4**4 + 1.4*2*8**4)/32,
    }
    actual = solver.get_area_moment_of_inertia_by_type({'cd': {'deep': coeff}}, 'deep')
    assert actual == pytest.approx(expected, rel=1e-12)
    assert solver.get_moment_coefficient() == {'x': 1, 'y': 1, 'z': 1}


def test_structural_and_displaced_fluid_inertia_have_distinct_units():
    solver = example_solver()
    inertia = solver.get_mass_moment_of_inertia()
    fluid = solver.get_hydrodynamic_intertia()
    for structural, axis, face_square_sum in [('Ix', 'x', 20), ('Iy', 'y', 68), ('Iz', 'z', 80)]:
        assert inertia[structural] == pytest.approx(2400*face_square_sum/12)
        # Existing fluid-inertia API returns tonne.m^2, structural API kg.m^2.
        assert Q_(fluid[axis], 'tonne*m**2').to('kg*m**2').magnitude == pytest.approx(
            (2400-1600)*face_square_sum/12)


@pytest.mark.parametrize(('a', 'b', 'expected'), [
    (2, 1, 0.525), (2, 3.18, 0.97081), (2, 3.2, 0.58247191011236), (2, 4, 0.832),
])
def test_rotational_fit_recovers_synthetic_quadratic(a, b, expected):
    # At b/a = 0.5 the upper curve gives 0.4 + 0.1 + 0.025.
    # At 1.6 the lower curve gives 0.253125 at 5/8, scaled by 2*1.6³/3.56.
    # At 2 the lower curve gives 0.26 at 1/2, scaled by 16/5.
    assert example_solver().get_newman_ca(a, b, b/a, a/b) == pytest.approx(expected, rel=1e-10)


def test_existing_interpolator_rejects_ratios_outside_supplied_nodes():
    solver = example_solver()
    dimensions = {'a': 1, 'b': 200, 'c': 1, 'volume': 1}
    with pytest.raises(ValueError):
        solver.get_translational_ca(dimensions, 0)
