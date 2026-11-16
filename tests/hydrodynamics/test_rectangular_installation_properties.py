"""Illustrative example regressions against the existing rectangular solver.

All geometry and coefficient curves are synthetic. Linear lookup curves and
quadratic rotational curves permit independent analytic expectations without
publishing any engineering coefficient table. The method in a legacy
engineering workbook names DNV-RP-N103 (July 2017), Appendix B (drag), Appendix A
(added mass), 4.6.3.3 (three-dimensional correction), and section 4.6.4
(perforation). These tests verify arithmetic, with physical applicability
unestablished. Structural inertia retains the existing centre-of-mass basis;
rotational coefficients retain the existing polynomial fitting behavior.
"""
import math

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


@pytest.mark.parametrize(('axis', 'a', 'b', 'c', 'fraction'), [
    ('x', 2, 4, 8, 0), ('y', 2, 8, 4, 0), ('z', 4, 8, 2, 0.2),
])
def test_translational_drag_with_independent_linear_curve(axis, a, b, c, fraction):
    area, cd = getattr(example_solver(), f'get_translational_{axis}_cd')()
    ratio = c / math.sqrt(a*b)
    # Independent lines through the illustrative lookup endpoints.
    expected_deep = 0.4 + 0.004*b/a if ratio < 1 else 0.6 + 0.004*ratio
    assert area == pytest.approx(a*b*(1-fraction), rel=1e-12)
    assert cd['deep'] == pytest.approx(expected_deep, rel=1e-12)
    assert cd['splash'] == pytest.approx(max(3*expected_deep, 2.5), rel=1e-12)


@pytest.mark.parametrize(('axis', 'a', 'b', 'c', 'fraction'), [
    ('x', 2, 4, 8, 0), ('y', 2, 8, 4, 0), ('z', 4, 8, 2, 0.2),
])
def test_translational_added_mass_and_normalization(axis, a, b, c, fraction):
    mass, ca = getattr(example_solver(), f'get_translational_{axis}_ca')()
    coefficient = 0.2 + 0.004*b/a
    lam = math.sqrt(a*b)/(c+math.sqrt(a*b))
    solid = 1000*coefficient*math.pi*a*a*b/4
    corrected = solid*(1+math.sqrt((1-lam*lam)/(2*(1+lam*lam))))
    factor = 1 if fraction == 0 else 0.7+0.3*math.cos(math.pi*(fraction-0.05)/0.34)
    assert mass == pytest.approx(corrected*factor, rel=1e-12)
    assert ca == pytest.approx(mass/(2400-1600), rel=1e-12)


@pytest.mark.parametrize('fraction', [0, 0.05, 0.2, 0.339, 0.34, 0.5])
def test_perforation_piecewise_branches(fraction):
    mass_kg = 600.0  # Illustrative example; thresholds belong to the existing method.
    if fraction <= 0.05:
        expected = mass_kg
    elif fraction < 0.34:
        expected = mass_kg*(0.7+0.3*math.cos(math.pi*(fraction-0.05)/0.34))
    else:
        expected = mass_kg*math.exp((0.1-fraction)/0.28)
    assert example_solver().get_mass_added_with_perforation(mass_kg, fraction) == pytest.approx(expected)


@pytest.mark.parametrize('deep', [0.4, 0.9, 1.4])
def test_splash_drag_clamp(deep):
    assert example_solver().get_splash_from_deep_drag(deep) == max(3*deep, 2.5)


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


@pytest.mark.parametrize(('a', 'b'), [(2, 1), (2, 3.2), (2, 4)])
def test_rotational_fit_recovers_synthetic_quadratic(a, b):
    ratio = b/a
    if ratio < 1.6:
        expected = 0.4+0.2*ratio+0.1*ratio*ratio
    else:
        inverse = a/b
        expected = (0.3-0.1*inverse+0.04*inverse*inverse)*2*b**3/(a*(a*a+b*b))
    assert example_solver().get_newman_ca(a, b, ratio, a/b) == pytest.approx(expected, rel=1e-10)


@pytest.mark.parametrize('supplied', [
    {}, {'Ix': -2, 'Iy': 3, 'Iz': 4}, {'Ix': True, 'Iy': 3, 'Iz': 4},
    {'Ix': float('nan'), 'Iy': 3, 'Iz': 4}, {'Ix': 20, 'Iy': 3, 'Iz': 4},
])
def test_existing_supplied_inertia_validation(supplied):
    solver = example_solver()
    solver.cfg['inputs']['moments_of_inertia_at_cog'] = supplied
    with pytest.raises(ValueError, match='moments_of_inertia_at_cog'):
        solver.get_mass_moment_of_inertia()


def test_existing_interpolator_rejects_ratios_outside_supplied_nodes():
    solver = example_solver()
    dimensions = {'a': 1, 'b': 200, 'c': 1, 'volume': 1}
    with pytest.raises(ValueError):
        solver.get_translational_ca(dimensions, 0)
