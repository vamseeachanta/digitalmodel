"""Illustrative example inputs and invented coefficient tables, not design data.

These checks exercise existing methods against hand formulas and identities.
The illustrative tables deliberately differ from any engineering reference.
No external solver, exporter, macro or source fixture is executed.
"""
from copy import deepcopy
from math import cos, exp, pi, sqrt

import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (
    DNVRPH103_hydrodynamics_rectangular,
)


@pytest.fixture
def solver():
    """Illustrative example: SI geometry/masses and invented linear tables."""
    ratios = [0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75]
    instance = DNVRPH103_hydrodynamics_rectangular()
    instance.cfg = {
        'rho_water': 1000.0,
        'inputs': {
            'l': 8.0, 'w': 3.0, 'h': 0.5,
            'm_air': 1800.0, 'm_water': 900.0,
            'cog': {'x': 0, 'y': 0, 'z': 0},
            'perforation_ratio': {'x': 0, 'y': 0, 'z': 0.2},
        },
        'look_up': {
            'prism': {'drag': {
                'ratio': [1, 3, 9, 27], 'value': [0.6, 0.8, 1.0, 1.2],
            }},
            'plate': {
                'drag': {'ratio': [1, 3, 9, 27], 'value': [0.5, 1.1, 1.7, 2.3]},
                'added_mass': {
                    'ratio': [1, 3, 9, 27], 'value': [0.4, 0.8, 1.2, 1.6],
                },
            },
            'rotational': {
                'ratio': ratios,
                'm55_upper': [2 + r for r in ratios],
                'm55_lower': [4 - r for r in ratios],
            },
        },
    }
    return instance


@pytest.mark.parametrize('axis,dimensions', [
    ('x', (0.5, 3, 8)), ('y', (0.5, 8, 3)), ('z', (3, 8, 0.5)),
])
def test_directional_dimensions_and_displacement(solver, axis, dimensions):
    a, b, c = dimensions
    actual = getattr(solver, f'get_{axis}_dimensions')()
    assert actual == pytest.approx({'a': a, 'b': b, 'c': c, 'volume': 0.9})


def test_drag_areas_interpolation_and_splash_floor(solver):
    actual = solver.get_translational_properties()
    expected_deep = {
        'x': 0.8 + 0.2 * (8 / sqrt(1.5) - 3) / 6,
        'y': 0.6 + 0.2 * (1.5 - 1) / 2,
        'z': 0.5 + 0.6 * (8 / 3 - 1) / 2,
    }
    assert actual['area_drag'] == pytest.approx({'x': 1.5, 'y': 4, 'z': 19.2})
    assert actual['cd']['deep'] == pytest.approx(expected_deep, rel=1e-12)
    assert actual['cd']['splash'] == pytest.approx({
        'x': 3 * expected_deep['x'], 'y': 2.5, 'z': 3 * expected_deep['z'],
    })


@pytest.mark.parametrize('axis,a,b,c,coefficient', [
    ('x', 0.5, 3, 8, 0.8 + 0.4 * (6 - 3) / 6),
    ('y', 0.5, 8, 3, 1.2 + 0.4 * (16 - 9) / 18),
    ('z', 3, 8, 0.5, 0.4 + 0.4 * (8 / 3 - 1) / 2),
])
def test_translational_added_mass_and_displaced_mass_normalization(
    solver, axis, a, b, c, coefficient,
):
    # Closed source method, independently evaluated from the illustrative data.
    lam = sqrt(a * b) / (c + sqrt(a * b))
    unperforated = 1000 * coefficient * pi * a**2 * b / 4 * (
        1 + sqrt((1 - lam**2) / (2 * (1 + lam**2)))
    )
    reduction = 0.7 + 0.3 * cos(pi * (0.2 - 0.05) / 0.34) if axis == 'z' else 1
    expected = unperforated * reduction
    mass, normalized = getattr(solver, f'get_translational_{axis}_ca')()
    assert mass == pytest.approx(expected, rel=1e-12)
    assert normalized == pytest.approx(expected / (1800 - 900), rel=1e-12)


@pytest.mark.parametrize('fraction,ratio', [
    (0.0, 1.0), (0.05, 1.0),
    (0.2, 0.7 + 0.3 * cos(pi * (0.2 - 0.05) / 0.34)),
    (0.34, exp((0.1 - 0.34) / 0.28)),
    (0.5, exp((0.1 - 0.5) / 0.28)),
])
def test_perforation_branches_and_linearity_in_mass(solver, fraction, ratio):
    assert solver.get_mass_added_with_perforation(200, fraction) == pytest.approx(200 * ratio)
    assert solver.get_mass_added_with_perforation(0, fraction) == 0


@pytest.mark.parametrize('zone,cd', [
    ('deep', {'x': 0.4, 'y': 0.8, 'z': 1.2}),
    ('splash', {'x': 1.2, 'y': 2.4, 'z': 3.6}),
])
def test_rotational_drag_area_moments(solver, zone, cd):
    actual = solver.get_area_moment_of_inertia_by_type({'cd': {zone: cd}}, zone)
    assert actual == pytest.approx({
        'x': (cd['y'] * 8 * 0.5**4 + cd['z'] * 8 * 3**4) / 32,
        'y': (cd['x'] * 3 * 0.5**4 + cd['z'] * 3 * 8**4) / 32,
        'z': (cd['x'] * 0.5 * 3**4 + cd['y'] * 0.5 * 8**4) / 32,
    })


def test_intrinsic_and_displaced_water_inertia(solver):
    # Box integral I/m=(edge_1^2+edge_2^2)/12; output units differ.
    assert solver.get_mass_moment_of_inertia() == pytest.approx({
        'Ix': 1800 * (3**2 + 0.5**2) / 12,
        'Iy': 1800 * (8**2 + 0.5**2) / 12,
        'Iz': 1800 * (8**2 + 3**2) / 12,
    })
    assert solver.get_hydrodynamic_intertia() == pytest.approx({
        'x': 0.9 * (3**2 + 0.5**2) / 12,
        'y': 0.9 * (8**2 + 0.5**2) / 12,
        'z': 0.9 * (8**2 + 3**2) / 12,
    })


@pytest.mark.parametrize('b_over_a', [0.5, 1.6, 2.0])
def test_rotational_fitting_recovers_invented_linear_curves(solver, b_over_a):
    a, b = 2.0, 2.0 * b_over_a
    # Interpolated data are exact straight lines; no fitting implementation
    # is used to calculate the expected value.
    expected = 2 + b_over_a if b_over_a < 1.6 else (
        (4 - 1 / b_over_a) * 2 * b**3 / (a * (a**2 + b**2))
    )
    assert solver.get_newman_ca(a, b, b / a, a / b) == pytest.approx(expected, rel=1e-11)


def test_complete_property_flow_is_in_memory_and_preserves_configuration(solver):
    cfg = deepcopy(solver.cfg)
    before = deepcopy(cfg)
    properties = solver.get_properties(cfg)
    assert cfg == before
    assert set(properties) == {'translational', 'rotational', 'I', 'shape_coordinates'}
    assert properties['rotational']['cd'] == {'x': 1.0, 'y': 1.0, 'z': 1.0}
    # Analytic linear curves from the fixture, with equivalent ellipsoid axes.
    for axis, a, b in [('x', 4, sqrt(1.5 / pi)),
                       ('y', 1.5, sqrt(4 / pi)),
                       ('z', 0.25, sqrt(24 / pi))]:
        ratio = b / a
        expected = 2 + ratio if ratio < 1.6 else (
            (4 - 1 / ratio) * 2 * b**3 / (a * (a**2 + b**2))
        )
        assert properties['rotational']['ca'][axis] == pytest.approx(expected, rel=1e-11)
    vertices = properties['shape_coordinates']
    assert len(vertices) == 8
    assert {tuple(v) for v in vertices} == {
        (x, y, z) for x in (-4, 4) for y in (-1.5, 1.5) for z in (-0.25, 0.25)
    }


@pytest.mark.parametrize('method', ['get_translational_ca', 'get_translational_cd'])
def test_outside_supplied_table_domain_raises(solver, method):
    dimensions = {'a': 1, 'b': 100, 'c': 0.5, 'volume': 1}
    with pytest.raises(ValueError):
        getattr(solver, method)(dimensions, 0)


@pytest.mark.parametrize('moments', [
    {'Ix': 2, 'Iy': 1, 'Iz': 0},
    {'Ix': float('nan'), 'Iy': 2, 'Iz': 3},
    {'Ix': 9, 'Iy': 2, 'Iz': 3},
])
def test_existing_supplied_inertia_validation(solver, moments):
    solver.cfg['inputs']['moments_of_inertia_at_cog'] = moments
    with pytest.raises(ValueError, match='moments_of_inertia_at_cog'):
        solver.get_mass_moment_of_inertia()


def test_dimensional_scaling_of_full_body_properties(solver):
    base = solver.get_properties(deepcopy(solver.cfg))
    scaled_cfg = deepcopy(solver.cfg)
    for key in ('l', 'w', 'h'):
        scaled_cfg['inputs'][key] *= 2
    for key in ('m_air', 'm_water'):
        scaled_cfg['inputs'][key] *= 8
    scaled = solver.get_properties(scaled_cfg)
    for axis in 'xyz':
        assert scaled['translational']['area_drag'][axis] == pytest.approx(
            4 * base['translational']['area_drag'][axis])
        assert scaled['translational']['added_mass'][axis] == pytest.approx(
            8 * base['translational']['added_mass'][axis])
        assert scaled['translational']['ca'][axis] == pytest.approx(base['translational']['ca'][axis])
        assert scaled['rotational']['I'][axis] == pytest.approx(32 * base['rotational']['I'][axis])
        assert scaled['rotational']['am']['deep'][axis] == pytest.approx(
            32 * base['rotational']['am']['deep'][axis])
