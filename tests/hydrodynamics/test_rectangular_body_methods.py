"""Illustrative examples exercising existing rectangular-body methods.

No parallel hydrodynamic implementation is introduced. Lookup ordinates below
are synthetic linear/quadratic laws used as analytic test oracles; they are
not engineering coefficient tables or qualified design inputs.

A legacy engineering workbook states DNV-RP-N103 (July 2017), appendix B
(rectangular drag), items 4.6.2.2 and 4.6.2.4 (oscillatory-flow drag), appendix A
(plate added mass), 4.6.3.3 (three-dimensional correction) and section 4.6.4
(perforation). Its rectangular-body software manual reference has clause not
stated in source. The existing implementation retains its H103 naming.

Method and units: lengths are m, density kg/m3, mass kg, areas m2, drag area
moments m5. Added mass is rho*C*pi*a^2*b/4 times the three-dimensional factor
1+sqrt((1-lambda^2)/(2*(1+lambda^2))), lambda=sqrt(ab)/(c+sqrt(ab)). Ca is added
mass divided by displaced mass. The rotational hydrodynamic inertia method
returns t.m2, while structural inertia returns kg.m2. Tests keep that boundary
explicit. Inputs have positive finite dimensions and finite lookup coverage.

Input gaps and limits: horizontal reuse of the three-dimensional correction
and the source's rotational fit provenance are not independently qualified.
Rounded source fit polynomials and source offset-inertia conventions need
separate reconciliation; existing methods are not changed to match them.
Model-file generation, macros, deployment rotation and environmental loads are
outside these regressions. No design suitability follows from arithmetic tests.
"""

from copy import deepcopy
import math

import pytest

from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (
    DNVRPH103_hydrodynamics_rectangular,
)


def example_solver():
    """Illustrative example: geometry plus explicitly synthetic lookup laws."""
    ratios = [1.0, 16.0]
    rotational_ratios = [i / 4 for i in range(9)]
    solver = DNVRPH103_hydrodynamics_rectangular()
    solver.cfg = {
        'rho_water': 1000.0,
        'inputs': {
            'l': 8.0, 'w': 4.0, 'h': 2.0,
            'm_air': 24000.0, 'm_water': 16000.0,
            'cog': {'x': 0.0, 'y': 0.0, 'z': 0.0},
            'perforation_ratio': {'x': 0.0, 'y': 0.0, 'z': 0.2},
        },
        'look_up': {
            'prism': {'drag': {
                'ratio': ratios, 'value': [0.4 + 0.1 * r for r in ratios]}},
            'plate': {
                'drag': {'ratio': ratios, 'value': [0.8 + 0.2 * r for r in ratios]},
                'added_mass': {'ratio': ratios, 'value': [0.3 + 0.05 * r for r in ratios]},
            },
            'rotational': {
                'ratio': rotational_ratios,
                'm55_upper': [1 + 0.5 * r**2 for r in rotational_ratios],
                'm55_lower': [2 - 0.25 * r for r in rotational_ratios],
            },
        },
    }
    return solver


@pytest.mark.parametrize('axis,a,b,c', [('x', 2, 4, 8), ('y', 2, 8, 4), ('z', 4, 8, 2)])
def test_dimensions_sort_projected_face_and_retain_displacement(axis, a, b, c):
    solver = example_solver()
    assert getattr(solver, f'get_{axis}_dimensions')() == {
        'a': a, 'b': b, 'c': c, 'volume': 8.0}


@pytest.mark.parametrize('axis,expected_area,deep', [
    ('x', 8.0, 0.4 + 0.1 * (8 / math.sqrt(8))),
    ('y', 16.0, 0.4 + 0.1),
    ('z', 32.0 * (1 - 0.2), 0.8 + 0.2 * 2),
])
def test_drag_uses_synthetic_linear_laws_and_explicit_flow_branches(axis, expected_area, deep):
    solver = example_solver()
    area, coefficients = getattr(solver, f'get_translational_{axis}_cd')()
    assert area == pytest.approx(expected_area, rel=1e-12)
    assert coefficients == pytest.approx({'deep': deep, 'splash': max(3 * deep, 2.5)})


@pytest.mark.parametrize('deep,expected', [(0.2, 2.5), (1.0, 3.0), (2.0, 6.0)])
def test_splash_correction_floor_and_multiplier(deep, expected):
    assert example_solver().get_splash_from_deep_drag(deep) == expected


@pytest.mark.parametrize('fraction', [0.0, 0.04, 0.2, 0.4, 0.5])
def test_perforation_branches_with_independent_example_mass(fraction):
    # Illustrative 200 kg uncorrected added mass; formula evaluated by branch.
    if fraction <= 0.05:
        expected = 200.0
    elif fraction < 0.34:
        expected = 200 * (0.7 + 0.3 * math.cos(math.pi * (fraction - 0.05) / 0.34))
    else:
        expected = 200 * math.exp((0.1 - fraction) / 0.28)
    assert example_solver().get_mass_added_with_perforation(200, fraction) == pytest.approx(expected)


@pytest.mark.parametrize('axis,a,b,c,fraction', [
    ('x', 2, 4, 8, 0.0), ('y', 2, 8, 4, 0.0), ('z', 4, 8, 2, 0.2),
])
def test_added_mass_and_displaced_mass_normalization(axis, a, b, c, fraction):
    solver = example_solver()
    coefficient = 0.3 + 0.05 * (b / a)
    reference_volume = math.pi * a**2 * b / 4
    lam = math.sqrt(a * b) / (c + math.sqrt(a * b))
    correction = 1 + math.sqrt((1 - lam**2) / (2 * (1 + lam**2)))
    reduction = 1 if fraction == 0 else 0.7 + 0.3 * math.cos(math.pi * (fraction - 0.05) / 0.34)
    expected_kg = 1000 * coefficient * reference_volume * correction * reduction
    mass_kg, ca = getattr(solver, f'get_translational_{axis}_ca')()
    assert mass_kg == pytest.approx(expected_kg, rel=1e-12)
    assert ca == pytest.approx(expected_kg / (24000 - 16000), rel=1e-12)


@pytest.mark.parametrize('zone', ['splash', 'deep'])
def test_rotational_drag_area_moments(zone):
    solver = example_solver()
    # Illustrative coefficients distinguish every term and axis.
    properties = {'cd': {zone: {'x': 2.0, 'y': 4.0, 'z': 6.0}}}
    expected = {
        'x': (4 * 8 * 2**4 + 6 * 8 * 4**4) / 32,
        'y': (2 * 4 * 2**4 + 6 * 4 * 8**4) / 32,
        'z': (2 * 2 * 4**4 + 4 * 2 * 8**4) / 32,
    }
    assert solver.get_area_moment_of_inertia_by_type(properties, zone) == expected
    assert solver.get_moment_coefficient() == {'x': 1, 'y': 1, 'z': 1}


def test_hydrodynamic_and_structural_inertia_keep_their_distinct_units():
    solver = example_solver()
    expected_kg_m2 = {
        'Ix': 24000 * (4**2 + 2**2) / 12,
        'Iy': 24000 * (8**2 + 2**2) / 12,
        'Iz': 24000 * (8**2 + 4**2) / 12,
    }
    assert solver.get_mass_moment_of_inertia() == pytest.approx(expected_kg_m2)
    expected_t_m2 = dict(zip('xyz', [8 * 20 / 12, 8 * 68 / 12, 8 * 80 / 12]))
    assert solver.get_hydrodynamic_intertia() == pytest.approx(expected_t_m2)


def test_rotational_fit_recovers_analytic_polynomials_in_both_branches():
    solver = example_solver()
    expected = {}
    for axis, a, b in [('x', 4, math.sqrt(8 / math.pi)),
                       ('y', 2, math.sqrt(16 / math.pi)),
                       ('z', 1, math.sqrt(32 / math.pi))]:
        ratio = b / a
        expected[axis] = (1 + 0.5 * ratio**2 if ratio < 1.6 else
                          (2 - 0.25 * (a / b)) * 2 * b**3 / (a * (a**2 + b**2)))
    assert solver.get_added_inertia_coefficient() == pytest.approx(expected, rel=1e-10)


def test_property_calls_preserve_input_configuration():
    solver = example_solver()
    original = deepcopy(solver.cfg)
    properties = solver.get_properties(solver.cfg)
    assert solver.cfg == original
    assert properties['translational']['area_drag']['z'] == pytest.approx(25.6)


@pytest.mark.parametrize('supplied', [
    {'Ix': -1, 'Iy': 2, 'Iz': 2},
    {'Ix': math.inf, 'Iy': 2, 'Iz': 2},
    {'Ix': 1, 'Iy': 1, 'Iz': 4},
    {'Ix': True, 'Iy': 2, 'Iz': 2},
])
def test_existing_supplied_inertia_validation(supplied):
    solver = example_solver()
    solver.cfg['inputs']['moments_of_inertia_at_cog'] = supplied
    with pytest.raises(ValueError):
        solver.get_mass_moment_of_inertia()


def test_existing_interpolation_rejects_outside_supplied_domain():
    solver = example_solver()
    solver.cfg['inputs']['l'] = 200
    with pytest.raises(ValueError):
        solver.get_translational_x_cd()
