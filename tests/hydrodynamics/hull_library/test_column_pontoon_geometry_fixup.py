"""Geometry regressions: SI hand formulas and a 0.5 percent criterion."""
from collections import Counter
from math import pi

import numpy as np
import pytest
from pydantic import ValidationError

from digitalmodel.hydrodynamics.hull_library import column_pontoon_form as form


def test_hard_tank_is_at_waterline():
    _, report = form.generate_column_pontoon(form.ColumnPontoonParameters(
        diameter=6, draft=20, hard_tank_diameter=12, hard_tank_height=4,
        panel_target_size=3,
    ))
    volume = pi * (6**2 * 4 + 3**2 * 16)
    kb = (pi * 6**2 * 4 * 18 + pi * 3**2 * 16 * 8) / volume
    assert report.waterplane_area == pytest.approx(pi * 6**2, rel=0.005)
    assert report.displacement == pytest.approx(volume, rel=0.005)
    assert report.kb == pytest.approx(kb, rel=0.005)
    assert report.bm == pytest.approx((pi * 6**4 / 4 / volume,) * 2, rel=0.005)


def test_hard_tank_and_separate_keel_plate():
    params = form.ColumnPontoonParameters(
        diameter=6, draft=20, hard_tank_diameter=12, hard_tank_depth=4,
        heave_plate_diameter=14, heave_plate_thickness=2, panel_target_size=3,
    )
    _, report = form.generate_column_pontoon(params)
    volume = pi * (6**2 * 4 + 3**2 * 14 + 7**2 * 2)
    assert report.waterplane_area == pytest.approx(pi * 6**2, rel=0.005)
    assert report.displacement == pytest.approx(volume, rel=0.005)
    assert report.bm == pytest.approx((pi * 6**4 / 4 / volume,) * 2, rel=0.005)
    assert {s['name'] for s in report.primitive_checks} == {
        'column_0', 'hard_tank_0', 'heave_plate_0'}


@pytest.mark.parametrize('parameters', [
    dict(count=3, spacing=30, diameter=6, draft=20, pontoon_layout='ring',
         pontoon_width=8, pontoon_height=3),
    dict(count=4, spacing=60, square_side=12, draft=20, pontoon_layout='twin',
         pontoon_width=16, pontoon_height=8),
])
def test_pontoon_wider_than_endpoint_is_rejected(parameters):
    with pytest.raises(ValidationError, match='pontoon width'):
        form.ColumnPontoonParameters(**parameters)


@pytest.mark.parametrize('changes', [
    {'diameter': 0}, {'diameter': -1}, {'draft': 0}, {'draft': -1},
    {'panel_target_size': 0}, {'panel_target_size': -1},
    {'pontoon_width': 0}, {'pontoon_width': -1},
    {'pontoon_height': 0}, {'pontoon_height': -1},
    {'spacing': 0}, {'spacing': -1}, {'spacing': 6},
    {'draft': 2},
])
def test_invalid_sizes_overlap_and_shallow_draft(changes):
    with pytest.raises(ValidationError):
        form.ColumnPontoonParameters(**(dict(
            count=3, spacing=30, diameter=6, draft=20, pontoon_layout='ring',
            pontoon_width=2, pontoon_height=3,
        ) | changes))


def test_near_circle_rounded_column_retains_half_percent_acceptance():
    params = form.ColumnPontoonParameters(
        square_side=4, corner_radius=1.9, draft=4, panel_target_size=4)
    _, report = form.generate_column_pontoon(params)
    area = 16-(4-pi)*1.9**2
    assert report.waterplane_area == pytest.approx(area, rel=.005)
    assert report.displacement == pytest.approx(4*area, rel=.005)
    assert all(check['passed'] for check in report.primitive_checks)


def test_near_circle_unequal_pontoon_retains_half_percent_acceptance():
    params = form.ColumnPontoonParameters(
        count=3, spacing=10, square_side=4, draft=5, panel_target_size=4,
        pontoon_layout='ring', pontoon_width=2, pontoon_height=2.2,
        pontoon_corner_radius=1)
    solid = next(s for s in form._primitives(params)[0] if s['name'].startswith('pontoon'))
    check = form._primitive_check(solid, params.draft)
    area = 2*2.2-(4-pi)
    assert check['volume']['mesh'] == pytest.approx(10*area, rel=.005)
    assert check['passed']


@pytest.mark.parametrize('width,height,radius', [(2, 3, 1), (1.6, 8, 0)])
def test_unequal_rounded_primitive_cap_has_convex_closed_quads(width, height, radius):
    params = form.ColumnPontoonParameters(
        count=3, spacing=10, square_side=4, draft=10, panel_target_size=1,
        pontoon_layout='ring', pontoon_width=width, pontoon_height=height,
        pontoon_corner_radius=radius)
    solid = next(s for s in form._primitives(params)[0] if s['name'].startswith('pontoon'))
    mesh = form._quad_mesh(solid['faces'], True)
    assert form._is_watertight(mesh)
    points = mesh.vertices[mesh.panels]
    edges = np.roll(points, -1, axis=1)-points
    turns = np.cross(edges, np.roll(edges, -1, axis=1))
    assert np.all(np.sum(turns*mesh.normals[:, None, :], axis=2) > 0)
    assert form._primitive_check(solid, params.draft)['passed']


def test_unequal_rounded_cap_knots_are_present_in_side_rings():
    params = form.ColumnPontoonParameters(
        count=3, spacing=10, square_side=4, draft=5, panel_target_size=1,
        pontoon_layout='ring', pontoon_width=2, pontoon_height=3,
        pontoon_corner_radius=1)
    solid = next(s for s in form._primitives(params)[0] if s['name'].startswith('pontoon'))
    edges = Counter()
    for face in solid['faces']:
        for a, b in zip(face, np.roll(face, -1, axis=0)):
            key = tuple(sorted((tuple(np.round(a, 8)), tuple(np.round(b, 8)))))
            edges[key] += 1
    assert set(edges.values()) == {2}
