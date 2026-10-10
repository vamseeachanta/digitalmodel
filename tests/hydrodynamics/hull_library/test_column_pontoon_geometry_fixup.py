"""Geometry regressions: SI hand formulas and a 0.5 percent criterion."""
from math import pi

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
