# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Illustrative example: explicit asymmetric drag masks, independent geometry."""
from dataclasses import FrozenInstanceError, replace
import math
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.infrastructure.base_solvers.hydrodynamics.rectangular_drag_elements import (
    RectangularDragGrid,
    rectangular_drag_elements,
)
from scripts.ci.detect_touched_domains import parse_domains, path_matches_root
from tests.scripts._pytest_selection import pytest_selects_target


def example_grid():
    """Illustrative example: 8 m by 3 m, three columns and two rows."""
    return RectangularDragGrid(
        length_m=8.0, width_m=3.0, count_x=3, count_y=2,
        drag_area_x_m2=12.0, drag_area_y_m2=18.0,
        gross_drag_area_z_m2=24.0, perforation_fraction=0.25,
        mask_x=(1, 0, 0, 0, 0, 1), mask_y=(0, 1, 0, 0, 0, 0),
    )


@pytest.mark.parametrize('filename', [
    'test_rectangular_drag_elements.py', 'test_rectangular_installation_properties.py',
])
def test_rectangular_suite_runs_in_hydrodynamics_shard(pytestconfig, filename):
    root = pytestconfig.rootpath
    suite = Path(__file__).with_name(filename)
    assert suite.is_file()
    path = suite.relative_to(root).as_posix()
    domains = parse_domains(root / 'tests/DOMAINS.md')
    assert [d.name for d in domains if any(path_matches_root(path, r) for r in d.roots)] == [
        'hydrodynamics-other'
    ]
    config = yaml.safe_load((root / '.claude/quality-gates.yaml').read_text(encoding='utf-8'))
    gate = config['gates']['tests-hydrodynamics-other']
    assert gate['enabled'] and gate['failure_action'] == 'block'
    assert any(path_matches_root(path, r) for r in gate['test_roots'])
    assert pytest_selects_target(pytestconfig, gate['command'], path, [path])


def test_integral_scalar_counts_are_normalized_before_multiplication():
    grid = replace(example_grid(), count_x=np.int64(3), count_y=np.uint64(2))
    assert type(grid.count_x) is type(grid.count_y) is int
    assert rectangular_drag_elements(grid) == rectangular_drag_elements(example_grid())


def test_asymmetric_grid_centres_and_independent_areas():
    elements = rectangular_drag_elements(example_grid())
    assert len(elements) == 6
    assert [e.x_m for e in elements] == pytest.approx([-8/3, 0, 8/3] * 2)
    assert [e.y_m for e in elements] == pytest.approx([-3/4] * 3 + [3/4] * 3)
    assert all(e.z_m == 0 for e in elements)
    assert [e.drag_area_x_m2 for e in elements] == [6, 0, 0, 0, 0, 6]
    assert [e.drag_area_y_m2 for e in elements] == [0, 6, 0, 0, 0, 0]
    assert [e.drag_area_z_m2 for e in elements] == [3] * 6
    assert sum(e.drag_area_z_m2 for e in elements) == 24 * (1 - 0.25)
    assert sum(e.drag_area_x_m2 for e in elements) == 12
    assert sum(e.drag_area_y_m2 for e in elements) == 6


@pytest.mark.parametrize(('field', 'value'), [
    ('length_m', 0), ('width_m', -2), ('length_m', float('nan')),
    ('length_m', float('inf')), ('width_m', True), ('length_m', '8'),
    ('count_x', 0), ('count_y', -2), ('count_x', 2.5), ('count_y', True),
    ('count_x', np.bool_(True)), ('count_y', np.int64(-1)),
    ('drag_area_x_m2', -1), ('drag_area_y_m2', float('nan')),
    ('gross_drag_area_z_m2', float('inf')), ('perforation_fraction', -0.1),
    ('perforation_fraction', 1.1), ('perforation_fraction', True),
    ('mask_x', (1,)), ('mask_y', None), ('mask_y', (1,) * 7),
    ('mask_x', (-1, 0, 0, 0, 0, 0)), ('mask_y', (float('nan'),) * 6),
    ('mask_x', (True,) * 6), ('mask_y', (1.1,) * 6),
])
def test_invalid_grid_is_rejected(field, value):
    with pytest.raises(ValueError):
        replace(example_grid(), **{field: value})


def test_inputs_and_outputs_are_immutable_and_masks_are_copied():
    mask = [1, 0, 0, 0, 0, 1]
    grid = replace(example_grid(), mask_x=mask)
    mask[0] = 0
    assert grid.mask_x == (1, 0, 0, 0, 0, 1)
    with pytest.raises(FrozenInstanceError):
        grid.length_m = 10
    with pytest.raises(FrozenInstanceError):
        rectangular_drag_elements(grid)[0].x_m = 10


@pytest.mark.parametrize('fraction', [0.0, 1.0])
def test_vertical_area_perforation_limits(fraction):
    elements = rectangular_drag_elements(replace(example_grid(), perforation_fraction=fraction))
    assert sum(e.drag_area_z_m2 for e in elements) == 24 * (1-fraction)


def test_zero_drag_areas_are_valid():
    grid = replace(example_grid(), drag_area_x_m2=0, drag_area_y_m2=0, gross_drag_area_z_m2=0)
    assert all(e.drag_area_x_m2 == e.drag_area_y_m2 == e.drag_area_z_m2 == 0
               for e in rectangular_drag_elements(grid))


def test_large_finite_dimensions_do_not_overflow_centres():
    grid = replace(example_grid(), length_m=1.6e308, width_m=1.6e308)
    elements = rectangular_drag_elements(grid)
    assert all(math.isfinite(e.x_m) and math.isfinite(e.y_m) for e in elements)
    assert elements[0].x_m < 0 < elements[-1].x_m


@pytest.mark.parametrize('field', ['length_m', 'gross_drag_area_z_m2'])
def test_unrepresentable_positive_spacing_or_area_is_rejected(field):
    with pytest.raises(ValueError):
        rectangular_drag_elements(replace(example_grid(), **{field: 5e-324}))


@pytest.mark.parametrize(('quanta', 'fraction'), [
    (8, 0), (11, 0.5), (600_000_000_001, 0), (2_000_000_000_000, 0.49999999998750005),
])
def test_inaccurate_positive_subnormal_area_is_rejected(quanta, fraction):
    # In six cells, 8q rounds to 6q in total. Half of 11q would also round to 6q.
    # Losing 1q from 600000000001q exceeds the relative tolerance of 1e-12.
    # For the last binary fraction the nearest total, 1000000000026q, errs by
    # 1.0001e-12 relative to the exact solid area. Rounding 1-p first hides it.
    q = math.ulp(0.0)
    grid = replace(example_grid(), gross_drag_area_z_m2=quanta*q, perforation_fraction=fraction)
    with pytest.raises(ValueError, match='area'):
        rectangular_drag_elements(grid)


@pytest.mark.parametrize(('field', 'quanta'), [('drag_area_x_m2', 5), ('drag_area_y_m2', 4)])
def test_subnormal_lateral_allocations_meet_the_same_tolerance(field, quanta):
    # The active x cells need 5q/2 each. The active y cell needs 4q/3.
    grid = replace(example_grid(), **{field: quanta*math.ulp(0.0)})
    with pytest.raises(ValueError, match='area'):
        rectangular_drag_elements(grid)


@pytest.mark.parametrize(('quanta', 'fraction', 'cell_quanta'), [
    (12, 0, 2), (6_000_000_000_001, 0, 10**12),
    (2_000_000_000_000, 0.4999999999905, 166_666_666_670),
])
def test_subnormal_area_within_relative_tolerance_is_retained(quanta, fraction, cell_quanta):
    # Both exact sharing and a 1q loss in over 6 trillion quanta meet 1e-12.
    # For the perforated case, 6*166666666670q is within 9.9998e-13.
    q = math.ulp(0.0)
    grid = replace(example_grid(), gross_drag_area_z_m2=quanta*q, perforation_fraction=fraction)
    assert [e.drag_area_z_m2 for e in rectangular_drag_elements(grid)] == [cell_quanta*q] * 6


def test_adjacent_centres_must_remain_distinct_in_subnormal_geometry():
    grid = RectangularDragGrid(
        length_m=4e-323, width_m=3, count_x=11, count_y=1,
        drag_area_x_m2=0, drag_area_y_m2=0, gross_drag_area_z_m2=0,
        perforation_fraction=0, mask_x=(0,) * 11, mask_y=(0,) * 11,
    )
    with pytest.raises(ValueError, match='centres'):
        rectangular_drag_elements(grid)
