"""Illustrative example flaw footprints with independently resolved projections."""

import math
from dataclasses import FrozenInstanceError

import pytest

from digitalmodel.asset_integrity.metal_loss_flaw_geometry import (
    FlawFootprint,
    project_flaw_extents,
)


@pytest.mark.parametrize('angle,expected', [
    (0.0, (0.08, 0.03)), (math.pi / 2, (0.03, 0.08)),
    (math.pi / 6, (0.08 * math.sqrt(3) / 2 + 0.03 / 2,
                   0.08 / 2 + 0.03 * math.sqrt(3) / 2)),
])
def test_rectangle_projection(angle, expected):
    extents = project_flaw_extents(FlawFootprint(0.08, 0.03, angle))
    assert (extents.longitudinal_m, extents.circumferential_m) == pytest.approx(expected)


def test_square_is_symmetric_and_scales_linearly():
    a = project_flaw_extents(FlawFootprint(0.04, 0.04, math.pi / 4))
    b = project_flaw_extents(FlawFootprint(0.08, 0.08, math.pi / 4))
    assert a.longitudinal_m == pytest.approx(a.circumferential_m)
    assert b.longitudinal_m == pytest.approx(2 * a.longitudinal_m)


@pytest.mark.parametrize('length,width,angle', [
    (0.0, 0.03, 0.0), (0.08, -0.03, 0.0), (0.08, 0.03, -0.1),
    (0.08, 0.03, math.pi), (0.08, 0.03, math.nan),
    (math.inf, 0.03, 0.0), (True, 0.03, 0.0),
])
def test_invalid_footprint(length, width, angle):
    with pytest.raises(ValueError):
        FlawFootprint(length, width, angle)


def test_footprint_is_frozen():
    with pytest.raises(FrozenInstanceError):
        FlawFootprint(0.08, 0.03, 0.0).orientation_rad = 0.1
