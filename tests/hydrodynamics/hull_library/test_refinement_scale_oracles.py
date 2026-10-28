# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent planar rectangle oracles for generated panels.

Hand calculation: a rectangle with sides W and H has area W*H. After L
midpoint subdivisions it has 4**L children, with sides W/2**L and H/2**L,
area W*H/4**L each, and unchanged total area and coordinate bounds. In the
cyclic coordinate planes XY, YZ and ZX, counterclockwise winding gives +Z,
+X and +Y respectively; reversing the winding reverses the normal.

The resolved cases use binary scales 2**(-7), 1 and 2**5, levels 1 through 3,
and integer power-of-two translations. Their shortest child edge is 2**(-10),
which has an exact ten-place decimal representation; all tested translated
midpoints are also representable in binary64. The small square has side
2**(-7): its child areas at these levels are 2**(-16), 2**(-18), 2**(-20).
Only the last is below the existing 1e-6 advisory area threshold (source:
mesh_refiner.compute_quality_metrics); none is geometrically collapsed.

Areas below are measured independently with the signed planar shoelace sum,
using local coordinates to avoid subtracting large translated products.
Package areas and normals are observations, never the source of expectations.
Separate boundary characterizations cite the current implementation, not a
mesh acceptance criterion. No self-intersection qualification is asserted.
"""

from dataclasses import astuple
from math import fsum

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from digitalmodel.hydrodynamics.hull_library.mesh_refiner import (
    compute_quality_metrics,
    refine_mesh,
)


# Cyclic axes and their right-hand normals are derived in the module docstring.
PLANES = ((0, 1), (1, 2), (2, 0))
LEVELS = (1, 2, 3)
# This is an existing advisory threshold, not a generated-panel removal floor.
ADVISORY_AREA = 1e-6  # Source: mesh_refiner.compute_quality_metrics.


def _rectangle(width, height, origin, axes, winding=1):
    """Build one rectangle from its dimensions, without package geometry tools."""
    vertices = np.tile(np.asarray(origin, dtype=float), (4, 1))
    vertices[:, axes[0]] += [0, width, width, 0]
    vertices[:, axes[1]] += [0, 0, height, height]
    panel = [0, 1, 2, 3] if winding == 1 else [0, 3, 2, 1]
    return PanelMesh(vertices=vertices, panels=np.array([panel], dtype=np.int32))


def _signed_areas(mesh, axes):
    """Use the planar shoelace formula, independently of triangle cross products."""
    areas = []
    for panel in mesh.panels:
        points = mesh.vertices[panel][:, axes]
        points = points - points[0]
        terms = [
            points[i, 0] * points[(i + 1) % 4, 1]
            - points[(i + 1) % 4, 0] * points[i, 1]
            for i in range(4)
        ]
        areas.append(fsum(terms) / 2)
    return np.array(areas)


def _assert_rectangle_oracle(mesh, width, height, origin, axes, levels, winding):
    """Check counts, every child, grid coverage, bounds and oriented normals."""
    divisions = 2**levels
    child_count = divisions**2
    child_area = width * height / child_count
    assert mesh.n_panels == child_count
    assert mesh.n_vertices == (divisions + 1) ** 2
    assert all(len(set(panel)) == 4 for panel in mesh.panels)

    signed_areas = _signed_areas(mesh, axes)
    np.testing.assert_array_equal(
        signed_areas, np.full(child_count, winding * child_area)
    )
    assert fsum(abs(area) for area in signed_areas) == width * height
    np.testing.assert_array_equal(mesh.panel_areas, np.full(child_count, child_area))

    lower = np.asarray(origin, dtype=float)
    upper = lower.copy()
    upper[axes[0]] += width
    upper[axes[1]] += height
    np.testing.assert_array_equal(np.min(mesh.vertices, axis=0), lower)
    np.testing.assert_array_equal(np.max(mesh.vertices, axis=0), upper)
    box_min, box_max = mesh.bounding_box
    np.testing.assert_array_equal(box_min, lower)
    np.testing.assert_array_equal(box_max, upper)

    # A full grid of distinct lower corners also detects a duplicated child
    # replacing an omitted one, even if panel count and total area still agree.
    corners = {
        tuple(np.min(mesh.vertices[panel][:, axes], axis=0)) for panel in mesh.panels
    }
    expected_corners = {
        (
            lower[axes[0]] + i * width / divisions,
            lower[axes[1]] + j * height / divisions,
        )
        for i in range(divisions)
        for j in range(divisions)
    }
    assert corners == expected_corners

    normal = np.zeros(3)
    normal[3 - sum(axes)] = winding  # The remaining cyclic axis.
    np.testing.assert_array_equal(mesh.normals, np.tile(normal, (child_count, 1)))
    for values in (mesh.vertices, mesh.panel_areas, mesh.normals, mesh.panel_centers):
        assert np.all(np.isfinite(values))
    metrics = compute_quality_metrics(mesh)
    assert np.all(np.isfinite(astuple(metrics)))
    assert metrics.total_area == width * height
    # Aspect ratio finiteness is arithmetic coverage, not mesh qualification.


@pytest.mark.parametrize("axes", PLANES, ids=("xy", "yz", "zx"))
@pytest.mark.parametrize("winding", (1, -1), ids=("forward", "reverse"))
@pytest.mark.parametrize(
    "origin", ((0, 0, 0), (32, -64, 16), (2**20, -(2**20), 2**20))
)
def test_translated_rectangle_area(origin, axes, winding):
    """A 4 by 2 rectangle has sixteen area-1/2 children at level two."""
    source = _rectangle(4, 2, origin, axes, winding)
    refined = refine_mesh(source, levels=2)
    _assert_rectangle_oracle(refined, 4, 2, origin, axes, 2, winding)


@pytest.mark.parametrize("axes", PLANES, ids=("xy", "yz", "zx"))
@pytest.mark.parametrize("winding", (1, -1), ids=("forward", "reverse"))
@pytest.mark.parametrize("levels", LEVELS)
@pytest.mark.parametrize("scale", (2.0**-7, 1.0, 2.0**5))
def test_refined_area_by_hand(scale, levels, axes, winding):
    """Scaled 4 by 2 rectangles retain area 8*s**2 at each subdivision."""
    width, height = 4 * scale, 2 * scale
    origin = (32, -64, 16)
    source = _rectangle(width, height, origin, axes, winding)
    refined = refine_mesh(source, levels=levels)
    _assert_rectangle_oracle(refined, width, height, origin, axes, levels, winding)


@pytest.mark.parametrize("axes", PLANES, ids=("xy", "yz", "zx"))
@pytest.mark.parametrize("winding", (1, -1), ids=("forward", "reverse"))
@pytest.mark.parametrize("levels", LEVELS)
def test_resolved_small_children_survive(levels, axes, winding):
    """The advisory count can flag every child without removing any child."""
    side = 2.0**-7
    origin = (32, -64, 16)
    source = _rectangle(side, side, origin, axes, winding)
    refined = refine_mesh(source, levels=levels)
    _assert_rectangle_oracle(refined, side, side, origin, axes, levels, winding)
    child_count = 4**levels
    child_area = side**2 / child_count
    expected_advisory_count = child_count if child_area < ADVISORY_AREA else 0
    assert compute_quality_metrics(source).degenerate_count == 0
    assert compute_quality_metrics(refined).degenerate_count == expected_advisory_count
    assert child_count - refined.n_panels == 0


def test_rounding_collapse_boundary():
    """Characterize removal below the decimal welding resolution.

    Hand calculation: side 2**(-35) is positive but less than half of 1e-10.
    Every vertex and midpoint therefore rounds to the origin under the existing
    ten-place welding in mesh_refiner._subdivide_once. Fewer than three distinct
    indices removes every child; the advisory area count is a separate result.
    """
    side = 2.0**-35
    source = _rectangle(side, side, (0, 0, 0), (0, 1))
    assert _signed_areas(source, (0, 1))[0] == side**2 > 0
    assert compute_quality_metrics(source).degenerate_count == 1
    refined = refine_mesh(source)
    assert refined.n_panels == 0
    np.testing.assert_array_equal(refined.vertices, [[0, 0, 0]])
    assert compute_quality_metrics(refined).degenerate_count == 0


def test_small_normal_fallback_boundary():
    """Record an existing direction limitation without redefining acceptance.

    Hand calculation: a 4e-6 by 2e-6 YZ rectangle has four area-2e-12 children
    pointing +X. Its cross-product magnitude is below the existing 1e-10
    cutoff in PanelMesh._compute_normals, which substitutes +Z. The panels
    survive with finite areas, but their reported normals are unsupported here.
    """
    source = _rectangle(4e-6, 2e-6, (0, 0, 0), (1, 2))
    refined = refine_mesh(source)
    assert refined.n_panels == 4
    assert all(len(set(panel)) == 4 for panel in refined.panels)
    # Allow accumulated binary64 roundoff in the decimal-dimension example;
    # 32 eps bounds the fewer than 32 scalar operations per shoelace area.
    tolerance = 32 * np.finfo(float).eps
    np.testing.assert_allclose(
        _signed_areas(refined, (1, 2)), 2e-12, rtol=tolerance, atol=0
    )
    np.testing.assert_allclose(refined.panel_areas, 2e-12, rtol=tolerance, atol=0)
    np.testing.assert_array_equal(refined.normals, np.tile([0, 0, 1], (4, 1)))
    assert compute_quality_metrics(refined).degenerate_count == 4
    for values in (refined.vertices, refined.panel_areas, refined.normals):
        assert np.all(np.isfinite(values))
