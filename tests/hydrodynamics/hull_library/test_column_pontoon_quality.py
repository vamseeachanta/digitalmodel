"""Actual quad edge-ratio optimisation at unavoidable acute facet corners."""
import numpy as np
import pytest

from digitalmodel.hydrodynamics.hull_library.column_pontoon_quality import (
    optimize_quad_centres,
    quad_ratios,
)


@pytest.mark.parametrize('scale', [1e-4, 1, 1e4])
def test_flat_375_degree_triangle_has_convex_quads_within_bound(scale):
    height = np.tan(np.deg2rad(3.75))
    tri = scale*np.array([[[-1., 0., 0.], [1., 0., 0.], [0., height, 0.]]])
    weights = np.linalg.norm(np.roll(tri, -1, axis=1)-np.roll(tri, -2, axis=1), axis=2)
    incenter = np.sum(tri*weights[..., None], axis=1)/weights.sum(axis=1)[:, None]
    assert quad_ratios(tri, incenter)[0] > 20
    centres, quality = optimize_quad_centres(tri)
    assert quality[0] < 20
    mids = (tri[0]+np.roll(tri[0], -1, axis=0))/2
    for i in range(3):
        quad = np.array([tri[0, i], mids[i], centres[0], mids[i-1]])
        turns = np.cross(np.roll(quad, -1, axis=0)-quad,
                         np.roll(quad, -2, axis=0)-np.roll(quad, -1, axis=0))
        assert np.all(turns[:, 2] > 0)
    midpoint = (tri[0, 0]+tri[0, 1])/2
    children = np.array([[tri[0, 0], midpoint, tri[0, 2]],
                         [midpoint, tri[0, 1], tri[0, 2]]])
    _, child_quality = optimize_quad_centres(children)
    assert child_quality.max() < 20
    assert quality[0] == pytest.approx(quad_ratios(tri, centres)[0])



def test_rigid_transform_preserves_quality():
    tri = np.array([[[0., 0., 0.], [2., 0., 0.], [1., .08, 0.]]])
    _, first = optimize_quad_centres(tri)
    transformed = tri[:, :, [2, 0, 1]] + [10, 20, 30]
    _, second = optimize_quad_centres(transformed)
    assert second == pytest.approx(first)


def test_optimized_three_quads_retain_triangle_area():
    tri = np.array([[[0., 0., 0.], [2., 0., 0.], [1., .08, 0.]]])
    centres, _ = optimize_quad_centres(tri)
    mids = (tri+np.roll(tri, -1, axis=1))/2
    area = 0.
    for i in range(3):
        quad = np.array([tri[0, i], mids[0, i], centres[0], mids[0, i-1]])
        signed = np.sum(np.cross(quad, np.roll(quad, -1, axis=0)), axis=0)/2
        turns = np.cross(np.roll(quad, -1, axis=0)-quad,
                         np.roll(quad, -2, axis=0)-np.roll(quad, -1, axis=0))
        assert np.all(turns[:, 2] > 0)
        assert signed[2] > 0
        area += signed[2]
    assert area == pytest.approx(.08)
