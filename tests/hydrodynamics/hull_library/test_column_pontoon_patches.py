"""Clipped patches shall fail closed instead of dropping unresolved area."""
import numpy as np
import pytest

from digitalmodel.hydrodynamics.hull_library.column_pontoon_patches import triangulate


@pytest.mark.parametrize('ring', [
    [[0, 0, 0], [1, 0, 0], [2, 0, 0], [3, 0, 0]],
    [[0, 0, 0], [1, 1, 0], [0, 1, 0], [1, 0, 0]],
])
def test_degenerate_or_crossed_patch_raises(ring):
    with pytest.raises(ValueError, match='triangulate'):
        triangulate(np.array(ring, dtype=float))


def test_nonzero_normal_without_admissible_ear_raises():
    ring = np.array([[-1, 0, 0], [1, 1, 0], [-2, 2, 0], [2, 3, 0]], dtype=float)
    assert np.linalg.norm(np.sum(np.cross(ring, np.roll(ring, -1, axis=0)), axis=0)) > 0
    with pytest.raises(ValueError, match='preserving its boundary'):
        triangulate(ring)


@pytest.mark.parametrize('scale', [1, 1e-4, 1e-7])
def test_valid_slender_patch_is_scale_invariant(scale):
    ring = scale * np.array([[0, 0, 0], [10, 0, 0], [10, 1, 0], [0, 1, 0]], dtype=float)
    triangles = triangulate(ring)
    area = sum(np.linalg.norm(np.cross(t[1]-t[0], t[2]-t[0])) / 2 for t in triangles)
    assert len(triangles) == 2
    assert area == pytest.approx(10 * scale**2, rel=1e-12)
