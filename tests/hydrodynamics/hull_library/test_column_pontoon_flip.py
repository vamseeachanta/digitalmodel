"""Interior diagonal flips improve quality without changing the surface."""
import numpy as np
from digitalmodel.hydrodynamics.hull_library.column_pontoon_flip import flip_planar


def test_planar_bad_diagonal_improves_angles_and_preserves_vertices():
    a, b, c, d = np.array([[0, 0, 0], [10, 0, 0], [10, 1, 0], [0, .1, 0]], float)
    triangles = [np.array([a, b, c]), np.array([a, c, d])]
    result, owners, changed = flip_planar(triangles, [0, 1])
    shared = set(map(tuple, result[0])) & set(map(tuple, result[1]))
    assert shared == {tuple(b), tuple(d)}
    assert set(map(tuple, np.vstack(result))) == set(map(tuple, np.vstack(triangles)))
    assert owners == [0, 1]
    assert changed == {0, 1}


def test_true_crease_is_retained():
    triangles = [np.array([[0, 0, 0], [10, 0, 0], [10, 1, 0]], float),
                 np.array([[0, 0, 0], [10, 1, 0], [0, .1, 1]], float)]
    result, _, changed = flip_planar(triangles, [0, 1])
    assert all(np.array_equal(a, b) for a, b in zip(result, triangles))
    assert not changed
