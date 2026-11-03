# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand geometry for the analyzer's tetrahedron metric conventions.

Basis: an equilateral triangle of side one has altitude sqrt(3)/2, area
sqrt(3)/4 and centroid (1/2, sqrt(3)/6). Its centroid-to-corner distance
squared is 1/3, so a unit-edge regular tetrahedron has height sqrt(2/3).
The determinant of its three corner edges is 1/sqrt(2); V = det/6.

Quality definitions: quality_analyzer.MeshQualityAnalyzer documents
J = 3*(6V)**(2/3)/sum(edge**2) and AR = longest_edge/minimum_altitude.
Its skewness is 1 - smallest_face_area/largest_face_area; size is the
longest edge. These conventions give J = 2**(-4/3), AR = sqrt(3/2),
and skewness zero for a regular tetrahedron (neither J nor AR is unit
normalized). Expectations below use these hand results, not analyzer helpers.
"""

from itertools import combinations
from math import sqrt

import numpy as np
import pytest

from digitalmodel.solvers.gmsh_meshing.quality_analyzer import MeshQualityAnalyzer

# Floating point tolerance for the short determinant and rigid-motion arithmetic.
TOLERANCE = {"rel": 1e-12, "abs": 1e-12}
# Four vertices, one element; connectivity indices are zero-based by API contract.
ONE_TET = np.array([[0, 1, 2, 3]])


def _regular_nodes():
    """Construct unit-edge vertices using the triangle/height derivation above."""
    return np.array(
        [
            [0, 0, 0],
            [1, 0, 0],
            [1 / 2, sqrt(3) / 2, 0],
            [1 / 2, sqrt(3) / 6, sqrt(2 / 3)],
        ]
    )


def _stretched_nodes():
    """Triple the apex height, leaving the unit equilateral base fixed."""
    nodes = _regular_nodes()
    nodes[3, 2] *= 3
    return nodes


# Hand results for the unit tetrahedron, derived in the module docstring.
REGULAR_METRICS = (2 ** (-4 / 3), sqrt(3 / 2), 0)
# Triple height: V = 1/(2*sqrt(2)); three base edges have squared length 1
# and three apex edges have squared length 1/3 + 9*(2/3) = 19/3.
# Hence sum(edge**2) = 22. Each lateral face has area sqrt(73)/(4*sqrt(3)):
# its squared altitude to a base edge is 1/12 + 6 = 73/12.
# Minimum altitude = 3V/Amax = 3*sqrt(6)/sqrt(73).
STRETCHED_METRICS = (
    3 * (3 / sqrt(2)) ** (2 / 3) / 22,
    sqrt(19 / 3) * sqrt(73) / (3 * sqrt(6)),
    1 - 3 / sqrt(73),
)


def _assert_metrics(quality, expected):
    """Check each reported statistic against independent one-element values."""
    for metric, value in zip(("jacobian", "aspect_ratio", "skewness"), expected):
        for statistic in ("min", "max", "mean"):
            assert getattr(quality, f"{statistic}_{metric}") == pytest.approx(
                value, **TOLERANCE
            )


def test_regular_tetra_hand_geometry():
    """Six unit edges, determinant volume and all three hand-derived metrics."""
    nodes = _regular_nodes()
    edges = [np.linalg.norm(nodes[j] - nodes[i]) for i, j in combinations(range(4), 2)]
    assert edges == pytest.approx([1] * 6, **TOLERANCE)
    determinant = np.linalg.det(nodes[1:] - nodes[0])
    assert determinant == pytest.approx(1 / sqrt(2), **TOLERANCE)
    assert abs(determinant) / 6 == pytest.approx(sqrt(2) / 12, **TOLERANCE)

    analyzer = MeshQualityAnalyzer()
    _assert_metrics(analyzer.analyze_tetrahedral_mesh(nodes, ONE_TET), REGULAR_METRICS)
    stats = analyzer.compute_mesh_statistics(nodes, ONE_TET)
    assert stats.n_nodes == 4
    assert stats.n_elements == 1
    assert stats.dimension == 3
    assert stats.element_type == "tetrahedron"
    for statistic in ("min", "max", "mean"):
        assert getattr(stats, f"{statistic}_element_size") == pytest.approx(
            1, **TOLERANCE
        )
    _assert_metrics(stats.quality, REGULAR_METRICS)


# A cyclic permutation (x,y,z)->(z,x,y) is a proper 120-degree rotation
# about (1,1,1), with determinant +1. The integer translation and scale
# factors are arbitrary synthetic inputs, not engineering acceptance limits.
ROTATION = np.array([[0, 0, 1], [1, 0, 0], [0, 1, 0]])
TRANSLATION = np.array([5, -7, 11])


@pytest.mark.parametrize(
    "make_nodes, expected, longest_edge",
    [
        pytest.param(_regular_nodes, REGULAR_METRICS, 1, id="regular"),
        pytest.param(
            _stretched_nodes, STRETCHED_METRICS, sqrt(19 / 3), id="stretched"
        ),
    ],
)
@pytest.mark.parametrize(
    "rotate, translate, scale",
    [
        pytest.param(False, True, 1, id="translation"),
        pytest.param(True, False, 1, id="rotation"),
        pytest.param(False, False, 1 / 4, id="scale-down"),
        pytest.param(True, True, 4, id="rigid-motion-and-scale-up"),
    ],
)
def test_quality_rigid_motion_and_scale_invariance(
    make_nodes, expected, longest_edge, rotate, translate, scale
):
    """Dimensionless metrics stay fixed; all reported lengths scale by s."""
    nodes = make_nodes() * scale
    if rotate:
        nodes = nodes @ ROTATION.T
    if translate:
        nodes = nodes + TRANSLATION

    analyzer = MeshQualityAnalyzer()
    _assert_metrics(analyzer.analyze_tetrahedral_mesh(nodes, ONE_TET), expected)
    stats = analyzer.compute_mesh_statistics(nodes, ONE_TET)
    assert stats.n_nodes == 4
    assert stats.n_elements == 1
    _assert_metrics(stats.quality, expected)
    for statistic in ("min", "max", "mean"):
        assert getattr(stats, f"{statistic}_element_size") == pytest.approx(
            scale * longest_edge, **TOLERANCE
        )


def test_stretched_tetra_discriminates_shape():
    """A triple-height tetrahedron worsens each shape metric by hand geometry."""
    nodes = _stretched_nodes()
    edges = sorted(
        np.linalg.norm(nodes[j] - nodes[i]) for i, j in combinations(range(4), 2)
    )
    assert edges == pytest.approx([1] * 3 + [sqrt(19 / 3)] * 3, **TOLERANCE)
    determinant = np.linalg.det(nodes[1:] - nodes[0])
    assert determinant == pytest.approx(3 / sqrt(2), **TOLERANCE)
    assert abs(determinant) / 6 == pytest.approx(sqrt(2) / 4, **TOLERANCE)

    analyzer = MeshQualityAnalyzer()
    regular = analyzer.analyze_tetrahedral_mesh(_regular_nodes(), ONE_TET)
    stretched = analyzer.analyze_tetrahedral_mesh(nodes, ONE_TET)
    _assert_metrics(stretched, STRETCHED_METRICS)
    assert stretched.mean_jacobian < regular.mean_jacobian
    assert stretched.mean_aspect_ratio > regular.mean_aspect_ratio
    assert stretched.mean_skewness > regular.mean_skewness


@pytest.mark.parametrize("dictionary_input", [False, True], ids=["array", "gmsh-dict"])
def test_quality_statistics_element_count_readback(dictionary_input):
    """Read two disjoint tets and aggregate independent per-element results."""
    # Translation separates the two four-node elements without changing shape.
    nodes = np.vstack((_regular_nodes(), _stretched_nodes() + TRANSLATION))
    connectivity = np.vstack((ONE_TET, ONE_TET + 4))
    elements = (
        {"Tetrahedron 4": {"connectivity": connectivity, "dimension": 3}}
        if dictionary_input
        else connectivity
    )
    stats = MeshQualityAnalyzer().compute_mesh_statistics(nodes, elements)
    assert stats.n_nodes == 8
    assert stats.n_elements == 2
    assert stats.dimension == 3
    assert stats.element_type == "tetrahedron"
    assert stats.elements_per_node == pytest.approx(2 / 8, **TOLERANCE)
    assert stats.min_element_size == pytest.approx(1, **TOLERANCE)
    assert stats.max_element_size == pytest.approx(sqrt(19 / 3), **TOLERANCE)
    assert stats.mean_element_size == pytest.approx(
        (1 + sqrt(19 / 3)) / 2, **TOLERANCE
    )
    for metric, regular, stretched in zip(
        ("jacobian", "aspect_ratio", "skewness"), REGULAR_METRICS, STRETCHED_METRICS
    ):
        for statistic, value in (
            ("min", min(regular, stretched)),
            ("max", max(regular, stretched)),
            ("mean", (regular + stretched) / 2),
        ):
            assert getattr(stats.quality, f"{statistic}_{metric}") == pytest.approx(
                value, **TOLERANCE
            )
