"""Synthetic classification regressions; dimensions follow the cutout tests."""

from copy import deepcopy

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from digitalmodel.hydrodynamics.hull_library.mesh_cutouts import (
    MoonpoolFootprint,
    cut_moonpool,
)
from digitalmodel.hydrodynamics.hull_library.mesh_refiner import refine_mesh
from tests.hydrodynamics.hull_library.test_mesh_cutouts import box_mesh


def rectangular_cutout() -> PanelMesh:
    return cut_moonpool(
        box_mesh(),
        MoonpoolFootprint.rectangle(center=(9.3, 0.4), length=3.7, width=2.9),
        wall_layers=3,
    ).mesh


def test_rectangular_cutout_flags_follow_first_refinement() -> None:
    source = rectangular_cutout()
    result = refine_mesh(source)
    assert len(result.metadata["moonpool_wall"]) == result.n_panels
    assert result.metadata["moonpool_wall"] == [
        flag for flag in source.metadata["moonpool_wall"] for _ in range(4)
    ]


@pytest.mark.parametrize("levels", [1, 2])
def test_children_keep_parent_wall_planes_normals_and_flags(levels: int) -> None:
    source = rectangular_cutout()
    before = deepcopy(source.metadata)
    result = refine_mesh(source, levels=levels)
    assert result.panel_centers is not None
    assert result.normals is not None
    # Rectangle edges: center +/- half-dimension; box keel=-4, waterline=0.
    planes = [
        (0, 9.3 - 3.7 / 2, 1),
        (0, 9.3 + 3.7 / 2, -1),
        (1, 0.4 - 2.9 / 2, 1),
        (1, 0.4 + 2.9 / 2, -1),
    ]
    children = 4**levels
    for key in ("moonpool_wall", "moonpool_index"):
        assert len(result.metadata[key]) == result.n_panels
        assert result.metadata[key] == [v for v in before[key] for _ in range(children)]
    for parent, flag in enumerate(before["moonpool_wall"]):
        section = slice(parent * children, (parent + 1) * children)
        assert all(v == flag for v in result.metadata["moonpool_wall"][section])
        if not flag:
            continue
        parent_points = source.vertices[source.panels[parent]]
        axis, coordinate, sign = next(
            plane
            for plane in planes
            if np.allclose(parent_points[:, plane[0]], plane[1])
        )
        centers = result.panel_centers[section]
        np.testing.assert_allclose(centers[:, axis], coordinate)
        assert np.all(centers >= parent_points.min(axis=0) - 1e-9)
        assert np.all(centers <= parent_points.max(axis=0) + 1e-9)
        normal = np.zeros(3)
        normal[axis] = sign  # Inward toward the rectangular water column.
        np.testing.assert_allclose(
            result.normals[section], np.tile(normal, (children, 1))
        )
    assert source.metadata == before
    assert result.metadata["cutouts"] == before["cutouts"]


def test_repeated_calls_preserve_cumulative_opening_ids() -> None:
    # The disjoint-cut fixture uses centers 5 and 15 in a 20 m box.
    source = box_mesh()
    for x in (5, 15):
        source = cut_moonpool(
            source, MoonpoolFootprint.rectangle(center=(x, 0), length=2, width=2)
        ).mesh
    before = deepcopy(source.metadata)
    result = refine_mesh(refine_mesh(source))
    for key in ("moonpool_wall", "moonpool_index"):
        assert len(result.metadata[key]) == result.n_panels
        assert result.metadata[key] == [v for v in before[key] for _ in range(4 * 4)]
    assert set(result.metadata["moonpool_index"]) == {-1, 0, 1}
    assert result.metadata["cutouts"] == before["cutouts"]
    assert source.metadata == before


@pytest.mark.parametrize("key", ["moonpool_wall", "moonpool_index"])
@pytest.mark.parametrize("bad", [[], [0], None, [[0]] * 5, "wrong"])
def test_unmappable_classification_raises(key: str, bad: object) -> None:
    source = box_mesh()
    source.metadata[key] = bad
    with pytest.raises(ValueError, match=key):
        refine_mesh(source)


@pytest.mark.parametrize("flag", [False, True])
def test_mapping_follows_filtered_children(flag: bool) -> None:
    # First parent collapses entirely; the unit square retains four children.
    # This exercises the existing <3 unique vertices removal, not a new rule.
    mesh = PanelMesh(
        vertices=np.array(
            [[0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [1.0, 1.0, 0.0], [0.0, 1.0, 0.0]]
        ),
        panels=np.array([[0, 0, 0, 0], [0, 1, 2, 3]], dtype=np.int32),
        metadata={
            "moonpool_wall": [not flag, flag],
            "moonpool_index": [0 if not flag else -1, 0 if flag else -1],
        },
    )
    result = refine_mesh(mesh)
    assert result.n_panels == 4
    assert result.metadata["moonpool_wall"] == [flag] * 4
    assert result.metadata["moonpool_index"] == [0 if flag else -1] * 4


def test_all_filtered_children_leave_empty_classifications() -> None:
    mesh = PanelMesh(
        vertices=np.zeros((1, 3)),
        panels=np.zeros((1, 4), dtype=np.int32),
        metadata={"moonpool_wall": [False], "moonpool_index": [-1]},
    )
    result = refine_mesh(mesh)
    assert result.n_panels == 0
    assert result.metadata["moonpool_wall"] == []
    assert result.metadata["moonpool_index"] == []


def test_unclassified_quad_geometry_and_ordinary_metadata() -> None:
    # Unit square subdivision has nine half-unit grid points and four
    # quarter-area children, centered at Cartesian products of 1/4 and 3/4.
    source = PanelMesh(
        vertices=np.array(
            [[0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [1.0, 1.0, 0.0], [0.0, 1.0, 0.0]]
        ),
        panels=np.array([[0, 1, 2, 3]], dtype=np.int32),
        metadata={"ordinary_list": ["one entry"], "source": "synthetic"},
    )
    result = refine_mesh(source)
    np.testing.assert_array_equal(
        result.vertices, [[x, y, 0] for x in (0, 0.5, 1) for y in (0, 0.5, 1)]
    )
    np.testing.assert_array_equal(
        result.panels, [[0, 3, 4, 1], [3, 6, 7, 4], [4, 7, 8, 5], [1, 4, 5, 2]]
    )
    assert result.panel_areas is not None
    assert result.panel_centers is not None
    np.testing.assert_allclose(result.panel_areas, [1 / 4] * 4)
    np.testing.assert_allclose(
        result.panel_centers,
        [[0.25, 0.25, 0], [0.75, 0.25, 0], [0.75, 0.75, 0], [0.25, 0.75, 0]],
    )
    assert result.metadata == {**source.metadata, "refinement_levels": 1}
