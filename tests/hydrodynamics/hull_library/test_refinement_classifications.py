"""Synthetic classification regressions; dimensions follow the cutout tests."""

from copy import deepcopy
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from digitalmodel.hydrodynamics.hull_library.mesh_cutouts import (
    MoonpoolFootprint,
    cut_moonpool,
)
from digitalmodel.hydrodynamics.hull_library.mesh_refiner import (
    export_mesh_family,
    generate_mesh_family,
    refine_mesh,
)
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


@pytest.mark.parametrize("key", ["moonpool_wall", "moonpool_index"])
def test_classified_family_rejects_coarsening(key: str) -> None:
    """Known panel keys cannot survive an unmapped topology change.

    Source: mesh_cutouts._assemble defines False and -1 for ordinary panels.
    Even these classifications must survive, so key presence is sufficient.
    The half-density request comes from generate_mesh_family's documented
    factors; its coarsening backends have no parent classification mapping.
    """
    source = box_mesh()
    source.metadata[key] = [False if key == "moonpool_wall" else -1] * source.n_panels
    before = deepcopy(source.metadata)
    with pytest.raises(ValueError, match="coarsen.*classifications"):
        generate_mesh_family(source, factors=[0.5])
    assert source.metadata == before


@pytest.mark.parametrize("key", ["moonpool_wall", "moonpool_index"])
def test_classified_family_export_fails_before_writing(key: str, tmp_path: Path) -> None:
    """GDFHandler.write has geometry and symmetry fields, no classification field.

    A mixed family must fail before writing even its ordinary first member.
    Source: generate_mesh_family documents factor 1 as the original mesh.
    """
    source = box_mesh()
    family = generate_mesh_family(source, factors=[1.0])
    classified = box_mesh()
    classified.metadata[key] = (
        [False if key == "moonpool_wall" else -1] * classified.n_panels
    )
    family.extend(generate_mesh_family(classified, factors=[1.0]))
    output = tmp_path / "geometry"
    with pytest.raises(ValueError, match="GDF.*classifications"):
        export_mesh_family(family, output)
    assert not output.exists()
    assert list(tmp_path.iterdir()) == []


def test_empty_classifications_survive_repeated_refinement() -> None:
    """Hand calculation: a point has no children with three unique vertices.

    Source: mesh_refiner._subdivide_once's existing validity rule. After the
    first pass removes every child, the second pass has no parents; both
    classifications therefore remain empty without dropping cutout metadata.
    """
    source = PanelMesh(
        vertices=np.zeros((1, 3)),
        panels=np.zeros((1, 4), dtype=np.int32),
        metadata={"moonpool_wall": [False], "moonpool_index": [-1], "cutouts": []},
    )
    result = refine_mesh(source, levels=2)
    assert result.panels.shape == (0, 4)
    assert result.metadata == {
        "moonpool_wall": [],
        "moonpool_index": [],
        "cutouts": [],
        "refinement_levels": 2,
    }


def test_classified_family_preserves_original_and_refined_members() -> None:
    """Original values and four children per quad follow the documented factors.

    Source: mesh_refiner.generate_mesh_family and _subdivide_once. The expected
    metadata is a direct parent-to-child expansion, independent of geometry.
    """
    source = rectangular_cutout()
    before = deepcopy(source.metadata)
    original, refined = generate_mesh_family(source, factors=[1.0, 4.0])
    for key in ("moonpool_wall", "moonpool_index"):
        assert original.mesh.metadata[key] == before[key]
        assert refined.mesh.metadata[key] == [v for v in before[key] for _ in range(4)]
    for member in (original, refined):
        assert member.mesh.metadata["cutouts"] == before["cutouts"]
    assert source.metadata == before


def test_partial_filter_keeps_each_retained_child_parent() -> None:
    """Hand calculation under _subdivide_once's three-unique-vertex rule.

    The first parent [A,A,A,B] uses unit-segment endpoints A=0 and B=1.
    Its centre is 1/4 and nonzero midpoints are 1/2. Child vertex coordinates
    are [0,0,1/4,1/2], [0,0,0,1/4], [1/4,0,0,1/2], [1/2,1/4,1/2,1].
    Only the second child has fewer than three distinct vertices. The next
    parent is a unit square retaining four children, so flags must have three
    True entries followed by four False entries. Collinear children remain
    valid under this existing index rule; no physical quality claim is made.
    """
    source = PanelMesh(
        vertices=np.array([[0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0]], dtype=float),
        panels=np.array([[0, 0, 0, 1], [0, 1, 2, 3]], dtype=np.int32),
        metadata={"moonpool_wall": [True, False], "moonpool_index": [0, -1]},
    )
    result = refine_mesh(source)
    assert result.n_panels == 3 + 4
    assert result.metadata["moonpool_wall"] == [True] * 3 + [False] * 4
    assert result.metadata["moonpool_index"] == [0] * 3 + [-1] * 4


def test_ordinary_panel_length_metadata_does_not_block_family(tmp_path: Path) -> None:
    """Sequence length alone does not establish the mesh_cutouts panel schema.

    Source: generate_mesh_family's documented half/original/fourfold factors.
    Ordinary metadata must not trigger classification guards at either API.
    """
    source = box_mesh()
    source.metadata["ordinary_list"] = ["ordinary"] * source.n_panels
    family = generate_mesh_family(source, factors=[0.5, 1.0, 4.0])
    paths = export_mesh_family(family, tmp_path / "geometry")
    assert len(paths) == len(family)
    assert all(path.is_file() for path in paths)


def test_classified_family_keeps_near_unity_identity_behavior() -> None:
    """The next float below unity takes generate_mesh_family's identity branch.

    Source: the existing factor comparison in generate_mesh_family. An identity
    copy has the same panel order, so classifications require no new mapping.
    """
    source = rectangular_cutout()
    (member,) = generate_mesh_family(source, factors=[float(np.nextafter(1.0, 0.0))])
    np.testing.assert_array_equal(member.mesh.panels, source.panels)
    for key in ("moonpool_wall", "moonpool_index", "cutouts"):
        assert member.mesh.metadata[key] == source.metadata[key]
