"""Cutout locality, quality and reporting regressions."""

import math
import numpy as np
import pytest
from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from .test_mesh_cutouts import (
    api,
    box_mesh,
    flat_monohull,
    volume,
    assert_wetted_closed,
)


def panel_coordinates(mesh, mask=None):
    panels = mesh.panels if mask is None else mesh.panels[mask]
    return {tuple(map(tuple, mesh.vertices[list(dict.fromkeys(p))])) for p in panels}


def test_circle_preserves_side_quads_and_bounds_new_triangle_quality():
    Footprint, cut = api()
    source = box_mesh()
    result = cut(source, Footprint.circle(center=(9.3, 0.4), radius=1.7))
    assert panel_coordinates(
        source, [False, True, True, True, True]
    ) <= panel_coordinates(result.mesh)
    quality = result.report["mesh_quality"]
    assert quality["max_triangle_aspect_ratio"] <= 50
    assert quality["min_panel_area"] == pytest.approx(min(result.mesh.panel_areas))
    assert quality["min_panel_area"] > 1e-8
    # Angle criterion is computed independently from side lengths, rather than
    # trusting the report's aspect metric.
    for panel in result.mesh.panels:
        if len(set(panel)) != 3:
            continue
        p = result.mesh.vertices[list(dict.fromkeys(panel))]
        lengths = np.linalg.norm(p - np.roll(p, 1, axis=0), axis=1)
        angles = [
            math.degrees(
                math.acos(np.clip((a * a + b * b - c * c) / (2 * a * b), -1, 1))
            )
            for a, b, c in [lengths, np.roll(lengths, 1), np.roll(lengths, 2)]
        ]
        assert min(angles) >= 1.14
    assert quality["triangle_count"] > 0
    assert quality["quad_count"] + quality["triangle_count"] == result.mesh.n_panels
    assert_wetted_closed(result.mesh)


def test_remote_nonconforming_transition_is_preserved():
    Footprint, cut = api()
    source = box_mesh()
    extra = np.array(
        [
            [30.0, 0.0, -2.0],
            [32.0, 0.0, -2.0],
            [32.0, 0.0, 0.0],
            [30.0, 0.0, 0.0],
            [31.0, 0.0, -2.0],
            [31.0, 1.0, -2.0],
            [30.0, 1.0, -2.0],
        ]
    )
    source = PanelMesh(
        vertices=np.vstack((source.vertices, extra)),
        panels=np.vstack((source.panels, [[8, 9, 10, 11], [8, 14, 13, 12]])),
    )
    result = cut(source, Footprint.circle(center=(9.3, 0.4), radius=1.7))
    assert panel_coordinates(source, [False] * 5 + [True] * 2) <= panel_coordinates(
        result.mesh
    )


def test_near_mesh_line_rejected_with_explicit_relative_tolerance():
    Footprint, cut = api()
    footprint = Footprint.rectangle(center=(10.00001, 0.0), length=4.0, width=2.0)
    with pytest.raises(ValueError, match="clearance_tolerance"):
        cut(flat_monohull(False), footprint, clearance_tolerance=1e-4)


def test_near_existing_cut_rejected():
    Footprint, cut = api()
    first = cut(
        box_mesh(), Footprint.rectangle(center=(8.0, 0.0), length=2.0, width=2.0)
    )
    with pytest.raises(ValueError, match="clearance_tolerance"):
        cut(
            first.mesh,
            Footprint.rectangle(center=(10.000001, 0.0), length=2.0, width=2.0),
        )


@pytest.mark.parametrize("segments", [8, 16, 32])
def test_circle_equal_area_polygon_without_segment_floor(segments):
    Footprint, cut = api()
    result = cut(
        box_mesh(), Footprint.circle(center=(9.3, 0.4), radius=1.7, segments=segments)
    )
    entry = result.report["cutouts"][-1]
    assert entry["footprint_area"] == pytest.approx(math.pi * 1.7**2, rel=1e-12)
    assert volume(box_mesh()) - volume(result.mesh) == pytest.approx(
        entry["footprint_area"] * 4, rel=1e-9
    )
    assert entry["nominal_area"] == pytest.approx(math.pi * 1.7**2)


def test_wall_indices_identify_each_cutout():
    Footprint, cut = api()
    first = cut(
        box_mesh(), Footprint.rectangle(center=(5.0, 0.0), length=2.0, width=2.0)
    )
    second = cut(first.mesh, Footprint.circle(center=(15.0, 0.0), radius=1.0))
    ids = np.asarray(second.mesh.metadata["moonpool_index"])
    flags = np.asarray(second.mesh.metadata["moonpool_wall"])
    assert len(ids) == second.mesh.n_panels
    assert set(ids[flags]) == {0, 1}
    assert np.all(ids[~flags] == -1)
    assert np.all(second.mesh.panel_centers[ids == 0, 0] < 10)
    assert np.all(second.mesh.panel_centers[ids == 1, 0] > 10)


def test_nonzero_waterline_removed_volume_and_seam():
    Footprint, cut = api()
    source = box_mesh()
    source.vertices[:, 2] += 2.5
    result = cut(
        source,
        Footprint.rectangle(center=(9.3, 0.4), length=3.7, width=2.9),
        waterline=2.5,
    )
    entry = result.report["cutouts"][-1]
    assert entry["draft"] == 4
    assert entry["removed_volume"] == pytest.approx(3.7 * 2.9 * 4, rel=1e-9)
    # Translate the open waterline to z=0 before signed-volume integration.
    source.vertices[:, 2] -= 2.5
    result.mesh.vertices[:, 2] -= 2.5
    assert volume(source) - volume(result.mesh) == pytest.approx(
        entry["removed_volume"], rel=1e-9
    )
    assert_wetted_closed(result.mesh)


@pytest.mark.parametrize("tolerance", [0, -1, float("nan"), float("inf")])
def test_invalid_clearance_tolerance(tolerance):
    Footprint, cut = api()
    with pytest.raises(ValueError, match="clearance_tolerance"):
        cut(
            box_mesh(),
            Footprint.circle(center=(9.3, 0.4), radius=1.7),
            clearance_tolerance=tolerance,
        )


def test_offcentre_cut_on_expanded_half_model():
    Footprint, cut = api()
    source = flat_monohull(True)
    result = cut(source, Footprint.circle(center=(10.3, 0.4), radius=1.3))
    entry = result.report["cutouts"][-1]
    assert 2 * volume(source) - volume(result.mesh) == pytest.approx(
        entry["footprint_area"] * 4, rel=1e-9
    )
    assert_wetted_closed(result.mesh)
    assert result.mesh.symmetry_plane is None


def test_unindexed_source_walls_are_rejected_instead_of_losing_flags():
    Footprint, cut = api()
    first = cut(
        box_mesh(), Footprint.rectangle(center=(5.0, 0.0), length=2.0, width=2.0)
    )
    del first.mesh.metadata["moonpool_index"]
    with pytest.raises(ValueError, match="moonpool_index"):
        cut(first.mesh, Footprint.circle(center=(15.0, 0.0), radius=1.0))


def test_subresolution_cut_rejected_instead_of_reporting_unchanged_mesh():
    Footprint, cut = api()
    with pytest.raises(ValueError, match="resolution"):
        cut(box_mesh(), Footprint.circle(center=(9.3, 0.4), radius=1e-5))
