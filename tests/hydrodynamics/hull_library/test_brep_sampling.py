"""Source-reference and topology checks for the opt-in BRep experiment."""

import numpy as np
import pytest

pytest.importorskip("hullprod")

from digitalmodel.hydrodynamics.hull_library.hull_surface_brep import (
    bspline_face_from_grid,
    profile_point_grid,
    profile_to_step,
)
from digitalmodel.hydrodynamics.hull_library.hull_surface_brep_sampling import (
    reference_section,
    section_arclength_grid,
    shared_bottom_face,
)


def test_arclength_points_preserve_reference(ship_profile):
    grid = section_arclength_grid(ship_profile, 17, 19)
    assert grid.shape == (17, 19, 3)
    for section in grid:
        assert section[:, 1] == pytest.approx(
            reference_section(ship_profile, section[0, 0], section[:, 2]), abs=1e-12
        )
        assert np.all(np.diff(section[:, 2]) > 0)
    assert grid[:, 0, 2] == pytest.approx(-ship_profile.draft)
    assert grid[:, -1, 2] == pytest.approx(0)


def test_reference_matches_default_grid(ship_profile):
    grid = profile_point_grid(ship_profile, 13, 17)
    for section in grid:
        assert reference_section(
            ship_profile, section[0, 0], section[:, 2]
        ) == pytest.approx(section[:, 1], abs=1e-12)


def test_partial_offsets_retain_constant_endpoint_extensions(box_profile):
    from digitalmodel.hydrodynamics.hull_library.profile_schema import HullStation

    profile = box_profile.model_copy(
        update={
            "stations": [
                HullStation(x_position=x, waterline_offsets=[(2, 3), (6, 9)])
                for x in (0, 50, 100)
            ]
        }
    )
    grid = section_arclength_grid(profile, 9, 17)
    for section in grid:
        expected = np.interp(section[:, 2] + profile.draft, [2, 6], [3, 9])
        assert section[:, 1] == pytest.approx(expected, abs=1e-12)
    assert grid[:, 0, 1] == pytest.approx(3)
    assert grid[:, -1, 1] == pytest.approx(9)


def test_shared_bottom_exact_box_area(box_profile):
    from OCP.BRepCheck import BRepCheck_Analyzer
    from OCP.BRepGProp import BRepGProp
    from OCP.GProp import GProp_GProps

    face = bspline_face_from_grid(section_arclength_grid(box_profile, 9, 9))
    bottom = shared_bottom_face(face, box_profile.draft)
    assert BRepCheck_Analyzer(bottom).IsValid()
    props = GProp_GProps()
    BRepGProp.SurfaceProperties_s(bottom, props)
    assert props.Mass() == pytest.approx(1000, abs=1e-6)


def test_shared_bottom_rejects_nonplanar_keel(box_profile):
    grid = profile_point_grid(box_profile, 9, 9)
    grid[:, :, 2] += np.linspace(0, 0.1, 9)[:, None]
    face = bspline_face_from_grid(grid)
    with pytest.raises(ValueError, match="planar"):
        shared_bottom_face(face, box_profile.draft)


def test_bottom_reuses_side_topological_edge(box_profile):
    from OCP.TopAbs import TopAbs_EDGE
    from OCP.TopExp import TopExp_Explorer
    from OCP.TopoDS import TopoDS

    from digitalmodel.hydrodynamics.hull_library.hull_surface_brep_sampling import (
        _keel_edge,
    )

    face = bspline_face_from_grid(section_arclength_grid(box_profile, 9, 9))
    keel = _keel_edge(face)
    bottom = shared_bottom_face(face, box_profile.draft)
    explorer = TopExp_Explorer(bottom, TopAbs_EDGE)
    shared = 0
    while explorer.More():
        shared += int(TopoDS.Edge_s(explorer.Current()).IsSame(keel))
        explorer.Next()
    assert shared == 1


@pytest.mark.parametrize("count", [True, 1, 2.5])
def test_sampling_rejects_invalid_counts(box_profile, count):
    with pytest.raises(ValueError, match="counts"):
        section_arclength_grid(box_profile, count, 11)


def test_unknown_sampling_rejected(box_profile, tmp_path):
    with pytest.raises(ValueError, match="sampling"):
        profile_to_step(box_profile, tmp_path / "box.step", sampling="unknown")
