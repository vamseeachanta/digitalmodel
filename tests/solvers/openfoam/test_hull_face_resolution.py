#!/usr/bin/env python3
"""
ABOUTME: The post-mesh gate (#2033): the largest face on the hull patch,
against the cell the refinement staging was supposed to deliver.

WHY THIS GATE AND NOT ANOTHER. The mesh that invalidated a campaign passed
every check that existed. ``checkMesh`` said "Mesh OK": the cells were not
skewed, not non-orthogonal, not inverted -- they were simply enormous, in the
one place where that mattered. The layer-coverage number was 95-96 % and was
correct and irrelevant: it measures the boundary layer on a surface, and the
surface it was measuring had never been resolved. Cell COUNT was in the right
order of magnitude, because the cells the staging did refine were the ones
astern where nothing happens.

The one number that separated the two was the size of the biggest face on the
hull. On the defective mesh the worst face was 18.5 m2 against a 0.33 m target
cell -- a ratio of ~170. On the correctly-refined 90 % of the SAME patch the
worst face was 0.284 m2, a ratio of 2.6. Two numbers, an order of magnitude
and a half apart, from one cheap traversal of the boundary.
"""

from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Sequence

import pytest

from digitalmodel.solvers.openfoam.hull_face_resolution import (
    DEFAULT_FACE_AREA_FACTOR,
    HullFaceResolutionError,
    assert_patch_face_resolution,
    finest_in_plane_cell_m,
    patch_face_areas,
)

_HEADER = (
    "FoamFile\n"
    "{{\n"
    "    version     2.0;\n"
    "    format      {fmt};\n"
    "    class       {cls};\n"
    "    location    \"constant/polyMesh\";\n"
    "    object      {obj};\n"
    "}}\n"
)


def _write_list(path: Path, cls: str, obj: str, entries: Sequence[str],
                fmt: str = "ascii") -> None:
    body = "\n".join(entries)
    path.write_text(
        _HEADER.format(fmt=fmt, cls=cls, obj=obj)
        + f"\n{len(entries)}\n(\n{body}\n)\n"
    )


def write_polymesh(root: Path, quad_sides: Sequence[float],
                   patch: str = "hull", other_faces: int = 2,
                   fmt: str = "ascii") -> Path:
    """A polyMesh whose ``patch`` carries one square face per entry in
    ``quad_sides``, laid out flat so the areas are exactly the squares.

    ``other_faces`` internal faces are written FIRST, so ``startFace`` is a
    real offset and a reader that ignores it grabs the wrong faces -- which is
    the failure mode that would make this gate read a patch it was not asked
    about and pass.
    """
    poly = root / "constant" / "polyMesh"
    poly.mkdir(parents=True, exist_ok=True)

    points: list[str] = []
    faces: list[str] = []
    # Internal filler faces: deliberately HUGE, so a gate that reads them
    # instead of the patch fails loudly rather than passing quietly.
    for i in range(other_faces):
        base = len(points)
        x = 100.0 * i
        for px, py in ((0, 0), (50, 0), (50, 50), (0, 50)):
            points.append(f"({x + px} {py} 0)")
        faces.append(f"4({base} {base + 1} {base + 2} {base + 3})")

    start_face = len(faces)
    for i, side in enumerate(quad_sides):
        base = len(points)
        x = 10.0 * i
        for px, py in ((0.0, 0.0), (side, 0.0), (side, side), (0.0, side)):
            points.append(f"({x + px} {py} 3.5)")
        faces.append(f"4({base} {base + 1} {base + 2} {base + 3})")

    _write_list(poly / "points", "vectorField", "points", points, fmt=fmt)
    _write_list(poly / "faces", "faceList", "faces", faces, fmt=fmt)
    (poly / "boundary").write_text(
        _HEADER.format(fmt="ascii", cls="polyBoundaryMesh", obj="boundary")
        + "\n2\n(\n"
        "    inlet\n    {\n        type            patch;\n"
        "        nFaces          0;\n"
        f"        startFace       {start_face};\n    }}\n"
        f"    {patch}\n    {{\n        type            wall;\n"
        f"        nFaces          {len(quad_sides)};\n"
        f"        startFace       {start_face};\n    }}\n"
        ")\n"
    )
    return root


# --------------------------------------------------------------------------- #
#  Reading the patch
# --------------------------------------------------------------------------- #

def test_the_largest_face_on_the_patch_is_found(tmp_path):
    case = write_polymesh(tmp_path, [0.4, 0.9, 0.2])
    areas = patch_face_areas(case / "constant" / "polyMesh", "hull")
    assert areas.n_faces == 3
    assert areas.max_area_m2 == pytest.approx(0.81)
    assert areas.total_area_m2 == pytest.approx(0.16 + 0.81 + 0.04)
    # The worst face has to be locatable in the case, not just counted.
    assert areas.max_area_centre[2] == pytest.approx(3.5)


def test_the_internal_faces_are_not_mistaken_for_the_patch(tmp_path):
    """``startFace`` is load-bearing. The filler faces are 2500 m2 each."""
    case = write_polymesh(tmp_path, [0.4], other_faces=3)
    areas = patch_face_areas(case / "constant" / "polyMesh", "hull")
    assert areas.max_area_m2 == pytest.approx(0.16)


def test_an_absent_patch_fails_closed_and_names_what_is_there(tmp_path):
    """A renamed hull patch must stop the chain. Reporting "no faces, nothing
    over the limit" is the shape of every defect this gate exists to catch."""
    case = write_polymesh(tmp_path, [0.4], patch="hull_surface")
    with pytest.raises(HullFaceResolutionError) as exc:
        patch_face_areas(case / "constant" / "polyMesh", "hull")
    assert "hull_surface" in str(exc.value)


def test_a_binary_mesh_fails_closed_rather_than_reading_zero_faces(tmp_path):
    case = write_polymesh(tmp_path, [0.4], fmt="binary")
    with pytest.raises(HullFaceResolutionError) as exc:
        patch_face_areas(case / "constant" / "polyMesh", "hull")
    assert "binary" in str(exc.value).lower()


# --------------------------------------------------------------------------- #
#  The assertion
# --------------------------------------------------------------------------- #

def _areas(tmp_path, side: float):
    case = write_polymesh(tmp_path, [side * 0.5, side])
    return patch_face_areas(case / "constant" / "polyMesh", "hull")


def test_a_correctly_refined_patch_passes(tmp_path):
    """The measured ratio on the well-refined 90 % of the defective mesh was
    0.284 m2 against a 0.33 m cell = 2.6. The threshold must clear that with
    room, or the gate fails good meshes."""
    finest = 0.33
    areas = _areas(tmp_path, math.sqrt(2.6) * finest)
    ratio = assert_patch_face_resolution(areas, finest)
    assert ratio == pytest.approx(2.6, rel=1e-6)


def test_the_defective_mesh_is_rejected_and_the_numbers_are_reported(tmp_path):
    """The ratio the campaign actually ran on: ~170."""
    finest = 0.33
    areas = _areas(tmp_path, math.sqrt(170.0) * finest)
    with pytest.raises(HullFaceResolutionError) as exc:
        assert_patch_face_resolution(areas, finest)
    message = str(exc.value)
    assert "170" in message, "the actual ratio is reported"
    assert f"{DEFAULT_FACE_AREA_FACTOR:g}" in message, "so is the limit"
    assert "hull" in message
    # The worst face has to be findable: a ratio with no location is a number
    # nobody can act on.
    assert f"{areas.max_area_m2:.4g}" in message
    assert "3.5" in message, "the worst face's position is quoted"


def test_the_threshold_is_a_parameter_and_bites_just_above_it(tmp_path):
    finest = 0.33
    areas = _areas(tmp_path, math.sqrt(6.0) * finest)
    assert assert_patch_face_resolution(areas, finest, factor=6.5) > 0
    with pytest.raises(HullFaceResolutionError):
        assert_patch_face_resolution(areas, finest, factor=5.5)


def test_a_non_positive_cell_size_is_refused(tmp_path):
    areas = _areas(tmp_path, 0.1)
    with pytest.raises(HullFaceResolutionError):
        assert_patch_face_resolution(areas, 0.0)


# --------------------------------------------------------------------------- #
#  Where the target cell size comes from
# --------------------------------------------------------------------------- #

def test_the_target_cell_is_read_from_the_cases_own_provenance(tmp_path):
    (tmp_path / "case_provenance.json").write_text(
        json.dumps({"mesh": {"finest_in_plane_cell_m": 0.328125}})
    )
    assert finest_in_plane_cell_m(tmp_path) == pytest.approx(0.328125)


def test_a_case_that_cannot_state_its_target_cell_fails_closed(tmp_path):
    """No provenance means the gate cannot run. It must say so and stop.

    Skipping is worse than failing: a missing check reads greener than a
    failing one, and this gate exists precisely because an absent signal was
    read as a passing one for an entire campaign.
    """
    with pytest.raises(HullFaceResolutionError) as exc:
        finest_in_plane_cell_m(tmp_path)
    assert "case_provenance.json" in str(exc.value)

    (tmp_path / "case_provenance.json").write_text(json.dumps({"mesh": {}}))
    with pytest.raises(HullFaceResolutionError):
        finest_in_plane_cell_m(tmp_path)
