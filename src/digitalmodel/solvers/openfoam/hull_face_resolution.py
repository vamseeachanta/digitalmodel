#!/usr/bin/env python3
"""
ABOUTME: POST-MESH GATE (#2033). The largest face on the hull patch, measured
off the written mesh, against the cell the refinement staging was supposed to
deliver.

WHY THIS NUMBER. A mesh whose refinement boxes missed 42 % of the hull passed
everything that existed. ``checkMesh`` said "Mesh OK" -- the cells were not
skewed, not non-orthogonal, not inverted, they were merely enormous where it
mattered. The layer-coverage figure was 95-96 % and was both correct and
irrelevant: it scores the boundary layer on a surface, and the surface it
scored had never been resolved. The cell count was plausible, because the
cells the staging DID refine were the ones astern where nothing happens.

One number separated the good mesh from the bad, and it was on the boundary
the case exists to integrate:

    defective region (0.7 % of the patch)   max face 18.5 m2   ratio ~170
    correctly refined region (90 % of it)   max face 0.284 m2  ratio 2.6

Same patch, same run, same traversal. The gate is that traversal.

WHAT IT DOES NOT DO. It does not check that the mesh is fine ENOUGH in an
absolute sense -- that is the staging's job and the pre-mesh assertion in
``hull_placement`` is where a mis-placed box is caught. This one catches the
class of failure where the staging was right and the mesher did not deliver
it: a surface the boxes never reached, a patch renamed so the levels never
applied, a snap that failed back to the background cell.
"""

from __future__ import annotations

import json
import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterator, List, Sequence, Tuple

__all__ = [
    "DEFAULT_FACE_AREA_FACTOR",
    "HullFaceResolutionError",
    "PatchFaceAreas",
    "assert_patch_face_resolution",
    "finest_in_plane_cell_m",
    "patch_face_areas",
]

Vec3 = Tuple[float, float, float]

#: ``max hull face area < k * (finest cell)^2``. k, and the working.
#:
#: A boundary face is a plane section of a cell. The largest plane section of
#: a cube of side h is the rectangle through two opposite edges, area
#: sqrt(2) h^2 = 1.41 h^2 -- that is the floor for any correct mesh, before
#: anything else.
#:
#: snappyHexMesh leaves 2:1 transitions, so a face may legitimately sit one
#: refinement level coarser than the finest cell: 2x linear, 4x in area. That
#: takes the bound to 4 * 1.41 = 5.66 h^2.
#:
#: The remaining slack absorbs two things this lane knows it has: the cells
#: near the hull are ANISOTROPIC, because the refineMesh passes cut x and y
#: while the vertical cell comes from the blockMesh ladder (0.44 m against a
#: 0.328 m in-plane cell on the hull this was measured on); and snapping
#: displaces face points onto the surface, which stretches them.
#:
#: 8 is 5.66 rounded up through that slack. It is not tuned to just-pass: the
#: correctly refined 90 % of the defective mesh scored 2.6, three times INSIDE
#: the limit, and the defective 0.7 % scored ~170, twenty-one times OUTSIDE
#: it. Any k in roughly [4, 40] separates those two, so the gate's verdict is
#: not sensitive to where in that range it is set.
DEFAULT_FACE_AREA_FACTOR = 8.0


class HullFaceResolutionError(ValueError):
    """A written mesh whose hull patch is too coarse to integrate a pressure,
    or a case that cannot state what "too coarse" would mean."""


@dataclass(frozen=True)
class PatchFaceAreas:
    """One traversal of one patch. Enough to act on, not only to judge."""

    patch: str
    n_faces: int
    max_area_m2: float
    max_area_face: int
    max_area_centre: Vec3
    total_area_m2: float

    @property
    def mean_area_m2(self) -> float:
        return self.total_area_m2 / self.n_faces


# --------------------------------------------------------------------------- #
#  The verdict
# --------------------------------------------------------------------------- #

def assert_patch_face_resolution(
    areas: PatchFaceAreas,
    finest_cell_m: float,
    *,
    factor: float = DEFAULT_FACE_AREA_FACTOR,
) -> float:
    """Raise unless ``max face area < factor * finest_cell_m^2``.

    Returns the measured ratio on success, so a passing gate still puts a
    number on the record. A gate that only ever prints "OK" is one nobody can
    watch drift.
    """
    if finest_cell_m <= 0.0:
        raise HullFaceResolutionError(
            f"finest_cell_m must be positive, got {finest_cell_m!r}; without "
            f"it there is no scale to compare a face area against"
        )
    if factor <= 0.0:
        raise HullFaceResolutionError(f"factor must be positive, got {factor!r}")

    cell_area = finest_cell_m * finest_cell_m
    ratio = areas.max_area_m2 / cell_area
    if ratio < factor:
        return ratio
    x, y, z = areas.max_area_centre
    raise HullFaceResolutionError(
        f"patch {areas.patch!r}: largest face is {areas.max_area_m2:.4g} m2, "
        f"which is {ratio:.4g}x the target cell area "
        f"({finest_cell_m:.4g} m)^2 = {cell_area:.4g} m2. The limit is "
        f"{factor:g}x. Worst face is index {areas.max_area_face} at "
        f"({x:.4g} {y:.4g} {z:.4g}); mean face on this patch is "
        f"{areas.mean_area_m2:.4g} m2 over {areas.n_faces} faces. "
        f"A face this size on the wetted surface integrates a pressure the "
        f"mesh cannot represent -- check that every refinement box contains "
        f"the hull and that the surface carries a non-zero refinement level."
    )


def finest_in_plane_cell_m(case_dir: Path | str) -> float:
    """The target cell, read from the case's own provenance.

    FAILS CLOSED. A case that cannot say what cell it was built for cannot be
    gated, and "cannot be gated" must stop the chain rather than skip quietly.
    An absent check reads greener than a failing one, which is exactly how the
    defect this gate exists for survived: every signal that could have carried
    it was either absent or measuring something else.
    """
    path = Path(case_dir) / "case_provenance.json"
    if not path.is_file():
        raise HullFaceResolutionError(
            f"no case_provenance.json at {path}: the gate cannot determine the "
            f"cell size this mesh was built for. Pass it explicitly "
            f"(--finest-cell / DM_CFD_FINEST_CELL_M) for a case built outside "
            f"the hull case builder."
        )
    try:
        data = json.loads(path.read_text())
    except json.JSONDecodeError as exc:
        raise HullFaceResolutionError(f"{path} is not valid JSON: {exc}") from exc
    value = (data.get("mesh") or {}).get("finest_in_plane_cell_m")
    if value is None or float(value) <= 0.0:
        raise HullFaceResolutionError(
            f"{path} does not record mesh.finest_in_plane_cell_m "
            f"(found {value!r}). It is written by the hull case builders; a "
            f"case from another route must state the cell explicitly."
        )
    return float(value)


# --------------------------------------------------------------------------- #
#  Reading the mesh
# --------------------------------------------------------------------------- #

_BOUNDARY_ENTRY = re.compile(
    r"(\w+)\s*\{[^{}]*?nFaces\s+(\d+)\s*;[^{}]*?startFace\s+(\d+)\s*;[^{}]*?\}",
    re.S,
)


def patch_face_areas(polymesh_dir: Path | str, patch: str) -> PatchFaceAreas:
    """Traverse one boundary patch of a written polyMesh and measure it."""
    poly = Path(polymesh_dir)
    bounds = _read_boundary(poly / "boundary")
    if patch not in bounds:
        raise HullFaceResolutionError(
            f"patch {patch!r} is not in {poly / 'boundary'}; it carries "
            f"{sorted(bounds)}. A renamed hull patch is not a reason to pass: "
            f"the force function objects name it too, so the case would "
            f"report nothing."
        )
    n_faces, start_face = bounds[patch]
    if n_faces < 1:
        raise HullFaceResolutionError(
            f"patch {patch!r} has {n_faces} faces; there is no surface to "
            f"integrate a force over"
        )
    faces = _read_faces(poly / "faces", start_face, n_faces)
    points = _read_points(poly / "points", {i for f in faces for i in f})

    best_area, best_index, best_centre = -1.0, -1, (0.0, 0.0, 0.0)
    total = 0.0
    for offset, face in enumerate(faces):
        area, centre = _face_area_and_centre([points[i] for i in face])
        total += area
        if area > best_area:
            best_area, best_index, best_centre = area, start_face + offset, centre
    return PatchFaceAreas(
        patch=patch,
        n_faces=n_faces,
        max_area_m2=best_area,
        max_area_face=best_index,
        max_area_centre=best_centre,
        total_area_m2=total,
    )


def _face_area_and_centre(verts: Sequence[Vec3]) -> Tuple[float, Vec3]:
    """OpenFOAM's own decomposition: fan the polygon about its average point.

    A hull face after snapping is not planar, and the flat cross-product of
    the first three vertices would under-report exactly the warped faces this
    gate is looking for.
    """
    n = len(verts)
    if n < 3:
        return 0.0, (0.0, 0.0, 0.0)
    cx = sum(v[0] for v in verts) / n
    cy = sum(v[1] for v in verts) / n
    cz = sum(v[2] for v in verts) / n
    ax = ay = az = 0.0
    for i in range(n):
        p, q = verts[i], verts[(i + 1) % n]
        ux, uy, uz = p[0] - cx, p[1] - cy, p[2] - cz
        vx, vy, vz = q[0] - cx, q[1] - cy, q[2] - cz
        ax += uy * vz - uz * vy
        ay += uz * vx - ux * vz
        az += ux * vy - uy * vx
    return 0.5 * math.sqrt(ax * ax + ay * ay + az * az), (cx, cy, cz)


def _read_boundary(path: Path) -> Dict[str, Tuple[int, int]]:
    text = _read_ascii(path)
    return {
        m.group(1): (int(m.group(2)), int(m.group(3)))
        for m in _BOUNDARY_ENTRY.finditer(text)
    }


def _read_faces(path: Path, start: int, count: int) -> List[List[int]]:
    faces: List[List[int]] = []
    for index, line in enumerate(_list_body(path)):
        if index < start:
            continue
        if index >= start + count:
            break
        inside = line[line.index("(") + 1: line.rindex(")")]
        faces.append([int(tok) for tok in inside.split()])
    if len(faces) != count:
        raise HullFaceResolutionError(
            f"{path}: expected {count} faces from index {start}, read "
            f"{len(faces)}. The face list is shorter than the boundary claims."
        )
    return faces


def _read_points(path: Path, wanted: set) -> Dict[int, Vec3]:
    """Only the points the patch refers to.

    A production hull mesh has tens of millions of points and a few tens of
    thousands of hull faces; materialising the whole field to measure the
    boundary would make the gate cost more than it saves.
    """
    points: Dict[int, Vec3] = {}
    for index, line in enumerate(_list_body(path)):
        if index not in wanted:
            continue
        inside = line[line.index("(") + 1: line.rindex(")")]
        x, y, z = (float(tok) for tok in inside.split())
        points[index] = (x, y, z)
    missing = wanted - set(points)
    if missing:
        raise HullFaceResolutionError(
            f"{path}: {len(missing)} point(s) referenced by the patch are not "
            f"in the points file (first: {min(missing)})"
        )
    return points


def _require_ascii(path: Path) -> None:
    """Refuse a binary mesh explicitly. Only the header is read to decide."""
    if not path.is_file():
        raise HullFaceResolutionError(f"no such polyMesh file: {path}")
    with path.open(errors="replace") as handle:
        header = handle.read(2000)
    if re.search(r"format\s+binary\s*;", header):
        raise HullFaceResolutionError(
            f"{path} is written in binary format; this gate reads ascii. "
            f"Set writeFormat ascii, or run foamFormatConvert -ascii. It does "
            f"NOT fall through to a pass: an unreadable mesh is an ungated one."
        )


def _read_ascii(path: Path) -> str:
    _require_ascii(path)
    return path.read_text(errors="replace")


def _list_body(path: Path) -> Iterator[str]:
    """Yield each entry of an OpenFOAM list, one line at a time.

    STREAMED, not tokenised whole. The list writer puts one entry per line,
    and ``points`` on a production hull mesh is a few hundred megabytes of
    text; the whole-file tokeniser the rest of this package uses would make
    the gate cost more than the mistake it prevents.
    """
    _require_ascii(path)
    with path.open(errors="replace") as handle:
        started = False
        for line in handle:
            stripped = line.strip()
            if not started:
                if stripped == "(":
                    started = True
                continue
            if stripped == ")":
                break
            if stripped:
                yield stripped
