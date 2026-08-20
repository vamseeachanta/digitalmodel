"""Open-boundary capping for client hull surfaces (#2023).

WHAT THESE TESTS DEFEND
-----------------------
Client hull surfaces routinely arrive as OPEN shells: the moulded surface
stops at deck level and there is no lid. ``snappyHexMesh`` cannot tell inside
from outside through such an opening, so the ingestion gate refuses them --
correctly, but that leaves an ordinary client hull unusable.

Capping closes the opening. It is also the single most dangerous repair this
stage can perform, because a lid placed BELOW the waterline changes both
quantities the analysis exists to report:

  * a cap below the waterline adds area to ``wetted_surface_m2`` and volume to
    ``displacement_m3``, silently. The case still meshes and still solves.
  * a cap whose triangulation self-intersects encloses the wrong region, and
    a naive centroid fan self-intersects the moment the plan-view outline is
    concave -- a transom notch is enough.
  * a boundary that is NOT a closed loop is a TEAR, not an opening. Capping it
    would hide a modelling defect behind a watertight verdict.

So the assertions below are, in order: the loops are found and verified as
genuine cycles; a torn boundary is REFUSED; a cap below the waterline is
REFUSED unless explicitly forced; and a cap above the waterline leaves the
hydrostatics bit-for-bit alone.

Every fixture is synthetic and built in pure Python, so this file needs
neither ``rhino3dm`` nor ``OCP`` nor a client file. The real-hull check is
opt-in through ``DIGITALMODEL_3DM_HULL`` and skips cleanly when it is unset,
exactly as ``test_brep_tessellate.py`` does.
"""

from __future__ import annotations

import inspect
import math
import os
from pathlib import Path
from typing import Callable, List, Tuple

import pytest

from digitalmodel.naval_architecture.hull_cap import (
    BoundaryLoop,
    CapBelowWaterlineError,
    CapTriangulationError,
    TornBoundaryError,
    cap_boundary_loops,
    find_boundary_loops,
    triangulate_loop,
)
from digitalmodel.naval_architecture.hull_ingest import (
    HullIngestError,
    NotWatertightError,
    ingest_triangles,
)
from digitalmodel.naval_architecture.kcs_geometry import (
    check_surface,
    enclosed_volume,
    wetted_surface_area,
)

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]


# --------------------------------------------------------------------------- #
#  Synthetic fixtures - exact arithmetic, no CAD dependency
# --------------------------------------------------------------------------- #

def _quad(a: Vec3, b: Vec3, c: Vec3, d: Vec3) -> List[Tri]:
    return [(a, b, c), (a, c, d)]


def _area(a: Vec3, b: Vec3, c: Vec3) -> float:
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    nx, ny, nz = uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx
    return 0.5 * math.sqrt(nx * nx + ny * ny + nz * nz)


def closed_box(length: float, beam: float, depth: float) -> List[Tri]:
    """A closed rectangular barge: keel at z=0, centreplane at y=0."""
    x0, x1 = 0.0, length
    y0, y1 = -beam / 2.0, beam / 2.0
    z0, z1 = 0.0, depth
    a, b = (x0, y0, z0), (x1, y0, z0)
    c, d = (x1, y1, z0), (x0, y1, z0)
    e, f = (x0, y0, z1), (x1, y0, z1)
    g, h = (x1, y1, z1), (x0, y1, z1)
    tris: List[Tri] = []
    tris += _quad(a, d, c, b)      # bottom
    tris += _quad(e, f, g, h)      # top (the lid)
    tris += _quad(a, b, f, e)      # side at y0
    tris += _quad(d, h, g, c)      # side at y1
    tris += _quad(a, e, h, d)      # end at x0
    tris += _quad(b, c, g, f)      # end at x1
    return tris


def lidless_box(length: float, beam: float, depth: float) -> List[Tri]:
    """``closed_box`` with its top face deleted: one planar rim at z=depth.

    The simplest possible instance of the client case -- a hull surface that
    stops at deck level.
    """
    tris = closed_box(length, beam, depth)
    lid = set(_quad((0.0, -beam / 2.0, depth), (length, -beam / 2.0, depth),
                    (length, beam / 2.0, depth), (0.0, beam / 2.0, depth)))
    return [t for t in tris if t not in lid]


def open_cylinder(
    radius: float, z_bottom: float, z_top: float, segments: int = 24
) -> List[Tri]:
    """A tube with no lids: TWO boundary loops, one at each end.

    Used to prove loop finding separates components rather than merging every
    open edge into one boundary, and to exercise the waterline rule with one
    loop above the plane and one below it.
    """
    tris: List[Tri] = []
    ring = [
        (
            radius * math.cos(2.0 * math.pi * i / segments),
            radius * math.sin(2.0 * math.pi * i / segments),
        )
        for i in range(segments)
    ]
    for i in range(segments):
        (x0, y0), (x1, y1) = ring[i], ring[(i + 1) % segments]
        tris += _quad(
            (x0, y0, z_bottom), (x1, y1, z_bottom),
            (x1, y1, z_top), (x0, y0, z_top),
        )
    return tris


#: Plan outline of a deck with a rectangular notch bitten out of its +x end --
#: a transom cutout, the shape that makes a centroid fan self-intersect. Listed
#: counter-clockwise. The extra vertices at x=80 on the long edges and at
#: y=+-8 on the x=0 edge exist so the bottom grid below meets the side walls
#: vertex-for-vertex; a T-junction there would manufacture open edges that have
#: nothing to do with the missing lid.
NOTCHED_PLAN: List[Tuple[float, float]] = [
    (0.0, -20.0), (80.0, -20.0), (100.0, -20.0), (100.0, -8.0),
    (80.0, -8.0), (80.0, 8.0), (100.0, 8.0), (100.0, 20.0),
    (80.0, 20.0), (0.0, 20.0), (0.0, 8.0), (0.0, -8.0),
]

#: Closed-form plan area of ``NOTCHED_PLAN``: 100 x 40 less a 20 x 16 notch.
NOTCHED_PLAN_AREA = 100.0 * 40.0 - 20.0 * 16.0

_GRID_X = (0.0, 80.0, 100.0)
_GRID_Y = (-20.0, -8.0, 8.0, 20.0)


def notched_tub(
    rim_z: float | Callable[[int], float] = 10.0,
) -> List[Tri]:
    """A flat-bottomed tub whose plan view is ``NOTCHED_PLAN`` and whose top
    is open.

    ``rim_z`` may be a constant (a planar rim, so the cap's area is known in
    closed form) or a function of the outline vertex index (a NON-PLANAR rim,
    which is what a real deck edge is).

    The bottom is a grid over ``_GRID_X`` x ``_GRID_Y`` with the notch cell
    removed, so every interior edge is shared by exactly two triangles and
    every boundary edge is covered by a side wall.
    """
    height = rim_z if callable(rim_z) else (lambda _i: float(rim_z))

    tris: List[Tri] = []
    for i in range(len(_GRID_X) - 1):
        for j in range(len(_GRID_Y) - 1):
            if i == 1 and j == 1:          # the notch: no bottom there
                continue
            x0, x1 = _GRID_X[i], _GRID_X[i + 1]
            y0, y1 = _GRID_Y[j], _GRID_Y[j + 1]
            tris += _quad(
                (x0, y0, 0.0), (x0, y1, 0.0), (x1, y1, 0.0), (x1, y0, 0.0)
            )

    n = len(NOTCHED_PLAN)
    for i in range(n):
        (x0, y0), (x1, y1) = NOTCHED_PLAN[i], NOTCHED_PLAN[(i + 1) % n]
        tris += _quad(
            (x0, y0, 0.0), (x1, y1, 0.0),
            (x1, y1, height((i + 1) % n)), (x0, y0, height(i)),
        )
    return [t for t in tris if _area(*t) > 0.0]


def pinched_box(length: float, beam: float, depth: float) -> List[Tri]:
    """A TORN surface: two holes meeting at a single shared vertex.

    Removing two triangles that share exactly one corner leaves an open-edge
    graph in which that corner has degree FOUR. Each hole is a triangle, so a
    loop finder that only walks edges would happily produce two "loops" and cap
    them; the surface is nonetheless torn at a point, and the honest response
    is to say so rather than to close it.

    A boundary vertex of a manifold sheet always has EVEN degree in the
    open-edge graph -- each surface sector at that vertex contributes two open
    edges -- so a pinch is the realistic tear, not a dangling edge.
    """
    tris = closed_box(length, beam, depth)
    x1 = length
    y0, y1 = -beam / 2.0, beam / 2.0
    a = (0.0, y0, 0.0)
    b = (x1, y0, 0.0)
    c = (x1, y1, 0.0)
    d = (0.0, y1, 0.0)
    f = (x1, y0, depth)
    torn = {(a, b, f), (a, d, c)}          # share only the corner ``a``
    kept = [t for t in tris if t not in torn]
    assert len(kept) == len(tris) - 2, "fixture no longer removes two triangles"
    return kept


# --------------------------------------------------------------------------- #
#  Finding the loops
# --------------------------------------------------------------------------- #

def test_lidless_box_has_one_boundary_loop_of_four_vertices() -> None:
    loops = find_boundary_loops(lidless_box(100.0, 20.0, 10.0))
    assert len(loops) == 1
    loop = loops[0]
    assert isinstance(loop, BoundaryLoop)
    assert len(loop.vertices) == 4
    assert loop.z_min == pytest.approx(10.0)
    assert loop.z_max == pytest.approx(10.0)
    # consecutive rim vertices must actually be adjacent on the box rim
    for p, q in zip(loop.vertices, loop.vertices[1:] + loop.vertices[:1]):
        assert p != q
        assert (p[0] == q[0]) or (p[1] == q[1])


def test_a_closed_surface_has_no_boundary_loops() -> None:
    assert find_boundary_loops(closed_box(100.0, 20.0, 10.0)) == []


def test_open_cylinder_yields_two_separate_loops() -> None:
    """Two components, not one boundary of 48 edges."""
    loops = find_boundary_loops(open_cylinder(5.0, -2.0, 8.0, segments=24))
    assert len(loops) == 2
    assert sorted(len(loop.vertices) for loop in loops) == [24, 24]
    zs = sorted(round(loop.z_min, 9) for loop in loops)
    assert zs == [-2.0, 8.0]


def test_loop_vertices_are_a_cycle_with_every_vertex_of_degree_two() -> None:
    loops = find_boundary_loops(notched_tub())
    assert len(loops) == 1
    verts = loops[0].vertices
    assert len(verts) == len(NOTCHED_PLAN)
    assert len(set(verts)) == len(verts), "a cycle visits each vertex once"


def test_a_torn_boundary_is_refused_rather_than_capped() -> None:
    """Capping a tear would hide a modelling defect behind a green verdict."""
    with pytest.raises(TornBoundaryError) as excinfo:
        find_boundary_loops(pinched_box(100.0, 20.0, 10.0))
    message = str(excinfo.value)
    assert "torn" in message.lower()
    assert "degree" in message.lower()


def test_capping_refuses_a_torn_surface_too() -> None:
    with pytest.raises(TornBoundaryError):
        cap_boundary_loops(pinched_box(100.0, 20.0, 10.0), waterline_z=5.0)


def test_torn_boundary_error_is_a_hull_ingest_error() -> None:
    """Callers already handle this stage's failures as one family."""
    assert issubclass(TornBoundaryError, HullIngestError)
    assert issubclass(CapBelowWaterlineError, HullIngestError)
    assert issubclass(CapTriangulationError, HullIngestError)


# --------------------------------------------------------------------------- #
#  Closure - gate 1
# --------------------------------------------------------------------------- #

def test_capping_closes_a_lidless_box() -> None:
    tris = lidless_box(100.0, 20.0, 10.0)
    before = check_surface(tris)
    assert before.open_edge_count == 4

    result = cap_boundary_loops(tris, waterline_z=5.0)
    after = check_surface(result.triangles)
    assert after.open_edge_count == 0
    assert after.nonmanifold_edge_count == 0
    assert after.degenerate_count == 0
    assert result.n_cap_triangles == len(result.triangles) - len(tris)


def test_capping_closes_both_ends_of_an_open_cylinder() -> None:
    tris = open_cylinder(5.0, 0.0, 8.0, segments=24)
    result = cap_boundary_loops(tris, waterline_z=-1.0)
    assert result.n_caps == 2
    after = check_surface(result.triangles)
    assert after.open_edge_count == 0
    assert after.nonmanifold_edge_count == 0
    # two 24-gon caps -> 22 triangles each
    assert result.n_cap_triangles == 44


def test_capping_closes_a_non_planar_concave_rim() -> None:
    """The real case: a deck edge that is neither flat nor convex in plan."""
    tris = notched_tub(rim_z=lambda i: 10.0 + 0.25 * math.sin(1.7 * i))
    result = cap_boundary_loops(tris, waterline_z=5.0)
    after = check_surface(result.triangles)
    assert after.open_edge_count == 0
    assert after.nonmanifold_edge_count == 0
    assert after.degenerate_count == 0
    assert result.loops[0].z_max - result.loops[0].z_min > 0.1


# --------------------------------------------------------------------------- #
#  The triangulation is valid, not merely closed - gate 4
# --------------------------------------------------------------------------- #

def test_cap_of_a_concave_outline_has_exactly_the_plan_area() -> None:
    """A centroid fan over this outline covers ground OUTSIDE the polygon.

    The notch is bitten out of the +x end, and the fan triangle from the
    centroid to the outline edge (80, 8)-(100, 8) crosses the notch. Its total
    area therefore EXCEEDS the polygon's. Ear clipping in the dominant plane
    partitions the polygon exactly, so the cap area equals the plan area to
    round-off -- which is the cheapest available proof that the triangulation
    does not self-intersect.
    """
    tris = notched_tub(rim_z=10.0)
    result = cap_boundary_loops(tris, waterline_z=5.0)
    assert result.cap_area == pytest.approx(NOTCHED_PLAN_AREA, rel=1e-12)
    assert result.n_cap_triangles == len(NOTCHED_PLAN) - 2


def test_a_centroid_fan_would_overshoot_the_same_outline() -> None:
    """Documents the failure the test above rules out, so it cannot be passed
    by an implementation that happens to fan from the centroid."""
    cx = sum(p[0] for p in NOTCHED_PLAN) / len(NOTCHED_PLAN)
    cy = sum(p[1] for p in NOTCHED_PLAN) / len(NOTCHED_PLAN)
    fan = 0.0
    n = len(NOTCHED_PLAN)
    for i in range(n):
        (x0, y0), (x1, y1) = NOTCHED_PLAN[i], NOTCHED_PLAN[(i + 1) % n]
        fan += _area((cx, cy, 0.0), (x0, y0, 0.0), (x1, y1, 0.0))
    assert fan > NOTCHED_PLAN_AREA * 1.01


def test_cap_area_is_comparable_to_the_loops_plan_extent() -> None:
    """An order-of-magnitude mismatch means a folded or degenerate cap."""
    result = cap_boundary_loops(notched_tub(rim_z=10.0), waterline_z=5.0)
    loop = result.loops[0]
    lo, hi = loop.bbox
    plan_extent = (hi[0] - lo[0]) * (hi[1] - lo[1])
    assert 0.1 * plan_extent < result.cap_area <= plan_extent * 1.000001


def test_every_cap_triangle_has_positive_area() -> None:
    """A zero-area cap triangle is invisible to a mesher and reopens the edge
    it was meant to close: ``check_surface`` skips degenerates when it counts
    edges, so the surface would read as leaky again."""
    result = cap_boundary_loops(
        notched_tub(rim_z=lambda i: 10.0 + 0.25 * math.sin(1.7 * i)),
        waterline_z=5.0,
    )
    for tri in result.cap_triangles:
        assert _area(*tri) > 0.0


def test_a_self_overlapping_plan_outline_is_refused() -> None:
    """Ear clipping is only valid on a SIMPLE polygon. A loop whose projection
    onto the dominant plane crosses itself cannot be capped by this method, and
    guessing would produce a lid that folds through the hull."""
    # An UNEQUAL bowtie: its two lobes do not cancel, so the loop still has a
    # net projected area and is not caught by the degenerate-loop guard. Its
    # first and third edges cross at (3.75, 3.75).
    bowtie = [
        (0.0, 0.0, 5.0), (10.0, 10.0, 5.0), (10.0, 0.0, 5.0), (0.0, 6.0, 5.0),
    ]
    loop = BoundaryLoop(vertices=bowtie)
    with pytest.raises(CapTriangulationError) as excinfo:
        triangulate_loop(loop)
    assert "self" in str(excinfo.value).lower()


# --------------------------------------------------------------------------- #
#  The waterline rule - the safety-critical one
# --------------------------------------------------------------------------- #

def test_a_loop_below_the_waterline_is_refused_by_default() -> None:
    """A cap below the waterline changes displacement and wetted area, which
    are exactly the quantities the analysis reports."""
    tris = lidless_box(100.0, 20.0, 10.0)
    with pytest.raises(CapBelowWaterlineError) as excinfo:
        cap_boundary_loops(tris, waterline_z=12.0)
    message = str(excinfo.value)
    assert "10" in message                       # the loop's z range is named
    assert "12" in message                       # and so is the waterline
    assert "displacement" in message.lower() or "wetted" in message.lower()


def test_a_loop_exactly_on_the_waterline_is_refused() -> None:
    """``wetted_surface_area`` counts a triangle whose vertices are all at or
    below the plane as fully wetted, so a lid AT the waterline is not free."""
    with pytest.raises(CapBelowWaterlineError):
        cap_boundary_loops(lidless_box(100.0, 20.0, 10.0), waterline_z=10.0)


def test_one_submerged_loop_refuses_the_whole_cap() -> None:
    """The cylinder's bottom rim is below the waterline and its top rim is
    above. Capping only the safe one would still leave the surface open, so
    the refusal has to cover the whole operation."""
    tris = open_cylinder(5.0, -2.0, 8.0, segments=16)
    with pytest.raises(CapBelowWaterlineError) as excinfo:
        cap_boundary_loops(tris, waterline_z=0.0)
    assert "-2" in str(excinfo.value)


def test_a_submerged_loop_can_be_capped_with_an_explicit_opt_in() -> None:
    tris = lidless_box(100.0, 20.0, 10.0)
    result = cap_boundary_loops(
        tris, waterline_z=12.0, allow_below_waterline=True
    )
    assert check_surface(result.triangles).open_edge_count == 0
    assert result.below_waterline is True


def test_the_opt_in_is_off_by_default() -> None:
    signature = inspect.signature(cap_boundary_loops)
    assert signature.parameters["allow_below_waterline"].default is False


# --------------------------------------------------------------------------- #
#  Hydrostatics are unchanged by a cap above the waterline - gate 2 and 3
# --------------------------------------------------------------------------- #

def test_a_cap_above_the_waterline_changes_no_hydrostatic_quantity() -> None:
    tris = lidless_box(100.0, 20.0, 10.0)
    waterline = 5.0
    area_before = wetted_surface_area(tris, waterline)
    volume_before = enclosed_volume(tris, waterline)

    result = cap_boundary_loops(tris, waterline_z=waterline)
    area_after = wetted_surface_area(result.triangles, waterline)
    volume_after = enclosed_volume(result.triangles, waterline)

    assert area_after == pytest.approx(area_before, rel=1e-12)
    assert volume_after == pytest.approx(volume_before, rel=1e-12)
    # and both agree with the closed form for a box
    assert volume_after == pytest.approx(100.0 * 20.0 * 5.0)
    assert area_after == pytest.approx(
        100.0 * 20.0 + 2 * 100.0 * 5.0 + 2 * 20.0 * 5.0
    )


def test_the_closed_body_encloses_more_than_its_submerged_part() -> None:
    """Sanity: a capped hull holds more than the part of it under water."""
    tris = lidless_box(100.0, 20.0, 10.0)
    result = cap_boundary_loops(tris, waterline_z=5.0)
    submerged = enclosed_volume(result.triangles, 5.0)
    whole = enclosed_volume(result.triangles, 10.0)
    assert whole > submerged > 0.0
    assert whole == pytest.approx(100.0 * 20.0 * 10.0)


# --------------------------------------------------------------------------- #
#  Wiring into the ingestion stage - opt-in, never silent
# --------------------------------------------------------------------------- #

def test_ingest_still_refuses_an_open_hull_without_the_flag(tmp_path: Path) -> None:
    """Capping is an additional route to watertightness, not a relaxation of
    the gate. The default path must keep refusing."""
    with pytest.raises(NotWatertightError):
        ingest_triangles(
            lidless_box(100.0, 20.0, 10.0),
            tmp_path / "case",
            source_name="synthetic.3dm",
            units_in="m",
            forward="+x",
            draft_m=5.0,
        )
    assert not (tmp_path / "case" / "hull_manifest.json").exists()


def test_cap_flag_defaults_to_false() -> None:
    from digitalmodel.naval_architecture import hull_ingest

    for func in (hull_ingest.ingest_triangles, hull_ingest.ingest_3dm):
        signature = inspect.signature(func)
        assert signature.parameters["cap_open_boundaries"].default is False
        assert signature.parameters["cap_below_waterline"].default is False


def test_ingest_with_the_cap_flag_produces_a_watertight_hull(tmp_path: Path) -> None:
    manifest = ingest_triangles(
        lidless_box(100.0, 20.0, 10.0),
        tmp_path / "case",
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
        cap_open_boundaries=True,
    )
    assert manifest.watertight is True
    assert manifest.open_edge_count == 0
    assert manifest.nonmanifold_edge_count == 0
    assert manifest.displacement_m3 == pytest.approx(100.0 * 20.0 * 5.0)
    assert (tmp_path / "case" / "hull.stl").exists()


def test_the_manifest_records_that_capping_happened(tmp_path: Path) -> None:
    """A hull that was silently closed for you is a hull whose displacement
    you cannot audit."""
    manifest = ingest_triangles(
        lidless_box(100.0, 20.0, 10.0),
        tmp_path / "case",
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
        cap_open_boundaries=True,
    )
    provenance = manifest.to_dict()["provenance"]
    assert provenance["capped"] is True
    assert provenance["cap_triangles_added"] == 2
    assert provenance["cap_area_m2"] == pytest.approx(100.0 * 20.0)
    loops = provenance["cap_loops"]
    assert len(loops) == 1
    assert loops[0]["n_vertices"] == 4
    assert loops[0]["n_triangles"] == 2
    assert loops[0]["z_min_m"] == pytest.approx(10.0)
    assert loops[0]["z_max_m"] == pytest.approx(10.0)
    assert any("cap" in note.lower() for note in manifest.notes)


def test_an_uncapped_ingest_records_no_capping(tmp_path: Path) -> None:
    manifest = ingest_triangles(
        closed_box(100.0, 20.0, 10.0),
        tmp_path / "case",
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
    )
    provenance = manifest.to_dict()["provenance"]
    assert provenance["capped"] is False
    assert provenance["cap_loops"] == []
    assert provenance["cap_triangles_added"] == 0


def test_ingest_refuses_to_cap_a_hull_that_would_be_capped_underwater(
    tmp_path: Path,
) -> None:
    """The rim sits at 10 m; the caller asked for a 12 m draft. Closing that
    hull adds displacement the manifest would then report as the client's."""
    with pytest.raises(CapBelowWaterlineError):
        ingest_triangles(
            lidless_box(100.0, 20.0, 10.0),
            tmp_path / "case",
            source_name="synthetic.3dm",
            units_in="m",
            forward="+x",
            draft_m=12.0,
            cap_open_boundaries=True,
        )
    assert not (tmp_path / "case" / "hull_manifest.json").exists()


def test_ingest_capping_needs_a_draft(tmp_path: Path) -> None:
    """Without a draft the stage places the waterline at the top of the hull
    and treats it as fully submerged, so no cap can be above water."""
    with pytest.raises(HullIngestError) as excinfo:
        ingest_triangles(
            lidless_box(100.0, 20.0, 10.0),
            tmp_path / "case",
            source_name="synthetic.3dm",
            units_in="m",
            forward="+x",
            cap_open_boundaries=True,
        )
    assert "draft" in str(excinfo.value).lower()


def test_ingest_capping_leaves_the_hydrostatics_alone(tmp_path: Path) -> None:
    """The manifest of a capped hull must report the same displacement and
    wetted area as the uncapped surface it came from."""
    open_hull = lidless_box(100.0, 20.0, 10.0)
    forced = ingest_triangles(
        open_hull,
        tmp_path / "open",
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
        force=True,
    )
    capped = ingest_triangles(
        open_hull,
        tmp_path / "capped",
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
        cap_open_boundaries=True,
    )
    assert forced.watertight is False
    assert capped.watertight is True
    assert capped.displacement_m3 == pytest.approx(
        forced.displacement_m3, rel=1e-12
    )
    assert capped.wetted_surface_m2 == pytest.approx(
        forced.wetted_surface_m2, rel=1e-12
    )
    assert capped.lpp_m == pytest.approx(forced.lpp_m)
    assert capped.beam_m == pytest.approx(forced.beam_m)


def test_capping_a_torn_hull_refuses_at_ingest(tmp_path: Path) -> None:
    with pytest.raises(TornBoundaryError):
        ingest_triangles(
            pinched_box(100.0, 20.0, 10.0),
            tmp_path / "case",
            source_name="synthetic.3dm",
            units_in="m",
            forward="+x",
            draft_m=5.0,
            cap_open_boundaries=True,
        )


# --------------------------------------------------------------------------- #
#  The real hull - opt-in, skipped unless a path is supplied out of band
# --------------------------------------------------------------------------- #

_REAL_3DM = os.environ.get("DIGITALMODEL_3DM_HULL", "")

requires_real_3dm = pytest.mark.skipif(
    not _REAL_3DM or not Path(_REAL_3DM).is_file(),
    reason="set DIGITALMODEL_3DM_HULL to a readable .3dm hull to run this",
)


@requires_real_3dm
def test_real_open_hull_caps_to_a_watertight_stl(tmp_path: Path) -> None:
    """The acceptance gate on a genuine client hull.

    Asserted as: closure after capping, hydrostatics unchanged by the cap to a
    tight tolerance, and a closed-body volume larger than the submerged one.
    """
    pytest.importorskip("rhino3dm", reason="needs the 'cad' extra")
    from digitalmodel.naval_architecture.hull_ingest import ingest_3dm

    layers = os.environ.get("DIGITALMODEL_3DM_LAYERS", "Hull")
    draft = float(os.environ.get("DIGITALMODEL_3DM_DRAFT_M", "10.0"))

    forced = ingest_3dm(
        Path(_REAL_3DM),
        tmp_path / "open",
        layers=layers.split(","),
        draft_m=draft,
        tessellate_breps=True,
        force=True,
    )
    capped = ingest_3dm(
        Path(_REAL_3DM),
        tmp_path / "capped",
        layers=layers.split(","),
        draft_m=draft,
        tessellate_breps=True,
        cap_open_boundaries=True,
    )

    assert forced.watertight is False
    assert capped.watertight is True
    assert capped.open_edge_count == 0
    assert capped.nonmanifold_edge_count == 0
    assert capped.displacement_m3 == pytest.approx(
        forced.displacement_m3, rel=1e-9
    )
    assert capped.wetted_surface_m2 == pytest.approx(
        forced.wetted_surface_m2, rel=1e-9
    )
    assert capped.cap_triangles_added > 0
    assert capped.cap_loops and capped.cap_loops[0]["z_min_m"] > draft
    assert (tmp_path / "capped" / "hull.stl").exists()
