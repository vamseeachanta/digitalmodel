"""
ABOUTME: Tests for ``naval_architecture.brep_tessellate`` (#2023) -- the trimmed
NURBS Brep -> watertight triangle-soup converter built on the OpenCASCADE
kernel.

WHAT THESE TESTS ARE ACTUALLY GUARDING
--------------------------------------
One failure mode, and it is not "the code crashes". A converter that ignores
the trim boundaries and grid-evaluates each surface over its full parameter
domain produces a mesh that LOOKS fine -- plausible triangle count, plausible
shape -- and is the wrong SIZE. On the file that motivated this module the
untrimmed evaluation overshot the true half-beam by 3.9 m and the true depth by
1.6 m, then reported a displaced volume of 13 cubic metres for a 158 m ship.
It would have meshed, solved, and returned a confident wrong resistance.

So the tests below are bounding-box tests before they are anything else:
``test_trimmed_patch_stays_inside_its_boundary`` and its companion
``test_untrimmed_surface_would_overshoot_the_trim`` are the miniature form of
the acceptance gate, expressed on a synthetic patch so they run in CI with no
client file present.

Everything here is synthetic. The real hull that motivated the module is
covered by ``test_real_3dm_hull_passes_the_acceptance_gate``, which reads a
path from an environment variable and SKIPS when it is unset, so no client
geometry and no client identifier ever enters this repository.
"""

from __future__ import annotations

import math
import os
from pathlib import Path

import pytest

from digitalmodel.naval_architecture import brep_tessellate as bt

rhino3dm = pytest.importorskip("rhino3dm", reason="needs the 'cad' extra")

occ_required = pytest.mark.skipif(
    not bt.has_cad_kernel(),
    reason="needs the OpenCASCADE kernel from the 'cad' extra (cadquery-ocp)",
)


# --------------------------------------------------------------------------- #
#  Synthetic geometry -- no client file, no fixture on disk
# --------------------------------------------------------------------------- #

def _ruled_patch() -> "rhino3dm.NurbsSurface":
    """A degree (3, 1) ruled surface, the shape of most faces in a hull Brep."""
    lower = rhino3dm.NurbsCurve.CreateControlPointCurve(
        [
            rhino3dm.Point3d(0.0, 0.0, 0.0),
            rhino3dm.Point3d(3.0, 2.0, 0.0),
            rhino3dm.Point3d(7.0, -1.0, 0.0),
            rhino3dm.Point3d(10.0, 0.0, 0.0),
        ],
        3,
    )
    upper = rhino3dm.NurbsCurve.CreateControlPointCurve(
        [
            rhino3dm.Point3d(0.0, 0.0, 5.0),
            rhino3dm.Point3d(3.0, -2.0, 5.0),
            rhino3dm.Point3d(7.0, 3.0, 5.0),
            rhino3dm.Point3d(10.0, 0.0, 5.0),
        ],
        3,
    )
    return rhino3dm.NurbsSurface.CreateRuledSurface(lower, upper)


def _sphere_patch() -> "rhino3dm.NurbsSurface":
    """A RATIONAL surface. Weights are a separate array in OpenCASCADE and
    dropping them silently deforms the surface, so one test carries them."""
    return rhino3dm.NurbsSurface.CreateFromSphere(
        rhino3dm.Sphere(rhino3dm.Point3d(1.0, -2.0, 0.5), 2.0)
    )


def _sub_rectangle_edges(surface, u0, u1, v0, v1):
    """The four 3D boundary curves of a parametric sub-rectangle of ``surface``.

    This is the whole point of the module in miniature: these curves are the
    only thing that says where the face stops. There is no 2D trim curve
    available from a ``.3dm`` read through ``rhino3dm``, so the face has to be
    recovered from 3D edges alone.
    """
    bottom = surface.IsoCurve(0, v0).Trim(u0, u1)
    right = surface.IsoCurve(1, u1).Trim(v0, v1)
    top = surface.IsoCurve(0, v1).Trim(u0, u1)
    left = surface.IsoCurve(1, u0).Trim(v0, v1)
    return [c.ToNurbsCurve() for c in (bottom, right, top, left)]


def _bbox(triangles):
    xs = [p[0] for t in triangles for p in t]
    ys = [p[1] for t in triangles for p in t]
    zs = [p[2] for t in triangles for p in t]
    return (min(xs), min(ys), min(zs)), (max(xs), max(ys), max(zs))


def _surface_bbox(surface, u0, u1, v0, v1, n=60):
    lo = [math.inf] * 3
    hi = [-math.inf] * 3
    for i in range(n + 1):
        for j in range(n + 1):
            p = surface.PointAt(u0 + (u1 - u0) * i / n, v0 + (v1 - v0) * j / n)
            for k, value in enumerate((p.X, p.Y, p.Z)):
                lo[k] = min(lo[k], value)
                hi[k] = max(hi[k], value)
    return tuple(lo), tuple(hi)


# --------------------------------------------------------------------------- #
#  Knot conversion -- pure, runs without the kernel
# --------------------------------------------------------------------------- #

def test_knot_vector_restores_the_two_knots_opennurbs_omits():
    """openNURBS stores ``n + degree - 1`` knots; OpenCASCADE wants the full
    clamped vector expressed as values plus multiplicities.

    Getting this wrong does not raise -- it produces a surface of the right
    degree with the wrong parameterisation, which then fails to match the
    trim edges by a small amount. So it is asserted directly.
    """
    values, mults = bt.knot_vector_to_occ([0.0, 0.0, 0.0, 4.0, 4.0, 4.0], 3)
    assert values == [0.0, 4.0]
    assert mults == [4, 4]
    assert sum(mults) == 4 + 3 + 1  # cv_count + degree + 1


def test_knot_vector_keeps_interior_knots_and_their_multiplicity():
    values, mults = bt.knot_vector_to_occ(
        [0.0, 0.0, 0.0, 1.0, 2.0, 2.0, 3.0, 3.0, 3.0], 3
    )
    assert values == [0.0, 1.0, 2.0, 3.0]
    assert mults == [4, 1, 2, 4]


def test_missing_kernel_names_the_extra_that_fixes_it(monkeypatch):
    """The core package must import without the kernel, and the error a caller
    sees must name the install that resolves it rather than surfacing a bare
    ``ModuleNotFoundError`` from a transitive import."""
    monkeypatch.setattr(bt, "_OCC_AVAILABLE", False)
    monkeypatch.setattr(bt, "_OCC_IMPORT_ERROR", ImportError("no OCP"))
    with pytest.raises(bt.MissingCadKernelError) as excinfo:
        bt.require_cad_kernel()
    message = str(excinfo.value)
    assert bt.CAD_EXTRA in message
    assert "cadquery-ocp" in message


def test_missing_kernel_error_is_catchable_as_a_hull_ingest_error():
    """Callers already handle ``hull_ingest`` failures as one family; a new
    module must not force a second ``except`` clause on them."""
    from digitalmodel.naval_architecture import hull_ingest

    assert issubclass(bt.MissingCadKernelError, hull_ingest.MissingCadDependencyError)
    assert issubclass(bt.BrepTessellationError, hull_ingest.HullIngestError)


# --------------------------------------------------------------------------- #
#  Geometry transfer fidelity
# --------------------------------------------------------------------------- #

@occ_required
def test_surface_transfer_reproduces_rhino_evaluation():
    surface = _ruled_patch()
    occ = bt.nurbs_surface_to_occ(surface)
    du, dv = surface.Domain(0), surface.Domain(1)
    worst = 0.0
    for i in range(11):
        for j in range(11):
            u = du.T0 + (du.T1 - du.T0) * i / 10
            v = dv.T0 + (dv.T1 - dv.T0) * j / 10
            expected = surface.PointAt(u, v)
            got = occ.Value(u, v)
            worst = max(
                worst,
                math.dist(
                    (expected.X, expected.Y, expected.Z),
                    (got.X(), got.Y(), got.Z()),
                ),
            )
    assert worst < 1e-9


@occ_required
def test_rational_surface_transfer_keeps_its_weights():
    """A sphere is exactly representable only as a RATIONAL NURBS. If the
    weights are dropped the transferred surface is still smooth and still
    closed -- it is simply no longer a sphere, by about 8% of the radius.
    """
    surface = _sphere_patch()
    assert surface.IsRational
    occ = bt.nurbs_surface_to_occ(surface)
    assert occ.IsURational() or occ.IsVRational()
    centre = (1.0, -2.0, 0.5)
    du, dv = surface.Domain(0), surface.Domain(1)
    for i in range(9):
        for j in range(9):
            u = du.T0 + (du.T1 - du.T0) * i / 8
            v = dv.T0 + (dv.T1 - dv.T0) * j / 8
            got = occ.Value(u, v)
            assert math.dist((got.X(), got.Y(), got.Z()), centre) == pytest.approx(
                2.0, abs=1e-9
            )


@occ_required
def test_curve_transfer_reproduces_rhino_evaluation():
    curve = rhino3dm.NurbsCurve.CreateControlPointCurve(
        [
            rhino3dm.Point3d(0.0, 0.0, 0.0),
            rhino3dm.Point3d(1.0, 4.0, 2.0),
            rhino3dm.Point3d(5.0, -1.0, 3.0),
            rhino3dm.Point3d(6.0, 0.0, 0.0),
        ],
        3,
    )
    occ = bt.nurbs_curve_to_occ(curve)
    domain = curve.Domain
    for i in range(21):
        t = domain.T0 + (domain.T1 - domain.T0) * i / 20
        expected = curve.PointAt(t)
        got = occ.Value(t)
        assert math.dist(
            (expected.X, expected.Y, expected.Z), (got.X(), got.Y(), got.Z())
        ) < 1e-9


@occ_required
def test_rational_curve_transfer_keeps_its_weights():
    """The curve path shares the homogeneous-coordinate convention with the
    surface path and had the same bug. A circle is the cheapest witness: read
    the control points without dividing by the weight and it stays a smooth
    closed loop, just no longer a circle.
    """
    curve = rhino3dm.NurbsCurve.CreateFromCircle(rhino3dm.Circle(3.0))
    assert curve.IsRational
    occ = bt.nurbs_curve_to_occ(curve)
    first, last = occ.FirstParameter(), occ.LastParameter()
    for i in range(25):
        point = occ.Value(first + (last - first) * i / 24)
        assert math.hypot(point.X(), point.Y()) == pytest.approx(3.0, abs=1e-9)
        assert point.Z() == pytest.approx(0.0, abs=1e-9)


# --------------------------------------------------------------------------- #
#  The trim gate, in miniature
# --------------------------------------------------------------------------- #

@occ_required
def test_trimmed_patch_stays_inside_its_boundary():
    """The face is bounded by 3D edges covering u in [0.15, 0.55] only. Its
    mesh must fill that region and stop there.

    OpenCASCADE recovers the 2D p-curves by projecting those 3D edges onto the
    surface; if that projection is skipped or fails, the mesher falls back to
    the surface's natural bounds and the patch silently becomes the whole
    surface. That is exactly the bug this module exists to avoid, so it is
    asserted on the bounding box rather than on the triangle count.
    """
    surface = _ruled_patch()
    u0, u1, v0, v1 = 0.15, 0.55, 0.2, 0.8
    edges = _sub_rectangle_edges(surface, u0, u1, v0, v1)

    result = bt.tessellate_patches(
        surfaces=[surface],
        edge_curves=edges,
        face_trims=[[(0, False), (1, False), (2, True), (3, True)]],
        linear_deflection=1e-3,
    )

    assert result.faces_converted == 1
    assert result.triangles
    lo, hi = _bbox(result.triangles)
    want_lo, want_hi = _surface_bbox(surface, u0, u1, v0, v1)
    for axis in range(3):
        assert lo[axis] == pytest.approx(want_lo[axis], abs=5e-3)
        assert hi[axis] == pytest.approx(want_hi[axis], abs=5e-3)


@occ_required
def test_untrimmed_surface_would_overshoot_the_trim():
    """Documents the failure mode the previous attempt hit, so the assertion
    above cannot be satisfied by a converter that ignores trims."""
    surface = _ruled_patch()
    u0, u1, v0, v1 = 0.15, 0.55, 0.2, 0.8
    trimmed_lo, trimmed_hi = _surface_bbox(surface, u0, u1, v0, v1)
    full_lo, full_hi = _surface_bbox(surface, 0.0, 1.0, 0.0, 1.0)
    assert full_hi[0] - full_lo[0] > (trimmed_hi[0] - trimmed_lo[0]) + 1.0

    edges = _sub_rectangle_edges(surface, u0, u1, v0, v1)
    result = bt.tessellate_patches(
        surfaces=[surface],
        edge_curves=edges,
        face_trims=[[(0, False), (1, False), (2, True), (3, True)]],
        linear_deflection=1e-3,
    )
    lo, hi = _bbox(result.triangles)
    assert hi[0] < full_hi[0] - 1.0


@occ_required
def test_trimmed_patch_on_a_curved_surface():
    """The p-curve projection is trivial on a plane and not on a doubly curved
    surface. The two hardest faces of the motivating hull carry 123 edges each
    on a degree (3, 3) surface, so the curved path is the one that matters."""
    surface = _sphere_patch()
    u0, u1, v0, v1 = 0.4, 1.6, 0.5, 1.2
    edges = _sub_rectangle_edges(surface, u0, u1, v0, v1)
    result = bt.tessellate_patches(
        surfaces=[surface],
        edge_curves=edges,
        face_trims=[[(0, False), (1, False), (2, True), (3, True)]],
        linear_deflection=1e-3,
    )
    assert result.faces_converted == 1
    assert result.faces_failed == []
    lo, hi = _bbox(result.triangles)
    want_lo, want_hi = _surface_bbox(surface, u0, u1, v0, v1)
    for axis in range(3):
        assert lo[axis] == pytest.approx(want_lo[axis], abs=1e-2)
        assert hi[axis] == pytest.approx(want_hi[axis], abs=1e-2)
    # every vertex still on the sphere
    for tri in result.triangles:
        for p in tri:
            assert math.dist(p, (1.0, -2.0, 0.5)) == pytest.approx(2.0, abs=2e-3)


# --------------------------------------------------------------------------- #
#  Sewing -- the seam between two faces must be conformal, not merely close
# --------------------------------------------------------------------------- #

@occ_required
def test_adjacent_patches_share_their_seam_after_sewing():
    """Two faces built from the SAME edge index must come out of the mesher
    sharing vertices along that edge.

    If they do not, every seam in a 244-face hull contributes a row of open
    edges and the surface reads as leaky even though the Brep is not. The
    previous attempt left 4174 open edges this way.
    """
    surface = _ruled_patch()
    left_edges = _sub_rectangle_edges(surface, 0.1, 0.5, 0.2, 0.8)
    right_edges = _sub_rectangle_edges(surface, 0.5, 0.9, 0.2, 0.8)
    # left[1] (u = 0.5) and right[3] (u = 0.5) are the same curve: share it.
    curves = [left_edges[0], left_edges[1], left_edges[2], left_edges[3],
              right_edges[0], right_edges[2], right_edges[1]]
    face_trims = [
        [(0, False), (1, False), (2, True), (3, True)],
        [(4, False), (6, False), (5, True), (1, True)],
    ]
    result = bt.tessellate_patches(
        surfaces=[surface, surface],
        edge_curves=curves,
        face_trims=face_trims,
        linear_deflection=1e-3,
    )
    assert result.faces_converted == 2

    from digitalmodel.naval_architecture.hull_ingest import weld_vertices
    from digitalmodel.naval_architecture.kcs_geometry import check_surface

    check = check_surface(weld_vertices(result.triangles, 1e-9))
    assert check.nonmanifold_edge_count == 0
    # A flat-ish strip: every open edge belongs to the outer perimeter, and the
    # shared seam contributes none. Interior leakage would push this far up.
    assert check.open_edge_count < check.triangle_count // 2
    assert result.free_edge_count > 0  # the strip is an open sheet, as expected


# --------------------------------------------------------------------------- #
#  Fidelity reporting
# --------------------------------------------------------------------------- #

@occ_required
def test_fidelity_reports_overshoot_against_a_reference_box():
    surface = _ruled_patch()
    edges = _sub_rectangle_edges(surface, 0.15, 0.55, 0.2, 0.8)
    result = bt.tessellate_patches(
        surfaces=[surface],
        edge_curves=edges,
        face_trims=[[(0, False), (1, False), (2, True), (3, True)]],
        linear_deflection=1e-3,
    )
    fidelity = result.fidelity
    assert fidelity is not None
    # The trim box is built from the same 3D edges that bound the face, so the
    # mesh may bulge slightly outside it but must not miss it.
    assert fidelity.max_trim_deviation < 5e-3
    assert fidelity.max_overshoot == 0.0
    assert fidelity.within_reference_box


# --------------------------------------------------------------------------- #
#  hull_ingest wiring -- opt-in only
# --------------------------------------------------------------------------- #

def test_hull_ingest_does_not_tessellate_breps_by_default():
    """The fallback must never fire silently. A file with no cached meshes has
    to keep raising ``NoMeshGeometryError`` unless the caller asked for the
    kernel path explicitly, because tessellating a Brep is a modelling
    decision (deflection, trim recovery) and not a format detail."""
    import inspect

    from digitalmodel.naval_architecture import hull_ingest

    signature = inspect.signature(hull_ingest.ingest_3dm)
    assert signature.parameters["tessellate_breps"].default is False
    signature = inspect.signature(hull_ingest.read_3dm_triangles)
    assert signature.parameters["tessellate_breps"].default is False


def test_no_mesh_error_points_at_the_kernel_fallback():
    """The error a user hits first should name every fix, including this one."""
    from digitalmodel.naval_architecture import hull_ingest

    assert "tessellate_breps" in hull_ingest.NO_MESH_REMEDIES


# --------------------------------------------------------------------------- #
#  The real hull -- skipped unless a path is supplied out of band
# --------------------------------------------------------------------------- #

@occ_required
@pytest.mark.skipif(
    not os.environ.get("DIGITALMODEL_3DM_HULL"),
    reason="set DIGITALMODEL_3DM_HULL to a .3dm hull to run the acceptance gate",
)
def test_real_3dm_hull_passes_the_acceptance_gate():
    from digitalmodel.naval_architecture.hull_ingest import weld_vertices
    from digitalmodel.naval_architecture.kcs_geometry import (
        check_surface,
        enclosed_volume,
        wetted_surface_area,
    )

    source = Path(os.environ["DIGITALMODEL_3DM_HULL"])
    layers = os.environ.get("DIGITALMODEL_3DM_LAYERS")
    result = bt.tessellate_3dm(
        source, layers=layers.split(",") if layers else None
    )

    scale = {"mm": 0.001, "cm": 0.01, "m": 1.0}[result.declared_units or "m"]
    tris = [tuple(tuple(c * scale for c in p) for p in t) for t in result.triangles]

    # Gate 1 -- no overshoot beyond the Brep's own bounding box.
    assert result.fidelity.max_overshoot * scale < 0.01
    assert result.fidelity.max_trim_deviation * scale < 0.01

    # Gate 2 -- closure.
    check = check_surface(weld_vertices(tris, 1e-6))
    assert check.nonmanifold_edge_count == 0
    assert check.degenerate_count == 0
    assert check.open_edge_count < 500  # a boundary ring, not scattered leakage

    # Gate 3 -- physical sanity at 10 m draft.
    keel = min(p[2] for t in tris for p in t)
    waterline = keel + 10.0
    volume = enclosed_volume(tris, waterline)
    area = wetted_surface_area(tris, waterline)
    below_x = [p[0] for t in tris for p in t if p[2] <= waterline]
    below_y = [p[1] for t in tris for p in t if p[2] <= waterline]
    lwl = max(below_x) - min(below_x)
    beam = max(below_y) - min(below_y)
    block = volume / (lwl * beam * 10.0)
    assert 10_000.0 < volume < 100_000.0
    assert 0.55 < block < 0.92
    assert area > 1_000.0
