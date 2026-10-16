"""
ABOUTME: Trimmed-NURBS Brep tessellation for client CAD hulls (#2023). Rebuilds a
Rhino ``.3dm`` Brep inside the OpenCASCADE kernel and meshes it, so a hull that
carries no cached render meshes can still reach ``hull_ingest`` as a watertight
triangle soup.

WHY THIS MODULE EXISTS
----------------------
``hull_ingest`` reads the render meshes Rhino cached in a ``.3dm``. When a file
carries none -- which is normal for a model saved without shading, or saved
"geometry only" -- it refuses, and that refusal is correct: ``rhino3dm`` is a
file reader with no tessellator. This module supplies the missing kernel rather
than weakening that refusal.

THE ONE FAILURE MODE WORTH DESIGNING AGAINST
--------------------------------------------
The obvious shortcut is to grid-evaluate each NURBS surface over its full
parameter domain. It is a dozen lines, it needs no kernel, and it is wrong,
because a Brep face is a REGION of its surface and the surface routinely
extends far outside it. On the file that motivated this module, two faces are
degree (3, 3) sheets spanning the entire hull, each cut down by 123 trims. Grid
evaluation of those two produced:

    axis   Brep bounding box        grid evaluation      error
    y      -14.55 ..  15.08 m       -14.55 ..  19.00 m   +3.92 m
    z       -0.02 ..  14.82 m        -0.01 ..  16.37 m   +1.55 m

and, after welding, 4174 open edges and a displaced volume of 13 cubic metres
for a 158 m ship. Every one of those numbers is discoverable in seconds. None
of them is visible in a rendering of the mesh, which is the danger: the case
meshes, the solver converges, and the resistance is confidently wrong.

So the bounding box is the acceptance criterion, not the triangle count, and
``TrimFidelity`` on every result exists to make it impossible to ship the mesh
without looking at it.

HOW THE TRIM IS RECOVERED WITHOUT TRIM CURVES
---------------------------------------------
``rhino3dm``'s ``BrepTrim`` exposes only ``EdgeIndex``, ``StartVertexIndex``,
``EndVertexIndex`` and ``IsReversed``. There is no 2D trim curve and no trim
domain -- so the parameter-space boundary that Rhino stores is simply not
readable through this binding, and looking harder does not help.

What IS readable is the 3D edge each trim points at: ``Brep.Edges`` are real
curves. That is enough, because OpenCASCADE can go the other way. Assemble the
face's boundary wire from those 3D edges in trim order, honouring
``IsReversed``, hand it to ``BRepBuilderAPI_MakeFace`` together with the
surface, and then project each edge onto the surface with
``ShapeFix_Edge::FixAddPCurve`` to synthesise the p-curve Rhino would not give
us. The trim is recovered rather than transferred.

Sharing one ``TopoDS_Edge`` between the two faces that use it is deliberate and
is what makes the seams conformal: OpenCASCADE stores one p-curve per (edge,
face) pair on a single edge, ``BRepMesh`` discretises each edge once, and both
faces then receive identical nodes along it. Sewing afterwards has almost
nothing left to do -- on the motivating hull it reported exactly the five free
edges the Brep itself has, and the meshed soup had zero non-manifold edges at
a weld tolerance of zero.

WHAT THIS MODULE DOES NOT DO
----------------------------
It does not scale, orient, translate, close or gate the hull. All of that is
``hull_ingest``'s job and duplicating it here would create a second, divergent
opinion about what a normalised hull is. Triangles come out in FILE units
alongside the declared unit system, exactly like ``read_3dm_triangles``.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

from digitalmodel.naval_architecture.hull_ingest import (
    CAD_EXTRA,
    HullIngestError,
    MissingCadDependencyError,
)

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]
Box = Tuple[Vec3, Vec3]

__all__ = [
    "CAD_EXTRA",
    "DEFAULT_ANGULAR_DEFLECTION_RAD",
    "DEFLECTION_FRACTION_OF_DIAGONAL",
    "SEWING_FRACTION_OF_DIAGONAL",
    "BrepTessellation",
    "BrepTessellationError",
    "MissingCadKernelError",
    "NoBrepGeometryError",
    "TrimFidelity",
    "has_cad_kernel",
    "knot_vector_to_occ",
    "nurbs_curve_to_occ",
    "nurbs_surface_to_occ",
    "require_cad_kernel",
    "tessellate_3dm",
    "tessellate_brep",
    "tessellate_patches",
]

#: Chordal deflection of the tessellation, as a fraction of the model's
#: bounding-box diagonal. 2e-4 puts a 160 m hull at about 32 mm, which is
#: comfortably finer than any snappyHexMesh surface cell it will feed and still
#: cheap. Override per call when the geometry is small or unusually fine.
DEFLECTION_FRACTION_OF_DIAGONAL = 2.0e-4

#: Angular deflection in radians. Controls tessellation of tight curvature --
#: bilge radii and bulb noses -- independently of the chordal limit.
DEFAULT_ANGULAR_DEFLECTION_RAD = 0.3

#: Sewing tolerance, as a fraction of the diagonal. Deliberately tiny: edges
#: are SHARED between faces by construction here, so sewing is a consistency
#: check rather than a repair. A large tolerance would let it weld faces that
#: genuinely do not meet and hide a real modelling defect.
SEWING_FRACTION_OF_DIAGONAL = 1.0e-7

#: Tolerance handed to the p-curve projection, as a fraction of the diagonal.
PCURVE_FRACTION_OF_DIAGONAL = 1.0e-8


# --------------------------------------------------------------------------- #
#  Errors
# --------------------------------------------------------------------------- #

class BrepTessellationError(HullIngestError):
    """A Brep could not be rebuilt or meshed."""


class MissingCadKernelError(MissingCadDependencyError):
    """The OpenCASCADE kernel is not installed.

    Subclasses ``MissingCadDependencyError`` on purpose: a caller that already
    handles "the CAD extra is missing" should not need a second branch.
    """


class NoBrepGeometryError(BrepTessellationError):
    """The file, or the selected layers, hold no Brep this converter can use."""


# --------------------------------------------------------------------------- #
#  Lazy kernel import
# --------------------------------------------------------------------------- #

_OCC: Dict[str, object] = {}
_OCC_AVAILABLE = False
_OCC_IMPORT_ERROR: Optional[BaseException] = None


def _load_occ() -> None:
    """Import the OpenCASCADE bindings once, without making them a hard
    dependency of the package.

    ``cadquery-ocp`` publishes manylinux/macos/windows wheels of OCCT 7.9 under
    the module name ``OCP``, which is why it is the binding this module targets.

    ``pythonocc-core`` wraps the SAME kernel as ``OCC.Core`` and is deliberately
    NOT accepted as a drop-in here. Its Python surface differs in ways this
    module touches -- ``topods.Edge`` versus ``TopoDS.Edge_s``,
    ``BRep_Tool.Triangulation`` versus ``Triangulation_s`` -- so a "try the
    other spelling" fallback would import cleanly and then fail at the first
    call with an AttributeError that reads like a kernel bug. Supporting it
    means a tested shim, not a second string in a loop.
    """
    global _OCC_AVAILABLE, _OCC_IMPORT_ERROR
    if _OCC_AVAILABLE or _OCC_IMPORT_ERROR is not None:
        return
    try:
        import importlib  # noqa: PLC0415

        mods = {
            name: importlib.import_module(f"OCP.{name}")
            for name in (
                "gp", "TColgp", "TColStd", "Geom", "BRep", "BRepBuilderAPI",
                "BRepMesh", "BRepTools", "TopoDS", "TopAbs", "TopExp",
                "TopLoc", "ShapeFix",
            )
        }
    except ImportError as exc:
        _OCC_IMPORT_ERROR = exc
        return
    _OCC.clear()
    _OCC.update(mods)
    _OCC_AVAILABLE = True
    _OCC_IMPORT_ERROR = None


def has_cad_kernel() -> bool:
    """True when an OpenCASCADE binding is importable."""
    _load_occ()
    return _OCC_AVAILABLE


def require_cad_kernel() -> None:
    """Raise a message that names the install, not a transitive ImportError."""
    _load_occ()
    if _OCC_AVAILABLE:
        return
    raise MissingCadKernelError(
        "Tessellating trimmed NURBS Breps requires an OpenCASCADE binding, "
        "which is not installed.\n"
        f"    pip install 'digitalmodel[{CAD_EXTRA}]'\n"
        "    uv pip install cadquery-ocp\n"
        "cadquery-ocp ships binary wheels of OpenCASCADE 7.9 as the module "
        "'OCP'. pythonocc-core wraps the same kernel but spells its API "
        "differently and is not a drop-in for this module.\n"
        "Everything else in digitalmodel works without it. If the hull was "
        "saved from Rhino with render meshes, hull_ingest needs no kernel at "
        "all."
    ) from _OCC_IMPORT_ERROR


# --------------------------------------------------------------------------- #
#  openNURBS -> OpenCASCADE geometry transfer
# --------------------------------------------------------------------------- #

def knot_vector_to_occ(
    knots: Sequence[float], degree: int, *, relative_tol: float = 1e-12
) -> Tuple[List[float], List[int]]:
    """Convert an openNURBS knot vector to OpenCASCADE knots + multiplicities.

    openNURBS stores ``cv_count + degree - 1`` knots, omitting the first and
    last of the ``cv_count + degree + 1`` knots of the standard clamped vector
    because they are never referenced by the basis functions. OpenCASCADE wants
    the standard vector, expressed as distinct values with multiplicities.

    Restoring the two omitted knots by duplicating the ends is exact for a
    clamped vector, which is what every openNURBS surface and curve carries
    unless it is periodic; periodic geometry is rejected upstream rather than
    silently mis-parameterised here.
    """
    if len(knots) < 2:
        raise BrepTessellationError(
            f"knot vector of length {len(knots)} cannot describe a degree "
            f"{degree} span"
        )
    full = [knots[0], *knots, knots[-1]]
    values: List[float] = []
    mults: List[int] = []
    for knot in full:
        if values and abs(knot - values[-1]) <= relative_tol * max(1.0, abs(knot)):
            mults[-1] += 1
        else:
            values.append(float(knot))
            mults.append(1)
    return values, mults


def nurbs_surface_to_occ(surface):
    """Transfer a ``rhino3dm.NurbsSurface`` into a ``Geom_BSplineSurface``.

    Control points come back from ``rhino3dm`` as ``Point4d`` in HOMOGENEOUS
    coordinates -- ``(x*w, y*w, z*w, w)`` -- while OpenCASCADE's poles array
    wants Euclidean poles with the weights in a separate array. The divide by
    ``w`` is therefore mandatory and is the reason ``_dehomogenise`` exists
    rather than a direct copy.

    This is worth stating because the convention is invisible on ordinary
    geometry: every weight of a non-rational surface is 1.0, so the wrong
    reading is exactly right on the 244 non-rational faces of the hull that
    motivated this module and silently wrong on any arc, fillet, circle or
    sphere. Checked against a rational sphere, where the un-divided reading
    yields a smooth, closed, plausible surface of radius 1.80 instead of 2.00.
    """
    require_cad_kernel()
    TColgp = _OCC["TColgp"]
    TColStd = _OCC["TColStd"]
    gp = _OCC["gp"]
    Geom = _OCC["Geom"]

    if surface.IsPeriodic(0) or surface.IsPeriodic(1):
        raise BrepTessellationError(
            "periodic NURBS surfaces are not supported by this transfer; "
            "split the surface at its seam in Rhino before export"
        )

    nu, nv = surface.Points.CountU, surface.Points.CountV
    degree_u, degree_v = surface.Degree(0), surface.Degree(1)
    rational = bool(surface.IsRational)

    poles = TColgp.TColgp_Array2OfPnt(1, nu, 1, nv)
    weights = TColStd.TColStd_Array2OfReal(1, nu, 1, nv) if rational else None
    for i in range(nu):
        for j in range(nv):
            x, y, z, w = _dehomogenise(surface.Points.GetControlPoint(i, j))
            poles.SetValue(i + 1, j + 1, gp.gp_Pnt(x, y, z))
            if weights is not None:
                weights.SetValue(i + 1, j + 1, w)

    uk, um = knot_vector_to_occ(surface.KnotsU.ToList(), degree_u)
    vk, vm = knot_vector_to_occ(surface.KnotsV.ToList(), degree_v)
    _check_knot_budget(uk, um, nu, degree_u, "U")
    _check_knot_budget(vk, vm, nv, degree_v, "V")

    UK = TColStd.TColStd_Array1OfReal(1, len(uk))
    UM = TColStd.TColStd_Array1OfInteger(1, len(um))
    VK = TColStd.TColStd_Array1OfReal(1, len(vk))
    VM = TColStd.TColStd_Array1OfInteger(1, len(vm))
    for i, (k, m) in enumerate(zip(uk, um)):
        UK.SetValue(i + 1, k)
        UM.SetValue(i + 1, m)
    for i, (k, m) in enumerate(zip(vk, vm)):
        VK.SetValue(i + 1, k)
        VM.SetValue(i + 1, m)

    if weights is None:
        return Geom.Geom_BSplineSurface(
            poles, UK, VK, UM, VM, degree_u, degree_v, False, False
        )
    return Geom.Geom_BSplineSurface(
        poles, weights, UK, VK, UM, VM, degree_u, degree_v, False, False
    )


def nurbs_curve_to_occ(curve):
    """Transfer a ``rhino3dm.NurbsCurve`` into a ``Geom_BSplineCurve``."""
    require_cad_kernel()
    TColgp = _OCC["TColgp"]
    TColStd = _OCC["TColStd"]
    gp = _OCC["gp"]
    Geom = _OCC["Geom"]

    # NOTE: ``IsPeriodic`` is a PROPERTY on rhino3dm's Curve and a METHOD on
    # its Surface. Calling the property returns a bool and then calls it.
    if bool(curve.IsPeriodic):
        raise BrepTessellationError(
            "periodic NURBS curves are not supported by this transfer"
        )

    n = len(curve.Points)
    degree = curve.Degree
    rational = bool(curve.IsRational)
    poles = TColgp.TColgp_Array1OfPnt(1, n)
    weights = TColStd.TColStd_Array1OfReal(1, n) if rational else None
    for i in range(n):
        x, y, z, w = _dehomogenise(curve.Points[i])
        poles.SetValue(i + 1, gp.gp_Pnt(x, y, z))
        if weights is not None:
            weights.SetValue(i + 1, w)

    values, mults = knot_vector_to_occ(curve.Knots.ToList(), degree)
    _check_knot_budget(values, mults, n, degree, "curve")
    K = TColStd.TColStd_Array1OfReal(1, len(values))
    M = TColStd.TColStd_Array1OfInteger(1, len(mults))
    for i, (k, m) in enumerate(zip(values, mults)):
        K.SetValue(i + 1, k)
        M.SetValue(i + 1, m)

    if weights is None:
        return Geom.Geom_BSplineCurve(poles, K, M, degree, False)
    return Geom.Geom_BSplineCurve(poles, weights, K, M, degree, False)


def _dehomogenise(point) -> Tuple[float, float, float, float]:
    """``rhino3dm.Point4d`` (homogeneous) -> Euclidean pole plus its weight."""
    w = point.W
    if w == 0.0:
        raise BrepTessellationError(
            "control point carries a zero weight, which has no Euclidean image"
        )
    return point.X / w, point.Y / w, point.Z / w, w


def _check_knot_budget(values, mults, n_poles, degree, label) -> None:
    """A wrong knot count is the one transfer error that does not raise.

    OpenCASCADE would accept a plausible-but-wrong vector and hand back a
    surface of the right degree with the wrong parameterisation, which then
    misses the trim edges by a small, hard-to-attribute amount. So the identity
    ``sum(multiplicities) == poles + degree + 1`` is asserted before the
    constructor sees it.
    """
    want = n_poles + degree + 1
    got = sum(mults)
    if got != want:
        raise BrepTessellationError(
            f"{label} knot vector does not match the control net: "
            f"{got} knots for {n_poles} poles at degree {degree} "
            f"(expected {want}); the surface would be mis-parameterised"
        )


# --------------------------------------------------------------------------- #
#  Results
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class TrimFidelity:
    """Did the trims actually take effect? The one question worth asking.

    Two references, because neither alone is sufficient:

    ``reference_bbox``
        Rhino's own ``Brep.GetBoundingBox()``. It is an UPPER bound, not a
        tight box -- Rhino unions per-face boxes derived from the untrimmed
        surfaces, so a heavily trimmed face contributes far more than it
        occupies. Useful in exactly one direction: any mesh point outside it
        proves a trim was ignored. That is what caught the earlier attempt.

    ``trim_bbox``
        The box of the 3D trim edges themselves, sampled. Those edges ARE the
        face boundaries, so this is tight to within the amount a trimmed
        surface interior bulges past its own boundary -- millimetres on a hull.
        This is the reference that catches a mesh which is too SMALL, which the
        upper bound cannot see.
    """

    mesh_bbox: Box
    reference_bbox: Optional[Box]
    trim_bbox: Optional[Box]
    overshoot: Dict[str, float] = field(default_factory=dict)
    trim_deviation: Dict[str, float] = field(default_factory=dict)

    @property
    def max_overshoot(self) -> float:
        """Furthest the mesh reaches outside the reference box. Must be ~0."""
        return max(self.overshoot.values(), default=0.0)

    @property
    def max_trim_deviation(self) -> float:
        """Largest disagreement with the trim-edge box, either direction."""
        return max(self.trim_deviation.values(), default=0.0)

    @property
    def within_reference_box(self) -> bool:
        return self.max_overshoot == 0.0

    def describe(self) -> str:
        lines = []
        for axis in ("x", "y", "z"):
            i = "xyz".index(axis)
            row = f"  {axis}  mesh {self.mesh_bbox[0][i]:14.4f} .. {self.mesh_bbox[1][i]:14.4f}"
            if self.trim_bbox is not None:
                row += (
                    f"   trim {self.trim_bbox[0][i]:14.4f} .. "
                    f"{self.trim_bbox[1][i]:14.4f}"
                )
            if self.reference_bbox is not None:
                row += (
                    f"   brep {self.reference_bbox[0][i]:14.4f} .. "
                    f"{self.reference_bbox[1][i]:14.4f}"
                )
            lines.append(row)
        lines.append(
            f"  max overshoot beyond brep box {self.max_overshoot:.6g}; "
            f"max deviation from trim box {self.max_trim_deviation:.6g}"
        )
        return "\n".join(lines)


@dataclass(frozen=True)
class BrepTessellation:
    """A meshed Brep plus everything needed to decide whether to trust it.

    ``triangles`` are in FILE units. Scaling, orientation and origin belong to
    ``hull_ingest``; producing them here would create a second opinion about
    what a normalised hull is.
    """

    triangles: List[Tri]
    declared_units: Optional[str] = None
    face_count: int = 0
    faces_converted: int = 0
    faces_failed: List[Tuple[int, str]] = field(default_factory=list)
    edge_count: int = 0
    free_edge_count: int = 0
    degenerate_shape_count: int = 0
    multiple_edge_count: int = 0
    singular_trims_skipped: int = 0
    linear_deflection: float = 0.0
    angular_deflection: float = 0.0
    layer_faces: Dict[str, int] = field(default_factory=dict)
    layers_available: List[str] = field(default_factory=list)
    fidelity: Optional[TrimFidelity] = None

    @property
    def triangle_count(self) -> int:
        return len(self.triangles)


# --------------------------------------------------------------------------- #
#  The rebuild
# --------------------------------------------------------------------------- #

def tessellate_patches(
    *,
    surfaces: Sequence[object],
    edge_curves: Sequence[object],
    face_trims: Sequence[Sequence[Tuple[int, bool]]],
    inner_loops: Optional[Sequence[Sequence[Sequence[Tuple[int, bool]]]]] = None,
    face_reversed: Optional[Sequence[bool]] = None,
    linear_deflection: Optional[float] = None,
    angular_deflection: float = DEFAULT_ANGULAR_DEFLECTION_RAD,
    sewing_tolerance: Optional[float] = None,
    reference_bbox: Optional[Box] = None,
    declared_units: Optional[str] = None,
) -> BrepTessellation:
    """Rebuild and mesh a Brep given its surfaces, 3D edges and trim order.

    Kept separate from :func:`tessellate_brep` so the whole kernel path is
    testable from synthetic geometry -- a ruled patch and four iso-curves
    exercise exactly the same code as a 244-face hull, and CI needs no client
    file to prove the trim is honoured.

    ``face_trims[i]`` is the outer loop of face ``i`` as ``(edge_index,
    is_reversed)`` pairs IN TRIM ORDER. Order matters: the wire builder chains
    edges end to end, and a shuffled loop produces a face that fails rather
    than one that is subtly wrong -- which is the desired direction of failure.
    An ``edge_index`` below zero marks a singular trim (a degenerate pole) and
    is skipped, since it contributes no length to the boundary.
    """
    require_cad_kernel()
    BRepBuilderAPI = _OCC["BRepBuilderAPI"]
    TopoDS = _OCC["TopoDS"]
    TopAbs = _OCC["TopAbs"]
    TopExp = _OCC["TopExp"]
    ShapeFix = _OCC["ShapeFix"]

    if len(surfaces) != len(face_trims):
        raise BrepTessellationError(
            f"{len(surfaces)} surfaces but {len(face_trims)} trim loops"
        )

    occ_curves = [nurbs_curve_to_occ(c) for c in edge_curves]
    trim_box = _curve_bbox(occ_curves)
    diagonal = _diagonal(trim_box) or 1.0
    if linear_deflection is None:
        linear_deflection = max(diagonal * DEFLECTION_FRACTION_OF_DIAGONAL, 1e-9)
    if sewing_tolerance is None:
        sewing_tolerance = max(diagonal * SEWING_FRACTION_OF_DIAGONAL, 1e-12)
    pcurve_tolerance = max(diagonal * PCURVE_FRACTION_OF_DIAGONAL, 1e-12)

    # ONE TopoDS_Edge per Brep edge, shared by both faces that use it. This is
    # what makes the seams conformal after meshing -- see the module docstring.
    edges = [
        BRepBuilderAPI.BRepBuilderAPI_MakeEdge(c).Edge() for c in occ_curves
    ]

    fixer = ShapeFix.ShapeFix_Edge()
    faces = []
    failures: List[Tuple[int, str]] = []
    singular_skipped = 0

    def build_wire(loop) -> Tuple[object, Optional[str], int]:
        maker = BRepBuilderAPI.BRepBuilderAPI_MakeWire()
        skipped = 0
        for edge_index, reversed_ in loop:
            if edge_index < 0:
                skipped += 1
                continue
            if edge_index >= len(edges):
                return None, f"trim references edge {edge_index} of {len(edges)}", skipped
            orientation = TopAbs.TopAbs_REVERSED if reversed_ else TopAbs.TopAbs_FORWARD
            maker.Add(TopoDS.TopoDS.Edge_s(edges[edge_index].Oriented(orientation)))
        if not maker.IsDone():
            return None, f"wire not closed (MakeWire error {maker.Error()})", skipped
        return maker.Wire(), None, skipped

    for index, (surface, loop) in enumerate(zip(surfaces, face_trims)):
        try:
            occ_surface = nurbs_surface_to_occ(surface)
        except BrepTessellationError as exc:
            failures.append((index, str(exc)))
            continue
        wire, error, skipped = build_wire(loop)
        singular_skipped += skipped
        if wire is None:
            failures.append((index, error))
            continue
        maker = BRepBuilderAPI.BRepBuilderAPI_MakeFace(occ_surface, wire, False)
        if inner_loops is not None and index < len(inner_loops):
            for hole in inner_loops[index]:
                hole_wire, hole_error, hole_skipped = build_wire(hole)
                singular_skipped += hole_skipped
                if hole_wire is None:
                    failures.append((index, f"inner loop: {hole_error}"))
                    continue
                maker.Add(hole_wire)
        if not maker.IsDone():
            failures.append((index, f"MakeFace error {maker.Error()}"))
            continue
        face = maker.Face()

        # Synthesise the p-curves Rhino would not give us, by projecting each
        # boundary edge onto the surface. Without this the face carries a wire
        # with no parameter-space image and the mesher falls back to the
        # surface's NATURAL bounds -- i.e. silently un-trims the face.
        explorer = TopExp.TopExp_Explorer(face, TopAbs.TopAbs_EDGE)
        while explorer.More():
            fixer.FixAddPCurve(
                TopoDS.TopoDS.Edge_s(explorer.Current()),
                face,
                False,
                pcurve_tolerance,
            )
            explorer.Next()

        if face_reversed is not None and index < len(face_reversed) and face_reversed[index]:
            face = TopoDS.TopoDS.Face_s(face.Reversed())
        faces.append(face)

    if not faces:
        raise BrepTessellationError(
            "no face of the Brep could be rebuilt in the kernel; "
            f"{len(failures)} failures, first: "
            f"{failures[0][1] if failures else 'unknown'}"
        )

    sewing = BRepBuilderAPI.BRepBuilderAPI_Sewing(
        sewing_tolerance, True, True, True, False
    )
    for face in faces:
        sewing.Add(face)
    sewing.Perform()
    shape = sewing.SewedShape()

    triangles = _mesh_shape(shape, linear_deflection, angular_deflection)
    if not triangles:
        raise BrepTessellationError(
            "the rebuilt shape produced no triangles; the mesher was given a "
            f"chordal deflection of {linear_deflection:g} in file units"
        )

    mesh_box = _triangle_bbox(triangles)
    return BrepTessellation(
        triangles=triangles,
        declared_units=declared_units,
        face_count=len(surfaces),
        faces_converted=len(faces),
        faces_failed=failures,
        edge_count=len(edges),
        free_edge_count=sewing.NbFreeEdges(),
        degenerate_shape_count=sewing.NbDegeneratedShapes(),
        multiple_edge_count=sewing.NbMultipleEdges(),
        singular_trims_skipped=singular_skipped,
        linear_deflection=linear_deflection,
        angular_deflection=angular_deflection,
        fidelity=_fidelity(mesh_box, reference_bbox, trim_box),
    )


def _mesh_shape(shape, linear_deflection: float, angular_deflection: float) -> List[Tri]:
    BRepMesh = _OCC["BRepMesh"]
    BRep = _OCC["BRep"]
    TopoDS = _OCC["TopoDS"]
    TopAbs = _OCC["TopAbs"]
    TopExp = _OCC["TopExp"]
    TopLoc = _OCC["TopLoc"]

    BRepMesh.BRepMesh_IncrementalMesh(
        shape, linear_deflection, False, angular_deflection, True
    )

    triangles: List[Tri] = []
    explorer = TopExp.TopExp_Explorer(shape, TopAbs.TopAbs_FACE)
    while explorer.More():
        face = TopoDS.TopoDS.Face_s(explorer.Current())
        location = TopLoc.TopLoc_Location()
        triangulation = BRep.BRep_Tool.Triangulation_s(face, location)
        if triangulation is None:
            explorer.Next()
            continue
        transform = location.Transformation()
        reversed_ = face.Orientation() == TopAbs.TopAbs_REVERSED
        nodes = [
            triangulation.Node(i).Transformed(transform)
            for i in range(1, triangulation.NbNodes() + 1)
        ]
        for i in range(1, triangulation.NbTriangles() + 1):
            a, b, c = triangulation.Triangle(i).Get()
            if reversed_:
                a, c = c, a
            triangles.append(
                tuple(
                    (nodes[k - 1].X(), nodes[k - 1].Y(), nodes[k - 1].Z())
                    for k in (a, b, c)
                )
            )
        explorer.Next()
    return triangles


# --------------------------------------------------------------------------- #
#  Bounding boxes
# --------------------------------------------------------------------------- #

def _triangle_bbox(triangles: Sequence[Tri]) -> Box:
    xs = [p[0] for t in triangles for p in t]
    ys = [p[1] for t in triangles for p in t]
    zs = [p[2] for t in triangles for p in t]
    return (min(xs), min(ys), min(zs)), (max(xs), max(ys), max(zs))


def _curve_bbox(curves, samples: int = 33) -> Optional[Box]:
    """Box of the trim edges, sampled. Tight to within the surface's bulge."""
    if not curves:
        return None
    lo = [math.inf] * 3
    hi = [-math.inf] * 3
    for curve in curves:
        first, last = curve.FirstParameter(), curve.LastParameter()
        for i in range(samples + 1):
            point = curve.Value(first + (last - first) * i / samples)
            for k, value in enumerate((point.X(), point.Y(), point.Z())):
                lo[k] = min(lo[k], value)
                hi[k] = max(hi[k], value)
    return tuple(lo), tuple(hi)


def _diagonal(box: Optional[Box]) -> float:
    if box is None:
        return 0.0
    lo, hi = box
    return math.dist(lo, hi)


def _fidelity(
    mesh_box: Box, reference: Optional[Box], trim: Optional[Box]
) -> TrimFidelity:
    overshoot: Dict[str, float] = {}
    deviation: Dict[str, float] = {}
    for i, axis in enumerate("xyz"):
        if reference is not None:
            overshoot[axis] = max(
                0.0,
                reference[0][i] - mesh_box[0][i],
                mesh_box[1][i] - reference[1][i],
            )
        if trim is not None:
            deviation[axis] = max(
                abs(mesh_box[0][i] - trim[0][i]), abs(mesh_box[1][i] - trim[1][i])
            )
    return TrimFidelity(
        mesh_bbox=mesh_box,
        reference_bbox=reference,
        trim_bbox=trim,
        overshoot=overshoot,
        trim_deviation=deviation,
    )


# --------------------------------------------------------------------------- #
#  rhino3dm entry points
# --------------------------------------------------------------------------- #

def _brep_parts(brep):
    """Pull surfaces, 3D edges and trim order out of a ``rhino3dm.Brep``."""
    surfaces = []
    outer: List[List[Tuple[int, bool]]] = []
    inner: List[List[List[Tuple[int, bool]]]] = []
    reversed_flags: List[bool] = []

    for i in range(len(brep.Faces)):
        face = brep.Faces[i]
        surfaces.append(face.ToNurbsSurface())
        reversed_flags.append(bool(face.OrientationIsReversed))
        loops = list(face.Loops)
        outer_loop = None
        holes: List[List[Tuple[int, bool]]] = []
        for loop in loops:
            pairs = [(t.EdgeIndex, bool(t.IsReversed)) for t in loop.Trims]
            if outer_loop is None and str(loop.LoopType).endswith("Outer"):
                outer_loop = pairs
            else:
                holes.append(pairs)
        if outer_loop is None:  # no loop declared Outer: take the first
            outer_loop = holes.pop(0) if holes else []
        outer.append(outer_loop)
        inner.append(holes)

    curves = [brep.Edges[j].ToNurbsCurve() for j in range(len(brep.Edges))]
    return surfaces, curves, outer, inner, reversed_flags


def tessellate_brep(
    brep,
    *,
    linear_deflection: Optional[float] = None,
    angular_deflection: float = DEFAULT_ANGULAR_DEFLECTION_RAD,
    sewing_tolerance: Optional[float] = None,
    declared_units: Optional[str] = None,
) -> BrepTessellation:
    """Mesh one ``rhino3dm.Brep``. Triangles come back in FILE units."""
    surfaces, curves, outer, inner, reversed_flags = _brep_parts(brep)
    if not surfaces:
        raise NoBrepGeometryError("the Brep has no faces")
    return tessellate_patches(
        surfaces=surfaces,
        edge_curves=curves,
        face_trims=outer,
        inner_loops=inner,
        face_reversed=reversed_flags,
        linear_deflection=linear_deflection,
        angular_deflection=angular_deflection,
        sewing_tolerance=sewing_tolerance,
        reference_bbox=_rhino_bbox(brep),
        declared_units=declared_units,
    )


def _rhino_bbox(geometry) -> Optional[Box]:
    try:
        box = geometry.GetBoundingBox()
    except Exception:  # pragma: no cover - defensive
        return None
    if box is None:
        return None
    return (
        (box.Min.X, box.Min.Y, box.Min.Z),
        (box.Max.X, box.Max.Y, box.Max.Z),
    )


def _union(a: Optional[Box], b: Optional[Box]) -> Optional[Box]:
    if a is None:
        return b
    if b is None:
        return a
    return (
        tuple(min(a[0][i], b[0][i]) for i in range(3)),
        tuple(max(a[1][i], b[1][i]) for i in range(3)),
    )


def tessellate_3dm(
    path: Path | str,
    *,
    layers: Optional[Iterable[str]] = None,
    linear_deflection: Optional[float] = None,
    angular_deflection: float = DEFAULT_ANGULAR_DEFLECTION_RAD,
    sewing_tolerance: Optional[float] = None,
) -> BrepTessellation:
    """Tessellate every Brep on the selected layers of a ``.3dm``.

    ``layers`` is not optional in spirit. A client file routinely carries the
    hull alongside a rudder, a shaft boss, appendages and traced 2D drawings,
    and meshing all of it produces a "hull" that is not one. The available
    layer names are reported on the result so the second call can be right.
    """
    require_cad_kernel()
    from digitalmodel.naval_architecture.hull_ingest import (  # noqa: PLC0415
        RHINO_UNIT_CODES,
        _import_rhino3dm,
        _normalise_layer_filter,
    )

    rhino3dm = _import_rhino3dm()
    source = Path(path)
    if not source.is_file():
        raise BrepTessellationError(f"no such file: {source}")
    model = rhino3dm.File3dm.Read(str(source))
    if model is None:
        raise BrepTessellationError(f"rhino3dm could not read {source.name}")

    try:
        declared = RHINO_UNIT_CODES.get(int(model.Settings.ModelUnitSystem))
    except Exception:  # pragma: no cover - older files may lack settings
        declared = None

    layer_names = {i: layer.Name for i, layer in enumerate(model.Layers)}
    available = [layer.Name for layer in model.Layers]
    wanted = _normalise_layer_filter(layers)

    breps: List[Tuple[str, object]] = []
    for obj in model.Objects:
        try:
            name = layer_names.get(obj.Attributes.LayerIndex, "")
        except Exception:  # pragma: no cover
            name = ""
        if wanted is not None and name.strip().lower() not in wanted:
            continue
        geometry = obj.Geometry
        kind = type(geometry).__name__
        if kind == "Extrusion":
            try:
                geometry = geometry.ToBrep(True)
                kind = "Brep"
            except Exception:  # pragma: no cover
                continue
        if kind == "Brep":
            breps.append((name, geometry))

    if not breps:
        raise NoBrepGeometryError(
            f"{source.name} holds no Brep on the selected layers.\n"
            f"    layers requested: {sorted(wanted) if wanted else 'all'}\n"
            f"    layers available: {available}"
        )

    surfaces: List[object] = []
    curves: List[object] = []
    outer: List[List[Tuple[int, bool]]] = []
    inner: List[List[List[Tuple[int, bool]]]] = []
    flags: List[bool] = []
    layer_faces: Dict[str, int] = {}
    reference: Optional[Box] = None

    # Several Breps on one layer are rebuilt into ONE shell so that sewing can
    # close the seams between them; edge indices are rebased per Brep.
    for name, brep in breps:
        s, c, o, i, f = _brep_parts(brep)
        offset = len(curves)
        surfaces.extend(s)
        curves.extend(c)
        outer.extend(
            [[(e + offset if e >= 0 else e, r) for e, r in loop] for loop in o]
        )
        inner.extend(
            [
                [[(e + offset if e >= 0 else e, r) for e, r in hole] for hole in holes]
                for holes in i
            ]
        )
        flags.extend(f)
        layer_faces[name] = layer_faces.get(name, 0) + len(s)
        reference = _union(reference, _rhino_bbox(brep))

    result = tessellate_patches(
        surfaces=surfaces,
        edge_curves=curves,
        face_trims=outer,
        inner_loops=inner,
        face_reversed=flags,
        linear_deflection=linear_deflection,
        angular_deflection=angular_deflection,
        sewing_tolerance=sewing_tolerance,
        reference_bbox=reference,
        declared_units=declared,
    )
    return BrepTessellation(
        triangles=result.triangles,
        declared_units=declared,
        face_count=result.face_count,
        faces_converted=result.faces_converted,
        faces_failed=result.faces_failed,
        edge_count=result.edge_count,
        free_edge_count=result.free_edge_count,
        degenerate_shape_count=result.degenerate_shape_count,
        multiple_edge_count=result.multiple_edge_count,
        singular_trims_skipped=result.singular_trims_skipped,
        linear_deflection=result.linear_deflection,
        angular_deflection=result.angular_deflection,
        layer_faces=layer_faces,
        layers_available=available,
        fidelity=result.fidelity,
    )
