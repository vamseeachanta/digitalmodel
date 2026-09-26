"""Build metre-coordinate hull BReps with the optional bundled OpenCascade bindings."""

from __future__ import annotations

from contextlib import contextmanager, redirect_stdout
from io import StringIO
from pathlib import Path
from threading import RLock

import numpy as np

from .mesh_generator import _shape_preserving_interp
from .profile_schema import HullProfile

# OCCT STEP units and messenger settings are process-global.
_OCP_LOCK = RLock()


def profile_point_grid(profile: HullProfile, n_x=None, n_z=None) -> np.ndarray:
    """Resample starboard offsets in marine coordinates (metres), keel first."""
    stations = sorted(profile.stations, key=lambda station: station.x_position)
    n_x = max(41, len(stations)) if n_x is None else n_x
    n_z = (
        max(11, max(len(s.waterline_offsets) for s in stations)) if n_z is None else n_z
    )
    for count in (n_x, n_z):
        if (
            isinstance(count, bool)
            or not isinstance(count, (int, np.integer))
            or count < 2
        ):
            raise ValueError("grid counts must be integers >= 2")
    x = np.linspace(0, profile.length_bp, n_x)
    z = np.linspace(-profile.draft, 0, n_z)
    station_x = np.array([s.x_position for s in stations])
    station_y = []
    for station in stations:
        offsets = np.array(sorted(station.waterline_offsets))
        station_y.append(
            _shape_preserving_interp(
                offsets[:, 0],
                offsets[:, 1],
                z + profile.draft,
                (offsets[0, 1], offsets[-1, 1]),
            )
        )
    station_y = np.asarray(station_y)
    points = np.empty((n_x, n_z, 3))
    points[:, :, 0] = x[:, None]
    points[:, :, 2] = z[None, :]
    for j in range(n_z):
        points[:, j, 1] = np.maximum(
            _shape_preserving_interp(station_x, station_y[:, j], x, (0, 0)), 0
        )
    return _validate_grid(points)


def _validate_grid(points):
    points = np.asarray(points, dtype=float)
    if points.ndim != 3 or points.shape[2] != 3 or min(points.shape[:2]) < 2:
        raise ValueError("points must be a grid of shape (n_x >= 2, n_z >= 2, 3)")
    if not np.isfinite(points).all():
        raise ValueError("grid coordinates must be finite")
    return points


def bspline_face_from_grid(points, *, deg_min=3, deg_max=8, tol=1e-6):
    """Fit a C2 B-spline face; all coordinates and tolerances are metres."""
    points = _validate_grid(points)
    if not 1 <= deg_min <= deg_max <= 25 or not np.isfinite(tol) or tol <= 0:
        raise ValueError("invalid B-spline degrees or tolerance")
    from OCP.BRepBuilderAPI import BRepBuilderAPI_MakeFace
    from OCP.GeomAbs import GeomAbs_C2
    from OCP.GeomAPI import GeomAPI_PointsToBSplineSurface
    from OCP.gp import gp_Pnt
    from OCP.TColgp import TColgp_Array2OfPnt

    nx, nz, _ = points.shape
    array = TColgp_Array2OfPnt(1, nx, 1, nz)
    for i in range(nx):
        for j in range(nz):
            array.SetValue(i + 1, j + 1, gp_Pnt(*points[i, j]))
    fit = GeomAPI_PointsToBSplineSurface(array, deg_min, deg_max, GeomAbs_C2, tol)
    if not fit.IsDone():
        raise RuntimeError("OpenCascade B-spline fitting failed")
    builder = BRepBuilderAPI_MakeFace(fit.Surface(), tol)
    if not builder.IsDone():
        raise RuntimeError("OpenCascade face construction failed")
    return builder.Face()


def _sew(*shapes):
    from OCP.BRepBuilderAPI import BRepBuilderAPI_Sewing

    sewing = BRepBuilderAPI_Sewing(1e-4)
    for shape in shapes:
        sewing.Add(shape)
    sewing.Perform()
    result = sewing.SewedShape()
    if result.IsNull():
        raise RuntimeError("OpenCascade sewing produced an empty shape")
    return result


def mirror_and_sew(face, plane="y"):
    """Mirror about an origin plane normal to x, y or z and sew both halves."""
    if plane not in ("x", "y", "z"):
        raise ValueError("plane must be x, y or z")
    from OCP.BRepBuilderAPI import BRepBuilderAPI_Transform
    from OCP.gp import gp_Ax2, gp_Dir, gp_Pnt, gp_Trsf

    normal = [int(axis == plane) for axis in ("x", "y", "z")]
    transform = gp_Trsf()
    transform.SetMirror(gp_Ax2(gp_Pnt(0, 0, 0), gp_Dir(*normal)))
    mirrored = BRepBuilderAPI_Transform(face, transform, True).Shape()
    return _sew(face, mirrored)


def flat_bottom_face(points):
    """Planar half-bottom matching sampled keel offsets; None for zero area.

    The polygon follows the sampled keel edge, as the panel generator does.
    It adds no end caps or waterplane.
    """
    points = _validate_grid(points)
    edge = points[:, 0, :]
    if not np.allclose(edge[:, 2], edge[0, 2], rtol=0, atol=1e-8):
        raise ValueError("keel edge must be planar at constant z")
    if np.any(edge[:, 1] < 0) or np.any(np.diff(edge[:, 0]) <= 0):
        raise ValueError("keel edge must be starboard with increasing x")
    if np.all(edge[:, 1] <= 1e-10):
        return None
    from OCP.BRepBuilderAPI import BRepBuilderAPI_MakeFace, BRepBuilderAPI_MakePolygon
    from OCP.gp import gp_Pnt

    polygon = BRepBuilderAPI_MakePolygon()
    vertices = [*edge, [edge[-1, 0], 0, edge[-1, 2]], [edge[0, 0], 0, edge[0, 2]]]
    previous = None
    for point in vertices:
        if previous is None or not np.allclose(point, previous, rtol=0, atol=1e-10):
            polygon.Add(gp_Pnt(*point))
            previous = point
    polygon.Close()
    builder = BRepBuilderAPI_MakeFace(polygon.Wire(), True)
    if not builder.IsDone():
        raise RuntimeError("OpenCascade flat bottom construction failed")
    from OCP.BRepCheck import BRepCheck_Analyzer

    face = builder.Face()
    if not BRepCheck_Analyzer(face).IsValid():
        raise ValueError("flat bottom has invalid or self-touching topology")
    return face


@contextmanager
def _step_settings(unit):
    """Restore global units and native message levels even when writing fails."""
    from OCP.Interface import Interface_Static
    from OCP.Message import Message, Message_Fail
    from OCP.STEPControl import STEPControl_Controller

    with _OCP_LOCK:
        STEPControl_Controller.Init_s()
        old = {
            key: Interface_Static.CVal_s(key)
            for key in (
                "write.step.unit",
                "xstep.cascade.unit",
            )
        }
        printers = [
            (p, p.GetTraceLevel()) for p in Message.DefaultMessenger_s().Printers()
        ]
        try:
            Interface_Static.SetCVal_s("xstep.cascade.unit", "M")
            Interface_Static.SetCVal_s("write.step.unit", unit)
            for printer, _ in printers:
                printer.SetTraceLevel(Message_Fail)
            with redirect_stdout(StringIO()):
                yield
        finally:
            for key, value in old.items():
                Interface_Static.SetCVal_s(key, value)
            for printer, level in printers:
                printer.SetTraceLevel(level)


def export_step(shape, path, unit="M") -> Path:
    """Write metre-coordinate geometry to STEP, preserving physical dimensions."""
    if unit not in ("M", "MM", "CM", "INCH", "FT"):
        raise ValueError("unsupported STEP unit")
    from OCP.IFSelect import IFSelect_RetDone
    from OCP.STEPControl import STEPControl_AsIs, STEPControl_Writer

    path = Path(path)
    with _step_settings(unit):
        writer = STEPControl_Writer()
        if writer.Transfer(shape, STEPControl_AsIs) != IFSelect_RetDone:
            raise RuntimeError("OpenCascade STEP transfer failed")
        if writer.Write(str(path)) != IFSelect_RetDone:
            raise RuntimeError("OpenCascade STEP write failed")
    return path


def profile_to_step(
    profile, path, *, mirror=True, bottom=True, n_x=None, n_z=None
) -> Path:
    """Fit the wetted sides and optional flat bottom; export in metres."""
    points = profile_point_grid(profile, n_x, n_z)
    shape = bspline_face_from_grid(points)
    if bottom:
        base = flat_bottom_face(points)
        if base is not None:
            shape = _sew(shape, base)
    if mirror:
        shape = mirror_and_sew(shape)
    return export_step(shape, path)
