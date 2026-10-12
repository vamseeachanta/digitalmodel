"""Experimental section coordinates preserving the fixed-z/PCHIP reference."""

from itertools import pairwise

import numpy as np

from .mesh_generator import _shape_preserving_interp


def reference_section(profile, x, z):
    """Evaluate the existing between-offset reference, including endpoint fills."""
    stations = sorted(profile.stations, key=lambda station: station.x_position)
    values = []
    for station in stations:
        offsets = np.asarray(sorted(station.waterline_offsets))
        values.append(
            _shape_preserving_interp(
                offsets[:, 0],
                offsets[:, 1],
                np.asarray(z) + profile.draft,
                (offsets[0, 1], offsets[-1, 1]),
            )
        )
    station_x = np.array([station.x_position for station in stations])
    return np.maximum(
        [
            _shape_preserving_interp(station_x, column, [x], (0, 0))[0]
            for column in np.asarray(values).T
        ],
        0,
    )


def section_arclength_grid(profile, n_x=None, n_z=None):
    """Sample each reference section by chord-length quadrature, keel first.

    Dense fixed-z chords approximate the coordinate map only. Final ordinates
    are reevaluated on the immutable reference, never interpolated in arc length
    between source stations. This is an experimental, unqualified fit input.
    """
    from .hull_surface_brep import profile_point_grid

    default = profile_point_grid(profile, n_x, n_z)
    nx, nz = default.shape[:2]
    dense = profile_point_grid(profile, nx, max(4097, 32 * nz + 1))
    points = np.empty_like(default)
    for i, section in enumerate(dense):
        lengths = np.r_[0, np.cumsum(np.linalg.norm(np.diff(section, axis=0), axis=1))]
        if not np.isfinite(lengths).all() or np.any(np.diff(lengths) <= 0):
            raise ValueError("section must have finite positive arc length")
        z = np.interp(np.linspace(0, lengths[-1], nz), lengths, section[:, 2])
        points[i, :, 0] = section[0, 0]
        points[i, :, 2] = z
        points[i, :, 1] = reference_section(profile, section[0, 0], z)
    return points


def _keel_edge(face):
    from OCP.BRepTools import BRepTools
    from OCP.TopAbs import TopAbs_EDGE, TopAbs_FORWARD
    from OCP.TopExp import TopExp_Explorer
    from OCP.TopoDS import TopoDS

    vmin = BRepTools.UVBounds_s(face)[2]
    explorer = TopExp_Explorer(face, TopAbs_EDGE)
    matches = []
    while explorer.More():
        edge = TopoDS.Edge_s(explorer.Current())
        bounds = BRepTools.UVBounds_s(face, edge)
        if abs(bounds[2] - vmin) <= 1e-12 and abs(bounds[3] - vmin) <= 1e-12:
            matches.append(TopoDS.Edge_s(edge.Oriented(TopAbs_FORWARD)))
        explorer.Next()
    if len(matches) != 1:
        raise ValueError("fitted side must have one natural keel edge")
    return matches[0]


def _keel_curve(edge, draft):
    from OCP.BRepAdaptor import BRepAdaptor_Curve
    from OCP.GeomAbs import GeomAbs_BSplineCurve

    adaptor = BRepAdaptor_Curve(edge)
    if adaptor.GetType() != GeomAbs_BSplineCurve:
        raise ValueError("keel must be a B-spline")
    curve = adaptor.BSpline()  # transformed copy; never modify the side's curve
    if curve.IsRational() or curve.IsPeriodic():
        raise ValueError("keel must be nonrational and nonperiodic")
    curve.Segment(adaptor.FirstParameter(), adaptor.LastParameter())
    poles = np.array([curve.Pole(i).Coord() for i in range(1, curve.NbPoles() + 1)])
    if not np.isfinite(poles).all() or np.max(abs(poles[:, 2] + draft)) > 1e-6:
        raise ValueError("keel must be planar at the profile draft")
    if np.min(poles[:, 1]) < -1e-6 or np.any(np.diff(poles[:, 0]) <= 0):
        raise ValueError("keel must be starboard with increasing x")
    return curve, poles


def shared_bottom_face(face, draft):
    """Bound a planar half-bottom by the side's actual edge; never flatten it."""
    from OCP.BRepBuilderAPI import (
        BRepBuilderAPI_MakeEdge,
        BRepBuilderAPI_MakeFace,
        BRepBuilderAPI_MakeWire,
    )
    from OCP.BRepCheck import BRepCheck_Analyzer
    from OCP.gp import gp_Pnt

    edge = _keel_edge(face)
    curve, poles = _keel_curve(edge, draft)
    if np.max(abs(poles[:, 1])) <= 1e-10:
        return None
    first = np.array(curve.StartPoint().Coord())
    last = np.array(curve.EndPoint().Coord())
    vertices = [last, [last[0], 0, last[2]], [first[0], 0, first[2]], first]
    wire = BRepBuilderAPI_MakeWire(edge)
    for start, end in pairwise(vertices):
        if np.linalg.norm(np.asarray(end) - start) > 1e-10:
            wire.Add(BRepBuilderAPI_MakeEdge(gp_Pnt(*start), gp_Pnt(*end)).Edge())
    if not wire.IsDone():
        raise ValueError("shared keel bottom wire construction failed")
    builder = BRepBuilderAPI_MakeFace(wire.Wire(), True)
    if not builder.IsDone() or not BRepCheck_Analyzer(builder.Face()).IsValid():
        raise ValueError("shared keel bottom has invalid topology")
    return builder.Face()
