"""
ABOUTME: FreeCAD model builder for GT1R parachute frame
ABOUTME: Creates 3D tube solids (straight + bent) from frame_geometry_3d, exports STEP
"""
import math
import os
import sys
from pathlib import Path
from typing import Optional

FREECAD_LIB_PATHS = [
    "/usr/lib/freecad-python3/lib",
    "/usr/lib/freecad/lib",
]
for p in FREECAD_LIB_PATHS:
    if os.path.exists(p) and p not in sys.path:
        sys.path.append(p)

try:
    import FreeCAD
    import Part

    FREECAD_AVAILABLE = True
except ImportError:
    FREECAD_AVAILABLE = False

from digitalmodel.structural.parachute.frame_geometry_3d import (
    FrameGeometry3D,
    Member3D,
    Node3D,
)


def _require_freecad():
    if not FREECAD_AVAILABLE:
        raise RuntimeError(
            "FreeCAD not available — install FreeCAD or run via freecadcmd"
        )


def _make_tube(doc, name, start, end, od, wall):
    """Create a hollow tube (straight) between two 3D points."""
    _require_freecad()
    p1 = FreeCAD.Vector(start.x, start.y, start.z)
    p2 = FreeCAD.Vector(end.x, end.y, end.z)
    direction = p2 - p1
    length = direction.Length

    if length < 1e-6:
        raise ValueError(f"Zero-length member: {name}")

    outer = Part.makeCylinder(od / 2, length, p1, direction)
    inner = Part.makeCylinder(
        (od - 2 * wall) / 2, length, p1, direction
    )
    tube_shape = outer.cut(inner)

    obj = doc.addObject("Part::Feature", name)
    obj.Shape = tube_shape
    return obj


def _make_bent_tube(doc, name, start, end, od, wall, bend_clr):
    """Create a hollow tube following a circular arc between two 3D points.

    Uses a wire sweep: builds a circular arc from start to end with the
    given centerline radius, then sweeps a hollow circle profile along it.
    Falls back to straight tube if arc computation fails.
    """
    _require_freecad()
    p1 = FreeCAD.Vector(start.x, start.y, start.z)
    p2 = FreeCAD.Vector(end.x, end.y, end.z)
    chord = p2 - p1
    chord_len = chord.Length

    if chord_len < 1e-6:
        raise ValueError(f"Zero-length member: {name}")

    R = bend_clr
    if chord_len > 2 * R:
        # Chord too long for this radius — fall back to straight
        return _make_tube(doc, name, start, end, od, wall)

    # Compute arc center and mid-point
    mid = (p1 + p2) * 0.5
    chord_dir = chord.normalize()

    # Find perpendicular direction for arc bulge
    # Use Z-up as reference, project out the chord component
    up = FreeCAD.Vector(0, 0, 1)
    perp = up - chord_dir * up.dot(chord_dir)
    if perp.Length < 1e-6:
        # Chord is vertical, use Y as reference
        up = FreeCAD.Vector(0, 1, 0)
        perp = up - chord_dir * up.dot(chord_dir)
    perp.normalize()

    # Sag = R - sqrt(R^2 - (chord/2)^2)
    half_chord = chord_len / 2
    sag = R - math.sqrt(max(R * R - half_chord * half_chord, 0))
    arc_mid = mid + perp * sag

    # Build 3-point arc
    try:
        arc = Part.Arc(p1, arc_mid, p2)
        wire = Part.Wire([arc.toShape()])

        # Sweep profile: hollow circle at start
        outer_circle = Part.makeCircle(od / 2, p1, chord)
        inner_circle = Part.makeCircle((od - 2 * wall) / 2, p1, chord)
        outer_face = Part.Face(Part.Wire(outer_circle))
        inner_face = Part.Face(Part.Wire(inner_circle))

        outer_solid = wire.makePipeShell([Part.Wire(outer_circle)], True, True)
        inner_solid = wire.makePipeShell([Part.Wire(inner_circle)], True, True)
        tube_shape = outer_solid.cut(inner_solid)

        obj = doc.addObject("Part::Feature", name)
        obj.Shape = tube_shape
        return obj
    except Exception:
        # Fall back to straight tube if arc sweep fails
        return _make_tube(doc, name, start, end, od, wall)


def _make_connection_marker(doc, name, node, conn_type, radius=0.5):
    """Place a small sphere at connection location."""
    _require_freecad()
    sphere = Part.makeSphere(
        radius, FreeCAD.Vector(node.x, node.y, node.z)
    )
    obj = doc.addObject("Part::Feature", name)
    obj.Shape = sphere
    return obj


def build_freecad_model(geo, doc_name="GT1R_ParachuteFrame"):
    """Build FreeCAD document with tube solids for all members.

    Straight members use cylinders. Bend members use arc-swept profiles.
    """
    _require_freecad()
    doc = FreeCAD.newDocument(doc_name)

    for member in geo.members:
        start = geo.nodes[member.start_node]
        end = geo.nodes[member.end_node]
        od = member.section.get("OD", 1.75)
        wall = member.section.get("wall", 0.120)
        name = f"Tube_{member.id}_{member.label}"

        if member.is_bend and member.bend_clr > 0:
            _make_bent_tube(doc, name, start, end, od, wall, member.bend_clr)
        else:
            _make_tube(doc, name, start, end, od, wall)

    for conn in geo.connections:
        node = geo.nodes[conn.node_id]
        name = f"Conn_{conn.node_id}_{conn.conn_type}"
        _make_connection_marker(
            doc, name, node, conn.conn_type
        )

    doc.recompute()
    return doc


def export_step(doc, output_path):
    """Export all objects to STEP file."""
    _require_freecad()
    shapes = [o for o in doc.Objects if hasattr(o, "Shape")]
    if not shapes:
        raise ValueError("No shapes in document to export")
    Part.export(shapes, output_path)
    return output_path


def export_iges(doc, output_path):
    """Export all objects to IGES file."""
    _require_freecad()
    shapes = [o for o in doc.Objects if hasattr(o, "Shape")]
    if not shapes:
        raise ValueError("No shapes in document to export")
    Part.export(shapes, output_path)
    return output_path
