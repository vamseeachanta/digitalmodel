"""
ABOUTME: FreeCAD model builder for GT1R parachute frame
ABOUTME: Creates 3D tube solids from frame_geometry_3d, exports STEP
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
    """Create a hollow tube between two 3D points."""
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
    """Build FreeCAD document with tube solids for all members."""
    _require_freecad()
    doc = FreeCAD.newDocument(doc_name)

    for member in geo.members:
        start = geo.nodes[member.start_node]
        end = geo.nodes[member.end_node]
        od = member.section.get("OD", 1.5)
        wall = member.section.get("wall", 0.120)
        name = f"Tube_{member.id}_{member.label}"
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
