# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Read straight DXF modelspace primitives without flattening or repair."""

from pathlib import Path
from typing import Any, NoReturn

from .models import (
    CadGeometry,
    CadReadError,
    EntityKind,
    GeometryEntity,
    LengthUnit,
    Point3D,
    UnitsError,
    normalized_geometry,
)
from .units import resolve_units

# ezdxf 1.4.5, DXF Units, $INSUNITS table (accessed 2026-10-19).
# https://ezdxf.readthedocs.io/en/stable/concepts/units.html#insunits
# Code 0 denotes no declaration; unsupported codes must not become unitless.
DXF_UNITS: dict[int, LengthUnit | None] = {
    0: None,
    1: "in",
    2: "ft",
    4: "mm",
    5: "cm",
    6: "m",
}


def _reject(entity: Any, reason: str) -> NoReturn:
    raise CadReadError(f"DXF {entity.dxftype()} handle {entity.dxf.handle}: {reason}")


def _primitive(entity: Any) -> tuple[EntityKind, list[Point3D], bool]:
    kind = entity.dxftype()
    if getattr(entity.dxf, "thickness", 0):
        _reject(entity, "thickness is unsupported")
    if kind == "POINT":
        return "point", [tuple(entity.dxf.location)], False
    if kind == "LINE":
        return "line", [tuple(entity.dxf.start), tuple(entity.dxf.end)], False
    if kind == "LWPOLYLINE":
        # ezdxf 1.4.5, LWPOLYLINE: bulge=0 is straight; widths and thickness
        # describe geometry beyond a centreline. Preserve OCS via WCS vertices.
        # https://ezdxf.readthedocs.io/en/stable/dxfentities/lwpolyline.html
        if entity.has_arc:
            _reject(entity, "bulge arcs are unsupported")
        if entity.has_width:
            _reject(entity, "width is unsupported")
        return "polyline", list(entity.vertices_in_wcs()), entity.closed
    if kind == "POLYLINE":
        if not (entity.is_2d_polyline or entity.is_3d_polyline):
            _reject(entity, "polyface and polygon meshes are unsupported")
        # DXF POLYLINE curve/spline-fit flags (ezdxf 1.4.5 POLYLINE docs):
        # use the library's symbolic flags instead of accepting fitted vertices.
        from ezdxf.lldxf import const

        if entity.dxf.flags & (
            const.POLYLINE_CURVE_FIT_VERTICES_ADDED
            | const.POLYLINE_SPLINE_FIT_VERTICES_ADDED
        ):
            _reject(entity, "fitted polylines are unsupported")
        if entity.has_arc:
            _reject(entity, "bulge arcs are unsupported")
        if entity.has_width:
            _reject(entity, "width is unsupported")
        return "polyline", list(entity.points_in_wcs()), entity.is_closed
    if kind == "3DFACE":
        # ezdxf 1.4.5 3DFACE: a triangle repeats its third vertex as fourth.
        # https://ezdxf.readthedocs.io/en/stable/dxfentities/3dface.html
        points = list(entity.wcs_vertices(close=False))
        if points[-1] == points[-2]:
            points.pop()
        return ("triangle" if len(points) == 3 else "polygon"), points, False
    _reject(entity, "unsupported entity; no partial geometry is returned")


def read_dxf(path: Path, units: LengthUnit | None) -> CadGeometry:
    """Load DXF modelspace only; require consistent, supported length units."""
    try:
        import ezdxf
    except ImportError:
        raise ImportError("DXF reading requires digitalmodel[drawings]") from None
    try:
        doc = ezdxf.readfile(path)
    except (ezdxf.DXFError, OSError, UnicodeError):
        raise CadReadError("DXF file cannot be read") from None
    code = doc.header.get("$INSUNITS", 0)
    if code not in DXF_UNITS:
        raise UnitsError("Unsupported DXF $INSUNITS declaration")
    decision = resolve_units(DXF_UNITS[code], units)
    points: list[Point3D] = []
    entities: list[GeometryEntity] = []
    for entity in doc.modelspace():
        kind, vertices, closed = _primitive(entity)
        if not vertices:
            _reject(entity, "empty primitive")
        indices = tuple(range(len(points), len(points) + len(vertices)))
        if closed:
            indices += (indices[0],)
        points.extend(vertices)
        entities.append(
            GeometryEntity(kind, indices, entity.dxf.layer, entity.dxf.handle)
        )
    return normalized_geometry(points, entities, decision, "dxf")
