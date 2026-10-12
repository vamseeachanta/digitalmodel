"""DXF entity extraction, diagnostics and unambiguous fragment assembly."""

import importlib
from dataclasses import dataclass

import numpy as np
from pydantic import BaseModel, Field

SUPPORTED = {"LINE", "LWPOLYLINE", "POLYLINE", "ARC", "SPLINE"}
UNIT_FACTORS = {"mm": 0.001, "m": 1.0, "ft": 0.3048, "in": 0.0254}
UNIT_CODES = {"mm": 4, "m": 6, "ft": 2, "in": 1}


class DxfReadReport(BaseModel):
    """Modelspace counts; used includes station labels when assignment uses them."""

    entities_seen: int = 0
    entities_used: int = 0
    entities_skipped: int = 0
    seen_by_type: dict[str, int] = Field(default_factory=dict)
    seen_by_layer: dict[str, int] = Field(default_factory=dict)
    used_by_type: dict[str, int] = Field(default_factory=dict)
    used_by_layer: dict[str, int] = Field(default_factory=dict)
    skipped_by_type: dict[str, int] = Field(default_factory=dict)
    skipped_by_layer: dict[str, int] = Field(default_factory=dict)
    fragments_joined: int = 0
    stations_found: int = 0
    station_assignments: list[dict] = Field(default_factory=list)
    flattening_tolerance_m: float = 0
    units: str | None = None
    errors: list[str] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)

    def count(self, category, entity):
        for key, value in (("type", entity.dxftype()), ("layer", entity.dxf.layer)):
            counts = getattr(self, category + "_by_" + key)
            counts[value] = counts.get(value, 0) + 1
        field = "entities_" + category
        setattr(self, field, getattr(self, field) + 1)


class DxfLinesError(ValueError):
    """An invalid/ambiguous drawing with a partial read report attached."""

    def __init__(self, message, report=None):
        self.report = report if report is not None else DxfReadReport()
        self.report.errors.append(message)
        super().__init__(message)


def require_ezdxf():
    """Keep the optional drawings dependency out of module import paths."""
    try:
        return importlib.import_module("ezdxf")
    except ImportError as exc:
        raise ImportError(
            "DXF support requires pip install 'digitalmodel[drawings]'"
        ) from exc


@dataclass
class Curve:
    points: np.ndarray
    handles: list[str]
    layers: list[str]
    order: int

    @property
    def context(self):
        return f"handles={self.handles}, layers={self.layers}"


def _vertices(entity, distance):
    kind = entity.dxftype()
    if kind == "LINE":
        return [entity.dxf.start, entity.dxf.end]
    if kind in {"ARC", "SPLINE"}:
        return list(entity.flattening(distance))
    if kind == "POLYLINE" and (entity.is_polygon_mesh or entity.is_poly_face_mesh):
        raise ValueError("Meshes are not station curves")
    source = (
        entity.vertices_in_wcs() if kind == "LWPOLYLINE" else entity.points_in_wcs()
    )
    previous = next(iter(source))
    points = []
    for segment in entity.virtual_entities():
        vertices = _vertices(segment, distance)
        if previous.isclose(vertices[-1]):
            vertices.reverse()
        if not previous.isclose(vertices[0]):
            raise ValueError("Disconnected polyline segments")
        points.extend(vertices if not points else vertices[1:])
        previous = vertices[-1]
    return points


def flatten(entity, distance, report, order):
    context = f"handle={entity.dxf.handle}, layer={entity.dxf.layer}"
    try:
        xyz = np.asarray([tuple(p) for p in _vertices(entity, distance)], dtype=float)
    except Exception as exc:
        raise DxfLinesError(
            f"Cannot flatten {context} ({type(exc).__name__})", report
        ) from exc
    if xyz.ndim != 2 or len(xyz) < 2 or not np.isfinite(xyz).all():
        raise DxfLinesError(f"Invalid or empty curve: {context}", report)
    if np.max(np.abs(xyz[:, 2] - xyz[0, 2])) > distance:
        raise DxfLinesError(f"Non-planar drawing curve: {context}", report)
    points = xyz[:, :2]
    return Curve(points, [entity.dxf.handle], [entity.dxf.layer], order)


def collect(doc, config, report):
    curves, labels = [], []
    body = {name.casefold() for name in config.body_plan_layers}
    label_layer = (config.station_label_layer or "").casefold()
    for entity in doc.modelspace():
        report.count("seen", entity)
        layer, kind = entity.dxf.layer.casefold(), entity.dxftype()
        if layer in body and kind in SUPPORTED:
            curves.append(entity)
        elif (
            layer == label_layer
            and kind in {"TEXT", "MTEXT"}
            and config.station_x is None
        ):
            labels.append(entity)
        else:
            report.count("skipped", entity)
    for layer in body:
        if not any(e.dxf.layer.casefold() == layer for e in curves):
            handles = [
                e.dxf.handle
                for e in doc.modelspace()
                if e.dxf.layer.casefold() == layer
            ]
            raise DxfLinesError(
                f"Empty body-plan layer={layer.upper()}, handles={handles}", report
            )
    return curves, labels


def orient(curve, report):
    points = curve.points
    points = points[np.r_[True, np.any(np.diff(points, axis=0) != 0, axis=1)]]
    if len(points) < 2:
        raise DxfLinesError(f"Degenerate curve: {curve.context}", report)
    if points[-1, 1] < points[0, 1]:
        points = points[::-1]
    dz = np.diff(points[:, 1])
    if np.any(dz < -1e-10):
        raise DxfLinesError(f"Non-monotone station: {curve.context}", report)
    if np.any(dz <= 0):
        raise DxfLinesError(
            f"Station has multiple breadths at one height: {curve.context}", report
        )
    curve.points = points
    return curve


def join_fragments(curves, tolerance, report):
    """Join only unique upward continuations; overlapping z ranges stay separate."""
    curves = [orient(c, report) for c in curves]
    while True:
        pairs = [
            (i, j)
            for i, a in enumerate(curves)
            for j, b in enumerate(curves)
            if i != j
            and b.points[0, 1] >= a.points[-1, 1] - tolerance
            and b.points[-1, 1] > a.points[-1, 1] + tolerance
            and np.linalg.norm(a.points[-1] - b.points[0]) <= tolerance
        ]
        if not pairs:
            return sorted(curves, key=lambda c: c.order)
        starts, ends = [i for i, _ in pairs], [j for _, j in pairs]
        if len(set(starts)) != len(starts) or len(set(ends)) != len(ends):
            context = "; ".join(c.context for c in curves)
            raise DxfLinesError(f"Ambiguous fragment join: {context}", report)
        i, j = pairs[0]
        a, b = curves[i], curves[j]
        merged = Curve(
            np.vstack((a.points, b.points[1:])),
            a.handles + b.handles,
            sorted(set(a.layers + b.layers)),
            min(a.order, b.order),
        )
        curves = [c for k, c in enumerate(curves) if k not in (i, j)] + [merged]
        report.fragments_joined += 1


def distance_to_curve(position, curve):
    start, end = curve.points[:-1], curve.points[1:]
    delta = end - start
    t = np.clip(
        np.sum((position - start) * delta, axis=1) / np.sum(delta * delta, axis=1), 0, 1
    )
    return float(np.min(np.linalg.norm(position - start - t[:, None] * delta, axis=1)))


def assign_labels(curves, labels, config, factor, report):
    """Every label must have one nearest curve and every curve one numeric label."""
    assigned = {}
    for label in labels:
        context = f"handle={label.dxf.handle}, layer={label.dxf.layer}"
        content = label.plain_text() if label.dxftype() == "MTEXT" else label.dxf.text
        try:
            value = float(content.strip()) * factor
        except ValueError as exc:
            raise DxfLinesError(
                f"Non-numeric station label: {context}", report
            ) from exc
        position = np.array(tuple(label.dxf.insert)[:2])
        if not np.isfinite(value) or not np.isfinite(position).all():
            raise DxfLinesError(f"Non-finite label: {context}", report)
        distances = np.array([distance_to_curve(position, c) for c in curves])
        nearest = np.flatnonzero(distances <= distances.min() + config.tolerance)
        if len(nearest) != 1 or int(nearest[0]) in assigned:
            context += "; " + "; ".join(curves[i].context for i in nearest)
            raise DxfLinesError(
                f"ambiguous station label: {context}; supply station_x", report
            )
        assigned[int(nearest[0])] = value
        report.count("used", label)
    for i, curve in enumerate(curves):
        if i not in assigned:
            raise DxfLinesError(
                f"Missing station label or station_x: {curve.context}", report
            )
    return [assigned[i] for i in range(len(curves))]
