"""Direct column/pontoon panel unions. Metres; z=0 waterline, z upward.

Faceted convex primitives are clipped at their intersections; no BRep kernel.
Hydrostatics describe the union, while primitive checks are non-additive.
"""

from dataclasses import dataclass, field
from math import ceil, pi
from typing import Any, Literal, cast

import numpy as np
from numpy.typing import NDArray
from pydantic import BaseModel, ConfigDict, Field, model_validator
from scipy.spatial import ConvexHull, cKDTree

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from digitalmodel.hydrodynamics.diffraction.mesh_orientation import (
    OrientationReport, orient_outward, orientation_report,
)
from .curvature_screen import hullprod_available, screen_panel_mesh
from .profile_schema import HullType

_EPS = 1e-8
Array = NDArray[np.float64]
Solid = dict[str, Any]


class ColumnPontoonParameters(BaseModel):
    """Regular column layout; optional centre column gets radial ring connectors.

    Spacing is adjacent centre distance (four columns form a square). Plates
    and hard tanks replace the bottom portion of columns, not extra ballast.
    Twin pontoons connect columns 0-1 and 2-3. A lid is geometric only.
    Target size controls axial/profile edges; intersection/cap fans may differ.
    """

    model_config = ConfigDict(frozen=True, extra="forbid", allow_inf_nan=False)
    count: int = Field(default=1, ge=1, le=16)
    layout_radius: float | None = Field(default=None, gt=0)
    spacing: float | None = Field(default=None, gt=0)
    diameter: float | None = Field(default=None, gt=0)
    square_side: float | None = Field(default=None, gt=0)
    corner_radius: float = Field(default=0, ge=0)
    draft: float = Field(gt=0)
    center_diameter: float | None = Field(default=None, gt=0)
    pontoon_layout: Literal["none", "ring", "twin"] = "none"
    pontoon_width: float = Field(default=0, ge=0)
    pontoon_height: float = Field(default=0, ge=0)
    pontoon_corner_radius: float = Field(default=0, ge=0)
    pontoon_center_z: float | None = None
    heave_plate_diameter: float | None = Field(default=None, gt=0)
    heave_plate_thickness: float = Field(default=0, ge=0)
    hard_tank_diameter: float | None = Field(default=None, gt=0)
    hard_tank_height: float = Field(default=0, ge=0)
    panel_target_size: float = Field(default=2, gt=0)
    lid: bool = False
    bracing: bool = False
    comparator_class: Literal["analytic-control", "published-geometry"] = "analytic-control"

    @model_validator(mode="after")
    def validate_geometry(self) -> "ColumnPontoonParameters":
        if (self.diameter is None) == (self.square_side is None):
            raise ValueError("supply exactly one of diameter and square_side")
        if self.corner_radius > (self.square_side or 0) / 2:
            raise ValueError("corner_radius requires square_side and cannot exceed side/2")
        if self.count > 1 and (self.spacing is None) == (self.layout_radius is None):
            raise ValueError("multiple columns require exactly one spacing or layout_radius")
        if self.count == 1 and any((self.spacing, self.layout_radius, self.center_diameter)):
            raise ValueError("single column cannot have layout or centre column")
        for diameter, height in ((self.heave_plate_diameter, self.heave_plate_thickness),
                                 (self.hard_tank_diameter, self.hard_tank_height)):
            if bool(diameter) != bool(height) or height >= self.draft:
                raise ValueError("plate/tank needs diameter and height strictly below draft")
        if self.heave_plate_diameter and self.hard_tank_diameter:
            raise ValueError("choose heave plate or hard tank")
        extent = max(self.diameter or (self.square_side or 0) * np.sqrt(2),
                     self.heave_plate_diameter or 0, self.hard_tank_diameter or 0)
        if self.count > 1 and self.resolved_spacing <= extent:
            raise ValueError("columns/plates must not overlap")
        if self.center_diameter and self.radius <= (extent + self.center_diameter) / 2:
            raise ValueError("centre column overlaps outer columns")
        if self.pontoon_layout != "none":
            if self.count < 3 or (self.pontoon_layout == "twin" and self.count != 4):
                raise ValueError("ring requires >=3 columns; twin requires four")
            if min(self.pontoon_width, self.pontoon_height) <= 0:
                raise ValueError("pontoons require positive width and height")
            z = self.pontoon_center_z if self.pontoon_center_z is not None else -self.draft + self.pontoon_height / 2
            if z - self.pontoon_height / 2 < -self.draft - _EPS or z + self.pontoon_height / 2 >= 0:
                raise ValueError("pontoons must fit strictly below waterline and above keel")
        elif any((self.pontoon_width, self.pontoon_height, self.pontoon_corner_radius,
                  self.pontoon_center_z)):
            raise ValueError("pontoon dimensions require a layout")
        if self.pontoon_corner_radius > min(self.pontoon_width, self.pontoon_height) / 2:
            raise ValueError("pontoon radius exceeds half section dimension")
        return self

    @property
    def radius(self) -> float:
        return self.layout_radius or (self.spacing or 0) / (2 * np.sin(pi / self.count))

    @property
    def resolved_spacing(self) -> float:
        return self.spacing or 2 * self.radius * np.sin(pi / self.count)


@dataclass
class ColumnPontoonReport:
    """Union hydrostatics: KB above keel, BM roll/pitch about waterplane centroid."""

    displacement: float
    waterplane_area: float
    column_spacing: float
    pontoon_section_area: float
    kb: float
    bm: tuple[float, float]
    panel_count: int
    winding: OrientationReport
    wetted_closed: bool
    watertight: bool
    primitive_checks: list[dict]
    comparator_class: str = "analytic-control"
    screening: object | None = None
    notes: list[str] = field(default_factory=list)


def _profile(width: float, height: float, radius: float, target: float) -> Array:
    """CCW rounded rectangle; the circular limit retains 128 circumference facets."""
    points: list[Array] = []
    for sx, sy, start in ((1, 1, 0), (-1, 1, pi/2), (-1, -1, pi), (1, -1, 3*pi/2)):
        centre = np.array([sx * (width/2 - radius), sy * (height/2 - radius)])
        minimum = 32 if radius == min(width, height)/2 else 8
        angles = np.linspace(start, start + pi/2, max(minimum, ceil(pi*radius/(2*target))) + 1)
        arc = centre + radius * np.column_stack((np.cos(angles), np.sin(angles)))
        points.extend(arc if radius else [centre])
    cleaned: list[Array] = []
    for a, b in zip(points, points[1:] + points[:1]):
        if np.linalg.norm(b - a) > _EPS:
            cleaned.extend(a + t * (b-a) for t in np.arange(ceil(np.linalg.norm(b-a)/target))
                           / ceil(np.linalg.norm(b-a)/target))
    return np.array(cleaned)


def _solid(profile: Array, origin: Array, e1: Array, e2: Array, axis: Array,
           length: float, target: float, area: float, name: str) -> Solid:
    levels = np.linspace(0, length, ceil(length/target) + 1)
    rings = [origin + profile[:, :1]*e1 + profile[:, 1:]*e2 + z*axis for z in levels]
    faces = [rings[0][::-1], rings[-1]]
    for lower, upper in zip(rings, rings[1:]):
        faces.extend(np.array([lower[i], lower[(i+1)%len(lower)],
                               upper[(i+1)%len(lower)], upper[i]]) for i in range(len(lower)))
    vertices = np.vstack(rings)
    planes = np.unique(np.round(ConvexHull(vertices).equations, 12), axis=0)
    waterplane = area if axis[2] == 1 and abs(vertices.max(0)[2]) < _EPS else 0.
    width = np.ptp(profile[:, 0])
    r = np.sqrt(max(0., width**2-area)/(4-pi)) if axis[2] == 1 else 0.
    c = width/2-r
    inertia = width**4/12 - 4*(c*c*r*r*(1-pi/4)+c*r**3/3+r**4*(1/3-pi/16))
    return dict(faces=faces, planes=planes, bounds=(vertices.min(0), vertices.max(0)),
                analytic_volume=area*length, analytic_waterplane_area=waterplane,
                centroid_z=origin[2]+axis[2]*length/2,
                analytic_bm=(inertia/(area*length),)*2 if waterplane else (0., 0.), name=name)


def _primitives(p: ColumnPontoonParameters) -> tuple[list[Solid], float]:
    target, solids = p.panel_target_size, []
    angles = np.arange(p.count)*2*pi/p.count + (pi/4 if p.count == 4 else pi/3)
    centres = np.column_stack((p.radius*np.cos(angles), p.radius*np.sin(angles), np.zeros(p.count)))
    for i, centre in enumerate(centres):
        diameter = p.heave_plate_diameter or p.hard_tank_diameter
        height = p.heave_plate_thickness or p.hard_tank_height
        width = p.diameter or p.square_side or 0
        section = _profile(width, width, p.diameter/2 if p.diameter else p.corner_radius, target)
        area = pi*(p.diameter/2)**2 if p.diameter else width**2 - (4-pi)*p.corner_radius**2
        solids.append(_solid(section, centre + [0, 0, -p.draft+height], np.eye(3)[0],
                             np.eye(3)[1], np.eye(3)[2], p.draft-height, target, area, f"column_{i}"))
        if diameter:
            solids.append(_solid(_profile(diameter, diameter, diameter/2, target),
                                 centre+[0, 0, -p.draft], np.eye(3)[0], np.eye(3)[1],
                                 np.eye(3)[2], height, target, pi*(diameter/2)**2, f"base_{i}"))
    if p.center_diameter:
        d = p.center_diameter
        solids.append(_solid(_profile(d, d, d/2, target), np.array([0, 0, -p.draft]),
                             np.eye(3)[0], np.eye(3)[1], np.eye(3)[2], p.draft,
                             target, pi*(d/2)**2, "centre_column"))
    links = []
    if p.pontoon_layout == "ring":
        links = [(centres[i], centres[(i+1)%p.count]) for i in range(p.count)]
        if p.center_diameter:
            links += [(np.zeros(3), c) for c in centres]
    elif p.pontoon_layout == "twin":
        links = [(centres[0], centres[1]), (centres[2], centres[3])]
    section_area = p.pontoon_width*p.pontoon_height - (4-pi)*p.pontoon_corner_radius**2
    for i, (a, b) in enumerate(links):
        length = float(np.linalg.norm(b-a))
        axis = (b-a)/length
        z = p.pontoon_center_z if p.pontoon_center_z is not None else -p.draft+p.pontoon_height/2
        solids.append(_solid(_profile(p.pontoon_width, p.pontoon_height, p.pontoon_corner_radius, target),
                             a+[0, 0, z], np.array([-axis[1], axis[0], 0]), np.eye(3)[2],
                             axis, length, target, section_area, f"pontoon_{i}"))
    return solids, section_area


def _clip(poly: Array, plane: Array, inside: bool) -> Array | None:
    values = poly @ plane[:3] + plane[3]
    values[np.abs(values) < _EPS] = 0
    result = []
    for a, b, da, db in zip(poly, np.roll(poly, -1, axis=0), values, np.roll(values, -1)):
        if (da <= 0 if inside else da >= 0):
            result.append(a)
        if da*db < 0:
            result.append(a + da/(da-db)*(b-a))
    if len(result) < 3:
        return None
    poly = np.array(result)
    poly = poly[np.linalg.norm(poly - np.roll(poly, 1, axis=0), axis=1) > _EPS]
    return poly if len(poly) >= 3 else None


def _subtract(poly: Array, cutter: Solid, source_index: int, cutter_index: int) -> list[Array]:
    low, high = cutter["bounds"]
    if np.any(poly.max(0) < low - _EPS) or np.any(poly.min(0) > high + _EPS):
        return [poly]
    distances = poly @ cutter["planes"][:, :3].T + cutter["planes"][:, 3]
    if np.any(np.min(distances, axis=0) > _EPS):
        return [poly]
    normal = np.sum(np.cross(poly, np.roll(poly, -1, axis=0)), axis=0)
    coplanar = np.max(np.abs(distances), axis=0) < _EPS
    if source_index < cutter_index and np.any(coplanar & (cutter["planes"][:, :3] @ normal > 0)):
        return [poly]  # Keep same-facing overlap once, from the earlier primitive.
    outside, remaining = [], poly
    # Cut the tightest planes first to avoid fragmenting the far outside region.
    for plane in cutter["planes"][np.argsort(np.min(distances, axis=0))[::-1]]:
        values = remaining @ plane[:3] + plane[3]
        if np.max(values) <= _EPS:
            continue
        fragment = _clip(remaining, plane, False)
        if fragment is not None:
            outside.append(fragment)
        clipped = _clip(remaining, plane, True)
        if clipped is None:
            break
        remaining = clipped
    return outside


def _union_faces(solids: list[Solid]) -> list[Array]:
    output = []
    for i, solid in enumerate(solids):
        for face in solid["faces"]:
            fragments = [face]
            for j, cutter in enumerate(solids):
                if i != j:
                    fragments = [piece for fragment in fragments
                                 for piece in _subtract(fragment, cutter, i, j)]
            output.extend(fragments)
    return output


def _snap_faces(faces: list[Array]) -> tuple[list[Array], Array]:
    points = np.unique(np.round(np.vstack(faces), 8), axis=0)
    tree = cKDTree(points)
    parents = np.arange(len(points))
    def root(i: int) -> int:
        while parents[i] != i:
            parents[i] = parents[parents[i]]
            i = parents[i]
        return i
    for a, b in tree.query_pairs(10*_EPS):
        parents[root(b)] = root(a)
    mapped = points[[root(i) for i in range(len(points))]]
    snapped = [mapped[tree.query(face)[1]] for face in faces]
    points = np.unique(mapped, axis=0)
    return cast(list[Array], snapped), cast(Array, points)


def _quad_mesh(faces: list[Array], lid: bool) -> PanelMesh:
    snapped, points = _snap_faces(faces)
    tree = cKDTree(points)
    vertices: list[Array] = []
    panels: list[list[int]] = []
    index: dict[tuple, int] = {}
    def vertex(point: Array) -> int:
        key = tuple(np.round(point, 8))
        if key not in index:
            index[key] = len(vertices)
            vertices.append(point)
        return index[key]
    for face in snapped:
        face = face[np.linalg.norm(face-np.roll(face, 1, axis=0), axis=1) > _EPS]
        if len(face) < 3:
            continue
        if not lid and np.max(np.abs(face[:, 2])) < _EPS:
            continue
        boundary = []
        for a, b in zip(face, np.roll(face, -1, axis=0)):
            edge = b-a
            length2 = edge @ edge
            candidates = points[tree.query_ball_point((a+b)/2, np.sqrt(length2)/2+_EPS)]
            t = (candidates-a) @ edge / length2
            on = (np.linalg.norm(candidates-a-t[:, None]*edge, axis=1) < 3*_EPS) & (t >= -_EPS) & (t < 1-_EPS)
            boundary.extend(candidates[on][np.argsort(t[on])])
        ring = np.array(boundary)
        centre = face.mean(0)
        midpoints = (ring + np.roll(ring, -1, axis=0))/2
        for i, a in enumerate(ring):
            quad = [a, midpoints[i], centre, midpoints[i-1]]
            if np.linalg.norm(np.cross(quad[1]-a, centre-a)) > 1e-10:
                panels.append([vertex(v) for v in quad])
    return PanelMesh(np.array(vertices), np.array(panels, dtype=np.int32), name="column_pontoon")


def _hydrostatics(faces: list[Array], draft: float) -> tuple[float, float, float, tuple[float, float]]:
    volume, moment, area, first, second = 0., 0., 0., np.zeros(2), np.zeros(2)
    for face in faces:
        for i in range(1, len(face)-1):
            tri = face[[0, i, i+1]]
            v = np.dot(tri[0], np.cross(tri[1], tri[2]))/6
            volume += v
            moment += v*np.sum(tri[:, 2])/4
            if np.max(np.abs(tri[:, 2])) < _EPS:
                a = np.cross(tri[1]-tri[0], tri[2]-tri[0])[2]/2
                xy = tri[:, :2]
                area += a
                first += a*xy.sum(0)/3
                second += a*(np.sum(xy**2, axis=0) + xy[0]*xy[1] + xy[0]*xy[2] + xy[1]*xy[2])/6
    inertia = second - first**2/area if area else np.zeros(2)
    bm = (inertia/volume)[::-1]
    return float(volume), float(area), float(moment/volume+draft), (float(bm[0]), float(bm[1]))


def _orient_wetted(mesh: PanelMesh) -> PanelMesh:
    """Run the existing strict BFS/volume check independently on each body."""
    parents = list(range(mesh.n_panels))
    def root(i: int) -> int:
        while parents[i] != i:
            parents[i] = parents[parents[i]]
            i = parents[i]
        return i
    owner: dict[int, int] = {}
    for i, panel in enumerate(mesh.panels):
        for vertex in panel:
            if vertex in owner:
                parents[root(i)] = root(owner[vertex])
            else:
                owner[vertex] = i
    groups: dict[int, list[int]] = {}
    for i in range(mesh.n_panels):
        groups.setdefault(root(i), []).append(i)
    panels = mesh.panels.copy()
    for group in groups.values():
        component = PanelMesh(mesh.vertices, panels[group])
        fixed, _ = orient_outward(component)
        panels[group] = cast(PanelMesh, fixed).panels
    return PanelMesh(mesh.vertices, panels, name=mesh.name)


def _is_watertight(mesh: PanelMesh) -> bool:
    edges = np.stack((mesh.panels, np.roll(mesh.panels, -1, axis=1)), axis=-1).reshape(-1, 2)
    _, counts = np.unique(np.sort(edges, axis=1), axis=0, return_counts=True)
    return bool(np.all(counts == 2))


def generate_column_pontoon(params: ColumnPontoonParameters, *, screen: bool = False) -> tuple[PanelMesh, ColumnPontoonReport]:
    """Return (PanelMesh, report); screening is optional and never an I_D gate."""
    solids, section_area = _primitives(params)
    faces = _union_faces(solids)
    wetted = _quad_mesh(faces, False)
    wetted = _orient_wetted(wetted)
    winding = orientation_report(wetted)
    mesh = _quad_mesh(faces, True) if params.lid else wetted
    volume, area, kb, bm = _hydrostatics(faces, params.draft)
    checks = [dict(name=s["name"], analytic_volume=s["analytic_volume"],
                   analytic_waterplane_area=s["analytic_waterplane_area"],
                   analytic_kb=s["centroid_z"]+params.draft, analytic_bm=s["analytic_bm"])
              for s in solids]
    report = ColumnPontoonReport(volume, area, params.resolved_spacing, section_area,
                                kb, bm, mesh.n_panels, winding,
                                winding.submerged_boundary_edges == 0,
                                _is_watertight(mesh), checks)
    report.comparator_class = params.comparator_class
    report.notes.append("crease-dominated paneling metric; no I_D acceptance threshold (D6)")
    if params.bracing:
        report.notes.append("bracing omitted: Morison-type members, not panel-type in v1")
    if screen and hullprod_available():
        kind = HullType.SPAR if params.count == 1 else HullType.SEMI_PONTOON
        lref = max(np.ptp(mesh.vertices, axis=0))
        report.screening = screen_panel_mesh(wetted, lref=lref, hull_type=kind)
        for solid, check in zip(solids, checks):
            primitive = _quad_mesh(solid["faces"], False)
            check["screening"] = screen_panel_mesh(primitive, lref=max(np.ptp(primitive.vertices, axis=0)), hull_type=kind)
    elif screen:
        report.notes.append("HullProd optional dependency unavailable; screening not evaluated")
    return mesh, report
