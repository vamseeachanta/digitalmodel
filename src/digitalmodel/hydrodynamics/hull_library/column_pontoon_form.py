"""Direct column/pontoon panel unions. Metres; z=0 waterline, z upward.

Faceted convex primitives are clipped at their intersections; no BRep kernel.
Hydrostatics describe the union, while primitive checks are non-additive.
"""

from dataclasses import dataclass, field
from math import pi
from typing import Any, Literal, cast

import numpy as np
from numpy.typing import NDArray
from pydantic import BaseModel, ConfigDict, Field, model_validator

from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
from digitalmodel.hydrodynamics.diffraction.mesh_orientation import (
    OrientationReport, orient_outward, orientation_report,
)
from .curvature_screen import hullprod_available, screen_panel_mesh
from .profile_schema import HullType
from .column_pontoon_mesh import _quad_mesh, _union_faces
from .column_pontoon_primitives import _primitives

_EPS = 1e-8
Array = NDArray[np.float64]
Solid = dict[str, Any]


class ColumnPontoonParameters(BaseModel):
    """Regular column layout; optional centre column gets radial ring connectors.

    Spacing is adjacent centre distance (four columns form a square). Plates
    replace the keel portion; hard tanks replace the waterline portion.
    Twin pontoons connect columns 0-1 and 2-3. A lid is geometric only.
    Target size controls axial/profile edges; clipped junctions require local refinement.
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
    hard_tank_depth: float | None = Field(default=None, gt=0)
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
        plate_height = self.heave_plate_thickness
        tank_depth = self.resolved_tank_depth
        for diameter, height in ((self.heave_plate_diameter, plate_height),
                                 (self.hard_tank_diameter, tank_depth)):
            if bool(diameter) != bool(height) or height >= self.draft:
                raise ValueError("plate/tank needs diameter and height strictly below draft")
        if self.hard_tank_depth and self.hard_tank_height and self.hard_tank_depth != self.hard_tank_height:
            raise ValueError("hard_tank_depth and legacy hard_tank_height must agree")
        if tank_depth + plate_height >= self.draft:
            raise ValueError("hard tank and keel plate must leave positive column depth")
        if self.hard_tank_diameter and (not self.diameter or self.hard_tank_diameter <= self.diameter):
            raise ValueError("hard tank requires circular column and larger diameter")
        extent = max(self.diameter or (self.square_side or 0) * np.sqrt(2),
                     self.heave_plate_diameter or 0, self.hard_tank_diameter or 0)
        if self.count > 1 and self.resolved_spacing <= extent:
            raise ValueError("columns/plates must not overlap")
        if self.center_diameter and self.radius <= (extent + self.center_diameter) / 2:
            raise ValueError("centre column overlaps outer columns")
        self._validate_pontoons()
        return self

    def _validate_pontoons(self) -> None:
        if self.pontoon_layout != "none":
            if self.count < 3 or (self.pontoon_layout == "twin" and self.count != 4):
                raise ValueError("ring requires >=3 columns; twin requires four")
            if min(self.pontoon_width, self.pontoon_height) <= 0:
                raise ValueError("pontoons require positive width and height")
            endpoint_width = min(self.diameter or self.square_side or 0,
                                 self.center_diameter or float("inf"))
            if self.pontoon_width > endpoint_width:
                raise ValueError("pontoon width cannot exceed its endpoint column width")
            z = self.pontoon_center_z if self.pontoon_center_z is not None else -self.draft + self.pontoon_height / 2
            if z - self.pontoon_height / 2 < -self.draft - _EPS or z + self.pontoon_height / 2 >= 0:
                raise ValueError("pontoons must fit strictly below waterline and above keel")
        elif any((self.pontoon_width, self.pontoon_height, self.pontoon_corner_radius,
                  self.pontoon_center_z)):
            raise ValueError("pontoon dimensions require a layout")
        if self.pontoon_corner_radius > min(self.pontoon_width, self.pontoon_height) / 2:
            raise ValueError("pontoon radius exceeds half section dimension")

    @property
    def resolved_tank_depth(self) -> float:
        """Legacy height is interpreted as tank depth below the waterline."""
        return self.hard_tank_depth or self.hard_tank_height

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
    max_aspect_ratio: float = 0.
    min_edge: float = 0.
    max_edge: float = 0.
    closure_status: str = "open-at-waterline"
    edge_ratio_bound: float = 20.
    edge_ratio_passed: bool | None = None
    comparator_class: str = "analytic-control"
    screening: object | None = None
    notes: list[str] = field(default_factory=list)


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


def _primitive_check(solid: Solid, draft: float) -> dict:
    """Compare integrated emitted closed primitive panels with exact dimensions."""
    primitive = _quad_mesh(solid["faces"], True)
    values = _hydrostatics(list(primitive.vertices[primitive.panels]), draft)
    analytic = (solid["analytic_volume"], solid["analytic_waterplane_area"],
                solid["centroid_z"] + draft, solid["analytic_bm"])
    check = {"name": solid["name"]}
    for name, mesh, exact in zip(("volume", "waterplane_area", "kb", "bm"), values, analytic):
        delta = np.abs(np.asarray(mesh) - np.asarray(exact))
        nonzero = np.asarray(exact) != 0
        relative = np.where(nonzero, delta / np.where(nonzero, np.abs(exact), 1.),
                            np.where(delta <= 1e-8, 0., np.inf))
        check["analytic_" + name] = exact
        check[name] = dict(mesh=mesh, analytic=exact,
                           relative_difference=relative.tolist(), tolerance=0.005, zero_absolute_tolerance=1e-8,
                           passed=bool(np.all(relative <= 0.005)))
    check["passed"] = all(check[name]["passed"] for name in ("volume", "waterplane_area", "kb", "bm"))
    return check


def generate_column_pontoon(params: ColumnPontoonParameters, *, screen: bool = False) -> tuple[PanelMesh, ColumnPontoonReport]:
    """Return (PanelMesh, report); screening is optional and never an I_D gate."""
    solids, section_area = _primitives(params)
    faces = _union_faces(solids)
    closed = _quad_mesh(faces, True)
    top = np.max(np.abs(closed.vertices[closed.panels, 2]), axis=1) < _EPS
    wetted = _orient_wetted(PanelMesh(closed.vertices, closed.panels[~top], name=closed.name))
    winding = orientation_report(wetted)
    mesh = wetted
    if params.lid:
        # Reuse the oriented wetted panels; geometric lid normals point up.
        panels = closed.panels.copy()
        panels[~top] = wetted.panels
        down = top & (cast(Array, closed.normals)[:, 2] < 0)
        panels[down] = panels[down, ::-1]
        mesh = PanelMesh(closed.vertices, panels, name=closed.name)
    volume, area, kb, bm = _hydrostatics(faces, params.draft)
    checks = [_primitive_check(s, params.draft) for s in solids]
    report = ColumnPontoonReport(volume, area, params.resolved_spacing, section_area,
                                kb, bm, mesh.n_panels, winding,
                                winding.submerged_boundary_edges == 0,
                                _is_watertight(mesh), checks)
    edges = np.linalg.norm(mesh.vertices[mesh.panels] -
                           mesh.vertices[np.roll(mesh.panels, -1, axis=1)], axis=2)
    report.max_aspect_ratio = float(np.max(edges.max(axis=1) / edges.min(axis=1)))
    report.min_edge, report.max_edge = float(edges.min()), float(edges.max())
    report.edge_ratio_passed = report.max_aspect_ratio <= report.edge_ratio_bound
    if not report.edge_ratio_passed:
        report.notes.append(f"edge-ratio bound exceeded: {report.max_aspect_ratio:.3f} > {report.edge_ratio_bound:g}")
    report.closure_status = ("closed-with-lid" if report.watertight else
                             "invalid-lid" if params.lid else "open-at-waterline")
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
