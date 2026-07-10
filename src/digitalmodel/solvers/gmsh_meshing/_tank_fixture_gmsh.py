"""Gmsh implementation and MSH2 parser for the synthetic L-tank fixture."""

from __future__ import annotations

import math
import os
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any, TYPE_CHECKING

if TYPE_CHECKING:
    from .tank_fixture import TankFixtureSpec

from ._msh2_contract import (
    Msh2Contract,
    PHYSICAL_NAMES,
    inspect_msh2,
    validate_msh2_contract,
)

MESH_OPTIONS = {
    "Mesh.MshFileVersion": 2.2, "Mesh.Binary": 0.0, "Mesh.SaveAll": 0.0,
    "Mesh.ElementOrder": 1.0, "Mesh.Algorithm3D": 4.0,
    "Mesh.OptimizeNetgen": 1.0, "Mesh.OptimizeThreshold": 1.0,
    "Mesh.Reproducible": 1.0, "Mesh.RandomSeed": 0.0,
    "General.NumThreads": 1.0, "Mesh.MaxNumThreads1D": 1.0,
    "Mesh.MaxNumThreads2D": 1.0,
    "Mesh.MaxNumThreads3D": 1.0,
}


@dataclass(frozen=True)
class PerforatedMemberGeometry:
    orientation: str
    entity: tuple[int, int]
    bounds: tuple[float, ...]
    volume: float
    opening_bounds: tuple[float, ...]
    opening_volume: float
    opening_surface_tags: tuple[int, ...]


@dataclass(frozen=True)
class FixtureBuildSummary:
    output_path: Path
    volume_count: int
    member_geometry: tuple[PerforatedMemberGeometry, ...]
    fluid_volume: float
    wall_surface_tags: tuple[int, ...]
    atmosphere_surface_tags: tuple[int, ...]
    unassigned_surface_tags: tuple[int, ...]
    overlapping_surface_tags: tuple[int, ...]
    min_signed_determinant: float
    shape_min_jacobian: float
    mesh_options: dict[str, float]
    reopened_serialized_mesh: bool

    @property
    def member_count(self) -> int:
        return len(self.member_geometry)

    @property
    def opening_count(self) -> int:
        return sum(len(member.opening_surface_tags) for member in self.member_geometry)


def _gmsh_module():
    try:
        import gmsh
    except (ImportError, OSError) as exc:
        raise ImportError("Gmsh 4.x is required for the tank fixture") from exc
    return gmsh


def build_tank_fixture(
    spec: "TankFixtureSpec", output_path: Path | str
) -> FixtureBuildSummary:
    """Build, attest, and atomically promote a deterministic MSH2 source mesh."""
    output = Path(output_path)
    output.parent.mkdir(parents=True, exist_ok=True)
    gmsh = _gmsh_module()
    owns_session = not bool(gmsh.isInitialized())
    if owns_session:
        gmsh.initialize()
    try:
        gmsh.clear()
        gmsh.option.setNumber("General.Terminal", 0)
        _set_mesh_options(gmsh)
        volume_tags, members = _build_occ_geometry(gmsh, spec)
        groups = _classify_and_tag_surfaces(gmsh, spec, volume_tags)
        fluid_volume = _validate_occ_volume(gmsh, spec, volume_tags)
        minimum, shape_minimum = _generate_and_check_mesh(gmsh, spec)
        contract = _write_verified_mesh(gmsh, output)
        validate_msh2_contract(contract)
        return FixtureBuildSummary(
            output_path=output, volume_count=len(volume_tags),
            member_geometry=members, fluid_volume=fluid_volume,
            wall_surface_tags=groups["walls"], atmosphere_surface_tags=groups["atmosphere"],
            unassigned_surface_tags=groups["unassigned"], overlapping_surface_tags=groups["overlap"],
            min_signed_determinant=minimum, shape_min_jacobian=shape_minimum,
            mesh_options=dict(MESH_OPTIONS),
            reopened_serialized_mesh=True,
        )
    finally:
        if owns_session:
            gmsh.finalize()


def _set_mesh_options(gmsh: Any) -> None:
    for name, value in MESH_OPTIONS.items():
        gmsh.option.setNumber(name, value)


def _build_occ_geometry(gmsh: Any, spec: "TankFixtureSpec"):
    tank = spec.tank
    occ = gmsh.model.occ
    longitudinal_leg = occ.addBox(
        0.0, 0.0, 0.0, tank.longitudinal_leg_width, tank.height, tank.length
    )
    transverse_leg = occ.addBox(
        0.0, 0.0, 0.0, tank.breadth, tank.height, tank.transverse_leg_length
    )
    fluid, _ = occ.fuse(
        [(3, longitudinal_leg)], [(3, transverse_leg)],
        removeObject=True, removeTool=True,
    )
    members = _build_perforated_members(gmsh, spec)
    entities = [member.entity for member in members]
    result, _ = occ.cut(fluid, entities, removeObject=True, removeTool=True)
    occ.synchronize()
    volume_tags = sorted(tag for dim, tag in result if dim == 3)
    if len(volume_tags) != 1:
        raise ValueError(f"fixture must contain one connected fluid volume, got {len(volume_tags)}")
    return volume_tags, members


def _build_perforated_members(gmsh: Any, spec: "TankFixtureSpec") -> tuple[PerforatedMemberGeometry, ...]:
    occ = gmsh.model.occ
    member = spec.members
    tank = spec.tank
    x0 = 0.5 * (tank.longitudinal_leg_width - member.thickness)
    z0 = tank.transverse_leg_length + member.edge_clearance
    longitudinal = occ.addBox(
        x0, 0.0, z0, member.thickness, member.height,
        spec.longitudinal_member_span,
    )
    long_opening = _longitudinal_opening(gmsh, spec, x0, z0)
    long_result, _ = occ.cut([(3, longitudinal)], [long_opening])
    transverse_x0 = tank.longitudinal_leg_width + member.edge_clearance
    transverse_z0 = 0.5 * (tank.transverse_leg_length - member.thickness)
    transverse = occ.addBox(
        transverse_x0, 0.0, transverse_z0,
        spec.transverse_member_span, member.height, member.thickness,
    )
    transverse_opening = _transverse_opening(
        gmsh, spec, transverse_x0, transverse_z0
    )
    transverse_result, _ = occ.cut([(3, transverse)], [transverse_opening])
    occ.synchronize()
    return (
        _validated_member(gmsh, spec, "longitudinal-z", long_result),
        _validated_member(gmsh, spec, "transverse-x", transverse_result),
    )


def _member_contract(spec: "TankFixtureSpec", orientation: str):
    member = spec.members
    tank = spec.tank
    if orientation == "longitudinal-z":
        span = spec.longitudinal_member_span
        x0 = 0.5 * (tank.longitudinal_leg_width - member.thickness)
        z0 = tank.transverse_leg_length + member.edge_clearance
        opening = spec.longitudinal_opening
        bounds = (x0, 0.0, z0, x0 + member.thickness, member.height, z0 + span)
        opening_bounds = (
            x0, 0.5 * member.height - opening.vertical_radius,
            z0 + 0.5 * span - opening.span_radius, x0 + member.thickness,
            0.5 * member.height + opening.vertical_radius,
            z0 + 0.5 * span + opening.span_radius,
        )
        opening_volume = member.thickness * math.pi * opening.vertical_radius * opening.span_radius
        return bounds, opening_bounds, member.thickness * member.height * span, opening_volume
    span = spec.transverse_member_span
    x0 = tank.longitudinal_leg_width + member.edge_clearance
    z0 = 0.5 * (tank.transverse_leg_length - member.thickness)
    opening = spec.transverse_opening
    mid_x = x0 + 0.5 * span
    bounds = (x0, 0.0, z0, x0 + span, member.height, z0 + member.thickness)
    opening_bounds = (
        mid_x - opening.transverse_radius, 0.5 * member.height - opening.vertical_radius,
        z0, mid_x + opening.transverse_radius,
        0.5 * member.height + opening.vertical_radius, z0 + member.thickness,
    )
    opening_volume = member.thickness * math.pi * opening.transverse_radius * opening.vertical_radius
    return bounds, opening_bounds, span * member.height * member.thickness, opening_volume


def _validated_member(
    gmsh: Any, spec: "TankFixtureSpec", orientation: str, result: list[tuple[int, int]]
) -> PerforatedMemberGeometry:
    entities = tuple(item for item in result if item[0] == 3)
    if len(entities) != 1:
        raise ValueError(f"{orientation} member cut produced {len(entities)} volumes")
    entity = entities[0]
    expected_bounds, opening_bounds, gross_volume, expected_opening = _member_contract(spec, orientation)
    tolerance = max(max(map(abs, expected_bounds)), 1.0) * 1e-6
    bounds = tuple(gmsh.model.getBoundingBox(*entity))
    if any(abs(actual - expected) > tolerance for actual, expected in zip(bounds, expected_bounds)):
        raise ValueError(f"{orientation} member bounds differ: {bounds}")
    volume = gmsh.model.occ.getMass(*entity)
    if not math.isclose(volume, gross_volume - expected_opening, rel_tol=1e-8, abs_tol=1e-12):
        raise ValueError(f"{orientation} perforated volume differs: {volume}")
    opening_volume = gross_volume - volume
    surfaces = (
        (tag, tuple(gmsh.model.getBoundingBox(dim, tag)))
        for dim, tag in gmsh.model.getBoundary([entity], combined=False, oriented=False)
        if dim == 2
    )
    opening_surfaces = tuple(
        (tag, surface_bounds) for tag, surface_bounds in surfaces
        if all(
            abs(actual - expected) <= tolerance
            for actual, expected in zip(surface_bounds, opening_bounds)
        )
    )
    if len(opening_surfaces) != 1:
        raise ValueError(f"{orientation} requires one through-opening surface")
    opening_tag, actual_opening_bounds = opening_surfaces[0]
    return PerforatedMemberGeometry(
        orientation, entity, bounds, volume, actual_opening_bounds,
        opening_volume, (opening_tag,),
    )


def _longitudinal_opening(gmsh: Any, spec: "TankFixtureSpec", x0: float, z0: float) -> tuple[int, int]:
    opening = spec.longitudinal_opening
    if opening.span_radius >= opening.vertical_radius:
        radii = (opening.span_radius, opening.vertical_radius)
        x_axis = [0.0, 0.0, 1.0]
    else:
        radii = (opening.vertical_radius, opening.span_radius)
        x_axis = [0.0, 1.0, 0.0]
    disk = gmsh.model.occ.addDisk(
        x0,
        0.5 * spec.members.height,
        z0 + 0.5 * spec.longitudinal_member_span,
        *radii,
        zAxis=[1.0, 0.0, 0.0],
        xAxis=x_axis,
    )
    extrusion = gmsh.model.occ.extrude(
        [(2, disk)], spec.members.thickness, 0.0, 0.0
    )
    return next(item for item in extrusion if item[0] == 3)


def _transverse_opening(gmsh: Any, spec: "TankFixtureSpec", x0: float, z0: float) -> tuple[int, int]:
    opening = spec.transverse_opening
    if opening.transverse_radius >= opening.vertical_radius:
        radii = (opening.transverse_radius, opening.vertical_radius)
        x_axis = [1.0, 0.0, 0.0]
    else:
        radii = (opening.vertical_radius, opening.transverse_radius)
        x_axis = [0.0, 1.0, 0.0]
    disk = gmsh.model.occ.addDisk(
        x0 + 0.5 * spec.transverse_member_span,
        0.5 * spec.members.height,
        z0,
        *radii,
        zAxis=[0.0, 0.0, 1.0],
        xAxis=x_axis,
    )
    extrusion = gmsh.model.occ.extrude(
        [(2, disk)], 0.0, 0.0, spec.members.thickness
    )
    return next(item for item in extrusion if item[0] == 3)


def _classify_and_tag_surfaces(gmsh: Any, spec: "TankFixtureSpec", volume_tags: list[int]):
    boundary = gmsh.model.getBoundary(
        [(3, tag) for tag in volume_tags], combined=True, oriented=False
    )
    all_surfaces = {tag for dim, tag in boundary if dim == 2}
    tolerance = max(spec.tank.height, 1.0) * 1e-6
    atmosphere = {
        tag for tag in all_surfaces
        if _is_top_surface(gmsh, tag, spec.tank.height, tolerance)
    }
    walls = all_surfaces - atmosphere
    if not walls or not atmosphere:
        raise ValueError("fixture requires nonempty wall and atmosphere surfaces")
    _add_physical_group(gmsh, 3, volume_tags, "fluid")
    _add_physical_group(gmsh, 2, sorted(walls), "walls")
    _add_physical_group(gmsh, 2, sorted(atmosphere), "atmosphere")
    return {
        "walls": tuple(sorted(walls)),
        "atmosphere": tuple(sorted(atmosphere)),
        "unassigned": tuple(sorted(all_surfaces - walls - atmosphere)),
        "overlap": tuple(sorted(walls & atmosphere)),
    }


def _is_top_surface(gmsh: Any, tag: int, top: float, tolerance: float) -> bool:
    _, ymin, _, _, ymax, _ = gmsh.model.getBoundingBox(2, tag)
    return abs(ymin - top) <= tolerance and abs(ymax - top) <= tolerance


def _add_physical_group(gmsh: Any, dimension: int, entity_tags: list[int], name: str) -> None:
    _, physical_id = PHYSICAL_NAMES[name]
    gmsh.model.addPhysicalGroup(dimension, entity_tags, physical_id)
    gmsh.model.setPhysicalName(dimension, physical_id, name)


def _validate_occ_volume(
    gmsh: Any, spec: "TankFixtureSpec", volume_tags: list[int]
) -> float:
    volume = sum(gmsh.model.occ.getMass(3, tag) for tag in volume_tags)
    tolerance = max(1e-10, abs(spec.expected_fluid_volume) * 1e-8)
    if abs(volume - spec.expected_fluid_volume) > tolerance:
        raise ValueError(
            f"boolean volume {volume} != expected {spec.expected_fluid_volume}"
        )
    return volume


def _generate_and_check_mesh(
    gmsh: Any, spec: "TankFixtureSpec"
) -> tuple[float, float]:
    gmsh.model.mesh.setSize(gmsh.model.getEntities(0), spec.element_size)
    gmsh.model.mesh.generate(3)
    nodes, tetrahedra, determinants = _mesh_arrays(gmsh)
    minimum = min(determinants)
    tolerance = max(spec.element_size**3, 1.0) * 1e-12
    if minimum <= tolerance:
        raise ValueError(f"non-positive tetrahedral determinant: {minimum}")
    from .quality_analyzer import MeshQualityAnalyzer

    quality = MeshQualityAnalyzer().analyze_tetrahedral_mesh(nodes, tetrahedra)
    return minimum, quality.min_jacobian


def _mesh_arrays(gmsh: Any):
    import numpy as np

    node_tags, coordinates, _ = gmsh.model.mesh.getNodes()
    nodes = np.asarray(coordinates, dtype=float).reshape((-1, 3))
    index_by_tag = {int(tag): index for index, tag in enumerate(node_tags)}
    _, node_connectivity = gmsh.model.mesh.getElementsByType(4)
    tags = np.asarray(node_connectivity, dtype=int).reshape((-1, 4))
    tetrahedra = np.asarray(
        [[index_by_tag[int(tag)] for tag in row] for row in tags], dtype=int
    )
    determinants = [_signed_determinant(nodes[row]) for row in tetrahedra]
    if not determinants:
        raise ValueError("fixture mesh contains no first-order tetrahedra")
    return nodes, tetrahedra, determinants


def _signed_determinant(points: Any) -> float:
    a = points[1] - points[0]
    b = points[2] - points[0]
    c = points[3] - points[0]
    return float(
        a[0] * (b[1] * c[2] - b[2] * c[1])
        - a[1] * (b[0] * c[2] - b[2] * c[0])
        + a[2] * (b[0] * c[1] - b[1] * c[0])
    )


def _write_verified_mesh(gmsh: Any, output: Path) -> Msh2Contract:
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{output.name}.", suffix=".tmp.msh", dir=output.parent
    )
    os.close(descriptor)
    temporary = Path(temporary_name)
    try:
        gmsh.write(str(temporary))
        contract = inspect_msh2(temporary)
        validate_msh2_contract(contract)
        gmsh.clear()
        gmsh.open(str(temporary))
        _validate_reopened_mesh(gmsh)
        os.replace(temporary, output)
        return contract
    finally:
        temporary.unlink(missing_ok=True)


def _validate_reopened_mesh(gmsh: Any) -> None:
    names = {
        gmsh.model.getPhysicalName(dim, tag): (dim, tag)
        for dim, tag in gmsh.model.getPhysicalGroups()
    }
    if names != PHYSICAL_NAMES:
        raise ValueError(f"reopened physical groups differ: {names}")
    element_types = set(gmsh.model.mesh.getElementTypes())
    if element_types != {2, 4}:
        raise ValueError(f"reopened MSH has unexpected element types: {element_types}")
