"""Offline symmetry and free-flotation checks before writing native inputs.

GDF semantics: WAMIT User Manual v7.4, section 6.1, Low-order method:
https://www.wamit.com/manualv7.4/wamit_v74manualch6.html
ISX/ISY are binary x=0/y=0 cuts, not full-body geometric properties.
"""

import json
from pathlib import Path
from types import SimpleNamespace

import numpy as np

from digitalmodel.hydrodynamics.diffraction.equilibrium_checks import (
    check_mass_displacement,
)
from digitalmodel.hydrodynamics.diffraction.mesh_orientation import orientation_report


# Binary flags and their plane names: WAMIT v7.4, section 6.1.
FLAGS = {"none": (0, 0), "xz": (0, 1), "yz": (1, 0), "xz+yz": (1, 1)}


def parse_symmetry_flags(line):
    """Reject malformed and non-binary flags rather than truncating them."""
    try:
        tokens = line.split()
        flags = tuple(int(token) for token in tokens[:2])
        if len(flags) != 2 or any(flag not in (0, 1) for flag in flags):
            raise ValueError
    except ValueError as exc:
        raise ValueError("Invalid GDF symmetry: expected binary ISX and ISY") from exc
    return flags


def read_gdf_panels(file_path: Path) -> tuple[tuple[int, int], np.ndarray]:
    """Read stored panels using WAMIT v7.4, section 6.1 coordinate ordering.

    Line breaks may occur anywhere in the twelve scalars defining each panel.
    Comment lines, an optional title and numbered panel annotations are supported.
    """
    with file_path.open(encoding="utf-8", errors="replace") as stream:
        lines = [
            line.strip() for line in stream
            if line.strip() and not line.lstrip().startswith("#")
        ]
    if lines:
        try:
            float(lines[0].split()[0])
        except ValueError:
            lines = lines[1:]
        else:
            # A numeric title remains a title when the next two records
            # contain ULEN/GRAV and a complete pair of symmetry flags.
            if len(lines) > 2 and len(lines[2].split()) == 2:
                try:
                    parse_symmetry_flags(lines[2])
                except ValueError:
                    pass  # The ordinary header validation below will diagnose it.
                else:
                    lines = lines[1:]
    if len(lines) < 3:
        raise ValueError(f"{file_path.name}: incomplete GDF header")
    try:
        flags = parse_symmetry_flags(lines[1])
    except ValueError as exc:
        raise ValueError(f"{file_path.name}: {exc}") from exc
    try:
        panel_count = int(lines[2].split()[0])
    except (ValueError, IndexError) as exc:
        raise ValueError(
            f"{file_path.name}: Invalid GDF panel count: expected a positive integer"
        ) from exc
    if panel_count <= 0:
        raise ValueError(
            f"{file_path.name}: Invalid GDF panel count: expected a positive integer"
        )
    return flags, _gdf_coordinates(lines[3:], panel_count, file_path.name)


def _gdf_coordinates(lines: list[str], panel_count: int, name: str) -> np.ndarray:
    """Require complete finite coordinates and retain panel/vertex error context."""
    tokens = []
    for line in lines:
        coordinates, marker, annotation = line.partition("end of panel")
        tokens.extend(coordinates.split())
        if marker and (
            len(tokens) % 12 or annotation.strip() != str(len(tokens) // 12)
        ):
            raise ValueError(f"{name}: Invalid GDF coordinates: invalid panel annotation")
    expected = 12 * panel_count
    if len(tokens) != expected:
        context = f"{name}: panel {len(tokens) // 12 + 1}, vertex {len(tokens) % 12 // 3 + 1}"
        raise ValueError(
            f"Invalid GDF coordinates ({context}): expected {expected} values "
            f"for {panel_count} panels, got {len(tokens)}"
        )
    values = np.empty(expected, dtype=np.float64)
    for index, token in enumerate(tokens):
        try:
            values[index] = float(token)
        except ValueError as exc:
            raise ValueError(
                f"Invalid GDF coordinates ({name}: panel {index // 12 + 1}, "
                f"vertex {index % 12 // 3 + 1}): coordinates must be numeric"
            ) from exc
    if not np.isfinite(values).all():
        index = int(np.flatnonzero(~np.isfinite(values))[0])
        raise ValueError(
            f"Invalid GDF coordinates ({name}: panel {index // 12 + 1}, "
            f"vertex {index % 12 // 3 + 1}): coordinates must be finite"
        )
    return values.reshape(panel_count, 4, 3)


def read_gdf_mesh(file_path: Path):
    """Build a solver-neutral welded mesh, preserving cyclic triangle checks."""
    symmetry_flags, panel_coordinates = read_gdf_panels(file_path)
    # Read panel vertices (4 vertices per panel)
    vertices: list[tuple[float, float, float]] = []
    panels: list[list[int]] = []
    vertex_map: dict[tuple[float, float, float], int] = {}

    for panel_number, coordinates in enumerate(panel_coordinates, 1):
        panel_vertices: list[int] = []
        for point in coordinates:
            vertex = tuple(point)
            if vertex not in vertex_map:
                vertex_map[vertex] = len(vertices)
                vertices.append(vertex)
            panel_vertices.append(vertex_map[vertex])

        unique_count = len(set(panel_vertices))
        repeated_positions = [
            index
            for index, node in enumerate(panel_vertices)
            if panel_vertices.count(node) == 2
        ]
        is_triangle = unique_count == 3 and repeated_positions in (
            [0, 1],
            [1, 2],
            [2, 3],
            [0, 3],
        )
        if unique_count != 4 and not is_triangle:
            raise ValueError(
                f"{file_path.name}: panel {panel_number}: expected four unique "
                "vertices or a triangle with a cyclic-adjacent repeated vertex"
            )
        panels.append(panel_vertices)

    return SimpleNamespace(
        vertices=np.array(vertices, dtype=np.float64),
        panels=np.array(panels, dtype=np.int32),
        name=file_path.stem,
        symmetry_flags=symmetry_flags,
    )


def validate_symmetry(flags, declared, vertices=None, panels=None):
    """Require the spec and the stored sector to describe the same cuts.

    When provided, ``vertices`` is modified in place: coordinates within the
    cut-plane tolerance are snapped to zero, including before a later validation
    error. Validate a disposable array when the original coordinates are needed.
    Pass the validated array (or a copy made afterwards) to ``expand_sector``.
    """
    declared = getattr(declared, "value", declared)
    if flags != FLAGS[declared]:
        raise ValueError(
            f"GDF symmetry ISX={flags[0]}, ISY={flags[1]} disagrees with "
            f"spec symmetry {declared!r}; expected ISX={FLAGS[declared][0]}, "
            f"ISY={FLAGS[declared][1]}"
        )
    if vertices is not None:
        # Shared body-relative centreline tolerance, in mesh metre units.
        tolerance = max(1e-8, 2e-6 * float(np.ptp(vertices[:, 0])))
        for axis, flag in enumerate(flags):
            if not flag:
                continue
            coordinates = vertices[:, axis]
            on_plane = np.abs(coordinates) <= tolerance
            coordinates[on_plane] = 0.0
            if panels is not None:
                plane_panels = np.flatnonzero(on_plane[panels].all(axis=1))
                if len(plane_panels):
                    numbers = ", ".join(str(int(i) + 1) for i in plane_panels)
                    raise ValueError(
                        f"GDF symmetry {declared!r} forbids panels in the "
                        f"{'xy'[axis]}=0 cut plane: {numbers}"
                    )
            if np.any(coordinates < -tolerance) and np.any(coordinates > tolerance):
                raise ValueError(
                    f"GDF symmetry ISX={flags[0]}, ISY={flags[1]}, "
                    f"spec symmetry {declared!r}: panels occupy both sides "
                    f"of the declared {'xy'[axis]}=0 cut"
                )


def expand_sector(mesh):
    """Reflect each declared cut, reversing winding to preserve outward normals.

    ``mesh.vertices`` must first pass ``validate_symmetry`` with the same flags
    and panels. That function snaps cut-plane nodes in place; expansion relies
    on those snapped coordinates for exact welding and does not snap them here.
    """
    if not any(mesh.symmetry_flags):
        return SimpleNamespace(vertices=mesh.vertices, panels=mesh.panels)
    vertices = mesh.vertices.copy()
    panels = mesh.panels.copy()
    for axis, flag in enumerate(mesh.symmetry_flags):
        if flag:
            reflected = vertices.copy()
            reflected[:, axis] *= -1
            # Keep the fan's first corner and diagonal when reversing winding.
            reverse = [0, 3, 2, 1] if panels.shape[1] == 4 else [0, 2, 1]
            panels = np.concatenate([panels, panels[:, reverse] + len(vertices)])
            vertices = np.concatenate([vertices, reflected])
    # Cut nodes were snapped by validate_symmetry before exact welding.
    vertices, inverse = np.unique(vertices, axis=0, return_inverse=True)
    return SimpleNamespace(vertices=vertices, panels=inverse[panels])


def buoyancy_preflight(spec, spec_dir=None):
    """Return DF2 screens; a supplied criterion makes unreliable evidence fatal.

    V = integral(z n_z dA), by the divergence theorem (implemented and explained
    in mesh_orientation). Source: WAMIT v7.4, section 3.1, Hydrostatic data;
    its fluid-normal sign is reversed here for outward panel vector areas.
    Archimedes: displaced mass = rho*V (same section); the existing
    equilibrium_checks.check_mass_displacement defines the residual denominator.
    Gravity cancels in the weight/buoyancy ratio. No acceptance limit is assumed.
    This screen applies only to upright, metre-unit wetted meshes at z=0.
    """
    reports = []
    for body in spec.get_bodies():
        geometry = body.vessel.geometry
        tolerance = geometry.buoyancy_relative_tolerance
        report = {"check": "DF2", "body": body.vessel.name, "status": "NOT_EVALUATED"}
        reason = None
        if tolerance is None:
            reason = "buoyancy_relative_tolerance is required for DF2 evaluation"
        elif body.vessel.inertia.mode != "explicit":
            reason = "DF2 requires an explicit mass independent of mesh displacement"
        elif (
            geometry.length_units != "m"
            or geometry.waterline_z != 0
            or any(body.attitude)
            or body.position[2] != 0
        ):
            reason = "DF2 requires upright metre-unit wetted geometry at z=0"
        else:
            path = Path(geometry.mesh_file)
            source = spec_dir if spec_dir is not None else spec._source_dir
            if not path.is_absolute() and source is not None:
                path = Path(source) / path
            if path.suffix.lower() != ".gdf":
                reason = "DF2 supports GDF geometry only"
            else:
                mesh = read_gdf_mesh(path)
                validate_symmetry(
                    mesh.symmetry_flags, geometry.symmetry, mesh.vertices, mesh.panels
                )
                full = expand_sector(mesh)
                orientation = orientation_report(full)
                if not orientation.ok:
                    reason = (
                        "DF2 requires a closed, outward wetted mesh: "
                        + orientation.describe()
                    )
                else:
                    issues = check_mass_displacement(
                        body.vessel.inertia.mass,
                        orientation.volume,
                        spec.environment.water_density,
                        tolerance,
                    )
                    if issues:
                        raise ValueError("DF2: " + "; ".join(issues))
                    displaced_mass = spec.environment.water_density * orientation.volume
                    report.update(
                        status="PASS",
                        volume_m3=orientation.volume,
                        mass_kg=body.vessel.inertia.mass,
                        displaced_mass_kg=displaced_mass,
                        residual_kg=body.vessel.inertia.mass - displaced_mass,
                        relative_tolerance=tolerance,
                    )
        if reason:
            if tolerance is not None:
                raise ValueError(reason)
            report["reason"] = reason
        reports.append(report)
    return reports


def write_preflight_reports(reports, output_dir):
    """Persist evaluated and unevaluated screens next to generated inputs."""
    path = Path(output_dir) / "preflight_reports.json"
    path.write_text(
        json.dumps(reports, indent=2) + "\n", encoding="utf-8", newline="\n"
    )
    return path
