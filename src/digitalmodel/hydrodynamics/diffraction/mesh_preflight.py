"""Offline symmetry and free-flotation checks before writing native inputs.

GDF semantics: WAMIT User Manual v7.4, section 6.1, Low-order method:
https://www.wamit.com/manualv7.4/wamit_v74manualch6.html
ISX/ISY are binary x=0/y=0 cuts, not full-body geometric properties.
"""

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


def validate_symmetry(flags, declared, vertices=None):
    """Require the spec and the stored sector to describe the same cuts."""
    declared = getattr(declared, "value", declared)
    if flags != FLAGS[declared]:
        raise ValueError(
            f"GDF symmetry ISX={flags[0]}, ISY={flags[1]} disagrees with "
            f"spec symmetry {declared!r}; expected ISX={FLAGS[declared][0]}, "
            f"ISY={FLAGS[declared][1]}"
        )
    if vertices is not None:
        for axis, flag in enumerate(flags):
            coordinates = vertices[:, axis]
            if flag and np.any(coordinates < 0) and np.any(coordinates > 0):
                raise ValueError(
                    f"GDF symmetry ISX={flags[0]}, ISY={flags[1]}, "
                    f"spec symmetry {declared!r}: panels occupy both sides "
                    f"of the declared {'xy'[axis]}=0 cut"
                )


def expand_sector(mesh):
    """Reflect each declared cut, reversing winding to preserve outward normals."""
    if not any(mesh.symmetry_flags):
        return SimpleNamespace(vertices=mesh.vertices, panels=mesh.panels)
    vertices = mesh.vertices.copy()
    panels = mesh.panels.copy()
    for axis, flag in enumerate(mesh.symmetry_flags):
        if flag:
            reflected = vertices.copy()
            reflected[:, axis] *= -1
            panels = np.concatenate([panels, panels[:, ::-1] + len(vertices)])
            vertices = np.concatenate([vertices, reflected])
    # Exact welding matches the repository GDF reader, without a new tolerance.
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
    from digitalmodel.hydrodynamics.diffraction.aqwa_backend import AQWABackend

    reports = []
    for body in spec.get_bodies():
        geometry = body.vessel.geometry
        tolerance = geometry.buoyancy_relative_tolerance
        report = {"check": "DF2", "status": "NOT_EVALUATED"}
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
                mesh = AQWABackend()._parse_gdf(path)
                validate_symmetry(mesh.symmetry_flags, geometry.symmetry, mesh.vertices)
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
                    report.update(
                        status="PASS",
                        volume_m3=orientation.volume,
                        relative_tolerance=tolerance,
                    )
        if reason:
            if tolerance is not None:
                raise ValueError(reason)
            report["reason"] = reason
        reports.append(report)
    return reports
