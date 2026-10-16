# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Import geometry into Capytaine FloatingBody objects.

The optional solver dependency is Capytaine >= 3.0.0. The public load_mesh()
and non-mutating immersed_part() calls also exist in the preceding major
version; the solver wrapper uses the version 3 matrix engine.
Reference: Capytaine (2026), version 3.0 changelog, new mesh module:
https://capytaine.org/stable/changelog.html
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np

from .models import BodyConfig, DOF, MeshFormat

logger = logging.getLogger(__name__)


def _import_capytaine():
    """Lazy import of capytaine to allow module loading without the dependency."""
    try:
        import capytaine as cpt
        return cpt
    except ImportError as e:
        raise ImportError(
            "Capytaine is required but not installed. "
            "Install it in the capytaine-env or activate the environment."
        ) from e


def _require_capytaine3(cpt):
    """Check the matrix-engine requirement before any native preparation."""
    version = str(getattr(cpt, "__version__", "unknown"))
    try:
        major = int(version.split(".", 1)[0])
    except ValueError:
        major = 0
    if major < 3:
        raise RuntimeError(
            f"Installed Capytaine version {version}; Capytaine 3 or later is required."
        )


def create_floating_body(config: BodyConfig):
    """Create a Capytaine FloatingBody from a BodyConfig.

    Args:
        config: Body configuration with mesh source and DOF specification.

    Returns:
        capytaine.FloatingBody configured with DOFs and properties.
    """
    cpt = _import_capytaine()
    _require_capytaine3(cpt)

    if config.mesh.predefined_shape is not None:
        mesh = _create_predefined_mesh(cpt, config.mesh.predefined_shape, config.mesh.shape_params)
    elif config.mesh.path is not None:
        mesh = _load_mesh_file(cpt, config.mesh.path, config.mesh.format)
    else:
        raise ValueError("Must specify either mesh.path or mesh.predefined_shape")

    # Apply symmetry if requested
    if config.mesh.use_symmetry and config.mesh.symmetry_plane:
        mesh = _apply_symmetry(cpt, mesh, config.mesh.symmetry_plane)

    mesh = mesh.immersed_part()
    lid_mesh = None
    if config.use_lid:
        lid_z = config.lid_z if config.lid_z is not None else 0.0
        if not np.isfinite(lid_z) or lid_z > 0.0:
            raise ValueError("The lid must be at or below the free surface")
        radius = config.lid_faces_max_radius
        if radius is not None and (not np.isfinite(radius) or radius <= 0.0):
            raise ValueError("The lid panel radius must be finite and positive")
        if radius is None:
            lid_mesh = mesh.generate_lid(z=lid_z)
        else:
            from .lid import resolved_lid
            lid_mesh = resolved_lid(mesh, lid_z, radius)
        if lid_mesh.nb_faces == 0:
            raise ValueError("The requested lid is empty")

    # Create body with all rigid body DOFs, then remove unwanted ones.
    # Capytaine 2.3+ returns a RigidBodyDofsPlaceholder from rigid_body_dofs()
    # that is resolved inside FloatingBody.__init__.
    body = cpt.FloatingBody(
        mesh=mesh,
        dofs=cpt.rigid_body_dofs(rotation_center=config.rotation_center_or_com),
        center_of_mass=config.center_of_mass,
        mass=config.mass,
        lid_mesh=lid_mesh,
        name=config.mesh.name,
    )

    # Filter to requested DOFs
    requested_dof_names = {d.value for d in config.dofs}
    for dof_name in list(body.dofs.keys()):
        if dof_name not in requested_dof_names:
            del body.dofs[dof_name]

    if config.use_lid and body.lid_mesh is None:
        raise ValueError("The requested lid was not attached")

    logger.info(
        "Created FloatingBody '%s': %d panels, %d DOFs",
        body.name, body.mesh.nb_faces, len(body.dofs),
    )
    return body


def _create_predefined_mesh(cpt, shape: str, params: Dict):
    """Create a predefined mesh shape for testing/validation."""
    shape = shape.lower()
    if shape == "sphere":
        return cpt.mesh_sphere(
            radius=params.get("radius", 1.0),
            center=tuple(params.get("center", (0, 0, 0))),
            name=params.get("name", "sphere"),
            resolution=params.get("resolution", (20, 20)),
        )
    elif shape == "cylinder":
        return cpt.mesh_vertical_cylinder(
            radius=params.get("radius", 1.0),
            length=params.get("length", 2.0),
            center=tuple(params.get("center", (0, 0, 0))),
            name=params.get("name", "cylinder"),
            resolution=params.get("resolution", (20, 20, 5)),
        )
    elif shape == "box":
        return cpt.mesh_parallelepiped(
            size=tuple(params.get("size", (2.0, 2.0, 2.0))),
            center=tuple(params.get("center", (0, 0, 0))),
            name=params.get("name", "box"),
            resolution=params.get("resolution", (10, 10, 10)),
        )
    else:
        raise ValueError(f"Unknown predefined shape: {shape}. Use 'sphere', 'cylinder', or 'box'.")


def _load_mesh_file(cpt, path: Path, fmt: MeshFormat):
    """Load mesh from file using Capytaine's built-in readers or meshio."""
    path = Path(path)
    if not path.exists():
        raise FileNotFoundError(f"Mesh file not found: {path}")

    if fmt == MeshFormat.MESHIO:
        return _load_via_meshio(cpt, path)

    if fmt == MeshFormat.AUTO:
        fmt = _detect_format(path)

    logger.info("Loading mesh from %s (format: %s)", path, fmt.value)
    # Let Capytaine auto-detect from extension — its internal format strings
    # are version-dependent and passing explicit format can cause errors.
    return cpt.load_mesh(str(path))


def _load_via_meshio(cpt, path: Path):
    """Load mesh via meshio for formats not natively supported."""
    try:
        import meshio
    except ImportError as e:
        raise ImportError("meshio is required for this mesh format") from e

    from capytaine.io.meshio import load_from_meshio

    meshio_mesh = meshio.read(str(path))
    logger.info("Loaded mesh via meshio: %d points, %d cells", len(meshio_mesh.points), len(meshio_mesh.cells))
    return load_from_meshio(meshio_mesh, name=path.stem)


def _detect_format(path: Path) -> MeshFormat:
    """Detect mesh format from file extension."""
    ext = path.suffix.lower()
    format_map = {
        ".stl": MeshFormat.STL,
        ".gdf": MeshFormat.GDF,
        ".msh": MeshFormat.MSH,
        ".mar": MeshFormat.MAR,
    }
    if ext in format_map:
        return format_map[ext]
    # Fall back to meshio for unknown extensions
    return MeshFormat.MESHIO


def _capytaine_format_string(fmt: MeshFormat) -> Optional[str]:
    """Map MeshFormat enum to Capytaine's format string."""
    mapping = {
        MeshFormat.STL: "STL",
        MeshFormat.GDF: "GDF",
        MeshFormat.MSH: "MSH",
        MeshFormat.MAR: "MAR",
    }
    return mapping.get(fmt)


def _apply_symmetry(cpt, mesh, plane: str):
    """Wrap mesh with reflection symmetry."""
    plane_map = {
        "xOz": cpt.xOz_Plane,
        "yOz": cpt.yOz_Plane,
    }
    sym_plane = plane_map.get(plane)
    if sym_plane is None:
        raise ValueError(f"Unknown symmetry plane: {plane}. Use 'xOz' or 'yOz'.")
    return cpt.ReflectionSymmetricMesh(mesh, sym_plane, name=mesh.name)
