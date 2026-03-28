"""
ABOUTME: Mesh adapter for importing geometry into Capytaine FloatingBody objects.
ABOUTME: Supports STL, GDF, MSH, meshio formats and predefined test shapes.
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


def create_floating_body(config: BodyConfig):
    """Create a Capytaine FloatingBody from a BodyConfig.

    Args:
        config: Body configuration with mesh source and DOF specification.

    Returns:
        capytaine.FloatingBody configured with DOFs and properties.
    """
    cpt = _import_capytaine()

    if config.mesh.predefined_shape is not None:
        mesh = _create_predefined_mesh(cpt, config.mesh.predefined_shape, config.mesh.shape_params)
    elif config.mesh.path is not None:
        mesh = _load_mesh_file(cpt, config.mesh.path, config.mesh.format)
    else:
        raise ValueError("Must specify either mesh.path or mesh.predefined_shape")

    # Apply symmetry if requested
    if config.mesh.use_symmetry and config.mesh.symmetry_plane:
        mesh = _apply_symmetry(cpt, mesh, config.mesh.symmetry_plane)

    # Create body with all rigid body DOFs, then remove unwanted ones.
    # Capytaine 2.3+ returns a RigidBodyDofsPlaceholder from rigid_body_dofs()
    # that is resolved inside FloatingBody.__init__.
    body = cpt.FloatingBody(
        mesh=mesh,
        dofs=cpt.rigid_body_dofs(rotation_center=config.rotation_center_or_com),
        center_of_mass=config.center_of_mass,
        name=config.mesh.name,
    )

    # Filter to requested DOFs
    requested_dof_names = {d.value for d in config.dofs}
    for dof_name in list(body.dofs.keys()):
        if dof_name not in requested_dof_names:
            del body.dofs[dof_name]

    # Keep only the immersed part (panels below z=0)
    body.keep_immersed_part()

    # Lid mesh for irregular frequency removal
    if config.use_lid:
        lid_z = config.lid_z if config.lid_z is not None else 0.0
        try:
            body.add_lid(z=lid_z)
            logger.info("Added lid mesh at z=%.2f for irregular frequency removal", lid_z)
        except Exception:
            logger.warning("Failed to add lid mesh — continuing without it")

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
