"""
ABOUTME: Hull profile library for defining hull shapes as line profiles,
generating panel meshes on demand, producing schematics, and linking
hull->size->diffraction->RAOs->accelerations in a queryable catalog.
"""

__version__ = "0.1.0"

from .profile_schema import HullType, HullStation, HullProfile
from .mesh_generator import MeshGeneratorConfig, HullMeshGenerator
from .coarsen_mesh import coarsen_mesh
from .schematic_generator import SchematicGenerator
from .catalog import (
    SeaStateDefinition,
    HullVariation,
    MotionResponse,
    HullCatalogEntry,
    HullCatalog,
)
from .panel_catalog import PanelFormat, RaoReference, PanelCatalogEntry, PanelCatalog
from .rao_registry import RaoRegistry
from .mesh_scaler import (
    ScaleDimensions,
    ScaleResult,
    scale_mesh_uniform,
    scale_mesh_parametric,
    scale_mesh_to_target,
    validate_scaled_mesh,
    export_scaled_gdf,
)

__all__ = [
    # Profile schema
    "HullType",
    "HullStation",
    "HullProfile",
    # Mesh generation
    "MeshGeneratorConfig",
    "HullMeshGenerator",
    "coarsen_mesh",
    # Schematics
    "SchematicGenerator",
    # Catalog
    "SeaStateDefinition",
    "HullVariation",
    "MotionResponse",
    "HullCatalogEntry",
    "HullCatalog",
    # Panel catalog
    "PanelFormat",
    "RaoReference",
    "PanelCatalogEntry",
    "PanelCatalog",
    # RAO registry
    "RaoRegistry",
    # Mesh scaler
    "ScaleDimensions",
    "ScaleResult",
    "scale_mesh_uniform",
    "scale_mesh_parametric",
    "scale_mesh_to_target",
    "validate_scaled_mesh",
    "export_scaled_gdf",
]
