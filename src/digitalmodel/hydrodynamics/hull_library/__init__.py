"""
ABOUTME: Hull profile library for defining hull shapes as line profiles,
generating panel meshes on demand, producing schematics, and linking
hull->size->diffraction->RAOs->accelerations in a queryable catalog.
"""

__version__ = "0.1.0"

from .profile_schema import HullType, HullStation, HullProfile
from .mesh_generator import MeshGeneratorConfig, HullMeshGenerator, coarsen_mesh
from .schematic_generator import SchematicGenerator
from .catalog import (
    SeaStateDefinition,
    HullVariation,
    MotionResponse,
    HullCatalogEntry,
    HullCatalog,
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
]
