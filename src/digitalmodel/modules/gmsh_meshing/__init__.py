#!/usr/bin/env python3
"""
ABOUTME: GMSH meshing module for finite element mesh generation, quality assessment,
and export to multiple analysis formats (ANSYS, OpenFOAM, Abaqus, VTK).
"""

__version__ = "1.0.0"

from .models import (
    MeshQuality,
    MeshStatistics,
    GeometryType,
    ElementType,
    MeshAlgorithm,
)
from .mesh_generator import GMSHMeshGenerator
from .quality_analyzer import MeshQualityAnalyzer

__all__ = [
    "MeshQuality",
    "MeshStatistics",
    "GeometryType",
    "ElementType",
    "MeshAlgorithm",
    "GMSHMeshGenerator",
    "MeshQualityAnalyzer",
]
