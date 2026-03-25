"""BEMRosetta Mesh Module

Mesh format handlers for panel mesh conversion:
- GDF: WAMIT geometry definition format
- DAT: AQWA/NEMOH panel mesh format
- STL: Stereolithography format for visualization

Each handler implements MeshHandlerInterface for read/write operations.
The convert_mesh utility provides format conversion between supported types.
"""

from .mesh_handler import BaseMeshHandler, convert_mesh
from .gdf_handler import GDFHandler
from .dat_handler import DATHandler
from .stl_handler import STLHandler

__all__ = [
    "BaseMeshHandler",
    "GDFHandler",
    "DATHandler",
    "STLHandler",
    "convert_mesh",
]
