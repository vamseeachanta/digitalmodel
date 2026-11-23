"""
BEMRosetta Data Models Module

Pydantic data models for hydrodynamic analysis data:
- BEMSolverMetadata: Solver information, analysis parameters
- QTFData: Quadratic transfer function coefficients
- QTFType: QTF component enumeration (sum/difference frequency)
- PanelMesh: Panel mesh geometry representation
- MeshFormat: Supported mesh format enumeration
- MeshQualityReport: Mesh quality metrics and validation results
- ConversionResult: Conversion operation result with status and outputs
"""

from .solver_metadata import BEMSolverMetadata
from .qtf_data import QTFData, QTFType
from .mesh_models import PanelMesh, MeshFormat, MeshQualityReport
from .conversion_result import ConversionResult

__all__ = [
    "BEMSolverMetadata",
    "QTFData",
    "QTFType",
    "PanelMesh",
    "MeshFormat",
    "MeshQualityReport",
    "ConversionResult",
]
