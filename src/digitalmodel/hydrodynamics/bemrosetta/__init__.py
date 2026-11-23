"""
BEMRosetta Integration Module

Provides Python interface for hydrodynamic coefficient conversion,
with focus on AQWA -> OrcaFlex workflow.

Features:
- Parse AQWA diffraction analysis outputs (.LIS, .DAT, .QTF)
- Extract RAOs, added mass, damping, and QTF coefficients
- Validate data quality (symmetry, causality, physical limits)
- Export to OrcaFlex YAML format
- Mesh format conversion (GDF, DAT, STL)

Optional: BEMRosetta_cl.exe integration for extended features.

Example:
    from digitalmodel.hydrodynamics.bemrosetta import (
        AQWAParser,
        OrcaFlexConverter,
        is_bemrosetta_available,
    )

    # Parse AQWA results
    parser = AQWAParser()
    results = parser.parse("analysis.LIS")

    # Convert to OrcaFlex format
    converter = OrcaFlexConverter()
    converter.convert(results, Path("output/"))
"""

__version__ = "1.0.0"
__author__ = "Digital Model Team"

from pathlib import Path

# Module paths
MODULE_DIR = Path(__file__).parent
CONFIG_DIR = MODULE_DIR / "configs"
TEMPLATES_DIR = MODULE_DIR / "templates"

# Feature detection
from .core.runner import is_bemrosetta_available, get_runner, BEMRosettaRunner

# Exceptions
from .core.exceptions import (
    BEMRosettaError,
    ParserError,
    ConverterError,
    ValidationError,
    MeshError,
    ExecutableNotFoundError,
)

# Interfaces
from .core.interfaces import (
    ParserInterface,
    ConverterInterface,
    MeshHandlerInterface,
)

# Data models
from .models import (
    BEMSolverMetadata,
    QTFData,
    QTFType,
    PanelMesh,
    MeshFormat,
    MeshQualityReport,
    ConversionResult,
)

# Parsers
from .parsers import AQWAParser, AQWAParseResult, QTFParser

# Converters
from .converters import OrcaFlexConverter, convert_to_orcaflex

# Mesh handlers
from .mesh import GDFHandler, DATHandler, STLHandler, convert_mesh

# Validators
from .validators import (
    CoefficientValidator,
    CausalityChecker,
    validate_coefficients,
    check_causality,
)

__all__ = [
    # Version
    "__version__",

    # Module paths
    "MODULE_DIR",
    "CONFIG_DIR",
    "TEMPLATES_DIR",

    # Feature detection
    "is_bemrosetta_available",
    "get_runner",
    "BEMRosettaRunner",

    # Exceptions
    "BEMRosettaError",
    "ParserError",
    "ConverterError",
    "ValidationError",
    "MeshError",
    "ExecutableNotFoundError",

    # Interfaces
    "ParserInterface",
    "ConverterInterface",
    "MeshHandlerInterface",

    # Data models
    "BEMSolverMetadata",
    "QTFData",
    "QTFType",
    "PanelMesh",
    "MeshFormat",
    "MeshQualityReport",
    "ConversionResult",

    # Parsers
    "AQWAParser",
    "AQWAParseResult",
    "QTFParser",

    # Converters
    "OrcaFlexConverter",
    "convert_to_orcaflex",

    # Mesh handlers
    "GDFHandler",
    "DATHandler",
    "STLHandler",
    "convert_mesh",

    # Validators
    "CoefficientValidator",
    "CausalityChecker",
    "validate_coefficients",
    "check_causality",
]


def get_module_info() -> dict:
    """Get module information and feature status.

    Returns:
        dict: Module information including version and feature availability.
    """
    return {
        "name": "bemrosetta",
        "version": __version__,
        "bemrosetta_executable_available": is_bemrosetta_available(),
        "supported_input_formats": ["AQWA (.LIS, .DAT, .QTF)"],
        "supported_output_formats": ["OrcaFlex (YAML, CSV)"],
        "supported_mesh_formats": ["GDF", "DAT", "STL"],
    }
