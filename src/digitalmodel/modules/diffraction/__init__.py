"""
Diffraction Analysis - Unified Output Module

Standardized schemas and converters for AQWA and OrcaWave diffraction results.

Phase 3: Automation + QA (Complete)

This module provides:
- Unified data schemas for RAO, added mass, and damping coefficients
- Converters from AQWA and OrcaWave to unified format
- OrcaFlex export in multiple formats (YAML, CSV, Excel)
- Comprehensive validation of results completeness and physical validity
- Batch processing for multiple vessels/configurations
- AQWA vs OrcaWave comparison framework
- Geometry quality validation
- Test utilities for converter validation
"""

from digitalmodel.modules.diffraction.output_schemas import (
    # Main results containers
    DiffractionResults,
    RAOSet,
    AddedMassSet,
    DampingSet,

    # Component data structures
    RAOComponent,
    HydrodynamicMatrix,
    FrequencyData,
    HeadingData,

    # Enumerations
    DOF,
    Unit,

    # Validation functions
    validate_rao_completeness,
    validate_matrix_set,
    validate_diffraction_results
)

from digitalmodel.modules.diffraction.orcaflex_exporter import OrcaFlexExporter

from digitalmodel.modules.diffraction.aqwa_converter import (
    AQWAConverter,
    convert_aqwa_results
)

# OrcaWave converter (requires OrcFxAPI)
try:
    from digitalmodel.modules.diffraction.orcawave_converter import (
        OrcaWaveConverter,
        convert_orcawave_results
    )
    ORCAWAVE_AVAILABLE = True
except ImportError:
    ORCAWAVE_AVAILABLE = False
    # Provide placeholder classes when OrcFxAPI not available
    OrcaWaveConverter = None
    convert_orcawave_results = None

from digitalmodel.modules.diffraction.output_validator import (
    OutputValidator,
    validate_results
)

# Phase 3: Automation + QA tools
from digitalmodel.modules.diffraction.comparison_framework import (
    DiffractionComparator,
    compare_diffraction_results
)

from digitalmodel.modules.diffraction.batch_processor import (
    BatchProcessor,
    BatchConfiguration,
    process_batch_from_config_file
)

from digitalmodel.modules.diffraction.geometry_quality import (
    GeometryQualityChecker,
    GeometryQualityReport
)

# Test utilities (optional - for testing without OrcFxAPI)
try:
    from digitalmodel.modules.diffraction.orcawave_test_utilities import (
        MockDataGenerator,
        create_test_vessel
    )
    TEST_UTILITIES_AVAILABLE = True
except ImportError:
    TEST_UTILITIES_AVAILABLE = False
    MockDataGenerator = None
    create_test_vessel = None

__all__ = [
    # Results containers
    'DiffractionResults',
    'RAOSet',
    'AddedMassSet',
    'DampingSet',

    # Components
    'RAOComponent',
    'HydrodynamicMatrix',
    'FrequencyData',
    'HeadingData',

    # Enums
    'DOF',
    'Unit',

    # Validation
    'validate_rao_completeness',
    'validate_matrix_set',
    'validate_diffraction_results',
    'OutputValidator',
    'validate_results',

    # Export
    'OrcaFlexExporter',

    # Converters
    'AQWAConverter',
    'convert_aqwa_results',
    'OrcaWaveConverter',
    'convert_orcawave_results',
    'ORCAWAVE_AVAILABLE',

    # Phase 3: Automation + QA
    'DiffractionComparator',
    'compare_diffraction_results',
    'BatchProcessor',
    'BatchConfiguration',
    'process_batch_from_config_file',
    'GeometryQualityChecker',
    'GeometryQualityReport',
    'MockDataGenerator',
    'create_test_vessel',
    'TEST_UTILITIES_AVAILABLE',
]

__version__ = "3.0.0"
__author__ = "Digital Model Development Team"
__status__ = "Phase 3 - Automation + QA (Complete)"
