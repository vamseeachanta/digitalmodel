"""
Diffraction Analysis - Unified Output Module

Standardized schemas and converters for AQWA and OrcaWave diffraction results.

Phase 2: Output Standardization (Complete)

This module provides:
- Unified data schemas for RAO, added mass, and damping coefficients
- Converters from AQWA and OrcaWave to unified format
- OrcaFlex export in multiple formats (YAML, CSV, Excel)
- Comprehensive validation of results completeness and physical validity
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

from digitalmodel.modules.diffraction.output_validator import (
    OutputValidator,
    validate_results
)

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
]

__version__ = "2.0.0"
__author__ = "Digital Model Development Team"
__status__ = "Phase 2 Complete - Output Standardization"
