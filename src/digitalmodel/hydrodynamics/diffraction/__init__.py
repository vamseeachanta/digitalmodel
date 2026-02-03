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

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
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

from digitalmodel.hydrodynamics.diffraction.orcaflex_exporter import OrcaFlexExporter

from digitalmodel.hydrodynamics.diffraction.aqwa_converter import (
    AQWAConverter,
    convert_aqwa_results
)

# OrcaWave converter (requires OrcFxAPI)
try:
    from digitalmodel.hydrodynamics.diffraction.orcawave_converter import (
        OrcaWaveConverter,
        convert_orcawave_results
    )
    ORCAWAVE_AVAILABLE = True
except ImportError:
    ORCAWAVE_AVAILABLE = False
    # Provide placeholder classes when OrcFxAPI not available
    OrcaWaveConverter = None
    convert_orcawave_results = None

from digitalmodel.hydrodynamics.diffraction.output_validator import (
    OutputValidator,
    validate_results
)

# Phase 3: Automation + QA tools
from digitalmodel.hydrodynamics.diffraction.comparison_framework import (
    DiffractionComparator,
    compare_diffraction_results
)

from digitalmodel.hydrodynamics.diffraction.batch_processor import (
    BatchProcessor,
    BatchConfiguration,
    process_batch_from_config_file
)

from digitalmodel.hydrodynamics.diffraction.geometry_quality import (
    GeometryQualityChecker,
    GeometryQualityReport
)

# WRK-029: OrcaWave runner
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    OrcaWaveRunner,
    RunConfig,
    RunResult,
    RunStatus,
    run_orcawave,
)

# WRK-025: AQWA runner
from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
    AQWARunner,
    AQWARunConfig,
    AQWARunResult,
    AQWARunStatus,
    run_aqwa,
)

# WRK-027 + WRK-028: AQWA batch runner + postprocessing
from digitalmodel.hydrodynamics.diffraction.aqwa_result_extractor import (
    AQWAResultExtractor,
    AQWAExtractionResult,
)
from digitalmodel.hydrodynamics.diffraction.aqwa_batch_runner import (
    AQWABatchRunner,
    AQWABatchConfig,
    AQWABatchJobConfig,
    AQWABatchJobResult,
    AQWABatchReport,
    AQWAExecutionMode,
    run_aqwa_batch,
    run_aqwa_batch_from_specs,
)

# WRK-030: Batch runner + postprocessing
from digitalmodel.hydrodynamics.diffraction.result_extractor import (
    ResultExtractor,
    ExtractionResult,
)
from digitalmodel.hydrodynamics.diffraction.orcawave_batch_runner import (
    OrcaWaveBatchRunner,
    OrcaWaveBatchConfig,
    BatchJobConfig,
    BatchJobResult,
    OrcaWaveBatchReport,
    ExecutionMode,
    run_orcawave_batch,
    run_orcawave_batch_from_specs,
)
from digitalmodel.hydrodynamics.diffraction.rao_plotter import RAOPlotter
from digitalmodel.hydrodynamics.diffraction.polars_exporter import PolarsExporter

# Test utilities (optional - for testing without OrcFxAPI)
try:
    from digitalmodel.hydrodynamics.diffraction.orcawave_test_utilities import (
        MockDataGenerator,
        create_test_vessel
    )
    from digitalmodel.hydrodynamics.diffraction.orcawave_data_extraction import (
        OrcaWaveDataExtractor,
        extract_all_rao_data,
        extract_all_added_mass,
        extract_all_damping
    )
    TEST_UTILITIES_AVAILABLE = True
except ImportError:
    TEST_UTILITIES_AVAILABLE = False
    MockDataGenerator = None
    create_test_vessel = None
    OrcaWaveDataExtractor = None
    extract_all_rao_data = None
    extract_all_added_mass = None
    extract_all_damping = None

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
    'OrcaWaveDataExtractor',
    'extract_all_rao_data',
    'extract_all_added_mass',
    'extract_all_damping',
    'TEST_UTILITIES_AVAILABLE',

    # WRK-029: OrcaWave runner
    'OrcaWaveRunner',
    'RunConfig',
    'RunResult',
    'RunStatus',
    'run_orcawave',

    # WRK-025: AQWA runner
    'AQWARunner',
    'AQWARunConfig',
    'AQWARunResult',
    'AQWARunStatus',
    'run_aqwa',

    # WRK-027 + WRK-028: AQWA batch runner + postprocessing
    'AQWAResultExtractor',
    'AQWAExtractionResult',
    'AQWABatchRunner',
    'AQWABatchConfig',
    'AQWABatchJobConfig',
    'AQWABatchJobResult',
    'AQWABatchReport',
    'AQWAExecutionMode',
    'run_aqwa_batch',
    'run_aqwa_batch_from_specs',

    # WRK-030: Batch runner + postprocessing
    'ResultExtractor',
    'ExtractionResult',
    'OrcaWaveBatchRunner',
    'OrcaWaveBatchConfig',
    'BatchJobConfig',
    'BatchJobResult',
    'OrcaWaveBatchReport',
    'ExecutionMode',
    'run_orcawave_batch',
    'run_orcawave_batch_from_specs',
    'RAOPlotter',
    'PolarsExporter',
]

__version__ = "3.0.0"
__author__ = "Digital Model Development Team"
__status__ = "Phase 3 - Automation + QA (Complete)"
