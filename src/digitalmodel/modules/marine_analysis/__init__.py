"""Marine Analysis Module - Comprehensive Marine Engineering Tools.

This module provides comprehensive tools for marine engineering analysis including:
- RAO reading and processing (AQWA, OrcaFlex)
- Performance profiling and optimization
- Data extraction (OCIMF, hydro coefficients, mooring)
- Validation and verification tools
- Visualization and reporting

Submodules:
    profiling: Performance analysis and optimization
    extraction: Data extraction from various sources
    validation: Validation and verification tools
    visualization: Marine engineering data visualization
    analysis: General analysis utilities
"""

# Main unified interface (v2.0 - NEW)
from .unified_rao_reader import (
    UnifiedRAOReader,
    RAOReaderError,
    read_rao_file
)

# Data models (v2.0 - NEW)
from .models.rao_data import (
    RAOData,
    RAOType,
    DisplacementRAO,
    VelocityRAO,
    AccelerationRAO,
    UnifiedRAOData,
    DOFData,
    RAOMetadata,
    SourceFormat
)

# Parsers (v2.0 - NEW)
from .parsers.aqwa_lis_parser import AQWALISParser, AQWAParseError
from .parsers.orcaflex_yml_parser import OrcaFlexYMLParser, OrcaFlexParseError

# Visualization (v2.0 - NEW)
# Note: RAOPlotter is part of rao_plotter module, not visualization submodule
try:
    from .visualization.rao_plotter import RAOPlotter
except ImportError:
    RAOPlotter = None  # Optional dependency

# Legacy compatibility
from .rao_processor import RAODataProcessor, RAOImportError
from .rao_validators import RAODataValidators, ValidationReport
from .rao_interpolator import RAOInterpolator
from .aqwa_reader import AQWAReader
from .orcaflex_reader import OrcaFlexReader

# Submodule imports (v2.1 - NEW)
from . import profiling
from . import extraction
from . import validation
from . import visualization as viz_tools
from . import analysis

# Phase 1-3 marine engineering modules (v2.2 - CONSOLIDATED)
from . import catenary
from . import environmental_loading
from . import hydrodynamic_coefficients
from . import wave_spectra

__all__ = [
    # Core RAO functionality
    'UnifiedRAOReader', 'RAOReaderError', 'read_rao_file',
    'RAOData', 'RAOType', 'DisplacementRAO', 'VelocityRAO', 'AccelerationRAO',
    'UnifiedRAOData', 'DOFData', 'RAOMetadata', 'SourceFormat',
    'AQWALISParser', 'AQWAParseError', 'OrcaFlexYMLParser', 'OrcaFlexParseError',
    'RAOPlotter',
    'RAODataProcessor', 'RAOImportError', 'RAODataValidators', 'ValidationReport',
    'RAOInterpolator', 'AQWAReader', 'OrcaFlexReader',
    # Submodules
    'profiling', 'extraction', 'validation', 'viz_tools', 'analysis',
    # Marine engineering modules
    'catenary', 'environmental_loading', 'hydrodynamic_coefficients', 'wave_spectra'
]

__version__ = '2.2.0'
