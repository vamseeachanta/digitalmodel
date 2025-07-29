"""Marine Analysis Module for 6-DOF Motion Analysis.

This module provides comprehensive marine analysis capabilities including:
- RAO data import and processing from multiple sources (AQWA, OrcaFlex)
- 6-DOF motion analysis in frequency and time domain
- Data validation and quality checking
- DataFrame output with manual verification support
"""

from .rao_processor import RAODataProcessor, RAOData
from .aqwa_reader import AQWAReader
from .orcaflex_reader import OrcaFlexReader
from .rao_interpolator import RAOInterpolator
from .rao_validators import RAODataValidators, ValidationReport

__all__ = [
    'RAODataProcessor',
    'RAOData',
    'AQWAReader',
    'OrcaFlexReader',
    'RAOInterpolator',
    'RAODataValidators',
    'ValidationReport'
]

__version__ = '1.0.0'