# -*- coding: utf-8 -*-
"""
Analysis module initialization

This module initializes the analysis components for the digitalmodel package.
"""

__version__ = '0.1.0'
__author__ = 'Vamsee Achanta'

# Import plate capacity analysis modules
try:
    from .plate_capacity import (
        PlateProperties, AppliedLoads, BucklingConstants, BucklingResults,
        PlateBucklingAnalyzer, BoundaryCondition, create_plate_from_legacy_data,
        run_example_analysis
    )
    from .multi_plate_analyzer import (
        MultiPlateAnalyzer, PlateConfiguration, MultiPlateResults,
        create_legacy_multi_plate_analyzer
    )

    _PLATE_CAPACITY_AVAILABLE = True
except ImportError as e:
    _PLATE_CAPACITY_AVAILABLE = False
    import warnings
    warnings.warn(f"Plate capacity modules not available: {e}", ImportWarning)

# Try to import existing analysis modules
try:
    from .apistd2rd import *
    _APISTD2RD_AVAILABLE = True
except ImportError:
    _APISTD2RD_AVAILABLE = False

__all__ = []

# Add plate capacity exports if available
if _PLATE_CAPACITY_AVAILABLE:
    __all__.extend([
        'PlateProperties', 'AppliedLoads', 'BucklingConstants', 'BucklingResults',
        'PlateBucklingAnalyzer', 'BoundaryCondition', 'create_plate_from_legacy_data',
        'run_example_analysis', 'MultiPlateAnalyzer', 'PlateConfiguration',
        'MultiPlateResults', 'create_legacy_multi_plate_analyzer'
    ])