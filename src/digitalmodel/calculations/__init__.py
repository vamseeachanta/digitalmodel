# -*- coding: utf-8 -*-
"""
Calculations module for digitalmodel package.

This module contains various engineering calculations and computational methods.
"""

__version__ = '0.1.0'
__author__ = 'Vamsee Achanta'

# Import plate buckling calculations
try:
    from .plate_buckling import (
        ElasticBucklingCalculator, SlendernessCalculator, UltimateStrengthCalculator,
        UsageFactorCalculator, BucklingCoefficients, PlateEdgeCondition,
        StressDirection, calculate_plate_buckling_212
    )

    _PLATE_BUCKLING_AVAILABLE = True
except ImportError as e:
    _PLATE_BUCKLING_AVAILABLE = False
    import warnings
    warnings.warn(f"Plate buckling calculation modules not available: {e}", ImportWarning)

# Try to import existing calculation modules
try:
    from .pipe_properties import *
    from .stress_calculations import *

    _PIPE_CALCULATIONS_AVAILABLE = True
except ImportError as e:
    _PIPE_CALCULATIONS_AVAILABLE = False
    # Silently ignore since these might not exist yet

__all__ = []

# Add plate buckling exports if available
if _PLATE_BUCKLING_AVAILABLE:
    __all__.extend([
        'ElasticBucklingCalculator', 'SlendernessCalculator', 'UltimateStrengthCalculator',
        'UsageFactorCalculator', 'BucklingCoefficients', 'PlateEdgeCondition',
        'StressDirection', 'calculate_plate_buckling_212'
    ])