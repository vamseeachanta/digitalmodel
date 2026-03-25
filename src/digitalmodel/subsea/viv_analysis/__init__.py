#!/usr/bin/env python3
"""
ABOUTME: VIV Analysis Module - Vortex-Induced Vibration analysis for risers
and tubular members with natural frequency, vortex shedding, susceptibility
screening, and fatigue assessment per DNV-RP-C205 and DNV-RP-F105.
"""

__version__ = '1.0.0'

# Data models
from .models import (
    # Enumerations
    BoundaryCondition,
    CurrentProfileType,
    DesignCode,

    # Core data structures
    MaterialProperties,
    TubularMember,
    FluidProperties,
    CurrentProfile,

    # Results
    NaturalFrequencyResult,
    VortexSheddingResult,
    ReducedVelocityResult,
    VIVScreeningResult,
    VIVFatigueResult,

    # Material library
    STEEL_CARBON,
    STEEL_STAINLESS,
    TITANIUM,
    VIV_MATERIALS,
    get_material,

    # Constants
    STROUHAL_NUMBERS,
    LOCK_IN_RANGES,
    ADDED_MASS_COEFFICIENTS,
)

# Analysis classes
from .frequency_calculator import FrequencyCalculator
from .vortex_shedding import VortexSheddingAnalyzer
from .screening import VIVScreening
from .fatigue import VIVFatigueCalculator

__all__ = [
    # Version
    '__version__',

    # Enumerations
    'BoundaryCondition',
    'CurrentProfileType',
    'DesignCode',

    # Models
    'MaterialProperties',
    'TubularMember',
    'FluidProperties',
    'CurrentProfile',

    # Results
    'NaturalFrequencyResult',
    'VortexSheddingResult',
    'ReducedVelocityResult',
    'VIVScreeningResult',
    'VIVFatigueResult',

    # Analyzers
    'FrequencyCalculator',
    'VortexSheddingAnalyzer',
    'VIVScreening',
    'VIVFatigueCalculator',

    # Material library
    'STEEL_CARBON',
    'STEEL_STAINLESS',
    'TITANIUM',
    'VIV_MATERIALS',
    'get_material',

    # Constants
    'STROUHAL_NUMBERS',
    'LOCK_IN_RANGES',
    'ADDED_MASS_COEFFICIENTS',
]
