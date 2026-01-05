#!/usr/bin/env python3
"""
ABOUTME: Catenary Riser Module - Simple and lazy wave catenary riser analysis
with effective weight calculations and OrcaFlex model generation per DNV-OS-F201
and API RP 1111.
"""

__version__ = '1.0.0'

# Models
from .models import (
    # Enumerations
    RiserType,
    FluidType,

    # Material and fluid properties
    MaterialProperties,
    FluidProperties,

    # Configuration
    RiserConfiguration,
    BuoyancyModule,
    LazyWaveConfiguration,

    # Results
    CatenaryRiserResult,
    LazyWaveResult,
    EffectiveWeightResult,

    # Material library
    STEEL_API_5L_X65,
    STEEL_API_5L_X70,
    RISER_MATERIALS,
    get_material,

    # Fluid library
    SEAWATER,
    PRODUCTION_OIL,
    DRILLING_MUD,
    AIR,
    FLUIDS,
    get_fluid,
)

# Analyzers
from .simple_catenary import SimpleCatenaryAnalyzer
from .effective_weight import EffectiveWeightCalculator
from .lazy_wave import LazyWaveAnalyzer

__all__ = [
    # Version
    '__version__',

    # Enumerations
    'RiserType',
    'FluidType',

    # Models
    'MaterialProperties',
    'FluidProperties',
    'RiserConfiguration',
    'BuoyancyModule',
    'LazyWaveConfiguration',

    # Results
    'CatenaryRiserResult',
    'LazyWaveResult',
    'EffectiveWeightResult',

    # Analyzers
    'SimpleCatenaryAnalyzer',
    'EffectiveWeightCalculator',
    'LazyWaveAnalyzer',

    # Material library
    'STEEL_API_5L_X65',
    'STEEL_API_5L_X70',
    'RISER_MATERIALS',
    'get_material',

    # Fluid library
    'SEAWATER',
    'PRODUCTION_OIL',
    'DRILLING_MUD',
    'AIR',
    'FLUIDS',
    'get_fluid',
]
