"""Reservoir Engineering Analysis Module.

This module provides comprehensive tools for reservoir engineering calculations,
modeling, and analysis. It includes:

- Rock and fluid property calculations
- Reservoir modeling and simulation tools
- Data analysis and visualization capabilities
- Stratigraphic correlation and visualization

Examples:
    >>> from digitalmodel.marine_ops.reservoir import ReservoirProperties, ReservoirModel
    >>> props = ReservoirProperties(porosity=0.2, permeability=100)
    >>> model = ReservoirModel(properties=props)
"""

from .properties import (
    ReservoirProperties,
    RockProperties,
    FluidProperties,
    PVTProperties,
    calculate_porosity,
    calculate_permeability,
    calculate_water_saturation,
)

from .modeling import (
    ReservoirModel,
    MaterialBalance,
    WellTestAnalysis,
    ReservoirSimulation,
    ProductionForecast,
    ReservoirCharacterization,
)

from .analysis import (
    ReservoirAnalysis,
    StratigraphicAnalysis,
    LogAnalysis,
    CoreAnalysis,
    WellCorrelation,
    ReservoirMapping,
    PerformanceAnalysis,
)

__all__ = [
    # Properties
    'ReservoirProperties',
    'RockProperties', 
    'FluidProperties',
    'PVTProperties',
    'calculate_porosity',
    'calculate_permeability',
    'calculate_water_saturation',
    
    # Modeling
    'ReservoirModel',
    'MaterialBalance',
    'WellTestAnalysis',
    'ReservoirSimulation',
    'ProductionForecast',
    'ReservoirCharacterization',
    
    # Analysis
    'ReservoirAnalysis',
    'StratigraphicAnalysis',
    'LogAnalysis',
    'CoreAnalysis',
    'WellCorrelation',
    'ReservoirMapping',
    'PerformanceAnalysis',
]

__version__ = "1.0.0"
__author__ = "DigitalModel Reservoir Team"
