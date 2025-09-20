"""
Fatigue Analysis Module

This module provides comprehensive fatigue analysis capabilities for offshore structures,
implementing various methodologies including rainflow counting, S-N curve analysis, and
spectral fatigue assessment.

Key Features:
- Reference seastate scaling methodology for strut foundations
- ASTM E1049 rainflow counting algorithm
- S-N curve fatigue damage calculation with Miner's rule
- Multiple vessel configuration support with operational weighting
- Tension-to-stress conversion with lookup tables
- CLI interface for batch processing

Usage:
    # Command line interface
    python -m digitalmodel.modules.fatigue_analysis --help
    
    # Python API
    from digitalmodel.modules.fatigue_analysis import ReferenceSeaStateProcessor
    processor = ReferenceSeaStateProcessor(data_path="./data")
    processor.run_analysis()
"""

from .rainflow_counter import RainflowCounter, rainflow_count
from .fatigue_damage_calculator import (
    FatigueDamageCalculator,
    SNCurveParameters,
    calculate_combined_damage
)
from .reference_seastate_processor import (
    ReferenceSeaStateProcessor,
    Configuration,
    FatigueCondition,
    FatigueResult,
    TensionToStressConverter
)
from .visualizer import FatigueVisualizer, generate_all_visualizations

__all__ = [
    # Rainflow counting
    'RainflowCounter',
    'rainflow_count',
    # Fatigue damage calculation
    'FatigueDamageCalculator', 
    'SNCurveParameters',
    'calculate_combined_damage',
    # Reference seastate processing
    'ReferenceSeaStateProcessor',
    'Configuration',
    'FatigueCondition',
    'FatigueResult',
    'TensionToStressConverter',
    # Visualization
    'FatigueVisualizer',
    'generate_all_visualizations'
]

__version__ = '1.2.0'