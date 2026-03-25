#!/usr/bin/env python3
"""
ABOUTME: Mooring Analysis Module - Design and analyze mooring systems including
CALM/SALM buoys, spread mooring, catenary analysis, and safety factor verification.

This module provides comprehensive mooring design capabilities per DNV-OS-E301,
API RP 2SK, and ABS standards.
"""

__version__ = '1.0.0'

# Data models
from .models import (
    # Enumerations
    MooringType,
    LineType,
    ConditionType,

    # Core data structures
    MooringLineProperties,
    AnchorProperties,
    MooringLine,
    EnvironmentalConditions,
    VesselParticulars,
    MooringSystem,

    # Results
    CatenaryResult,
    StiffnessResult,
    EnvironmentalLoads,
    DesignLoadCase,
    MooringDesignResult,

    # Material library
    CHAIN_R3_84MM,
    CHAIN_R3_102MM,
    CHAIN_R4_84MM,
    WIRE_76MM,
    POLYESTER_140MM,
    POLYESTER_180MM,
    MOORING_MATERIALS,
    get_material,
)

# Analysis classes
from .catenary import CatenaryAnalyzer
from .designer import MooringDesigner
from .orcaflex_generator import OrcaFlexModelGenerator

__all__ = [
    # Version
    '__version__',

    # Enumerations
    'MooringType',
    'LineType',
    'ConditionType',

    # Models
    'MooringLineProperties',
    'AnchorProperties',
    'MooringLine',
    'EnvironmentalConditions',
    'VesselParticulars',
    'MooringSystem',

    # Results
    'CatenaryResult',
    'StiffnessResult',
    'EnvironmentalLoads',
    'DesignLoadCase',
    'MooringDesignResult',

    # Analyzers
    'CatenaryAnalyzer',
    'MooringDesigner',
    'OrcaFlexModelGenerator',

    # Material library
    'CHAIN_R3_84MM',
    'CHAIN_R3_102MM',
    'CHAIN_R4_84MM',
    'WIRE_76MM',
    'POLYESTER_140MM',
    'POLYESTER_180MM',
    'MOORING_MATERIALS',
    'get_material',
]
