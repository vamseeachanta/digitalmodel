"""
Structural Analysis Module

Comprehensive structural analysis for offshore and marine structures including:
- Von Mises stress calculations
- Plate buckling checks (DNV-RP-C201)
- Column buckling analysis (Eurocode 3)
- Member capacity verification
- Combined loading assessment
- Safety factor reporting

Complies with DNV, API, ISO, and Eurocode 3 standards.
"""

from .models import (
    StressState,
    MaterialProperties,
    PlateGeometry,
    BucklingResult,
    CapacityResult,
    STEEL_S355,
    STEEL_S420,
    STEEL_S275,
)

from .stress_calculator import StressCalculator

from .buckling import (
    PlateBucklingAnalyzer,
    ColumnBucklingAnalyzer,
)

from .capacity import MemberCapacityChecker

__all__ = [
    # Models
    'StressState',
    'MaterialProperties',
    'PlateGeometry',
    'BucklingResult',
    'CapacityResult',
    # Materials
    'STEEL_S355',
    'STEEL_S420',
    'STEEL_S275',
    # Calculators
    'StressCalculator',
    'PlateBucklingAnalyzer',
    'ColumnBucklingAnalyzer',
    'MemberCapacityChecker',
]

__version__ = '1.0.0'
