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
    STEEL_GRADE_A,
    STEEL_AH36,
    STEEL_EH40,
    MARINE_GRADES,
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
    # Marine hull grades
    'STEEL_GRADE_A',
    'STEEL_AH36',
    'STEEL_EH40',
    'MARINE_GRADES',
    # Calculators
    'StressCalculator',
    'PlateBucklingAnalyzer',
    'ColumnBucklingAnalyzer',
    'MemberCapacityChecker',
]

__version__ = '1.0.0'
