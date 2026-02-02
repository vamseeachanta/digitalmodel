"""Validation Module for Marine Engineering Computations.

This module provides comprehensive validation tools for Phase 2 marine
engineering implementations, including catenary mooring solvers and
hydrodynamic analysis results.

Modules:
    validate_phase2: Complete Phase 2 validation suite
    validate_catenary: Catenary mooring solver validation

Example:
    >>> from digitalmodel.marine_analysis.validation import validate_phase2
    >>> results = validate_phase2.run_full_validation()
    >>> print(results.generate_report())
"""

from .validate_phase2 import *
from .validate_catenary import *

__all__ = [
    'validate_phase2',
    'validate_catenary'
]
