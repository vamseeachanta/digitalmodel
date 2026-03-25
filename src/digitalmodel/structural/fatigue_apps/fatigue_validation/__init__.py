"""
ABOUTME: Fatigue analysis input file validation system
ABOUTME: Provides multi-level validation for fatigue analysis configurations and S-N curves

This module provides comprehensive validation for fatigue analysis input files:

- Level 1: YAML syntax and S-N curve data validation
- Level 2: Fatigue calculation engine validation
- Level 3: S-N curve compliance, DFF ranges, SCF limits

Usage:
    from digitalmodel.structural.fatigue_apps.fatigue_validation import FatigueValidator

    validator = FatigueValidator()
    results = validator.validate_all('/path/to/fatigue_config.yml')
"""

from .validator import FatigueValidator
from .models import FatigueValidationResult, FatigueValidationConfig

__version__ = '1.0.0'
__all__ = [
    'FatigueValidator',
    'FatigueValidationResult',
    'FatigueValidationConfig'
]
