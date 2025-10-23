"""
ABOUTME: AQWA hydrodynamic analysis input file validation system
ABOUTME: Provides multi-level validation for AQWA YAML configuration files

This module provides comprehensive validation for AQWA hydrodynamic analysis input files:

- Level 1: YAML syntax and structure validation
- Level 2: AQWA software availability and DAT/LIS file validation
- Level 3: Hydrodynamic coefficient ranges and RAO reasonableness checks

Usage:
    from digitalmodel.modules.aqwa.aqwa_validation import AQWAValidator

    validator = AQWAValidator()
    results = validator.validate_all('/path/to/aqwa_config.yml')
"""

from .validator import AQWAValidator
from .models import AQWAValidationResult, AQWAValidationConfig

__version__ = '1.0.0'
__all__ = [
    'AQWAValidator',
    'AQWAValidationResult',
    'AQWAValidationConfig'
]
