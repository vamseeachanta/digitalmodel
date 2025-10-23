"""
ABOUTME: OrcaFlex modular YAML input file validation system
ABOUTME: Provides multi-level validation with OrcaFlex API integration and physical consistency checks

This module provides comprehensive validation for OrcaFlex modular YAML input files:

- Level 1: YAML syntax and structure validation
- Level 2: OrcaFlex API loading and static analysis
- Level 3: Physical consistency and parameter range validation

Usage:
    from digitalmodel.modules.orcaflex.modular_input_validation import ModularInputValidator

    validator = ModularInputValidator()
    results = validator.validate_all('/path/to/base/file.yml')
"""

from .validator import ModularInputValidator
from .models import ValidationResult, ValidationLevel, ValidationStatus
from .config import ValidationConfig

__version__ = '1.0.0'
__all__ = [
    'ModularInputValidator',
    'ValidationResult',
    'ValidationLevel',
    'ValidationStatus',
    'ValidationConfig'
]
