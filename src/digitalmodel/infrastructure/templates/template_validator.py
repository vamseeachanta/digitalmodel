#!/usr/bin/env python3
"""Backward-compatibility shim for template_validator (WRK-415 Phase 2A).

The template validator has moved to digitalmodel.infrastructure.validation.
This shim re-exports everything from the new location for existing callers.
"""
import warnings

warnings.warn(
    "digitalmodel.infrastructure.templates.template_validator is deprecated. "
    "Import from digitalmodel.infrastructure.validation.template_validator instead. "
    "Will be removed in a future release.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.validation.template_validator import (
    ValidationResult,
    ValidationRule,
    RequiredFieldsRule,
    ValueRangeRule,
    EngineeringValidator,
    TemplateValidator,
    validate_template,
    create_validation_report,
)

__all__ = [
    "ValidationResult",
    "ValidationRule",
    "RequiredFieldsRule",
    "ValueRangeRule",
    "EngineeringValidator",
    "TemplateValidator",
    "validate_template",
    "create_validation_report",
]
