# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Validation helpers for subsea cross-section definitions."""

from __future__ import annotations

from typing import Any

from pydantic import ValidationError

from digitalmodel.subsea.cross_sections.schema import (
    CrossSectionDefinition,
    ValidationIssue,
    ValidationReport,
)


def _error_to_issue(error: dict[str, Any]) -> ValidationIssue:
    loc = error.get("loc") or ("definition",)
    path = ".".join(str(part) for part in loc) if loc else "definition"
    code = str(error.get("type", "validation_error"))
    message = str(error.get("msg", "validation error"))
    return ValidationIssue(code=code, path=path, message=message, severity="error")


def validate_cross_section(definition: CrossSectionDefinition | dict[str, Any]) -> ValidationReport:
    """Validate a cross-section object or mapping and return a stable report."""

    try:
        if isinstance(definition, CrossSectionDefinition):
            # Models are mutable, including nested UnitValue instances. Validate
            # their current data rather than trusting a previously accepted object.
            definition = definition.model_dump()
        parsed = CrossSectionDefinition.model_validate(definition)
    except ValidationError as exc:
        errors = [_error_to_issue(error) for error in exc.errors()]
        return ValidationReport(
            is_valid=False,
            errors=errors,
            warnings=[],
            summary=f"{len(errors)} validation error(s)",
        )
    return ValidationReport(
        is_valid=True,
        errors=[],
        warnings=[],
        summary=f"Cross-section {parsed.id} is valid",
    )
