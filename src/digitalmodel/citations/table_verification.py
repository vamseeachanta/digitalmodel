# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exact table comparisons bound to independently supplied quantity metadata.

The contract is issue #2621 and the existing Citation/CitedValue schema:
unit labels and source identity must agree and resolve before numbers are
compared. No conversion, numerical tolerance or engineering limit is inferred.
"""

from __future__ import annotations

from dataclasses import fields
import math
from pathlib import Path

from digitalmodel.citations.schema import Citation, CitedValue, validate_citation


class TableVerificationError(ValueError):
    """A table quantity disagrees with its independent reference contract."""


def verify_table_value(
    actual: CitedValue,
    expected: CitedValue,
    *,
    repo_root: Path | None = None,
) -> None:
    """Require exact units, citation metadata, source resolution and value.

    ``expected`` must come from an independent reference, including its unit
    and citation, rather than copying metadata from ``actual``. Every citation
    field is compared, including the note that may identify a row or basis.
    Matching citations are resolved on each call with ``validate_citation``;
    its resolution errors propagate. Resolution establishes frontmatter
    identity only, not authenticity of source bytes or existence of a section.

    Exact unit labels are the dimension contract for this bounded interface.
    Aliases and conversions require a separately justified caller operation.
    Both values must be finite real int/float scalars, excluding booleans.
    Explicit exceptions preserve all checks under optimized Python execution.
    """
    for quantity in (actual, expected):
        if not isinstance(quantity, CitedValue):
            raise TableVerificationError("a CitedValue is required")
        if not isinstance(quantity.units, str) or not quantity.units.strip():
            raise TableVerificationError("nonempty units are required")
    if actual.units != expected.units:
        raise TableVerificationError("table units mismatch")

    for quantity in (actual, expected):
        if not isinstance(quantity.citation, Citation):
            raise TableVerificationError("a Citation is required")
    for field in fields(Citation):
        if getattr(actual.citation, field.name) != getattr(expected.citation, field.name):
            raise TableVerificationError(f"table citation mismatch: {field.name}")
    # The citations are equal; resolve once, afresh for each comparison.
    validate_citation(expected.citation, repo_root=repo_root)

    for quantity in (actual, expected):
        value = quantity.value
        if isinstance(value, bool) or not isinstance(value, (int, float)):
            raise TableVerificationError("finite numeric table values are required")
        # Integers are finite without conversion to a potentially overflowing float.
        if isinstance(value, float) and not math.isfinite(value):
            raise TableVerificationError("finite numeric table values are required")
    if actual.value != expected.value:
        raise TableVerificationError("table value mismatch")
