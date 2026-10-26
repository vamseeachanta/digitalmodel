# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Small, offline YAML comparison policy with explicit omission evidence."""

from .taxonomy import (
    MISSING,
    REGISTRY,
    Diff,
    DiffCategory,
    Rule,
    classify_diff,
    is_semantically_equivalent,
)

__all__ = [
    "MISSING",
    "REGISTRY",
    "Diff",
    "DiffCategory",
    "Rule",
    "classify_diff",
    "is_semantically_equivalent",
]
