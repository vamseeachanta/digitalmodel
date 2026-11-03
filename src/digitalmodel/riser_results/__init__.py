# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Solver-independent riser results exchange and validation."""

from .schema import read_document, validate_document

__all__ = ["read_document", "validate_document"]
