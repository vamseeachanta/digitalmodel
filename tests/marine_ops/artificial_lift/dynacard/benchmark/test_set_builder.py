# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compatibility import for the package-owned labelled-card builder."""

from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.test_set_builder import (
    LabelledCard,
    build_hold_out_test_set,
)

__all__ = ["LabelledCard", "build_hold_out_test_set"]
