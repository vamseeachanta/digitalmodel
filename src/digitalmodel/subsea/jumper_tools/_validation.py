# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Shared validation for explicitly supplied SI inputs."""

import math


def finite_value(name: str, value: float, *, positive: bool = False) -> None:
    """Reject nonfinite, negative and optionally zero values."""
    if isinstance(value, bool) or not math.isfinite(value):
        raise ValueError(f"{name} must be a finite number")
    if value < 0 or (positive and value == 0):
        bound = "positive" if positive else "nonnegative"
        raise ValueError(f"{name} must be {bound}")


def element_count(value: int) -> None:
    """Element counts are positive integers, excluding booleans."""
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise ValueError("element count must be a positive integer")
