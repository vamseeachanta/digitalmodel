# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Input contracts shared by asset-integrity screening calculations."""

import math
from numbers import Real


def finite_number(name, value, *, minimum=None, maximum=None, positive=False):
    """Reject unknown, non-finite or out-of-domain data without coercion."""
    if isinstance(value, bool) or not isinstance(value, Real) or not math.isfinite(value):
        raise ValueError(f"{name} must be a finite number")
    if positive and value <= 0:
        raise ValueError(f"{name} must be positive and finite")
    if minimum is not None and value < minimum:
        raise ValueError(f"{name} must be >= {minimum}")
    if maximum is not None and value > maximum:
        raise ValueError(f"{name} must be <= {maximum}")


def pressure_basis(strength, demand, *, safety_factor=None):
    """Capacity needs positive strength; acceptance needs a positive demand."""
    finite_number("material strength", strength, positive=True)
    if demand is not None:
        finite_number("pressure demand", demand, positive=True)
    if safety_factor is not None:
        finite_number("safety_factor", safety_factor, minimum=1)


class InapplicableAssessment(ValueError):
    """A numeric capacity cannot qualify acceptance outside method limits."""

    def __init__(self, applicability):
        self.applicability = applicability
        super().__init__(applicability.note)
