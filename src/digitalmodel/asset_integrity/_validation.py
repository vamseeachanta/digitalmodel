# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Input-domain guards shared by the public integrity assessments."""

import math
from numbers import Real

import numpy as np


def finite_number(value, name, *, positive=True):
    """Require a measured finite number, without coercing strings or booleans."""
    if (isinstance(value, (bool, np.bool_)) or not isinstance(value, Real)
            or not math.isfinite(value)
            or (value <= 0 if positive else value < 0)):
        domain = "positive" if positive else "nonnegative"
        raise ValueError(f"{name} must be a finite {domain} number")
    return value


def measured_grid(grid):
    """Require full coverage by finite positive wall readings; retain input."""
    values = grid.to_numpy()
    if (values.ndim != 2 or not values.size or values.dtype.kind not in "iuf"
            or not np.isfinite(values).all() or (values <= 0).any()):
        raise ValueError(
            "Wall readings require a nonempty grid of finite positive numeric "
            "measurements with complete coverage; unmeasured cells require resurvey"
        )
    return values
