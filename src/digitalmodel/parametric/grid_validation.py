"""Validate the declared Cartesian grid before interpolation reshapes rows."""
import itertools

import numpy as np


def validate_grid(axes, grid, response):
    names = [ax.name for ax in axes]
    if len(names) != len(set(names)) or response not in grid:
        raise ValueError("invalid grid columns")
    expected_values = [ax.values if ax.is_categorical else ax.grid for ax in axes]
    if any(not values or len(values) != len(set(values)) for values in expected_values):
        raise ValueError("grid axes must contain unique knots")
    expected = set(itertools.product(*expected_values))
    try:
        actual = list(grid[names].itertuples(index=False, name=None)) if names else [()] * len(grid)
    except KeyError as exc:
        raise ValueError("missing grid axis column") from exc
    if len(actual) != len(expected) or set(actual) != expected:
        raise ValueError("grid requires exactly one row per declared knot combination")
    if not np.isfinite(grid[response].to_numpy(dtype=float)).all():
        raise ValueError("grid responses must be finite")
