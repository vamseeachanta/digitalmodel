"""Choose the curved coordinate when a holdout is off-grid on several axes."""
import math

from digitalmodel.parametric.atlas import _interval_index


def refinement_point(atlas, point, response):
    actual = response(point)
    transform = math.log10 if atlas.physics == "log_log" else float
    scores = []
    for axis in atlas.continuous_axes:
        value = point[axis.name]
        if any(math.isclose(value, knot, rel_tol=1e-9, abs_tol=1e-12) for knot in axis.grid):
            continue
        i = _interval_index(axis.grid, value)
        lo, hi = axis.grid[i:i + 2]
        coord = math.log10 if axis.scale == "log" else float
        weight = (coord(value) - coord(lo)) / (coord(hi) - coord(lo))
        left = transform(response({**point, axis.name: lo}))
        right = transform(response({**point, axis.name: hi}))
        error = abs((1 - weight) * left + weight * right - transform(actual))
        mid = math.sqrt(lo * hi) if axis.scale == "log" else (lo + hi) / 2
        scores.append((error, axis.name, mid))
    if not scores:
        return point
    _, name, mid = max(scores)
    # Only the selected coordinate is off-knot, preserving _refine_axis's API.
    return {axis.name: mid if axis.name == name else axis.grid[0]
            for axis in atlas.continuous_axes}
