"""Fail-closed numerical and assessment-state guards for legacy entry points."""

import math
from numbers import Real

import numpy as np


def finite_number(value, name, *, minimum=0, maximum=None, positive=False):
    """Require a real finite value in the stated domain, without coercion."""
    if (isinstance(value, (bool, np.bool_)) or not isinstance(value, Real)
            or not math.isfinite(value) or value < minimum
            or (positive and value <= 0)
            or (maximum is not None and value > maximum)):
        raise ValueError(f"{name} must be finite and within its valid domain")
    return value


def bracketed_interpolate(target, samples, values):
    """Interpolate only within finite, strictly increasing sample coverage."""
    finite_number(target, "interpolation demand")
    x, y = np.asarray(samples), np.asarray(values)
    if x.ndim == 1 and y.ndim == 1:
        for value in (*x, *y):
            finite_number(value, 'interpolation sample')
    if (x.ndim != 1 or y.ndim != 1 or len(x) != len(y) or len(x) < 2
            or not np.isfinite(x).all() or not np.isfinite(y).all()
            or not (np.diff(x) > 0).all()
            or not ((np.diff(y) > 0).all() or (np.diff(y) < 0).all())
            or not x[0] <= target <= x[-1]):
        raise ValueError("finite ordered samples and a solved demand bracket are required")
    return float(np.interp(target, x, y))


def solve_bracket(target, samples, arguments, evaluate):
    """Refine an existing monotonic bracket and verify the physical residual."""
    bracketed_interpolate(target, samples, arguments)
    index = int(np.searchsorted(samples, target))
    if index == 0:
        result = arguments[0]
    else:
        left, right = arguments[index - 1], arguments[index]
        result = left
        for _ in range(80):
            result = (left + right) / 2
            value = finite_number(evaluate(result), "inverse response")
            if abs(value - target) <= 1e-8 * max(1, abs(target)):
                break
            if value < target:
                left = result
            else:
                right = result
    value = finite_number(evaluate(result), "inverse residual")
    if abs(value - target) > 1e-6 * max(1, abs(target)):
        raise ValueError("inverse residual exceeds the solved-bracket tolerance")
    return float(result)


def not_evaluated(reason, revision=None):
    """A disabled or unqualified check cannot carry a previous clearance."""
    return {"status": "NOT_EVALUATED", "reason": reason,
            "criterion": "all required checks evaluated on the current input revision",
            "input_revision": revision if revision is not None else "unspecified"}
