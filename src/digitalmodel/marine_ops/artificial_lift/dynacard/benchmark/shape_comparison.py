# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Dimensionless closed-loop comparison for canonical pump-card shapes.

Method: repository canonical shape regression, test_card_generator_shapes.py,
introduced for https://github.com/vamseeachanta/digitalmodel/issues/1875.
Reference shapes: Mendeley Data (2020), version 1, DOI 10.17632/hnfwm9mjs6.1,
CC0. Coordinates have arbitrary units; per-axis scaling removes absolute load
and stroke information. This is a shape comparison, not field validation.

For each axis q, use (q-min(q))/(max(q)-min(q)). Close the polygon and interpolate
at equal cumulative Euclidean arc-length intervals. Distance is the minimum
mean Euclidean separation over cyclic sample shifts and both traversal orders.
"""

import numpy as np


def _normalise(a: np.ndarray) -> np.ndarray:
    """Scale one axis onto 0-1. The plate carries no engineering units."""
    lo, hi = float(np.min(a)), float(np.max(a))
    if hi == lo:
        return np.zeros_like(a)
    if not np.isfinite(hi - lo):
        # Changing units first preserves the ratio when opposite-sign finite
        # coordinates would otherwise overflow during range subtraction.
        a = a / max(abs(lo), abs(hi))
        lo, hi = float(np.min(a)), float(np.max(a))
    return (a - lo) / (hi - lo)


def _resample_loop(x: np.ndarray, y: np.ndarray, m: int) -> np.ndarray:
    """Resample a closed loop to ``m`` points at uniform arc length."""
    px = np.append(x, x[0])
    py = np.append(y, y[0])
    step = np.hypot(np.diff(px), np.diff(py))
    s = np.concatenate([[0.0], np.cumsum(step)])
    if s[-1] == 0:
        return np.zeros((m, 2))
    t = np.linspace(0, s[-1], m, endpoint=False)
    return np.stack([np.interp(t, s, px), np.interp(t, s, py)], axis=1)


# 128 is the sampling resolution of the existing canonical regression method
# (issue 1875, Method; existing test_card_generator_shapes.py), not a physical constant.
DEFAULT_LOOP_POINTS = 128


def prepare(position, load, *, points: int = DEFAULT_LOOP_POINTS) -> np.ndarray:
    """Per-axis 0-1 normalised, arc-length resampled closed loop."""
    if isinstance(points, bool) or not isinstance(points, int) or points < 3:
        raise ValueError("points must be an integer of at least three")
    position = np.asarray(position, dtype=float)
    load = np.asarray(load, dtype=float)
    if (position.ndim != 1 or load.ndim != 1
            or position.shape != load.shape or len(position) < 3
            or not np.isfinite(position).all() or not np.isfinite(load).all()):
        raise ValueError("Require paired finite one-dimensional coordinates")
    return _resample_loop(
        _normalise(position),
        _normalise(load),
        m=points,
    )


def loop_distance(a: np.ndarray, b: np.ndarray) -> float:
    """Distance between two prepared loops, invariant to discrete sample phase.

    Minimum mean point-to-point distance over every circular shift of ``b``
    and both traversal directions. A card has no canonical start sample and no
    canonical direction, so a metric that cared about either would be measuring
    the recorder rather than the pump.
    """
    a = np.asarray(a, dtype=float)
    b = np.asarray(b, dtype=float)
    if (a.ndim != 2 or a.shape[1] != 2 or len(a) < 3 or a.shape != b.shape
            or not np.isfinite(a).all() or not np.isfinite(b).all()):
        raise ValueError("Require equally sampled finite planar loops")
    best = np.inf
    for oriented in (b, b[::-1]):
        for shift in range(len(oriented)):
            d = float(
                np.linalg.norm(a - np.roll(oriented, shift, axis=0), axis=1).mean()
            )
            best = min(best, d)
    return best
