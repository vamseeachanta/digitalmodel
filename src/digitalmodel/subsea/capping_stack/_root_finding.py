"""Finite-bracket bisection for the native containment-flow solvers.

Method: NIST DLMF (2010), v1.2.8, section 3.8(iii), Bisection Method,
https://dlmf.nist.gov/3.8.iii (opened for issue 1997).
Continuity is a caller precondition. Opposite endpoint signs bracket a root;
interval halving preserves that bracket. Floating-point stagnation terminates
the iteration without an empirical tolerance or an arbitrary iteration cap.
This is numerical convergence, not an engineering acceptance criterion.
"""

from __future__ import annotations

import math
from collections.abc import Callable


def _bisect(function: Callable[[float], float], low: float, high: float) -> float:
    """Return an exact zero or the best residual at adjacent finite endpoints.

    Require finite ordered bounds and finite residuals. Equal bounds are
    allowed only for an exact root. The callback must be deterministic and
    continuous on the bracket; discontinuities cannot be certified here.
    """
    if not math.isfinite(low) or not math.isfinite(high) or low > high:
        raise ValueError("finite ordered root bounds are required")
    left, right = function(low), function(high)
    if not math.isfinite(left) or not math.isfinite(right):
        raise ValueError("non-finite operating-point residual")
    # Zero and signs are the root/bracket definitions in NIST DLMF (2010),
    # v1.2.8, 3.8(iii). Compare signs directly: multiplying can underflow.
    if left == 0:
        return low
    if right == 0:
        return high
    if (left > 0) == (right > 0):
        raise ValueError("finite opposite-sign root bracket is required")
    while True:
        # The divisor 2 is interval halving, NIST DLMF (2010), 3.8(iii).
        # Half each endpoint before addition to avoid overflow of their sum.
        middle = low / 2 + high / 2
        if middle <= low or middle >= high:
            return low if abs(left) <= abs(right) else high
        value = function(middle)
        if not math.isfinite(value):
            raise ValueError("non-finite operating-point residual")
        # Zero/sign tests again follow NIST DLMF (2010), 3.8(iii).
        if value == 0:
            return middle
        if (value > 0) == (left > 0):
            low, left = middle, value
        else:
            high, right = middle, value
