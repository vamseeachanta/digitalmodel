# ABOUTME: Numerical integration methods for naval architecture calculations
# ABOUTME: Simpson's rules for waterplane area, sectional area, volume
"""
Numerical integration methods for hull form calculations.

Covers Simpson's 1st and 2nd rules as used in USNA EN400 Chapter 2
for waterplane area, sectional area curves, and volume integration.
"""

from typing import List


def simpsons_first_rule(ordinates: List[float], spacing: float) -> float:
    """Simpson's 1st rule (1/3 rule) for numerical integration.

    A = h/3 * (y0 + 4*y1 + 2*y2 + 4*y3 + ... + yn)

    Requires odd number of ordinates (even number of intervals).

    Args:
        ordinates: y-values at equally spaced stations
        spacing: distance between stations (h)

    Returns:
        Integrated area
    """
    n = len(ordinates)
    if n < 3:
        raise ValueError("Need at least 3 ordinates for Simpson's 1st rule")
    if n % 2 == 0:
        raise ValueError(
            f"Simpson's 1st rule requires odd number of ordinates, got {n}"
        )

    multipliers = (
        [1]
        + [4 if i % 2 == 1 else 2 for i in range(1, n - 1)]
        + [1]
    )
    return spacing / 3 * sum(
        m * y for m, y in zip(multipliers, ordinates)
    )
