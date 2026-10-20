# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Thresholded constant-amplitude growth with explicit material calibration.

Equation basis: MIT, Fatigue (2001), Eq. (2); AFGROW Damage Tolerant
Design Handbook, online Section 5.1.2, Eq. (5.1.3), accessed 2026-10-20.
No BS 7910 or API 579 material constants or acceptance limits are supplied.
The existing Paris-only API in crack_growth remains unchanged.
"""

from __future__ import annotations

import math
from collections.abc import Callable
from dataclasses import dataclass


@dataclass(frozen=True)
class GrowthLaw:
    """R=0 coefficient in m/cycle/(MPa sqrt(m))**exponent.

    All parameters are required. ``threshold`` is DeltaK_th in MPa sqrt(m)
    at the supplied service R and environment, compared to the raw range.
    ``basis`` identifies calibration data, validity range and conditions.
    This implementation supports tension-tension cycles, 0 <= R < 1.
    """

    coefficient: float
    exponent: float
    threshold: float
    stress_ratio: float
    walker_exponent: float
    basis: str

    def __post_init__(self):
        values = (
            self.coefficient,
            self.exponent,
            self.threshold,
            self.stress_ratio,
            self.walker_exponent,
        )
        if not all(math.isfinite(v) for v in values):
            raise ValueError("growth parameters must be finite")
        if self.coefficient <= 0 or self.exponent <= 0 or self.threshold < 0:
            raise ValueError("coefficient/exponent must be positive; threshold >= 0")
        if not 0 <= self.stress_ratio < 1:
            raise ValueError("stress_ratio must satisfy 0 <= R < 1")
        if not 0 <= self.walker_exponent <= 1:
            raise ValueError("walker_exponent must lie in [0, 1]")
        if not self.basis.strip():
            raise ValueError("material calibration basis is required")

    def rate(self, delta_k: float) -> float:
        """Return da/dN [m/cycle]; equality at threshold means no growth."""
        if not math.isfinite(delta_k) or delta_k < 0:
            raise ValueError("DeltaK must be finite and non-negative")
        if delta_k <= self.threshold:
            return 0.0
        # AFGROW Handbook 5.1.2 Eq. (5.1.3): C*[(1-R)^gamma*Kmax]^m.
        # Kmax=DeltaK/(1-R), hence the equivalent range below.
        equivalent = delta_k / (1 - self.stress_ratio) ** (1 - self.walker_exponent)
        try:
            rate = self.coefficient * equivalent**self.exponent
        except OverflowError as exc:
            raise ValueError("growth rate overflow") from exc
        if not math.isfinite(rate) or rate <= 0:
            raise ValueError("growth rate is outside finite positive range")
        return rate


@dataclass(frozen=True)
class GrowthPoint:
    depth_m: float
    cycles: float


@dataclass(frozen=True)
class GrowthResult:
    status: str
    life_cycles: float | None
    history: tuple[GrowthPoint, ...]
    basis: str


def integrate_growth(
    law: GrowthLaw,
    delta_k: float | Callable[[float], float],
    *,
    initial_m: float,
    limit_m: float,
    steps: int,
) -> GrowthResult:
    """Integrate da/rate(a) by composite Simpson quadrature.

    Callables receive depth in metres and return DeltaK in MPa sqrt(m).
    Endpoints and midpoints are checked. Arrest returns no finite life and
    only fully traversed panels; no fabricated limiting-depth endpoint.
    Step refinement is required for a changing K solution. A dip between
    sampled points cannot be excluded for an arbitrary user callable.
    Already at/above the limit returns zero life. No load-sequence effects,
    changing aspect ratio, creep, corrosion or temperature model is included.
    """
    if not all(math.isfinite(v) and v > 0 for v in (initial_m, limit_m)):
        raise ValueError("depths must be finite and positive")
    if isinstance(steps, bool) or not isinstance(steps, int) or steps < 1:
        raise ValueError("steps must be a positive integer")
    history = [GrowthPoint(initial_m, 0.0)]
    if initial_m >= limit_m:
        return GrowthResult("LIMIT_REACHED", 0.0, tuple(history), law.basis)
    dk = delta_k if callable(delta_k) else lambda _: delta_k
    cycles = 0.0
    for i in range(steps):
        left = initial_m + (limit_m - initial_m) * i / steps
        right = initial_m + (limit_m - initial_m) * (i + 1) / steps
        rates = [law.rate(dk(a)) for a in (left, (left + right) / 2, right)]
        if 0.0 in rates:
            return GrowthResult("ARRESTED", None, tuple(history), law.basis)
        # Simpson weights (1, 4, 1)/6 are numerical quadrature, not material data.
        cycles += (right - left) / 6 * (1 / rates[0] + 4 / rates[1] + 1 / rates[2])
        if not math.isfinite(cycles):
            raise ValueError("integrated cycles exceed finite range")
        history.append(GrowthPoint(right, cycles))
    return GrowthResult("LIMIT_REACHED", cycles, tuple(history), law.basis)
