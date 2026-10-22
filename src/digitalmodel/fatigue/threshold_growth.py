# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Thresholded constant-amplitude growth with explicit material calibration.

Equation basis: MIT, Fatigue (2001), Eq. (2); AFGROW Damage Tolerant
Design Handbook, online Section 5.1.2, Eq. (5.1.3), accessed 2026-10-20.
Numerics: Driscoll & Braun, Fundamentals of Numerical Computation, online
Julia edition, Section 5.7, Eqs. (5.7.2)-(5.7.4), accessed 2026-10-22:
https://tobydriscoll.net/fnc-julia/localapprox/adaptive.html
No BS 7910 or API 579 material constants or acceptance limits are supplied.
The legacy Paris-only API delegates to the same integration kernel.
"""

from __future__ import annotations

import math
from collections.abc import Callable
from dataclasses import dataclass
from numbers import Integral, Real


def validate_scalar(value, name, *, integer=False):
    """Return a finite real/integral scalar; booleans are never numbers here.

    ``numbers.Real``/``Integral`` include compatible NumPy scalar types.
    Callers apply the physical range constraints after scalar validation.
    """
    expected = Integral if integer else Real
    if isinstance(value, bool) or not isinstance(value, expected):
        raise ValueError(f"{name} must be a finite {'integer' if integer else 'real scalar'}")
    try:
        result = int(value) if integer else float(value)
        finite = math.isfinite(result)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must be finite") from exc
    if not finite:
        raise ValueError(f"{name} must be finite")
    return result


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
        for name in ("coefficient", "exponent", "threshold", "stress_ratio", "walker_exponent"):
            object.__setattr__(self, name, validate_scalar(getattr(self, name), name))
        if self.coefficient <= 0 or self.exponent <= 0 or self.threshold < 0:
            raise ValueError("coefficient/exponent must be positive; threshold >= 0")
        if not 0 <= self.stress_ratio < 1:
            raise ValueError("stress_ratio must satisfy 0 <= R < 1")
        if not 0 <= self.walker_exponent <= 1:
            raise ValueError("walker_exponent must lie in [0, 1]")
        if not isinstance(self.basis, str) or not self.basis.strip():
            raise ValueError("material calibration basis is required")

    def rate(self, delta_k: float) -> float:
        """Return da/dN [m/cycle]; equality at threshold means no growth."""
        delta_k = validate_scalar(delta_k, "DeltaK")
        if delta_k < 0:
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
    estimated_error_cycles: float | None


def _simpson(left, right, values):
    integral = (right - left) * (values[0] / 6 + values[1] * (2 / 3) + values[2] / 6)
    if not math.isfinite(integral) or integral <= 0:
        raise ValueError("integrated cycles are outside finite positive range")
    return integral


def _adaptive_panel(inverse_rate, left, right, values, coarse, atol, rtol, budget, level=0):
    middle = left + (right - left) / 2
    quarter = left + (middle - left) / 2
    three_quarter = middle + (right - middle) / 2
    if not left < quarter < middle < three_quarter < right:
        raise ValueError("growth integration cannot converge at depth resolution")
    new_values = (inverse_rate(quarter), inverse_rate(three_quarter))
    if None in new_values:
        return None
    left_values = (values[0], new_values[0], values[1])
    right_values = (values[1], new_values[1], values[2])
    left_sum = _simpson(left, middle, left_values)
    right_sum = _simpson(middle, right, right_values)
    refined = left_sum + right_sum
    if not math.isfinite(refined):
        raise ValueError("integrated cycles exceed finite range")
    correction = (refined - coarse) / 15  # Driscoll & Braun, Eq. (5.7.3).
    estimate = refined + correction
    if not math.isfinite(estimate) or estimate <= 0:
        raise ValueError("corrected integrated cycles must be finite and positive")
    error = abs(correction)
    # Two refinement levels reduce a single nested-grid alias; arbitrary
    # user callables still require the smoothness assumption in the API.
    if level >= 1 and error <= atol + rtol * abs(estimate):
        return estimate, error
    if budget <= 1:
        raise ValueError("growth integration did not converge within max_refinements")
    first = _adaptive_panel(
        inverse_rate, left, middle, left_values, left_sum,
        atol / 2, rtol, budget - 1, level + 1,
    )
    if first is None:
        return None
    second = _adaptive_panel(
        inverse_rate, middle, right, right_values, right_sum,
        atol / 2, rtol, budget - 1, level + 1,
    )
    return None if second is None else (first[0] + second[0], first[1] + second[1])


def _integration_controls(initial_m, limit_m, steps, relative, absolute, refinements):
    initial_m = validate_scalar(initial_m, "initial_m")
    limit_m = validate_scalar(limit_m, "limit_m")
    steps = validate_scalar(steps, "steps", integer=True)
    relative = validate_scalar(relative, "relative_tolerance")
    absolute = validate_scalar(absolute, "absolute_tolerance")
    refinements = validate_scalar(refinements, "max_refinements", integer=True)
    if initial_m <= 0 or limit_m <= 0:
        raise ValueError("depths must be finite and positive")
    if steps < 1:
        raise ValueError("steps must be a positive integer")
    if not 0 <= relative < 1:
        raise ValueError("relative_tolerance must lie in [0, 1)")
    if absolute < 0 or relative + absolute <= 0:
        raise ValueError("tolerances must be non-negative and at least one positive")
    if refinements < 2:
        raise ValueError("max_refinements must be an integer of at least 2")
    return initial_m, limit_m, steps, relative, absolute, refinements


def integrate_growth(
    law: GrowthLaw,
    delta_k: float | Callable[[float], float],
    *,
    initial_m: float,
    limit_m: float,
    steps: int,
    relative_tolerance: float = 1e-6,
    absolute_tolerance: float = 1e-6,
    max_refinements: int = 20,
) -> GrowthResult:
    """Integrate da/rate(a) by adaptive Simpson quadrature.

    Deterministic callables receive depth [m] and return DeltaK [MPa sqrt(m)].
    ``steps`` sets the initial panels. The summed estimated error is bounded
    by absolute_tolerance [cycles] + relative_tolerance * life_cycles.
    max_refinements must be at least 2 and limits bisection depth;
    exhaustion raises ValueError. Large panel/refinement counts may exhaust
    time or memory; no separate process resource bound is imposed here.
    The Simpson /15 estimate assumes a smooth reciprocal growth rate;
    finite samples cannot certify arbitrary callables, exclude unsampled
    threshold dips or singularities, or bound their true integral error.
    Arrest returns no finite life/error and only fully traversed initial
    panels; no fabricated limiting-depth endpoint is included.
    Already at/above the limit returns zero life. No load-sequence effects,
    changing aspect ratio, creep, corrosion or temperature model is included.
    """
    if not isinstance(law, GrowthLaw):
        raise ValueError("law must be a GrowthLaw")
    controls = _integration_controls(
        initial_m, limit_m, steps, relative_tolerance, absolute_tolerance, max_refinements,
    )
    if not callable(delta_k):
        delta_k = validate_scalar(delta_k, "DeltaK")
        if delta_k < 0:
            raise ValueError("DeltaK must be non-negative")
    return _integrate_panels(law, delta_k, *controls)


def _integrate_panels(law, delta_k, initial_m, limit_m, steps, relative, absolute, refinements):
    history = [GrowthPoint(initial_m, 0.0)]
    if initial_m >= limit_m:
        return GrowthResult("LIMIT_REACHED", 0.0, tuple(history), law.basis, 0.0)
    dk = delta_k if callable(delta_k) else lambda _: delta_k

    def inverse_rate(depth):
        rate = law.rate(dk(depth))
        if rate == 0:
            return None
        inverse = 1 / rate
        if not math.isfinite(inverse):
            raise ValueError("inverse growth rate exceeds finite range")
        return inverse

    cycles = error = 0.0
    for i in range(steps):
        left = initial_m + (limit_m - initial_m) * i / steps
        right = limit_m if i == steps - 1 else initial_m + (limit_m - initial_m) * (i + 1) / steps
        values = tuple(inverse_rate(a) for a in (left, left + (right - left) / 2, right))
        panel = None if None in values else _adaptive_panel(
            inverse_rate, left, right, values, _simpson(left, right, values),
            absolute / steps, relative, refinements,
        )
        if panel is None:
            return GrowthResult("ARRESTED", None, tuple(history), law.basis, None)
        cycles += panel[0]
        error += panel[1]
        if not math.isfinite(cycles) or not math.isfinite(error):
            raise ValueError("integrated cycles exceed finite range")
        history.append(GrowthPoint(right, cycles))
    return GrowthResult("LIMIT_REACHED", cycles, tuple(history), law.basis, error)
