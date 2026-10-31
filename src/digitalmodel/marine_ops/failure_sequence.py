# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reusable first-threshold ranking for explicit demand/capacity load paths.

Callers supply compatible units, capacity provenance and a load-factor grid.
A threshold is the first observed D/C >= 1, refined inside its first bracket.
Features narrower than the grid can be missed; provide their breakpoints.
Ranking extrapolates independent paths beyond the first failure. It does not
redistribute loads or predict a physical cascade after that failure.
"""

from __future__ import annotations

import math
from collections.abc import Callable, Sequence
from dataclasses import dataclass


@dataclass(frozen=True)
class FailureMode:
    """One nonnegative demand and strictly positive capacity, in matching units."""

    name: str
    demand: Callable[[float], float]
    capacity: Callable[[float], float]
    basis: str

    def utilization(self, factor: float) -> float:
        """Evaluate a finite ratio, rejecting invalid callback results."""
        demand, capacity = self.demand(factor), self.capacity(factor)
        _number('demand', demand)
        _number('capacity', capacity, positive=True)
        ratio = demand / capacity
        _number('utilization', ratio)
        return ratio


@dataclass(frozen=True)
class ModeTrace:
    """Sampled trace and upper threshold bracket; None means not reached."""

    name: str
    basis: str
    utilizations: tuple[float, ...]
    first_failure_factor: float | None
    last_nonfailure_factor: float | None
    failure_utilization: float | None
    status: str


@dataclass(frozen=True)
class FailureSequence:
    """All mode traces, retaining input order and ties in governing thresholds."""

    load_factors: tuple[float, ...]
    modes: tuple[ModeTrace, ...]

    @property
    def ordered(self) -> tuple[ModeTrace, ...]:
        """Return reached modes sorted by threshold, retaining ties stably."""
        return tuple(sorted((m for m in self.modes if m.first_failure_factor is not None),
                            key=lambda m: m.first_failure_factor if m.first_failure_factor is not None else math.inf))

    @property
    def governing(self) -> tuple[str, ...]:
        """Return every mode at the first threshold within numerical resolution."""
        ordered = self.ordered
        if not ordered:
            return ()
        first = ordered[0].first_failure_factor
        if first is None:
            return ()
        return tuple(m.name for m in ordered
                     if m.first_failure_factor is not None
                     and math.isclose(m.first_failure_factor, first, rel_tol=1e-7, abs_tol=1e-7))


def _number(name: str, value: float, *, positive: bool = False) -> None:
    if (isinstance(value, bool) or not isinstance(value, (int, float))
            or not math.isfinite(value) or value < 0 or (positive and value == 0)):
        raise ValueError(f'{name} must be a finite {"positive" if positive else "nonnegative"} number')


def rank_failures(modes: Sequence[FailureMode], load_factors: Sequence[float]) -> FailureSequence:
    """Locate first sampled crossings by 50 bracket-preserving bisections.

    Within each bracket assume one first crossing and no hidden excursion.
    Continuous roots and upward jumps both return the failing upper bound.
    The returned utilization distinguishes an equality from a jump. A mode
    already failed at zero is explicit; unreached modes remain right-censored.
    Fifty bisections are a numerical budget, not an engineering safety factor.
    """
    factors = tuple(load_factors)
    if len(factors) < 2:
        raise ValueError('at least two load factors starting at zero are required')
    for value in factors:
        _number('load factor', value)
    if factors[0] != 0 or any(b <= a for a, b in zip(factors, factors[1:])):
        raise ValueError('load factors must start at zero and strictly increase')
    if not modes or any(not m.name.strip() or not m.basis.strip() for m in modes):
        raise ValueError('named modes with explicit capacity basis are required')
    if len({m.name for m in modes}) != len(modes):
        raise ValueError('mode names must be unique')
    rows = []
    for mode in modes:
        values = tuple(mode.utilization(f) for f in factors)
        index = next((i for i, value in enumerate(values) if value >= 1), None)
        lower, upper, at_failure = None, None, None
        status = 'not_reached'
        if index == 0:
            upper, at_failure, status = 0.0, values[0], 'failed_at_start'
        elif index is not None:
            lower, upper = factors[index - 1], factors[index]
            for _ in range(50):
                middle = (lower + upper) / 2
                if mode.utilization(middle) >= 1:
                    upper = middle
                else:
                    lower = middle
            at_failure, status = mode.utilization(upper), 'reached'
        rows.append(ModeTrace(mode.name, mode.basis, values, upper, lower, at_failure, status))
    return FailureSequence(factors, tuple(rows))
