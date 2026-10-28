# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Shared rainflow kernel extracted from fatigue_apps.rainflow_counter.

The stack algorithm and full/half-cycle convention are retained from that
module, whose reference is ASTM E1049-85 and the simple rainflow algorithm.
No standard document or solver binding is needed to use this kernel.
"""

from dataclasses import dataclass
from math import isfinite
from typing import Iterable


@dataclass
class RainflowCycle:
    """A single rainflow cycle."""
    range_value: float  # Stress/load range
    mean_value: float   # Mean stress/load
    count: float        # Cycle count (0.5 for half cycles, 1.0 for full)
    start_index: int    # Starting index in time series
    end_index: int      # Ending index in time series



def count_reversals(reversals: Iterable[float], indices: Iterable[int]) -> list[RainflowCycle]:
    """Count an already extracted sequence of reversals using the shared kernel."""
    cycles = []
    stack = []

    for reversal, index in zip(reversals, indices):
        stack.append((reversal, index))

        while len(stack) >= 3:
            new_range = abs(stack[-2][0] - stack[-1][0])
            prior_range = abs(stack[-3][0] - stack[-2][0])

            if new_range < prior_range:
                break

            cycle_range = prior_range
            if cycle_range > 0:
                cycles.append(RainflowCycle(
                    range_value=cycle_range,
                    mean_value=(stack[-3][0] + stack[-2][0]) / 2,
                    count=0.5 if len(stack) == 3 else 1.0,
                    start_index=stack[-3][1],
                    end_index=stack[-2][1]
                ))

            if len(stack) == 3:
                stack.pop(0)
            else:
                last = stack.pop()
                stack.pop()
                stack.pop()
                stack.append(last)

    # Process remaining points in stack as half cycles
    for i in range(len(stack) - 1):
        cycle_range = abs(stack[i][0] - stack[i + 1][0])
        if cycle_range == 0:
            continue
        cycles.append(RainflowCycle(
            range_value=cycle_range,
            mean_value=(stack[i][0] + stack[i + 1][0]) / 2,
            count=0.5,
            start_index=stack[i][1],
            end_index=stack[i + 1][1]
        ))

    return cycles


def count_cycles(signal: Iterable[float]) -> list[RainflowCycle]:
    """Extract reversals, retaining plateaux endpoints, then count cycles."""
    points = []
    for index, raw in enumerate(signal):
        value = float(raw)
        if not isfinite(value):
            raise ValueError("rainflow signal must contain finite values")
        if not points or value != points[-1][0]:
            points.append((value, index))
    if len(points) < 2:
        return []
    reversals = [points[0]]
    for previous, current, following in zip(points, points[1:], points[2:]):
        if (current[0] - previous[0]) * (following[0] - current[0]) < 0:
            reversals.append(current)
    reversals.append(points[-1])
    return count_reversals((p[0] for p in reversals), (p[1] for p in reversals))
