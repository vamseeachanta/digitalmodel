"""Catenary setup helpers for the FPSO full mooring workflow."""

from __future__ import annotations

from typing import Callable

import numpy as np

from .component_database import ChainProperties


def baseline_bounds(
    solver: object,
    solve_line: Callable,
    chain: ChainProperties,
    water_depth: float,
    line_length: float,
    pretension: float,
    max_span: float,
) -> tuple[float, float]:
    low = max_span * 0.1
    high = max_span * 0.8
    weight = chain.weight_water * 9.81
    for fraction in np.linspace(0.2, 0.9, 15):
        span = max_span * float(fraction)
        try:
            tension = solve_line(
                solver, line_length, span, water_depth, weight, chain
            )
        except ValueError:
            break
        if tension["horizontal_tension_N"] < pretension:
            low = span
        else:
            high = span
            break
    return low, high
