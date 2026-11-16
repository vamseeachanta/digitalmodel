"""Sample statistics for a single equal-weight wall-thickness profile.

Source: a legacy engineering workbook, API 579, ``Table 4.3``. Edition and
measurement-adequacy requirements are not supplied (Input gaps). API RP 16Q
is named elsewhere: clause not stated in source. No acceptance is inferred.

Inputs are positive readings in metres. Mean and minimum reuse GridParser
through its inch boundary using the shared unit registry. Sample variance
uses statistics.variance: sum((mean-t_i)**2)/(n-1). COV = sqrt(variance)/mean.
Residuals use mean-minus-reading, preserving their sign and input order.
Independent equally weighted readings are assumed; no spatial integration,
profile selection, missing-value imputation, or measurement qualification
is performed. All readings must be finite; at least two are required.
"""

from __future__ import annotations

import math
import statistics
from dataclasses import dataclass

import pandas as pd

from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
from digitalmodel.units import ureg


@dataclass(frozen=True)
class ThicknessReadings:
    """Immutable equal-weight readings, SI length units."""

    thicknesses_m: tuple[float, ...]

    def __post_init__(self) -> None:
        if not isinstance(self.thicknesses_m, tuple) or len(self.thicknesses_m) < 2:
            raise ValueError('at least two thickness readings in a tuple are required')
        for value in self.thicknesses_m:
            if isinstance(value, bool) or not isinstance(value, (int, float)):
                raise ValueError('thickness readings must be finite positive numbers')
            if not math.isfinite(value) or value <= 0:
                raise ValueError('thickness readings must be finite positive numbers')


@dataclass(frozen=True)
class ThicknessStatistics:
    """Lengths in metres, squared lengths in square metres, COV unitless."""

    minimum_m: float
    mean_m: float
    residuals_m: tuple[float, ...]
    squared_residuals_m2: tuple[float, ...]
    sum_squared_residuals_m2: float
    sample_variance_m2: float
    coefficient_of_variation_ratio: float


def sample_thickness_statistics(inputs: ThicknessReadings) -> ThicknessStatistics:
    """Reduce a profile using existing grid reductions and sample statistics."""
    inch_values = ureg.Quantity(inputs.thicknesses_m, 'm').to('inch').magnitude
    frame = pd.DataFrame(inch_values)
    minimum = ureg.Quantity(GridParser.min_thickness(frame), 'inch').to('m').magnitude
    mean = ureg.Quantity(GridParser.mean_thickness(frame), 'inch').to('m').magnitude
    residuals = tuple(mean - value for value in inputs.thicknesses_m)
    squares = tuple(value ** 2 for value in residuals)
    variance = statistics.variance(inputs.thicknesses_m)
    return ThicknessStatistics(
        minimum, mean, residuals, squares, math.fsum(squares), variance,
        math.sqrt(variance) / mean,
    )
