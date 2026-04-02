"""General post-processing utilities for OrcaFlex results.

Provides time series statistics extraction (max, min, std, significant),
range graph generation (tension/bending vs arc length), envelope plots,
and extreme value analysis (Gumbel/Weibull fitting).

Does NOT require OrcFxAPI — works on numpy arrays and pandas DataFrames.

References:
    - DNV-RP-C205: Environmental Conditions and Environmental Loads
    - Gumbel (1958): Statistics of Extremes
    - API RP 2SK: Design and Analysis of Stationkeeping Systems (statistics)
"""

import math
from typing import Dict, List, Optional, Tuple, Union

import numpy as np
import pandas as pd
from pydantic import BaseModel, Field
from scipy import stats as sp_stats


# ---------------------------------------------------------------------------
# Time series statistics
# ---------------------------------------------------------------------------

class TimeSeriesStats(BaseModel):
    """Statistical summary of a time series."""
    name: str = Field("", description="Variable name")
    n_samples: int = Field(0, ge=0, description="Number of samples")
    mean: float = Field(0.0, description="Mean value")
    std: float = Field(0.0, ge=0.0, description="Standard deviation")
    min_val: float = Field(0.0, description="Minimum value")
    max_val: float = Field(0.0, description="Maximum value")
    significant: float = Field(0.0, description="Significant value (mean of top 1/3)")
    p01: float = Field(0.0, description="1st percentile")
    p05: float = Field(0.0, description="5th percentile")
    p50: float = Field(0.0, description="50th percentile (median)")
    p95: float = Field(0.0, description="95th percentile")
    p99: float = Field(0.0, description="99th percentile")
    rms: float = Field(0.0, ge=0.0, description="Root mean square")
    skewness: float = Field(0.0, description="Skewness")
    kurtosis: float = Field(0.0, description="Excess kurtosis")


def compute_time_series_stats(
    data: np.ndarray,
    name: str = "",
) -> TimeSeriesStats:
    """Compute comprehensive statistics from a time series.

    Args:
        data: 1D array of time series values.
        name: Variable name for labeling.

    Returns:
        TimeSeriesStats with all statistical measures.
    """
    if len(data) == 0:
        return TimeSeriesStats(name=name)

    data = np.asarray(data, dtype=float)
    n = len(data)

    # Significant value: mean of top 1/3 of absolute values
    sorted_abs = np.sort(np.abs(data))
    n_third = max(1, n // 3)
    significant = float(np.mean(sorted_abs[-n_third:]))

    # RMS
    rms = float(np.sqrt(np.mean(data**2)))

    return TimeSeriesStats(
        name=name,
        n_samples=n,
        mean=round(float(np.mean(data)), 6),
        std=round(float(np.std(data, ddof=1)) if n > 1 else 0.0, 6),
        min_val=round(float(np.min(data)), 6),
        max_val=round(float(np.max(data)), 6),
        significant=round(significant, 6),
        p01=round(float(np.percentile(data, 1)), 6),
        p05=round(float(np.percentile(data, 5)), 6),
        p50=round(float(np.percentile(data, 50)), 6),
        p95=round(float(np.percentile(data, 95)), 6),
        p99=round(float(np.percentile(data, 99)), 6),
        rms=round(rms, 6),
        skewness=round(float(sp_stats.skew(data)), 6) if n > 2 else 0.0,
        kurtosis=round(float(sp_stats.kurtosis(data)), 6) if n > 3 else 0.0,
    )


# ---------------------------------------------------------------------------
# Range graph (arc length profile)
# ---------------------------------------------------------------------------

class RangeGraphData(BaseModel):
    """Range graph data: statistics vs arc length.

    Used for tension/bending moment/curvature envelope plots.
    """
    variable_name: str = Field("", description="Variable plotted")
    arc_lengths: List[float] = Field(default_factory=list, description="Arc length positions (m)")
    mean_values: List[float] = Field(default_factory=list, description="Mean at each position")
    max_values: List[float] = Field(default_factory=list, description="Max at each position")
    min_values: List[float] = Field(default_factory=list, description="Min at each position")
    std_values: List[float] = Field(default_factory=list, description="Std at each position")

    @property
    def max_of_max(self) -> float:
        """Overall maximum across all positions."""
        return max(self.max_values) if self.max_values else 0.0

    @property
    def min_of_min(self) -> float:
        """Overall minimum across all positions."""
        return min(self.min_values) if self.min_values else 0.0


def generate_range_graph(
    arc_lengths: np.ndarray,
    time_histories: np.ndarray,
    variable_name: str = "Effective Tension (kN)",
) -> RangeGraphData:
    """Generate range graph data from time histories at multiple positions.

    Args:
        arc_lengths: 1D array of arc length positions (m), shape (n_positions,).
        time_histories: 2D array, shape (n_timesteps, n_positions).
        variable_name: Name of the variable.

    Returns:
        RangeGraphData with statistics at each position.
    """
    n_pos = len(arc_lengths)
    if time_histories.ndim == 1:
        time_histories = time_histories.reshape(-1, 1)

    means = []
    maxs = []
    mins = []
    stds = []

    for j in range(n_pos):
        col = time_histories[:, j] if j < time_histories.shape[1] else np.zeros(time_histories.shape[0])
        means.append(round(float(np.mean(col)), 4))
        maxs.append(round(float(np.max(col)), 4))
        mins.append(round(float(np.min(col)), 4))
        stds.append(round(float(np.std(col, ddof=1)) if len(col) > 1 else 0.0, 4))

    return RangeGraphData(
        variable_name=variable_name,
        arc_lengths=[round(float(a), 2) for a in arc_lengths],
        mean_values=means,
        max_values=maxs,
        min_values=mins,
        std_values=stds,
    )


# ---------------------------------------------------------------------------
# Extreme value analysis
# ---------------------------------------------------------------------------

class ExtremeValueResult(BaseModel):
    """Result of extreme value analysis."""
    distribution: str = Field(..., description="Distribution fitted (Gumbel or Weibull)")
    location: float = Field(0.0, description="Location parameter (mu)")
    scale: float = Field(0.0, description="Scale parameter (sigma/beta)")
    shape: float = Field(0.0, description="Shape parameter (for Weibull)")
    return_values: Dict[str, float] = Field(default_factory=dict,
                                             description="Return period -> extreme value")
    ks_statistic: float = Field(0.0, description="Kolmogorov-Smirnov statistic")
    ks_pvalue: float = Field(0.0, description="KS test p-value")


def fit_gumbel(
    maxima: np.ndarray,
    return_periods: Optional[List[float]] = None,
) -> ExtremeValueResult:
    """Fit Gumbel (Type I) extreme value distribution to block maxima.

    CDF: F(x) = exp(-exp(-(x-mu)/sigma))

    Args:
        maxima: Array of block maxima (e.g., 3-hour storm maxima).
        return_periods: Return periods in same units as block duration.

    Returns:
        ExtremeValueResult with fitted parameters and return values.

    Reference: Gumbel (1958), DNV-RP-C205 Section 3.4.
    """
    if return_periods is None:
        return_periods = [10, 50, 100, 1000]

    maxima = np.asarray(maxima, dtype=float)

    # Fit using scipy
    loc, scale = sp_stats.gumbel_r.fit(maxima)

    # Return values: x_T = mu + sigma * (-ln(-ln(1 - 1/T)))
    return_values = {}
    for T in return_periods:
        p = 1.0 - 1.0 / T
        x_T = sp_stats.gumbel_r.ppf(p, loc=loc, scale=scale)
        return_values[f"{T:.0f}"] = round(float(x_T), 4)

    # Goodness-of-fit
    ks_stat, ks_p = sp_stats.kstest(maxima, "gumbel_r", args=(loc, scale))

    return ExtremeValueResult(
        distribution="Gumbel",
        location=round(float(loc), 4),
        scale=round(float(scale), 4),
        shape=0.0,
        return_values=return_values,
        ks_statistic=round(float(ks_stat), 4),
        ks_pvalue=round(float(ks_p), 4),
    )


def fit_weibull(
    maxima: np.ndarray,
    return_periods: Optional[List[float]] = None,
) -> ExtremeValueResult:
    """Fit 3-parameter Weibull distribution to maxima.

    Args:
        maxima: Array of extreme values.
        return_periods: Return periods.

    Returns:
        ExtremeValueResult with fitted parameters.

    Reference: DNV-RP-C205 Section 3.4.
    """
    if return_periods is None:
        return_periods = [10, 50, 100, 1000]

    maxima = np.asarray(maxima, dtype=float)

    # Fit Weibull (3-param: shape c, loc, scale)
    shape, loc, scale = sp_stats.weibull_min.fit(maxima, floc=0)

    return_values = {}
    for T in return_periods:
        p = 1.0 - 1.0 / T
        x_T = sp_stats.weibull_min.ppf(p, shape, loc=loc, scale=scale)
        return_values[f"{T:.0f}"] = round(float(x_T), 4)

    ks_stat, ks_p = sp_stats.kstest(maxima, "weibull_min", args=(shape, loc, scale))

    return ExtremeValueResult(
        distribution="Weibull",
        location=round(float(loc), 4),
        scale=round(float(scale), 4),
        shape=round(float(shape), 4),
        return_values=return_values,
        ks_statistic=round(float(ks_stat), 4),
        ks_pvalue=round(float(ks_p), 4),
    )


# ---------------------------------------------------------------------------
# Multi-seed processing
# ---------------------------------------------------------------------------

class MultiSeedSummary(BaseModel):
    """Summary statistics across multiple random seeds.

    Common in OrcaFlex dynamic analysis where multiple seeds are run
    for the same sea state to capture statistical variability.

    Reference: API RP 2SK Section 6.3.
    """
    n_seeds: int = Field(0, description="Number of seeds")
    max_of_max: float = Field(0.0, description="Maximum of all seed maxima")
    mean_of_max: float = Field(0.0, description="Mean of seed maxima")
    std_of_max: float = Field(0.0, description="Std dev of seed maxima")
    mpme: float = Field(0.0, description="Most Probable Maximum Extreme (Gumbel)")
    characteristic: float = Field(0.0, description="Characteristic extreme (mean + 1.28*std)")


def process_multi_seed(
    seed_maxima: np.ndarray,
    variable_name: str = "",
) -> MultiSeedSummary:
    """Process results from multiple random seeds.

    Calculates MPME (Most Probable Maximum Extreme) and characteristic
    extreme values from seed maxima.

    MPME = mean + 0.5772 * std * sqrt(6) / pi  (Gumbel mode)

    Args:
        seed_maxima: Array of maximum values, one per seed.
        variable_name: Name of the variable.

    Returns:
        MultiSeedSummary with combined statistics.

    Reference: API RP 2SK Section 6.3.
    """
    n = len(seed_maxima)
    if n == 0:
        return MultiSeedSummary()

    seed_maxima = np.asarray(seed_maxima, dtype=float)
    mean_max = float(np.mean(seed_maxima))
    std_max = float(np.std(seed_maxima, ddof=1)) if n > 1 else 0.0

    # MPME via Gumbel: mode = mean - 0.5772 * sigma_gumbel
    # sigma_gumbel = std * sqrt(6) / pi
    sigma_gumbel = std_max * math.sqrt(6) / math.pi if n > 1 else 0.0
    euler_mascheroni = 0.5772
    mpme = mean_max - euler_mascheroni * sigma_gumbel  # This is the mode

    # Characteristic: mean + 1.28 * std (90th percentile approx)
    characteristic = mean_max + 1.28 * std_max

    return MultiSeedSummary(
        n_seeds=n,
        max_of_max=round(float(np.max(seed_maxima)), 4),
        mean_of_max=round(mean_max, 4),
        std_of_max=round(std_max, 4),
        mpme=round(mpme, 4),
        characteristic=round(characteristic, 4),
    )
