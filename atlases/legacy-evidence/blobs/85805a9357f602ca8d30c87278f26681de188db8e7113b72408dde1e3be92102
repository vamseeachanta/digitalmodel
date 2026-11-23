"""Weather window analysis for marine operations.

Provides wave scatter diagram processing, operability tables from Hs/Tp limits,
persistence analysis (consecutive hours below threshold), seasonal variation,
and waiting-on-weather (WoW) statistics.

Does NOT require OrcFxAPI — operates on hindcast / metocean data.

References:
    - DNV-RP-H103: Modelling and Analysis of Marine Operations (weather criteria)
    - DNV-OS-H101 / DNV-ST-N001: Marine Operations, General
    - API RP 2MET: Metocean Design and Operating Conditions
    - Noble Denton 0027/ND: General Guidelines for Marine Operations
"""

import math
from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Scatter diagram
# ---------------------------------------------------------------------------

class ScatterDiagram(BaseModel):
    """Wave scatter diagram (Hs-Tp joint probability).

    Reference: DNV-RP-C205 Section 3.3 — joint distribution of Hs and Tp.
    """
    hs_bins: List[float] = Field(
        default=[0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0],
        description="Upper bin edges for Hs (m)",
    )
    tp_bins: List[float] = Field(
        default=[4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
        description="Upper bin edges for Tp (s)",
    )
    occurrences: Optional[List[List[float]]] = Field(
        None,
        description="2D array [n_hs x n_tp] of occurrence counts or probabilities",
    )

    def generate_default_scatter(self) -> List[List[float]]:
        """Generate a default scatter diagram (GOM-like distribution).

        This is a simplified synthetic distribution for demonstration.
        For real projects, use site-specific hindcast data.

        Returns:
            2D list of occurrence probabilities.
        """
        n_hs = len(self.hs_bins)
        n_tp = len(self.tp_bins)
        scatter = np.zeros((n_hs, n_tp))

        for i, hs in enumerate(self.hs_bins):
            for j, tp in enumerate(self.tp_bins):
                # Bivariate distribution (simplified log-normal-like)
                hs_mid = hs - 0.25
                tp_mid = tp - 0.5
                # Most probable Tp for given Hs: Tp ≈ 4.5 * sqrt(Hs)
                tp_peak = 4.5 * math.sqrt(max(hs_mid, 0.1))
                # Gaussian-ish kernel
                prob = math.exp(-0.5 * ((hs_mid - 1.5) / 1.2) ** 2) * \
                       math.exp(-0.5 * ((tp_mid - tp_peak) / 1.5) ** 2)
                scatter[i, j] = max(0, prob)

        # Normalise to sum to 1
        total = scatter.sum()
        if total > 0:
            scatter = scatter / total

        return scatter.tolist()

    def get_occurrences(self) -> np.ndarray:
        """Return occurrences as numpy array, generating default if not provided."""
        if self.occurrences is not None:
            return np.array(self.occurrences)
        return np.array(self.generate_default_scatter())

    def total_probability_below(self, hs_limit: float, tp_limit: Optional[float] = None) -> float:
        """Calculate total probability of sea states below thresholds.

        Args:
            hs_limit: Maximum significant wave height (m).
            tp_limit: Maximum peak period (s). If None, no Tp limit.

        Returns:
            Total probability (0-1).
        """
        scatter = self.get_occurrences()
        total = 0.0
        for i, hs in enumerate(self.hs_bins):
            for j, tp in enumerate(self.tp_bins):
                if i < scatter.shape[0] and j < scatter.shape[1]:
                    if hs <= hs_limit and (tp_limit is None or tp <= tp_limit):
                        total += scatter[i, j]
        return round(total, 4)


# ---------------------------------------------------------------------------
# Operability table
# ---------------------------------------------------------------------------

class OperabilityInput(BaseModel):
    """Input for operability calculation.

    Reference: DNV-OS-H101 Section 3.
    """
    hs_limit: float = Field(2.5, gt=0.0, description="Operational Hs limit (m)")
    tp_limit: Optional[float] = Field(None, gt=0.0, description="Operational Tp limit (s)")
    wind_limit_mps: Optional[float] = Field(None, gt=0.0, description="Wind speed limit (m/s)")
    current_limit_mps: Optional[float] = Field(None, gt=0.0, description="Current speed limit (m/s)")
    scatter: ScatterDiagram = Field(default_factory=ScatterDiagram)

    def calculate_operability(self) -> Dict[str, float]:
        """Calculate overall operability percentage.

        Returns:
            Dict with operability metrics.
        """
        prob = self.scatter.total_probability_below(self.hs_limit, self.tp_limit)

        return {
            "operability_pct": round(prob * 100.0, 1),
            "non_operability_pct": round((1.0 - prob) * 100.0, 1),
            "hs_limit_m": self.hs_limit,
            "tp_limit_s": self.tp_limit,
        }


class OperabilityTable(BaseModel):
    """Generate operability for a range of Hs limits.

    Reference: Noble Denton 0027/ND Section 4.
    """
    hs_limits: List[float] = Field(
        default=[1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0],
        description="Hs limits to evaluate (m)",
    )
    scatter: ScatterDiagram = Field(default_factory=ScatterDiagram)

    def generate_table(self) -> List[Dict[str, float]]:
        """Generate operability vs Hs limit table.

        Returns:
            List of dicts with hs_limit and operability_pct.
        """
        return [
            {
                "hs_limit_m": hs,
                "operability_pct": round(self.scatter.total_probability_below(hs) * 100.0, 1),
            }
            for hs in self.hs_limits
        ]


# ---------------------------------------------------------------------------
# Persistence analysis
# ---------------------------------------------------------------------------

class PersistenceResult(BaseModel):
    """Result of persistence (weather window) analysis."""
    hs_limit: float = Field(..., description="Hs threshold (m)")
    mean_window_hours: float = Field(..., description="Mean weather window duration (hours)")
    median_window_hours: float = Field(..., description="Median weather window duration (hours)")
    p10_window_hours: float = Field(..., description="10th percentile window (hours)")
    p90_window_hours: float = Field(..., description="90th percentile window (hours)")
    max_window_hours: float = Field(..., description="Maximum weather window (hours)")
    num_windows: int = Field(..., description="Number of windows found")
    total_hours_below: float = Field(..., description="Total hours below threshold")


def analyse_persistence(
    hs_timeseries: np.ndarray,
    hs_limit: float,
    time_step_hours: float = 3.0,
) -> PersistenceResult:
    """Analyse persistence of weather windows in Hs time series.

    A 'weather window' is a consecutive period where Hs <= hs_limit.

    Args:
        hs_timeseries: Array of Hs values (m).
        hs_limit: Operational Hs limit (m).
        time_step_hours: Time step between observations (hours).

    Returns:
        PersistenceResult with window statistics.

    Reference: DNV-OS-H101 Section 3, Noble Denton 0027/ND.
    """
    below = hs_timeseries <= hs_limit
    total_below = float(np.sum(below)) * time_step_hours

    # Find consecutive windows
    windows: List[float] = []
    current_window = 0
    for val in below:
        if val:
            current_window += 1
        else:
            if current_window > 0:
                windows.append(current_window * time_step_hours)
            current_window = 0
    if current_window > 0:
        windows.append(current_window * time_step_hours)

    if len(windows) == 0:
        return PersistenceResult(
            hs_limit=hs_limit,
            mean_window_hours=0.0,
            median_window_hours=0.0,
            p10_window_hours=0.0,
            p90_window_hours=0.0,
            max_window_hours=0.0,
            num_windows=0,
            total_hours_below=total_below,
        )

    arr = np.array(windows)
    return PersistenceResult(
        hs_limit=hs_limit,
        mean_window_hours=round(float(np.mean(arr)), 1),
        median_window_hours=round(float(np.median(arr)), 1),
        p10_window_hours=round(float(np.percentile(arr, 10)), 1),
        p90_window_hours=round(float(np.percentile(arr, 90)), 1),
        max_window_hours=round(float(np.max(arr)), 1),
        num_windows=len(windows),
        total_hours_below=round(total_below, 1),
    )


# ---------------------------------------------------------------------------
# Seasonal variation
# ---------------------------------------------------------------------------

class SeasonalOperability(BaseModel):
    """Monthly/seasonal operability statistics.

    Reference: API RP 2MET Section 5.
    """
    month_names: List[str] = Field(
        default=["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    )
    operability_pct: List[float] = Field(
        default=[40, 45, 55, 65, 75, 85, 90, 88, 80, 65, 50, 42],
        description="Monthly operability percentages (example for North Sea)",
    )
    hs_limit: float = Field(2.5, gt=0.0, description="Hs limit used (m)")

    def best_months(self, min_operability: float = 70.0) -> List[str]:
        """Return months with operability above threshold.

        Args:
            min_operability: Minimum operability % to be considered suitable.

        Returns:
            List of suitable month names.
        """
        return [
            m for m, op in zip(self.month_names, self.operability_pct)
            if op >= min_operability
        ]

    def annual_average(self) -> float:
        """Annual average operability (%)."""
        return round(sum(self.operability_pct) / len(self.operability_pct), 1)


# ---------------------------------------------------------------------------
# Waiting on Weather (WoW) statistics
# ---------------------------------------------------------------------------

class WoWEstimate(BaseModel):
    """Waiting-on-Weather estimate for operation planning.

    Reference: Noble Denton 0027/ND Section 4, DNV-OS-H101.
    """
    operation_duration_hours: float = Field(72.0, gt=0.0, description="Reference operation duration (hours)")
    operability_pct: float = Field(70.0, gt=0.0, le=100.0, description="Operability (%)")
    contingency_factor: float = Field(1.5, ge=1.0, description="Contingency factor on WoW")

    def estimate_wow_hours(self) -> Dict[str, float]:
        """Estimate waiting-on-weather hours.

        Simple model: WoW = (1/operability - 1) * duration * contingency
        Better: use persistence analysis for access probability.

        Returns:
            Dict with WoW estimates.
        """
        op_frac = self.operability_pct / 100.0
        if op_frac >= 1.0:
            wow_base = 0.0
        else:
            wow_base = (1.0 / op_frac - 1.0) * self.operation_duration_hours

        wow_design = wow_base * self.contingency_factor
        total_duration = self.operation_duration_hours + wow_design

        return {
            "base_wow_hours": round(wow_base, 1),
            "design_wow_hours": round(wow_design, 1),
            "total_operation_hours": round(total_duration, 1),
            "total_operation_days": round(total_duration / 24.0, 1),
        }
