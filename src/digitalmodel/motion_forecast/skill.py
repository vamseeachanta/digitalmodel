"""Forecast skill across many measured records (digitalmodel #1360).

Pools forecast-vs-measured residuals across a batch of ``SkillRecord`` pairs to
show how skill degrades with lead time and to summarise aggregate error.

Statistical care (per review):
- RMSE and bias **pool** residuals across records (sample-weighted — not a mean
  of per-record RMSEs; note denser-sampled records carry more weight, and
  autocorrelated residuals reduce the effective sample count).
- Non-finite residuals (e.g. MRU dropouts) are masked out before pooling, so one
  bad sample cannot poison the batch.
- Correlation does **not** pool (concatenating records with different DC offsets
  is a Simpson artifact) — it is reported as the per-record distribution
  (median + IQR).
- Lead time uses ``lead = t_measured - forecast.origin_time`` (>= 0, since
  ``origin_time == forecast.t[0]`` and the overlap starts at ``max(starts)``).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Sequence, Tuple

import numpy as np

from .measured import MeasuredMotion
from .models import DOF_NAMES
from .reconcile import overlap_error, overlap_residuals


@dataclass
class SkillRecord:
    """A forecast paired with the motion later measured on the shared clock."""

    forecast: object      # MotionForecast (or duck-typed .t/.dof/.origin_time)
    measured: MeasuredMotion

    def __post_init__(self):
        lo = max(self.measured.t[0], self.forecast.t[0])
        hi = min(self.measured.t[-1], self.forecast.t[-1])
        if hi <= lo:
            raise ValueError("SkillRecord: forecast and measured do not overlap in time")
        if self.forecast.origin_time > self.forecast.t[0] + 1e-9:
            raise ValueError(
                "SkillRecord: forecast.origin_time must be <= forecast.t[0] (lead >= 0)"
            )


def _pooled(records: Sequence[SkillRecord], dof: str) -> Tuple[np.ndarray, np.ndarray]:
    """Concatenate (lead_time, residual) across records for one DOF."""
    leads: List[np.ndarray] = []
    resid: List[np.ndarray] = []
    for r in records:
        tq, res = overlap_residuals(r.measured, r.forecast)
        leads.append(tq - float(r.forecast.origin_time))
        resid.append(res[dof][2])
    if not leads:
        return np.array([]), np.array([])
    lead_arr = np.concatenate(leads)
    resid_arr = np.concatenate(resid)
    finite = np.isfinite(resid_arr) & np.isfinite(lead_arr)  # drop MRU dropouts
    return lead_arr[finite], resid_arr[finite]


def error_vs_lead_time(
    records: Sequence[SkillRecord], dof: str, *, n_bins: int = 10
) -> Dict[float, float]:
    """RMSE of the pooled residual binned by lead time (the skill-decay curve).

    Returns ``{bin_center: rmse}`` for non-empty bins.
    """
    leads, resid = _pooled(records, dof)
    if leads.size == 0:
        return {}
    edges = np.linspace(leads.min(), leads.max() + 1e-12, n_bins + 1)
    out: Dict[float, float] = {}
    for i in range(n_bins):
        m = (leads >= edges[i]) & (leads < edges[i + 1])
        if np.any(m):
            center = 0.5 * (edges[i] + edges[i + 1])
            out[float(center)] = float(np.sqrt(np.mean(resid[m] ** 2)))
    return out


@dataclass
class AggregateSkill:
    rmse: float
    bias: float
    n_samples: int
    correlation_median: float | None
    correlation_iqr: float | None


def aggregate_skill(records: Sequence[SkillRecord]) -> Dict[str, AggregateSkill]:
    """Pooled RMSE/bias + per-record correlation distribution, per DOF."""
    if not records:
        raise ValueError("aggregate_skill needs at least one record")
    out: Dict[str, AggregateSkill] = {}
    for d in DOF_NAMES:
        _leads, resid = _pooled(records, d)
        corrs = [c for c in
                 (overlap_error(r.measured, r.forecast)[d].correlation for r in records)
                 if c is not None]
        if corrs:
            cmed = float(np.median(corrs))
            ciqr = float(np.percentile(corrs, 75) - np.percentile(corrs, 25))
        else:
            cmed = ciqr = None
        out[d] = AggregateSkill(
            rmse=float(np.sqrt(np.mean(resid**2))) if resid.size else 0.0,
            bias=float(np.mean(resid)) if resid.size else 0.0,
            n_samples=int(resid.size),
            correlation_median=cmed, correlation_iqr=ciqr,
        )
    return out
