"""Output-space recalibration of a forecast (digitalmodel #1360).

Fits a per-DOF affine correction ``x_corr = gain*x + bias`` (least squares of
forecast -> measured over pooled overlaps) that reduces systematic amplitude
scaling + DC bias.

CAVEAT (by construction): an affine correction is **invariant in correlation**
(a positive-gain scale + shift does not change ``corrcoef``), so it cannot fix
the **phase decorrelation** that dominates lead-time skill decay. RMSE going
down is therefore NOT evidence of a better forecast — a shrinking ``gain``
regresses toward the mean. ``holdout_report`` reports correlation alongside RMSE
so this is visible. This is an OUTPUT-space correction, explicitly not an
RAO-model (hydrodynamic-coefficient) recalibration (deferred).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence, Tuple

import numpy as np

from .models import DOF_NAMES, MotionForecast
from .reconcile import overlap_residuals
from .skill import SkillRecord


@dataclass
class Correction:
    """Per-DOF affine output correction ``x_corr = gain*x + bias``."""

    gain: Dict[str, float]
    bias: Dict[str, float]

    def apply(self, forecast) -> MotionForecast:
        dof = {d: self.gain[d] * np.asarray(forecast.dof[d], dtype=float) + self.bias[d]
               for d in DOF_NAMES}
        return MotionForecast(
            t=np.asarray(forecast.t, dtype=float), dof=dof,
            origin_time=forecast.origin_time, horizon=forecast.horizon,
            metadata={"correction": "affine"},
        )


def _pooled_m_fc(records: Sequence[SkillRecord], dof: str) -> Tuple[np.ndarray, np.ndarray]:
    ms: List[np.ndarray] = []
    fcs: List[np.ndarray] = []
    for r in records:
        _tq, res = overlap_residuals(r.measured, r.forecast)
        m, fc, _ = res[dof]
        ms.append(m)
        fcs.append(fc)
    m_arr, fc_arr = np.concatenate(ms), np.concatenate(fcs)
    finite = np.isfinite(m_arr) & np.isfinite(fc_arr)  # drop MRU dropouts
    return m_arr[finite], fc_arr[finite]


def fit_correction(records: Sequence[SkillRecord], *, var_eps: float = 1e-12) -> Correction:
    """Least-squares per-DOF ``gain, bias`` of forecast -> measured.

    Zero-variance guard: when the forecast DOF is ~constant over the overlap,
    fall back to bias-only (``gain = 1``) instead of dividing by ~0.
    """
    gain: Dict[str, float] = {}
    bias: Dict[str, float] = {}
    for d in DOF_NAMES:
        m, fc = _pooled_m_fc(records, d)
        vfc = float(np.var(fc))
        if vfc <= var_eps:
            g = 1.0
            b = float(np.mean(m) - np.mean(fc))
        else:
            cov = float(np.mean((fc - fc.mean()) * (m - m.mean())))
            g = cov / vfc
            b = float(np.mean(m) - g * np.mean(fc))
        gain[d] = g
        bias[d] = b
    return Correction(gain=gain, bias=bias)


def _corr(a: np.ndarray, b: np.ndarray) -> Optional[float]:
    if a.size >= 2 and np.std(a) > 0 and np.std(b) > 0:
        return float(np.corrcoef(a, b)[0, 1])
    return None


@dataclass
class HoldoutResult:
    rmse_before: float
    rmse_after: float
    correlation_before: Optional[float]
    correlation_after: Optional[float]


def holdout_report(
    records: Sequence[SkillRecord], *, test_fraction: float = 0.5
) -> Dict[str, HoldoutResult]:
    """Fit on a train split, report before/after RMSE + correlation on the test split.

    Generalisation, not memorisation: the correction is fit only on train.
    """
    n = len(records)
    if n < 2:
        raise ValueError("holdout_report needs >= 2 records")
    k = max(1, int(round(n * (1.0 - test_fraction))))
    k = min(k, n - 1)  # keep at least one test record
    train, test = records[:k], records[k:]
    corr = fit_correction(train)

    out: Dict[str, HoldoutResult] = {}
    for d in DOF_NAMES:
        m, fc = _pooled_m_fc(test, d)
        fc_c = corr.gain[d] * fc + corr.bias[d]
        out[d] = HoldoutResult(
            rmse_before=float(np.sqrt(np.mean((m - fc) ** 2))),
            rmse_after=float(np.sqrt(np.mean((m - fc_c) ** 2))),
            correlation_before=_corr(m, fc),
            correlation_after=_corr(m, fc_c),
        )
    return out
