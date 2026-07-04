"""Reconcile measured motion against a forecast (digitalmodel #1367).

All alignment is on the shared absolute wall-clock (``MotionForecast.t`` and
``MeasuredMotion.t`` are both absolute seconds). The forecast is **resampled**
onto the measured timestamps before any metric is taken. Metrics only — the
#1360 learning loop consumes these; recalibration lives there.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple

import numpy as np

from digitalmodel.marine_ops.installation.go_no_go import DecisionState

from .criteria import Criterion
from .decision import DISPLAY_LABEL, _classify
from .derived import compute_governing
from .measured import MeasuredMotion
from .models import DOF_NAMES

Offset = Tuple[float, float, float]


def _resample(forecast, t_query: np.ndarray) -> Dict[str, np.ndarray]:
    """Per-DOF interpolate the forecast onto ``t_query`` (absolute s).

    Fail-closed: raises if any query time is outside the forecast's range
    (no extrapolation beyond the predictable zone).
    """
    ft = np.asarray(forecast.t, dtype=float)
    tq = np.asarray(t_query, dtype=float)
    if tq.size and (tq.min() < ft[0] - 1e-9 or tq.max() > ft[-1] + 1e-9):
        raise ValueError(
            f"query times [{tq.min():.3f}, {tq.max():.3f}] outside forecast "
            f"range [{ft[0]:.3f}, {ft[-1]:.3f}] (no extrapolation)"
        )
    return {d: np.interp(tq, ft, np.asarray(forecast.dof[d], dtype=float))
            for d in DOF_NAMES}


def seam_offset(measured: MeasuredMotion, forecast) -> Dict[str, float]:
    """Per-DOF ``measured(now) - forecast(now)`` at the measured "now".

    Raises if ``measured.now`` is outside the forecast range.
    """
    now = measured.now
    fc = _resample(forecast, np.array([now]))
    return {d: float(measured.dof[d][-1] - fc[d][0]) for d in DOF_NAMES}


@dataclass
class DofError:
    rmse: float
    bias: float
    correlation: Optional[float]


def overlap_error(
    measured: MeasuredMotion, forecast, *, min_overlap_samples: int = 3
) -> Dict[str, DofError]:
    """Per-DOF RMSE / bias / correlation of forecast vs measured on their overlap.

    The forecast is interpolated onto the measured timestamps within the
    absolute-time overlap ``[max(starts), min(ends)]``. Correlation is ``None``
    on a zero-variance or too-short window (RMSE/bias remain defined).
    """
    mt = measured.t
    lo = max(mt[0], forecast.t[0])
    hi = min(mt[-1], forecast.t[-1])
    if hi <= lo:
        raise ValueError("no time overlap between measured and forecast")
    mask = (mt >= lo) & (mt <= hi)
    tq = mt[mask]
    if tq.size < min_overlap_samples:
        raise ValueError(
            f"overlap has {tq.size} samples (< min_overlap_samples={min_overlap_samples})"
        )
    fc = _resample(forecast, tq)
    out: Dict[str, DofError] = {}
    for d in DOF_NAMES:
        m = measured.dof[d][mask]
        err = m - fc[d]
        rmse = float(np.sqrt(np.mean(err**2)))
        bias = float(np.mean(err))
        # tq.size >= min_overlap_samples already guaranteed above
        if np.std(m) > 0 and np.std(fc[d]) > 0:
            corr = float(np.corrcoef(m, fc[d])[0, 1])
        else:
            corr = None
        out[d] = DofError(rmse=rmse, bias=bias, correlation=corr)
    return out


@dataclass
class MeasuredDecision:
    """Instantaneous measured-mode go/no-go (mirrors RollingDecision, no lead time)."""

    operation: str
    governing: str
    unit: str
    state: DecisionState
    current_value: float
    caution: float
    limit: float
    t: np.ndarray
    values: np.ndarray
    states: List[DecisionState]

    @property
    def display(self) -> str:
        return DISPLAY_LABEL[self.state]


def measured_status(
    measured: MeasuredMotion, criterion: Criterion, *, offset: Optional[Offset] = None
) -> MeasuredDecision:
    """Classify the **latest** measured governing value (now = ``t[-1]``).

    Uses the same ``decision._classify`` as the forecast mode, so measured and
    forecast classify identically (strict boundaries + NaN -> NO-GO).
    """
    if measured.t.size < 2:
        raise ValueError(
            "measured_status requires >= 2 samples (a velocity governing needs a time base)"
        )
    series = compute_governing(measured, criterion.governing, offset or criterion.poi_offset)
    v = series.values
    current = float(v[-1])
    state = _classify(current, criterion.caution, criterion.limit)
    states = [_classify(float(x), criterion.caution, criterion.limit) for x in v]
    return MeasuredDecision(
        operation=criterion.key, governing=criterion.governing, unit=series.unit,
        state=state, current_value=current,
        caution=criterion.caution, limit=criterion.limit,
        t=series.t, values=v, states=states,
    )
