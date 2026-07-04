"""Rolling go/no-go with lead time (digitalmodel #1359).

Scans a governing quantity across the predictable-zone horizon and returns the
operator-facing verdict the live demo (#1361) shows: a state *now* plus how long
the clear window lasts. Reuses the criterion taxonomy from
``marine_ops.installation.go_no_go`` (``DecisionState``, ``CriterionResult``,
``_check_criterion``); "CAUTION" is a display label for ``DecisionState.MARGINAL``.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import List, Optional, Tuple

import numpy as np

from digitalmodel.marine_ops.installation.go_no_go import (
    CriterionResult,
    DecisionState,
    _check_criterion,
)

from .criteria import Criterion
from .derived import compute_governing
from .models import MotionForecast

Offset = Tuple[float, float, float]

DISPLAY_LABEL = {
    DecisionState.GO: "GO",
    DecisionState.MARGINAL: "CAUTION",
    DecisionState.NO_GO: "NO-GO",
}


@dataclass
class RollingDecision:
    """Rolling go/no-go for one operation over the forecast horizon."""

    operation: str
    governing: str
    unit: str
    state: DecisionState
    current_value: float
    caution: float
    limit: float
    lead_time_to_caution: Optional[float]  # s from now; None if never
    lead_time_to_no_go: Optional[float]    # s from now; None if never
    criterion: CriterionResult             # _check_criterion at "now"
    t: np.ndarray
    values: np.ndarray                     # governing magnitude series
    states: List[DecisionState]            # per-instant classification

    @property
    def display(self) -> str:
        return DISPLAY_LABEL[self.state]


def _first_crossing(t: np.ndarray, v: np.ndarray, level: float, now: float) -> Optional[float]:
    """Lead time (s) to the first sample with ``v > level`` (strict), else None.

    Strict ``>`` keeps this consistent with :func:`_classify` and the reused
    ``go_no_go._check_criterion`` band (``value == limit`` is still MARGINAL, so
    it must not register as a NO-GO crossing).
    """
    mask = v > level
    if not mask.any():
        return None
    return float(t[int(np.argmax(mask))] - now)


def _classify(value: float, caution: float, limit: float) -> DecisionState:
    """Classify a governing value, matching ``_check_criterion`` boundaries.

    ``value > limit`` -> NO_GO (FAIL); ``value > caution`` -> MARGINAL (WARNING);
    else GO (PASS). NaN is unsafe -> NO_GO (fail-closed)."""
    if not np.isfinite(value):
        return DecisionState.NO_GO
    if value > limit:
        return DecisionState.NO_GO
    if value > caution:
        return DecisionState.MARGINAL
    return DecisionState.GO


def rolling_decision(
    motion: MotionForecast,
    criterion: Criterion,
    *,
    offset: Optional[Offset] = None,
) -> RollingDecision:
    """Evaluate the rolling go/no-go for ``criterion`` against ``motion``.

    ``state`` reflects the situation *now* (the first sample): GO if clear,
    MARGINAL if in the caution band, NO_GO if over the limit. The lead times
    say how long until the governing quantity first reaches caution / the limit
    (0 if already there, None if never within the horizon).
    """
    series = compute_governing(motion, criterion.governing, offset or criterion.poi_offset)
    t, v = series.t, series.values
    now = float(t[0])
    current = float(v[0])

    state = _classify(current, criterion.caution, criterion.limit)
    lead_caution = 0.0 if current > criterion.caution else _first_crossing(
        t, v, criterion.caution, now)
    lead_no_go = 0.0 if current > criterion.limit else _first_crossing(
        t, v, criterion.limit, now)

    # Fail-closed: a non-finite governing value anywhere is unsafe -> NO-GO now.
    if not np.all(np.isfinite(v)):
        state = DecisionState.NO_GO
        lead_caution = lead_no_go = 0.0

    crit = _check_criterion(
        name=criterion.label, value=current, limit=criterion.limit,
        unit=criterion.unit, above_is_safe=False,
        warning_factor=criterion.warning_factor,
        description=criterion.governing, reference=criterion.basis,
    )
    states = [_classify(float(x), criterion.caution, criterion.limit) for x in v]

    return RollingDecision(
        operation=criterion.key, governing=criterion.governing, unit=series.unit,
        state=state, current_value=current,
        caution=criterion.caution, limit=criterion.limit,
        lead_time_to_caution=lead_caution, lead_time_to_no_go=lead_no_go,
        criterion=crit, t=t, values=v, states=states,
    )
