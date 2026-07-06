"""Shared rolling go/no-go spine (#1377).

The classifier + lead-time mechanics that BOTH the motion-forecast rolling decision
(#1359, ``motion_forecast.decision``) and the drilling-riser twin loop (#1377,
``drilling_riser.twin_loop``) roll their verdicts with — the single "one rolling-verdict
spine" the two domains share. Generic over any ``(t, magnitude-series, caution, limit)``;
imports only numpy + the ``DecisionState`` taxonomy from
``marine_ops.installation.go_no_go`` (no motion / riser domain imports, no cycles).

``motion_forecast.decision`` RE-EXPORTS these names, so ``decision._classify`` /
``decision._first_crossing`` / ``decision.DISPLAY_LABEL`` keep resolving for existing
consumers (e.g. ``motion_forecast.reconcile``).
"""
from __future__ import annotations

from typing import Optional

import numpy as np

from digitalmodel.marine_ops.installation.go_no_go import DecisionState

#: "CAUTION" is the operator-facing display label for ``DecisionState.MARGINAL``.
DISPLAY_LABEL = {
    DecisionState.GO: "GO",
    DecisionState.MARGINAL: "CAUTION",
    DecisionState.NO_GO: "NO-GO",
}


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
