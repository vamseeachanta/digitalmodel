"""Transparent, bounded residual-correction spine (twin B #1374, epic #1372).

Domain-neutral primitives: a bounded affine :class:`ResidualModel` that corrects a
physics *prediction* toward *measured* reality, and forecast-skill metrics. Generic
on float arrays — it imports NOTHING from a physics domain (no ``drilling_riser`` /
``motion_forecast``) and, deliberately, NOTHING from ``digitalmodel.citations``: the
correction is a data-driven empirical estimate, never a standard and never a cited
value. The drilling-riser adapter (:mod:`digitalmodel.drilling_riser.response_correction`)
consumes it now; the motion-forecast learning loop (#1360) can import the same spine.

Safety contract (the correction is a MONITORING aid, not a limit-relaxer):
  * it is TRANSPARENT — the fit is an inspectable (scale, bias), no opaque weights;
  * it is BOUNDED — scale/bias/output are clipped to physical bounds;
  * it FAILS CLOSED — insufficient / out-of-bounds / out-of-training-domain inputs
    ESCALATE (a status), they never silently emit a fabricated correction;
  * a *consumer* decides operability with ``max(corrected, raw)`` so the correction
    can only ever tighten a verdict — that policy lives in the consumer, not here.
"""
from digitalmodel.residual.model import (
    ACTIVE,
    BIAS_MAX,
    MIN_PAIRS,
    SCALE_MAX,
    SCALE_MIN,
    Correction,
    ResidualModel,
)
from digitalmodel.residual.skill import correlation, is_significant, rmse, skill_gain

__all__ = [
    "ResidualModel",
    "Correction",
    "ACTIVE",
    "SCALE_MIN",
    "SCALE_MAX",
    "BIAS_MAX",
    "MIN_PAIRS",
    "rmse",
    "correlation",
    "skill_gain",
    "is_significant",
]
