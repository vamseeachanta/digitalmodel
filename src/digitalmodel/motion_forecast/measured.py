"""Measured-motion representation (digitalmodel #1367).

The *measured* data mode: live vessel & structure motions from an onboard MMS /
MRU, the "now" ground truth the forecast is anchored to and scored against.

``MeasuredMotion`` shares the ``.t`` / ``.dof`` field contract with
``MotionForecast`` (so it flows through the #1359 ``derived`` layer unchanged),
but its "now" is the **last** sample ``t[-1]`` (the past up to now), whereas a
forecast's now is ``t[0]``. ``t`` is **absolute seconds on a shared wall-clock**
with ``MotionForecast.t`` so the two can be reconciled.

WARNING: do not pass a ``MeasuredMotion`` to ``decision.rolling_decision`` — it
treats ``t[0]`` as "now" and scans forward, which for a measured record classifies
the *oldest* sample and computes lead times into the past. Use
``reconcile.measured_status`` (the sanctioned measured entry point).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict

import numpy as np

from .models import DOF_NAMES


@dataclass
class MeasuredMotion:
    """Measured 6-DOF motion up to "now" (= ``t[-1]``).

    ``t``: absolute seconds (shared clock with ``MotionForecast.t``), ascending.
    ``dof``: name -> series; translations m, rotations deg.
    """

    t: np.ndarray
    dof: Dict[str, np.ndarray]
    metadata: Dict[str, object] = field(default_factory=dict)

    def __post_init__(self):
        self.t = np.asarray(self.t, dtype=float)
        if self.t.ndim != 1 or self.t.size < 1:
            raise ValueError("MeasuredMotion.t must be a non-empty 1-D array")
        if self.t.size >= 2 and not np.all(np.diff(self.t) > 0):
            raise ValueError("MeasuredMotion.t must be strictly ascending (shared clock)")
        missing = [d for d in DOF_NAMES if d not in self.dof]
        if missing:
            raise ValueError(f"MeasuredMotion.dof missing DOFs: {missing}")
        for d in DOF_NAMES:
            self.dof[d] = np.asarray(self.dof[d], dtype=float)
            if self.dof[d].shape != self.t.shape:
                raise ValueError(f"dof[{d!r}] shape {self.dof[d].shape} != t {self.t.shape}")

    @property
    def now(self) -> float:
        return float(self.t[-1])

    def at_now(self) -> Dict[str, float]:
        """Latest 6-DOF values (the current measured motion)."""
        return {d: float(self.dof[d][-1]) for d in DOF_NAMES}

    def window(self, seconds: float) -> "MeasuredMotion":
        """Trailing slice of the last ``seconds`` up to now."""
        if seconds <= 0:
            raise ValueError("window seconds must be > 0")
        mask = self.t >= (self.now - seconds)
        return MeasuredMotion(
            t=self.t[mask],
            dof={d: self.dof[d][mask] for d in DOF_NAMES},
            metadata=dict(self.metadata),
        )
