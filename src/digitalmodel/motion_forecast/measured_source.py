"""Measured-motion ingest (digitalmodel #1367).

Sensing-agnostic sources that yield a :class:`MeasuredMotion`. A
``MeasuredMotionSource`` is anything with ``read() -> MeasuredMotion``; two
concrete implementations ship:

- :class:`SyntheticMMS` — a deterministic stub: a "truth" motion plus optional
  seeded sensor noise, standing in for a live feed in tests/demos.
- :func:`from_csv` — read a 6-DOF time series file.

Real MMS/MRU wire protocols (NMEA / network) and hardware time-sync are
partner/deployment integrations, out of scope here.
"""

from __future__ import annotations

from typing import Protocol, runtime_checkable

import numpy as np

from .measured import MeasuredMotion
from .models import DOF_NAMES


@runtime_checkable
class MeasuredMotionSource(Protocol):
    """Anything that yields a measured motion record."""

    def read(self) -> MeasuredMotion: ...


class SyntheticMMS:
    """Deterministic measured-motion stub: truth + seeded per-DOF noise.

    ``truth`` is any object exposing ``.t`` and ``.dof`` (e.g. a reconstructed
    ``MotionForecast`` used as ground truth). ``noise_std`` (same units as each
    DOF) adds seeded Gaussian sensor noise; 0 reproduces the truth exactly.
    """

    def __init__(self, truth, noise_std: float = 0.0, seed: int = 20260704):
        self._t = np.asarray(truth.t, dtype=float)
        self._dof = {d: np.asarray(truth.dof[d], dtype=float) for d in DOF_NAMES}
        self._noise = float(noise_std)
        self._seed = int(seed)

    def read(self) -> MeasuredMotion:
        rng = np.random.default_rng(self._seed)
        dof = {}
        for d in DOF_NAMES:
            base = self._dof[d]
            dof[d] = base + (rng.normal(0.0, self._noise, size=base.shape)
                             if self._noise > 0 else 0.0)
        return MeasuredMotion(t=self._t.copy(), dof=dof,
                              metadata={"source": "SyntheticMMS", "noise_std": self._noise})


def from_csv(path: str) -> MeasuredMotion:
    """Read a measured 6-DOF record from CSV.

    Columns (header required): ``t, surge, sway, heave, roll, pitch, yaw``.
    ``t`` is absolute seconds; rotations (roll/pitch/yaw) are **degrees** to
    match the model convention. Extra columns are ignored.
    """
    data = np.genfromtxt(path, delimiter=",", names=True)
    cols = data.dtype.names or ()
    required = ("t",) + DOF_NAMES
    missing = [c for c in required if c not in cols]
    if missing:
        raise ValueError(f"CSV missing columns: {missing} (have {list(cols)})")
    t = np.atleast_1d(data["t"]).astype(float)
    dof = {d: np.atleast_1d(data[d]).astype(float) for d in DOF_NAMES}
    return MeasuredMotion(t=t, dof=dof, metadata={"source": f"csv:{path}"})
