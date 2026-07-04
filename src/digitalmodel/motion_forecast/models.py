"""Data models for short-horizon motion forecasting (digitalmodel #1358).

The engine reconstructs the time-domain 6-DOF motion of an asset a short
horizon ahead, from a *phased* incident-wave forecast propagated to the asset
location and transferred through the asset's RAO.

Design notes
------------
- ``WaveForecast`` carries discrete phased components **and** the
  ``phase_reference_location`` those phases are referenced to. This is the seam
  #1357 (``wave_forecast``) plugs into; any object exposing these attributes is
  accepted (structural typing).
- Motion is stored per DOF: translations (surge/sway/heave) in metres,
  rotations (roll/pitch/yaw) in **degrees** (matching ``DisplacementRAO`` units).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Sequence, Tuple

import numpy as np

GRAVITY = 9.80665  # m/s^2

DOF_NAMES: Tuple[str, ...] = ("surge", "sway", "heave", "roll", "pitch", "yaw")
ROTATION_DOFS: Tuple[str, ...] = ("roll", "pitch", "yaw")


@dataclass(frozen=True)
class WaveComponent:
    """A single phased wave component.

    Attributes
    ----------
    omega:
        Angular frequency (rad/s).
    amplitude:
        Component amplitude (m).
    phase:
        Phase (rad) at ``WaveForecast.phase_reference_location`` under the
        cosine convention ``eta = amplitude * cos(omega * t + phase)``.
    heading:
        Propagation heading (deg), *going-to* convention, measured from +x.
    """

    omega: float
    amplitude: float
    phase: float
    heading: float = 0.0


@dataclass
class WaveForecast:
    """A phase-resolved short-horizon wave forecast at a reference location.

    ``#1357`` produces this (or any duck-typed equivalent). The engine only
    reads these attributes.
    """

    components: List[WaveComponent]
    horizon: float                                   # s, predictable-zone length
    origin_time: float = 0.0                          # s, "now"
    phase_reference_location: Tuple[float, float] = (0.0, 0.0)  # (x, y) m
    water_depth: Optional[float] = None               # m; None => deep water

    def frequencies(self) -> np.ndarray:
        return np.array([c.omega for c in self.components], dtype=float)

    def amplitudes(self) -> np.ndarray:
        return np.array([c.amplitude for c in self.components], dtype=float)


@dataclass
class MotionForecast:
    """Reconstructed 6-DOF motion over a time grid.

    ``dof`` maps each DOF name to a time series aligned with ``t`` — translations
    in metres, rotations in degrees. ``horizon``/``origin_time`` carry the
    predictable-zone bounds the forecast is valid within.
    """

    t: np.ndarray
    dof: Dict[str, np.ndarray]
    origin_time: float
    horizon: float
    metadata: Dict[str, object] = field(default_factory=dict)

    def significant(self, dof_name: str) -> float:
        """Significant (2*std) amplitude of a DOF over the record."""
        return 2.0 * float(np.std(self.dof[dof_name]))

    def max_abs(self, dof_name: str) -> float:
        return float(np.max(np.abs(self.dof[dof_name])))

    def as_arrays(self) -> Dict[str, np.ndarray]:
        out = {"t": self.t}
        out.update(self.dof)
        return out
