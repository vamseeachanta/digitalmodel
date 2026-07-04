"""RAO adapters exposing a complex transfer ``H(dof, omega, heading)``.

Two providers implement the same tiny interface the reconstruction core needs:

- :class:`GridRAO` — wraps a parsed ``DisplacementRAO`` grid (from
  ``unified_rao_reader``) and does **complex** (amplitude * e^{i*phase}) bilinear
  interpolation over (frequency, heading). Interpolating the complex value, not
  raw degrees, avoids phase-wraparound artefacts. Phase is normalised to the
  canonical lead convention (:mod:`.conventions`) on construction.
- :class:`AnalyticRAO` — closed-form ``H`` callables per DOF, for tests and for
  a quick idealised asset without a solver file.

Consuming ``DisplacementRAO`` directly (not ``rao_interpolator.interpolate_2d``)
sidesteps the ``UnifiedRAOData`` vs ``rao_processor.RAOData`` shape mismatch.
"""

from __future__ import annotations

from typing import Callable, Dict, Optional

import numpy as np

from .conventions import RAOSource, normalize_phase_deg
from .models import DOF_NAMES


def _interp_complex(
    freqs: np.ndarray, headings: np.ndarray, grid: np.ndarray,
    omega: float, heading: float,
) -> complex:
    """Bilinear interpolation of a complex (n_freq, n_head) grid.

    Flat extrapolation outside range (np.interp semantics). Single-heading grids
    interpolate over frequency only.
    """
    def _interp_freq(col: np.ndarray) -> complex:
        re = np.interp(omega, freqs, col.real)
        im = np.interp(omega, freqs, col.imag)
        return complex(re, im)

    if headings.size == 1:
        return _interp_freq(grid[:, 0])

    # bracket heading (no periodic wrap; RAO heading axis assumed monotonic)
    h = float(np.clip(heading, headings[0], headings[-1]))
    j = int(np.searchsorted(headings, h))
    if j <= 0:
        return _interp_freq(grid[:, 0])
    if j >= headings.size:
        return _interp_freq(grid[:, -1])
    h0, h1 = headings[j - 1], headings[j]
    c0 = _interp_freq(grid[:, j - 1])
    c1 = _interp_freq(grid[:, j])
    w = 0.0 if h1 == h0 else (h - h0) / (h1 - h0)
    return (1.0 - w) * c0 + w * c1


class GridRAO:
    """Complex transfer built from a parsed ``DisplacementRAO`` grid."""

    def __init__(self, displacement_rao, source: RAOSource):
        rao = displacement_rao
        self.freqs = np.asarray(rao.frequencies, dtype=float)     # rad/s
        self.headings = np.asarray(rao.headings, dtype=float)     # deg
        self.source = source
        self._H: Dict[str, np.ndarray] = {}
        for dof in DOF_NAMES:
            d = rao.get_dof_data(dof)
            phase_rad = np.deg2rad(normalize_phase_deg(d.phase, source))
            self._H[dof] = np.asarray(d.amplitude, dtype=float) * np.exp(1j * phase_rad)

    @classmethod
    def from_unified(cls, unified, source: RAOSource) -> "GridRAO":
        if not unified.has_displacement():
            raise ValueError("UnifiedRAOData has no displacement RAO")
        return cls(unified.displacement, source)

    @classmethod
    def from_file(cls, file_path: str, source: RAOSource) -> "GridRAO":
        from digitalmodel.marine_ops.marine_analysis.unified_rao_reader import (
            read_rao_file,
        )
        unified = read_rao_file(file_path)
        return cls.from_unified(unified, source)

    def H(self, dof: str, omega: float, heading: float = 0.0) -> complex:
        return _interp_complex(self.freqs, self.headings, self._H[dof], omega, heading)


class AnalyticRAO:
    """Closed-form transfer: ``callables[dof](omega, heading) -> complex``.

    Missing DOFs return ``0`` (no response). Amplitudes are motion-per-unit-wave
    (m/m for translations, deg/m for rotations); phase is the canonical lead.
    """

    def __init__(self, callables: Dict[str, Callable[[float, float], complex]]):
        self._c = dict(callables)

    def H(self, dof: str, omega: float, heading: float = 0.0) -> complex:
        f: Optional[Callable[[float, float], complex]] = self._c.get(dof)
        return complex(0.0) if f is None else complex(f(omega, heading))
