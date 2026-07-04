"""Phase-resolved wave forecast: horizon, directional spread, surface (#1357).

The physics-honest pieces the offering owns, behind the ``WaveForecast`` protocol
(#1358). The genuinely hard front-end — reconstructing a phase-resolved surface
from X-band radar / wave lidar and evolving it nonlinearly (HOS-Ocean) — is
**partner/licensed** and out of scope; ``WaveFieldSource`` is the seam it plugs
into. This module provides linear-wave-theory synthesis/propagation only.

Two honestly-named horizon functions:
- ``coherence_horizon`` — an INDICATIVE bandwidth-limited coherence time, NOT the
  Deterministic Predictable Zone.
- ``dpz_horizon`` — the rigorous group-velocity DPZ temporal width, which needs a
  measurement aperture.
"""

from __future__ import annotations

from typing import List, Optional, Protocol, Sequence, Tuple, runtime_checkable

import numpy as np

from .models import GRAVITY, WaveComponent, WaveForecast
from .reconstruct import wavenumber
from .wave_source import jonswap_spectrum


# ---- horizons ---------------------------------------------------------------

def _moments(components: Sequence[WaveComponent]) -> Tuple[float, float, float]:
    """Spectral moments m0, m1, m2 from a discrete component set (in omega)."""
    w = np.array([c.omega for c in components], dtype=float)
    e = np.array([0.5 * c.amplitude**2 for c in components], dtype=float)  # variance
    m0 = float(np.sum(e))
    m1 = float(np.sum(e * w))
    m2 = float(np.sum(e * w * w))
    return m0, m1, m2


def coherence_horizon(
    components: Sequence[WaveComponent], *,
    tau_min: float = 15.0, tau_max: float = 120.0, k_coh: float = 1.0,
) -> float:
    """INDICATIVE bandwidth-limited coherence time (seconds).

    NOT the Deterministic Predictable Zone (use :func:`dpz_horizon` for that).
    ``nu = sqrt(max(0, m0*m2/m1^2 - 1))`` (spectral width); ``tau = k_coh /
    (nu * f_peak)`` clamped to ``[tau_min, tau_max]``. Narrow-band -> small nu ->
    longer horizon. Monochromatic (nu = 0) -> ``tau_max``.
    """
    m0, m1, m2 = _moments(components)
    if m0 <= 0 or m1 <= 0:
        return tau_max
    nu = float(np.sqrt(max(0.0, m0 * m2 / (m1 * m1) - 1.0)))
    if nu <= 1e-9:
        return tau_max
    # peak frequency (Hz) = omega of the largest component / 2pi
    w_peak = max(components, key=lambda c: c.amplitude).omega
    f_peak = w_peak / (2.0 * np.pi)
    tau = k_coh / (nu * f_peak)
    return float(np.clip(tau, tau_min, tau_max))


def dpz_horizon(
    components: Sequence[WaveComponent], *,
    aperture: float, energetic_fraction: float = 0.9,
) -> float:
    """Rigorous group-velocity DPZ temporal width (seconds).

    Over the band carrying the central ``energetic_fraction`` of variance,
    ``tau = aperture * (1/cg_min - 1/cg_max)`` with deep-water group velocity
    ``cg = 0.5 g / omega`` (the spread in group-arrival times across the
    measurement aperture; Naaijen 2018).
    """
    if aperture <= 0:
        raise ValueError("aperture must be > 0 for the DPZ horizon")
    comps = sorted(components, key=lambda c: 0.5 * c.amplitude**2, reverse=True)
    total = sum(0.5 * c.amplitude**2 for c in comps)
    if total <= 0:
        raise ValueError("no wave energy")
    acc, band = 0.0, []
    for c in comps:
        band.append(c)
        acc += 0.5 * c.amplitude**2
        if acc >= energetic_fraction * total:
            break
    cg = np.array([0.5 * GRAVITY / c.omega for c in band], dtype=float)
    return float(aperture * (1.0 / cg.min() - 1.0 / cg.max()))


# ---- directional spreading --------------------------------------------------

def directional_spread(
    headings: np.ndarray, mean_heading: float, s: float
) -> np.ndarray:
    """cos-2s directional weights (sum to 1) over the sampled headings."""
    d = ((np.asarray(headings, float) - mean_heading + 180.0) % 360.0) - 180.0  # [-180,180)
    w = np.cos(np.deg2rad(d) / 2.0) ** (2.0 * s)
    w = np.where(np.abs(d) < 180.0, w, 0.0)
    total = w.sum()
    if total <= 0:
        raise ValueError("degenerate directional spread")
    return w / total


def synthesize_directional_forecast(
    hs: float, tp: float, *,
    mean_heading: float = 0.0, spread_s: float = 10.0,
    n_freq: int = 32, n_dir: int = 7, gamma: float = 3.3,
    dir_halfwidth: float = 90.0,
    aperture: Optional[float] = None,
    phase_reference_location: Tuple[float, float] = (0.0, 0.0),
    water_depth: Optional[float] = None, origin_time: float = 0.0,
    seed: int = 20260704,
) -> WaveForecast:
    """Short-crested phased forecast; physics horizon.

    Reuses ``wave_source``'s omega-grid so ``n_dir=1`` matches the long-crested
    amplitudes. Directional energy split ``a_ij = sqrt(2 S_i dω_i w_j)`` with
    ``sum_j w_j = 1`` preserves variance. Horizon from ``dpz_horizon`` when an
    ``aperture`` is given, else ``coherence_horizon``.
    """
    wp = 2.0 * np.pi / tp
    edges = np.linspace(0.4 * wp, 3.2 * wp, n_freq + 1)
    omega = 0.5 * (edges[:-1] + edges[1:])
    d_omega = np.diff(edges)
    s_i = jonswap_spectrum(omega, hs, tp, gamma)

    if n_dir <= 1:
        headings = np.array([mean_heading])
        weights = np.array([1.0])
    else:
        headings = mean_heading + np.linspace(-dir_halfwidth, dir_halfwidth, n_dir)
        weights = directional_spread(headings, mean_heading, spread_s)

    rng = np.random.default_rng(seed)
    components: List[WaveComponent] = []
    for i in range(n_freq):
        for j in range(len(headings)):
            a = float(np.sqrt(2.0 * s_i[i] * d_omega[i] * weights[j]))
            components.append(WaveComponent(
                omega=float(omega[i]), amplitude=a,
                phase=float(rng.uniform(0.0, 2.0 * np.pi)),
                heading=float(headings[j])))

    horizon = (dpz_horizon(components, aperture=aperture)
               if aperture else coherence_horizon(components))
    return WaveForecast(
        components=components, horizon=horizon, origin_time=origin_time,
        phase_reference_location=phase_reference_location, water_depth=water_depth,
    )


# ---- free surface -----------------------------------------------------------

def surface_elevation(forecast: WaveForecast, x: float, y: float, t):
    """Free-surface elevation eta(x, y, t) (m), LWT.

    Matches ``reconstruct``'s incident-phase convention exactly:
    ``eta = sum_i a_i cos(omega_i t + phi_i - k_i (d_hat_i . (p - p_ref)))``.
    ``t`` may be a scalar or array.
    """
    t = np.asarray(t, dtype=float)
    xr, yr = forecast.phase_reference_location
    dx, dy = x - xr, y - yr
    eta = np.zeros_like(t)
    for c in forecast.components:
        k = wavenumber(c.omega, forecast.water_depth)
        brad = np.deg2rad(c.heading)
        proj = np.cos(brad) * dx + np.sin(brad) * dy
        eta = eta + c.amplitude * np.cos(c.omega * t + c.phase - k * proj)
    return eta


# ---- ingest seam (real reconstruction is partner/licensed) ------------------

@runtime_checkable
class WaveFieldSource(Protocol):
    """Anything that yields a phase-resolved ``WaveForecast``."""

    def read(self) -> WaveForecast: ...


class SyntheticWaveField:
    """Deterministic stub source; the seam a real radar/lidar/HOS front-end fills."""

    def __init__(self, hs: float, tp: float, **kwargs):
        self._hs, self._tp, self._kw = hs, tp, kwargs

    def read(self) -> WaveForecast:
        return synthesize_directional_forecast(self._hs, self._tp, **self._kw)
