"""Fallback synthetic phased-sea generator (digitalmodel #1358).

Produces a :class:`WaveForecast` of discrete phased components from a JONSWAP
sea state with deterministic (seeded) phases. This is the seam that #1357
(``wave_forecast``) replaces with a real phase-resolved surface; until then it
lets the engine build and be validated standalone.

Component amplitudes follow ``a_i = sqrt(2 * S(omega_i) * d_omega)`` so the
component set carries the spectrum's variance (``sum(0.5*a_i^2) = m0``).
"""

from __future__ import annotations

from typing import Optional, Tuple

import numpy as np

from .models import WaveComponent, WaveForecast


def jonswap_spectrum(
    omega: np.ndarray, hs: float, tp: float, gamma: float = 3.3
) -> np.ndarray:
    """JONSWAP one-sided spectral density S(omega) (m^2 s / rad).

    Normalised so that ``4*sqrt(trapz(S, omega)) == hs`` to close numerical
    error in the peak-shape/gamma factors.
    """
    omega = np.asarray(omega, dtype=float)
    wp = 2.0 * np.pi / tp
    sigma = np.where(omega <= wp, 0.07, 0.09)
    # Pierson-Moskowitz base * peak-enhancement
    with np.errstate(divide="ignore", over="ignore", invalid="ignore"):
        pm = (
            5.0 / 16.0 * hs**2 * wp**4 / omega**5
            * np.exp(-1.25 * (wp / omega) ** 4)
        )
        r = np.exp(-((omega - wp) ** 2) / (2.0 * sigma**2 * wp**2))
        s = pm * gamma**r
    s = np.where(np.isfinite(s) & (omega > 0), s, 0.0)
    # renormalise to the requested Hs
    _trapz = np.trapezoid if hasattr(np, "trapezoid") else np.trapz
    m0 = float(_trapz(s, omega))
    if m0 > 0:
        s = s * (hs / (4.0 * np.sqrt(m0))) ** 2
    return s


def synthesize_forecast(
    hs: float,
    tp: float,
    *,
    gamma: float = 3.3,
    heading: float = 0.0,
    n_components: int = 48,
    horizon: float = 90.0,
    omega_range: Optional[Tuple[float, float]] = None,
    phase_reference_location: Tuple[float, float] = (0.0, 0.0),
    water_depth: Optional[float] = None,
    origin_time: float = 0.0,
    seed: int = 20260704,
) -> WaveForecast:
    """Synthesize a deterministic phased :class:`WaveForecast` from a sea state.

    Parameters
    ----------
    hs, tp, gamma:
        JONSWAP significant height (m), peak period (s), peak factor.
    heading:
        Propagation heading (deg, going-to) applied to all components
        (long-crested — directional spreading is a #1358 fast-follow).
    n_components:
        Number of discrete components across ``omega_range``.
    horizon:
        Predictable-zone length (s) carried on the forecast.
    seed:
        Seeds the component phases (deterministic sea).
    """
    wp = 2.0 * np.pi / tp
    lo, hi = omega_range or (0.4 * wp, 3.2 * wp)
    edges = np.linspace(lo, hi, n_components + 1)
    omega = 0.5 * (edges[:-1] + edges[1:])
    d_omega = np.diff(edges)

    s = jonswap_spectrum(omega, hs, tp, gamma)
    amp = np.sqrt(2.0 * s * d_omega)

    rng = np.random.default_rng(seed)
    phases = rng.uniform(0.0, 2.0 * np.pi, size=n_components)

    components = [
        WaveComponent(omega=float(omega[i]), amplitude=float(amp[i]),
                      phase=float(phases[i]), heading=float(heading))
        for i in range(n_components)
    ]
    return WaveForecast(
        components=components,
        horizon=horizon,
        origin_time=origin_time,
        phase_reference_location=phase_reference_location,
        water_depth=water_depth,
    )
