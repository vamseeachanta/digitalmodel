"""Spectral bookkeeping + realised-variance validation (digitalmodel #1358).

Two complementary checks:

- ``test_analytic_variance_matches_oracle_m0`` — a *bookkeeping/convergence*
  check that the variance the reconstruction will realise,
  ``sum(0.5*a_i^2*|H_i|^2)``, equals the ``hydrodynamics.seakeeping`` spectral
  m0 ``integral(|H|^2 S dω)`` on a fine grid. Both sides use the same S and |H|,
  so this validates the amplitude/energy bookkeeping (a_i = sqrt(2 S dω)), not
  the time-domain engine itself.
- ``test_realized_record_variance_close_to_analytic`` — drives
  ``reconstruct_motion`` and checks the realised record variance matches, which
  *does* exercise the engine (phase-preservation itself is guarded by the
  regular-wave tests in test_reconstruct).
"""

import numpy as np
import pytest

from digitalmodel.hydrodynamics.seakeeping import (
    compute_response_spectrum,
    spectral_moments,
)
from digitalmodel.motion_forecast import (
    AnalyticRAO,
    reconstruct_motion,
    synthesize_forecast,
)
from digitalmodel.motion_forecast.wave_source import jonswap_spectrum


def _heave_H(omega, heading=0.0):
    """Smooth 2nd-order-ish heave transfer (complex), Tn=10s, zeta=0.2."""
    wn = 2.0 * np.pi / 10.0
    r = omega / wn
    return 1.0 / complex(1.0 - r * r, 2.0 * 0.2 * r)


def test_energy_conservation_of_synthesis():
    hs, tp = 3.0, 10.0
    fc = synthesize_forecast(hs, tp, n_components=300, horizon=90.0)
    a = fc.amplitudes()
    m0 = float(np.sum(0.5 * a**2))
    assert 4.0 * np.sqrt(m0) == pytest.approx(hs, rel=0.01)


def test_analytic_variance_matches_oracle_m0():
    hs, tp = 2.5, 9.0
    fc = synthesize_forecast(hs, tp, n_components=400, horizon=90.0)

    # reconstruction-side analytic variance: sum 0.5 a_i^2 |H_i|^2
    var_recon = 0.0
    for c in fc.components:
        var_recon += 0.5 * c.amplitude**2 * abs(_heave_H(c.omega)) ** 2

    # oracle on a fine grid: m0 = integral(|H|^2 S dω)
    wp = 2.0 * np.pi / tp
    w = np.linspace(0.4 * wp, 3.2 * wp, 4000)
    S = jonswap_spectrum(w, hs, tp)
    rao_amp = np.array([abs(_heave_H(wi)) for wi in w])
    S_resp = compute_response_spectrum(rao_amp, S)
    m0 = spectral_moments(w, S_resp, orders=[0])[0]

    assert var_recon == pytest.approx(m0, rel=0.02)


def test_realized_record_variance_close_to_analytic():
    """A long reconstructed record realises the analytic variance (looser)."""
    hs, tp = 2.5, 9.0
    fc = synthesize_forecast(hs, tp, n_components=400, horizon=1500.0, seed=7)
    rao = AnalyticRAO({"heave": _heave_H})
    mf = reconstruct_motion(fc, rao, dt=0.25)

    var_recon = sum(0.5 * c.amplitude**2 * abs(_heave_H(c.omega)) ** 2
                    for c in fc.components)
    realized = float(np.var(mf.dof["heave"]))
    assert realized == pytest.approx(var_recon, rel=0.12)
