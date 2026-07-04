#!/usr/bin/env python3
"""
ABOUTME: Free-decay heave damping test for the overset floating body (#1332) —
the decisive attribution of the near-resonance RAO reduction found in #1324.

The #1324 wave sweep found the overset CFD does not reproduce the inviscid
heave resonance (grid-independent RAO ~ 0.7-1.0 vs a potential-flow peak
~ 2.0). A forced-wave sweep cannot separate the candidate causes (physical
bluff-body separation damping vs a gauge-to-body incident-wave decay confound
vs overset numerical damping). A **free-decay** test can: release the body from
a still-water displacement and watch it ring down. There is no wave and no
gauge, so the measured damped natural period and damping ratio are properties of
the body's dynamics alone.

Method
------
- ``build_decay_config(offset)`` = the verified #1302 geometry with the wave off
  (``wave_height = 0``) and the body raised by ``initial_heave_offset`` — a clean
  release-from-displacement in still water at the unchanged depth.
- ``analyze_ringdown(t, z)`` fits the settled ring-down to a damped cosine
  ``z_eq + A0 e^{-zeta*wn*t} cos(wd t + phi)`` (``wd = wn sqrt(1-zeta^2)``) →
  the damped natural frequency and the **total** damping ratio zeta.
- Attribution (:func:`attribution`): compare the measured natural frequency to
  the reference resonance frequency (confirms the body's dynamics), and the
  measured zeta to the potential-flow radiation damping ratio from the frozen
  reference (``rao_reference_capytaine.csv``). The *excess* of zeta over the
  radiation value is the viscous + numerical damping the inviscid BEM omits;
  substituting the measured total damping into the reference transfer function
  predicts the flattened resonance peak, which is checked against the #1324
  forced-sweep near-resonance RAO.
- Two release amplitudes discriminate linear (radiation + numerical, amplitude-
  independent) from quadratic form/separation drag (amplitude-dependent): a
  zeta that grows with release amplitude is the fingerprint of physical
  bluff-body separation damping.

Reference box scale
-------------------
The frozen reference's ``a33``/``b33``/``|F3|`` columns are for the L = 6 m box
of ``generate_rao_reference.py`` (the RAO itself is dimensionless). The damping
ratio and natural frequency are scale-invariant, so the attribution uses those.
"""
from __future__ import annotations

import math
from dataclasses import replace
from typing import Any, Dict, Tuple

from .wave_excited_body import RHO_WATER, WaveExcitedBodyConfig
from .wave_excited_body_rao import load_reference_curve, reference_peak
from .wave_tank import GRAVITY

# Reference box scale (must match generate_rao_reference.py).
_REF_BOX_LENGTH = 6.0
_REF_BEAM = 0.2
_REF_HEIGHT = 0.2
_REF_DENSITY = 500.0


def build_decay_config(offset: float = 0.03,
                       end_time: float = 8.0,
                       base: WaveExcitedBodyConfig | None = None,
                       ) -> WaveExcitedBodyConfig:
    """Still-water free-decay config: release the body from ``+offset`` heave.

    Reuses the verified #1302 geometry (depth 0.4 m, body at x = 13, nz = 49)
    with the wave off. ``offset`` (m) is the initial heave displacement above
    the Archimedes equilibrium; the body, its component mesh and its centre of
    mass shift together (see ``WaveExcitedBodyConfig.initial_heave_offset``).
    """
    base = base or WaveExcitedBodyConfig(nz=49, nx=250)
    return replace(
        base,
        wave_height=0.0,                 # still water
        initial_heave_offset=offset,
        end_time=float(end_time),
        write_interval=1.0,
        steady_window=(0.0, float(end_time)),
        name=f"validation_wave_excited_body_decay_o{offset:g}".replace(".", "p"),
    )


def analyze_ringdown(t, z,
                  settle_start: float = 0.0) -> Dict[str, Any]:
    """Damping ratio + natural frequency of a heave ring-down ``z(t)``.

    Uses the classical **logarithmic-decrement** method on the extrema
    envelope (robust to the decayed tail's noise floor, which would trap a raw
    damped-cosine least-squares fit in a spurious high-frequency minimum), then
    refines with a seeded damped-cosine ``curve_fit``.

    Args:
        t, z: time (s) and CoM height (m) from the sixDoF ``report on`` log.
        settle_start: drop ``t < settle_start`` (solver startup) before fitting.

    Returns:
        Dict with ``z_eq`` (m), ``amp0`` (m), ``zeta`` (total damping ratio),
        ``omega_d``/``omega_n`` (rad/s), ``period_d`` (s), ``n_extrema`` (extrema
        used) and the fit ``rms`` (m). ``zeta``/``omega`` are from the refined
        fit; ``zeta_logdec`` reports the independent log-decrement estimate.
    """
    import numpy as np
    from scipy.optimize import curve_fit
    from scipy.signal import find_peaks

    t = np.asarray(t, float)
    z = np.asarray(z, float)
    keep = t >= settle_start
    t, z = t[keep], z[keep]
    t0 = t - t[0]

    z_eq0 = float(np.median(z[-max(5, len(z) // 10):]))
    d = z - z_eq0

    # frequency seed from the FFT dominant line on a uniform resample — robust
    # to a fast decay (few cycles) and to the tail noise floor that would trap
    # a raw least-squares fit at a spurious high frequency.
    tu = np.linspace(t0[0], t0[-1], len(t0))
    du = np.interp(tu, t0, d)
    spec = np.abs(np.fft.rfft(du * np.hanning(len(du))))
    freqs = np.fft.rfftfreq(len(du), tu[1] - tu[0])
    wd0 = 2.0 * math.pi * float(freqs[1 + int(np.argmax(spec[1:]))])
    wn0 = wd0 / math.sqrt(1.0 - 0.3 ** 2)

    # independent log-decrement cross-check on the extrema envelope, with a
    # distance guard (~0.4 of a period) so noise ripples are not counted.
    dist = max(3, int(0.4 * (2.0 * math.pi / wd0) / (tu[1] - tu[0])))
    noise = float(np.std(d[-max(5, len(d) // 8):]))
    thr = max(3.0 * noise, 0.03 * abs(d[0]))
    hi, _ = find_peaks(d, height=thr, distance=dist)
    lo, _ = find_peaks(-d, height=thr, distance=dist)
    ext = np.sort(np.concatenate([hi, lo]))
    n_extrema = int(len(ext))
    if n_extrema >= 3:
        te, ae = t0[ext], np.abs(d[ext])
        alpha = -float(np.polyfit(te, np.log(ae), 1)[0])      # zeta*wn
        zeta_logdec = float(np.clip(alpha / math.sqrt(wd0 ** 2 + alpha ** 2),
                                    1e-3, 0.95))
    else:
        zeta_logdec = float("nan")

    def model(tt, z_eq, A0, zeta, wn, phi):
        zc = np.clip(zeta, 1e-4, 0.999)
        wd = wn * np.sqrt(1.0 - zc ** 2)
        return z_eq + A0 * np.exp(-zc * wn * tt) * np.cos(wd * tt + phi)

    p0 = [z_eq0, d[0], 0.3, wn0, 0.0]
    bounds = ([z_eq0 - 0.02, -0.1, 1e-3, 0.5 * wn0, -math.pi],
              [z_eq0 + 0.02, 0.1, 0.95, 1.8 * wn0, math.pi])
    try:
        popt, _ = curve_fit(model, t0, z, p0=p0, bounds=bounds, maxfev=200000)
        z_eq, A0, zeta, wn, phi = popt
        rms = float(np.sqrt(np.mean((z - model(t0, *popt)) ** 2)))
    except Exception:
        z_eq, A0, zeta, wn = z_eq0, d[0], 0.3, wn0
        rms = float("nan")

    wd = wn * math.sqrt(1.0 - zeta ** 2)
    return {
        "z_eq": float(z_eq),
        "amp0": float(abs(A0)),
        "zeta": float(zeta),
        "zeta_logdec": float(zeta_logdec),
        "omega_d": float(wd),
        "omega_n": float(wn),
        "period_d": float(2.0 * math.pi / wd),
        "n_extrema": n_extrema,
        "rms": rms,
    }


def reference_scale() -> Tuple[float, float]:
    """(M, c33) of the reference L = 6 m box — the transfer-function scale."""
    M = _REF_DENSITY * _REF_BEAM * _REF_HEIGHT * _REF_BOX_LENGTH
    c33 = RHO_WATER * GRAVITY * _REF_BEAM * _REF_BOX_LENGTH
    return M, c33


def reference_damping_ratio(curve: Dict[str, Any] | None = None
                            ) -> Dict[str, float]:
    """Potential-flow (radiation-only) damping ratio at the reference peak."""
    import numpy as np

    curve = curve or load_reference_curve()
    M, c33 = reference_scale()
    peak_T, _ = reference_peak(curve)
    i = int(np.argmin(np.abs(curve["period_s"] - peak_T)))
    a33 = float(curve["added_mass_a33"][i])
    b33 = float(curve["radiation_damping_b33"][i])
    wn = math.sqrt(c33 / (M + a33))
    zeta_rad = b33 / (2.0 * (M + a33) * wn)
    return {"omega_n": wn, "period_n": 2.0 * math.pi / wn,
            "zeta_radiation": zeta_rad, "a33": a33, "b33": b33}


def predict_peak_rao(zeta_total: float,
                     curve: Dict[str, Any] | None = None) -> float:
    """Reference resonance peak RAO if the total damping ratio were ``zeta_total``.

    Rebuilds the reference transfer function and scales the radiation damping
    ``b33`` so the resonance damping ratio equals ``zeta_total`` (frequency-flat
    scaling), returning the resulting peak RAO. ``zeta_total`` = the radiation
    value reproduces the inviscid peak; larger values flatten it.
    """
    import numpy as np

    curve = curve or load_reference_curve()
    M, c33 = reference_scale()
    om = curve["omega_rad_s"]
    a33 = curve["added_mass_a33"]
    b33 = curve["radiation_damping_b33"]
    F3 = curve["excitation_force_abs"]
    zr = reference_damping_ratio(curve)["zeta_radiation"]
    scale = zeta_total / zr
    rao = np.abs(F3) / np.abs(c33 - (M + a33) * om ** 2 + 1j * (b33 * scale) * om)
    return float(np.max(rao))


def attribution(decay: Dict[str, Any],
                forced_rao_near_resonance: float,
                curve: Dict[str, Any] | None = None) -> Dict[str, Any]:
    """Attribute the #1324 near-resonance reduction from a free-decay result.

    Args:
        decay: :func:`analyze_ringdown` output (measured zeta, omega_n).
        forced_rao_near_resonance: the #1324 forced-sweep RAO nearest the peak
            (the response the reduction is being explained).
    """
    curve = curve or load_reference_curve()
    ref = reference_damping_ratio(curve)
    zeta_cfd = decay["zeta"]
    zeta_rad = ref["zeta_radiation"]
    predicted_peak = predict_peak_rao(zeta_cfd, curve)
    inviscid_peak = predict_peak_rao(zeta_rad, curve)
    return {
        "omega_n_cfd": decay["omega_n"],
        "omega_n_reference": ref["omega_n"],
        "omega_n_rel_error": (decay["omega_n"] - ref["omega_n"]) / ref["omega_n"],
        "zeta_cfd": zeta_cfd,
        "zeta_radiation": zeta_rad,
        "zeta_excess_ratio": zeta_cfd / zeta_rad,
        "predicted_peak_rao": predicted_peak,
        "inviscid_peak_rao": inviscid_peak,
        "forced_rao_near_resonance": forced_rao_near_resonance,
        "peak_rao_prediction_error":
            (predicted_peak - forced_rao_near_resonance) / forced_rao_near_resonance,
    }
