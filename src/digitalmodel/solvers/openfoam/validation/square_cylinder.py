# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Summarize supplied square-cylinder drag and lift coefficient histories.

Definitions are arithmetic sample mean Cd, population RMS of Cl minus its
sample mean, and Strouhal St = shedding_frequency * width / speed. These
are postprocessing definitions, not experimental reference values.

This synthetic example has eight complete cycles of a unit-frequency sine.
The finite geometric-series identity gives mean Cd = 2 and lift RMS =
3/sqrt(2); the Strouhal definition gives St = 1 * 2 / 4. All example inputs
are chosen for this hand calculation, not taken from a solver or experiment.

>>> times = np.arange(256) / 32
>>> cd = 2 + 0.5 * np.cos(2 * np.pi * times)
>>> cl = 3 * np.sin(2 * np.pi * times)
>>> summary = summarize_square_cylinder_history(
...     times, cd, cl, speed=4, width=2, averaging_interval=(0, 8)
... )
>>> tuple(round(value, 1) for value in (
...     summary.mean_cd, summary.rms_cl, summary.shedding_frequency,
...     summary.strouhal,
... ))
(2.0, 2.1, 1.0, 0.5)
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
from numpy.typing import ArrayLike

from digitalmodel.solvers.openfoam.spectral_analysis import compute_fft_spectrum


@dataclass(frozen=True)
class SquareCylinderSummary:
    """Sample statistics and the dominant nonzero lift FFT bin.

    Attributes:
        mean_cd: Arithmetic mean drag coefficient in the selected interval.
        rms_cl: Population RMS of lift coefficient fluctuations about its mean.
        shedding_frequency: Dominant positive FFT-bin frequency in Hz; NaN
            when the selected lift has no fluctuating spectral content.
        strouhal: Shedding frequency times width divided by speed; NaN
            when no shedding frequency can be identified.
        averaging_interval: Requested half-open time bounds in seconds.
        sample_count: Number of supplied samples inside those bounds.
        frequency_resolution: FFT-bin spacing, sample rate / sample count, Hz.
    """

    mean_cd: float
    rms_cl: float
    shedding_frequency: float
    strouhal: float
    averaging_interval: tuple[float, float]
    sample_count: int
    frequency_resolution: float


def summarize_square_cylinder_history(
    times: ArrayLike,
    cd: ArrayLike,
    cl: ArrayLike,
    *,
    speed: float,
    width: float,
    averaging_interval: tuple[float, float],
) -> SquareCylinderSummary:
    """Summarize uniformly sampled coefficients over explicit time bounds.

    All metrics use samples satisfying ``start <= time < stop``. Bounds need
    not coincide with sample times; only supplied samples are used, without
    interpolation or extrapolation. The full supplied history is validated
    before selecting this interval. Uniformity allows a relative spacing
    error of sqrt(float64 machine epsilon), a floating-point tolerance rather
    than permission to resample irregular data.

    The existing one-sided FFT API removes the lift mean. A rectangular
    window preserves the selected record and its exact discrete frequencies.
    The largest positive-frequency amplitude sets the shedding frequency
    (including Nyquist); equal amplitudes select the lowest frequency.
    Resolution is sample_rate / sample_count; no sub-bin estimation is made.
    Four selected samples are the minimum policy, providing two positive
    frequency bins, including Nyquist, in the shortest accepted record.
    Record length and sampling must still resolve the physical shedding.

    Args:
        times: One-dimensional, finite, strictly increasing sample times, s.
        cd: One-dimensional dimensionless drag coefficients aligned to times.
        cl: One-dimensional dimensionless lift coefficients aligned to times.
        speed: Finite positive reference flow speed in m/s.
        width: Finite positive square-cylinder reference width in m.
        averaging_interval: Finite increasing (start, stop) time bounds in s.

    Returns:
        Statistics of the selected record, with its sample count and FFT
        resolution. A constant lift record has zero RMS and NaN frequency/St.
        The result makes no claim of agreement with experimental literature.

    Raises:
        ValueError: If inputs are nonfinite, histories are not aligned vectors
            with uniform increasing times, scales/bounds are invalid, or fewer
            than four samples are supplied or selected.
    """
    time_values, drag, lift = (
        np.asarray(values, dtype=np.float64) for values in (times, cd, cl)
    )
    if any(values.ndim != 1 for values in (time_values, drag, lift)):
        raise ValueError("times, cd and cl must be one-dimensional.")
    if not (time_values.size == drag.size == lift.size):
        raise ValueError("times, cd and cl must have the same length.")
    if any(not np.all(np.isfinite(values)) for values in (time_values, drag, lift)):
        raise ValueError("times, cd and cl must contain only finite values.")
    if time_values.size < 4:
        raise ValueError("At least four history samples are required.")
    for name, scale in (("speed", speed), ("width", width)):
        if not np.isfinite(scale) or scale <= 0:
            raise ValueError(f"{name} must be finite and positive.")

    bounds = np.asarray(averaging_interval, dtype=np.float64)
    if bounds.shape != (2,) or not np.all(np.isfinite(bounds)):
        raise ValueError("averaging_interval must contain two finite bounds.")
    start, stop = (float(bound) for bound in bounds)
    if start >= stop:
        raise ValueError("averaging_interval must have start < stop.")

    spacings = np.diff(time_values)
    if np.any(spacings <= 0):
        raise ValueError("times must be strictly increasing.")
    spacing = float(np.mean(spacings))
    if not np.allclose(
        spacings, spacing, rtol=np.sqrt(np.finfo(np.float64).eps), atol=0.0
    ):
        raise ValueError("times must be uniformly sampled.")

    selected = (time_values >= start) & (time_values < stop)
    sample_count = int(np.count_nonzero(selected))
    if sample_count < 4:
        raise ValueError("At least four samples must lie in averaging_interval.")
    selected_lift = lift[selected]
    fluctuating_lift = selected_lift - np.mean(selected_lift)
    # Constant records have exactly zero fluctuations even when summation of
    # a repeated decimal value would round its mean away from that value.
    if np.all(selected_lift == selected_lift[0]):
        fluctuating_lift = np.zeros_like(selected_lift)

    sample_rate = 1.0 / spacing
    frequencies, amplitudes = compute_fft_spectrum(
        fluctuating_lift, sample_rate, detrend="constant", window=False
    )
    # The real FFT starts at DC; all remaining bins have positive frequency.
    peak = int(np.argmax(amplitudes[1:])) + 1
    frequency = float(frequencies[peak]) if amplitudes[peak] > 0 else float("nan")

    return SquareCylinderSummary(
        mean_cd=float(np.mean(drag[selected])),
        rms_cl=float(np.sqrt(np.mean(fluctuating_lift**2))),
        shedding_frequency=frequency,
        strouhal=frequency * width / speed,
        averaging_interval=(start, stop),
        sample_count=sample_count,
        frequency_resolution=sample_rate / sample_count,
    )
