#!/usr/bin/env python3
"""
ABOUTME: Spectral analysis for CFD sloshing post-processing - extracts the
dominant fluid natural frequency from a time series via FFT / Welch PSD and
provides the analytical prismatic-tank tanh dispersion relation as a cross-check.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import List, Literal, Tuple

import numpy as np
from numpy.typing import NDArray
from scipy.signal import find_peaks, welch

# Standard gravitational acceleration (m/s^2).
GRAVITY = 9.80665


# ============================================================================
# SpectralPeak / SpectrumResult
# ============================================================================


@dataclass
class SpectralPeak:
    """A single peak in a power/amplitude spectrum.

    Attributes:
        frequency: Peak frequency (Hz).
        amplitude: Spectral amplitude/power at the peak (units of the spectrum).
    """

    frequency: float
    amplitude: float


@dataclass
class SpectrumResult:
    """Result of a spectral analysis of a scalar time series.

    Attributes:
        frequencies: Frequency bins (Hz).
        spectrum: Spectral magnitude (FFT) or power density (Welch).
        peaks: Detected peaks sorted by descending amplitude.
        method: The spectral method used ('fft' or 'welch').
        sample_rate: Sampling frequency of the input series (Hz).
    """

    frequencies: NDArray[np.float64]
    spectrum: NDArray[np.float64]
    peaks: List[SpectralPeak]
    method: str
    sample_rate: float

    @property
    def dominant_frequency(self) -> float:
        """The frequency of the highest-amplitude peak (Hz).

        Returns ``nan`` when no peak was detected.
        """
        if not self.peaks:
            return float("nan")
        return self.peaks[0].frequency

    @property
    def dominant_period(self) -> float:
        """Period of the dominant frequency (s); ``nan`` if undefined."""
        f = self.dominant_frequency
        if not math.isfinite(f) or f == 0.0:
            return float("nan")
        return 1.0 / f

    def top_frequencies(self, n: int) -> List[float]:
        """Return the frequencies of the top-``n`` peaks (descending amplitude)."""
        return [p.frequency for p in self.peaks[:n]]


# ============================================================================
# Helpers
# ============================================================================


def _infer_sample_rate(times: NDArray[np.float64]) -> float:
    """Infer the sampling frequency from a (near-)uniform time vector.

    Args:
        times: Monotonically increasing time array (s).

    Returns:
        Sampling frequency (Hz) = 1 / mean(dt).

    Raises:
        ValueError: If fewer than two samples or non-positive spacing.
    """
    times = np.asarray(times, dtype=np.float64)
    if times.size < 2:
        raise ValueError("At least two time samples are required.")
    dt = np.diff(times)
    if np.any(dt <= 0):
        raise ValueError("Time vector must be strictly increasing.")
    return float(1.0 / np.mean(dt))


def _detrend(signal: NDArray[np.float64], mode: str) -> NDArray[np.float64]:
    """Remove the mean ('constant') or a linear trend ('linear'); else no-op."""
    if mode == "constant":
        return signal - np.mean(signal)
    if mode == "linear":
        n = signal.size
        x = np.arange(n, dtype=np.float64)
        slope, intercept = np.polyfit(x, signal, 1)
        return signal - (slope * x + intercept)
    return signal


# ============================================================================
# FFT spectrum
# ============================================================================


def compute_fft_spectrum(
    signal: NDArray[np.float64],
    sample_rate: float,
    *,
    detrend: Literal["constant", "linear", "none"] = "constant",
    window: bool = True,
) -> Tuple[NDArray[np.float64], NDArray[np.float64]]:
    """Compute a one-sided amplitude spectrum of a real signal via FFT.

    Args:
        signal: Real-valued 1-D time series (uniformly sampled).
        sample_rate: Sampling frequency (Hz).
        detrend: Detrending mode applied before the transform.
        window: If True, apply a Hann window (reduces spectral leakage).

    Returns:
        Tuple of (frequencies, amplitude_spectrum). Amplitudes are scaled so
        that a pure tone of amplitude ``A`` produces a peak of ~``A``.

    Raises:
        ValueError: If the signal has fewer than two samples.
    """
    sig = np.asarray(signal, dtype=np.float64).ravel()
    if sig.size < 2:
        raise ValueError("Signal must contain at least two samples.")

    sig = _detrend(sig, detrend)

    n = sig.size
    if window:
        win = np.hanning(n)
        # Coherent gain compensation so peak amplitude is preserved.
        coherent_gain = np.sum(win) / n
        sig = sig * win
    else:
        coherent_gain = 1.0

    fft_vals = np.fft.rfft(sig)
    freqs = np.fft.rfftfreq(n, d=1.0 / sample_rate)

    amp = np.abs(fft_vals) / n / coherent_gain
    # One-sided spectrum: double all bins except DC (and Nyquist if present).
    if amp.size > 1:
        amp[1:] *= 2.0
        if n % 2 == 0:
            amp[-1] /= 2.0

    return freqs, amp


# ============================================================================
# Welch PSD spectrum
# ============================================================================


def compute_welch_spectrum(
    signal: NDArray[np.float64],
    sample_rate: float,
    *,
    detrend: Literal["constant", "linear", "none"] = "constant",
    nperseg: int | None = None,
) -> Tuple[NDArray[np.float64], NDArray[np.float64]]:
    """Compute a Welch power-spectral-density estimate of a real signal.

    Welch's method averages periodograms over overlapping segments, trading
    frequency resolution for variance reduction - well suited to noisy CFD
    force/elevation series.

    Args:
        signal: Real-valued 1-D time series (uniformly sampled).
        sample_rate: Sampling frequency (Hz).
        detrend: Detrending mode passed to scipy ('none' -> False).
        nperseg: Segment length; defaults to min(256, len(signal)).

    Returns:
        Tuple of (frequencies, psd).
    """
    sig = np.asarray(signal, dtype=np.float64).ravel()
    if sig.size < 2:
        raise ValueError("Signal must contain at least two samples.")

    if nperseg is None:
        nperseg = min(256, sig.size)

    scipy_detrend: str | bool = False if detrend == "none" else detrend
    freqs, psd = welch(
        sig,
        fs=sample_rate,
        window="hann",
        nperseg=nperseg,
        detrend=scipy_detrend,
    )
    return freqs, psd


# ============================================================================
# Peak detection / dominant-frequency extraction
# ============================================================================


def _find_spectral_peaks(
    freqs: NDArray[np.float64],
    spectrum: NDArray[np.float64],
    *,
    n_peaks: int,
    min_frequency: float,
) -> List[SpectralPeak]:
    """Locate local maxima in a spectrum, sorted by descending amplitude."""
    mask = freqs >= min_frequency
    valid_idx = np.flatnonzero(mask)
    if valid_idx.size == 0:
        return []

    sub_spec = spectrum[mask]
    sub_freqs = freqs[mask]

    peak_local, props = find_peaks(sub_spec)
    if peak_local.size == 0:
        # Fall back to the single global maximum within the valid band.
        gmax = int(np.argmax(sub_spec))
        return [SpectralPeak(float(sub_freqs[gmax]), float(sub_spec[gmax]))]

    order = np.argsort(props["peak_heights"] if "peak_heights" in props else sub_spec[peak_local])[::-1]
    ordered = peak_local[order]

    peaks = [
        SpectralPeak(float(sub_freqs[i]), float(sub_spec[i])) for i in ordered[:n_peaks]
    ]
    return peaks


def extract_natural_frequency(
    signal: NDArray[np.float64],
    *,
    times: NDArray[np.float64] | None = None,
    sample_rate: float | None = None,
    method: Literal["fft", "welch"] = "fft",
    n_peaks: int = 3,
    min_frequency: float = 0.0,
    detrend: Literal["constant", "linear", "none"] = "constant",
) -> SpectrumResult:
    """Extract the dominant fluid natural frequency from a time series.

    Either ``times`` (from which the sample rate is inferred) or an explicit
    ``sample_rate`` must be supplied.

    Args:
        signal: Real-valued 1-D time series (free-surface elevation or force).
        times: Optional uniform time vector (s) used to infer ``sample_rate``.
        sample_rate: Explicit sampling frequency (Hz); overrides ``times``.
        method: Spectral method, ``'fft'`` (amplitude) or ``'welch'`` (PSD).
        n_peaks: Number of peaks to return (sorted by descending amplitude).
        min_frequency: Ignore peaks below this frequency (Hz) - excludes the
            DC/near-zero band that otherwise dominates a detrended spectrum.
        detrend: Detrending mode applied before the transform.

    Returns:
        SpectrumResult; ``.dominant_frequency`` is the natural frequency (Hz).

    Raises:
        ValueError: If neither ``times`` nor ``sample_rate`` is provided.
    """
    if sample_rate is None:
        if times is None:
            raise ValueError("Provide either 'times' or 'sample_rate'.")
        sample_rate = _infer_sample_rate(np.asarray(times, dtype=np.float64))

    if method == "fft":
        freqs, spectrum = compute_fft_spectrum(signal, sample_rate, detrend=detrend)
    elif method == "welch":
        freqs, spectrum = compute_welch_spectrum(signal, sample_rate, detrend=detrend)
    else:  # pragma: no cover - guarded by Literal typing
        raise ValueError(f"Unknown method: {method!r}")

    peaks = _find_spectral_peaks(
        freqs, spectrum, n_peaks=n_peaks, min_frequency=min_frequency
    )

    return SpectrumResult(
        frequencies=freqs,
        spectrum=spectrum,
        peaks=peaks,
        method=method,
        sample_rate=float(sample_rate),
    )


# ============================================================================
# Analytical cross-check: prismatic-tank sloshing natural frequency
# ============================================================================


def prismatic_tank_natural_frequency(
    length: float,
    fill_depth: float,
    *,
    mode: int = 1,
    gravity: float = GRAVITY,
) -> float:
    """Linear sloshing natural frequency for a rectangular (prismatic) tank.

    Implements the standard linear-potential dispersion relation::

        omega_n^2 = (n*pi*g / L) * tanh(n*pi*h / L)

    where ``L`` is the tank length (in the sloshing direction), ``h`` the
    still-water fill depth, ``n`` the mode number and ``g`` gravity.

    Args:
        length: Tank length L in the sloshing direction (m).
        fill_depth: Still-water fill depth h (m).
        mode: Sloshing mode number n (>= 1).
        gravity: Gravitational acceleration (m/s^2).

    Returns:
        Natural frequency f_n in Hz ( = omega_n / (2*pi) ).

    Raises:
        ValueError: If length/fill_depth are non-positive or mode < 1.
    """
    if length <= 0:
        raise ValueError("length must be positive.")
    if fill_depth <= 0:
        raise ValueError("fill_depth must be positive.")
    if mode < 1:
        raise ValueError("mode must be >= 1.")

    k = mode * math.pi / length
    omega_sq = gravity * k * math.tanh(k * fill_depth)
    omega = math.sqrt(omega_sq)
    return omega / (2.0 * math.pi)


# ============================================================================
# Wiring: measured-vs-analytical sloshing natural frequency
# ============================================================================


@dataclass
class SloshingFrequencyResult:
    """Measured fluid natural frequency cross-checked against the analytical value.

    Combines the FFT-extracted dominant frequency of a sloshing time series with
    the analytical prismatic-tank tanh dispersion relation, reporting the ratio
    and percent error so a run can be regression-gated in one call.

    Attributes:
        measured_frequency: Dominant natural frequency from the FFT/Welch
            spectrum (Hz).
        analytical_frequency: Linear prismatic-tank natural frequency (Hz).
        ratio: measured / analytical (dimensionless; 1.0 == perfect agreement).
        percent_error: 100 * (measured - analytical) / analytical (signed %).
        mode: Sloshing mode number the analytical value was computed for.
        length: Tank length L used for the analytical value (m).
        fill_depth: Still-water fill depth h used for the analytical value (m).
        spectrum: The full SpectrumResult from the extraction (peaks, bins).
    """

    measured_frequency: float
    analytical_frequency: float
    ratio: float
    percent_error: float
    mode: int
    length: float
    fill_depth: float
    spectrum: SpectrumResult

    @property
    def abs_percent_error(self) -> float:
        """Unsigned percent error, convenient for tolerance assertions."""
        return abs(self.percent_error)


def sloshing_natural_frequency(
    signal: NDArray[np.float64],
    *,
    length: float,
    fill_depth: float,
    times: NDArray[np.float64] | None = None,
    sample_rate: float | None = None,
    mode: int = 1,
    gravity: float = GRAVITY,
    method: Literal["fft", "welch"] = "fft",
    n_peaks: int = 3,
    min_frequency: float = 0.0,
    detrend: Literal["constant", "linear", "none"] = "constant",
) -> SloshingFrequencyResult:
    """Extract the fluid natural frequency and cross-check it analytically.

    Thin wiring over :func:`extract_natural_frequency` (FFT/Welch measurement)
    and :func:`prismatic_tank_natural_frequency` (analytical tanh dispersion).
    Given a free-surface-elevation or force/moment history plus the tank breadth
    ``L`` and fill depth ``h``, it returns the measured natural frequency, the
    analytical one, and their ratio / percent error in a single result.

    Either ``times`` (from which the sample rate is inferred) or an explicit
    ``sample_rate`` must be supplied.

    Args:
        signal: Real-valued 1-D time series (free-surface elevation or force).
        length: Tank length L in the sloshing direction (m).
        fill_depth: Still-water fill depth h (m).
        times: Optional uniform time vector (s) used to infer ``sample_rate``.
        sample_rate: Explicit sampling frequency (Hz); overrides ``times``.
        mode: Sloshing mode number n (>= 1) for the analytical cross-check.
        gravity: Gravitational acceleration (m/s^2).
        method: Spectral method, ``'fft'`` (amplitude) or ``'welch'`` (PSD).
        n_peaks: Number of peaks to keep in the returned spectrum.
        min_frequency: Ignore peaks below this frequency (Hz).
        detrend: Detrending mode applied before the transform.

    Returns:
        SloshingFrequencyResult with measured vs analytical frequency, ratio,
        and percent error.

    Raises:
        ValueError: If neither ``times`` nor ``sample_rate`` is provided, or if
            the analytical inputs are invalid (see
            :func:`prismatic_tank_natural_frequency`).
    """
    spectrum = extract_natural_frequency(
        signal,
        times=times,
        sample_rate=sample_rate,
        method=method,
        n_peaks=n_peaks,
        min_frequency=min_frequency,
        detrend=detrend,
    )
    measured = spectrum.dominant_frequency

    analytical = prismatic_tank_natural_frequency(
        length, fill_depth, mode=mode, gravity=gravity
    )

    if math.isfinite(measured) and analytical != 0.0:
        ratio = measured / analytical
        percent_error = 100.0 * (measured - analytical) / analytical
    else:
        ratio = float("nan")
        percent_error = float("nan")

    return SloshingFrequencyResult(
        measured_frequency=measured,
        analytical_frequency=analytical,
        ratio=ratio,
        percent_error=percent_error,
        mode=mode,
        length=length,
        fill_depth=fill_depth,
        spectrum=spectrum,
    )
