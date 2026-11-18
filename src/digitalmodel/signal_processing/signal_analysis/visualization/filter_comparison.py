# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compare FFT-masked records in time and one-sided amplitude coordinates."""

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from ..filters.frequency import FrequencyFilter


def _amplitude(samples: np.ndarray) -> np.ndarray:
    """Scale a real DFT to peak amplitudes in the input signal's units."""
    amplitude = np.abs(np.fft.rfft(samples)) / samples.size
    # Odd records have no Nyquist bin: their final positive bin is interior.
    amplitude[1 : None if samples.size % 2 else -1] *= 2
    if not np.all(np.isfinite(amplitude)):
        raise ValueError("signal must produce finite amplitude coordinates")
    return amplitude


def _coordinates(size: int, sampling_rate: float) -> tuple[np.ndarray, np.ndarray]:
    """Build seconds and Hz coordinates after the filter validates the rate."""
    rate = float(sampling_rate)
    with np.errstate(over="ignore"):
        time = np.arange(size) / rate
    if not np.all(np.isfinite(time)):
        raise ValueError("sampling_rate must produce finite time coordinates")
    frequencies = np.arange(size // 2 + 1) * (rate / size)
    if size % 2 == 0:
        frequencies[-1] = rate / 2
    return time, frequencies


def plot_filter_comparison(
    signal: list | np.ndarray,
    sampling_rate: float,
    low_cutoff: float | None = None,
    high_cutoff: float | None = None,
    *,
    units: str = "input units",
) -> go.Figure:
    """Return original and FFT-filtered time traces and amplitude spectra.

    Args:
        signal: Finite, nonempty, real one-dimensional samples, left unchanged.
        sampling_rate: Uniform sampling rate in Hz, positive and finite.
        low_cutoff: Inclusive lower bound in Hz; omitted means zero.
        high_cutoff: Inclusive upper bound in Hz; omitted means Nyquist.
        units: Display label for both signals and amplitudes; no conversion.

    Returns:
        A Plotly figure with time in seconds and frequency in Hz. Spectrum
        values are one-sided peak amplitudes, not power spectral density.
        DC and even-length Nyquist bins are not doubled. Nothing is displayed
        or saved; call write_html with include_plotlyjs=True for offline use.

    Raises:
        ValueError: If FrequencyFilter.fft_filter rejects the inputs, or time
            or amplitude coordinates are nonfinite at floating-point limits.

    Notes:
        Only an upper bound gives low pass, only a lower bound gives high
        pass, and both give band pass. Neither gives identity up to roundoff.
        The record is treated as periodic, without padding, windowing or
        detrending. Nonperiodic endpoints can cause spectral leakage; sharp
        masks can cause ringing and circular edge effects.
    """
    filtered = FrequencyFilter().fft_filter(
        signal, sampling_rate, low_cutoff, high_cutoff
    )
    samples = np.asarray(signal)
    time, frequencies = _coordinates(samples.size, sampling_rate)

    figure = make_subplots(
        rows=2,
        cols=1,
        subplot_titles=(
            "Time trace comparison",
            "One-sided amplitude spectrum comparison",
        ),
    )
    for row, coordinates in ((1, time), (2, frequencies)):
        for name, values, color in (
            ("Original", samples, "#1f77b4"),
            ("Filtered", filtered, "#ff7f0e"),
        ):
            figure.add_trace(
                go.Scatter(
                    x=coordinates,
                    y=values if row == 1 else _amplitude(values),
                    name=name,
                    mode="lines",
                    legendgroup=name,
                    showlegend=row == 1,
                    line={"color": color},
                ),
                row=row,
                col=1,
            )
    figure.update_xaxes(title_text="Time (s)", row=1, col=1)
    figure.update_xaxes(title_text="Frequency (Hz)", row=2, col=1)
    figure.update_yaxes(title_text=f"Signal ({units})", row=1, col=1)
    figure.update_yaxes(title_text=f"Amplitude ({units})", row=2, col=1)
    return figure
