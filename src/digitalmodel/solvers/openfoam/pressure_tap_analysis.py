#!/usr/bin/env python3
"""
ABOUTME: Post-processing for named wall pressure taps (dm#661): reads OpenFOAM
tap output back and computes, per named tap, peak, envelope (min/max), the full
time-history handle, spectral content and a design-equivalent pressure.

Design-equivalent pressure
--------------------------
The *design-equivalent pressure* is defined here as a high-percentile value of
the tap pressure time history (default: the 99th percentile). Sloshing impact
traces are spiky and a single-sample numerical over/under-shoot is not a sound
design basis; a high percentile is a robust near-peak statistic that captures
the sustained impact maxima while discarding isolated one-sample spikes. The
percentile is configurable, and the raw ``peak`` (true maximum) is always
reported alongside it.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Optional, Sequence, Tuple

import numpy as np
from numpy.typing import NDArray

from .post_processing import OpenFOAMPostProcessor
from .results_models import ProbeTimeSeries
from .spectral_analysis import compute_fft_spectrum


@dataclass
class PressureTapStatistics:
    """Summary statistics for one named pressure tap.

    Attributes:
        name: Tap name.
        times: Time vector of the tap trace (s).
        pressure: The full pressure time history (the time-history handle).
        peak: True maximum pressure over the record.
        min: Minimum pressure (lower envelope).
        max: Maximum pressure (upper envelope) - equals ``peak``.
        mean: Time-mean pressure.
        design_equivalent: High-percentile design-equivalent pressure (see the
            module docstring; default is the 99th percentile).
        design_percentile: The percentile used for ``design_equivalent``.
        frequencies: FFT frequency bins (Hz).
        spectrum: One-sided amplitude spectrum aligned with ``frequencies``.
        dominant_frequency: Frequency of the largest spectral peak (Hz).
    """

    name: str
    times: NDArray[np.float64]
    pressure: NDArray[np.float64]
    peak: float
    min: float
    max: float
    mean: float
    design_equivalent: float
    design_percentile: float
    frequencies: NDArray[np.float64]
    spectrum: NDArray[np.float64]
    dominant_frequency: float

    @property
    def envelope(self) -> Tuple[float, float]:
        """The (min, max) pressure envelope."""
        return (self.min, self.max)

    def to_dict(self) -> Dict[str, float]:
        """Scalar summary (drops the arrays) for logging / provenance."""
        return {
            "name": self.name,  # type: ignore[dict-item]
            "peak": self.peak,
            "min": self.min,
            "max": self.max,
            "mean": self.mean,
            "design_equivalent": self.design_equivalent,
            "design_percentile": self.design_percentile,
            "dominant_frequency": self.dominant_frequency,
        }


def compute_tap_statistics(
    probe_series: ProbeTimeSeries,
    tap_names: Optional[Sequence[str]] = None,
    *,
    design_percentile: float = 99.0,
    min_frequency: float = 0.0,
    detrend: str = "constant",
) -> Dict[str, PressureTapStatistics]:
    """Compute per-tap statistics from a parsed pressure ``probes`` series.

    Columns of ``probe_series.values`` are taken in order; ``tap_names`` (if
    given) labels them - it must match the ordered point-tap names used to
    build the case (see :func:`point_tap_names`). Without ``tap_names``, taps
    are labelled ``tap_0``, ``tap_1``, ...

    For each tap the peak, envelope (min/max), mean, the full time-history
    handle, a design-equivalent pressure (``design_percentile``-th percentile),
    and the FFT amplitude spectrum (via
    :func:`spectral_analysis.compute_fft_spectrum`) with its dominant frequency
    are returned.

    Args:
        probe_series: Parsed probe series (from
            :meth:`OpenFOAMPostProcessor.parse_probe_file`).
        tap_names: Optional names aligned with the value columns.
        design_percentile: Percentile for the design-equivalent pressure.
        min_frequency: Ignore spectral peaks below this frequency (Hz) when
            picking the dominant frequency (excludes the DC band).
        detrend: Detrend mode passed to ``compute_fft_spectrum``.

    Returns:
        Ordered mapping ``name -> PressureTapStatistics``.

    Raises:
        ValueError: If ``tap_names`` length does not match the column count, or
            if the percentile is outside ``[0, 100]``.
    """
    if not 0.0 <= design_percentile <= 100.0:
        raise ValueError("design_percentile must be in [0, 100].")

    values = np.asarray(probe_series.values, dtype=np.float64)
    if values.ndim == 1:
        values = values.reshape(-1, 1)
    n_cols = values.shape[1]

    if tap_names is not None:
        tap_names = list(tap_names)
        if len(tap_names) != n_cols:
            raise ValueError(
                f"tap_names has {len(tap_names)} entries but the series has "
                f"{n_cols} columns."
            )
    else:
        tap_names = [f"tap_{i}" for i in range(n_cols)]

    times = np.asarray(probe_series.times, dtype=np.float64)
    sample_rate = _infer_sample_rate(times)

    results: Dict[str, PressureTapStatistics] = {}
    for col, name in enumerate(tap_names):
        signal = values[:, col]
        freqs, spectrum = compute_fft_spectrum(
            signal, sample_rate, detrend=detrend  # type: ignore[arg-type]
        )
        band = freqs >= min_frequency
        if np.any(band):
            dom = float(freqs[band][int(np.argmax(spectrum[band]))])
        else:
            dom = float("nan")

        results[name] = PressureTapStatistics(
            name=name,
            times=times,
            pressure=signal,
            peak=float(np.max(signal)),
            min=float(np.min(signal)),
            max=float(np.max(signal)),
            mean=float(np.mean(signal)),
            design_equivalent=float(np.percentile(signal, design_percentile)),
            design_percentile=design_percentile,
            frequencies=freqs,
            spectrum=spectrum,
            dominant_frequency=dom,
        )
    return results


def read_tap_statistics(
    probe_file: Path | str,
    tap_names: Optional[Sequence[str]] = None,
    *,
    field_name: str = "p",
    design_percentile: float = 99.0,
    min_frequency: float = 0.0,
) -> Dict[str, PressureTapStatistics]:
    """Read one probe output file and compute per-tap statistics.

    Convenience wrapper that parses ``probe_file`` with
    :meth:`OpenFOAMPostProcessor.parse_probe_file` and forwards to
    :func:`compute_tap_statistics`.
    """
    probe_file = Path(probe_file)
    pp = OpenFOAMPostProcessor(case_dir=probe_file.parent)
    series = pp.parse_probe_file(probe_file, field_name=field_name)
    return compute_tap_statistics(
        series,
        tap_names,
        design_percentile=design_percentile,
        min_frequency=min_frequency,
    )


def _infer_sample_rate(times: NDArray[np.float64]) -> float:
    """Sampling frequency (Hz) from a (near-)uniform time vector."""
    times = np.asarray(times, dtype=np.float64)
    if times.size < 2:
        raise ValueError("At least two time samples are required.")
    dt = np.diff(times)
    if np.any(dt <= 0):
        raise ValueError("Time vector must be strictly increasing.")
    return float(1.0 / np.mean(dt))
