"""
Mock signal generators for the unit-tier seed equivalence benchmark.

These functions produce synthetic signals and RAOs that mimic a real
offshore dynamics solver without requiring OrcFxAPI.  They are used
exclusively when ``seed_equivalence.py --mock`` is invoked.
"""

from __future__ import annotations

from typing import List, Optional

import numpy as np

from digitalmodel.benchmarks.example_model_benchmark import (
    FrequencyDomainMetrics,
    ModelInventoryEntry,
    SeedEquivalenceResult,
    TimeDomainStats,
    check_seed_equivalence,
    compare_frequency_domain,
    compute_time_domain_stats,
)

# ---------------------------------------------------------------------------
# Constants (match main script)
# ---------------------------------------------------------------------------
MOCK_DURATION_S = 600.0
MOCK_FS_HZ = 1.0
MOCK_FREQ_POINTS = 64


def mock_time_series(
    seed: int,
    hs: float = 3.0,
    tp: float = 10.0,
    duration: float = MOCK_DURATION_S,
    fs: float = MOCK_FS_HZ,
) -> np.ndarray:
    """Generate a synthetic irregular wave response for unit-tier testing.

    Uses a sum-of-sinusoids with JONSWAP-inspired amplitude distribution and
    random phase seeded by *seed*.  Produces a stationary ergodic process
    whose statistics converge reproducibly across different seeds.

    Parameters
    ----------
    seed:
        Random seed for phase realisation.
    hs, tp:
        Significant wave height [m] and peak period [s].
    duration, fs:
        Time series duration [s] and sampling frequency [Hz].

    Returns
    -------
    numpy.ndarray — 1-D time series.
    """
    rng = np.random.default_rng(seed)
    n_components = 128
    t = np.arange(0, duration, 1.0 / fs)
    fp = 1.0 / tp
    freqs = np.linspace(0.5 * fp, 3.0 * fp, n_components)

    df = freqs[1] - freqs[0]
    gamma = 3.3
    sigma = np.where(freqs <= fp, 0.07, 0.09)
    r = np.exp(-0.5 * ((freqs / fp - 1.0) / sigma) ** 2)
    alpha = 0.0081 * hs ** 2
    with np.errstate(divide="ignore", invalid="ignore"):
        psd = (alpha * 9.81 ** 2
               / ((2 * np.pi) ** 4 * freqs ** 5)
               * np.exp(-1.25 * (fp / freqs) ** 4)
               * gamma ** r)
    psd = np.where(np.isfinite(psd), psd, 0.0)
    amplitudes = np.sqrt(2.0 * psd * df)
    phases = rng.uniform(0, 2 * np.pi, size=n_components)

    signal = np.zeros_like(t)
    for a, f, phi in zip(amplitudes, freqs, phases):
        signal += a * np.sin(2 * np.pi * f * t + phi)

    return signal


def mock_rao(
    freqs: np.ndarray,
    peak_freq: float = 0.10,
    peak_amp: float = 1.5,
) -> np.ndarray:
    """Generate a smooth mock RAO curve for unit-tier frequency-domain tests.

    Produces a Gaussian-shaped RAO centred at *peak_freq*.

    Parameters
    ----------
    freqs:     Frequency array [Hz].
    peak_freq: Frequency of RAO peak [Hz].
    peak_amp:  Amplitude at peak.

    Returns
    -------
    numpy.ndarray — RAO amplitudes.
    """
    sigma = 0.05
    return peak_amp * np.exp(-0.5 * ((freqs - peak_freq) / sigma) ** 2)


def run_mock_time_domain(
    entry: ModelInventoryEntry,
    seeds: List[int],
) -> Optional[SeedEquivalenceResult]:
    """Run time-domain benchmark using mock synthetic signals.

    Parameters
    ----------
    entry:  Inventory entry to benchmark.
    seeds:  List of integer seeds to use.

    Returns
    -------
    SeedEquivalenceResult or None (if the model has no time domain).
    """
    if not entry.has_time_domain:
        return None

    stats_list: List[TimeDomainStats] = []
    for seed in seeds:
        signal = mock_time_series(seed=seed)
        stats = compute_time_domain_stats(signal, seed=seed)
        stats_list.append(stats)

    return check_seed_equivalence(entry.name, stats_list)


def run_mock_frequency_domain(
    entry: ModelInventoryEntry,
) -> Optional[FrequencyDomainMetrics]:
    """Run frequency-domain benchmark using a mock reference vs computed RAO.

    The reference RAO is a smooth analytical curve; the computed RAO
    introduces a small systematic offset to simulate a realistic comparison.

    Parameters
    ----------
    entry: Inventory entry to benchmark.

    Returns
    -------
    FrequencyDomainMetrics or None (if the model has no frequency domain).
    """
    if not entry.has_frequency_domain:
        return None

    freqs = np.linspace(0.04, 0.50, MOCK_FREQ_POINTS)
    rao_ref = mock_rao(freqs)
    rng = np.random.default_rng(hash(entry.name) & 0xFFFF_FFFF)
    rao_cmp = rao_ref * (1.0 + rng.uniform(-0.01, 0.01, size=len(freqs)))

    return compare_frequency_domain(entry.name, freqs, rao_ref, rao_cmp)
