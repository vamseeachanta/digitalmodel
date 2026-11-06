# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exact, noise-free values supplement the existing spectral behaviour tests.

References: NumPy reference 1.26, DFT definition and Normalization;
SciPy user guide 1.14.1 (2024), Spectral Analysis and IIR Filter;
The Scientist and Engineer's Guide to Digital Signal Processing, hardcover
edition 1997, chapter 20, page 335 (cutoff convention).
URLs and the derivations are in the accompanying worked example.
Synthetic inputs and tolerances are explicitly chosen test controls.
"""

import numpy as np
import pytest
import runpy
from pathlib import Path

from digitalmodel.signal_processing.signal_analysis.core.spectral import (
    SpectralAnalyzer,
)
from digitalmodel.signal_processing.signal_analysis.filters.frequency import (
    FrequencyFilter,
)


@pytest.mark.parametrize("windowed", [False, True])
def test_all_amplitude_bins_and_parseval(windowed):
    """N=256, fs=256, A=4, f=16: |X[16]|=512, other positive bins zero.

    With a rectangular window, 2|X|/N=4 and df=1 Hz. This API's
    FFT power uses 2|X|²/N, hence peak=2048 and sum(power)/N=8.
    Two identical windows must preserve every bin. The 1e-9 absolute
    tolerance is the requested roundoff allowance, with no relative slack.
    """
    signal = 4 * np.sin(2 * np.pi * 16 * np.arange(256) / 256)
    analyzer = SpectralAnalyzer(sampling_rate=256)
    if windowed:
        spectrum = analyzer.window_averaged_fft(
            np.tile(signal, 2), window_size=256, overlap=0, window_function="boxcar"
        )
    else:
        spectrum = analyzer.compute_spectrum(signal, detrend="none")
    expected = np.zeros(128)
    expected[16] = 4
    assert spectrum.magnitude.to_numpy() == pytest.approx(expected, rel=0, abs=1e-9)
    assert spectrum.frequency.to_numpy() == pytest.approx(
        np.arange(128), rel=0, abs=1e-9
    )
    assert spectrum.power.sum() / 256 == pytest.approx(8, rel=0, abs=1e-9)
    assert np.mean(signal**2) == pytest.approx(8, rel=0, abs=1e-9)
    peaks = analyzer.find_peaks(n_peaks=1)
    assert peaks.frequency.tolist() == [16]
    assert peaks.power.iloc[0] == pytest.approx(2048, rel=0, abs=1e-9)


@pytest.mark.parametrize("method", ["periodogram", "welch"])
def test_psd_area_and_exact_bin_distribution(method):
    """A=4, f=16, fs=N=256: variance and one-sided PSD area both equal 8.

    SciPy 1.14.1 Spectral Analysis gives density=2|Xw|²/(fs sum(w²)).
    Boxcar: |X[16]|=512, sum(w²)=256, so bin 16 has density 8.
    Periodic Hann: w[n]=(1-cos(2 pi n/N))/2; its three Fourier
    coefficients are 1/2,-1/4,-1/4. Thus |Xw| at bins 15,16,17
    is 128,256,128, and sum(w²)=3N/8=96. Densities are
    4/3,16/3,4/3, whose sum times df=1 gives 8.
    These fractions are hand values, not a call to a reference PSD routine.
    Absolute tolerance 1e-9 allows floating-point trigonometry and FFT roundoff.
    """
    signal = 4 * np.sin(2 * np.pi * 16 * np.arange(1024) / 256)
    analyzer = SpectralAnalyzer(sampling_rate=256)
    if method == "welch":
        psd = analyzer.compute_psd(signal, method=method, nperseg=256, noverlap=0)
        expected = np.zeros(129)
        expected[15:18] = [4 / 3, 16 / 3, 4 / 3]
        df = 1
    else:
        # A one-second record keeps df identical to the hand calculation.
        psd = analyzer.compute_psd(signal[:256], method=method, window="boxcar")
        expected = np.zeros(129)
        expected[16] = 8
        df = 1
    assert psd.psd.to_numpy() == pytest.approx(expected, rel=0, abs=1e-9)
    assert psd.psd.sum() * df == pytest.approx(8, rel=0, abs=1e-9)
    assert psd.frequency.iloc[1] - psd.frequency.iloc[0] == pytest.approx(
        df, rel=0, abs=1e-9
    )
    assert analyzer.method == "fft"


def test_butterworth_lowpass_gain_and_cutoff():
    """Order 2, fs=256, fc=32: |H|²=1/(1+r^4), r=tan(pi f/fs)/tan(pi fc/fs).

    The IIR bilinear substitution is in SciPy user guide 1.14.1.
    At cutoff r=1, so gain=1/sqrt(2). At DC gain=1 exactly.
    At 1 Hz, r<0.030, so 1-|H|<r^4/2<4.1e-7; use 1e-6.
    Cutoff/DC tolerance 1e-12 permits coefficient evaluation roundoff.
    Chapter 20 page 335 of the 1997 hardcover DSP text identifies
    the cutoff at amplitude 0.707; the exact value is also stated in
    SciPy reference 1.14.1, butter, Parameters, Wn.
    """
    filt = FrequencyFilter(order=2)
    filt.design_filter(32, sampling_rate=256)
    frequencies, response = filt.get_frequency_response(
        worN=np.array([0, 1, 32]), fs=256
    )
    assert frequencies == pytest.approx([0, 1, 32], rel=0, abs=1e-12)
    assert abs(response[0]) == pytest.approx(1, rel=0, abs=1e-12)
    assert abs(response[1]) == pytest.approx(1, rel=0, abs=1e-6)
    assert abs(response[2]) == pytest.approx(1 / np.sqrt(2), rel=0, abs=1e-12)


def test_bandstop_attenuates_in_band_tone():
    """Order 2 bandstop at 16..64 Hz, fs=256, tone 32 Hz of amplitude 4.

    SciPy 1.14.1 IIR Filter gives the lowpass-to-bandstop substitution.
    With u=tan(pi f/fs), edges are 0.1989 and 1, tone u=0.4142.
    The equivalent lowpass ratio |(1-0.1989)u/(0.1989-u²)| exceeds
    12. Hence single-pass Butterworth gain <1/144; forward-backward
    filtering squares that gain (filtfilt Notes, same edition), below
    1/20736. A conservative amplitude bound of 4/1000 includes rounding
    and residual end transients. Discard 1024 samples at each end,
    four seconds at the chosen rate, to isolate steady-state response.
    """
    signal = 4 * np.sin(2 * np.pi * 32 * np.arange(4096) / 256)
    filtered = FrequencyFilter(order=2).bandstop_filter(signal, 16, 64, 256)
    assert np.max(np.abs(filtered[1024:-1024])) < 4 / 1000


@pytest.mark.parametrize("cutoff", [128, 129, (16, 129)])
def test_cutoff_at_or_above_nyquist_is_refused(cutoff):
    """At fs=256, Nyquist=128 Hz; digital Wn must be strictly below it.

    Source: SciPy reference 1.14.1, butter Parameters, Wn (0<Wn<1).
    """
    with pytest.raises(ValueError, match="critical frequencies"):
        FrequencyFilter().design_filter(
            cutoff,
            sampling_rate=256,
            btype="bandstop" if isinstance(cutoff, tuple) else "lowpass",
        )


def test_documented_example_executes_with_hand_answers():
    """Execute the shipped example: f=16, A=4, df=1, variance=PSD area=8.

    The complete geometric-sum derivation is in the example docstring.
    Absolute tolerance 1e-9 is the requested Fourier roundoff allowance.
    """
    root = Path(__file__).resolve().parents[3]
    namespace = runpy.run_path(
        str(root / "docs/domains/examples/signal_value_example.py")
    )
    assert namespace["run_example"]() == pytest.approx(
        {
            "frequency_hz": 16,
            "amplitude": 4,
            "resolution_hz": 1,
            "mean_square": 8,
            "psd_area": 8,
        },
        rel=0,
        abs=1e-9,
    )
