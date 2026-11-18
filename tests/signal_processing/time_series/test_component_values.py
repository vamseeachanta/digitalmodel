# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand values for the two legacy component APIs; no production changes.

Source: NumPy reference manual 1.26, Discrete Fourier Transform,
Implementation details and Normalization; SciPy user guide 1.14.1 (2024),
Spectral Analysis, for Parseval. See the linked sources in the worked example.
The synthetic inputs are chosen here, rather than measured acceptance limits.
The absolute tolerance 1e-9 is the requested roundoff allowance; relative
tolerance is disabled so that empty bins really are checked against zero.
"""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.signal_processing.time_series.time_series_components import (
    TimeSeriesComponents,
)
from digitalmodel.signal_processing.time_series.time_series_components_bop import (
    TimeSeriesComponents as VariantComponents,
)


@pytest.fixture(params=["components", "variant"])
def legacy(request):
    """Use each API's own configuration layout and return convention."""
    fft_cfg = {
        "time_step": 1 / 256,
        "filter": {"band_pass": {"flag": False}},
        "window": {"size": 256, "moving_average": {"flag": False}},
        "peaks": {
            "flag": True,
            "solver": "find_peaks",
            "min_height_percentage": 50,
            "min_distance_index_percentage": 1,
        },
    }
    cfg = {"fft": fft_cfg}
    if request.param == "variant":
        obj = VariantComponents({"default": {"analysis": {"fft": fft_cfg}}})
    else:
        obj = TimeSeriesComponents(cfg)
    return request.param, obj, cfg


def test_raw_amplitude_resolution_parseval_and_peaks(legacy):
    """For x[n]=4 sin(2 pi 16 n/256), N=256 and T=1 s.

    The two geometric sums leave X[16]=-512j and X[-16]=512j;
    every other bin is zero. Thus 2|X[16]|/N=4, |X[16]|²=262144,
    df=1/T=1 Hz, and mean(x²)=4²/2=8. Parseval gives
    sum(|X|²)/N²=524288/65536=8. The first API retains only
    nonnegative bins, so its non-DC sum needs doubling; the variant
    returns both sides. Neither API promises normalized raw power.
    """
    kind, obj, cfg = legacy
    signal = 4 * np.sin(2 * np.pi * 16 * np.arange(256) / 256)
    if kind == "variant":
        spectrum, reconstructed = obj.fft_analysis(signal)
        assert reconstructed == pytest.approx(signal, rel=0, abs=1e-9)
        mean_square = spectrum["power"].sum() / 65536
    else:
        result, _ = obj.fft_analysis(cfg, signal, 1 / 256)
        spectrum = result["fft"]
        mean_square = 2 * spectrum["power"].sum() / 65536
        assert spectrum.loc[spectrum.fft_freq == 16, "fft"].iloc[0] == pytest.approx(
            -512j, rel=0, abs=1e-9
        )
    positive = spectrum[spectrum.fft_freq >= 0]
    amplitude = 2 * np.sqrt(positive["power"].to_numpy()) / 256
    expected = np.zeros(len(positive))
    expected[16] = 4
    assert amplitude == pytest.approx(expected, rel=0, abs=1e-9)
    assert positive["fft_freq"].diff().dropna().to_numpy() == pytest.approx(
        np.ones(len(positive) - 1), rel=0, abs=1e-9
    )
    assert np.mean(signal**2) == pytest.approx(8, rel=0, abs=1e-9)
    assert mean_square == pytest.approx(8, rel=0, abs=1e-9)
    indices = obj.identify_signal_peaks(
        positive["fft_freq"].tolist(), positive["power"].tolist(), cfg["fft"]["peaks"]
    )
    assert list(indices) == [16]
    assert positive.iloc[indices[0]]["power"] == pytest.approx(262144, rel=0, abs=1e-9)


def test_legacy_apis_agree_with_hand_power():
    """Both raw APIs must give 262144 at +16 Hz, independently of each other.

    Same N=256, A=4 derivation as above: (N A/2)²=512²=262144.
    """
    signal = 4 * np.sin(2 * np.pi * 16 * np.arange(256) / 256)
    settings = {"time_step": 1 / 256, "filter": {}}
    cfg = {"fft": settings}
    first, _ = TimeSeriesComponents(cfg).fft_analysis(cfg, signal)
    second, _ = VariantComponents(
        {"default": {"analysis": {"fft": settings}}}
    ).fft_analysis(signal)
    for spectrum in (first["fft"], second):
        assert spectrum.loc[spectrum.fft_freq == 16, "power"].iloc[0] == pytest.approx(
            262144, rel=0, abs=1e-9
        )


def test_window_peak_frequency_and_flags(legacy):
    """Two identical one-second windows have the same sole positive peak, 16 Hz.

    Averaging cannot move a bin-centred tone; no amplitude claim is made here.
    """
    kind, obj, cfg = legacy
    signal = 4 * np.sin(2 * np.pi * 16 * np.arange(512) / 256)
    if kind == "variant":
        # The existing peak-flag assignment emits this warning on pandas 2.2.
        # Require the warning explicitly while checking its current numeric result.
        with pytest.warns((FutureWarning, pd.errors.SettingWithCopyWarning)) as caught:
            spectrum = obj.fft_window_analysis(signal)
        assert any("ChainedAssignmentError" in str(w.message) for w in caught)
    else:
        spectrum = obj.fft_window_analysis(cfg, signal, 1 / 256)
    peaks = spectrum[spectrum.peak_flag & (spectrum.fft_freq > 0)]
    assert peaks["fft_freq"].tolist() == [16]


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="Legacy window averages divide two windows by three; see worked example",
)
def test_window_average_preserves_amplitude_and_parseval(legacy):
    """An arithmetic mean of two identical spectra equals either spectrum.

    Each 256-sample window has peak power 262144 and amplitude 4;
    (262144+262144)/2=262144 and mean-square=8. The implementation
    divides by 3, which is not an average of the two windows.
    The assertion retains the hand value rather than adopting that divisor.
    """
    kind, obj, cfg = legacy
    # This case isolates averaging from the variant's chained peak assignment.
    cfg["fft"]["peaks"]["flag"] = False
    signal = 4 * np.sin(2 * np.pi * 16 * np.arange(512) / 256)
    if kind == "variant":
        spectrum = obj.fft_window_analysis(signal)
    else:
        spectrum = obj.fft_window_analysis(cfg, signal, 1 / 256)
    peak = spectrum.loc[spectrum.fft_freq == 16, "power"].iloc[0]
    assert peak == pytest.approx(262144, rel=0, abs=1e-9)
    assert 2 * np.sqrt(peak) / 256 == pytest.approx(4, rel=0, abs=1e-9)
    positive_power = spectrum.loc[spectrum.fft_freq > 0, "power"].sum()
    assert 2 * positive_power / 65536 == pytest.approx(8, rel=0, abs=1e-9)
