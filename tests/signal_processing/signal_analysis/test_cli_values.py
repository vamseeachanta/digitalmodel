# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise real CLI callbacks, CSV parsing and numeric output contracts.

DFT/PSD references: NumPy 1.26 DFT and SciPy 1.14.1 Spectral Analysis
(2024), cited in the worked example. All inputs are synthetic test choices.
An absolute 1e-9 allowance covers FFT, detrending and CSV roundoff.
Known interface mismatches are strict expected failures, not golden outputs.
Rainflow count values are NOT_EVALUATED: the CLI uses the defective in-house
path recorded in the repository rainflow exposure assessment and linked issue.
"""

import json

import numpy as np
import pandas as pd
import pytest
from click.testing import CliRunner

from digitalmodel.signal_processing.signal_analysis.cli import (
    fft_cmd,
    filter_cmd,
    psd_cmd,
    rainflow_cmd,
)


def _arguments(command, source, output):
    args = [str(source), "--column", "signal"]
    if command is filter_cmd:
        args.insert(1, str(output))
        args += ["--sampling-rate", "256", "--cutoff", "32"]
    else:
        args += ["--output", str(output)]
        if command is not rainflow_cmd:
            args += ["--sampling-rate", "256"]
    return args


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="FFT CLI iterates DataFrame labels and expects plural spectrum columns",
)
def test_fft_json_peak_has_hand_amplitude_and_frequency(tmp_path):
    """A symmetric phase-shifted sine has amplitude 4 at 16 Hz, N=fs=256.

    x[n]=4 cos(2 pi 16 (n-127.5)/256) is symmetric about the record
    midpoint. Its mean and linear least-squares slope are both zero,
    so the CLI's default linear detrending leaves the tone unchanged.
    DFT orthogonality gives one-sided amplitude 4 and frequency 16.
    """
    source, output = tmp_path / "tone.csv", tmp_path / "fft.json"
    signal = 4 * np.cos(2 * np.pi * 16 * (np.arange(256) - 127.5) / 256)
    pd.DataFrame({"signal": signal}).to_csv(source, index=False)
    result = CliRunner().invoke(
        fft_cmd,
        _arguments(fft_cmd, source, output) + ["--method", "fft", "--n-peaks", "1"],
    )
    assert result.exit_code == 0, result.output
    data = json.loads(output.read_text())
    assert data["peaks"][0]["frequency"] == pytest.approx(16, rel=0, abs=1e-9)
    assert data["peaks"][0]["amplitude"] == pytest.approx(4, rel=0, abs=1e-9)
    assert max(data["spectrum"]["amplitudes"]) == pytest.approx(4, rel=0, abs=1e-9)


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="PSD CLI reads frequencies but the analyzer returns frequency",
)
def test_psd_json_has_hand_area_and_peak(tmp_path):
    """Four seconds of A=4, f=16 Hz sine: PSD area=A²/2=8.

    Default 256-point periodic Hann at fs=256 has df=1 Hz and bin
    powers 4/3,16/3,4/3 at 15,16,17 Hz. Sum=8; maximum=16/3.
    Derivation: test_spectral_filter_values.test_psd_area_and_exact_bin_distribution.
    """
    source, output = tmp_path / "tone.csv", tmp_path / "psd.json"
    signal = 4 * np.sin(2 * np.pi * 16 * np.arange(1024) / 256)
    pd.DataFrame({"signal": signal}).to_csv(source, index=False)
    result = CliRunner().invoke(psd_cmd, _arguments(psd_cmd, source, output))
    assert result.exit_code == 0, result.output
    data = json.loads(output.read_text())
    assert data["total_power"] == pytest.approx(8, rel=0, abs=1e-9)
    assert data["peak_frequency"] == pytest.approx(16, rel=0, abs=1e-9)
    assert data["peak_psd"] == pytest.approx(16 / 3, rel=0, abs=1e-9)


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="Filter CLI calls TimeSeriesProcessor.filter_signal, which does not exist",
)
def test_filter_csv_preserves_constant_input(tmp_path):
    """A unit-DC-gain lowpass leaves a constant 4 unchanged at every sample.

    This is the exact zero-frequency limiting case of the Butterworth
    response in SciPy 1.14.1 butter and the cited 1997 DSP text.
    Forward-backward application also has DC gain 1²=1.
    """
    source, output = tmp_path / "constant.csv", tmp_path / "filtered.csv"
    pd.DataFrame({"signal": np.full(64, 4)}).to_csv(source, index=False)
    result = CliRunner().invoke(filter_cmd, _arguments(filter_cmd, source, output))
    assert result.exit_code == 0, result.output
    filtered = pd.read_csv(output)["filtered"].to_numpy()
    assert filtered == pytest.approx(np.full(64, 4), rel=0, abs=1e-9)


@pytest.mark.parametrize("command", [rainflow_cmd, fft_cmd, psd_cmd, filter_cmd])
def test_missing_input_has_usage_error(command, tmp_path):
    """Click's exists=True input contract rejects an absent CSV with status 2."""
    result = CliRunner().invoke(
        command, _arguments(command, tmp_path / "missing.csv", tmp_path / "output.json")
    )
    assert result.exit_code == 2
    assert "does not exist" in result.output


@pytest.mark.parametrize("command", [rainflow_cmd, fft_cmd, psd_cmd, filter_cmd])
def test_nonnumeric_column_has_analysis_error(command, tmp_path):
    """Text input is refused with the CLI's documented Error prefix and status 1.

    No cycle count from the known defective rainflow path is evaluated.
    A longer column avoids an unrelated minimum-record-length condition.
    """
    source, output = tmp_path / "text.csv", tmp_path / "output.json"
    pd.DataFrame({"signal": ["high", "low"] * 32}).to_csv(source, index=False)
    result = CliRunner().invoke(command, _arguments(command, source, output))
    assert result.exit_code == 1
    assert "Error:" in result.output
    assert not output.exists()
