# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline moment-file examples with independently calculated FFT expectations.

Hand calculation and synthetic choices (SI units throughout):
- Sample every 0.05 s for 32 s, excluding the endpoint: N = 32 / 0.05 = 640,
  sample rate = 1 / 0.05 = 20 Hz, FFT bin width = 1 / 32 = 0.03125 Hz.
  The FFT duration is N * dt, rather than the last timestamp minus the first.
- Period 1.6 s gives f = 1 / 1.6 = 0.625 Hz, exactly bin 20. Period 1.5 s
  gives f = 2/3 Hz, between bins 21 and 22; the nearest bin is 21/32 Hz.
  Half a bin (0.015625 Hz) bounds nearest-bin quantization error and is the
  acceptance tolerance for both raw and refined frequency in these examples.
- Choose M(t) = 8 + 4 sin(2*pi*t/T) + sin(4*pi*t/T) N m: the constant
  offset is twice the dominant amplitude; the second harmonic is one quarter
  of it. Pressure carries three quarters of M and viscosity the remainder.
  The x/y components are zero for this synthetic moment about z.
- Sign factors +/-1 and added offset 64 N m (sixteen dominant amplitudes)
  exercise sign reversal, an offset change, and their combination.

Column layouts follow ``parse_roll_moment`` in ``sloshing_2d_analysis``:
legacy pressure/viscous vectors; modern total/pressure/viscous vectors.
No solver or external output is needed.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.validation.sloshing_2d_analysis import (
    measure_natural_frequency,
    parse_roll_moment,
)
from digitalmodel.solvers.openfoam.validation.sloshing_2d_dicts import (
    ROLL_MOMENT_FO_NAME,
)


def _synthetic_moment(period: float) -> tuple[np.ndarray, np.ndarray]:
    """Construct the hand-calculated record described in the module docstring."""
    times = np.arange(640) * 0.05
    phase = 2 * np.pi * times / period
    moment = 8 + 4 * np.sin(phase) + np.sin(2 * phase)
    return times, moment


def _write_moment_file(
    case_dir: Path, layout: str, times: np.ndarray, moment: np.ndarray
) -> None:
    """Write the documented layout with nonzero pressure and viscous moments."""
    output_dir = case_dir / "postProcessing" / ROLL_MOMENT_FO_NAME / "0"
    output_dir.mkdir(parents=True)
    columns = "(pressure) (viscous)"
    if layout == "modern":
        columns = "(total) " + columns
    rows = ["# Moment", f"# Time {columns}", ""]
    for time, total in zip(times, moment):
        pressure = 0.75 * total
        viscous = total - pressure
        vectors = f"(0 0 {float(pressure)!r}) (0 0 {float(viscous)!r})"
        if layout == "modern":
            vectors = f"(0 0 {float(total)!r}) " + vectors
        rows.append(f"{float(time)!r} {vectors}")
    (output_dir / "moment.dat").write_text("\n".join(rows) + "\n", encoding="utf-8")


@pytest.mark.parametrize(
    "period,expected_hz,expected_bin_hz",
    [(1.6, 0.625, 20 / 32), (1.5, 2 / 3, 21 / 32)],
    ids=["on-bin", "between-bins"],
)
def test_synthetic_moment_frequency_legacy_and_modern(
    tmp_path: Path, period: float, expected_hz: float, expected_bin_hz: float
) -> None:
    times, moment = _synthetic_moment(period)
    parsed_signals = []
    for layout in ("legacy", "modern"):
        case_dir = tmp_path / layout
        _write_moment_file(case_dir, layout, times, moment)
        parsed_times, parsed_moment = parse_roll_moment(case_dir)
        assert len(parsed_times) == len(parsed_moment) == 640
        assert parsed_times == pytest.approx(times)
        assert parsed_moment == pytest.approx(moment)
        parsed_signals.append(parsed_moment)

        result = measure_natural_frequency(parsed_times, parsed_moment)
        assert result["n_samples"] == 640
        assert result["sample_rate"] == pytest.approx(20)
        assert result["freq_resolution"] == pytest.approx(0.03125)
        assert result["raw_frequency"] == pytest.approx(expected_bin_hz)
        for key in ("raw_frequency", "refined_frequency"):
            assert result[key] == pytest.approx(expected_hz, abs=0.015625)
    assert parsed_signals[0] == pytest.approx(parsed_signals[1])


@pytest.mark.parametrize(
    "sign,offset", [(-1, 0), (1, 64), (-1, 64)],
    ids=["sign", "offset", "sign-and-offset"],
)
def test_moment_frequency_sign_and_offset_invariance(
    tmp_path: Path, sign: int, offset: int
) -> None:
    times, moment = _synthetic_moment(period=1.5)
    transformed = sign * moment + offset
    parsed_signals = []
    for layout in ("legacy", "modern"):
        results = []
        for name, signal in (("reference", moment), ("transformed", transformed)):
            case_dir = tmp_path / layout / name
            _write_moment_file(case_dir, layout, times, signal)
            parsed_times, parsed_moment = parse_roll_moment(case_dir)
            assert len(parsed_times) == len(parsed_moment) == 640
            assert parsed_times == pytest.approx(times)
            assert parsed_moment == pytest.approx(signal)
            result = measure_natural_frequency(parsed_times, parsed_moment)
            assert result["n_samples"] == 640
            assert result["sample_rate"] == pytest.approx(20)
            assert result["freq_resolution"] == pytest.approx(0.03125)
            for key in ("raw_frequency", "refined_frequency"):
                assert result[key] == pytest.approx(2 / 3, abs=0.015625)
            results.append(result)
        assert results[1] == pytest.approx(results[0])
        parsed_signals.append(parsed_moment)
    assert parsed_signals[0] == pytest.approx(parsed_signals[1])
