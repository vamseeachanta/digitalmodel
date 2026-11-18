# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent discrete-sine hand calculations for history summaries.

All signal parameters below are synthetic test choices, not literature data.
Over an integer number of uniformly sampled cycles, sum(sin) = 0 and
sum(sin**2) / N = 1/2 (the finite geometric-series identity). Thus a sine
with amplitude A has fluctuating RMS A/sqrt(2). Strouhal follows its
definition St = f * width / speed; neither oracle calls the spectral API.
"""

import math

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.validation.square_cylinder import (
    summarize_square_cylinder_history,
)


def _history():
    """Choose eight seconds at 32 Hz: eight complete cycles of a 1 Hz sine."""
    times = np.arange(256) / 32
    cd = 2 + 0.5 * np.cos(2 * np.pi * times)
    cl = 3 * np.sin(2 * np.pi * times)
    return times, cd, cl


def _summarize(times, cd, cl, **kwargs):
    options = dict(speed=4.0, width=2.0, averaging_interval=(0.0, 8.0))
    options.update(kwargs)
    return summarize_square_cylinder_history(times, cd, cl, **options)


def test_periodic_history_hand_summary():
    """Eight cycles give mean Cd=2, RMS Cl=3/sqrt(2), St=1*2/4."""
    result = _summarize(*_history())

    assert result.mean_cd == pytest.approx(2)
    assert result.rms_cl == pytest.approx(3 / math.sqrt(2))
    assert result.shedding_frequency == pytest.approx(1)
    assert result.strouhal == pytest.approx(1 * 2 / 4)
    assert result.sample_count == 256
    assert result.averaging_interval == (0, 8)
    # DFT bin spacing: fs/N = 32/256 = 1/8 Hz.
    assert result.frequency_resolution == pytest.approx(1 / 8)


def test_mean_offset_does_not_set_shedding_frequency():
    """Use only [4, 8): four cycles, excluding a strong earlier tone and stop."""
    # Include t=8 deliberately, so selecting the right endpoint changes Cd.
    times = np.arange(257) / 32
    selected = (times >= 4) & (times < 8)
    cd = np.where(selected, 3.0, 99.0)
    cl = np.where(
        selected,
        100 + 2 * np.sin(2 * np.pi * times),
        500 + 50 * np.sin(2 * np.pi * 4 * times),
    )
    result = _summarize(times, cd, cl, averaging_interval=(4, 8))

    assert result.mean_cd == pytest.approx(3)
    assert result.rms_cl == pytest.approx(2 / math.sqrt(2))
    assert result.shedding_frequency == pytest.approx(1)
    assert result.strouhal == pytest.approx(1 / 2)
    assert result.sample_count == 128
    assert result.averaging_interval == (4, 8)
    assert result.frequency_resolution == pytest.approx(1 / 4)


def test_largest_nonzero_bin_includes_nyquist():
    """Alternating +/-2 has RMS 2 and frequency fs/2 = 16 Hz."""
    times, cd, _cl = _history()
    cl = 2 * (-1.0) ** np.arange(times.size)
    result = _summarize(times, cd, cl)

    assert result.rms_cl == pytest.approx(2)
    assert result.shedding_frequency == pytest.approx(16)
    assert result.strouhal == pytest.approx(16 * 2 / 4)


def test_largest_nonzero_bin_wins_over_smaller_tone():
    """Orthogonal 1 and 3 Hz tones: the amplitude-3 tone dominates."""
    times, cd, cl = _history()
    cl += np.sin(2 * np.pi * 3 * times)
    result = _summarize(times, cd, cl)

    # Mean square of orthogonal sines: (3**2 + 1**2)/2 = 5.
    assert result.rms_cl == pytest.approx(math.sqrt(5))
    assert result.shedding_frequency == pytest.approx(1)


@pytest.mark.parametrize("offset", [0.0, 100.0])
def test_constant_lift_has_no_shedding_frequency(offset):
    times, cd, cl = _history()
    cl[:] = offset
    result = _summarize(times, cd, cl)

    assert result.rms_cl == 0
    assert math.isnan(result.shedding_frequency)
    assert math.isnan(result.strouhal)


def test_accepts_lists_without_mutating_histories():
    histories = [array.tolist() for array in _history()]
    before = [values.copy() for values in histories]
    assert _summarize(*histories).mean_cd == pytest.approx(2)
    assert histories == before


@pytest.mark.parametrize("count", [0, 1, 2, 3])
def test_insufficient_samples_rejected(count):
    arrays = [array[:count] for array in _history()]
    with pytest.raises(ValueError, match="[Aa]t least four"):
        _summarize(*arrays)


def test_insufficient_samples_in_averaging_interval_rejected():
    with pytest.raises(ValueError, match="[Aa]t least four"):
        _summarize(*_history(), averaging_interval=(0, 2 / 32))


@pytest.mark.parametrize("interval", [(9, 10), (-2, -1)])
def test_empty_averaging_interval_rejected(interval):
    with pytest.raises(ValueError, match="[Aa]t least four"):
        _summarize(*_history(), averaging_interval=interval)


@pytest.mark.parametrize("index", [0, 1, 2])
def test_mismatched_lengths_rejected(index):
    arrays = list(_history())
    arrays[index] = arrays[index][:-1]
    with pytest.raises(ValueError, match="same length"):
        _summarize(*arrays)


@pytest.mark.parametrize("index", [0, 1, 2])
def test_multidimensional_input_rejected(index):
    arrays = list(_history())
    arrays[index] = arrays[index].reshape(16, 16)
    with pytest.raises(ValueError, match="one-dimensional"):
        _summarize(*arrays)


@pytest.mark.parametrize("index", [0, 1, 2])
@pytest.mark.parametrize("bad", [np.nan, np.inf, -np.inf])
def test_nonfinite_history_rejected(index, bad):
    arrays = list(_history())
    arrays[index][0] = bad
    # Reject invalid values even before the averaging interval.
    with pytest.raises(ValueError, match="finite"):
        _summarize(*arrays, averaging_interval=(4, 8))


@pytest.mark.parametrize("kind", ["irregular", "duplicate", "reversed"])
def test_invalid_time_spacing_rejected(kind):
    times, cd, cl = _history()
    if kind == "irregular":
        times[1] += 1 / 128  # A quarter of the chosen sample spacing.
    elif kind == "duplicate":
        times[1] = times[0]
    else:
        times = times[::-1]
    with pytest.raises(ValueError, match="uniform|increasing"):
        _summarize(times, cd, cl, averaging_interval=(4, 8))


@pytest.mark.parametrize("scale", ["speed", "width"])
@pytest.mark.parametrize("bad", [0.0, -1.0, np.nan, np.inf, -np.inf])
def test_invalid_physical_scales_rejected(scale, bad):
    with pytest.raises(ValueError, match=f"{scale}.*finite.*positive"):
        _summarize(*_history(), **{scale: bad})


@pytest.mark.parametrize(
    "interval",
    [(4, 4), (8, 4), (np.nan, 8), (0, np.inf), (-np.inf, 8), (0,), (0, 4, 8)],
)
def test_invalid_averaging_interval_rejected(interval):
    with pytest.raises(ValueError, match="averaging_interval"):
        _summarize(*_history(), averaging_interval=interval)
