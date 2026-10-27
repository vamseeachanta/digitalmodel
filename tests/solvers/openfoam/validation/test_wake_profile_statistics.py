# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic hand calculations for wake-profile statistics.

All numerical inputs below are constructed test data, not physical reference
values. Expected statistics follow the explicit sums in each test.
"""

import numpy as np
import pytest

from digitalmodel.solvers.openfoam.validation.wake_profiles import (
    wake_profile_statistics,
)


def test_wake_mean_rms_hand_sum():
    """Check each Cartesian component at each probe using population sums."""
    times = [0.0, 1.0, 2.0]
    coords = [[4.0, -1.0, 0.0], [4.0, 1.0, 0.0]]
    velocity = np.array([
        [[1.0, 2.0, -1.0], [2.0, -3.0, 5.0]],
        [[3.0, 2.0, 0.0], [2.0, 0.0, 7.0]],
        [[5.0, 2.0, 1.0], [8.0, 3.0, 9.0]],
    ])
    result = wake_profile_statistics(times, velocity, coords, (0.0, 2.0))
    # Means by hand: (1+3+5)/3, (2+2+2)/3, (-1+0+1)/3;
    # (2+2+8)/3, (-3+0+3)/3, (5+7+9)/3.
    np.testing.assert_allclose(result.mean_velocity, [[3, 2, 0], [4, 0, 7]])
    # Squared deviations by hand: (4+0+4), (0+0+0), (1+0+1);
    # (4+4+16), (9+0+9), (4+0+4). Population denominator is 3.
    expected_rms = np.sqrt(np.array([[8, 0, 2], [24, 18, 8]]) / 3)
    np.testing.assert_allclose(result.rms_fluctuations, expected_rms)
    np.testing.assert_array_equal(result.probe_coords, coords)
    np.testing.assert_array_equal(result.selected_times, times)
    assert result.sample_count == 3
    assert result.averaging_interval == (0.0, 2.0)
    assert result.rms_convention == "population"
    assert result.weighting == "equal_samples"


def test_wake_window_and_probe_order():
    """Window endpoints are inclusive; permutations and offsets commute."""
    times = np.array([0.0, 1.0, 2.0, 4.0, 5.0])
    coords = np.array([[4, -1, 0], [4, 1, 0]])
    velocity = np.array([[90, -90], [1, 2], [3, 2], [5, 8], [-90, 90]])
    interval = (0.5, 4.0)
    result = wake_profile_statistics(times, velocity, coords, interval)
    # Only [1,3,5] and [2,2,8] contribute: means 3 and 4; sums 8 and 24.
    np.testing.assert_allclose(result.mean_velocity, [3, 4])
    np.testing.assert_allclose(result.rms_fluctuations, np.sqrt([8 / 3, 24 / 3]))
    np.testing.assert_array_equal(result.selected_times, [1, 2, 4])
    assert result.sample_count == 3
    assert result.averaging_interval == interval
    order = [1, 0]
    permuted = wake_profile_statistics(
        times, velocity[:, order], coords[order], interval
    )
    np.testing.assert_array_equal(permuted.probe_coords, coords[order])
    np.testing.assert_allclose(permuted.mean_velocity, result.mean_velocity[order])
    np.testing.assert_allclose(
        permuted.rms_fluctuations, result.rms_fluctuations[order]
    )
    # Adding the constructed common offset shifts only the mean.
    offset = 16
    shifted = wake_profile_statistics(times, velocity + offset, coords, interval)
    np.testing.assert_allclose(shifted.mean_velocity, result.mean_velocity + offset)
    np.testing.assert_allclose(shifted.rms_fluctuations, result.rms_fluctuations)


def test_single_sample_and_copied_support():
    """One sample has zero fluctuation and returned support owns its data."""
    times = np.array([0.0, 1.0])
    coords = np.array([[4.0, 0.0, 0.0]])
    velocity = np.array([[2.0], [6.0]])
    result = wake_profile_statistics(times, velocity, coords, (1.0, 1.0))
    np.testing.assert_array_equal(result.mean_velocity, [6])
    np.testing.assert_array_equal(result.rms_fluctuations, [0])
    assert result.sample_count == 1
    times[:] = -1
    coords[:] = -1
    velocity[:] = -1
    np.testing.assert_array_equal(result.selected_times, [1])
    np.testing.assert_array_equal(result.probe_coords, [[4, 0, 0]])
    np.testing.assert_array_equal(result.mean_velocity, [6])


@pytest.mark.parametrize(
    "velocity",
    [
        [[np.finfo(np.float64).max], [np.finfo(np.float64).max]],
        [[1e200], [-1e200]],
    ],
    ids=["mean-overflow", "squared-deviation-overflow"],
)
def test_finite_wake_inputs_reject_reduction_overflow(velocity):
    """Finite samples may overflow the mean sum or the squared deviations."""
    assert np.all(np.isfinite(velocity))
    # Equal maxima overflow the mean sum; opposite samples have mean zero
    # but squared deviations beyond the floating-point range.
    with pytest.raises(
        ValueError, match="^velocity statistics exceed floating-point range$"
    ) as error:
        wake_profile_statistics([0, 1], velocity, [[0, 0, 0]], (0, 1))
    assert isinstance(error.value.__cause__, FloatingPointError)


@pytest.mark.parametrize(
    "times,velocity,coords,interval,match",
    [
        ([0, 1], [[1], [2]], [[0, 0, 0]], (2, 3), "no samples"),
        ([], np.empty((0, 1)), [[0, 0, 0]], (0, 1), "no samples"),
        ([[0, 1]], [[1], [2]], [[0, 0, 0]], (0, 1), "times"),
        ([0, 0], [[1], [2]], [[0, 0, 0]], (0, 1), "increasing"),
        ([1, 0], [[1], [2]], [[0, 0, 0]], (0, 1), "increasing"),
        ([0, np.nan], [[1], [2]], [[0, 0, 0]], (0, 1), "finite"),
        ([0, 1], [1, 2], [[0, 0, 0]], (0, 1), "velocity"),
        ([0, 1], [[1]], [[0, 0, 0]], (0, 1), "velocity"),
        ([0, 1], [[1, 2], [3, 4]], [[0, 0, 0]], (0, 1), "probe"),
        ([0, 1], np.ones((2, 1, 2)), [[0, 0, 0]], (0, 1), "velocity"),
        ([0, 1], np.ones((2, 1, 3, 1)), [[0, 0, 0]], (0, 1), "velocity"),
        ([0, 1], np.empty((2, 0)), np.empty((0, 3)), (0, 1), "probe"),
        ([0, 1], [[1], [2]], [[0, 0]], (0, 1), "coordinates"),
        ([0, 1], [[1], [2]], [0, 0, 0], (0, 1), "coordinates"),
        ([0, 1], [[1], [2]], [[0, np.inf, 0]], (0, 1), "finite"),
        ([0, 1], [[1], [np.nan]], [[0, 0, 0]], (0, 1), "finite"),
        # Nonfinite samples outside the averaging window also fail.
        ([0, 1], [[np.inf], [2]], [[0, 0, 0]], (1, 1), "finite"),
        ([0, 1], [[1], [2]], [[0, 0, 0]], (1, 0), "interval"),
        ([0, 1], [[1], [2]], [[0, 0, 0]], (0, np.inf), "finite"),
        ([0, 1], [[1], [2]], [[0, 0, 0]], (0,), "interval"),
        ([0, 1], [[1], [2]], [[0, 0, 0]], [[0, 1]], "interval"),
        ([0, 1], [[1j], [2]], [[0, 0, 0]], (0, 1), "real"),
    ],
)
def test_invalid_wake_inputs(times, velocity, coords, interval, match):
    """Malformed data fail before any statistics can be reported."""
    with pytest.raises(ValueError, match=match):
        wake_profile_statistics(times, velocity, coords, interval)
