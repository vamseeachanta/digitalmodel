# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Direct numerical API domain checks for issue #3283.

Source for the interval oracle: independent hand calculation, U/(f*D) =
12/(1*2) = 6. The supplied intervals test membership and inclusive endpoints;
they do not establish physical lock-in criteria. All other numeric inputs are
synthetic domain probes or integer-sequence cardinalities. The default band
control records the existing public API contract, (4, 8).
"""

import pytest

from digitalmodel.orcaflex.viv_screening import (
    BeamProperties,
    VIVScreeningInput,
    viv_screening,
)


INVALID_MODE_COUNTS = [
    pytest.param(0, id="zero"),
    pytest.param(-1, id="negative"),
    pytest.param(False, id="false"),
    pytest.param(True, id="true"),
    pytest.param(1.0, id="integral-float"),
    pytest.param(1.5, id="fractional"),
    pytest.param("2", id="numeric-string"),
    pytest.param(None, id="none"),
    pytest.param(float("nan"), id="nan"),
    pytest.param(float("inf"), id="positive-infinity"),
    pytest.param(float("-inf"), id="negative-infinity"),
]


@pytest.mark.parametrize("n_modes", INVALID_MODE_COUNTS)
def test_direct_screening_requires_positive_integer_modes(n_modes):
    """Reject invalid counts explicitly before a screening result is returned."""
    with pytest.raises(ValueError, match="n_modes must be a positive integer"):
        viv_screening(VIVScreeningInput(), BeamProperties(), n_modes=n_modes)


@pytest.mark.parametrize("n_modes", INVALID_MODE_COUNTS)
def test_frequency_list_requires_positive_integer_modes(n_modes):
    """Reject invalid counts explicitly instead of returning an empty list."""
    with pytest.raises(ValueError, match="n_modes must be a positive integer"):
        BeamProperties().natural_frequencies(n_modes=n_modes)


@pytest.mark.parametrize("n_modes", [1, 3])
def test_positive_integer_modes_check_every_requested_mode(n_modes):
    """The sequence from one through n has n entries, with no missing mode."""
    beam = BeamProperties()
    result = viv_screening(VIVScreeningInput(), beam, n_modes=n_modes)
    frequencies = beam.natural_frequencies(n_modes=n_modes)
    expected_modes = list(range(1, n_modes + 1))
    assert result.modes_checked == n_modes
    assert len(result.details) == len(frequencies) == n_modes
    assert [detail["mode"] for detail in result.details] == expected_modes
    assert [entry["mode"] for entry in frequencies] == expected_modes


@pytest.mark.parametrize(
    "interval",
    [
        pytest.param((8.0, 4.0), id="reversed"),
        pytest.param((4.0, 4.0), id="equal-positive"),
        pytest.param((0.0, 0.0), id="equal-zero"),
        pytest.param((-1.0, 8.0), id="negative-lower"),
        pytest.param((4.0, -1.0), id="negative-upper"),
        pytest.param((-8.0, -4.0), id="both-negative"),
        pytest.param((float("nan"), 8.0), id="nan-lower"),
        pytest.param((4.0, float("nan")), id="nan-upper"),
        pytest.param((float("inf"), 8.0), id="positive-infinity-lower"),
        pytest.param((4.0, float("inf")), id="positive-infinity-upper"),
        pytest.param((float("-inf"), 8.0), id="negative-infinity-lower"),
        pytest.param((4.0, float("-inf")), id="negative-infinity-upper"),
    ],
)
def test_lock_in_interval_requires_finite_ordered_nonnegative_bounds(interval):
    """Both bounds must be finite, nonnegative and strictly increasing."""
    with pytest.raises(
        ValueError,
        match="lock_in_vr_range must have finite nonnegative strictly increasing bounds",
    ):
        VIVScreeningInput(lock_in_vr_range=interval)


@pytest.mark.parametrize(
    "interval,expected_lock_in",
    [
        pytest.param([5.0, 7.0], True, id="interior"),
        pytest.param([7.0, 9.0], False, id="outside"),
        pytest.param([6.0, 7.0], True, id="inclusive-lower"),
        pytest.param([5.0, 6.0], True, id="inclusive-upper"),
        pytest.param([0.0, 1.0], False, id="zero-lower-admissible"),
    ],
)
def test_reduced_velocity_interval_hand_values(interval, expected_lock_in):
    """Hand oracle: U=12 m/s, D=2 m, f=1 Hz gives Vr=6 exactly."""
    viv_input = VIVScreeningInput(
        outer_diameter=2.0, current_speed=12.0, lock_in_vr_range=interval
    )
    assert viv_input.lock_in_vr_range == tuple(interval)
    result = viv_input.check_reduced_velocity(natural_freq=1.0)
    assert result["reduced_velocity"] == 6.0
    assert result["cross_flow_lock_in"] is expected_lock_in


def test_default_lock_in_band_is_preserved():
    """Keep the existing default band; its endpoints include the hand value."""
    viv_input = VIVScreeningInput(outer_diameter=2.0, current_speed=12.0)
    assert viv_input.lock_in_vr_range == (4.0, 8.0)
    result = viv_input.check_reduced_velocity(natural_freq=1.0)
    assert result["reduced_velocity"] == 6.0
    assert result["cross_flow_lock_in"] is True
