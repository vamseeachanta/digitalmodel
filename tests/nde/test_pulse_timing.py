# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Value checks for isolated synthetic pulses; no measured material data.

Travel-time basis: OpenStax, College Physics 2e (2022), section 2.3,
equation 2.5 (constant speed gives elapsed time = distance / speed):
https://openstax.org/books/college-physics-2e/pages/2-3-time-velocity-and-speed
All numbers below are chosen synthetic fixture inputs, not material constants
or engineering acceptance limits. Approximate comparisons use pytest's default
floating-point tolerance, not an inspection acceptance criterion.
"""

import math
from unittest.mock import patch

import pytest

from digitalmodel.nde.pulse_timing import pulse_peak_time


@pytest.mark.parametrize("polarity", [1.0, -1.0])
def test_synthetic_pulse_travel_time(polarity):
    """Hand calculation: 6 m / (2 m/s) = 3 s after emission.

    A synthetic Gaussian is centered at x = 6 m: p(t) = exp(-(6 - 2t)^2).
    Its unique absolute maximum occurs at t = 3 s; polarity cannot change it.
    The unit amplitude and width are fixture choices, not fitted coefficients.
    """
    times = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0]
    pressures = [polarity * math.exp(-((6.0 - 2.0 * t) ** 2)) for t in times]
    assert pulse_peak_time(times, pressures) == pytest.approx(3.0)


def test_streamed_synthetic_pulse_travel_time():
    """Hand calculation: emission at 1 s + 6 m / (2 m/s) = 4 s.

    The mock supplies a synthetic receiver trace, not actual Well output.
    Irregular timestamps deliberately prevent index-as-time from passing.
    The loader must preserve each pressure sample's timestamp and ordering.
    """
    from digitalmodel.nde.well_acoustic import acoustic_scattering_loader as mod

    times = [1.0, 1.5, 3.0, 4.0, 5.5]
    frames = [
        {"time_s": t, "pressure": math.exp(-((6.0 - 2.0 * (t - 1.0)) ** 2))}
        for t in times
    ]
    with (
        patch.object(mod, "WELL_AVAILABLE", True),
        patch.object(mod, "WellDataset", return_value=iter(frames)),
    ):
        sample = mod.AcousticScatteringLoader().stream_sample(n_steps=len(frames))
    received = sample["frames"]
    assert pulse_peak_time(
        [frame["time_s"] for frame in received],
        [frame["pressure"] for frame in received],
    ) == pytest.approx(4.0)


@pytest.mark.parametrize(
    "times, pressures, message",
    [
        ([], [], "nonempty"),
        ([0.0], [1.0, 2.0], "same length"),
        ([0.0, 0.0], [0.0, 1.0], "strictly increasing"),
        ([1.0, 0.0], [0.0, 1.0], "strictly increasing"),
        ([0.0, math.nan], [0.0, 1.0], "finite"),
        ([0.0, math.inf], [0.0, 1.0], "finite"),
        ([0.0, 1.0], [0.0, math.nan], "finite"),
        ([0.0, 1.0], [0.0, math.inf], "finite"),
        ([0.0, 1.0], [0.0, 0.0], "nonzero"),
        ([0.0, 1.0], [1.0, -1.0], "unique"),
    ],
)
def test_invalid_or_ambiguous_trace_rejected(times, pressures, message):
    """Invalid synthetic traces cannot establish a unique peak time."""
    with pytest.raises(ValueError, match=message):
        pulse_peak_time(times, pressures)


def test_single_sample_and_negative_time_origin():
    """A caller-defined time origin is retained, without an implicit offset."""
    assert pulse_peak_time([-1.0], [-2.0]) == -1.0
