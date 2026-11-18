# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic pulse anchors: OpenStax College Physics 2e (2022), §2.3, Eq. 2.5.

All distances, speeds and sample amplitudes below are fixture choices, not
material properties or inspection acceptance limits. Constant-speed elapsed
time is distance/speed; the peak timestamp adds the emission reference.
"""

from digitalmodel.nde.pulse_timing import pulse_peak_time


def test_peak_travel_time_literal():
    """6 m / (2 m/s)=3 s; isolated peak is sampled exactly at arrival."""
    assert pulse_peak_time([0, 1, 2, 3, 4], [0, 1, 2, 4, 1]) == 3


def test_peak_emission_offset_literal():
    """Emission at 1 s + 6 m / (2 m/s)=4 s; irregular sampling retains time."""
    assert pulse_peak_time([1, 1.5, 3, 4, 5.5], [0, 1, 2, 4, 1]) == 4


def test_negative_peak_literal():
    """4 m / (2 m/s)=2 s; abs(-4)=4 exceeds abs(2), so polarity is irrelevant."""
    assert pulse_peak_time([0, 1, 2, 3], [0, 2, -4, 1]) == 2
