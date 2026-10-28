# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand checks for the declared sinusoidal crank and timing model.

OpenStax University Physics Volume 1 (2016), section 15.1, equations 15.3,
15.4 and 15.7: harmonic position and velocity. Shift the origin to bottom of stroke
and use amplitude S/2. NIST SP 811 (2008), appendix B: minutes and seconds.
Inputs below are synthetic. Relative tolerances of 1e-11 and absolute
1e-12 tolerances cover decimal rounding and trigonometric roundoff only.
"""

import math

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import (
    Measurement,
    angular_velocity,
    crank_position,
    crank_velocity,
    divergence_onset,
    intervals_are_distinguishable,
    natural_frequency,
    peak_interval,
    peak_times,
    taper_adjusted_natural_frequency,
    time_at_position,
    time_from_card_position,
    undulations_per_half_stroke,
)


@pytest.mark.parametrize(
    "speed,omega", [(0.0, 0.0), (30.0, 3.14159265358979), (60.0, 6.28318530717959)]
)
def test_angular_velocity_in_radians_per_second(speed, omega):
    """One turn is 2*pi rad; N=0,30,60 per minute gives omega=0,pi,2*pi."""
    assert angular_velocity(speed) == pytest.approx(omega, rel=1e-11, abs=1e-12)


@pytest.mark.parametrize(
    "time,position,velocity",
    [
        (0.0, 0.0, 0.0),
        (0.25, 10.0, 62.8318530717959),
        (0.5, 20.0, 0.0),
        (0.75, 10.0, -62.8318530717959),
        (1.0, 0.0, 0.0),
    ],
)
def test_crank_at_cardinal_phases(time, position, velocity):
    """S=20 in, N=60/min: T=1 s and amplitude=10 in.

    At phases 0, pi/2, pi, 3*pi/2, 2*pi, x=10*(1-cos phase)
    gives 0,10,20,10,0 in; v=20*pi*sin phase gives 0,20*pi,0,-20*pi,0.
    """
    assert crank_position(time, 20.0, 60.0) == pytest.approx(
        position, rel=1e-11, abs=1e-12
    )
    assert crank_velocity(time, 20.0, 60.0) == pytest.approx(
        velocity, rel=1e-11, abs=1e-12
    )


@pytest.mark.parametrize(
    "position,time", [(0.0, 0.0), (5.0, 0.166666666666667), (10.0, 0.25), (20.0, 0.5)]
)
def test_time_inverse_has_independent_hand_values(position, time):
    """For S=20 in, T=1 s: x=5 means cos phase=1/2, phase=pi/3.

    Hence t=1/6 s, independent of the inverse implementation. Endpoints and
    midpoint give 0,1/4,1/2 s. Also check the forward map on the hand times.
    """
    assert time_at_position(position, 20.0, 60.0) == pytest.approx(time, abs=1e-12)
    assert crank_position(time, 20.0, 60.0) == pytest.approx(position, abs=1e-12)


def test_card_time_and_uncertainty_at_midstroke():
    """At x=10 in, t=1/4 s, v=20*pi in/s from the cardinal hand case.

    First-order inverse sensitivity gives dt=dx/abs(v). For a synthetic
    dx=1 in, dt=1/(20*pi)=0.0159154943092 s. This is propagation of an
    input uncertainty, not an asserted default digitization accuracy.
    """
    result = time_from_card_position(10.0, 20.0, 60.0, position_uncertainty_in=1.0)
    assert result.value == pytest.approx(0.25, abs=1e-12)
    assert result.uncertainty == pytest.approx(0.0159154943092, rel=1e-11)
    assert crank_position(result.value, 20.0, 60.0) == pytest.approx(10.0, abs=1e-12)


def test_card_time_at_bottom_has_unbounded_uncertainty():
    """The sinusoid has zero velocity at the bottom: dx/v is unbounded."""
    result = time_from_card_position(0.0, 20.0, 60.0, position_uncertainty_in=1.0)
    assert result.value == 0.0
    assert math.isinf(result.uncertainty)


@pytest.mark.parametrize(
    "separation,overlaps", [(0.75, True), (1.0, True), (1.25, False)]
)
def test_measurement_overlap_threshold(separation, overlaps):
    """Two +/-0.5 intervals overlap at center separations <=1 (closed ends)."""
    assert Measurement(0.0, 0.5).overlaps(Measurement(separation, 0.5)) is overlaps


@pytest.mark.parametrize(
    "last_time,distinguishable", [(2.75, False), (3.0, False), (3.25, True)]
)
def test_interval_resolution_at_below_and_above_threshold(last_time, distinguishable):
    """Synthetic t=[0,1,last] s each +/-0.25 s.

    Interval errors add to 0.5 s each. Their centers differ by last-2 s;
    the closed error intervals touch at last=3, so only last>3 resolves them.
    This checks the declared interval-arithmetic rule, not a physical error
    calibration or a statistical confidence level.
    """
    times = [Measurement(t, 0.25) for t in (0.0, 1.0, last_time)]
    assert intervals_are_distinguishable(times) is distinguishable


@pytest.mark.parametrize("count", [0, 1, 2])
def test_two_intervals_are_required_for_resolution(count):
    """Fewer than three peak times cannot form two intervals to compare."""
    assert (
        intervals_are_distinguishable([Measurement(t, 0.25) for t in range(count)])
        is False
    )


def test_natural_frequency_inverse_length_invariant():
    """Fixed-material axial-wave timing scales with length, so f(2L)=f(L)/2.

    NOT_EVALUATED: absolute API coefficient and default sonic speed; no
    accessible edition-specific source was established. The invariant does
    not certify the returned calibration or its agreement threshold.
    """
    short = natural_frequency(1000.0, check_agreement=False)
    long = natural_frequency(2000.0, check_agreement=False)
    assert short > 0.0
    assert short / long == pytest.approx(2.0, abs=1e-12)


def test_single_diameter_taper_identity():
    """A unit taper adjustment leaves a synthetic 60/min frequency unchanged.

    NOT_EVALUATED: non-unit empirical taper factors and section-dependent
    adjustment; the interface accepts a scalar, not section geometry.
    """
    assert taper_adjusted_natural_frequency(60.0) == 60.0
    assert taper_adjusted_natural_frequency(60.0, taper_factor=1.0) == 60.0


def test_peak_interval_and_undulation_count_from_frequency_definition():
    """A synthetic 120 cycles/min is 2 Hz: period=0.5 s.

    At N=30/min the crank half-period is 1 s, allowing two undulations.
    At N=60/min it is 0.5 s, allowing one. These are unit/period identities,
    independent of any empirical natural-frequency calibration.
    """
    assert peak_interval(120.0) == 0.5
    assert undulations_per_half_stroke(30.0, 120.0) == 2.0
    assert undulations_per_half_stroke(60.0, 120.0) == 1.0


def test_peak_train_hand_case_includes_exact_horizon():
    """N=30/min: full cycle=2 s, top=1 s. Ringing at 120/min: dt=0.5 s.

    Declared phase origins give up peaks [.5,1,1.5,2], down [1.5,2].
    Initial excitation at the origin itself is not a peak in this model.
    The model's physical peak excitation phase remains NOT_EVALUATED.
    """
    result = peak_times(30.0, 120.0)
    assert result.interval_s == 0.5
    assert result.top_of_stroke_s == 1.0
    assert result.upstroke == [0.5, 1.0, 1.5, 2.0]
    assert result.downstroke == [1.5, 2.0]


def test_peak_train_short_horizon_excludes_later_peaks():
    """Same hand case, with a synthetic 0.75 s horizon: only t=0.5 fits."""
    result = peak_times(30.0, 120.0, max_time_s=0.75)
    assert result.upstroke == [0.5]
    assert result.downstroke == []


def test_divergence_onset_hand_case_and_order_invariance():
    """Half periods for N=[15,30,60]/min are [2,1,0.5] s.

    The declared first-turnaround rule selects 0.5 s even if speeds repeat
    or arrive in a different order; no waveform divergence is simulated.
    """
    assert divergence_onset([15.0, 30.0, 60.0]) == 0.5
    assert divergence_onset([60.0, 15.0, 60.0, 30.0]) == 0.5
    assert divergence_onset([15.0]) == 2.0
