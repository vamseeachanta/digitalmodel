"""Analytical checks for bounded timing, independent of implementation outputs."""

import math

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump.surface_undulations import (
    analyse_undulations,
    harmonic_motion,
    position_time_bounds,
)


def test_exact_timing_bounds_at_midstroke_and_ends():
    """Hand calculation: S=10, N=6 => cycle 10 s, half 5 s.

    x=2.5 => acos(0.5)/(pi/5)=5/3 s; x=7.5 => 10/3 s.
    With ±2.5 in, midstroke bounds are [5/3,10/3]. At x=0
    and x=10, clipping gives [0,5/3] and [10/3,5], respectively.
    Source: University Physics Volume 1 (2016), section 15.1, inverse cosine.
    """
    middle = position_time_bounds(5, 10, 6, 2.5)
    assert (middle.lower_s, middle.upper_s) == pytest.approx((5 / 3, 10 / 3))
    lower = position_time_bounds(0, 10, 6, 2.5)
    upper = position_time_bounds(10, 10, 6, 2.5)
    assert (lower.lower_s, lower.upper_s) == pytest.approx((0, 5 / 3))
    assert (upper.lower_s, upper.upper_s) == pytest.approx((10 / 3, 5))
    down = position_time_bounds(2.5, 10, 6, 0, branch="downstroke")
    assert down.lower_s == pytest.approx(25 / 3)
    assert down.upper_s == down.lower_s


def test_motion_derivatives_and_reversal():
    """S=10, cycle 10: A=5, omega=pi/5; vmax=pi, amax=pi²/5.

    Direct derivatives of x=5(1-cos(pi*t/5)); section 15.1 (2016).
    Absolute tolerance 1e-12 only accommodates floating-point sin(pi).
    """
    bottom = harmonic_motion(0, 10, 6)
    middle = harmonic_motion(2.5, 10, 6)
    top = harmonic_motion(5, 10, 6)
    assert bottom == pytest.approx((0, 0, math.pi**2 / 5))
    assert middle == pytest.approx((5, math.pi, 0), abs=1e-12)
    assert top == pytest.approx((10, 0, -(math.pi**2) / 5), abs=1e-12)


def test_wave_period_and_speed_independence():
    """Invented uniform bar: L=100, c=400 gives T=4L/c=1 s.

    Node-to-antinode distance L=lambda/4 (University Physics Volume 1,
    2016, section 17.4); hence f=1 Hz=60 cycles/min. At 6 SPM,
    half-cycle=5 s gives 5 oscillations; at 3 SPM it gives 10.
    These are synthetic values, not assumed material constants.
    """
    a = analyse_undulations(
        rod_length_ft=100,
        wave_speed_ft_s=400,
        taper_factor=1,
        stroke_in=10,
        strokes_per_minute=6,
        peak_positions_in=(),
        position_error_in=0,
    )
    b = analyse_undulations(
        rod_length_ft=100,
        wave_speed_ft_s=400,
        taper_factor=1,
        stroke_in=10,
        strokes_per_minute=3,
        peak_positions_in=(),
        position_error_in=0,
    )
    assert a.period_s == b.period_s == 1
    assert a.natural_frequency_spm == 60
    assert a.speed_ratio == pytest.approx(0.1)
    assert a.undulations_per_half_stroke == 5
    assert b.undulations_per_half_stroke == 10
    assert a.timing_status == "NOT_EVALUATED"


def test_interval_bounds_and_compatibility():
    """Peaks x=2.5,7.5 correspond to t=5/3,10/3; interval=5/3.

    Choose L=100,c=240 => 4L/c=5/3 exactly. At error=2.5, the
    bounds for the two peaks are [0,2.5], [2.5,5], so dt is [0,5].
    """
    inputs = dict(
        rod_length_ft=100,
        wave_speed_ft_s=240,
        taper_factor=1,
        stroke_in=10,
        strokes_per_minute=6,
        peak_positions_in=(2.5, 7.5),
        position_error_in=2.5,
    )
    result = analyse_undulations(**inputs)
    assert result.timing_status == "COMPATIBLE"
    assert result.interval_bounds[0].lower_s == 0
    assert result.interval_bounds[0].upper_s == 5
    outside = analyse_undulations(**{**inputs, "wave_speed_ft_s": 40})
    assert outside.timing_status == "INCOMPATIBLE"
    assert result.not_evaluated["pump_fillage"]
    assert result.not_evaluated["gearbox_rating"]
    assert result.not_evaluated["downhole_diagnosis"]


@pytest.mark.parametrize(
    "field,value",
    [
        ("rod_length_ft", 0),
        ("wave_speed_ft_s", -1),
        ("taper_factor", 0),
        ("taper_factor", True),
        ("taper_factor", "1"),
        ("taper_factor", None),
        ("taper_factor", 1j),
        ("stroke_in", float("inf")),
        ("strokes_per_minute", float("nan")),
        ("position_error_in", -1),
        ("peak_positions_in", (5, 2)),
        ("peak_positions_in", (5, 5)),
        ("peak_positions_in", (-1,)),
        ("peak_positions_in", (float("nan"),)),
    ],
)
def test_invalid_analysis_input(field, value):
    inputs = dict(
        rod_length_ft=100,
        wave_speed_ft_s=400,
        taper_factor=1,
        stroke_in=10,
        strokes_per_minute=6,
        peak_positions_in=(2, 5),
        position_error_in=0,
    )
    with pytest.raises(ValueError):
        analyse_undulations(**{**inputs, field: value})


@pytest.mark.parametrize("branch", ["invalid", "UPSTROKE"])
def test_unknown_branch(branch):
    with pytest.raises(ValueError):
        position_time_bounds(5, 10, 6, 1, branch=branch)


def test_position_error_larger_than_stroke_covers_half_cycle():
    """Clipping [x-error,x+error] to [0,S] spans [0,30/N]."""
    bounds = position_time_bounds(5, 10, 6, 100)
    assert (bounds.lower_s, bounds.upper_s) == (0, 5)


@pytest.mark.parametrize(
    "overrides",
    [
        {"rod_length_ft": 1e308},
        {"wave_speed_ft_s": 1e-308},
        {"strokes_per_minute": 1e-308},
    ],
)
def test_unrepresentable_results_are_rejected(overrides):
    """Extreme synthetic numbers test arithmetic range, not operating limits."""
    settings = dict(
        rod_length_ft=100,
        wave_speed_ft_s=400,
        taper_factor=1,
        stroke_in=10,
        strokes_per_minute=6,
        peak_positions_in=(),
        position_error_in=0,
    )
    with pytest.raises(ValueError):
        analyse_undulations(**{**settings, **overrides})
