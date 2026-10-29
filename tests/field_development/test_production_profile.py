# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for a scalar production profile.

All example inputs are synthetic mathematical choices, not production data or
engineering defaults. Time is days, rate is bbl/day and volume is bbl. The main
example chooses a two-day ramp, three-day plateau, 120 bbl/day peak and ln(2)
per day decline, giving a one-day half-life. Fractions and factors below follow
from triangle/rectangle areas and integration of exp(-D*t).

Basis: OpenStax Calculus Volume 1, sections 6.8 (exponential decay) and 5.2
(definite integrals); see the public links in the module docstring. Numerical
sampling uses days too, so each rate-times-step term has units of barrels.
"""

import math
from decimal import Decimal, localcontext

import pytest

from digitalmodel.field_development.production_profile import (
    cumulative_production,
    rate_at_time,
)


@pytest.fixture
def profile():
    """Choose a synthetic profile whose decline halves the rate each day."""
    return dict(
        ramp_duration_days=2.0,
        plateau_duration_days=3.0,
        plateau_rate_bbl_per_day=120.0,
        decline_rate_per_day=math.log(2.0),
    )


@pytest.mark.parametrize(
    ("time_days", "expected"),
    [
        (0.0, 0.0),
        (1.0, 60.0),
        (2.0, 120.0),
        (4.0, 120.0),
        (5.0, 120.0),
        (6.0, 60.0),
        (7.0, 30.0),
    ],
)
def test_rate_matches_hand_calculation(profile, time_days, expected):
    """q=t*120/2 during ramp; after day 5, q=120*2**(-(t-5))."""
    assert rate_at_time(time_days, **profile) == pytest.approx(expected)


@pytest.mark.parametrize("boundary", [2.0, 5.0])
def test_profile_is_continuous(profile, boundary):
    """At day 2, 120*2/2=120; at day 5, 120*exp(0)=120.

    Adjacent representable times probe both sides without a chosen step size.
    Accumulated volume has limits 120 bbl at day 2 and 480 bbl at day 5.
    """
    expected_volume = {2.0: 120.0, 5.0: 480.0}[boundary]
    for time_days in (
        math.nextafter(boundary, -math.inf),
        boundary,
        math.nextafter(boundary, math.inf),
    ):
        assert rate_at_time(time_days, **profile) == pytest.approx(120.0)
        assert cumulative_production(time_days, **profile) == pytest.approx(
            expected_volume
        )


@pytest.mark.parametrize(
    ("time_days", "expected_bbl"),
    [
        (0.0, 0.0),
        (1.0, 30.0),
        (2.0, 120.0),
        (4.0, 360.0),
        (5.0, 480.0),
        (6.0, 480.0 + 60.0 / math.log(2.0)),
        (7.0, 480.0 + 90.0 / math.log(2.0)),
    ],
)
def test_cumulative_matches_hand_integrals(profile, time_days, expected_bbl):
    """Ramp: integral(60*t)=30*t**2, hence 120 bbl at day 2.

    Plateau: 120*(5-2)=360 bbl. One decline day adds
    120*(1-1/2)/ln(2)=60/ln(2); two add 120*(1-1/4)/ln(2).
    """
    assert cumulative_production(time_days, **profile) == pytest.approx(expected_bbl)


def test_zero_ramp_limit(profile):
    """No triangle: q(0)=120, plateau volume=3*120=360 bbl.

    One subsequent half-life adds 60/ln(2) bbl. The jump from zero before
    startup to 120 bbl/day at startup is intentional when ramp duration is zero.
    """
    profile["ramp_duration_days"] = 0.0
    assert rate_at_time(0.0, **profile) == 120.0
    assert cumulative_production(0.0, **profile) == 0.0
    assert rate_at_time(3.0, **profile) == 120.0
    assert rate_at_time(4.0, **profile) == pytest.approx(60.0)
    assert cumulative_production(4.0, **profile) == pytest.approx(
        360.0 + 60.0 / math.log(2.0)
    )


def test_zero_plateau_limit(profile):
    """Ramp ends at day 2 with 120 bbl; day 3 adds 60/ln(2) bbl."""
    profile["plateau_duration_days"] = 0.0
    assert rate_at_time(2.0, **profile) == 120.0
    assert rate_at_time(3.0, **profile) == pytest.approx(60.0)
    assert cumulative_production(3.0, **profile) == pytest.approx(
        120.0 + 60.0 / math.log(2.0)
    )


def test_immediate_decline(profile):
    """With both durations zero, q(1)=60 and N(1)=60/ln(2)."""
    profile.update(ramp_duration_days=0.0, plateau_duration_days=0.0)
    assert rate_at_time(0.0, **profile) == 120.0
    assert cumulative_production(0.0, **profile) == 0.0
    assert rate_at_time(1.0, **profile) == pytest.approx(60.0)
    assert cumulative_production(1.0, **profile) == pytest.approx(
        60.0 / math.log(2.0)
    )


@pytest.mark.parametrize("decline", [0.0, 1e-20])
def test_zero_decline_limit(profile, decline):
    """D=0 gives q=120 and N(10)=120+120*(10-2)=1080 bbl.

    At D=1e-20/day, the maximum relative rate loss over five decline days
    is bounded by D*5=5e-20, below floating point precision at this scale.
    """
    profile["decline_rate_per_day"] = decline
    assert rate_at_time(10.0, **profile) == pytest.approx(120.0)
    assert cumulative_production(10.0, **profile) == pytest.approx(1080.0)


@pytest.mark.parametrize("function", [rate_at_time, cumulative_production])
@pytest.mark.parametrize("time_days", [-10.0, -1.0, 0.0])
def test_before_startup(profile, function, time_days):
    """Before startup no rate or volume exists; a nonzero ramp starts at zero."""
    assert function(time_days, **profile) == 0.0


@pytest.mark.parametrize(
    "ramp,plateau,decline",
    [
        (2.0, 3.0, math.log(2.0)),
        (0.0, 3.0, math.log(2.0)),
        (2.0, 0.0, 0.0),
        (0.0, 0.0, 0.0),
    ],
)
@pytest.mark.parametrize("rate", [0.0, 120.0])
def test_cumulative_is_nondecreasing(profile, ramp, plateau, decline, rate):
    """Nonnegative rate implies nondecreasing integral; sample quarter days."""
    profile.update(
        ramp_duration_days=ramp,
        plateau_duration_days=plateau,
        decline_rate_per_day=decline,
        plateau_rate_bbl_per_day=rate,
    )
    volumes = [
        cumulative_production(index / 4.0, **profile) for index in range(-4, 41)
    ]
    assert all(math.isfinite(volume) and volume >= 0.0 for volume in volumes)
    assert all(left <= right for left, right in zip(volumes, volumes[1:]))
    if rate == 0.0:
        assert all(volume == 0.0 for volume in volumes)
        assert rate_at_time(10.0, **profile) == 0.0


def test_sampled_volume_has_same_units(profile):
    """Trapezoids use bbl/day * days = bbl over days 0 through 7.

    The ramp and plateau are integrated exactly on this grid. For the tail,
    |error| <= length*step**2*max(q'')/12 = 2*(0.001)**2*120*ln(2)**2/12
    < 1e-5 bbl. This independently bounds the sampling tolerance.
    """
    step_days = 0.001
    rates = [rate_at_time(index * step_days, **profile) for index in range(7001)]
    sampled_bbl = step_days * math.fsum(
        (left + right) / 2.0 for left, right in zip(rates, rates[1:])
    )
    assert sampled_bbl == pytest.approx(
        cumulative_production(7.0, **profile), abs=1e-5, rel=0.0
    )


def test_long_decline_limit(profile):
    """After 100 half-lives, tail volume approaches 120/ln(2) bbl.

    The residual fraction is 2**(-100), below floating point precision here.
    """
    assert cumulative_production(105.0, **profile) == pytest.approx(
        480.0 + 120.0 / math.log(2.0)
    )


@pytest.mark.parametrize("function", [rate_at_time, cumulative_production])
@pytest.mark.parametrize(
    "parameter",
    [
        "ramp_duration_days",
        "plateau_duration_days",
        "plateau_rate_bbl_per_day",
        "decline_rate_per_day",
    ],
)
@pytest.mark.parametrize(
    "value", [-1.0, math.nan, math.inf, -math.inf, True, "1", None, 1j]
)
def test_invalid_profile(profile, function, parameter, value):
    """Durations, rate and decline must be finite nonnegative real numbers."""
    profile[parameter] = value
    # Validation also applies before startup; invalid inputs never hide there.
    with pytest.raises(ValueError, match=parameter):
        function(-1.0, **profile)


@pytest.mark.parametrize("function", [rate_at_time, cumulative_production])
@pytest.mark.parametrize(
    "time_days", [math.nan, math.inf, -math.inf, True, "1", None, 1j]
)
def test_invalid_time(profile, function, time_days):
    """Only finite real times are accepted; negative finite times are valid."""
    with pytest.raises(ValueError, match="time_days"):
        function(time_days, **profile)


@pytest.mark.parametrize("function", [rate_at_time, cumulative_production])
@pytest.mark.parametrize(
    "rate,ramp,time_days",
    [
        (1e300, 1e250, 1e-100),
        (4.0, 2.0 * math.ulp(0.0), math.ulp(0.0)),
        (4.0, math.ulp(0.0), math.ulp(0.0)),
        (1e308, 2.0, 1.5),
        (1e300, 1e-100, 1e-200),
    ],
)
def test_ramp_preserves_representable_results(function, rate, ramp, time_days):
    """Decimal integrals retain small ratios and half-times independently.

    The first case has q=1e-50 and N=5e-151. With m the smallest positive
    float, Q=4, R=2*m and t=m give q=2 and N=m; at R=t=m, N=2*m.
    Decimal starts from the exact binary inputs and rounds only the result.
    """
    with localcontext() as context:
        context.prec = 80
        q, r, t = map(Decimal.from_float, (rate, ramp, time_days))
        expected = q * t / r
        if function is cumulative_production:
            expected *= t / Decimal(2)
    result = function(
        time_days,
        ramp_duration_days=ramp,
        plateau_duration_days=0.0,
        plateau_rate_bbl_per_day=rate,
        decline_rate_per_day=0.0,
    )
    assert result > 0.0
    assert result == pytest.approx(float(expected), rel=1e-14, abs=0.0)


@pytest.mark.parametrize("function", [rate_at_time, cumulative_production])
@pytest.mark.parametrize(
    "decline,time_days,expected_rate,expected_volume",
    [
        (1, 1, math.exp(-1.0), -math.expm1(-1.0)),
        (10**200, 10**200, 0.0, 1e-200),
        (10**200, 1, 0.0, 1e-200),
    ],
)
def test_decline_integer_float_parity(
    function, decline, time_days, expected_rate, expected_volume
):
    """For Q=1 and immediate decline, saturated decay gives q=0 and N=1/D.

    Integer and float inputs describe the same profile even when D*t exceeds
    float range. At D=t=1, q=exp(-1) and N=1-exp(-1).
    """
    expected = expected_rate if function is rate_at_time else expected_volume
    for convert in (int, float):
        result = function(
            convert(time_days),
            ramp_duration_days=convert(0),
            plateau_duration_days=convert(0),
            plateau_rate_bbl_per_day=convert(1),
            decline_rate_per_day=convert(decline),
        )
        assert result == pytest.approx(expected, rel=1e-14, abs=0.0)
