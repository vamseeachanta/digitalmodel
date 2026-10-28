# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Scalar production rates and analytic cumulative volumes.

Time is measured in days from the start of the ramp (t=0); rates are bbl/day,
nominal exponential decline D is per day, and cumulative volume is bbl.
All profile parameters are supplied by the caller, with no economic defaults.
Finite negative times return zero, after validation of the entire profile.

For ramp duration R, plateau duration P and plateau rate Q, the segments are
q(t)=Q*t/R during the ramp, q(t)=Q during the plateau, then
q(t)=Q*exp(-D*(t-R-P)). A zero ramp starts immediately at Q; a zero plateau
starts decline at the ramp end. D=0 holds Q indefinitely after the ramp.
Nonnegative finite real parameters are required; booleans are not parameters.

Mathematical basis:
    OpenStax Calculus Volume 1, section 6.8, Exponential Growth and Decay:
    dq/dt=-D*q with q(0)=Q gives q(s)=Q*exp(-D*s).
    https://openstax.org/books/calculus-volume-1/pages/6-8-exponential-growth-and-decay

    Section 5.2, The Definite Integral, supplies the accumulation basis:
    N(t)=integral(q(u) du), so (bbl/day)*day=bbl. Ramp area is Q*R/2,
    plateau area is Q*P, and tail area is Q*(1-exp(-D*s))/D. The factor
    1/2 follows from integral(u du)=u**2/2; zero is the startup baseline.
    https://openstax.org/books/calculus-volume-1/pages/5-2-the-definite-integral

These are mathematical profiles, without reserves constraints or an economic
cutoff. Very large volumes can exceed the floating point representable range.
"""

import math
from numbers import Real


def _scaled_product_ratio(factors: tuple[float, ...], divisor: float = 1.0) -> float:
    """Combine finite nonnegative ramp factors before rounding the exponent."""
    mantissa, exponent = math.frexp(divisor)
    mantissa = 1.0 / mantissa
    exponent = -exponent
    for factor in factors:
        fraction, shift = math.frexp(factor)
        mantissa *= fraction
        exponent += shift
    try:
        return math.ldexp(mantissa, exponent)
    except OverflowError:
        # A cumulative volume may exceed the representable floating range.
        return math.inf


def rate_at_time(
    time_days: float,
    *,
    ramp_duration_days: float,
    plateau_duration_days: float,
    plateau_rate_bbl_per_day: float,
    decline_rate_per_day: float,
) -> float:
    """Return the instantaneous production rate in bbl/day.

    Args:
        time_days: Finite elapsed days from the ramp start; negatives give zero.
        ramp_duration_days: Nonnegative time to reach the plateau rate, in days.
        plateau_duration_days: Nonnegative duration at the plateau rate, in days.
        plateau_rate_bbl_per_day: Nonnegative peak rate, in bbl/day.
        decline_rate_per_day: Nonnegative nominal exponential decline, in 1/day.

    Returns:
        Rate in bbl/day. With a zero ramp, the rate at time zero is the peak.

    Raises:
        ValueError: An argument is nonfinite, non-real or a boolean, or a profile
            parameter is negative. Negative finite times are accepted.
    """
    for name, value in (
        ("time_days", time_days),
        ("ramp_duration_days", ramp_duration_days),
        ("plateau_duration_days", plateau_duration_days),
        ("plateau_rate_bbl_per_day", plateau_rate_bbl_per_day),
        ("decline_rate_per_day", decline_rate_per_day),
    ):
        try:
            valid = (
                isinstance(value, Real)
                and not isinstance(value, bool)
                and math.isfinite(value)
                and (name == "time_days" or value >= 0.0)
            )
        except OverflowError:
            valid = False
        if not valid:
            raise ValueError(
                f"{name} must be a finite real number"
                " and profile parameters must be nonnegative"
            )

    if time_days < 0.0:
        return 0.0
    if time_days < ramp_duration_days:
        return _scaled_product_ratio(
            (plateau_rate_bbl_per_day, time_days), ramp_duration_days
        )
    after_ramp = time_days - ramp_duration_days
    if after_ramp <= plateau_duration_days:
        return plateau_rate_bbl_per_day
    decline_days = after_ramp - plateau_duration_days
    exponent = float(decline_rate_per_day) * float(decline_days)
    return plateau_rate_bbl_per_day * math.exp(-exponent)


def cumulative_production(
    time_days: float,
    *,
    ramp_duration_days: float,
    plateau_duration_days: float,
    plateau_rate_bbl_per_day: float,
    decline_rate_per_day: float,
) -> float:
    """Return analytic production from time zero through time_days, in bbl.

    Args:
        time_days: Finite elapsed days from the ramp start; negatives give zero.
        ramp_duration_days: Nonnegative time to reach the plateau rate, in days.
        plateau_duration_days: Nonnegative duration at the plateau rate, in days.
        plateau_rate_bbl_per_day: Nonnegative peak rate, in bbl/day.
        decline_rate_per_day: Nonnegative nominal exponential decline, in 1/day.

    Returns:
        Cumulative volume in bbl, including partial segments. For zero decline,
        the tail is a rectangle; expm1 avoids cancellation near that limit.

    Raises:
        ValueError: An argument is nonfinite, non-real or a boolean, or a profile
            parameter is negative. Negative finite times are accepted.
    """
    for name, value in (
        ("time_days", time_days),
        ("ramp_duration_days", ramp_duration_days),
        ("plateau_duration_days", plateau_duration_days),
        ("plateau_rate_bbl_per_day", plateau_rate_bbl_per_day),
        ("decline_rate_per_day", decline_rate_per_day),
    ):
        try:
            valid = (
                isinstance(value, Real)
                and not isinstance(value, bool)
                and math.isfinite(value)
                and (name == "time_days" or value >= 0.0)
            )
        except OverflowError:
            valid = False
        if not valid:
            raise ValueError(
                f"{name} must be a finite real number"
                " and profile parameters must be nonnegative"
            )

    if time_days <= 0.0:
        return 0.0
    if time_days < ramp_duration_days:
        return _scaled_product_ratio(
            (plateau_rate_bbl_per_day, time_days, time_days, 0.5),
            ramp_duration_days,
        )
    ramp_volume = _scaled_product_ratio(
        (plateau_rate_bbl_per_day, ramp_duration_days, 0.5)
    )
    after_ramp = time_days - ramp_duration_days
    plateau_volume = plateau_rate_bbl_per_day * min(
        after_ramp, plateau_duration_days
    )
    decline_days = max(after_ramp - plateau_duration_days, 0.0)
    exponent = float(decline_rate_per_day) * float(decline_days)
    if exponent == 0.0:
        # Also covers an underflowed D*s, whose relative loss is negligible.
        tail_volume = plateau_rate_bbl_per_day * decline_days
    else:
        tail_volume = plateau_rate_bbl_per_day * (
            -math.expm1(-exponent) / decline_rate_per_day
        )
    return ramp_volume + plateau_volume + tail_volume
