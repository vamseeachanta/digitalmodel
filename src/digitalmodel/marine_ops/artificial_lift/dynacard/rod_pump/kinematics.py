# ABOUTME: Rod-string natural frequency, card undulations, crank motion, peak phase.
# ABOUTME: Timing read off a position-axis card carries an explicit uncertainty.
"""Kinematics and free-vibration timing for a rod-pumped well.

Two results here are worth stating plainly because they are routinely
conflated in the field:

**Peak spacing is not peak phase.** The interval between load peaks on a card
is ``60/No'`` — a property of the rod string alone. It is the same on the
upstroke and the downstroke, and the same at every pumping speed. What *does*
change with speed is the phase: upstroke ringing is triggered at bottom of
stroke (``t = 0``), downstroke ringing at top of stroke (``t = 30/N``). Overlay
several speeds on a common time axis and the upstroke peaks align while the
downstroke peaks do not.

**Peaks look unevenly spaced on a card because the axis is position, not
time.** Polished-rod velocity goes to zero at both stroke ends, so a fixed
time interval maps to a shrinking distance near the top. Any time extracted
from a position-axis card therefore carries an uncertainty that grows as
velocity falls, and this module returns that uncertainty alongside the value
rather than a bare float.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import List, NamedTuple, Tuple

from .constants import (
    NATURAL_FREQUENCY_AGREEMENT_TOLERANCE_PCT,
    NATURAL_FREQUENCY_CONSTANT,
    NATURAL_FREQUENCY_SONIC_MULTIPLIER,
    SONIC_VELOCITY_STEEL_FT_S,
)


class Measurement(NamedTuple):
    """A value with its uncertainty, in the same units.

    Returned instead of a bare float wherever the number is derived from a
    position-axis card, so that a caller cannot accidentally treat a
    digitised reading as exact.
    """

    value: float
    uncertainty: float

    def overlaps(self, other: "Measurement") -> bool:
        """True if the two are indistinguishable within their uncertainties."""
        return abs(self.value - other.value) <= (self.uncertainty + other.uncertainty)

    def __str__(self) -> str:
        return f"{self.value:.3f} +/- {self.uncertainty:.3f}"


def natural_frequency(
    rod_length_ft: float,
    sonic_velocity_ft_s: float = SONIC_VELOCITY_STEEL_FT_S,
    check_agreement: bool = True,
) -> float:
    """Undamped natural frequency of the rod string, strokes per minute.

    API RP 11L gives ``No = 245,000 / L``. That constant is ``15 * c`` for
    steel, so the same number follows from the wave speed directly. The two
    routes are cross-checked, which catches a non-steel sonic velocity being
    passed while the RP 11L constant is still in play.

    Args:
        rod_length_ft: Total rod string length, feet.
        sonic_velocity_ft_s: Stress-wave speed. Steel by default; fibreglass
            is far slower and will trip the agreement check.
        check_agreement: Cross-check the two formulations.

    Raises:
        ValueError: If the length is not positive, or if the two routes
            disagree by more than the tolerance.
    """
    if rod_length_ft <= 0:
        raise ValueError(f"rod length must be positive; got {rod_length_ft}")

    from_constant = NATURAL_FREQUENCY_CONSTANT / rod_length_ft
    from_sonic = NATURAL_FREQUENCY_SONIC_MULTIPLIER * sonic_velocity_ft_s / rod_length_ft

    if check_agreement:
        disagreement = 100.0 * abs(from_constant - from_sonic) / from_constant
        if disagreement > NATURAL_FREQUENCY_AGREEMENT_TOLERANCE_PCT:
            raise ValueError(
                f"API RP 11L constant (No = {from_constant:.2f} SPM) and the "
                f"sonic-velocity form (15c/L = {from_sonic:.2f} SPM) disagree "
                f"by {disagreement:.2f}%, above the "
                f"{NATURAL_FREQUENCY_AGREEMENT_TOLERANCE_PCT}% tolerance. The "
                f"245,000 constant assumes steel; sonic velocity "
                f"{sonic_velocity_ft_s} ft/s is not steel."
            )
    return from_constant


def taper_adjusted_natural_frequency(
    natural_frequency_spm: float, taper_factor: float = 1.0
) -> float:
    """``No' = Fc * No``. ``Fc`` is 1.000 for a single-diameter string."""
    if taper_factor <= 0:
        raise ValueError(f"taper factor must be positive; got {taper_factor}")
    return taper_factor * natural_frequency_spm


def peak_interval(taper_adjusted_frequency_spm: float) -> float:
    """Time between successive load peaks, ``dt = 60/No'``, seconds.

    Independent of pumping speed — this is the string ringing at its own
    frequency, not responding to the crank.
    """
    return 60.0 / taper_adjusted_frequency_spm


def undulations_per_half_stroke(
    strokes_per_minute: float, taper_adjusted_frequency_spm: float
) -> float:
    """Number of load undulations visible per half stroke, ``0.5 / (N/No')``.

    Rises as the unit is slowed: a slower stroke gives the string more time to
    ring. Only at 1-2 SPM does a surface card start to look rectangular.
    """
    if strokes_per_minute <= 0:
        raise ValueError(f"SPM must be positive; got {strokes_per_minute}")
    return 0.5 / (strokes_per_minute / taper_adjusted_frequency_spm)


def angular_velocity(strokes_per_minute: float) -> float:
    """Crank angular velocity, radians per second."""
    return 2.0 * math.pi * strokes_per_minute / 60.0


def crank_position(time_s: float, stroke_in: float, strokes_per_minute: float) -> float:
    """Polished-rod position, ``x = (S/2)(1 - cos wt)``, inches from bottom."""
    omega = angular_velocity(strokes_per_minute)
    return (stroke_in / 2.0) * (1.0 - math.cos(omega * time_s))


def crank_velocity(time_s: float, stroke_in: float, strokes_per_minute: float) -> float:
    """Polished-rod velocity, ``v = (S/2) w sin(wt)``, inches per second."""
    omega = angular_velocity(strokes_per_minute)
    return (stroke_in / 2.0) * omega * math.sin(omega * time_s)


def time_at_position(
    position_in: float, stroke_in: float, strokes_per_minute: float
) -> float:
    """Invert the crank relation for the first half cycle, seconds.

    Raises:
        ValueError: If the position lies outside the stroke.
    """
    if not 0.0 <= position_in <= stroke_in:
        raise ValueError(
            f"position {position_in} in lies outside the 0-{stroke_in} in stroke"
        )
    omega = angular_velocity(strokes_per_minute)
    return math.acos(1.0 - 2.0 * position_in / stroke_in) / omega


def time_from_card_position(
    position_in: float,
    stroke_in: float,
    strokes_per_minute: float,
    position_uncertainty_in: float = 1.5,
) -> Measurement:
    """Convert a card position to a time, carrying the propagated uncertainty.

    ``dt = dx / v``, and velocity vanishes at both stroke ends, so the same
    digitising error buys far more time error near the top of the stroke than
    at mid-stroke. This is why peak-to-peak intervals read off a position-axis
    card can appear unequal when the underlying physics says they are not.

    Args:
        position_uncertainty_in: Digitising error on the position axis.
    """
    time_s = time_at_position(position_in, stroke_in, strokes_per_minute)
    velocity = abs(crank_velocity(time_s, stroke_in, strokes_per_minute))
    if velocity <= 0:
        return Measurement(time_s, math.inf)
    return Measurement(time_s, position_uncertainty_in / velocity)


def intervals_are_distinguishable(times: List[Measurement]) -> bool:
    """True only if every consecutive interval is resolvable given the errors.

    Guards the reporting path: consecutive peak-to-peak intervals must not be
    presented as a finding when their uncertainties overlap. On a card
    digitised to +/-1.5 in this is usually False, and the honest statement is
    the mean interval rather than the individual ones.
    """
    if len(times) < 3:
        return False
    intervals = [
        Measurement(
            times[i + 1].value - times[i].value,
            times[i + 1].uncertainty + times[i].uncertainty,
        )
        for i in range(len(times) - 1)
    ]
    return not any(
        a.overlaps(b) for a, b in zip(intervals, intervals[1:])
    )


@dataclass
class PeakTrains:
    """Predicted load-peak times for one stroke, seconds from bottom of stroke."""

    upstroke: List[float]
    downstroke: List[float]
    interval_s: float
    top_of_stroke_s: float


def peak_times(
    strokes_per_minute: float,
    taper_adjusted_frequency_spm: float,
    max_time_s: float = None,
) -> PeakTrains:
    """Predicted upstroke and downstroke peak trains with correct phase.

    Both trains share the interval ``60/No'``. They differ in phase reference:
    the upstroke train is triggered at bottom of stroke (``t = 0``), the
    downstroke train at top of stroke (``t = 30/N``). Consequently, across
    different pumping speeds the upstroke peaks coincide while the downstroke
    peaks separate — the top of stroke moves.

    Args:
        max_time_s: Horizon to generate to. Defaults to one full cycle.
    """
    interval = peak_interval(taper_adjusted_frequency_spm)
    top_of_stroke = 30.0 / strokes_per_minute
    horizon = max_time_s if max_time_s is not None else 60.0 / strokes_per_minute

    upstroke, k = [], 1
    while k * interval <= horizon:
        upstroke.append(k * interval)
        k += 1

    downstroke, k = [], 1
    while top_of_stroke + k * interval <= horizon:
        downstroke.append(top_of_stroke + k * interval)
        k += 1

    return PeakTrains(
        upstroke=upstroke,
        downstroke=downstroke,
        interval_s=interval,
        top_of_stroke_s=top_of_stroke,
    )


def divergence_onset(strokes_per_minute_values: List[float]) -> float:
    """Time at which overlaid traces at different speeds begin to disagree.

    Upstroke ringing is common to every speed, so traces track each other
    until the first of them turns around. That is the earliest top of stroke,
    ``min(30/N)`` — which is set by the *fastest* unit.
    """
    if not strokes_per_minute_values:
        raise ValueError("need at least one pumping speed")
    return min(30.0 / spm for spm in strokes_per_minute_values)
