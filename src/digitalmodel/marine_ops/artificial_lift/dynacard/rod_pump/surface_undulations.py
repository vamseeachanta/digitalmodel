"""Surface-card ringing with explicit material inputs and bounded peak times.

Sources: University Physics Volume 1 (2016), sections 15.1 (harmonic motion)
and 17.4 (quarter-wave node/antinode boundary conditions). The ideal uniform
longitudinal bar has the same node/antinode eigencondition: T=4L/c. This is a
screening model, not a damped wave inversion or proof of pump condition.

Wave speed and taper factor are required inputs. The calculation screens
ideal timing without applying a design-correlation speed envelope.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Sequence


@dataclass(frozen=True)
class TimeBounds:
    """Deterministic bounds in seconds; not statistical confidence limits."""

    lower_s: float
    upper_s: float


@dataclass(frozen=True)
class UndulationAnalysis:
    """Ideal ringing prediction and screenshot timing consistency only."""

    period_s: float
    natural_frequency_spm: float
    speed_ratio: float
    undulations_per_half_stroke: float
    peak_time_bounds: tuple[TimeBounds, ...]
    interval_bounds: tuple[TimeBounds, ...]
    timing_status: str
    not_evaluated: dict[str, str] = field(
        default_factory=lambda: {
            "pump_fillage": "Requires plunger travel, runtime and fluid volume basis; "
            "surface stroke and daily production alone do not identify fillage.",
            "gearbox_rating": "Requires pumping-unit geometry, counterbalance and rated torque; "
            "a prime mover designation is insufficient.",
            "downhole_diagnosis": "Requires validated surface-to-pump inversion, confirmed "
            "rod string, load calibration, tubing and damping inputs.",
        }
    )


def _positive(**values: float) -> None:
    for name, value in values.items():
        if not math.isfinite(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")


def harmonic_motion(
    time_s: float,
    stroke_in: float,
    strokes_per_minute: float,
) -> tuple[float, float, float]:
    """Return position, velocity, acceleration in in, in/s, in/s².

    University Physics Volume 1 (2016), section 15.1: translated cosine with
    amplitude S/2 and its first two derivatives. Sixty converts minutes to
    seconds; 2*pi converts cycles to radians. Constant crank speed is assumed.
    """
    _positive(stroke_in=stroke_in, strokes_per_minute=strokes_per_minute)
    if not math.isfinite(time_s):
        raise ValueError("time_s must be finite")
    omega = (2 * math.pi / 60) * strokes_per_minute
    phase = omega * time_s
    if not math.isfinite(phase):
        raise ValueError("motion exceeds floating-point range")
    amplitude = stroke_in / 2
    values = (
        amplitude * (1 - math.cos(phase)),
        amplitude * omega * math.sin(phase),
        amplitude * omega * omega * math.cos(phase),
    )
    if not all(math.isfinite(v) for v in values):
        raise ValueError("motion exceeds floating-point range")
    return values


def position_time_bounds(
    position_in: float,
    stroke_in: float,
    strokes_per_minute: float,
    position_error_in: float,
    *,
    branch: str = "upstroke",
) -> TimeBounds:
    """Invert both ends of a position-error interval, clipping to the stroke.

    Source: University Physics Volume 1 (2016), section 15.1,
    t=acos(1-2x/S)/omega. Monotonicity on the upstroke gives exact bounds
    under the harmonic assumption, including the endpoints where dx/v fails.
    Downstroke is reflected about half-cycle: t_down=cycle-t_up.
    """
    _positive(stroke_in=stroke_in, strokes_per_minute=strokes_per_minute)
    if not math.isfinite(position_in) or not 0 <= position_in <= stroke_in:
        raise ValueError("position must be finite and inside the stroke")
    if not math.isfinite(position_error_in) or position_error_in < 0:
        raise ValueError("position error must be finite and nonnegative")
    if branch not in ("upstroke", "downstroke"):
        raise ValueError("branch must be upstroke or downstroke")
    omega = (2 * math.pi / 60) * strokes_per_minute
    low = max(0, position_in - position_error_in)
    high = min(stroke_in, position_in + position_error_in)
    lower = math.acos(1 - 2 * (low / stroke_in)) / omega
    upper = math.acos(1 - 2 * (high / stroke_in)) / omega
    if branch == "downstroke":
        cycle = 60 / strokes_per_minute
        lower, upper = cycle - upper, cycle - lower
    if not all(math.isfinite(v) for v in (lower, upper)):
        raise ValueError("timing exceeds floating-point range")
    return TimeBounds(lower, upper)


def analyse_undulations(
    *,
    rod_length_ft: float,
    wave_speed_ft_s: float,
    taper_factor: float,
    stroke_in: float,
    strokes_per_minute: float,
    peak_positions_in: Sequence[float],
    position_error_in: float,
) -> UndulationAnalysis:
    """Compare user-selected upstroke peaks with ideal string ringing.

    Quarter-wave period T=4L/(c*Fc), frequency=60/T; half-cycle=30/N.
    Source: University Physics Volume 1 (2016), section 17.4, and the
    repository's rod-pump-rp11l.md (2026), taper-adjusted frequency definition.
    Fc must be supplied: one for a uniform bar, otherwise externally derived.

    Interval bounds use worst-case subtraction of adjacent time bounds.
    Compatibility means the predicted interval lies in every observed bound;
    it is neither a pump-health verdict nor a fitted phase/amplitude model.
    No acceptance coefficient or statistical uncertainty is assumed.
    """
    _positive(
        rod_length_ft=rod_length_ft,
        wave_speed_ft_s=wave_speed_ft_s,
        taper_factor=taper_factor,
        stroke_in=stroke_in,
        strokes_per_minute=strokes_per_minute,
    )
    if not math.isfinite(position_error_in) or position_error_in < 0:
        raise ValueError("position error must be finite and nonnegative")
    positions = tuple(peak_positions_in)
    times = tuple(
        position_time_bounds(x, stroke_in, strokes_per_minute, position_error_in)
        for x in positions
    )
    if any(b <= a for a, b in zip(positions, positions[1:])):
        raise ValueError("upstroke peak positions must be strictly increasing")
    period = 4 * rod_length_ft / (wave_speed_ft_s * taper_factor)
    _positive(period_s=period)
    frequency = 60 / period
    ratio = strokes_per_minute / frequency
    count = (30 / strokes_per_minute) / period
    _positive(
        natural_frequency_spm=frequency,
        speed_ratio=ratio,
        undulations_per_half_stroke=count,
    )
    intervals = tuple(
        TimeBounds(max(0, b.lower_s - a.upper_s), b.upper_s - a.lower_s)
        for a, b in zip(times, times[1:])
    )
    status = "NOT_EVALUATED"
    if intervals:
        status = (
            "COMPATIBLE"
            if all(i.lower_s <= period <= i.upper_s for i in intervals)
            else "INCOMPATIBLE"
        )
    return UndulationAnalysis(
        period_s=period,
        natural_frequency_spm=frequency,
        speed_ratio=ratio,
        undulations_per_half_stroke=count,
        peak_time_bounds=times,
        interval_bounds=intervals,
        timing_status=status,
    )
