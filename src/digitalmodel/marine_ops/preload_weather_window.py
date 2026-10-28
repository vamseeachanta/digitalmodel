# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline preload persistence and fixed-body regular-swell screening.

No climate series, installation limits or standard-derived constants are built
in. The caller supplies limits, duration, wave height, geometry and coefficients.
The companion example distinguishes assumed weather from supplied observations.
See docs/domains/geotechnical/preload-weather-window.md for equations, public
source context, datum definitions and limits on engineering use.

Clause context only: SNAME 5-5A Rev 3 (2008), 4.3, 4.4, 4.6 and 4.7;
ISO 19905-1:2023, 6.4, 7.3.2 and 7.3.3. This simplified incident-wave model
does not implement a complete standard assessment or establish compliance.
"""

from __future__ import annotations

import math
from collections.abc import Sequence
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from decimal import Decimal, ROUND_CEILING


def _finite(name: str, value: float, *, positive: bool = False) -> None:
    if not math.isfinite(value) or (value <= 0 if positive else value < 0):
        raise ValueError(
            f"{name} must be finite and {'positive' if positive else 'nonnegative'}"
        )


@dataclass(frozen=True)
class WaveSample:
    """UTC-aware block start, significant height (m) and peak period (s).

    Each observation represents a full cadence block, held constant. None or
    NaN marks missing evidence. Infinity and negative physical values are errors.
    """

    timestamp: datetime
    hs_m: float | None
    tp_s: float | None


@dataclass(frozen=True)
class SeaStateLimit:
    """Inclusive operational bounds; period limits remain explicit inputs."""

    max_hs_m: float
    min_tp_s: float
    max_tp_s: float

    def validate(self) -> None:
        """Reject nonphysical or reversed criteria."""
        _finite("maximum Hs", self.max_hs_m)
        _finite("minimum Tp", self.min_tp_s, positive=True)
        _finite("maximum Tp", self.max_tp_s, positive=True)
        if self.min_tp_s > self.max_tp_s:
            raise ValueError("minimum period exceeds maximum period")


@dataclass(frozen=True)
class MonthlyWindow:
    """Pooled calendar-month counts, attributed to the UTC start month.

    Probability is conditional on complete data for a randomly sampled start,
    not the chance of at least one window in a month. Bounds assign every unknown
    start to failure/success respectively. Incomplete terminal windows are
    censored, excluded from both bounds and counted separately.
    """

    month: int
    complete_starts: int
    workable_starts: int
    unknown_starts: int
    censored_starts: int
    probability: float | None
    lower_probability: float | None
    upper_probability: float | None


def _cadence(cadence_h: float) -> timedelta:
    _finite("cadence", cadence_h, positive=True)
    micros = Decimal(str(cadence_h)) * 3_600_000_000
    if micros != micros.to_integral_value() or micros < 1:
        raise ValueError("cadence must resolve to whole microseconds")
    return timedelta(microseconds=int(micros))


def _sample_state(sample: WaveSample, limit: SeaStateLimit) -> tuple[int, int]:
    missing = False
    for name, value in (("Hs", sample.hs_m), ("Tp", sample.tp_s)):
        if value is None or math.isnan(value):
            missing = True
        else:
            _finite(name, value, positive=name == "Tp")
    if missing or sample.hs_m is None or sample.tp_s is None:
        return 1, 0
    good = (
        sample.hs_m <= limit.max_hs_m
        and limit.min_tp_s <= sample.tp_s <= limit.max_tp_s
    )
    return 0, int(not good)


def _grid(
    samples: Sequence[WaveSample], step: timedelta, limit: SeaStateLimit
) -> tuple[datetime, list[tuple[int, int]]]:
    if not samples:
        raise ValueError("at least one sample is required")
    origin = samples[0].timestamp
    if origin.tzinfo is None or origin.utcoffset() is None:
        raise ValueError("timestamps must be timezone-aware")
    origin = origin.astimezone(timezone.utc)
    states = []
    previous_index = -1
    for sample in samples:
        if sample.timestamp.tzinfo is None or sample.timestamp.utcoffset() is None:
            raise ValueError("timestamps must be timezone-aware")
        offset = sample.timestamp.astimezone(timezone.utc) - origin
        index, remainder = divmod(offset, step)
        if remainder or index <= previous_index:
            raise ValueError("samples must be ordered, unique and cadence-aligned")
        # Avoid expanding an accidentally tiny cadence into unbounded memory.
        if index > 10_000_000:
            raise ValueError("series exceeds ten million cadence blocks")
        states.extend([(1, 0)] * (index - previous_index - 1))
        states.append(_sample_state(sample, limit))
        previous_index = index
    return origin, states


def monthly_windows(
    samples: Sequence[WaveSample],
    *,
    duration_h: float,
    cadence_h: float,
    limit: SeaStateLimit,
) -> tuple[MonthlyWindow, ...]:
    """Count persistent windows in O(N), retaining missing-data bounds.

    Duration rounds upward to whole cadence blocks; no interpolation or
    independent-sample probability is used. Windows may cross months/years.
    Missing blocks never earn working credit. Limits are tested on both Hs and
    Tp at every block. No coverage beyond the supplied record is inferred.
    """
    limit.validate()
    _finite("duration", duration_h, positive=True)
    step = _cadence(cadence_h)
    blocks = int(
        (Decimal(str(duration_h)) / Decimal(str(cadence_h))).to_integral_value(
            rounding=ROUND_CEILING
        )
    )
    origin, states = _grid(samples, step, limit)
    missing, bad = [0], [0]
    for unknown, failed in states:
        missing.append(missing[-1] + unknown)
        bad.append(bad[-1] + failed)
    counts = [[0, 0, 0, 0] for _ in range(12)]
    for index in range(len(states)):
        month = (origin + index * step).month - 1
        end = index + blocks
        if end > len(states):
            counts[month][3] += 1
        elif missing[end] > missing[index]:
            counts[month][2] += 1
        else:
            counts[month][0] += 1
            counts[month][1] += int(bad[end] == bad[index])
    results = []
    for month, (complete, good, unknown, censored) in enumerate(counts, 1):
        total = complete + unknown
        results.append(
            MonthlyWindow(
                month,
                complete,
                good,
                unknown,
                censored,
                good / complete if complete else None,
                good / total if total else None,
                (good + unknown) / total if total else None,
            )
        )
    return tuple(results)


@dataclass(frozen=True)
class HullGeometry:
    """Fixed box aligned with wave travel; bottom elevation above mean sea (m).

    Negative bottom elevation is immersed. Positive elevation is a still-water
    gap at mean tide. A free-floating hull's tide-following motion is excluded.
    """

    length_m: float
    breadth_m: float
    depth_m: float
    bottom_elevation_m: float


@dataclass(frozen=True)
class LegGeometry:
    """One equivalent vertical cylinder; three identical legs are screened.

    Diameter and coefficients are caller assumptions. Lattice shielding,
    individual chords/braces, inclination and member dynamics are unresolved.
    """

    diameter_m: float
    drag_coefficient: float
    inertia_coefficient: float


@dataclass(frozen=True)
class SwellLoads:
    """Unfactored force amplitudes (kN); unmodelled hull contact returns None.

    The sum of three leg peaks is an arithmetic upper bound, not a simultaneous
    global action. Leg spacing, wave phase and leg/hull phase are not supplied.
    Hull vertical amplitude is dynamic pressure, separate from static buoyancy.
    """

    water_depth_m: float
    draft_m: float
    still_water_gap_m: float
    buoyancy_kn: float
    wavenumber_m_inv: float
    leg_drag_amplitude_kn: float
    leg_inertia_amplitude_kn: float
    leg_peak_kn: float
    three_leg_sum_of_peaks_kn: float
    hull_horizontal_amplitude_kn: float | None
    hull_vertical_amplitude_kn: float | None
    hull_status: str


def _wavenumber(omega: float, depth: float, gravity: float) -> float:
    # Dimensionless dispersion: x*tanh(x)=omega²*h/g. Monotone bisection.
    target = omega**2 * depth / gravity
    low, high = 0.0, max(1.0, target + 1.0)
    for _ in range(100):
        middle = (low + high) / 2
        if middle * math.tanh(middle) < target:
            low = middle
        else:
            high = middle
    return (low + high) / (2 * depth)


def _decaying_ratio(
    k: float, y: float, depth: float, *, hyperbolic_sine: bool
) -> float:
    # cosh(ky)/cosh(kh) or sinh(ky)/cosh(kh), avoiding overflow in deep water.
    upper = math.exp(k * (y - depth))
    lower = math.exp(-k * (y + depth))
    return (upper - lower if hyperbolic_sine else upper + lower) / (
        1 + math.exp(-2 * k * depth)
    )


def _hull_forces(
    hull: HullGeometry,
    depth: float,
    draft: float,
    tide: float,
    amplitude: float,
    k: float,
    rho_g: float,
) -> tuple[float | None, float | None, str]:
    gap = hull.bottom_elevation_m - tide
    freeboard = hull.bottom_elevation_m + hull.depth_m - tide
    if freeboard <= amplitude:
        return None, None, "deck_wetting"
    if gap > amplitude:
        return 0.0, 0.0, "dry"
    if draft <= amplitude:
        return None, None, "intermittent_contact"
    y_bottom = depth - draft
    top = _decaying_ratio(k, depth, depth, hyperbolic_sine=True)
    bottom = _decaying_ratio(k, y_bottom, depth, hyperbolic_sine=True)
    end_face = rho_g * amplitude * hull.breadth_m * (top - bottom) / k
    phase = abs(math.sin(k * hull.length_m / 2))
    horizontal = 2 * end_face * phase
    pressure = (
        rho_g * amplitude * _decaying_ratio(k, y_bottom, depth, hyperbolic_sine=False)
    )
    vertical = pressure * hull.breadth_m * 2 * phase / k
    return horizontal / 1000, vertical / 1000, "immersed"


def _validate_geometry(hull: HullGeometry, leg: LegGeometry, mean_depth: float) -> None:
    for name in ("length_m", "breadth_m", "depth_m"):
        _finite(name, getattr(hull, name), positive=True)
    if (
        not math.isfinite(hull.bottom_elevation_m)
        or hull.bottom_elevation_m <= -mean_depth
    ):
        raise ValueError("hull bottom must be finite and above the seabed")
    _finite("leg diameter", leg.diameter_m, positive=True)
    _finite("drag coefficient", leg.drag_coefficient)
    _finite("inertia coefficient", leg.inertia_coefficient)


def swell_loads(
    *,
    wave_height_m: float,
    period_s: float,
    mean_depth_m: float,
    tide_level_m: float,
    hull: HullGeometry,
    leg: LegGeometry,
    density_kg_m3: float = 1025,
    gravity_m_s2: float = 9.81,
) -> SwellLoads:
    """Integrate regular-wave forces, with tide on a single mean-sea datum.

    H is an individual regular-wave height, not Hs. Water level excludes waves.
    The seabed and supported hull remain fixed; mean depth plus tide gives h.
    Velocity and acceleration are integrated over the still-water column.
    The exact peak combines quadrature drag and inertia, rather than summing
    their separate peaks. Hull forces integrate undisturbed incident pressure
    over opposite end faces and the bottom, excluding diffraction and motions.

    Density 1025 kg/m³ and gravity 9.81 m/s² are assumed physical inputs, not
    standard-prescribed values. Linear wave validity requires separate review;
    slamming, green water, surge, currents, wind, lattice loads and structural
    response are not calculated. No foundation or installation acceptance follows.
    """
    for name, value in (
        ("wave height", wave_height_m),
        ("period", period_s),
        ("mean depth", mean_depth_m),
        ("density", density_kg_m3),
        ("gravity", gravity_m_s2),
    ):
        _finite(name, value, positive=name != "wave height")
    if not math.isfinite(tide_level_m):
        raise ValueError("tide level must be finite")
    depth = mean_depth_m + tide_level_m
    _finite("instantaneous depth", depth, positive=True)
    if wave_height_m >= 2 * depth:
        raise ValueError("regular-wave trough reaches seabed")
    _validate_geometry(hull, leg, mean_depth_m)
    omega, amplitude = 2 * math.pi / period_s, wave_height_m / 2
    k = _wavenumber(omega, depth, gravity_m_s2)
    kh = k * depth
    inverse_sinh = 2 * math.exp(-kh) / (-math.expm1(-2 * kh))
    integral_u2 = (amplitude * omega) ** 2 * (
        depth * inverse_sinh**2 / 2 + 1 / (2 * k * math.tanh(kh))
    )
    drag = density_kg_m3 * leg.drag_coefficient * leg.diameter_m * integral_u2 / 2000
    inertia = (
        density_kg_m3
        * leg.inertia_coefficient
        * math.pi
        * leg.diameter_m**2
        / 4
        * amplitude
        * omega**2
        / k
        / 1000
    )
    peak = inertia if inertia >= 2 * drag else drag + inertia**2 / (4 * drag)
    draft = min(hull.depth_m, max(0.0, tide_level_m - hull.bottom_elevation_m))
    horizontal, vertical, status = _hull_forces(
        hull, depth, draft, tide_level_m, amplitude, k, density_kg_m3 * gravity_m_s2
    )
    if wave_height_m == 0:
        horizontal, vertical = 0.0, 0.0
    return SwellLoads(
        depth,
        draft,
        max(0.0, hull.bottom_elevation_m - tide_level_m),
        density_kg_m3 * gravity_m_s2 * hull.length_m * hull.breadth_m * draft / 1000,
        k,
        drag,
        inertia,
        peak,
        3 * peak,
        horizontal,
        vertical,
        status,
    )
