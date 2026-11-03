# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Planar MBES patch-test geometry, with metres, seconds and degrees as units.

Basis: NOAA, Field Procedures Manual (2020), section 1.5.6.2, pp. 26–28:
https://nauticalcharts.noaa.gov/publications/docs/standards-and-requirements/fpm/field_procedures_manual_2020.pdf

The tangent equations below are derived planar models of those line geometries,
not equations quoted from the manual or a ray-tracing model. See the derivations
and sign conventions in docs/domains/subsea/survey_qa/patch_test_example.md.
All offsets are estimated biases in these conventions, not instrument correctors.
No empirical coefficients, automatic feature matching or accuracy limits are used.
"""

from dataclasses import dataclass
from math import atan, degrees, fsum, isfinite, radians, sqrt, tan
from typing import Iterable


def _finite(value: float, name: str) -> float:
    """Normalize a scalar and refuse nonfinite observations."""
    try:
        result = float(value)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must be finite") from exc
    if not isfinite(result):
        raise ValueError(f"{name} must be finite")
    return result


def _values(values: Iterable[float], name: str) -> tuple[float, ...]:
    """Copy a sequence so later caller mutations cannot invalidate geometry."""
    return tuple(_finite(value, name) for value in values)


def _positive(value: float, name: str) -> float:
    value = _finite(value, name)
    # Zero is the geometric boundary for a positive length or forward speed.
    if value <= 0:
        raise ValueError(f"{name} must be positive")
    return value


def _roll_tangent(roll_deg: float) -> float:
    angle = _finite(roll_deg, "roll_deg")
    # Principal tangent branch: +/-90 degrees are mathematical singularities,
    # not an operational acceptance limit (planar derivation in the module doc).
    if not -90 < angle < 90:
        raise ValueError("roll_deg must lie strictly between -90 and 90 degrees")
    return tan(radians(angle))


@dataclass(frozen=True)
class ReciprocalSwaths:
    """Matched flat-bottom profiles, already expressed in one global frame.

    Across-track coordinates are shared by both runs, NOT each run's starboard
    coordinates. Depths are positive down, relative to the same datum. First and
    second runs must have reciprocal headings and equal speeds. Those acquisition
    conditions, flatness and environmental corrections are caller responsibilities.
    """

    across_track_m: tuple[float, ...]
    forward_depth_m: tuple[float, ...]
    reverse_depth_m: tuple[float, ...]

    def __post_init__(self) -> None:
        for name in ("across_track_m", "forward_depth_m", "reverse_depth_m"):
            object.__setattr__(self, name, _values(getattr(self, name), name))
        n = len(self.across_track_m)
        if len(self.forward_depth_m) != n or len(self.reverse_depth_m) != n:
            raise ValueError("swath coordinate and depth lengths must match")
        # A slope plus an intercept requires two distinct coordinates;
        # this is a rank requirement, not an empirical sample-count threshold.
        if len(set(self.across_track_m)) < 2:
            raise ValueError(
                "roll fit needs at least two distinct across-track coordinates"
            )
        for depth in (*self.forward_depth_m, *self.reverse_depth_m):
            _positive(depth, "depth_m")


@dataclass(frozen=True)
class RollEstimate:
    """Angular bias, nuisance datum difference and depth-difference fit error.

    fit_rmse_m measures dz minus the fitted line, not absolute survey accuracy.
    A two-point fit has no residual degrees of freedom; no uncertainty is claimed.
    """

    roll_deg: float
    vertical_offset_m: float
    fit_rmse_m: float
    sample_count: int


def estimate_roll(swaths: ReciprocalSwaths) -> RollEstimate:
    """Fit dz = a + 2*y*tan(roll) by ordinary least squares with an intercept."""
    y = swaths.across_track_m
    dz = tuple(
        a - b
        for a, b in zip(swaths.forward_depth_m, swaths.reverse_depth_m, strict=True)
    )
    n = len(y)
    mean_y, mean_dz = fsum(y) / n, fsum(dz) / n
    centered_y = tuple(value - mean_y for value in y)
    # OLS normal equations: NIST/SEMATECH e-Handbook, section 4.4.3.1,
    # "Least Squares", online edition accessed 2026-10-19:
    # https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd431.htm
    denominator = fsum(value * value for value in centered_y)
    if denominator == 0 or not isfinite(denominator):
        raise ValueError("across-track spread is not numerically resolvable")
    slope = fsum(a * (b - mean_dz) for a, b in zip(centered_y, dz)) / denominator
    intercept = mean_dz - slope * mean_y
    residuals = tuple(b - intercept - slope * a for a, b in zip(y, dz))
    rmse = sqrt(fsum(value * value for value in residuals) / n)
    # Factor 2: opposing apparent slopes, NOAA FPM (2020), 1.5.6.2 roll geometry;
    # dz=y*tan(r)-(-y*tan(r))=2*y*tan(r), as derived in the worked example.
    return RollEstimate(
        degrees(atan(_finite(slope, "fitted slope") / 2)),
        _finite(intercept, "fitted vertical offset"),
        _finite(rmse, "fit RMSE"),
        n,
    )


def synthetic_roll_swaths(
    across_track_m: Iterable[float],
    *,
    depth_m: float,
    roll_deg: float,
    forward_noise_m: Iterable[float] | None = None,
    reverse_noise_m: Iterable[float] | None = None,
) -> ReciprocalSwaths:
    """Generate z_forward=d+y*tan(r), z_reverse=d-y*tan(r) on a flat bottom.

    NOAA FPM (2020), 1.5.6.2 motivates the reciprocal flat-bottom geometry.
    Supplied noise is additive in metres; None means exactly zero noise. The
    generator is deterministic and makes no stochastic or survey-quality claim.
    """
    y = _values(across_track_m, "across_track_m")
    depth = _positive(depth_m, "depth_m")
    slope = _roll_tangent(roll_deg)

    def noise(values: Iterable[float] | None) -> tuple[float, ...]:
        result = (0.0,) * len(y) if values is None else _values(values, "noise_m")
        if len(result) != len(y):
            raise ValueError("noise and coordinate lengths must match")
        return result

    forward = tuple(depth + a * slope + b for a, b in zip(y, noise(forward_noise_m)))
    reverse = tuple(depth - a * slope + b for a, b in zip(y, noise(reverse_noise_m)))
    return ReciprocalSwaths(y, forward, reverse)


def correct_roll(swaths: ReciprocalSwaths, roll_deg: float) -> ReciprocalSwaths:
    """Subtract/add y*tan(r) in the declared profile model; retain datum offsets."""
    slope = _roll_tangent(roll_deg)
    forward = tuple(
        z - y * slope
        for y, z in zip(swaths.across_track_m, swaths.forward_depth_m, strict=True)
    )
    reverse = tuple(
        z + y * slope
        for y, z in zip(swaths.across_track_m, swaths.reverse_depth_m, strict=True)
    )
    return ReciprocalSwaths(swaths.across_track_m, forward, reverse)


def estimate_latency(
    along_track_shift_m: float,
    *,
    first_speed_m_s: float,
    second_speed_m_s: float,
) -> float:
    """Estimate seconds from x_first-x_second = (v_first-v_second)*latency.

    NOAA FPM (2020), 1.5.6.2: same heading, different speeds, matching slope or
    feature. Both speeds are positive in the common along-track direction.
    The shift is a caller-measured matched-feature displacement, not a depth gap.
    """
    shift = _finite(along_track_shift_m, "along_track_shift_m")
    first = _positive(first_speed_m_s, "first_speed_m_s")
    second = _positive(second_speed_m_s, "second_speed_m_s")
    if first == second:
        raise ValueError("latency needs different speeds")
    return _finite(shift / (first - second), "latency_s")


def estimate_pitch(along_track_shift_m: float, *, depth_m: float) -> float:
    """Estimate degrees from reciprocal nadir shift = 2*depth*tan(pitch).

    NOAA FPM (2020), 1.5.6.2 reciprocal slope geometry; depth is the common local
    positive-down depth at the matched feature. Latency must already be resolved.
    The factor 2 sums opposing nadir displacements (worked-example derivation).
    """
    shift = _finite(along_track_shift_m, "along_track_shift_m")
    depth = _positive(depth_m, "depth_m")
    return degrees(atan(shift / depth / 2))


def estimate_yaw(
    along_track_shift_m: float,
    *,
    first_across_track_m: float,
    second_across_track_m: float,
) -> float:
    """Estimate degrees from dx=(y_second-y_first)*tan(yaw).

    NOAA FPM (2020), 1.5.6.2 adjacent parallel lines at the same speed and heading.
    y values are signed reported feature coordinates relative to each line, in
    axes pointing to the same side; dx=x_first-x_second. Under the planar model
    each feature's along-track error is -y*tan(yaw). Pitch/roll are resolved first.
    """
    shift = _finite(along_track_shift_m, "along_track_shift_m")
    first = _finite(first_across_track_m, "first_across_track_m")
    second = _finite(second_across_track_m, "second_across_track_m")
    separation = _finite(second - first, "across-track separation")
    if separation == 0:
        raise ValueError("yaw needs distinct across-track feature coordinates")
    return degrees(atan(_finite(shift / separation, "yaw tangent")))
