# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Closed-form long-wave limits of free-floating body motion RAOs.

As the wave frequency tends to zero, a free-floating body follows the water surface:

- heave tends to the vertical particle excursion at the surface, 1 per unit wave amplitude;
- surge and sway tend to the horizontal particle excursion at the surface, coth(kh) per unit
  amplitude in water of depth h, resolved on the heading (cos and sin), for a small body
  whose draft is small relative to the depth or with kh much smaller than one;
- pitch and roll tend to the wave slope k, resolved on the heading. Optional amplification
  by 1/|1 - (w/wn)^2| assumes an undamped oscillator driven only by the hydrostatic
  wave-slope moment. It omits inertial excitation, effective-slope reduction and sway-roll /
  surge-pitch coupling, all of the same order as that correction. Heave uses the pure limit;
- yaw uses the slender-body assumption: sections along x follow the local sway excursion,
  giving k coth(kh) |sin(beta) cos(beta)|, tending to |sin(beta) cos(beta)|/h at finite depth.
  A body symmetric in plan following an irrotational excursion field has zero long-wave
  yaw instead. The slender-body value is an approximate comparator only.

These are plausibility comparators of the ``closed-form`` class for low-frequency results of a
diffraction program. A result inside the tolerance is ``not_implausible``; it is not validated.
Headings are the direction of wave travel from the body's +x axis, in degrees.
"""
from __future__ import annotations

import math
from dataclasses import dataclass

__all__ = ["LongWaveCheck", "dynamic_amplification", "expected_rao", "judge", "wavenumber"]

_TRANSLATIONS = ("surge", "sway", "heave")
_ROTATIONS = ("roll", "pitch")


def wavenumber(omega: float, depth: float, gravity: float = 9.80665) -> float:
    """Wavenumber k (rad/m) from the linear dispersion relation w^2 = g k tanh(k h).

    ``depth`` may be ``math.inf`` for deep water. Solved by Newton iteration to machine precision.
    """
    if not (math.isfinite(omega) and omega > 0.0):
        raise ValueError(f"frequency must be finite and positive, got {omega!r}")
    if not (depth > 0.0) or math.isnan(depth):
        raise ValueError(f"water depth must be positive, got {depth!r}")
    if not (math.isfinite(gravity) and gravity > 0.0):
        raise ValueError(f"gravity must be finite and positive, got {gravity!r}")
    k_deep = omega * omega / gravity
    if math.isinf(depth):
        return k_deep
    # Solve x tanh(x) = w^2 h / g for x = k h.
    target = k_deep * depth
    x = math.sqrt(target) if target < 1.0 else target
    for _ in range(100):
        t = math.tanh(x)
        f = x * t - target
        df = t + x * (1.0 - t * t)
        step = f / df
        x -= step
        if abs(step) <= 1e-15 * max(1.0, x):
            break
    return x / depth


def dynamic_amplification(omega: float, natural_frequency: float) -> float:
    """Quasi-static dynamic amplification 1/|1 - (w/wn)^2| of an undamped oscillator."""
    r2 = (omega / natural_frequency) ** 2
    if abs(1.0 - r2) < 1e-12:
        raise ValueError("frequency equals the natural frequency; amplification is unbounded")
    return 1.0 / abs(1.0 - r2)


def expected_rao(mode: str, omega: float, heading_deg: float, depth: float,
                 gravity: float = 9.80665, natural_frequency: float | None = None) -> float:
    """Long-wave limit of the RAO magnitude for ``mode`` (per metre of wave amplitude).

    Translations are dimensionless (m/m); rotations are in rad/m. Providing
    ``natural_frequency`` (rad/s) opts roll/pitch into the hydrostatic-moment-only
    oscillator approximation described above. Otherwise the pure slope limit is returned.
    """
    beta = math.radians(heading_deg)
    k = wavenumber(omega, depth, gravity)
    coth = 1.0 if math.isinf(depth) else 1.0 / math.tanh(k * depth)
    if mode == "heave":
        return 1.0
    if mode == "surge":
        return coth * abs(math.cos(beta))
    if mode == "sway":
        return coth * abs(math.sin(beta))
    if mode in _ROTATIONS:
        factor = abs(math.sin(beta)) if mode == "roll" else abs(math.cos(beta))
        amplification = (
            1.0 if natural_frequency is None
            else dynamic_amplification(omega, natural_frequency)
        )
        return k * factor * amplification
    if mode == "yaw":
        return k * coth * abs(math.sin(beta) * math.cos(beta))
    raise ValueError(f"unknown mode {mode!r}")


@dataclass(frozen=True)
class LongWaveCheck:
    """Comparator result; an inapplicable or zero comparison has no relative difference."""

    observed: float
    expected: float
    tolerance: float
    relative_difference: float | None
    verdict: str  # "not_applicable", "not_implausible" or "implausible"
    absolute_tolerance: float | None = None


def judge(observed: float, expected: float, tolerance: float, *,
          body_length: float, omega: float, depth: float,
          minimum_wavelength_ratio: float = 10.0,
          absolute_tolerance: float | None = None) -> LongWaveCheck:
    """Compare only when wavelength exceeds the stated multiple of body length.

    The default screening criterion is wavelength > 10 body lengths (kL < 2*pi/10).
    This is an explicit applicability screen, not an error bound; callers may fix a
    stricter ratio in advance. ``body_length`` and ``depth`` are metres, ``omega``
    is rad/s. A zero comparator requires ``absolute_tolerance`` in the RAO's units;
    its relative difference is undefined and is returned as None.
    """
    inputs = (observed, expected, tolerance, body_length, minimum_wavelength_ratio)
    if not all(math.isfinite(value) for value in inputs):
        raise ValueError("comparison inputs must be finite")
    if tolerance < 0 or body_length <= 0 or minimum_wavelength_ratio <= 1:
        raise ValueError("tolerance must be nonnegative, length positive and wavelength ratio > 1")
    if absolute_tolerance is not None and (
        not math.isfinite(absolute_tolerance) or absolute_tolerance < 0
    ):
        raise ValueError("absolute tolerance must be finite and nonnegative")
    wavelength = 2 * math.pi / wavenumber(omega, depth)
    if wavelength <= minimum_wavelength_ratio * body_length:
        return LongWaveCheck(observed, expected, tolerance, None, "not_applicable", absolute_tolerance)
    if expected == 0.0:
        if absolute_tolerance is None:
            raise ValueError("a zero comparator requires an absolute tolerance")
        verdict = "not_implausible" if abs(observed) <= absolute_tolerance else "implausible"
        return LongWaveCheck(observed, expected, tolerance, None, verdict, absolute_tolerance)
    rel = (observed - expected) / expected
    verdict = "not_implausible" if abs(rel) <= tolerance + 1e-12 else "implausible"
    return LongWaveCheck(observed, expected, tolerance, rel, verdict)
