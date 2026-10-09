"""Closed-form long-wave limits of free-floating body motion RAOs.

As the wave frequency tends to zero, a free-floating body follows the water surface:

- heave tends to the vertical particle excursion at the surface, 1 per unit wave amplitude;
- surge and sway tend to the horizontal particle excursion at the surface, coth(kh) per unit
  amplitude in water of depth h, resolved on the heading (cos and sin);
- pitch and roll tend to the wave slope k, resolved on the heading, amplified quasi-statically
  by 1/|1 - (w/wn)^2| as the frequency approaches the mode's natural frequency wn;
- yaw is approximated by a rigid body following the horizontal particle-excursion field,
  k coth(kh) |sin(beta) cos(beta)|. This neglects the difference between the hull's yaw inertia
  and that of the displaced water, so it is an approximate comparator only.

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

    Translations are dimensionless (m/m); rotations are in rad/m. ``natural_frequency`` (rad/s)
    is required for roll and pitch.
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
        if natural_frequency is None:
            raise ValueError(f"{mode} needs the mode's natural frequency for the amplification")
        factor = abs(math.sin(beta)) if mode == "roll" else abs(math.cos(beta))
        return k * factor * dynamic_amplification(omega, natural_frequency)
    if mode == "yaw":
        return k * coth * abs(math.sin(beta) * math.cos(beta))
    raise ValueError(f"unknown mode {mode!r}")


@dataclass(frozen=True)
class LongWaveCheck:
    observed: float
    expected: float
    tolerance: float
    relative_difference: float
    verdict: str  # "not_implausible" or "implausible"


def judge(observed: float, expected: float, tolerance: float) -> LongWaveCheck:
    """Compare an observed RAO with its long-wave limit at a relative tolerance fixed in advance."""
    if expected == 0.0 or not math.isfinite(expected):
        raise ValueError("the expected value must be finite and non-zero for a relative check")
    rel = (observed - expected) / expected
    verdict = "not_implausible" if abs(rel) <= tolerance + 1e-12 else "implausible"
    return LongWaveCheck(observed, expected, tolerance, rel, verdict)
