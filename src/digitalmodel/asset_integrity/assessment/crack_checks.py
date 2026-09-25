# ABOUTME: Consistency checks for FE-based crack-like-flaw assessments: K vs sigma_ref,
# ABOUTME: small-scale yielding, shakedown, LEFM-growth validity, mixed-mode Keq (#2157).
"""Consistency checks that the arithmetic of an assessment does not exercise.

Each check returns a small record with its number and pass/fail. None of them tunes an
input. Units: MPa, mm, MPa*sqrt(m).

- :func:`sigma_ref_consistency`: the crack-opening stress implied by K,
  ``sigma = K / (Y sqrt(pi a))`` over a stated range of Y, compared with the reference
  stress used for Lr. A ratio interval outside :data:`SIGMA_RATIO_BAND` means K and
  sigma_ref describe different stress fields.
- :func:`ssy_check`: Irwin plastic-zone size against the remaining ligament, with a
  caller-stated limit.
- :func:`shakedown_check`: elastic stress range against ``2 sigma_y``.
- :func:`growth_validity`: Lr > 1 at any state means the net section yields on the
  stated sigma_ref basis. A linear-elastic (Paris) growth life is then outside its
  validity, and is marked CONDITIONAL.
- :func:`keq_energy`: the energy-equivalent mixed-mode K,
  ``sqrt(KI^2 + KII^2 + KIII^2/(1 - nu))``.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Iterable, Tuple

#: Tolerance band on sigma_ref / sigma_implied, fixed in the #2157 plan before any run.
SIGMA_RATIO_BAND: Tuple[float, float] = (0.5, 2.0)
#: Default geometry-factor range: embedded (2/pi) to edge crack (1.12).
DEFAULT_Y_RANGE: Tuple[float, float] = (2.0 / math.pi, 1.12)


def mpa_sqrt_mm_to_mpa_sqrt_m(k_mpa_sqrt_mm: float) -> float:
    """Convert K from MPa*sqrt(mm) to MPa*sqrt(m)."""
    return k_mpa_sqrt_mm / math.sqrt(1000.0)


def implied_opening_stress(k_mpa_sqrt_m: float, a_mm: float, y: float) -> float:
    """Opening stress implied by K for crack depth ``a_mm`` and geometry factor ``y``."""
    if a_mm <= 0 or y <= 0:
        raise ValueError("need a_mm > 0 and y > 0.")
    return k_mpa_sqrt_m / (y * math.sqrt(math.pi * a_mm / 1000.0))


@dataclass(frozen=True)
class RatioCheck:
    passed: bool
    ratio_min: float
    ratio_max: float
    band: Tuple[float, float]
    y_range: Tuple[float, float]


def sigma_ref_consistency(
    sigma_ref_mpa: float,
    k_mpa_sqrt_m: float,
    a_mm: float,
    y_range: Tuple[float, float] = DEFAULT_Y_RANGE,
    band: Tuple[float, float] = SIGMA_RATIO_BAND,
) -> RatioCheck:
    """Compare sigma_ref with the opening stress K implies over ``y_range``.

    The ratio ``sigma_ref / sigma_implied`` spans an interval as Y varies. The check
    passes when that interval intersects ``band``.
    """
    lo_y, hi_y = sorted(y_range)
    r_a = sigma_ref_mpa / implied_opening_stress(k_mpa_sqrt_m, a_mm, lo_y)
    r_b = sigma_ref_mpa / implied_opening_stress(k_mpa_sqrt_m, a_mm, hi_y)
    r_min, r_max = min(r_a, r_b), max(r_a, r_b)
    passed = r_max >= band[0] and r_min <= band[1]
    return RatioCheck(passed, r_min, r_max, tuple(band), (lo_y, hi_y))


def irwin_plastic_zone_mm(
    k_mpa_sqrt_m: float, sigma_y_mpa: float, condition: str = "plane_stress"
) -> float:
    """Irwin plastic-zone size ``(1/2pi)(K/sigma_y)^2``, divided by 3 for plane strain."""
    rp = (1.0 / (2.0 * math.pi)) * (k_mpa_sqrt_m / sigma_y_mpa) ** 2 * 1000.0
    if condition == "plane_stress":
        return rp
    if condition == "plane_strain":
        return rp / 3.0
    raise ValueError("condition must be 'plane_stress' or 'plane_strain'.")


@dataclass(frozen=True)
class SimpleCheck:
    passed: bool
    ratio: float


def ssy_check(
    k_mpa_sqrt_m: float, sigma_y_mpa: float, *, ligament_mm: float, max_ratio: float
) -> SimpleCheck:
    """Plastic-zone size against the ligament. The limit ``max_ratio`` has no default."""
    ratio = irwin_plastic_zone_mm(k_mpa_sqrt_m, sigma_y_mpa) / ligament_mm
    return SimpleCheck(ratio <= max_ratio, ratio)


def shakedown_check(*, elastic_range_mpa: float, sigma_y_mpa: float) -> SimpleCheck:
    """Elastic stress range against ``2 sigma_y``; passes at or below 1."""
    ratio = elastic_range_mpa / (2.0 * sigma_y_mpa)
    return SimpleCheck(ratio <= 1.0, ratio)


@dataclass(frozen=True)
class ValidityResult:
    status: str  # "VALID" | "CONDITIONAL"
    reason: str
    lr_max_seen: float


def growth_validity(lr_values: Iterable[float]) -> ValidityResult:
    """Mark a linear-elastic growth life CONDITIONAL when any state has Lr > 1."""
    vals = list(lr_values)
    peak = max(vals) if vals else 0.0
    if peak > 1.0:
        return ValidityResult(
            "CONDITIONAL",
            f"net-section yield on the stated sigma_ref basis (Lr = {peak:.3f} > 1); "
            "linear-elastic growth is outside its validity",
            peak,
        )
    return ValidityResult("VALID", "all states have Lr <= 1", peak)


def keq_energy(k_i: float, k_ii: float, k_iii: float, nu: float) -> float:
    """Energy-equivalent mixed-mode K: ``sqrt(KI^2 + KII^2 + KIII^2 / (1 - nu))``."""
    if not 0.0 <= nu < 0.5:
        raise ValueError("Poisson's ratio must satisfy 0 <= nu < 0.5.")
    return math.sqrt(k_i**2 + k_ii**2 + k_iii**2 / (1.0 - nu))
