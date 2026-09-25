# ABOUTME: Edition-named FAD curves, Lr cut-off rules and the load margin to the
# ABOUTME: failure-assessment envelope along the primary-load path (#2157 P1).
"""Failure assessment diagram curves and envelope margin.

Curves, each named by the edition it represents:

- :func:`api579_2016_level2`:
  ``Kr = (1 - 0.14 Lr^2)(0.3 + 0.7 exp(-0.65 Lr^6))`` for ``Lr <= Lr_max``.
  Basis: a secondary source (*Materials* 19 (2026) 465, PMC12897954) that cites
  API 579-1/ASME FFS-1:2016 and equates its Level 2 curve with the BS 7910 Level 2A
  generic curve. The paper typesets the curve as a division, which gives Kr > 1 at
  Lr = 1 and is a typesetting error; the product form is the physical curve. The
  primary API text was not read (FileOpen DRM). This form is also the original R6
  Option 1 curve.
- :func:`bs7910_2013_option1`: the ``(1 + 0.5 Lr^2)^-0.5`` form, confirmed against the
  licensed BS 7910:2013 text. It delegates to
  :func:`digitalmodel.asset_integrity.assessment.crack_fad.fad_curve_option1`, so
  there is a single implementation of that curve.

The two curve forms differ numerically. An "API Level 2 == BS 7910 Option 1" claim is
wrong for the 2013 edition.

:func:`envelope_margin` scales only the primary terms (Lr and the primary part of Kr)
and holds any secondary contribution to Kr constant. With secondary stress present,
the proportional-load path therefore does not pass through the origin.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Callable, Optional

from digitalmodel.asset_integrity.assessment.crack_fad import fad_curve_option1


def api579_2016_level2(lr: float, lr_cut: float) -> float:
    """Kr on the API 579-1:2016 Level 2 curve; 0.0 beyond the cut-off ``lr_cut``."""
    if lr < 0:
        raise ValueError("Lr must be >= 0.")
    if lr > lr_cut:
        return 0.0
    return (1.0 - 0.14 * lr**2) * (0.3 + 0.7 * math.exp(-0.65 * lr**6))


def bs7910_2013_option1(
    lr: float, sigma_y_mpa: float, sigma_u_mpa: float, e_mpa: float = 207_000.0
) -> float:
    """Kr on the BS 7910:2013 Option 1 curve (delegates to ``crack_fad``)."""
    return fad_curve_option1(lr, sigma_y_mpa, sigma_u_mpa, e_mpa)


@dataclass(frozen=True)
class LrMax:
    """Plastic-collapse cut-off with the rule and basis that produced it."""

    value: float
    rule: str
    basis: str


def lr_max(
    rule: str,
    *,
    sigma_y_mpa: Optional[float] = None,
    sigma_u_mpa: Optional[float] = None,
    value: Optional[float] = None,
    basis: Optional[str] = None,
) -> LrMax:
    """Lr cut-off by an explicit rule.

    ``rule="flow"``: ``(sigma_y + sigma_u) / (2 sigma_y)``, which needs both strengths.
    ``rule="fixed"``: a stated value, which needs a non-empty ``basis`` string. A
    fixed cut-off with no stated basis is refused.
    """
    if rule == "flow":
        if sigma_y_mpa is None or sigma_u_mpa is None:
            raise ValueError("flow rule needs sigma_y_mpa and sigma_u_mpa.")
        if sigma_y_mpa <= 0 or sigma_u_mpa < sigma_y_mpa:
            raise ValueError("need 0 < sigma_y <= sigma_u.")
        v = (sigma_y_mpa + sigma_u_mpa) / (2.0 * sigma_y_mpa)
        return LrMax(
            value=v,
            rule="flow",
            basis=f"(sigma_y + sigma_u)/(2 sigma_y), sigma_y={sigma_y_mpa} MPa, "
            f"sigma_u={sigma_u_mpa} MPa",
        )
    if rule == "fixed":
        if value is None or value <= 0:
            raise ValueError("fixed rule needs a positive value.")
        if basis is None or not basis.strip():
            raise ValueError("fixed rule needs a stated basis.")
        return LrMax(value=float(value), rule="fixed", basis=basis.strip())
    raise ValueError(f"unknown Lr_max rule {rule!r}; use 'flow' or 'fixed'.")


@dataclass(frozen=True)
class MarginResult:
    """Load factor F to the envelope and the contact point."""

    factor: float
    mode: str  # "curve" | "cutoff"
    contact_lr: float
    contact_kr: float


def envelope_margin(
    lr_primary: float,
    kr_primary: float,
    *,
    curve: Callable[[float], float],
    lr_cut: float,
    kr_secondary: float = 0.0,
    scan_steps: int = 20_000,
) -> MarginResult:
    """Smallest load factor F at which ``(F Lr_p, F Kr_p + Kr_s)`` meets the envelope.

    Only primary terms scale. ``kr_secondary`` is held constant, because secondary
    stress does not scale with the applied load. F < 1 means the point is already
    outside the envelope. F = 0 means the secondary term alone meets it. The contact is
    found by scanning F in ``(0, lr_cut / lr_primary]`` and bisecting the first sign
    change to 1e-12. If no curve contact occurs before the cut-off, the cut-off governs.
    """
    if lr_primary <= 0 or kr_primary < 0 or kr_secondary < 0:
        raise ValueError("need lr_primary > 0, kr_primary >= 0, kr_secondary >= 0.")
    if kr_secondary >= curve(0.0):
        return MarginResult(0.0, "curve", 0.0, kr_secondary)

    f_cut = lr_cut / lr_primary

    def g(f: float) -> float:  # > 0 inside, <= 0 on/outside
        return curve(f * lr_primary) - (f * kr_primary + kr_secondary)

    prev = 0.0
    for i in range(1, scan_steps + 1):
        f = f_cut * i / scan_steps
        if g(f) <= 0.0:
            lo, hi = prev, f
            for _ in range(200):
                mid = 0.5 * (lo + hi)
                if g(mid) > 0.0:
                    lo = mid
                else:
                    hi = mid
                if hi - lo < 1e-12:
                    break
            fc = 0.5 * (lo + hi)
            return MarginResult(
                fc, "curve", fc * lr_primary, fc * kr_primary + kr_secondary
            )
        prev = f
    return MarginResult(f_cut, "cutoff", lr_cut, f_cut * kr_primary + kr_secondary)
