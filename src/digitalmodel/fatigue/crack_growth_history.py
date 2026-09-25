# ABOUTME: Fatigue crack growth from a DeltaK(a) history: Paris law with explicit
# ABOUTME: threshold arrest, Simpson quadrature, sensitivities, history checks (#2157).
"""Fatigue crack growth to a limit depth from a DeltaK(a) model.

Differences from :func:`digitalmodel.fatigue.crack_growth.paris_law_life`, which this
module does not call:

- Life is ``N = integral of da / (A DeltaK(a)^m)``, evaluated by composite Simpson
  quadrature (error below 1e-7 relative on smooth DeltaK(a)), not forward Euler.
- A threshold is explicit. If DeltaK falls to or below the effective threshold
  anywhere in ``[a0, af]``, including a crossing inside the interval, the result is
  ``ARRESTED`` with the arrest depth. Detection is exact for a
  :class:`TabulatedDeltaK`. For a general callable it uses sampling plus refinement of
  every sampled local minimum; a dip narrower than the sample spacing that leaves no
  trace in the samples is the stated limit (see ``_first_arrest``). A non-positive
  DeltaK without a threshold raises rather than truncating silently.
- The threshold temperature rule must be stated: ``"none"`` or ``"e_ratio"``
  (threshold scaled by ``E_T / E_ref``). There is no default.
- Growth-law constants are user inputs with a basis string. No standard-derived
  constant ships in this module; :func:`convert_coefficient_n_mm_to_mpa_sqrt_m` only
  converts units.

Units: crack depth in mm, da/dN in mm/cycle, DeltaK in MPa*sqrt(m).
"""

from __future__ import annotations

import math
from bisect import bisect_right
from dataclasses import dataclass, replace
from typing import Callable, List, Optional, Sequence, Tuple

DeltaKModel = Callable[[float], float]


def _require_positive(name: str, value: Optional[float]) -> float:
    """Return ``value`` if it is a finite number > 0, else raise ValueError."""
    if value is None or not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and > 0 (got {value!r}).")
    return float(value)


@dataclass(frozen=True)
class GrowthLaw:
    """Paris law ``da/dN = A DeltaK^m`` (mm/cycle, MPa*sqrt(m)) with its basis."""

    A: float
    m: float
    basis: str
    citation: Optional[str] = None

    def __post_init__(self) -> None:
        if not self.A > 0 or not self.m > 0:
            raise ValueError("growth-law constants must satisfy A > 0 and m > 0.")
        if not self.basis or not self.basis.strip():
            raise ValueError("a growth law needs a stated basis.")

    def rate(self, dk: float) -> float:
        return self.A * dk**self.m

    def with_e_ratio(self, e_ref_gpa: float, e_t_gpa: float) -> "GrowthLaw":
        """Temperature-correct A by ``(E_ref / E_T)^m``; the basis records it."""
        _require_positive("e_ref_gpa", e_ref_gpa)
        _require_positive("e_t_gpa", e_t_gpa)
        factor = (e_ref_gpa / e_t_gpa) ** self.m
        return replace(
            self,
            A=self.A * factor,
            basis=f"{self.basis}; E-ratio ({e_ref_gpa}/{e_t_gpa})^{self.m} "
            f"= {factor:.6g} applied",
        )


def convert_coefficient_n_mm_to_mpa_sqrt_m(a_n_mm: float, m: float) -> float:
    """Convert A from DeltaK in N/mm^1.5 to DeltaK in MPa*sqrt(m) (da/dN in mm/cycle)."""
    return a_n_mm * 1000.0 ** (m / 2.0)


@dataclass(frozen=True)
class Threshold:
    """Threshold DeltaK_th with an explicit temperature rule (no default)."""

    dk_th: float
    temperature_rule: str
    e_ref_gpa: Optional[float] = None
    e_t_gpa: Optional[float] = None

    def __post_init__(self) -> None:
        if self.dk_th <= 0:
            raise ValueError("dk_th must be > 0.")
        if self.temperature_rule not in ("none", "e_ratio"):
            raise ValueError("temperature_rule must be 'none' or 'e_ratio'.")
        if self.temperature_rule == "e_ratio":
            if self.e_ref_gpa is None or self.e_t_gpa is None:
                raise ValueError("the 'e_ratio' rule needs e_ref_gpa and e_t_gpa.")
            _require_positive("e_ref_gpa", self.e_ref_gpa)
            _require_positive("e_t_gpa", self.e_t_gpa)

    @property
    def effective(self) -> float:
        if self.temperature_rule == "e_ratio":
            return self.dk_th * self.e_t_gpa / self.e_ref_gpa
        return self.dk_th


@dataclass(frozen=True)
class LifeResult:
    status: str  # "GROWS" | "ARRESTED"
    cycles: Optional[float]
    a_arrest_mm: Optional[float] = None


def _scan_points(a0: float, af: float, n: int) -> List[float]:
    return [a0 + (af - a0) * i / n for i in range(n + 1)]


def _bisect_crossing(
    dk_of_a: DeltaKModel, lo: float, hi: float, dk_eff: float
) -> float:
    """First depth in ``(lo, hi]`` with dK <= dk_eff, given dK(lo) > dk_eff >= dK(hi)."""
    for _ in range(200):
        mid = 0.5 * (lo + hi)
        if dk_of_a(mid) <= dk_eff:
            hi = mid
        else:
            lo = mid
        if hi - lo < 1e-12:
            break
    return hi


def _golden_min(
    f: DeltaKModel, lo: float, hi: float, tol: float = 1e-12
) -> Tuple[float, float]:
    """Golden-section search for a minimum of ``f`` on ``[lo, hi]``; returns (x, f(x))."""
    g = (math.sqrt(5.0) - 1.0) / 2.0
    c, d = hi - g * (hi - lo), lo + g * (hi - lo)
    fc, fd = f(c), f(d)
    while hi - lo > tol:
        if fc <= fd:
            hi, d, fd = d, c, fc
            c = hi - g * (hi - lo)
            fc = f(c)
        else:
            lo, c, fc = c, d, fd
            d = lo + g * (hi - lo)
            fd = f(d)
    x = 0.5 * (lo + hi)
    return x, f(x)


def _first_arrest_tabulated(
    t: "TabulatedDeltaK", a0: float, af: float, dk_eff: float
) -> Optional[float]:
    """Exact first crossing for a piecewise-linear model: minima lie at its nodes."""
    pts = [a0] + [a for a in t.a if a0 < a < af] + [af]
    if t(pts[0]) <= dk_eff:
        return pts[0]
    for lo, hi in zip(pts, pts[1:]):
        k_lo, k_hi = t(lo), t(hi)
        if k_hi <= dk_eff:
            return lo + (k_lo - dk_eff) / (k_lo - k_hi) * (hi - lo)
    return None


def _first_arrest(
    dk_of_a: DeltaKModel, a0: float, af: float, dk_eff: float, n_scan: int
) -> Optional[float]:
    """First depth where dK <= dk_eff, or None.

    For a :class:`TabulatedDeltaK` the answer is exact, because every minimum of a
    piecewise-linear model lies at a node. For a general callable the search samples
    ``n_scan + 1`` equally spaced depths. It also checks every sampled local minimum
    (a sample no higher than both neighbours) with a golden-section search, so a dip
    between samples is found if the samples show the descent into it. A sub-threshold
    dip narrower than the sample spacing that leaves no trace in the samples cannot be
    detected. For such a model, pass a TabulatedDeltaK or raise ``n_scan``.
    """
    if isinstance(dk_of_a, TabulatedDeltaK):
        return _first_arrest_tabulated(dk_of_a, a0, af, dk_eff)
    pts = _scan_points(a0, af, n_scan)
    vals = [dk_of_a(a) for a in pts]
    if vals[0] <= dk_eff:
        return pts[0]
    for i in range(1, len(pts)):
        if vals[i] <= dk_eff:
            return _bisect_crossing(dk_of_a, pts[i - 1], pts[i], dk_eff)
        is_local_min = (
            i < len(pts) - 1 and vals[i] < vals[i - 1] and vals[i] <= vals[i + 1]
        )
        if is_local_min:
            x_min, f_min = _golden_min(dk_of_a, pts[i - 1], pts[i + 1])
            if f_min <= dk_eff:
                return _bisect_crossing(dk_of_a, pts[i - 1], x_min, dk_eff)
    return None


def life(
    law: GrowthLaw,
    dk_of_a: DeltaKModel,
    a0_mm: float,
    af_mm: float,
    *,
    threshold: Optional[Threshold],
    n_intervals: int = 4000,
    n_scan: int = 20_000,
) -> LifeResult:
    """Cycles to grow from ``a0_mm`` to ``af_mm``, or ARRESTED at a threshold crossing.

    ``threshold`` must be passed explicitly (``None`` means no threshold).
    """
    if not af_mm > a0_mm > 0:
        raise ValueError("need 0 < a0 < af.")
    if n_intervals < 2 or n_intervals % 2:
        raise ValueError("n_intervals must be an even integer >= 2.")
    if threshold is not None:
        a_arr = _first_arrest(dk_of_a, a0_mm, af_mm, threshold.effective, n_scan)
        if a_arr is not None:
            return LifeResult("ARRESTED", None, a_arr)
    h = (af_mm - a0_mm) / n_intervals
    total = 0.0
    for i in range(n_intervals + 1):
        a = a0_mm + i * h
        dk = dk_of_a(a)
        if not dk > 0:
            raise ValueError(
                f"DeltaK <= 0 at a = {a:.6g} mm; supply a threshold or a positive "
                "DeltaK model (no silent truncation)."
            )
        w = 1.0 if i in (0, n_intervals) else (4.0 if i % 2 else 2.0)
        total += w / law.rate(dk)
    return LifeResult("GROWS", total * h / 3.0)


def life_sqrt_closed_form(
    law: GrowthLaw, dk0: float, a0_mm: float, af_mm: float
) -> float:
    """Closed-form life for ``DeltaK(a) = dk0 sqrt(a / a0)`` with no threshold."""
    m, a0, af = law.m, a0_mm, af_mm
    pre = a0 ** (m / 2.0) / (law.A * dk0**m)
    if math.isclose(m, 2.0):
        return pre * math.log(af / a0)
    return pre * (2.0 / (m - 2.0)) * (a0 ** (1.0 - m / 2.0) - af ** (1.0 - m / 2.0))


def dk_sqrt_model(dk0: float, a0_mm: float) -> DeltaKModel:
    """``DeltaK(a) = dk0 sqrt(a / a0)``: constant-stress screening model."""
    return lambda a: dk0 * math.sqrt(a / a0_mm)


def dk_envelope(*models: DeltaKModel) -> DeltaKModel:
    """Pointwise maximum of several DeltaK(a) models."""
    if not models:
        raise ValueError("need at least one model.")
    return lambda a: max(f(a) for f in models)


class TabulatedDeltaK:
    """Piecewise-linear DeltaK(a) from discrete crack states.

    ``extrapolation`` is ``"raise"`` (default), ``"hold"`` or ``"linear"``; anything
    outside the tabulated depths must be requested explicitly.
    """

    def __init__(
        self,
        a_mm: Sequence[float],
        dk: Sequence[float],
        extrapolation: str = "raise",
    ) -> None:
        if len(a_mm) != len(dk) or len(a_mm) < 2:
            raise ValueError("need at least two (a, dK) pairs of equal length.")
        if any(b <= a for a, b in zip(a_mm, a_mm[1:])):
            raise ValueError("depths must be strictly increasing.")
        if extrapolation not in ("raise", "hold", "linear"):
            raise ValueError("extrapolation must be 'raise', 'hold' or 'linear'.")
        self.a = list(map(float, a_mm))
        self.dk = list(map(float, dk))
        self.extrapolation = extrapolation

    def _lin(self, i: int, a: float) -> float:
        a1, a2, k1, k2 = self.a[i], self.a[i + 1], self.dk[i], self.dk[i + 1]
        return k1 + (k2 - k1) * (a - a1) / (a2 - a1)

    def __call__(self, a: float) -> float:
        lo, hi = self.a[0], self.a[-1]
        if lo <= a <= hi:
            i = min(bisect_right(self.a, a) - 1, len(self.a) - 2)
            return self._lin(i, a)
        if self.extrapolation == "raise":
            raise ValueError(f"a = {a} mm is outside the tabulated range [{lo}, {hi}].")
        if self.extrapolation == "hold":
            return self.dk[0] if a < lo else self.dk[-1]
        return self._lin(0, a) if a < lo else self._lin(len(self.a) - 2, a)


def threshold_margin(
    dk_of_a: DeltaKModel,
    a0_mm: float,
    af_mm: float,
    threshold: Threshold,
    n_scan: int = 20_000,
) -> float:
    """Smallest fractional reduction in DeltaK over ``[a0, af]`` that reaches threshold."""
    eff = threshold.effective
    return min(1.0 - eff / dk_of_a(a) for a in _scan_points(a0_mm, af_mm, n_scan))


def dk_multiplier_to_demand(
    law: GrowthLaw,
    dk_of_a: DeltaKModel,
    a0_mm: float,
    af_mm: float,
    *,
    n_demand: float,
    threshold: Optional[Threshold],
) -> float:
    """Multiplier k on DeltaK(a) at which the life equals ``n_demand`` cycles.

    With no threshold this is the closed form ``(N / N_demand)^(1/m)``. That form is
    valid only for a single-slope law with no threshold. With a threshold, k comes
    from a numerical root find on the thresholded life.
    """
    if n_demand <= 0:
        raise ValueError("n_demand must be > 0.")
    if threshold is None:
        n = life(law, dk_of_a, a0_mm, af_mm, threshold=None).cycles
        return (n / n_demand) ** (1.0 / law.m)

    def n_of(k: float) -> float:
        r = life(law, lambda a: k * dk_of_a(a), a0_mm, af_mm, threshold=threshold)
        return math.inf if r.status == "ARRESTED" else r.cycles

    lo, hi = 1.0, 1.0
    while n_of(hi) > n_demand:
        hi *= 2.0
        if hi > 1e6:
            raise ValueError("no multiplier up to 1e6 reaches the demand.")
    while n_of(lo) < n_demand:
        lo /= 2.0
        if lo < 1e-6:
            raise ValueError("demand exceeds the life even at a 1e-6 multiplier.")
    for _ in range(200):
        mid = 0.5 * (lo + hi)
        if n_of(mid) > n_demand:
            lo = mid
        else:
            hi = mid
        if (hi - lo) / hi < 1e-12:
            break
    return 0.5 * (lo + hi)


def back_calculate_dk(
    history: Sequence[Tuple[float, float]], law: GrowthLaw
) -> List[float]:
    """DeltaK implied by each segment of a (cycles, extension_mm) growth history."""
    if len(history) < 2:
        raise ValueError("need at least two history points.")
    out = []
    for (n1, d1), (n2, d2) in zip(history, history[1:]):
        if n2 <= n1 or d2 < d1:
            raise ValueError(
                "history must have increasing cycles and non-decreasing extension."
            )
        out.append(((d2 - d1) / (n2 - n1) / law.A) ** (1.0 / law.m))
    return out


def cycles_to_extension(
    history: Sequence[Tuple[float, float]], extension_mm: float
) -> float:
    """Cycles at a target extension by linear interpolation of the history.

    The result is an interpolation between recorded points, not a recorded value. The
    history must be chronological: strictly increasing cycles and non-decreasing
    extension, in recorded order.
    """
    pts = list(history)
    if len(pts) < 2:
        raise ValueError("need at least two history points.")
    for (n1, d1), (n2, d2) in zip(pts, pts[1:]):
        if n2 <= n1 or d2 < d1:
            raise ValueError(
                "history must have strictly increasing cycles and non-decreasing "
                "extension in recorded order."
            )
    for (n1, d1), (n2, d2) in zip(pts, pts[1:]):
        if d1 <= extension_mm <= d2:
            if d2 == d1:
                return float(n1)
            return n1 + (extension_mm - d1) / (d2 - d1) * (n2 - n1)
    raise ValueError(f"extension {extension_mm} mm is outside the history range.")
