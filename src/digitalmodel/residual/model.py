"""Transparent bounded affine residual-correction model (twin B #1374).

``ResidualModel`` fits ``corrected = clip(scale * predicted + bias, lo, hi)`` from
(measured, predicted) pairs by least squares, with hard bounds and an
operating-domain gate. It is domain-neutral (generic float arrays) and imports
nothing from ``digitalmodel.citations`` — the fit is an empirical, data-driven
estimate, NEVER a standard and NEVER a cited value (see :meth:`provenance`).

Fail-closed semantics (the T2 correctness reframe): insufficient pairs, an
out-of-bounds fit, a NaN, or a query outside the fitted domain do NOT emit a
silently-corrected number — they ESCALATE (``Correction.status``), so a consumer
can defer to the exact physics and flag rather than trust the estimate.
"""
from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Any

import numpy as np

from digitalmodel.residual.skill import rmse

#: Bounds — a bad fixture cannot yield a wild / non-physical correction.
SCALE_MIN = 0.5
SCALE_MAX = 2.0
BIAS_MAX = 1.0
#: Minimum (measured, predicted) pairs before a fit is trusted.
MIN_PAIRS = 8

ACTIVE = "active"


def _escalate(reason: str) -> str:
    return f"escalate:{reason}"


@dataclass(frozen=True)
class Correction:
    """One applied correction: a PLAIN value + a status. NOT a ``CitedValue``.

    ``value`` is the corrected (tracking) estimate; when ``escalated`` the model
    could not be trusted for this input and ``value`` falls back to the raw
    ``predicted`` — a consumer must treat an escalated channel as "defer to exact".
    """

    value: float
    status: str = ACTIVE
    reason: str = ""

    @property
    def escalated(self) -> bool:
        return self.status != ACTIVE


@dataclass(frozen=True)
class ResidualModel:
    """A fitted (or fallback) bounded affine correction.

    Build with :meth:`fit`. ``status == ACTIVE`` iff the fit passed all gates; a
    fallback model carries ``escalate:<reason>`` and applies as identity.
    """

    scale: float = 1.0
    bias: float = 0.0
    lo: float = 0.0
    hi: float = math.inf
    n_pairs: int = 0
    skill_before: float = math.nan
    skill_after: float = math.nan
    domain_lo: float = -math.inf
    domain_hi: float = math.inf
    scale_min: float = SCALE_MIN
    scale_max: float = SCALE_MAX
    bias_max: float = BIAS_MAX
    status: str = _escalate("not_fitted")
    note: str = (
        "Empirical correction fit from measured-predicted pairs; a data-driven "
        "estimate, NOT a standard and NOT a cited value."
    )

    @property
    def active(self) -> bool:
        return self.status == ACTIVE

    # -- construction ----------------------------------------------------
    @classmethod
    def identity(cls, reason: str = "not_fitted", *, lo: float = 0.0, hi: float = math.inf) -> "ResidualModel":
        """A fallback model that applies as identity and reports ``escalate``."""
        return cls(scale=1.0, bias=0.0, lo=lo, hi=hi, status=_escalate(reason))

    @classmethod
    def fit(
        cls,
        measured,
        predicted,
        *,
        lo: float = 0.0,
        hi: float = math.inf,
        min_pairs: int = MIN_PAIRS,
        scale_bounds: tuple[float, float] = (SCALE_MIN, SCALE_MAX),
        bias_max: float = BIAS_MAX,
    ) -> "ResidualModel":
        """Least-squares fit of ``measured ≈ scale*predicted + bias``, gated.

        Returns a fallback identity model (``escalate:<reason>``) when there are
        fewer than ``min_pairs`` finite pairs, when the fit lands outside the
        bounds, or when the inputs are degenerate — never a silent wild fit.
        ``scale_bounds`` / ``bias_max`` / ``lo`` / ``hi`` are the caller's physical
        bounds (the riser adapter passes angle-appropriate ones; another domain sets
        its own) — the spine itself stays domain-neutral.
        """
        m = np.asarray(measured, dtype=float)
        p = np.asarray(predicted, dtype=float)
        if m.shape != p.shape:
            raise ValueError(f"measured/predicted shape mismatch: {m.shape} vs {p.shape}")
        finite = np.isfinite(m) & np.isfinite(p)
        m, p = m[finite], p[finite]
        n = int(m.size)
        if n < min_pairs:
            return cls.identity(f"insufficient_pairs({n}<{min_pairs})", lo=lo, hi=hi)
        if np.std(p) == 0:
            return cls.identity("degenerate_predicted(no_variation)", lo=lo, hi=hi)

        scale, bias = (float(v) for v in np.polyfit(p, m, 1))
        domain_lo, domain_hi = float(p.min()), float(p.max())
        rmse_before = rmse(m, p)
        corrected = np.clip(scale * p + bias, lo, hi)
        rmse_after = rmse(m, corrected)

        s_min, s_max = scale_bounds
        _tol = 1e-9  # tolerate a float-edge fit landing exactly on a bound
        if not (s_min - _tol <= scale <= s_max + _tol) or abs(bias) > bias_max + _tol:
            return cls(
                scale=scale, bias=bias, lo=lo, hi=hi, n_pairs=n,
                skill_before=rmse_before, skill_after=rmse_after,
                domain_lo=domain_lo, domain_hi=domain_hi,
                scale_min=s_min, scale_max=s_max, bias_max=bias_max,
                status=_escalate(f"fit_out_of_bounds(scale={scale:.3f},bias={bias:.3f})"),
            )
        if not (math.isfinite(scale) and math.isfinite(bias)):
            return cls.identity("non_finite_fit", lo=lo, hi=hi)

        return cls(
            scale=scale, bias=bias, lo=lo, hi=hi, n_pairs=n,
            skill_before=rmse_before, skill_after=rmse_after,
            domain_lo=domain_lo, domain_hi=domain_hi,
            scale_min=s_min, scale_max=s_max, bias_max=bias_max, status=ACTIVE,
        )

    # -- application -----------------------------------------------------
    def apply(self, predicted: float) -> Correction:
        """Correct one prediction. Out-of-domain or a non-active model ESCALATES
        (returns the raw ``predicted`` with an escalate status), never a
        silently-fabricated correction."""
        x = float(predicted)
        if not self.active:
            return Correction(x, self.status, self.status.split(":", 1)[-1])
        if not math.isfinite(x):
            return Correction(x, _escalate("non_finite_input"), "non_finite_input")
        if x < self.domain_lo or x > self.domain_hi:
            reason = f"out_of_domain({x:g} not in [{self.domain_lo:g},{self.domain_hi:g}])"
            return Correction(x, _escalate(reason), reason)
        value = float(np.clip(self.scale * x + self.bias, self.lo, self.hi))
        return Correction(value, ACTIVE)

    # -- provenance ------------------------------------------------------
    def provenance(self) -> dict[str, Any]:
        """Fully-transparent, explicitly NON-cited provenance (plain dict).

        The ``data_driven`` / ``not_a_standard`` / ``not_cited`` markers stop a
        downstream reader from mistaking the empirical (scale, bias) for a
        standards value (the T2 governance finding)."""
        return {
            "kind": "residual_correction",
            "data_driven": True,
            "not_a_standard": True,
            "not_cited": True,
            "scale": self.scale,
            "bias": self.bias,
            "n_pairs": self.n_pairs,
            "skill_before_rmse": self.skill_before,
            "skill_after_rmse": self.skill_after,
            "domain": [self.domain_lo, self.domain_hi],
            "bounds": {"scale": [self.scale_min, self.scale_max], "bias_abs_max": self.bias_max,
                       "output": [self.lo, self.hi]},
            "status": self.status,
            "note": self.note,
        }
