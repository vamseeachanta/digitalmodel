# ABOUTME: Corroded-pipe remaining-strength — ASME B31G, Modified B31G (0.85dL)
# ABOUTME: and RSTRENG effective-area failure-pressure methods (FFS Level 1/2).
"""Corroded-pipeline remaining-strength assessment.

Implements the three classic remaining-strength methods for a blunt metal-loss
defect in a pressurised pipe, all of the common form

    P_f = (2 * S_flow * t / D) * (1 - A/A0) / (1 - (A/A0) / M)

where ``S_flow`` is the flow stress, ``A/A0`` the fractional metal-loss area in
the longitudinal plane and ``M`` the Folias (bulging) factor:

* **Original B31G** — parabolic area ``A/A0 = (2/3)(d/t)``,
  ``S_flow = 1.1*SMYS``, ``M = sqrt(1 + 0.8 z)`` for ``z = L^2/(D t) <= 20``;
  for longer flaws the defect is treated as infinitely long
  (``P_f = 2 S_flow t/D * (1 - d/t)``).
* **Modified B31G (0.85 dL)** — ``A/A0 = 0.85(d/t)``,
  ``S_flow = SMYS + 10 ksi``, two-part ``M``
  (``sqrt(1 + 0.6275 z - 0.003375 z^2)`` for ``z <= 50`` else ``0.032 z + 3.3``).
* **RSTRENG effective-area** — same flow stress and ``M`` as Modified B31G but
  ``A`` is the *measured* metal-loss area from a river-bottom thickness profile;
  the governing (lowest failure pressure) sub-segment of the profile is
  searched and returned.

Formula constants confirmed against ASME B31G-2012 / the ``pipenostics``
reference. Units are US customary: lengths in inches, pressures and stresses in
psi.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional, Sequence

import numpy as np

# Flow-stress adder for Modified B31G / RSTRENG: SMYS + 10 ksi (= 68.95 MPa).
_FLOW_ADDER_PSI = 10_000.0
# Default safety factor applied to predicted failure pressure (B31G practice).
_DEFAULT_SAFETY_FACTOR = 1.39
# ASME B31G / Modified B31G validity limit on relative defect depth.  The
# methods are calibrated for d/t up to 0.80; deeper defects fall outside the
# qualified range and should be assessed by repair/replace criteria.
_B31G_MAX_DT = 0.80

# API 5L specified minimum yield strength (psi) — grade number is SMYS in ksi.
SMYS_PSI = {
    "X42": 42_000.0, "X46": 46_000.0, "X52": 52_000.0, "X56": 56_000.0,
    "X60": 60_000.0, "X65": 65_000.0, "X70": 70_000.0, "X80": 80_000.0,
}


@dataclass
class CorrodedPipeResult:
    """Remaining-strength result for a corroded pipe defect."""

    method: str                  # B31G | Modified_B31G | RSTRENG
    failure_pressure_psi: float  # predicted burst pressure of the defect
    intact_pressure_psi: float   # flow-stress pressure of the undamaged pipe
    flow_stress_psi: float
    folias_factor: float
    area_ratio: float            # A/A0 (fraction of wall area lost)
    rsf: float                   # failure_pressure / intact_pressure
    safe_pressure_psi: float     # failure_pressure / safety_factor
    acceptable: Optional[bool] = None   # safe_pressure >= MAOP (if given)
    details: dict = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Folias bulging factors
# ---------------------------------------------------------------------------
def folias_original(z: float) -> Optional[float]:
    """Original B31G Folias factor; ``None`` signals the infinite-length regime."""
    if z <= 20.0:
        return math.sqrt(1.0 + 0.8 * z)
    return None


def folias_modified(z: float) -> float:
    """Modified B31G / RSTRENG two-part Folias factor."""
    if z <= 50.0:
        return math.sqrt(1.0 + 0.6275 * z - 0.003375 * z * z)
    return 0.032 * z + 3.3


def _failure_pressure(flow: float, t: float, D: float, ar: float, M: float) -> float:
    """Common B31G failure-pressure form (guards full-penetration A/A0 -> 1)."""
    ar = min(ar, 0.999)
    return (2.0 * flow * t / D) * (1.0 - ar) / (1.0 - ar / M)


def _finalise(method, pf, intact, flow, M, ar, *, maop_psi, safety_factor, details,
              d_over_t=None):
    safe = pf / safety_factor
    if d_over_t is not None:
        within = d_over_t <= _B31G_MAX_DT
        details = {**details,
                   "within_applicability": bool(within),
                   "applicability_note": (
                       None if within else
                       f"d/t={d_over_t:.3f} exceeds B31G validity limit "
                       f"{_B31G_MAX_DT:.2f}; assess by repair/replace criteria")}
    return CorrodedPipeResult(
        method=method,
        failure_pressure_psi=pf,
        intact_pressure_psi=intact,
        flow_stress_psi=flow,
        folias_factor=M,
        area_ratio=ar,
        rsf=(pf / intact) if intact > 0 else float("nan"),
        safe_pressure_psi=safe,
        acceptable=(None if maop_psi is None else bool(safe >= maop_psi)),
        details=details,
    )


# ---------------------------------------------------------------------------
# Methods
# ---------------------------------------------------------------------------
def b31g_original(
    D: float, t: float, d: float, L: float, smys_psi: float,
    *, maop_psi: Optional[float] = None, safety_factor: float = _DEFAULT_SAFETY_FACTOR,
) -> CorrodedPipeResult:
    """Original ASME B31G remaining-strength failure pressure.

    Args:
        D: Pipe outside diameter (in). t: nominal wall thickness (in).
        d: maximum defect depth (in). L: axial defect length (in).
        smys_psi: specified minimum yield strength (psi).
        maop_psi: maximum allowable operating pressure for an accept/reject flag.
        safety_factor: applied to failure pressure for the safe pressure.
    """
    _validate(D, t, d, L)
    flow = 1.1 * smys_psi
    intact = 2.0 * flow * t / D
    z = L * L / (D * t)
    M = folias_original(z)
    if M is None:  # infinitely long flaw
        ar = d / t
        pf = 2.0 * flow * t / D * (1.0 - ar)
        M_report, regime = float("inf"), "infinite_length"
    else:
        ar = (2.0 / 3.0) * (d / t)
        pf = _failure_pressure(flow, t, D, ar, M)
        M_report, regime = M, "parabolic"
    return _finalise(
        "B31G", pf, intact, flow, M_report, ar,
        maop_psi=maop_psi, safety_factor=safety_factor,
        details={"z": z, "regime": regime, "d_over_t": d / t},
        d_over_t=d / t,
    )


def modified_b31g(
    D: float, t: float, d: float, L: float, smys_psi: float,
    *, maop_psi: Optional[float] = None, safety_factor: float = _DEFAULT_SAFETY_FACTOR,
) -> CorrodedPipeResult:
    """Modified B31G (0.85 dL) remaining-strength failure pressure."""
    _validate(D, t, d, L)
    flow = smys_psi + _FLOW_ADDER_PSI
    intact = 2.0 * flow * t / D
    z = L * L / (D * t)
    M = folias_modified(z)
    ar = 0.85 * (d / t)
    pf = _failure_pressure(flow, t, D, ar, M)
    return _finalise(
        "Modified_B31G", pf, intact, flow, M, ar,
        maop_psi=maop_psi, safety_factor=safety_factor,
        details={"z": z, "d_over_t": d / t},
        d_over_t=d / t,
    )


def rstreng_effective_area(
    D: float, t: float, positions_in: Sequence[float], depths_in: Sequence[float],
    smys_psi: float, *, maop_psi: Optional[float] = None,
    safety_factor: float = _DEFAULT_SAFETY_FACTOR,
) -> CorrodedPipeResult:
    """RSTRENG effective-area method from a river-bottom metal-loss profile.

    Args:
        positions_in: axial positions of the profile points (in), increasing.
        depths_in: metal-loss DEPTH at each position (in); 0 = full wall.
    The governing sub-segment (lowest predicted failure pressure) is returned.
    """
    _validate_geometry(D, t)
    x = np.asarray(positions_in, dtype=float)
    d = np.asarray(depths_in, dtype=float)
    if x.shape != d.shape or x.size < 2:
        raise ValueError("positions_in and depths_in must be equal-length (>=2).")
    if np.any(np.diff(x) <= 0):
        raise ValueError("positions_in must be strictly increasing.")
    if np.any(d < 0) or np.any(d > t):
        raise ValueError("depths must be within [0, t].")

    flow = smys_psi + _FLOW_ADDER_PSI
    intact = 2.0 * flow * t / D
    # Prefix trapezoidal integral of metal-loss depth for O(1) sub-segment area.
    seg = np.diff(x) * 0.5 * (d[:-1] + d[1:])           # area of each interval
    cum = np.concatenate(([0.0], np.cumsum(seg)))        # cum[k] = area up to x[k]

    n = x.size
    worst_pf = math.inf
    worst = None
    for i in range(n - 1):
        for j in range(i + 1, n):
            L = x[j] - x[i]
            if L <= 0:
                continue
            A = cum[j] - cum[i]            # effective metal-loss area
            A0 = t * L
            ar = A / A0 if A0 > 0 else 0.0
            z = L * L / (D * t)
            M = folias_modified(z)
            pf = _failure_pressure(flow, t, D, ar, M)
            if pf < worst_pf:
                worst_pf = pf
                worst = {"L": float(L), "ar": float(ar), "M": float(M),
                         "i": i, "j": j}
    max_dt = float(np.max(d)) / t
    return _finalise(
        "RSTRENG", worst_pf, intact, flow, worst["M"], worst["ar"],
        maop_psi=maop_psi, safety_factor=safety_factor,
        details={"critical_length_in": worst["L"],
                 "critical_segment": (worst["i"], worst["j"]),
                 "d_over_t": max_dt},
        d_over_t=max_dt,
    )


def allowable_flaw_length(
    D: float, t: float, d: float, smys_psi: float, design_pressure_psi: float,
    *, method: str = "modified", safety_factor: float = _DEFAULT_SAFETY_FACTOR,
    max_length_in: float = 1000.0,
) -> float:
    """Maximum axial flaw length (in) acceptable at the design pressure.

    Reconstructs the ASME B31G "allowable defect length" relationship by
    bisecting the failure-pressure curve for the chosen method.
    """
    fn = modified_b31g if method.lower().startswith("mod") else b31g_original
    # Acceptable at L=0; find the length where safe pressure drops to design.
    lo, hi = 1.0e-6, max_length_in
    if fn(D, t, d, hi, smys_psi, safety_factor=safety_factor).safe_pressure_psi \
            >= design_pressure_psi:
        return max_length_in
    for _ in range(80):
        mid = 0.5 * (lo + hi)
        safe = fn(D, t, d, mid, smys_psi, safety_factor=safety_factor).safe_pressure_psi
        if safe >= design_pressure_psi:
            lo = mid
        else:
            hi = mid
    return lo


def b31g_original_allowable_length(D: float, t: float, d: float) -> float:
    """Original ASME B31G maximum allowable longitudinal defect length (in).

    The canonical B31G acceptance chart (e.g. ASME B31G-2012 page 26):

        L_allow = 1.12 * B * sqrt(D * t)
        B = sqrt( (dt / (1.1*dt - 0.15))^2 - 1 ),  capped at 4.0,

    where ``dt = d/t``. ``B`` is taken as 4.0 for shallow defects (where the
    bracket is <= 0 or would exceed 4.0). Defects deeper than 80 % of the wall
    are not acceptable (returns 0.0 — repair/replace).

    Unlike :func:`allowable_flaw_length` (a pressure-based Modified-B31G
    inversion), this is the pressure-independent geometric B31G chart and
    reproduces the published ASME table for a given diameter.
    """
    _validate(D, t, d, 0.0)
    dt = d / t
    if dt > 0.80:
        return 0.0
    denom = 1.1 * dt - 0.15
    if denom <= 0.0:
        B = 4.0
    else:
        bracket = (dt / denom) ** 2 - 1.0
        B = 4.0 if bracket < 0.0 else min(4.0, math.sqrt(bracket))
    return 1.12 * B * math.sqrt(D * t)


# ---------------------------------------------------------------------------
def _validate_geometry(D: float, t: float) -> None:
    if D <= 0 or t <= 0 or t >= D:
        raise ValueError(f"Invalid geometry: D={D}, t={t}.")


def _validate(D: float, t: float, d: float, L: float) -> None:
    _validate_geometry(D, t)
    if not (0.0 <= d <= t):
        raise ValueError(f"defect depth d={d} must be in [0, t={t}].")
    if L < 0:
        raise ValueError(f"defect length L={L} must be >= 0.")
