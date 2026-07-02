# ABOUTME: Level-2 FAD assessment of crack-like flaws — BS 7910 Option 1 /
# ABOUTME: API 579-1 Part 9 Level 2: Kr-Lr point vs the failure envelope.
"""Crack-like flaw assessment on the Failure Assessment Diagram (#1270).

Activates the fracture tier behind :mod:`level3_escalation`: a surface
(semi-elliptical) flaw in a plate-like section is assessed as a single point
``(Lr, Kr)`` against the BS 7910:2013 **Option 1** failure-assessment curve
(equivalent to the API 579-1 Part 9 Level 2 FAD; the Level 2A ≡ Level 2
equivalence is the published cross-standard result).

Pieces, each anchored to its published source:

- **FAD curve** — BS 7910:2013 Clause 7.3.2 Option 1:
  ``f(Lr) = (1 + 0.5 Lr^2)^-0.5 * (0.3 + 0.7 exp(-mu Lr^6))`` for Lr <= 1,
  ``f(1) * Lr^((N-1)/(2N))`` for 1 < Lr <= Lr_max, with
  ``mu = min(0.001 E/sigma_y, 0.6)``, ``N = 0.3 (1 - sigma_y/sigma_u)`` and
  cutoff ``Lr_max = (sigma_y + sigma_u) / (2 sigma_y)``.  Numerically
  cross-checked against the repo's legacy implementation
  (``asset_integrity/common/fad.py``, used in prior class-society-reviewed
  work) by the test suite.
- **Stress intensity** — Newman-Raju (1981) semi-elliptical surface flaw in a
  finite plate under membrane + bending stress (deepest point and surface
  point), the standard engineering K-solution for this geometry.
- **Reference stress** — BS 7910 Annex P flat-plate surface-flaw solution
  (normal bending restraint).
- **Toughness fallback** — BS 7910 Annex J lower-bound Charpy correlation
  for the transition region, for when no fracture-toughness test exists.

Scope (tracer): flat-plate surface flaws under membrane + bending.  A thin
cylinder is assessed on its wall as a plate (supply the membrane/bending
stresses at the flaw); cylinder-specific K-solutions, embedded flaws and the
fatigue-growth clock are follow-ups.  Units: MPa, mm, MPa*sqrt(m).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional


# ---------------------------------------------------------------------------
# FAD curve — BS 7910:2013 Option 1 (== API 579-1 Part 9 Level 2)
# ---------------------------------------------------------------------------
def lr_max(sigma_y_mpa: float, sigma_u_mpa: float) -> float:
    """Plastic-collapse cutoff Lr_max = flow/yield (BS 7910 Cl. 7.3.2)."""
    return (sigma_y_mpa + sigma_u_mpa) / (2.0 * sigma_y_mpa)


def fad_curve_option1(
    lr: float, sigma_y_mpa: float, sigma_u_mpa: float, e_mpa: float = 207_000.0
) -> float:
    """f(Lr) on the BS 7910:2013 Option 1 failure-assessment curve.

    Returns 0.0 beyond the Lr_max cutoff.
    """
    if lr < 0:
        raise ValueError("Lr must be >= 0.")
    cutoff = lr_max(sigma_y_mpa, sigma_u_mpa)
    if lr > cutoff:
        return 0.0
    mu = min(0.001 * e_mpa / sigma_y_mpa, 0.6)
    if lr <= 1.0:
        return (1.0 + 0.5 * lr**2) ** -0.5 * (0.3 + 0.7 * math.exp(-mu * lr**6))
    f1 = (1.5) ** -0.5 * (0.3 + 0.7 * math.exp(-mu))
    n_exp = 0.3 * (1.0 - sigma_y_mpa / sigma_u_mpa)
    return f1 * lr ** ((n_exp - 1.0) / (2.0 * n_exp))


# ---------------------------------------------------------------------------
# Newman-Raju (1981) surface flaw K — membrane + bending, finite plate
# ---------------------------------------------------------------------------
def newman_raju_k(
    a_mm: float,
    c_mm: float,
    t_mm: float,
    sigma_m_mpa: float,
    sigma_b_mpa: float = 0.0,
    *,
    width_mm: float = 1.0e6,
    phi_deg: float = 90.0,
) -> float:
    """K (MPa*sqrt(m)) for a semi-elliptical surface flaw in a plate.

    Newman & Raju (1981) empirical solution, tension (membrane) + bending.
    ``phi_deg`` = 90 is the deepest point; 0 is the surface point.
    Validity (Newman & Raju 1981): 0 < a/c <= 1.0 (a <= c form), a/t < 1.0,
    c/width < 0.5.

    Args:
        a_mm: flaw depth. c_mm: flaw HALF-length. t_mm: wall thickness.
        sigma_m_mpa: membrane stress. sigma_b_mpa: outer-fibre bending stress.
        width_mm: plate half-width b (finite-width correction; default ~inf).
        phi_deg: parametric angle around the crack front.
    """
    if not (0 < a_mm < t_mm):
        raise ValueError("require 0 < a < t.")
    if c_mm <= 0 or a_mm / c_mm > 1.0 + 1e-9:
        raise ValueError("this form requires a/c <= 1 (a <= c).")
    if c_mm / width_mm >= 0.5:
        raise ValueError("Newman-Raju validity: c/b < 0.5.")

    aoc = a_mm / c_mm
    aot = a_mm / t_mm
    phi = math.radians(phi_deg)

    q = 1.0 + 1.464 * aoc**1.65
    m1 = 1.13 - 0.09 * aoc
    m2 = -0.54 + 0.89 / (0.2 + aoc)
    m3 = 0.5 - 1.0 / (0.65 + aoc) + 14.0 * (1.0 - aoc) ** 24
    g = 1.0 + (0.1 + 0.35 * aot**2) * (1.0 - math.sin(phi)) ** 2
    f_phi = (aoc**2 * math.cos(phi) ** 2 + math.sin(phi) ** 2) ** 0.25
    fw = math.sqrt(
        1.0 / math.cos(math.pi * c_mm / (2.0 * width_mm) * math.sqrt(aot))
    )
    f_factor = (m1 + m2 * aot**2 + m3 * aot**4) * g * f_phi * fw

    # bending multiplier H
    p_exp = 0.2 + aoc + 0.6 * aot
    h1 = 1.0 - 0.34 * aot - 0.11 * aoc * aot
    g1 = -1.22 - 0.12 * aoc
    g2 = 0.55 - 1.05 * aoc**0.75 + 0.47 * aoc**1.5
    h2 = 1.0 + g1 * aot + g2 * aot**2
    h = h1 + (h2 - h1) * math.sin(phi) ** p_exp

    a_m = a_mm / 1000.0  # -> metres for MPa*sqrt(m)
    return (sigma_m_mpa + h * sigma_b_mpa) * f_factor * math.sqrt(
        math.pi * a_m / q
    )


# ---------------------------------------------------------------------------
# Reference stress — BS 7910 Annex P, flat plate surface flaw
# ---------------------------------------------------------------------------
def reference_stress_surface_flaw(
    a_mm: float,
    c_mm: float,
    t_mm: float,
    sigma_m_mpa: float,
    sigma_b_mpa: float = 0.0,
) -> float:
    """Reference stress (MPa), BS 7910 Annex P flat-plate surface flaw.

    sigma_ref = [Pb + sqrt(Pb^2 + 9 Pm^2 (1-alpha)^2)] / [3 (1-alpha)^2],
    alpha'' = (a/t) / (1 + t/c)  (wide-plate form).
    Pb = 0 reduces to the net-section membrane amplification Pm / (1-alpha).
    """
    if not (0 < a_mm < t_mm) or c_mm <= 0:
        raise ValueError("require 0 < a < t and c > 0.")
    alpha = (a_mm / t_mm) / (1.0 + t_mm / c_mm)
    one_minus = 1.0 - alpha
    return (
        sigma_b_mpa
        + math.sqrt(sigma_b_mpa**2 + 9.0 * sigma_m_mpa**2 * one_minus**2)
    ) / (3.0 * one_minus**2)


# ---------------------------------------------------------------------------
# Toughness fallback — BS 7910 Annex J Charpy correlation
# ---------------------------------------------------------------------------
def kmat_from_charpy(
    cvn_joules: float, thickness_mm: float = 25.0
) -> float:
    """Lower-bound K_mat (MPa*sqrt(m)) from Charpy energy, BS 7910 Annex J.

    K_mat = (12 sqrt(CVN) - 20) (25/B)^0.25 + 20 — the transition-region
    lower-bound correlation.  Use measured toughness whenever available.
    """
    if cvn_joules <= 0 or thickness_mm <= 0:
        raise ValueError("CVN and thickness must be positive.")
    return (12.0 * math.sqrt(cvn_joules) - 20.0) * (
        25.0 / thickness_mm
    ) ** 0.25 + 20.0


# ---------------------------------------------------------------------------
# Level-2 FAD assessment
# ---------------------------------------------------------------------------
@dataclass
class CrackFlawAssessment:
    """One crack-like flaw assessed on the Option-1 FAD."""

    a_mm: float
    c_mm: float
    t_mm: float
    kr: float
    lr: float
    f_lr: float
    lr_max: float
    acceptable: bool
    margin: float          # f(Lr)/Kr along the load line (>1 = inside)
    k_applied: float       # MPa*sqrt(m)
    k_mat: float           # MPa*sqrt(m)
    sigma_ref_mpa: float
    notes: list = field(default_factory=list)


def assess_crack_like_flaw(
    *,
    a_mm: float,
    c_mm: float,
    t_mm: float,
    sigma_m_mpa: float,
    sigma_b_mpa: float = 0.0,
    sigma_y_mpa: float,
    sigma_u_mpa: float,
    k_mat: Optional[float] = None,
    cvn_joules: Optional[float] = None,
    e_mpa: float = 207_000.0,
    width_mm: float = 1.0e6,
) -> CrackFlawAssessment:
    """Assess a surface flaw at both crack-front points on the Option-1 FAD.

    Toughness: pass measured ``k_mat`` (MPa*sqrt(m)), or ``cvn_joules`` for
    the Annex J lower-bound fallback.  The governing (max) K of the deepest
    and surface points is used.
    """
    notes: list = []
    if k_mat is None:
        if cvn_joules is None:
            raise ValueError("supply k_mat or cvn_joules.")
        k_mat = kmat_from_charpy(cvn_joules, thickness_mm=t_mm)
        notes.append(
            f"K_mat={k_mat:.1f} MPa*sqrt(m) from CVN={cvn_joules:.0f} J "
            "(BS 7910 Annex J lower bound)."
        )

    k_deep = newman_raju_k(a_mm, c_mm, t_mm, sigma_m_mpa, sigma_b_mpa,
                           width_mm=width_mm, phi_deg=90.0)
    k_surf = newman_raju_k(a_mm, c_mm, t_mm, sigma_m_mpa, sigma_b_mpa,
                           width_mm=width_mm, phi_deg=0.0)
    k_applied = max(k_deep, k_surf)
    if k_surf > k_deep:
        notes.append("surface point governs (shallow/long flaw regime).")

    sigma_ref = reference_stress_surface_flaw(
        a_mm, c_mm, t_mm, sigma_m_mpa, sigma_b_mpa)
    lr = sigma_ref / sigma_y_mpa
    kr = k_applied / k_mat
    cutoff = lr_max(sigma_y_mpa, sigma_u_mpa)
    f_lr = fad_curve_option1(lr, sigma_y_mpa, sigma_u_mpa, e_mpa)
    acceptable = lr <= cutoff and kr <= f_lr
    margin = (f_lr / kr) if kr > 0 else float("inf")
    if lr > cutoff:
        notes.append(f"Lr={lr:.3f} beyond cutoff Lr_max={cutoff:.3f} "
                     "(plastic collapse governs).")
        margin = 0.0

    return CrackFlawAssessment(
        a_mm=a_mm, c_mm=c_mm, t_mm=t_mm,
        kr=kr, lr=lr, f_lr=f_lr, lr_max=cutoff,
        acceptable=acceptable, margin=margin,
        k_applied=k_applied, k_mat=k_mat, sigma_ref_mpa=sigma_ref,
        notes=notes,
    )


def critical_flaw_depth_mm(
    *,
    length_mm: float,
    t_mm: float,
    sigma_m_mpa: float,
    sigma_b_mpa: float = 0.0,
    sigma_y_mpa: float,
    sigma_u_mpa: float,
    k_mat: Optional[float] = None,
    cvn_joules: Optional[float] = None,
    e_mpa: float = 207_000.0,
    max_depth_fraction: float = 0.85,
    tol_mm: float = 0.01,
) -> float:
    """Deepest acceptable depth for a surface flaw of the given total length.

    Bisects :func:`assess_crack_like_flaw` — the fracture analogue of the
    metal-loss acceptance envelope (feeds the weld region of the riser-joint
    Level-1 chart).  Returns 0.0 when even a vanishing depth is unacceptable.
    Depth is additionally capped so a/c stays within Newman-Raju validity.
    """
    c_mm = length_mm / 2.0
    hi_cap = min(max_depth_fraction * t_mm, c_mm)  # a <= c validity
    kwargs = dict(c_mm=c_mm, t_mm=t_mm, sigma_m_mpa=sigma_m_mpa,
                  sigma_b_mpa=sigma_b_mpa, sigma_y_mpa=sigma_y_mpa,
                  sigma_u_mpa=sigma_u_mpa, k_mat=k_mat,
                  cvn_joules=cvn_joules, e_mpa=e_mpa)

    def ok(a: float) -> bool:
        return assess_crack_like_flaw(a_mm=a, **kwargs).acceptable

    lo = tol_mm
    if not ok(lo):
        return 0.0
    hi = hi_cap - tol_mm
    if hi <= lo:
        return 0.0
    if ok(hi):
        return hi
    while hi - lo > tol_mm:
        mid = 0.5 * (lo + hi)
        if ok(mid):
            lo = mid
        else:
            hi = mid
    return lo
