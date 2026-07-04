# ABOUTME: FFS acceptance-curve / envelope extraction — the locus of the
# ABOUTME: safe=MAOP and utilisation=1 boundaries from the validated engines.
"""FFS acceptance curves (the classic FFS acceptance envelopes).

Where :mod:`ffs_lookup` answers point queries, this module extracts the
*boundary* — the curve separating acceptable from unacceptable — by inverting
the validated calculations:

- ``pipe_acceptance_curve`` — maximum acceptable defect **length vs depth** for a
  corroded pipe at its MAOP (the B31G-style acceptance chart, for any method).
- ``plate_acceptance_curve`` — maximum acceptable **metal loss vs applied stress**
  for a plate (the utilisation = 1 envelope).
- ``general_metal_loss_curves`` — re-rated **MAWP vs FCA** and
  **remaining life vs corrosion rate** for general metal loss.

No physics here — boundaries are found by bisecting the golden-tested method
functions. Pipe units in/psi, plate units mm/MPa.
"""

from __future__ import annotations

from typing import Optional

from .corroded_pipe import (
    SMYS_PSI,
    b31g_original,
    modified_b31g,
    rstreng_effective_area,
)
from .dnv_rp_f101 import SMTS_PSI, dnv_f101_single_defect
from .ffs_lookup import barlow_maop_psi
from ..structural.structural_analysis.models import MARINE_GRADES, PlateGeometry
from ..structural.structural_analysis.plate_metal_loss_ffs import (
    max_acceptable_loss as _plate_max_loss,
)


# ---------------------------------------------------------------------------
# Pipe acceptance envelope (length vs depth at MAOP)
# ---------------------------------------------------------------------------
def _pipe_safe_pressure(method, D, t, d, L, grade) -> float:
    smys = SMYS_PSI[grade]
    m = method.lower().replace("-", "_").replace(" ", "_")
    if m in ("b31g", "b31g_original"):
        return b31g_original(D, t, d, L, smys).safe_pressure_psi
    if m in ("modified_b31g", "mod_b31g", "modified"):
        return modified_b31g(D, t, d, L, smys).safe_pressure_psi
    if m == "rstreng":
        return rstreng_effective_area(D, t, [0.0, L], [d, d], smys).safe_pressure_psi
    if m in ("dnv_f101", "dnv_rp_f101", "dnv"):
        return dnv_f101_single_defect(D, t, d, L, SMTS_PSI[grade]).allowable_pressure_psi
    raise ValueError(f"unknown method '{method}'.")


def _max_acceptable_length(method, D, t, d, grade, maop, max_length_in) -> float:
    """Largest defect length whose safe pressure still meets MAOP."""
    # Safe pressure decreases monotonically with length.
    if _pipe_safe_pressure(method, D, t, d, 1.0e-4, grade) < maop:
        return 0.0  # even a vanishing flaw of this depth fails at MAOP
    if _pipe_safe_pressure(method, D, t, d, max_length_in, grade) >= maop:
        return max_length_in
    lo, hi = 1.0e-4, max_length_in
    for _ in range(60):
        mid = 0.5 * (lo + hi)
        if _pipe_safe_pressure(method, D, t, d, mid, grade) >= maop:
            lo = mid
        else:
            hi = mid
    return lo


def pipe_acceptance_curve(
    D: float, t: float, grade: str, method: str = "modified_b31g", *,
    maop_psi: Optional[float] = None, depth_fracs: Optional[list] = None,
    max_length_in: float = 40.0,
) -> dict:
    """Max acceptable defect length for a range of depths, at the pipe's MAOP."""
    maop = maop_psi if maop_psi is not None else barlow_maop_psi(D, t, SMYS_PSI[grade])
    depth_fracs = depth_fracs or [round(0.1 * i, 2) for i in range(1, 9)]  # 0.1..0.8
    depth_in, max_len = [], []
    for frac in depth_fracs:
        d = frac * t
        depth_in.append(round(d, 4))
        max_len.append(round(
            _max_acceptable_length(method, D, t, d, grade, maop, max_length_in), 3))
    return {
        "domain": "pipe_acceptance_envelope",
        "D_in": D, "t_in": t, "grade": grade, "method": method,
        "maop_psi": round(float(maop), 2),
        "depth_frac": depth_fracs, "depth_in": depth_in,
        "max_acceptable_length_in": max_len,
    }


# ---------------------------------------------------------------------------
# Plate acceptance envelope (metal loss vs applied stress)
# ---------------------------------------------------------------------------
def plate_acceptance_curve(
    grade: str, length_mm: float, width_mm: float, thickness_mm: float, *,
    sigma_x_values: Optional[list] = None, fca_mm: float = 0.0,
) -> dict:
    """Max acceptable metal loss for a range of applied stresses."""
    material = MARINE_GRADES[grade]
    geometry = PlateGeometry(length_mm, width_mm, thickness_mm)
    sigma_x_values = sigma_x_values or [50.0, 75.0, 100.0, 125.0, 150.0,
                                        175.0, 200.0, 225.0, 250.0]
    sx, loss_mm, loss_pct = [], [], []
    for s in sigma_x_values:
        r = _plate_max_loss(geometry, material, s, fca_mm=fca_mm)
        sx.append(s)
        loss_mm.append(round(r["metal_loss_mm"], 3))
        loss_pct.append(round(r["metal_loss_pct"], 2))
    return {
        "domain": "plate_acceptance_envelope",
        "grade": grade, "length_mm": length_mm, "width_mm": width_mm,
        "thickness_mm": thickness_mm, "fca_mm": fca_mm,
        "sigma_x_mpa": sx,
        "max_acceptable_loss_mm": loss_mm,
        "max_acceptable_loss_pct": loss_pct,
    }


# ---------------------------------------------------------------------------
# General metal loss curves
# ---------------------------------------------------------------------------
def general_metal_loss_curves(
    D: float, t: float, grade: str, t_min_in: float, *,
    fca_values_in: Optional[list] = None, corrosion_rates_in_yr: Optional[list] = None,
    current_t_in: Optional[float] = None, design_factor: float = 0.72,
) -> dict:
    """Re-rated MAWP vs FCA and remaining life vs corrosion rate."""
    s_allow = SMYS_PSI[grade] * design_factor
    fca_values = fca_values_in or [round(0.02 * i, 3) for i in range(0, 11)]  # 0..0.2 in
    mawp = [round(2.0 * s_allow * max(t - f, 0.0) / D, 1) for f in fca_values]

    ct = current_t_in if current_t_in is not None else t
    rates = corrosion_rates_in_yr or [round(0.002 * i, 3) for i in range(1, 11)]
    life = [
        round(max(ct - t_min_in, 0.0) / r, 1) if r > 0 else float("inf")
        for r in rates
    ]
    return {
        "domain": "general_metal_loss",
        "D_in": D, "t_in": t, "grade": grade, "t_min_in": t_min_in,
        "fca_in": fca_values, "mawp_psi": mawp,
        "corrosion_rate_in_yr": rates, "remaining_life_yr": life,
    }
