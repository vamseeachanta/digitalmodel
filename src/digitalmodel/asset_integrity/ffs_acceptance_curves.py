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

import math
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
# Bounded preliminary pressure-demand wall screen
# ---------------------------------------------------------------------------
def pipe_pressure_wall_screen(
    D: float, t: float, grade: str, method: str, *, axial_length_in: float,
    pressure_psi: float, safety_factor: float, max_depth_fraction: float = 0.80,
    flaw_orientation: str = 'longitudinal', load_case: str = 'internal-pressure',
) -> dict:
    """Bounded preliminary arithmetic; no API 579 or asset acceptance.

    Reuses raw B31G-2012-labelled methods with retained applicability. Uniform
    loss gives RSTRENG a rectangular two-point axial profile; B31G methods use
    their own area approximations. Width/axial load/instability are not assessed.
    """
    if not all(math.isfinite(v) and v > 0 for v in
               (D, t, axial_length_in, pressure_psi, safety_factor, max_depth_fraction)):
        raise ValueError('Finite positive geometry, demand and factors required')
    if 2*t >= D or max_depth_fraction >= 1 or safety_factor < 1:
        raise ValueError('Invalid geometry, depth fraction or safety factor')
    if method not in {'b31g', 'modified_b31g', 'rstreng'}:
        raise ValueError('Use b31g, modified_b31g or rstreng')
    try:
        smys = SMYS_PSI[grade]
    except KeyError as exc:
        raise ValueError(f'Unknown pipe grade: {grade}') from exc
    result = dict(
        evidence_status='PRELIMINARY_ARITHMETIC_ONLY',
        method=method, code_basis='ASME B31G-2012 label; historical arithmetic anchors',
        api579_allowable_remaining_wall_in=None, asset_acceptance=None,
        preliminary_remaining_wall_in=None, reason_codes=[],
        inputs=dict(od_in=D, nominal_wall_in=t, smys_psi=smys, grade=grade,
                    axial_length_in=axial_length_in, pressure_psi=pressure_psi,
                    safety_factor=safety_factor, max_depth_fraction=max_depth_fraction,
                    flaw_orientation=flaw_orientation, load_case=load_case),
        monotonicity_samples=[], passing_bracket=None, failing_bracket=None,
        limitations=['HOOP_CONTAINMENT_ONLY', 'WIDTH_DOMAIN_NOT_QUALIFIED',
                     'NO_SEPARATE_AXIAL_STRESS_OR_INSTABILITY_CHECK',
                     'NOT_FULL_EDITION_SPECIFIC_COMPLIANCE_AUDIT'],
        wall_bracket_tolerance_in=1e-6,
        monotonicity_basis='Fixed nominal geometry/length: area ratio increases with depth; '
                           'constant M>=1 makes capacity nonincreasing with depth. '
                           'Original long-flaw branch is linear. Sampling also checked.')
    if flaw_orientation != 'longitudinal' or load_case != 'internal-pressure':
        result.update(status='INAPPLICABLE', reason_codes=['UNSUPPORTED_LOAD_OR_ORIENTATION'])
        return result
    if max_depth_fraction > .80:
        result.update(status='INAPPLICABLE', reason_codes=['B31G_DEPTH_BOUND_EXCEEDED'])
        return result

    def evaluate(wall, depth_override=None):
        depth = t-wall if depth_override is None else depth_override
        kw = dict(maop_psi=pressure_psi, safety_factor=safety_factor)
        if method == 'rstreng':
            raw = rstreng_effective_area(D, t, [0., axial_length_in], [depth, depth], smys, **kw)
        else:
            fn = b31g_original if method == 'b31g' else modified_b31g
            raw = fn(D, t, depth, axial_length_in, smys, **kw)
        return dict(remaining_wall_in=wall, depth_in=depth,
                    safe_pressure_psi=raw.safe_pressure_psi,
                    pressure_margin_psi=raw.safe_pressure_psi-pressure_psi,
                    applicability=raw.applicability.to_dict(),
                    failure_pressure_psi=raw.failure_pressure_psi,
                    flow_stress_psi=raw.flow_stress_psi, area_ratio=raw.area_ratio,
                    folias_factor=(raw.folias_factor if math.isfinite(raw.folias_factor) else None),
                    folias_factor_reason=('FINITE' if math.isfinite(raw.folias_factor)
                                          else 'ORIGINAL_B31G_INFINITE_LENGTH_REGIME'),
                    code_reference=raw.code_reference)

    samples = []
    for i in range(101):
        depth = max_depth_fraction*t*(100-i)/100
        # Correct construction roundoff only; caller bounds above .80 are rejected.
        while depth/t > max_depth_fraction:
            depth = math.nextafter(depth, 0.)
        samples.append(evaluate(t-depth, depth_override=depth))
    result['monotonicity_samples'] = samples
    if not all(row['applicability']['ok'] for row in samples):
        result.update(status='INAPPLICABLE', reason_codes=['UNDERLYING_APPLICABILITY_FLAG'])
        return result
    if any(b['safe_pressure_psi'] < a['safe_pressure_psi']-1e-9
           for a, b in zip(samples, samples[1:])):
        result.update(status='INAPPLICABLE', reason_codes=['NONMONOTONIC_PRESSURE'])
        return result
    passing, failing = samples[-1], samples[0]
    if failing['pressure_margin_psi'] >= 0:
        result.update(status='LOWER_BOUND_CENSORED', passing_bracket=failing,
                      reason_codes=['LOWEST_STUDY_WALL_MEETS_PRESSURE'])
        return result
    if passing['pressure_margin_psi'] < 0:
        result.update(status='NO_PRESSURE_SOLUTION', failing_bracket=passing,
                      reason_codes=['NOMINAL_WALL_BELOW_PRESSURE_DEMAND'])
        return result
    for _ in range(60):
        if passing['remaining_wall_in']-failing['remaining_wall_in'] <= 1e-6:
            break
        mid = evaluate((passing['remaining_wall_in']+failing['remaining_wall_in'])/2)
        if not mid['applicability']['ok']:
            result.update(status='INAPPLICABLE', flagged_evaluation=mid,
                          reason_codes=['UNDERLYING_APPLICABILITY_FLAG'])
            return result
        if mid['pressure_margin_psi'] >= 0:
            passing = mid
        else:
            failing = mid
    result.update(status='PRELIMINARY_THRESHOLD',
                  preliminary_remaining_wall_in=passing['remaining_wall_in'],
                  passing_bracket=passing, failing_bracket=failing)
    return result


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
