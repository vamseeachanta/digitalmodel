# ABOUTME: Hull-girder longitudinal strength — IACS UR S11 wave bending moment,
# ABOUTME: combined envelope, section-modulus yield, and simplified ultimate (S11A).
"""Hull-girder longitudinal strength (IACS UR S11 / S11A).

Combines the still-water bending moment (from
:mod:`digitalmodel.naval_architecture.loading_computer`) with the **wave** bending
moment per IACS UR S11 and checks the midship section against:

* **Yield** — section-modulus stress ``sigma = M / Z`` against the permissible
  ``175 / k`` N/mm^2 (``k`` = material factor).
* **Ultimate** — a *simplified* hull-girder ultimate capacity
  ``M_U = sigma_U * Z_compression`` with the IACS UR S11A partial-factor format
  ``gamma_S * M_sw + gamma_W * M_wv <= M_U / gamma_R``.

The simplified ultimate is a single-step ("first collapse") estimate: the
compression flange reaches its buckling-reduced ultimate stress ``sigma_U``
(e.g. from the validated stiffened-panel solver,
:meth:`...panel_buckling.StiffenedPanelBucklingAnalyzer.check_panel` ->
``critical_stress``) while the tension flange is at yield.  It ignores the
neutral-axis migration and post-buckling redistribution captured by the full
Smith incremental-iterative method, so it is a preliminary-design figure, not a
CSR-compliant M_U.

Units: bending moments in **kN·m** (matching ``loading_computer`` and the S11
rule), section modulus in **m^3**, stresses in **MPa**.

References: IACS UR S11 (Longitudinal Strength Standard), UR S11A (Longitudinal
Strength Standard for Container Ships / hull-girder ultimate), UR S34.
"""
from __future__ import annotations

from dataclasses import dataclass

CODE_REFERENCE = "IACS UR S11 / S11A"

# Permissible hull-girder bending stress numerator (N/mm^2): sigma_perm = 175/k.
_SIGMA_PERM_NUMERATOR = 175.0

# IACS material factor k by specified minimum yield (N/mm^2).
_MATERIAL_FACTOR_K = {
    235: 1.00,   # mild steel / Grade A
    315: 0.78,   # HT32
    355: 0.72,   # HT36 / AH36
    390: 0.68,   # HT40 / EH40
    460: 0.62,   # HT47
}

# IACS UR S11A partial safety factors (hull-girder ultimate check).
_GAMMA_S = 1.0   # still-water
_GAMMA_W = 1.2   # wave
_GAMMA_R = 1.1   # resistance (capacity)

# kN·m per (MPa * m^3): sigma[MPa]=1e6 Pa; M[N·m]=sigma*Z; /1e3 -> kN·m.
_KNM_PER_MPA_M3 = 1.0e3


# ---------------------------------------------------------------------------
# Wave bending moment — IACS UR S11
# ---------------------------------------------------------------------------
def wave_coefficient(length_m: float) -> float:
    """IACS UR S11 wave coefficient C for rule length ``length_m`` (m)."""
    L = length_m
    if L < 90.0:
        # Below the S11 range; extrapolate the 90-300 m branch (flagged in docs).
        return 10.75 - ((300.0 - L) / 100.0) ** 1.5
    if L <= 300.0:
        return 10.75 - ((300.0 - L) / 100.0) ** 1.5
    if L <= 350.0:
        return 10.75
    return 10.75 - ((L - 350.0) / 150.0) ** 1.5


@dataclass(frozen=True)
class WaveBendingMoment:
    """IACS UR S11 vertical wave bending moment amidships (kN·m)."""

    hogging_kn_m: float        # positive
    sagging_kn_m: float        # negative
    wave_coefficient: float
    code_reference: str = CODE_REFERENCE


def wave_bending_moment(length_m: float, beam_m: float,
                        block_coefficient: float) -> WaveBendingMoment:
    """IACS UR S11 vertical wave bending moment amidships.

    ``M_hog = +0.19 * C * L^2 * B * Cb``;
    ``M_sag = -0.11 * C * L^2 * B * (Cb + 0.7)`` (kN·m).
    """
    c = wave_coefficient(length_m)
    base = c * length_m ** 2 * beam_m
    hog = 0.19 * base * block_coefficient
    sag = -0.11 * base * (block_coefficient + 0.7)
    return WaveBendingMoment(hogging_kn_m=hog, sagging_kn_m=sag,
                             wave_coefficient=c)


# ---------------------------------------------------------------------------
# Combined still-water + wave envelope
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class CombinedEnvelope:
    """Combined still-water + wave bending-moment envelope (kN·m)."""

    hogging_kn_m: float        # positive (M_sw,hog + M_wv,hog)
    sagging_kn_m: float        # negative (M_sw,sag + M_wv,sag)


def combined_bending_moment(
    msw_hogging_kn_m: float, msw_sagging_kn_m: float, wave: WaveBendingMoment,
) -> CombinedEnvelope:
    """Combine still-water (signed) with the S11 wave moment, per condition.

    Hogging adds positive still-water + positive wave; sagging adds the
    (negative) still-water sag + the (negative) wave sag.
    """
    return CombinedEnvelope(
        hogging_kn_m=msw_hogging_kn_m + wave.hogging_kn_m,
        sagging_kn_m=msw_sagging_kn_m + wave.sagging_kn_m,
    )


# ---------------------------------------------------------------------------
# Material factor / permissible stress
# ---------------------------------------------------------------------------
def material_factor_k(yield_mpa: float) -> float:
    """IACS material factor ``k`` for a specified minimum yield (MPa).

    Exact for the tabulated yields (235/315/355/390/460); otherwise the
    nearest tabulated grade is used.
    """
    yi = round(yield_mpa)
    if yi in _MATERIAL_FACTOR_K:
        return _MATERIAL_FACTOR_K[yi]
    nearest = min(_MATERIAL_FACTOR_K, key=lambda y: abs(y - yield_mpa))
    return _MATERIAL_FACTOR_K[nearest]


def permissible_stress(yield_mpa: float) -> float:
    """IACS permissible hull-girder bending stress ``175 / k`` (MPa)."""
    return _SIGMA_PERM_NUMERATOR / material_factor_k(yield_mpa)


# ---------------------------------------------------------------------------
# Section-modulus (yield) check
# ---------------------------------------------------------------------------
@dataclass(frozen=True)
class SectionModulusCheck:
    """Hull-girder bending-stress (yield) utilisation."""

    stress_mpa: float
    permissible_mpa: float
    utilization: float
    passes: bool
    code_reference: str = CODE_REFERENCE


def bending_stress_mpa(moment_kn_m: float, section_modulus_m3: float) -> float:
    """Hull-girder bending stress (MPa) for a moment (kN·m) and Z (m^3)."""
    return abs(moment_kn_m) / (section_modulus_m3 * _KNM_PER_MPA_M3)


def section_modulus_check(
    moment_kn_m: float, section_modulus_m3: float, yield_mpa: float,
) -> SectionModulusCheck:
    """Yield check: bending stress vs the permissible ``175/k``."""
    sigma = bending_stress_mpa(moment_kn_m, section_modulus_m3)
    perm = permissible_stress(yield_mpa)
    return SectionModulusCheck(
        stress_mpa=sigma, permissible_mpa=perm,
        utilization=sigma / perm if perm > 0 else float("inf"),
        passes=bool(sigma <= perm),
    )


# ---------------------------------------------------------------------------
# Hull-girder ultimate strength (simplified) — IACS UR S11A format
# ---------------------------------------------------------------------------
def hull_girder_ultimate_moment(
    section_modulus_m3: float, ultimate_stress_mpa: float,
) -> float:
    """Simplified hull-girder ultimate moment ``M_U = sigma_U * Z`` (kN·m).

    ``ultimate_stress_mpa`` is the buckling-reduced ultimate stress of the
    compression flange (e.g. the panel solver's ``critical_stress``).
    """
    return ultimate_stress_mpa * section_modulus_m3 * _KNM_PER_MPA_M3


@dataclass(frozen=True)
class UltimateStrengthCheck:
    """IACS UR S11A hull-girder ultimate-strength utilisation."""

    design_moment_kn_m: float       # gamma_S*M_sw + gamma_W*M_wv
    ultimate_capacity_kn_m: float   # M_U
    factored_capacity_kn_m: float   # M_U / gamma_R
    utilization: float
    passes: bool
    gamma_s: float
    gamma_w: float
    gamma_r: float
    code_reference: str = CODE_REFERENCE


def ultimate_strength_check(
    msw_kn_m: float, mwv_kn_m: float, ultimate_moment_kn_m: float,
    *, gamma_s: float = _GAMMA_S, gamma_w: float = _GAMMA_W,
    gamma_r: float = _GAMMA_R,
) -> UltimateStrengthCheck:
    """IACS UR S11A check: ``gamma_S*M_sw + gamma_W*M_wv <= M_U / gamma_R``.

    Moments are magnitudes for the governing (hogging or sagging) condition.
    """
    design = gamma_s * abs(msw_kn_m) + gamma_w * abs(mwv_kn_m)
    factored_cap = abs(ultimate_moment_kn_m) / gamma_r
    return UltimateStrengthCheck(
        design_moment_kn_m=design,
        ultimate_capacity_kn_m=abs(ultimate_moment_kn_m),
        factored_capacity_kn_m=factored_cap,
        utilization=design / factored_cap if factored_cap > 0 else float("inf"),
        passes=bool(design <= factored_cap),
        gamma_s=gamma_s, gamma_w=gamma_w, gamma_r=gamma_r,
    )


def compression_flange_ultimate_stress(panel, material) -> float:
    """Buckling-reduced ultimate compressive stress (MPa) of a hull flange.

    Thin wrapper over the validated DNV-RP-C201 stiffened-panel solver: returns
    the governing-mode ``critical_stress`` for use as ``sigma_U`` in
    :func:`hull_girder_ultimate_moment`.
    """
    from digitalmodel.structural.structural_analysis.panel_buckling import (
        StiffenedPanelBucklingAnalyzer,
    )

    analyzer = StiffenedPanelBucklingAnalyzer(material)
    # Probe at the material yield; critical_stress is the capacity (probe-independent).
    result = analyzer.check_panel(panel, sigma_x=material.yield_strength)
    return result.critical_stress
