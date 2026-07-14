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
    """IACS UR S11 wave coefficient C for rule length ``length_m`` (m).

    Piecewise by length per UR S11 (Rev.7, 2010), 2.2.1:
    ``C = 10.75 - ((300 - L)/100)^1.5`` for ``90 <= L <= 300`` m,
    ``C = 10.75`` for ``300 < L <= 350`` m, and
    ``C = 10.75 - ((L - 350)/150)^1.5`` for ``350 < L <= 500`` m.
    Below 90 m the 90-300 m branch is extrapolated (outside the S11
    applicability range; flagged in docs).
    """
    L = length_m
    if L < 90.0:
        # Below the S11 range; extrapolate the 90-300 m branch (flagged in docs).
        return 10.75 - ((300.0 - L) / 100.0) ** 1.5
    if L <= 300.0:
        return 10.75 - ((300.0 - L) / 100.0) ** 1.5
    if L <= 350.0:
        return 10.75
    return 10.75 - ((L - 350.0) / 150.0) ** 1.5


#: IACS UR S11 (Rev.7, 2010) 2.2.1: Cb is not to be taken less than 0.60 in
#: the wave load formulae.
MIN_RULE_BLOCK_COEFFICIENT = 0.60


def rule_block_coefficient(block_coefficient: float) -> float:
    """Block coefficient floored at 0.60 per IACS UR S11 2.2.1."""
    if block_coefficient <= 0.0:
        raise ValueError("block_coefficient must be positive")
    return max(block_coefficient, MIN_RULE_BLOCK_COEFFICIENT)


def bending_distribution_factor(x_over_l: float) -> float:
    """IACS UR S11 distribution factor M for the wave bending moment.

    Per UR S11 (Rev.7, 2010) 2.2.1, Fig. 1: ``M = 0`` at the ends,
    ``M = 1.0`` between ``0.4 L`` and ``0.65 L`` from the aft end, linear
    between (``M = 2.5 x/L`` aft of ``0.4 L``; ``M = (1 - x/L)/0.35``
    forward of ``0.65 L``). ``x_over_l`` runs 0 (aft end of L) to 1.
    """
    xi = x_over_l
    if not (0.0 <= xi <= 1.0):
        raise ValueError("x_over_l must lie in [0, 1]")
    if xi < 0.4:
        return xi / 0.4
    if xi <= 0.65:
        return 1.0
    return (1.0 - xi) / 0.35


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

    Per UR S11 (Rev.7, 2010) 2.2.1 (amidships, distribution factor M = 1):
    ``M_hog = +190 * C * L^2 * B * Cb * 1e-3`` (kN·m) and
    ``M_sag = -110 * C * L^2 * B * (Cb + 0.7) * 1e-3`` (kN·m), with
    ``Cb >= 0.6`` (rule floor, applied here). Values along the length follow
    from :func:`bending_distribution_factor`.
    """
    c = wave_coefficient(length_m)
    cb = rule_block_coefficient(block_coefficient)
    base = c * length_m ** 2 * beam_m
    hog = 0.19 * base * cb
    sag = -0.11 * base * (cb + 0.7)
    return WaveBendingMoment(hogging_kn_m=hog, sagging_kn_m=sag,
                             wave_coefficient=c)


# ---------------------------------------------------------------------------
# Wave shear force — IACS UR S11
# ---------------------------------------------------------------------------
def shear_distribution_factor_f1(x_over_l: float,
                                 block_coefficient: float) -> float:
    """IACS UR S11 distribution factor F1 (positive wave shear force).

    Per UR S11 (Rev.7, 2010) 2.2.2, Fig. 2 (x from the aft end of L):
    0 at the ends; ``1.59 Cb / (Cb + 0.7)`` between ``0.2 L`` and ``0.3 L``;
    0.7 between ``0.4 L`` and ``0.6 L``; 1.0 between ``0.7 L`` and
    ``0.85 L``; linear between. ``Cb >= 0.6`` (rule floor, applied here).
    """
    cb = rule_block_coefficient(block_coefficient)
    a = 1.59 * cb / (cb + 0.7)
    return _piecewise_linear(
        x_over_l,
        ((0.0, 0.0), (0.2, a), (0.3, a), (0.4, 0.7), (0.6, 0.7),
         (0.7, 1.0), (0.85, 1.0), (1.0, 0.0)),
    )


def shear_distribution_factor_f2(x_over_l: float,
                                 block_coefficient: float) -> float:
    """IACS UR S11 distribution factor F2 (negative wave shear force).

    Per UR S11 (Rev.7, 2010) 2.2.2, Fig. 3 (x from the aft end of L):
    0 at the ends; 0.92 between ``0.2 L`` and ``0.3 L``; 0.7 between
    ``0.4 L`` and ``0.6 L``; ``1.73 Cb / (Cb + 0.7)`` between ``0.7 L`` and
    ``0.85 L``; linear between. ``Cb >= 0.6`` (rule floor, applied here).
    """
    cb = rule_block_coefficient(block_coefficient)
    b = 1.73 * cb / (cb + 0.7)
    return _piecewise_linear(
        x_over_l,
        ((0.0, 0.0), (0.2, 0.92), (0.3, 0.92), (0.4, 0.7), (0.6, 0.7),
         (0.7, b), (0.85, b), (1.0, 0.0)),
    )


def _piecewise_linear(x: float, knots: tuple[tuple[float, float], ...]) -> float:
    if not (knots[0][0] <= x <= knots[-1][0]):
        raise ValueError("x_over_l must lie in [0, 1]")
    for (x0, y0), (x1, y1) in zip(knots, knots[1:]):
        if x <= x1:
            return y0 + (y1 - y0) * (x - x0) / (x1 - x0)
    return knots[-1][1]  # pragma: no cover - unreachable by construction


@dataclass(frozen=True)
class WaveShearForce:
    """IACS UR S11 vertical wave shear force at one section (kN)."""

    positive_kn: float         # >= 0 (F1 applied)
    negative_kn: float         # <= 0 (F2 applied)
    f1: float
    f2: float
    wave_coefficient: float
    code_reference: str = CODE_REFERENCE


def wave_shear_force(length_m: float, beam_m: float, block_coefficient: float,
                     x_over_l: float) -> WaveShearForce:
    """IACS UR S11 vertical wave shear force at position ``x_over_l``.

    Per UR S11 (Rev.7, 2010) 2.2.2:
    ``F_wv(+) = +0.30 * F1 * C * L * B * (Cb + 0.7)`` (kN) and
    ``F_wv(-) = -0.30 * F2 * C * L * B * (Cb + 0.7)`` (kN), with the
    distribution factors of Figs. 2 and 3
    (:func:`shear_distribution_factor_f1` / ``_f2``) and ``Cb >= 0.6``
    (rule floor, applied here). ``x_over_l`` runs 0 (aft end of L) to 1.
    """
    c = wave_coefficient(length_m)
    cb = rule_block_coefficient(block_coefficient)
    base = 0.30 * c * length_m * beam_m * (cb + 0.7)
    f1 = shear_distribution_factor_f1(x_over_l, cb)
    f2 = shear_distribution_factor_f2(x_over_l, cb)
    return WaveShearForce(
        positive_kn=base * f1, negative_kn=-base * f2, f1=f1, f2=f2,
        wave_coefficient=c,
    )


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
