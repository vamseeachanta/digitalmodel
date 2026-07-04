# ABOUTME: API 579-1 Part 5 metal-loss RSF — longitudinal LTA (cylinder Folias Mt)
# ABOUTME: + circumferential extent net-section membrane; DNV-RP-F101 combined loading (stub).
"""Circumferential / combined-loading metal-loss fitness-for-service.

Extends the corroded-pipe (B31G / DNV-RP-F101) and longitudinal LTA work with
the two metal-loss assessments that govern a *circumferentially* extensive or
axially-loaded flaw, plus a placeholder for DNV-RP-F101 combined loading.

Three methods are provided:

1. **API 579-1/ASME FFS-1 Part 5 longitudinal LTA — Remaining Strength Factor.**
   The Level-1/Level-2 cylindrical-shell metal-loss screen.  With the Part 5
   shell parameter

       lambda = 1.285 * s / sqrt(D * Tc)

   (``s`` axial flaw length, ``D`` inside diameter, ``Tc`` corroded wall away
   from the LTA) and the Part 5 *cylinder* Folias bulging factor ``M_t`` from
   the 10th-order polynomial of Table 5.2 (valid ``lambda <= 20``), the
   remaining-thickness ratio ``Rt = (t - FCA)/Tc`` gives

       RSF = Rt / (1 - (1/M_t)*(1 - Rt))

   The flaw is acceptable if ``RSF >= RSF_a`` (default 0.90); otherwise the
   MAWP is reduced to ``MAWP_r = MAWP * RSF/RSF_a`` (Part 5 para. 5.4.2.2).

2. **API 579-1 Part 5 circumferential extent — net-section membrane RSF.**
   This is the *net-section / screening* solution for the axial (longitudinal-
   stress) capacity of a circumferentially extensive flaw — it is **NOT** a
   "circumferential Folias" and deliberately does not reuse ``M_t`` above.  A
   Level-1 circumferential extent screen ``c <= 2*s*(E_L/E_C)`` is checked and
   the net-section membrane RSF is

       RSF_circ = 1 - (d/t) * (c / (pi * Dm))

   (``c`` circumferential flaw width, ``Dm`` mean diameter, ``d`` flaw depth).
   The allowable axial membrane stress is ``RSF_circ * sigma_flow``.

3. **DNV-RP-F101 combined loading (Sec. 3.7.4) — STUB / NotImplemented.**
   Internal pressure plus superimposed longitudinal compressive stress reduces
   the pure-pressure capacity by a factor ``H1``.  The exact published ``H1``
   closed form (Sec. 3.7.4) could not be transcribed verbatim from an
   authoritative source, so the combined-loading capacity is **not** computed
   here — see :func:`dnv_f101_combined_loading` and the validation note in
   ``docs/domains/circumferential-defect-validation-2026-06-29.md``.  The
   pure-pressure base is real (delegates to ``dnv_f101_single_defect``); only
   the ``H1`` reduction is withheld rather than approximated.

Units are US customary throughout (inches, psi) unless noted.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional

from digitalmodel.asset_integrity.dnv_rp_f101 import dnv_f101_single_defect
from digitalmodel.codes import API_579, DNV_RP_F101

# ---------------------------------------------------------------------------
# API 579-1/ASME FFS-1 Part 5 — cylinder (longitudinal) Folias factor M_t.
# Table 5.2 polynomial: M_t = sqrt( sum_{i=0..10} a_i * lambda^i ), lambda <= 20.
# Coefficients per API 579-1/ASME FFS-1 Part 5 (longitudinal flaw, cylinder).
# ---------------------------------------------------------------------------
_PART5_MT_COEFFS = (
    1.0010,  # a0
    -0.014195,  # a1
    0.29090,  # a2
    -0.096420,  # a3
    0.020890,  # a4
    -0.0030540,  # a5
    2.9570e-4,  # a6
    -1.8462e-5,  # a7
    7.1553e-7,  # a8
    -1.5631e-8,  # a9
    1.4656e-10,  # a10
)

# Part 5 shell-parameter coefficient: lambda = 1.285 * s / sqrt(D*Tc).
_PART5_LAMBDA_COEFF = 1.285
# Validity limit on the Part 5 shell parameter for the M_t polynomial.
_PART5_LAMBDA_MAX = 20.0
# Default allowable Remaining Strength Factor (API 579-1 Part 2, RSF_a).
_DEFAULT_RSF_A = 0.90


# ---------------------------------------------------------------------------
# Result containers
# ---------------------------------------------------------------------------
@dataclass
class Part5LongitudinalRSFResult:
    """API 579-1 Part 5 longitudinal LTA remaining-strength result."""

    method: str  # "API579-Part5-longitudinal"
    rsf: float  # Remaining Strength Factor
    rsf_allowable: float  # RSF_a (acceptance threshold)
    acceptable: bool  # RSF >= RSF_a
    folias_Mt: float  # Part 5 cylinder Folias factor M_t
    shell_parameter: float  # lambda = 1.285 s / sqrt(D Tc)
    Rt: float  # remaining-thickness ratio (t-FCA)/Tc
    mawp_psi: Optional[float] = None  # supplied MAWP (intact)
    mawp_reduced_psi: Optional[float] = None  # MAWP * RSF/RSF_a when RSF < RSF_a
    details: dict = field(default_factory=dict)
    code_reference: str = API_579.label


@dataclass
class NetSectionCircumferentialResult:
    """API 579-1 Part 5 circumferential-extent net-section membrane result."""

    method: str  # "API579-Part5-circumferential-net-section"
    rsf_netsection: float  # net-section membrane RSF
    sigma_flow_psi: float  # flow stress used
    allowable_axial_stress_psi: float  # RSF_netsection * sigma_flow
    level1_screen_ok: Optional[bool] = None  # c <= 2 s (E_L/E_C) if s given
    details: dict = field(default_factory=dict)
    code_reference: str = API_579.label


# ---------------------------------------------------------------------------
# 1) API 579-1 Part 5 — cylinder Folias factor and longitudinal LTA RSF
# ---------------------------------------------------------------------------
def part5_folias_mt(lam: float) -> float:
    """API 579-1 Part 5 cylinder (longitudinal) Folias factor ``M_t``.

    ``M_t = sqrt( sum_i a_i * lambda^i )`` for the Part 5 cylinder polynomial,
    valid for ``lambda <= 20`` (raises outside that range).
    """
    if lam < 0:
        raise ValueError(f"shell parameter lambda={lam} must be >= 0.")
    if lam > _PART5_LAMBDA_MAX:
        raise ValueError(
            f"lambda={lam:.3f} exceeds the Part 5 M_t polynomial validity "
            f"limit {_PART5_LAMBDA_MAX:.0f}; flaw is beyond the Level-1/2 "
            "cylinder screen."
        )
    poly = sum(c * lam**i for i, c in enumerate(_PART5_MT_COEFFS))
    if poly <= 0:
        raise ValueError(f"non-physical M_t polynomial value {poly} at lambda={lam}.")
    return math.sqrt(poly)


def part5_shell_parameter(D: float, Tc: float, s: float) -> float:
    """API 579-1 Part 5 shell parameter ``lambda = 1.285 s / sqrt(D Tc)``."""
    if D <= 0 or Tc <= 0:
        raise ValueError(f"Invalid geometry: D={D}, Tc={Tc} (need both > 0).")
    if s < 0:
        raise ValueError(f"axial flaw length s={s} must be >= 0.")
    return _PART5_LAMBDA_COEFF * s / math.sqrt(D * Tc)


def api579_part5_longitudinal_rsf(
    D: float,
    Tc: float,
    s: float,
    tmm: float,
    *,
    FCA: float = 0.0,
    rsf_allowable: float = _DEFAULT_RSF_A,
    MAWP_psi: Optional[float] = None,
) -> Part5LongitudinalRSFResult:
    """API 579-1/ASME FFS-1 Part 5 longitudinal LTA Remaining Strength Factor.

    Args:
        D: inside diameter of the cylinder (in).
        Tc: corroded wall thickness away from the LTA (in) — the reference wall
            used for ``lambda`` and ``Rt``.
        s: axial (meridional) length of the flaw (in).
        tmm: minimum measured remaining thickness within the LTA (in).
        FCA: future corrosion allowance (in), subtracted from ``tmm`` for ``Rt``.
        rsf_allowable: acceptance threshold ``RSF_a`` (default 0.90).
        MAWP_psi: intact maximum allowable working pressure; if given and the
            flaw fails (``RSF < RSF_a``), the reduced ``MAWP_r`` is returned.

    Returns:
        :class:`Part5LongitudinalRSFResult` with ``RSF``, ``M_t``, ``lambda``,
        ``Rt``, the accept/reject flag and (on failure) the reduced MAWP.
    """
    if Tc <= 0 or D <= 0:
        raise ValueError(f"Invalid geometry: D={D}, Tc={Tc} (need both > 0).")
    if tmm < 0:
        raise ValueError(f"measured thickness tmm={tmm} must be >= 0.")
    if FCA < 0:
        raise ValueError(f"FCA={FCA} must be >= 0.")

    lam = part5_shell_parameter(D, Tc, s)
    Mt = part5_folias_mt(lam)
    Rt = (tmm - FCA) / Tc
    # Part 5 Level-1/2 longitudinal-extent RSF.
    rsf = Rt / (1.0 - (1.0 / Mt) * (1.0 - Rt))
    acceptable = rsf >= rsf_allowable

    mawp_reduced = None
    if MAWP_psi is not None and not acceptable:
        mawp_reduced = MAWP_psi * (rsf / rsf_allowable)

    return Part5LongitudinalRSFResult(
        method="API579-Part5-longitudinal",
        rsf=rsf,
        rsf_allowable=rsf_allowable,
        acceptable=bool(acceptable),
        folias_Mt=Mt,
        shell_parameter=lam,
        Rt=Rt,
        mawp_psi=MAWP_psi,
        mawp_reduced_psi=mawp_reduced,
        details={
            "D_in": D,
            "Tc_in": Tc,
            "s_in": s,
            "tmm_in": tmm,
            "FCA_in": FCA,
            "within_lambda_limit": bool(lam <= _PART5_LAMBDA_MAX),
        },
        code_reference=API_579.label,
    )


# ---------------------------------------------------------------------------
# 2) API 579-1 Part 5 — circumferential extent, net-section membrane RSF
# ---------------------------------------------------------------------------
def api579_part5_circumferential_netsection(
    Dm: float,
    t: float,
    d: float,
    c: float,
    sigma_flow_psi: float,
    *,
    s: Optional[float] = None,
    E_L: float = 1.0,
    E_C: float = 1.0,
) -> NetSectionCircumferentialResult:
    """API 579-1 Part 5 circumferential-extent **net-section membrane** RSF.

    Net-section (screening) axial capacity of a circumferentially extensive
    metal-loss flaw under longitudinal membrane stress.  This is **not** a
    "circumferential Folias" solution — it is the net-section membrane balance

        RSF_circ = 1 - (d/t) * (c / (pi * Dm))

    and the allowable axial membrane stress ``RSF_circ * sigma_flow``.

    Args:
        Dm: mean pipe diameter (in).
        t: nominal/corroded wall thickness (in).
        d: flaw depth (in), ``0 <= d <= t``.
        c: circumferential flaw width measured along the arc (in).
        sigma_flow_psi: flow stress (psi).
        s: optional axial flaw length (in) for the Level-1 circumferential
            extent screen ``c <= 2 s (E_L/E_C)``.
        E_L, E_C: longitudinal / circumferential weld joint efficiencies
            (default 1.0 for seamless).
    """
    if Dm <= 0 or t <= 0:
        raise ValueError(f"Invalid geometry: Dm={Dm}, t={t} (need both > 0).")
    if not (0.0 <= d <= t):
        raise ValueError(f"flaw depth d={d} must be in [0, t={t}].")
    if c < 0:
        raise ValueError(f"circumferential width c={c} must be >= 0.")
    if E_C <= 0:
        raise ValueError(f"E_C={E_C} must be > 0.")
    circ = math.pi * Dm
    if c > circ:
        raise ValueError(
            f"circumferential width c={c} exceeds circumference " f"pi*Dm={circ:.3f}."
        )

    rsf_circ = 1.0 - (d / t) * (c / circ)
    allowable_axial = rsf_circ * sigma_flow_psi

    screen_ok: Optional[bool] = None
    screen_limit = None
    if s is not None:
        if s < 0:
            raise ValueError(f"axial flaw length s={s} must be >= 0.")
        screen_limit = 2.0 * s * (E_L / E_C)
        screen_ok = c <= screen_limit

    return NetSectionCircumferentialResult(
        method="API579-Part5-circumferential-net-section",
        rsf_netsection=rsf_circ,
        sigma_flow_psi=sigma_flow_psi,
        allowable_axial_stress_psi=allowable_axial,
        level1_screen_ok=(None if screen_ok is None else bool(screen_ok)),
        details={
            "Dm_in": Dm,
            "t_in": t,
            "d_in": d,
            "c_in": c,
            "d_over_t": d / t,
            "circumference_in": circ,
            "theta_fraction": c / circ,
            "screen_limit_in": screen_limit,
            "E_L": E_L,
            "E_C": E_C,
            "solution_type": "net-section membrane (not circumferential Folias)",
        },
        code_reference=API_579.label,
    )


# ---------------------------------------------------------------------------
# 3) DNV-RP-F101 Sec. 3.7.4 combined loading — STUB (no fabricated H1)
# ---------------------------------------------------------------------------
def dnv_f101_combined_loading(
    D: float,
    t: float,
    d: float,
    L: float,
    c: float,
    smts_psi: float,
    sigma_L_psi: float,
    *args,
    **kwargs,
):
    """DNV-RP-F101 Sec. 3.7.4 combined loading — **NOT IMPLEMENTED**.

    Internal pressure plus a superimposed longitudinal compressive stress
    ``sigma_L`` (negative in compression) reduces the pure-pressure capacity of
    a single longitudinal corrosion defect by a factor ``H1``:

        p_corr,comp = [pure-pressure capacity] * H1.

    The pure-pressure base is computed here via
    :func:`digitalmodel.asset_integrity.dnv_rp_f101.dnv_f101_single_defect`
    (real), but the published ``H1`` closed form (DNV-RP-F101 Sec. 3.7.4)
    could not be transcribed verbatim from an authoritative source — the
    equation is typeset as an image in the publicly available editions and the
    extracted text is corrupt.  Rather than approximate or guess ``H1`` (which
    would be a fabricated safety calculation), this path is intentionally left
    unimplemented.

    Known validation anchor (DNV Sesam RP-F101 user manual worked example, for
    a *future* exact implementation, NOT used here): base-case pure-pressure
    capacity ``p_corr = 171.6 bar`` reduces to ``p_corr,comp = 163.31 bar`` for
    ``sigma_L = -190 MPa``, ``c = 50 mm``, ``theta = c/(pi*D) = 0.02``,
    ``d/t = 0.250``, ``gamma_m = 0.85``, ``gamma_d = 1.275``,
    ``f_u = 495.26 MPa``.

    Raises:
        NotImplementedError: always — the H1 factor is not available.
    """
    # Pure-pressure base is real and verifiable; H1 is what is missing.
    base = dnv_f101_single_defect(D, t, d, L, smts_psi)
    raise NotImplementedError(
        "DNV-RP-F101 Sec. 3.7.4 combined-loading factor H1 is not implemented: "
        "the exact published H1 closed form was not obtained from an "
        "authoritative source and will not be approximated. Pure-pressure "
        f"capacity (DNV-RP-F101 single defect) is {base.capacity_pressure_psi:.2f} "
        f"psi; see {DNV_RP_F101.label} Sec. 3.7.4 and the validation doc. "
        f"(inputs c={c}, sigma_L={sigma_L_psi} psi)"
    )
