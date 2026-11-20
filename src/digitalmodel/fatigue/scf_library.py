"""
Stress Concentration Factor (SCF) Library
==========================================

Parametric SCF equations for fatigue hotspot assessment of welded
steel structures, covering:

- **Tubular joints**: Efthymiou equations for T/Y, K, KT joints
  (DNV-RP-C203 Appendix B, ISO 19902 Annex A.16.5)
- **Plate connections**: butt welds, fillet welds, cruciform joints
  (DNV-RP-C203 Appendix D, IIW recommendations)
- **Common geometries**: shoulder fillet, circumferential groove

All equations return the SCF as a dimensionless multiplier applied to
the nominal stress range to obtain the hotspot stress range.

References
----------
- DNV-RP-C203 (2021), Appendix B (tubular joints) & Appendix D (plates)
- Efthymiou, M. (1988), "Development of SCF formulae …", OTC 4829
- ISO 19902:2020, Annex A.16.5
- IIW-2259-15, Section 3.3 — SCF for misalignment
"""

import math
from typing import Literal

from pydantic import BaseModel

from ._scf_details import (
    PlateGeometry as PlateGeometry,
    scf_butt_weld_misalignment as scf_butt_weld_misalignment,
    scf_circumferential_groove as scf_circumferential_groove,
    scf_cruciform_joint as scf_cruciform_joint,
    scf_fillet_weld_toe as scf_fillet_weld_toe,
    scf_shoulder_fillet as scf_shoulder_fillet,
)


# ---------------------------------------------------------------------------
# Pydantic input/output models
# ---------------------------------------------------------------------------

class TubularJointGeometry(BaseModel):
    """Geometry parameters for a tubular joint.

    Attributes
    ----------
    D : float  — chord outside diameter (mm)
    T : float  — chord wall thickness (mm)
    d : float  — brace outside diameter (mm)
    t : float  — brace wall thickness (mm)
    theta : float — brace-to-chord angle (degrees), default 90
    g : float  — gap between braces (mm), for K/KT joints, default 0
    L : float  — chord length between supports (mm), default 0 (auto)
    """
    D: float
    T: float
    d: float
    t: float
    theta: float = 90.0
    g: float = 0.0
    L: float = 0.0

    @property
    def beta(self) -> float:
        """Brace-to-chord diameter ratio d/D."""
        return self.d / self.D

    @property
    def gamma(self) -> float:
        """Chord radius-to-thickness ratio D/(2T)."""
        return self.D / (2.0 * self.T)

    @property
    def tau(self) -> float:
        """Brace-to-chord thickness ratio t/T."""
        return self.t / self.T

    @property
    def alpha(self) -> float:
        """Chord length parameter 2L/D.  Uses 2.5*D if L not set."""
        L = self.L if self.L > 0 else 2.5 * self.D
        return 2.0 * L / self.D

    @property
    def zeta(self) -> float:
        """Gap parameter g/D for K-joints."""
        return self.g / self.D


class SCFResult(BaseModel):
    """Result of an SCF calculation.

    Attributes
    ----------
    scf_chord : float — SCF at the chord side
    scf_brace : float — SCF at the brace side
    governing : float — max(scf_chord, scf_brace)
    method : str — description of the method used
    """
    scf_chord: float
    scf_brace: float
    governing: float = 0.0
    method: str = ""

    def model_post_init(self, __context: object) -> None:
        if self.governing == 0.0:
            self.governing = max(self.scf_chord, self.scf_brace)


# ---------------------------------------------------------------------------
# Efthymiou SCF equations — T/Y joints
# ---------------------------------------------------------------------------

def short_chord_factor_f1(beta: float, gamma: float, alpha: float) -> float:
    """Efthymiou short-chord correction factor F1 (axial saddle terms).

    DNV-RP-C203 (2021) Appendix B, Table B-1 / Efthymiou (1988), OTC 4829.
    For short chords (``alpha < 12``) the brace-**axial** *saddle* SCFs — chord
    saddle (Eqn. 1) and brace saddle (Eqn. 3) — are reduced by F1 for
    **fixed chord ends**::

        F1 = 1 - (0.83*beta - 0.56*beta^2 - 0.02) * gamma^0.23
                 * exp(-0.21 * gamma^(-1.16) * alpha^2.5)

    For ``alpha >= 12`` (long chord) the saddle SCFs are not reduced and
    ``F1 = 1.0``. Crown terms (Eqns. 2, 4) are never reduced.

    Companion corrections are :func:`short_chord_factor_f2` (pinned axial)
    and :func:`short_chord_factor_f3` (out-of-plane bending).

    Parameters
    ----------
    beta : float  — brace/chord diameter ratio d/D.
    gamma : float — chord radius/thickness ratio D/(2T).
    alpha : float — chord length parameter 2L/D.
    """
    if alpha >= 12.0:
        return 1.0
    return 1.0 - (0.83 * beta - 0.56 * beta**2 - 0.02) * gamma**0.23 * math.exp(
        -0.21 * gamma ** (-1.16) * alpha**2.5
    )


def short_chord_factor_f2(beta: float, gamma: float, alpha: float) -> float:
    """Axial saddle F2 for general/pinned chord-end fixity.

    DNV-RP-C203 (2021) Appendix B, Table B-1, axial load, general fixity,
    short-chord column; Efthymiou (1988), OTC 4829::

        F2 = 1 - (1.43*beta - 0.97*beta^2 - 0.03) * gamma^0.04
                 * exp(-0.71 * gamma^(-1.38) * alpha^2.5)

    ``beta=d/D``, ``gamma=D/(2T)``, ``alpha=2L/D``. For ``alpha >= 12``,
    return exactly 1.0. F2 corrects both axial saddles, never the crowns.
    """
    if alpha >= 12.0:
        return 1.0
    return 1.0 - (1.43 * beta - 0.97 * beta**2 - 0.03) * gamma**0.04 * math.exp(
        -0.71 * gamma ** (-1.38) * alpha**2.5
    )


def short_chord_factor_f3(beta: float, gamma: float, alpha: float) -> float:
    """OPB saddle short-chord factor F3.

    DNV-RP-C203 (2021) Appendix B, Table B-1, out-of-plane bending,
    saddle short-chord column; Efthymiou (1988), OTC 4829::

        F3 = 1 - 0.55*beta^1.8 * gamma^0.16
                 * exp(-0.49 * gamma^(-0.89) * alpha^1.8)

    ``beta=d/D``, ``gamma=D/(2T)``, ``alpha=2L/D``. For ``alpha >= 12``,
    return exactly 1.0. IPB crown terms take no short-chord correction.
    """
    if alpha >= 12.0:
        return 1.0
    return 1.0 - 0.55 * beta**1.8 * gamma**0.16 * math.exp(
        -0.49 * gamma ** (-0.89) * alpha**1.8
    )


def _axial_base_terms(
    b: float, g: float, t: float, a: float, sin_th: float,
) -> tuple[float, float, float, float]:
    """Uncorrected fixed-end axial Eqns 1–4 (DNV-RP-C203 App. B Table B-1)."""
    # Published Efthymiou (1988, OTC 4829) / DNV-RP-C203 App. B T/Y axial set.
    # Chord saddle:  gamma * tau^1.1 * (1.11 - 3(beta-0.52)^2) * sin(theta)^1.6
    scf_cs = g * t**1.1 * (1.11 - 3.0 * (b - 0.52) ** 2) * sin_th**1.6
    # Chord crown:   gamma^0.2 * tau * (2.65 + 5(beta-0.65)^2)
    #                + tau * beta * (0.25*alpha - 3) * sin(theta)
    scf_cc = (
        g**0.2 * t * (2.65 + 5.0 * (b - 0.65) ** 2) + t * b * (0.25 * a - 3.0) * sin_th
    )
    # Brace saddle:  1.3 + gamma * tau^0.52 * alpha^0.1
    #                * (0.187 - 1.25*beta^1.1*(beta-0.96)) * sin(theta)^(2.7 - 0.01*alpha)
    scf_bs = 1.3 + g * t**0.52 * a**0.1 * (
        0.187 - 1.25 * b**1.1 * (b - 0.96)
    ) * sin_th ** (2.7 - 0.01 * a)
    # Brace crown:   3 + gamma^1.2 * (0.12*exp(-4*beta) + 0.011*beta^2 - 0.045)
    #                + beta * tau * (0.1*alpha - 1.2)
    scf_bc = (
        3.0
        + g**1.2 * (0.12 * math.exp(-4.0 * b) + 0.011 * b**2 - 0.045)
        + b * t * (0.1 * a - 1.2)
    )
    return scf_cs, scf_cc, scf_bs, scf_bc


def efthymiou_ty_axial(
    geom: TubularJointGeometry, *, chord_ends: Literal["fixed", "pinned"] = "fixed",
) -> SCFResult:
    """T/Y axial SCF, DNV-RP-C203 (2021) App. B Table B-1; OTC 4829 (1988).

    Fixed ends use F1 on both saddles; pinned ends use F2 for ``alpha < 12``.
    Default ``L=2.5*D`` gives ``alpha=5``. Crowns are never factor-corrected.
    Pinned approximation: F2 applied to fixed-end saddle form; Eqn-5/6/7
    general-fixity base equations deferred because the appendix is unavailable.
    Fixed-end crowns (Eqns 2/4) and saddles (Eqns 1/3) are retained.
    An upper bound is not established: retained crowns may understate the
    general-fixity brace SCF at oblique angles when the brace crown governs.
    """
    if chord_ends not in ("fixed", "pinned"):
        raise ValueError("chord_ends must be 'fixed' or 'pinned'")
    b, g, t, a = geom.beta, geom.gamma, geom.tau, geom.alpha
    sin_th = math.sin(math.radians(geom.theta))
    scf_cs, scf_cc, scf_bs, scf_bc = _axial_base_terms(b, g, t, a, sin_th)
    factor = short_chord_factor_f1 if chord_ends == "fixed" else short_chord_factor_f2
    correction = factor(b, g, a)
    scf_cs *= correction
    scf_bs *= correction

    scf_chord = max(abs(scf_cs), abs(scf_cc), 1.0)
    scf_brace = max(abs(scf_bs), abs(scf_bc), 1.0)

    factor_name = "F1" if chord_ends == "fixed" else "F2"
    approximation = (
        "; fixed-end base approximation; upper bound not established"
        if chord_ends == "pinned" else ""
    )
    return SCFResult(
        scf_chord=round(scf_chord, 3),
        scf_brace=round(scf_brace, 3),
        method=f"Efthymiou T/Y axial, {chord_ends} chord ends{approximation} "
        f"(DNV-RP-C203 App. B Table B-1, {factor_name})",
    )


def efthymiou_ty_ipb(geom: TubularJointGeometry) -> SCFResult:
    """Efthymiou SCF for T/Y joint under in-plane bending.

    DNV-RP-C203 (2021), Table B-5.
    No short-chord correction — DNV-RP-C203 App. B marks IPB crowns 'None'.

    Parameters
    ----------
    geom : TubularJointGeometry

    Returns
    -------
    SCFResult
    """
    b = geom.beta
    g = geom.gamma
    t = geom.tau
    th = math.radians(geom.theta)
    sin_th = math.sin(th)

    # Chord crown
    scf_cc = 1.45 * b * t * g ** 0.7 * sin_th ** 0.06
    # Brace crown
    scf_bc = 1.0 + 0.65 * b * t ** 0.4 * g * (1.09 - 0.77 * b) * sin_th ** 0.06

    scf_chord = max(abs(scf_cc), 1.0)
    scf_brace = max(abs(scf_bc), 1.0)

    return SCFResult(
        scf_chord=round(scf_chord, 3),
        scf_brace=round(scf_brace, 3),
        method="Efthymiou T/Y in-plane bending (DNV-RP-C203 Table B-5)",
    )


def efthymiou_ty_opb(geom: TubularJointGeometry) -> SCFResult:
    """Efthymiou SCF for T/Y joint under out-of-plane bending.

    DNV-RP-C203 (2021) App. B, Table B-1, Eqn 10; OTC 4829 (1988).
    F3 reduces the chord saddle for ``alpha < 12``; ``alpha >= 12`` is unreduced.
    Eqn 11 could not be verified against the appendix. The legacy brace-saddle
    expression is unchanged and receives no F3; the brace SCF is unverified.
    """
    b = geom.beta
    g = geom.gamma
    t = geom.tau
    th = math.radians(geom.theta)
    sin_th = math.sin(th)

    scf_cs = g * t * b * (1.7 - 1.05 * b ** 3) * sin_th ** 1.6
    scf_cs *= short_chord_factor_f3(b, g, geom.alpha)
    scf_bs = t ** 0.9 * (0.12 * math.exp(-4.0 * b) + 0.011 * b ** 2 - 0.045)
    scf_bs = max(scf_bs, 0.0)

    scf_chord = max(abs(scf_cs), 1.0)
    scf_brace = max(abs(scf_bs), 1.0)

    return SCFResult(
        scf_chord=round(scf_chord, 3),
        scf_brace=round(scf_brace, 3),
        method="Efthymiou T/Y out-of-plane bending "
        "(DNV-RP-C203 App. B Table B-1, Eqn 10, F3; legacy brace unverified)",
    )


# ---------------------------------------------------------------------------
# K-joint SCF — simplified Efthymiou
# ---------------------------------------------------------------------------

def efthymiou_k_axial(
    geom: TubularJointGeometry,
) -> SCFResult:
    """Efthymiou SCF for K-joint (balanced axial).

    Simplified formulation from DNV-RP-C203 Table B-6.
    The gap parameter ζ = g/D must be positive.
    """
    b = geom.beta
    g = geom.gamma
    t = geom.tau
    z = geom.zeta
    th = math.radians(geom.theta)
    sin_th = math.sin(th)

    # Chord saddle (governing for K-joints typically)
    scf_cs = (t ** 0.9 * g ** 0.5 * (0.67 - b ** 2 + 1.16 * b)
              * sin_th ** (1.64 + 0.29 * b ** (-0.38))
              * (1.0 + 1.97 * z ** 0.25))
    # Brace saddle
    scf_bs = 1.0 + (scf_cs - 1.0) * 0.63

    scf_chord = max(abs(scf_cs), 1.0)
    scf_brace = max(abs(scf_bs), 1.0)

    return SCFResult(
        scf_chord=round(scf_chord, 3),
        scf_brace=round(scf_brace, 3),
        method="Efthymiou K-joint balanced axial (DNV-RP-C203 Table B-6)",
    )
