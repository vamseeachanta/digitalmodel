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
from typing import Optional, Union

import numpy as np
from pydantic import BaseModel, Field


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

def efthymiou_ty_axial(geom: TubularJointGeometry) -> SCFResult:
    """Efthymiou SCF for T/Y joint under axial brace load.

    DNV-RP-C203 (2021), Table B-5; Efthymiou (1988), OTC 4829.

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

    # Chord saddle
    scf_cs = g * t * (1.11 - 3.0 * (b - 0.52) ** 2) * sin_th ** 1.6
    # Chord crown
    scf_cc = g * t * (2.65 + 5.0 * (b - 0.65) ** 2) + t * b * (0.25 * geom.alpha - 3.0) * sin_th ** 0.2
    # Brace saddle
    scf_bs = 1.3 + g * t * 0.187 * b * (1.25 * b ** 0.5 - b ** 2.5) * sin_th ** 1.1
    # Brace crown
    scf_bc = 3.0 + g * t * 0.12 * math.exp(-4.0 * b) * (0.011 * b ** 2 - 0.045) + b * t * sin_th ** 0.2

    scf_chord = max(abs(scf_cs), abs(scf_cc), 1.0)
    scf_brace = max(abs(scf_bs), abs(scf_bc), 1.0)

    return SCFResult(
        scf_chord=round(scf_chord, 3),
        scf_brace=round(scf_brace, 3),
        method="Efthymiou T/Y axial (DNV-RP-C203 Table B-5)",
    )


def efthymiou_ty_ipb(geom: TubularJointGeometry) -> SCFResult:
    """Efthymiou SCF for T/Y joint under in-plane bending.

    DNV-RP-C203 (2021), Table B-5.

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

    DNV-RP-C203 (2021), Table B-5.
    """
    b = geom.beta
    g = geom.gamma
    t = geom.tau
    th = math.radians(geom.theta)
    sin_th = math.sin(th)

    scf_cs = g * t * b * (1.7 - 1.05 * b ** 3) * sin_th ** 1.6
    scf_bs = t ** 0.9 * (0.12 * math.exp(-4.0 * b) + 0.011 * b ** 2 - 0.045)
    scf_bs = max(scf_bs, 0.0)

    scf_chord = max(abs(scf_cs), 1.0)
    scf_brace = max(abs(scf_bs), 1.0)

    return SCFResult(
        scf_chord=round(scf_chord, 3),
        scf_brace=round(scf_brace, 3),
        method="Efthymiou T/Y out-of-plane bending (DNV-RP-C203 Table B-5)",
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


# ---------------------------------------------------------------------------
# Plate / weld SCFs — DNV-RP-C203 Appendix D
# ---------------------------------------------------------------------------

class PlateGeometry(BaseModel):
    """Geometry of a plated connection for SCF calculation.

    Attributes
    ----------
    t : float — plate thickness (mm)
    e : float — eccentricity / misalignment (mm), default 0
    l_weld : float — weld leg length (mm), default 0
    W : float — attachment width (mm), default 0
    """
    t: float
    e: float = 0.0
    l_weld: float = 0.0
    W: float = 0.0


def scf_butt_weld_misalignment(
    t1: float,
    t2: float,
    e: float = 0.0,
) -> float:
    """SCF for butt weld with thickness transition and/or misalignment.

    DNV-RP-C203 (2021), Appendix D, Eq. (D.1):

        SCF = 1 + 3 * (e / t) * exp(-t / L)

    Simplified form for axial misalignment where L → ∞ for long plates.

    Parameters
    ----------
    t1, t2 : float  — Plate thicknesses on either side (mm).
    e : float        — Linear misalignment (mm).

    Returns
    -------
    float : SCF (≥ 1.0)
    """
    t = min(t1, t2)
    if t <= 0:
        raise ValueError("Plate thickness must be positive")
    # Thickness transition
    scf_thickness = 1.0 + 2.5 * (abs(t2 - t1) / t) * (1.0 / (1.0 + (t2 / t1) ** 1.5))
    # Misalignment
    scf_misalign = 1.0 + 3.0 * abs(e) / t
    return round(max(scf_thickness, scf_misalign, 1.0), 4)


def scf_cruciform_joint(
    t: float,
    a_weld: float,
    e: float = 0.0,
) -> float:
    """SCF for cruciform (load-carrying fillet weld) joint.

    DNV-RP-C203 (2021), Appendix D, Table D-3.

    Parameters
    ----------
    t : float     — plate thickness (mm)
    a_weld : float — fillet weld throat thickness (mm)
    e : float     — misalignment (mm)

    Returns
    -------
    float : SCF
    """
    if t <= 0 or a_weld <= 0:
        raise ValueError("Thickness and weld throat must be positive")
    # Km from misalignment
    km = 1.0 + 6.0 * abs(e) / t
    # Weld geometry correction
    kw = 1.0 + 0.5 * (t / a_weld - 1.0) ** 0.5 if a_weld < t else 1.0
    return round(max(km * kw, 1.0), 4)


def scf_fillet_weld_toe(
    t: float,
    theta: float = 45.0,
    r: float = 0.0,
) -> float:
    """SCF at fillet weld toe.

    Semi-empirical equation from IIW-2259-15, Eq. (3.3-1).

    Parameters
    ----------
    t : float     — plate thickness (mm)
    theta : float — weld toe angle (degrees), default 45
    r : float     — weld toe radius (mm), default 0 (sharp)

    Returns
    -------
    float : SCF
    """
    if t <= 0:
        raise ValueError("Plate thickness must be positive")
    th_rad = math.radians(theta)
    if r <= 0:
        r = 0.5  # assume a minimal radius for sharp toe
    scf = 1.0 + 0.27 * (th_rad ** 0.37) * (t / r) ** 0.25
    return round(max(scf, 1.0), 4)


# ---------------------------------------------------------------------------
# Parametric SCF for common geometries
# ---------------------------------------------------------------------------

def scf_shoulder_fillet(
    D: float,
    d: float,
    r: float,
) -> float:
    """SCF for a stepped shaft with shoulder fillet under tension.

    Peterson's Stress Concentration Factors, Chart 3.1.

    Parameters
    ----------
    D : float — larger diameter (mm)
    d : float — smaller diameter (mm)
    r : float — fillet radius (mm)

    Returns
    -------
    float : SCF
    """
    if d <= 0 or D <= d or r <= 0:
        raise ValueError("Require D > d > 0 and r > 0")
    h = (D - d) / 2.0
    ratio = h / r
    # Peterson approximation (simplified)
    C1 = 0.926 + 1.157 * math.sqrt(ratio) - 0.099 * ratio
    C2 = 0.012 - 3.036 * math.sqrt(ratio) + 0.961 * ratio
    t_ratio = 2.0 * h / d  # h/r normalised
    scf = C1 + C2 * t_ratio
    return round(max(scf, 1.0), 4)


def scf_circumferential_groove(
    D: float,
    d: float,
    r: float,
) -> float:
    """SCF for a circumferential U-groove under tension.

    Peterson's Stress Concentration Factors, Chart 2.21.

    Parameters
    ----------
    D : float — outer diameter (mm)
    d : float — root diameter at groove (mm)
    r : float — groove root radius (mm)

    Returns
    -------
    float : SCF
    """
    if d <= 0 or D <= d or r <= 0:
        raise ValueError("Require D > d > 0 and r > 0")
    t = (D - d) / 2.0
    scf = 1.0 + 2.0 * math.sqrt(t / r)
    return round(scf, 4)
