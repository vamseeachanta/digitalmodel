"""Plate, weld and common-geometry SCFs re-exported by scf_library."""

import math

from pydantic import BaseModel


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
