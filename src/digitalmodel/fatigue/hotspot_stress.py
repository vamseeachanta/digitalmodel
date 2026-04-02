"""
Hotspot Stress Methodology
==========================

Implements hotspot (structural) stress determination for welded connections
per DNV-RP-C203 (2021) Section 4.3 and IIW-2259-15 Section 2.2.

Two approaches:

- **Type 'a'** (surface stress extrapolation): linear or quadratic
  extrapolation of surface stresses measured/computed at prescribed
  distances from the weld toe.
- **Type 'b'** (through-thickness linearisation): decompose the
  stress distribution through the plate thickness into membrane + bending.

Read-out points per DNV-RP-C203 Table 4-1:
  - For t ≤ 0.5t_attachment: 0.5t and 1.5t
  - Standard: 0.4t and 1.0t (linear), or 0.4t, 0.9t, 1.4t (quadratic)

References
----------
- DNV-RP-C203 (2021), Section 4.3 — Derivation of hot spot stress
- IIW-2259-15, Section 2.2 — Hot-spot stress approach
- Hobbacher, A.F. (2016), Recommendations for Fatigue Design of Welded
  Joints and Components, IIW document IIW-2259-15
"""

import math
from typing import List, Optional, Tuple, Union

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class HotspotInput(BaseModel):
    """Input data for hotspot stress extrapolation.

    Attributes
    ----------
    plate_thickness : float
        Plate thickness *t* at the weld (mm).
    distances : list[float]
        Distances from weld toe where stresses are evaluated (mm).
        Must be in ascending order.
    stresses : list[float]
        Corresponding stress values (MPa) at each distance.
    method : str
        ``"linear"`` (2-point) or ``"quadratic"`` (3-point).
        Default ``"linear"``.
    """
    plate_thickness: float
    distances: List[float]
    stresses: List[float]
    method: str = "linear"


class HotspotResult(BaseModel):
    """Result of hotspot stress extrapolation.

    Attributes
    ----------
    hotspot_stress : float
        Extrapolated hotspot stress at the weld toe (MPa).
    method : str
        Method used.
    read_points : dict
        Normalised read-out points used (distance/t → stress).
    gradient : float
        Stress gradient (MPa/mm) at the weld toe.
    """
    hotspot_stress: float
    method: str = ""
    read_points: dict = Field(default_factory=dict)
    gradient: float = 0.0


class ThroughThicknessResult(BaseModel):
    """Result of Type 'b' through-thickness linearisation.

    Attributes
    ----------
    membrane_stress : float
        Membrane (average) stress component (MPa).
    bending_stress : float
        Bending stress component at the surface (MPa).
    hotspot_stress : float
        membrane + bending (MPa).
    peak_stress : float
        Non-linear peak stress at the surface (MPa).
    """
    membrane_stress: float
    bending_stress: float
    hotspot_stress: float
    peak_stress: float = 0.0


# ---------------------------------------------------------------------------
# Type 'a' — Surface stress extrapolation
# ---------------------------------------------------------------------------

def extrapolate_hotspot_linear(
    plate_thickness: float,
    stress_at_04t: float,
    stress_at_10t: float,
) -> HotspotResult:
    """Two-point linear extrapolation to the weld toe.

    DNV-RP-C203 (2021) Eq. 4.1::

        σ_hs = 1.67 · σ_{0.4t} − 0.67 · σ_{1.0t}

    Parameters
    ----------
    plate_thickness : float
        Plate thickness *t* (mm).
    stress_at_04t : float
        Stress at 0.4t from weld toe (MPa).
    stress_at_10t : float
        Stress at 1.0t from weld toe (MPa).

    Returns
    -------
    HotspotResult
    """
    hs = 1.67 * stress_at_04t - 0.67 * stress_at_10t
    d1 = 0.4 * plate_thickness
    d2 = 1.0 * plate_thickness
    gradient = (stress_at_04t - stress_at_10t) / (d2 - d1)

    return HotspotResult(
        hotspot_stress=round(hs, 3),
        method="Linear extrapolation (DNV-RP-C203 Eq. 4.1)",
        read_points={"0.4t": stress_at_04t, "1.0t": stress_at_10t},
        gradient=round(gradient, 4),
    )


def extrapolate_hotspot_quadratic(
    plate_thickness: float,
    stress_at_04t: float,
    stress_at_09t: float,
    stress_at_14t: float,
) -> HotspotResult:
    """Three-point quadratic extrapolation to the weld toe.

    DNV-RP-C203 (2021) Eq. 4.2::

        σ_hs = 2.52 · σ_{0.4t} − 2.24 · σ_{0.9t} + 0.72 · σ_{1.4t}

    Parameters
    ----------
    plate_thickness : float
        Plate thickness *t* (mm).
    stress_at_04t, stress_at_09t, stress_at_14t : float
        Stresses at 0.4t, 0.9t, 1.4t from weld toe (MPa).

    Returns
    -------
    HotspotResult
    """
    hs = 2.52 * stress_at_04t - 2.24 * stress_at_09t + 0.72 * stress_at_14t

    d1 = 0.4 * plate_thickness
    d2 = 0.9 * plate_thickness
    gradient = (stress_at_04t - stress_at_09t) / (d2 - d1)

    return HotspotResult(
        hotspot_stress=round(hs, 3),
        method="Quadratic extrapolation (DNV-RP-C203 Eq. 4.2)",
        read_points={
            "0.4t": stress_at_04t,
            "0.9t": stress_at_09t,
            "1.4t": stress_at_14t,
        },
        gradient=round(gradient, 4),
    )


def extrapolate_hotspot(inp: HotspotInput) -> HotspotResult:
    """General-purpose hotspot extrapolation from arbitrary read-out points.

    Uses polynomial fitting (degree 1 for linear, degree 2 for quadratic)
    and evaluates at distance = 0 (weld toe).

    Parameters
    ----------
    inp : HotspotInput

    Returns
    -------
    HotspotResult
    """
    distances = np.asarray(inp.distances, dtype=float)
    stresses = np.asarray(inp.stresses, dtype=float)

    if len(distances) != len(stresses):
        raise ValueError("distances and stresses must have equal length")

    if inp.method == "linear":
        if len(distances) < 2:
            raise ValueError("Linear extrapolation needs at least 2 points")
        degree = 1
    elif inp.method == "quadratic":
        if len(distances) < 3:
            raise ValueError("Quadratic extrapolation needs at least 3 points")
        degree = 2
    else:
        raise ValueError(f"Unknown method '{inp.method}'. Use 'linear' or 'quadratic'.")

    coeffs = np.polyfit(distances, stresses, degree)
    poly = np.poly1d(coeffs)
    hs = float(poly(0.0))

    # Gradient at weld toe = derivative at x=0
    deriv = np.polyder(poly)
    grad = float(deriv(0.0))

    read_pts = {
        f"{d/inp.plate_thickness:.1f}t": s
        for d, s in zip(distances, stresses)
    }

    return HotspotResult(
        hotspot_stress=round(hs, 3),
        method=f"{inp.method.title()} extrapolation (general polynomial, deg {degree})",
        read_points=read_pts,
        gradient=round(grad, 4),
    )


# ---------------------------------------------------------------------------
# Type 'b' — Through-thickness linearisation
# ---------------------------------------------------------------------------

def through_thickness_linearisation(
    plate_thickness: float,
    through_thickness_positions: Union[List[float], np.ndarray],
    through_thickness_stresses: Union[List[float], np.ndarray],
) -> ThroughThicknessResult:
    """Decompose a through-thickness stress distribution per IIW Type 'b'.

    The stress distribution σ(y) through the plate thickness is decomposed
    into:
    - membrane stress σ_m = (1/t) ∫₀ᵗ σ(y) dy
    - bending stress σ_b = (6/t²) ∫₀ᵗ σ(y) · (t/2 − y) dy
    - non-linear peak σ_nl = σ_surface − (σ_m + σ_b)

    The hotspot stress = σ_m + σ_b.

    Per IIW-2259-15 Section 2.2.4.

    Parameters
    ----------
    plate_thickness : float
        Total plate thickness *t* (mm).
    through_thickness_positions : array-like
        Positions through thickness, 0 = weld toe surface, t = root (mm).
    through_thickness_stresses : array-like
        Corresponding stress values (MPa).

    Returns
    -------
    ThroughThicknessResult
    """
    t = plate_thickness
    y = np.asarray(through_thickness_positions, dtype=float)
    s = np.asarray(through_thickness_stresses, dtype=float)

    if len(y) != len(s):
        raise ValueError("positions and stresses must have equal length")
    if len(y) < 2:
        raise ValueError("Need at least 2 through-thickness points")

    # Sort by position
    order = np.argsort(y)
    y = y[order]
    s = s[order]

    # Membrane: average stress
    sigma_m = float(np.trapz(s, y) / t)

    # Bending: first moment about mid-thickness
    sigma_b = float(6.0 / t ** 2 * np.trapz(s * (t / 2.0 - y), y))

    # Surface stress (at y=0)
    sigma_surface = float(np.interp(0.0, y, s))

    # Non-linear peak
    sigma_nl = sigma_surface - (sigma_m + sigma_b)

    return ThroughThicknessResult(
        membrane_stress=round(sigma_m, 3),
        bending_stress=round(sigma_b, 3),
        hotspot_stress=round(sigma_m + sigma_b, 3),
        peak_stress=round(sigma_nl, 3),
    )


# ---------------------------------------------------------------------------
# Convenience: recommended read-out distances
# ---------------------------------------------------------------------------

def recommended_readout_distances(
    plate_thickness: float,
    method: str = "linear",
) -> List[float]:
    """Return recommended stress read-out distances from the weld toe.

    Per DNV-RP-C203 (2021) Table 4-1.

    Parameters
    ----------
    plate_thickness : float
        Plate thickness *t* (mm).
    method : str
        ``"linear"`` → 2 points (0.4t, 1.0t).
        ``"quadratic"`` → 3 points (0.4t, 0.9t, 1.4t).
        ``"fine_mesh"`` → IIW fine mesh (0.4t, 1.0t, 1.5t).

    Returns
    -------
    list[float]
        Distances in mm.
    """
    t = plate_thickness
    if method == "linear":
        return [0.4 * t, 1.0 * t]
    elif method == "quadratic":
        return [0.4 * t, 0.9 * t, 1.4 * t]
    elif method == "fine_mesh":
        return [0.4 * t, 1.0 * t, 1.5 * t]
    else:
        raise ValueError(f"Unknown method '{method}'")
