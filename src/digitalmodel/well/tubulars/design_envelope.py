# ABOUTME: Triaxial (von Mises) stress design envelope for casing/tubing strings (WRK-376)
# ABOUTME: API biaxial ellipse, isotropic VME, and anisotropic CRA/chrome VME envelopes

"""
Triaxial Stress Design Envelope for Casing / Tubing Strings
=============================================================
Implements three standard design envelope representations used in casing/tubing
string design per API TR 5C3 / ISO 10400:

1. **IsotropicVmeEnvelope**  — Von Mises equivalent (VME) ellipse for isotropic
   steel grades (J-55, N-80, L-80, P-110, Q-125).
2. **ApiEllipseEnvelope**    — API biaxial correction applied to rated burst and
   collapse capacities; burst/collapse limits corrected for axial load.
3. **AnisotropicVmeEnvelope** — Modified VME for CRA (corrosion resistant alloy) /
   chrome grades with different hoop and axial yield strengths (e.g. 25Cr duplex).

All envelopes produce plot-ready (axial_force_lbf, differential_pressure_psi)
coordinate pairs in numpy arrays for immediate charting or utilisation checks.

Sign conventions
----------------
- Axial force > 0 = tension;  < 0 = compression
- Differential pressure > 0 = burst (internal > external);
                          < 0 = collapse (external > internal)

Key references
--------------
- API TR 5C3 / ISO 10400:2011 — Formulae and equations for casing/tubing properties
- Bourgoyne et al., *Applied Drilling Engineering*, SPE Textbook Vol. 2, Ch. 7
- SPE 48330 — Casing design for directional and horizontal wells
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional

import numpy as np


# ---------------------------------------------------------------------------
# Geometry dataclass
# ---------------------------------------------------------------------------

@dataclass
class TubularGeometry:
    """Physical and material properties of a casing or tubing string.

    Parameters
    ----------
    od_in : float
        Nominal outside diameter (inches).
    id_in : float
        Nominal inside diameter (inches).
    smys_psi : float
        Specified Minimum Yield Strength (psi).  Isotropic axial yield.
    burst_rating_psi : float
        Rated internal yield (burst) pressure from API tables (psi).
    collapse_rating_psi : float
        Rated collapse pressure from API tables (psi).  Positive value.
    """

    od_in: float
    id_in: float
    smys_psi: float
    burst_rating_psi: float
    collapse_rating_psi: float

    # Derived attributes computed after __post_init__
    wall_thickness_in: float = field(init=False)
    mean_diameter_in: float = field(init=False)
    cross_section_area_in2: float = field(init=False)
    yield_force_lbf: float = field(init=False)

    def __post_init__(self) -> None:
        if self.od_in <= self.id_in:
            raise ValueError(
                f"OD must exceed ID: od={self.od_in}, id={self.id_in}"
            )
        if self.smys_psi <= 0.0:
            raise ValueError(f"SMYS must be positive, got {self.smys_psi}")
        self.wall_thickness_in = (self.od_in - self.id_in) / 2.0
        self.mean_diameter_in = (self.od_in + self.id_in) / 2.0
        self.cross_section_area_in2 = (
            math.pi / 4.0 * (self.od_in**2 - self.id_in**2)
        )
        self.yield_force_lbf = self.smys_psi * self.cross_section_area_in2


# ---------------------------------------------------------------------------
# Stress helper functions
# ---------------------------------------------------------------------------

def compute_hoop_stress(
    differential_pressure_psi: float,
    od_in: float,
    id_in: float,
) -> float:
    """Thin-wall hoop (tangential) stress from differential pressure.

    Uses mean-diameter thin-wall approximation:
        sigma_t = p_diff * D_mean / (2 * t)

    Parameters
    ----------
    differential_pressure_psi : float
        Internal minus external pressure (psi).  Positive = burst direction.
    od_in, id_in : float
        Outside and inside diameter (inches).

    Returns
    -------
    float
        Hoop stress (psi).  Positive = tensile (burst load).
    """
    t = (od_in - id_in) / 2.0
    d_mean = (od_in + id_in) / 2.0
    return differential_pressure_psi * d_mean / (2.0 * t)


def compute_vme_stress(
    sigma_axial_psi: float,
    sigma_hoop_psi: float,
    tau_psi: float = 0.0,
) -> float:
    """Von Mises Equivalent (VME) stress for a tubular cross-section.

    Full expression including torsional shear:
        sigma_vme = sqrt(sa^2 + st^2 - sa*st + 3*tau^2)

    Parameters
    ----------
    sigma_axial_psi : float
        Axial stress component (psi).  Positive = tensile.
    sigma_hoop_psi : float
        Hoop (tangential) stress component (psi).
    tau_psi : float, optional
        Torsional shear stress (psi).  Default 0.0.

    Returns
    -------
    float
        Von Mises equivalent stress (psi), always >= 0.
    """
    sa, st, tau = sigma_axial_psi, sigma_hoop_psi, tau_psi
    val = sa**2 + st**2 - sa * st + 3.0 * tau**2
    return math.sqrt(max(0.0, val))


# ---------------------------------------------------------------------------
# Isotropic VME Envelope
# ---------------------------------------------------------------------------

class IsotropicVmeEnvelope:
    """VME design ellipse for isotropic steel grades.

    Sweeps axial force from -F_yield to +F_yield and at each step solves for
    the differential pressure that puts the VME stress exactly on the design
    limit (SMYS / design_factor).

    The VME constraint is:
        sqrt(sigma_a^2 + sigma_t^2 - sigma_a*sigma_t) = SMYS / DF

    Solving for sigma_t (quadratic in sigma_t):
        sigma_t^2 - sigma_a*sigma_t + (sigma_a^2 - S_allow^2) = 0

    where S_allow = SMYS / DF.  The two roots correspond to the burst (+)
    and collapse (-) branches of the VME ellipse.
    """

    def __init__(
        self,
        geometry: TubularGeometry,
        design_factor: float,
    ) -> None:
        if design_factor <= 0.0:
            raise ValueError(
                f"design_factor must be positive, got {design_factor}"
            )
        self.geometry = geometry
        self.design_factor = design_factor

    def compute(self, n_points: int = 100) -> np.ndarray:
        """Return envelope as (n_points, 2) array of (axial_force_lbf, dp_psi).

        Parameters
        ----------
        n_points : int
            Number of points to generate along the full ellipse sweep.

        Returns
        -------
        np.ndarray, shape (n_points, 2)
            Column 0: axial force (lbf), Column 1: differential pressure (psi).
            Positive dp = burst side; negative dp = collapse side.
        """
        geom = self.geometry
        s_allow = geom.smys_psi / self.design_factor
        area = geom.cross_section_area_in2
        fy = geom.yield_force_lbf
        t = geom.wall_thickness_in
        d_mean = geom.mean_diameter_in
        # dp_factor converts sigma_t → dp: dp = sigma_t * (2*t / d_mean)
        dp_factor = 2.0 * t / d_mean

        # Split n_points evenly between burst and collapse halves
        half = n_points // 2
        extra = n_points - 2 * half  # handle odd counts

        # Burst branch: positive dp (tension-side hoop) — sweep tension only
        # Collapse branch: negative dp — sweep compression only
        # Together they form the full ellipse by sweeping axial force both ways.
        # Strategy: sweep axial stress from -S_allow to +S_allow; at each value
        # pick the positive (burst) root for the top half and the negative
        # (collapse) root for the bottom half.

        axial_stress_range = np.linspace(-s_allow, s_allow, n_points)

        axial_forces = np.empty(n_points)
        dp_values = np.empty(n_points)

        for i, sa in enumerate(axial_stress_range):
            # Quadratic: sigma_t^2 - sa*sigma_t + (sa^2 - s_allow^2) = 0
            # discriminant = sa^2 - 4*(sa^2 - s_allow^2)
            #              = 4*s_allow^2 - 3*sa^2
            disc = 4.0 * s_allow**2 - 3.0 * sa**2
            if disc < 0.0:
                disc = 0.0
            sqrt_disc = math.sqrt(disc)
            # Two roots: (sa ± sqrt(disc)) / 2
            st_pos = (sa + sqrt_disc) / 2.0  # burst root
            st_neg = (sa - sqrt_disc) / 2.0  # collapse root

            # Choose burst or collapse based on sweep half
            # Top half (burst): use positive hoop stress root
            if i < n_points // 2:
                st = st_pos
            else:
                st = st_neg

            axial_forces[i] = sa * area
            dp_values[i] = st * dp_factor

        return np.column_stack([axial_forces, dp_values])


# ---------------------------------------------------------------------------
# API Biaxial Ellipse Envelope
# ---------------------------------------------------------------------------

class ApiEllipseEnvelope:
    """API biaxial correction to rated burst and collapse for axial load.

    Applies the API TR 5C3 biaxial correction factors to the rated burst and
    collapse pressures as a function of axial stress:

    Burst (internal yield pressure corrected for axial load):
        p_burst(sa) = p_burst_rated * (sqrt(1 - 0.75*(sa/Y)^2) - 0.5*(sa/Y))

    Collapse (corrected collapse pressure):
        p_collapse(sa) = p_collapse_rated * (sqrt(1 - 0.75*(sa/Y)^2) - 0.5*(sa/Y))

    where sa = F_axial / A_steel, Y = SMYS.

    Note: The same biaxial factor applies to both burst and collapse in the API
    simplified formulation, capturing the Lame ellipse interaction.
    """

    def __init__(
        self,
        geometry: TubularGeometry,
        design_factor: float,
    ) -> None:
        self.geometry = geometry
        self.design_factor = design_factor

    def _biaxial_factor(self, sa_ratio: float) -> float:
        """Compute API biaxial correction factor.

        Parameters
        ----------
        sa_ratio : float
            sigma_axial / SMYS.  Positive = tension.

        Returns
        -------
        float
            Correction factor in [0, 1].
        """
        r = sa_ratio
        val = 1.0 - 0.75 * r**2
        factor = math.sqrt(max(0.0, val)) - 0.5 * r
        return max(0.0, factor)

    def compute(self, n_points: int = 100) -> np.ndarray:
        """Return API ellipse envelope as (n_points, 2) array.

        Sweeps axial force from -F_yield to +F_yield and computes the
        corrected burst/collapse limit at each axial load level.

        The burst side forms the top half of the envelope (dp > 0) and the
        collapse side forms the bottom half (dp < 0).

        Returns
        -------
        np.ndarray, shape (n_points, 2)
            Column 0: axial force (lbf), Column 1: differential pressure (psi).
        """
        geom = self.geometry
        df = self.design_factor
        area = geom.cross_section_area_in2
        smys = geom.smys_psi
        fy = geom.yield_force_lbf

        burst_rated = geom.burst_rating_psi / df
        collapse_rated = geom.collapse_rating_psi / df

        half = n_points // 2
        remainder = n_points - 2 * half

        # Burst half: axial from -fy to +fy (positive dp)
        f_burst = np.linspace(-fy, fy, half + remainder)
        # Collapse half: axial from +fy back to -fy (negative dp)
        f_collapse = np.linspace(fy, -fy, half)

        axial_all = np.concatenate([f_burst, f_collapse])
        dp_all = np.empty(len(axial_all))

        for i, f_ax in enumerate(axial_all):
            sa = f_ax / area
            sa_ratio = sa / smys
            bf = self._biaxial_factor(sa_ratio)
            if i < len(f_burst):
                dp_all[i] = burst_rated * bf
            else:
                dp_all[i] = -collapse_rated * bf

        return np.column_stack([axial_all, dp_all])


# ---------------------------------------------------------------------------
# Anisotropic VME Envelope (CRA / chrome grades)
# ---------------------------------------------------------------------------

class AnisotropicVmeEnvelope:
    """Anisotropic VME envelope for CRA / chrome grades.

    CRA (Corrosion Resistant Alloy) and chrome grades exhibit anisotropic
    yield behaviour due to texture effects during manufacturing:
        - Axial yield: Y_a = SMYS (as specified)
        - Hoop yield:  Y_t = yield_hoop_ratio * Y_a

    The modified VME criterion is:
        (sigma_a / Y_a)^2 + (sigma_t / Y_t)^2 - sigma_a*sigma_t/(Y_a*Y_t) = 1

    Rearranging for sigma_t at a given sigma_a (quadratic in sigma_t):
        (1/Y_t^2)*sigma_t^2 - (sigma_a/(Y_a*Y_t))*sigma_t
          + ((sigma_a/Y_a)^2 - 1) = 0

    Typical values: 25Cr duplex  → yield_hoop_ratio ≈ 1.05
                    13Cr martensitic → yield_hoop_ratio ≈ 1.0
    """

    def __init__(
        self,
        geometry: TubularGeometry,
        yield_hoop_ratio: float,
        design_factor: float,
    ) -> None:
        if yield_hoop_ratio <= 0.0:
            raise ValueError(
                f"yield_hoop_ratio must be positive, got {yield_hoop_ratio}"
            )
        if design_factor <= 0.0:
            raise ValueError(
                f"design_factor must be positive, got {design_factor}"
            )
        self.geometry = geometry
        self.yield_hoop_ratio = yield_hoop_ratio
        self.design_factor = design_factor

    def compute(self, n_points: int = 100) -> np.ndarray:
        """Return anisotropic VME envelope as (n_points, 2) array.

        Returns
        -------
        np.ndarray, shape (n_points, 2)
            Column 0: axial force (lbf), Column 1: differential pressure (psi).
        """
        geom = self.geometry
        df = self.design_factor
        ya = geom.smys_psi / df           # effective axial yield with DF
        yt = ya * self.yield_hoop_ratio   # effective hoop yield

        area = geom.cross_section_area_in2
        t = geom.wall_thickness_in
        d_mean = geom.mean_diameter_in
        dp_factor = 2.0 * t / d_mean

        # Sweep axial stress from -ya to +ya
        axial_stress_range = np.linspace(-ya, ya, n_points)

        axial_forces = np.empty(n_points)
        dp_values = np.empty(n_points)

        for i, sa in enumerate(axial_stress_range):
            # Quadratic in sigma_t:
            # A*st^2 + B*st + C = 0
            # A = 1/yt^2
            # B = -sa / (ya*yt)
            # C = (sa/ya)^2 - 1
            a_coef = 1.0 / yt**2
            b_coef = -sa / (ya * yt)
            c_coef = (sa / ya) ** 2 - 1.0

            disc = b_coef**2 - 4.0 * a_coef * c_coef
            if disc < 0.0:
                disc = 0.0
            sqrt_disc = math.sqrt(disc)

            st_pos = (-b_coef + sqrt_disc) / (2.0 * a_coef)  # burst root
            st_neg = (-b_coef - sqrt_disc) / (2.0 * a_coef)  # collapse root

            if i < n_points // 2:
                st = st_pos
            else:
                st = st_neg

            axial_forces[i] = sa * area
            dp_values[i] = st * dp_factor

        return np.column_stack([axial_forces, dp_values])


# ---------------------------------------------------------------------------
# Convenience function: all envelopes in one call
# ---------------------------------------------------------------------------

def design_envelope_points(
    geometry: TubularGeometry,
    design_factor: float = 1.0,
    n_points: int = 100,
    yield_hoop_ratio: Optional[float] = None,
) -> dict[str, np.ndarray]:
    """Compute all applicable design envelope curves for a tubular string.

    Parameters
    ----------
    geometry : TubularGeometry
        Casing or tubing physical and material properties.
    design_factor : float
        Safety / design factor to apply to all envelopes.  Default 1.0.
    n_points : int
        Number of envelope points per curve.
    yield_hoop_ratio : float, optional
        If provided (>0 and != 1.0), also compute the anisotropic VME envelope
        for CRA grades.  Must be > 0.

    Returns
    -------
    dict[str, np.ndarray]
        Keys: 'vme_isotropic', 'api_ellipse', and optionally 'vme_anisotropic'.
        Each value is an (n_points, 2) array of (axial_force_lbf, dp_psi).
    """
    results: dict[str, np.ndarray] = {}

    results["vme_isotropic"] = IsotropicVmeEnvelope(
        geometry=geometry,
        design_factor=design_factor,
    ).compute(n_points=n_points)

    results["api_ellipse"] = ApiEllipseEnvelope(
        geometry=geometry,
        design_factor=design_factor,
    ).compute(n_points=n_points)

    if yield_hoop_ratio is not None:
        results["vme_anisotropic"] = AnisotropicVmeEnvelope(
            geometry=geometry,
            yield_hoop_ratio=yield_hoop_ratio,
            design_factor=design_factor,
        ).compute(n_points=n_points)

    return results
