"""
ABOUTME: Wave spectrum library for OrcaWave motion response analysis.
Implements JONSWAP, Pierson-Moskowitz, Bretschneider, ISSC, Ochi-Hubble
(bimodal), and Torsethaugen spectra.  Provides spectral moments and
period relationships (Tp, Tz, T01, T02).

Focused on OrcaWave workflows: spectrum discretisation for RAO-based
response computation and short-term statistics.
"""

from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class SpectrumParameters(BaseModel):
    """Input parameters for a wave spectrum."""

    spectrum_type: str = Field(
        default="JONSWAP",
        description="Spectrum type: JONSWAP, PM, Bretschneider, ISSC, OchiHubble, Torsethaugen",
    )
    hs: float = Field(..., description="Significant wave height in metres", gt=0)
    tp: float = Field(..., description="Peak period in seconds", gt=0)
    gamma: float = Field(
        default=3.3,
        description="JONSWAP peak enhancement factor (typically 1.0-7.0)",
    )
    # Ochi-Hubble second mode
    hs2: Optional[float] = Field(default=None, description="Hs for swell component (Ochi-Hubble)")
    tp2: Optional[float] = Field(default=None, description="Tp for swell component (Ochi-Hubble)")
    gamma2: Optional[float] = Field(default=1.0, description="Gamma for swell component")


class SpectrumResult(BaseModel):
    """Discretised wave spectrum output."""

    frequencies: List[float] = Field(..., description="Frequency vector (rad/s)")
    spectral_density: List[float] = Field(
        ...,
        description="Spectral density S(omega) in m^2·s/rad",
    )
    hs_computed: float = Field(..., description="Hs from spectral integration")
    tp_computed: float = Field(..., description="Tp from peak of spectrum")
    tz: float = Field(..., description="Zero-crossing period T02")
    t01: float = Field(..., description="Mean period T01 = m0/m1")


class SpectralMoments(BaseModel):
    """Spectral moments m0 through m4."""

    m0: float
    m1: float
    m2: float
    m3: float
    m4: float


# ---------------------------------------------------------------------------
# Spectral moment helpers
# ---------------------------------------------------------------------------

def compute_spectral_moments(
    omega: np.ndarray,
    s_omega: np.ndarray,
    max_order: int = 4,
) -> SpectralMoments:
    """Compute spectral moments m_n = integral(omega^n * S(omega) d_omega).

    Args:
        omega: Frequency vector (rad/s).
        s_omega: Spectral density S(omega).
        max_order: Highest moment order.

    Returns:
        SpectralMoments with m0 through m4.
    """
    moments = {}
    for n in range(max_order + 1):
        moments[f"m{n}"] = float(np.trapz(omega**n * s_omega, omega))
    return SpectralMoments(**moments)


def spectral_periods(moments: SpectralMoments) -> Dict[str, float]:
    """Derive characteristic periods from spectral moments.

    Args:
        moments: Spectral moments.

    Returns:
        Dict with Tz (T02), T01, T_mean, bandwidth parameter epsilon.
    """
    m0, m1, m2, m4 = moments.m0, moments.m1, moments.m2, moments.m4
    tz = 2.0 * np.pi * np.sqrt(m0 / max(m2, 1e-30))
    t01 = 2.0 * np.pi * m0 / max(m1, 1e-30)
    eps = np.sqrt(1.0 - m2**2 / max(m0 * m4, 1e-30))
    return {"Tz": float(tz), "T01": float(t01), "epsilon": float(eps)}


# ---------------------------------------------------------------------------
# Spectrum functions
# ---------------------------------------------------------------------------

def pierson_moskowitz(
    omega: np.ndarray,
    hs: float,
    tp: float,
) -> np.ndarray:
    """Pierson-Moskowitz spectrum (fully developed sea).

    S(omega) = (5/16) * Hs^2 * omega_p^4 * omega^-5
               * exp(-5/4 * (omega_p/omega)^4)

    Args:
        omega: Frequency vector (rad/s).
        hs: Significant wave height (m).
        tp: Peak period (s).

    Returns:
        Spectral density array.
    """
    omega_p = 2.0 * np.pi / tp
    s = np.zeros_like(omega)
    mask = omega > 0
    s[mask] = (
        (5.0 / 16.0)
        * hs**2
        * omega_p**4
        * omega[mask] ** (-5)
        * np.exp(-1.25 * (omega_p / omega[mask]) ** 4)
    )
    return s


def jonswap(
    omega: np.ndarray,
    hs: float,
    tp: float,
    gamma: float = 3.3,
) -> np.ndarray:
    """JONSWAP spectrum (fetch-limited, North Sea).

    S_J(omega) = alpha_J * S_PM(omega) * gamma^b

    where b = exp(-0.5 * ((omega - omega_p) / (sigma * omega_p))^2)

    Args:
        omega: Frequency vector (rad/s).
        hs: Significant wave height (m).
        tp: Peak period (s).
        gamma: Peak enhancement factor (default 3.3).

    Returns:
        Spectral density array.
    """
    omega_p = 2.0 * np.pi / tp
    s_pm = pierson_moskowitz(omega, hs, tp)

    sigma = np.where(omega <= omega_p, 0.07, 0.09)
    exponent = np.where(
        omega > 0,
        -0.5 * ((omega - omega_p) / (sigma * omega_p + 1e-30)) ** 2,
        0.0,
    )
    enhancement = gamma ** np.exp(exponent)

    # Normalisation factor so that Hs is preserved
    # C_gamma ~ 1 - 0.287 * ln(gamma)
    c_gamma = 1.0 - 0.287 * np.log(gamma)

    s_j = c_gamma * s_pm * enhancement

    # Renormalize to exact Hs
    m0 = np.trapz(s_j, omega)
    target_m0 = hs**2 / 16.0
    if m0 > 1e-30:
        s_j *= target_m0 / m0

    return s_j


def bretschneider(
    omega: np.ndarray,
    hs: float,
    tp: float,
) -> np.ndarray:
    """Bretschneider (modified Pierson-Moskowitz) spectrum.

    Identical functional form to PM but parameterised by Hs and Tp.

    Args:
        omega: Frequency vector (rad/s).
        hs: Significant wave height (m).
        tp: Peak period (s).

    Returns:
        Spectral density array.
    """
    return pierson_moskowitz(omega, hs, tp)


def issc_spectrum(
    omega: np.ndarray,
    hs: float,
    tz: float,
) -> np.ndarray:
    """ISSC (modified Pierson-Moskowitz) spectrum parameterised by Tz.

    S(omega) = (Hs^2 / (4*pi)) * (2*pi/Tz)^4 * omega^-5
               * exp(-(1/pi) * (2*pi/Tz / omega)^4)

    Args:
        omega: Frequency vector (rad/s).
        hs: Significant wave height (m).
        tz: Zero-crossing period (s).

    Returns:
        Spectral density array.
    """
    omega_z = 2.0 * np.pi / tz
    s = np.zeros_like(omega)
    mask = omega > 0
    A = (hs**2 / (4.0 * np.pi)) * omega_z**4
    s[mask] = A * omega[mask] ** (-5) * np.exp(-(1.0 / np.pi) * (omega_z / omega[mask]) ** 4)
    return s


def ochi_hubble(
    omega: np.ndarray,
    hs1: float,
    tp1: float,
    gamma1: float,
    hs2: float,
    tp2: float,
    gamma2: float,
) -> np.ndarray:
    """Ochi-Hubble bimodal spectrum (combined wind-sea and swell).

    Each component is a modified Bretschneider::

        S_j(omega) = ((4*lam_j+1)/4 * omega_pj^4)^lam_j / (Gamma(lam_j))
                     * Hs_j^2 / omega^(4*lam_j+1)
                     * exp(-(4*lam_j+1)/4 * (omega_pj/omega)^4)

    where lam_j = gamma_j.

    Args:
        omega: Frequency vector (rad/s).
        hs1: Significant wave height, wind-sea (m).
        tp1: Peak period, wind-sea (s).
        gamma1: Shape factor, wind-sea.
        hs2: Significant wave height, swell (m).
        tp2: Peak period, swell (s).
        gamma2: Shape factor, swell.

    Returns:
        Total spectral density (sum of both components).
    """
    from scipy.special import gamma as gamma_func

    def _component(hs: float, tp: float, lam: float) -> np.ndarray:
        omega_p = 2.0 * np.pi / tp
        s = np.zeros_like(omega)
        mask = omega > 0
        q = (4.0 * lam + 1.0) / 4.0
        coeff = (q * omega_p**4) ** lam / gamma_func(lam) * (hs**2 / 4.0)
        s[mask] = (
            coeff
            * omega[mask] ** (-(4.0 * lam + 1.0))
            * np.exp(-q * (omega_p / omega[mask]) ** 4)
        )
        return s

    return _component(hs1, tp1, gamma1) + _component(hs2, tp2, gamma2)


def torsethaugen(
    omega: np.ndarray,
    hs: float,
    tp: float,
) -> np.ndarray:
    """Torsethaugen double-peaked spectrum (simplified).

    Splits into wind-sea and swell components based on Tp relative to
    the fully-developed sea peak period.  Uses a simplified partition
    following Torsethaugen & Haver (2004).

    Args:
        omega: Frequency vector (rad/s).
        hs: Significant wave height (m).
        tp: Peak period (s).

    Returns:
        Total spectral density array.
    """
    # Fully developed Tp: Tp_f ~ 6.6 * Hs^(1/3)  (simplified)
    tp_f = 6.6 * hs ** (1.0 / 3.0)

    if tp <= tp_f:
        # Wind-sea dominated
        hs_w = hs
        tp_w = tp
        hs_s = 0.1 * hs  # small swell component
        tp_s = tp_f
    else:
        # Swell dominated
        hs_s = hs
        tp_s = tp
        hs_w = 0.1 * hs
        tp_w = tp_f

    s_wind = jonswap(omega, hs_w, tp_w, gamma=3.3)
    s_swell = jonswap(omega, hs_s, tp_s, gamma=1.0)

    s_total = s_wind + s_swell

    # Renormalize to target Hs
    m0 = np.trapz(s_total, omega)
    target_m0 = hs**2 / 16.0
    if m0 > 1e-30:
        s_total *= target_m0 / m0

    return s_total


# ---------------------------------------------------------------------------
# High-level API
# ---------------------------------------------------------------------------

def generate_spectrum(
    params: SpectrumParameters,
    omega_min: float = 0.05,
    omega_max: float = 3.0,
    n_freq: int = 200,
) -> SpectrumResult:
    """Generate a discretised wave spectrum from parameters.

    Args:
        params: Spectrum parameters (type, Hs, Tp, etc.).
        omega_min: Minimum frequency (rad/s).
        omega_max: Maximum frequency (rad/s).
        n_freq: Number of frequency bins.

    Returns:
        SpectrumResult with frequencies, spectral density, and derived periods.
    """
    omega = np.linspace(omega_min, omega_max, n_freq)
    stype = params.spectrum_type.upper().replace("-", "").replace("_", "")

    if stype in ("PM", "PIERSONMOSKOWITZ"):
        s = pierson_moskowitz(omega, params.hs, params.tp)
    elif stype == "JONSWAP":
        s = jonswap(omega, params.hs, params.tp, params.gamma)
    elif stype in ("BRETSCHNEIDER", "BS"):
        s = bretschneider(omega, params.hs, params.tp)
    elif stype == "ISSC":
        tz = params.tp / 1.408  # approximate Tz from Tp for PM family
        s = issc_spectrum(omega, params.hs, tz)
    elif stype in ("OCHIHUBBLE", "OH"):
        hs2 = params.hs2 if params.hs2 is not None else 0.5 * params.hs
        tp2 = params.tp2 if params.tp2 is not None else 2.0 * params.tp
        g2 = params.gamma2 if params.gamma2 is not None else 1.0
        s = ochi_hubble(omega, params.hs, params.tp, params.gamma, hs2, tp2, g2)
    elif stype in ("TORSETHAUGEN", "TH"):
        s = torsethaugen(omega, params.hs, params.tp)
    else:
        raise ValueError(f"Unknown spectrum type: {params.spectrum_type}")

    moments = compute_spectral_moments(omega, s)
    hs_computed = 4.0 * np.sqrt(max(moments.m0, 0.0))

    # Peak period from spectrum
    peak_idx = int(np.argmax(s))
    tp_computed = 2.0 * np.pi / omega[peak_idx] if omega[peak_idx] > 0 else params.tp

    periods = spectral_periods(moments)

    return SpectrumResult(
        frequencies=omega.tolist(),
        spectral_density=s.tolist(),
        hs_computed=float(hs_computed),
        tp_computed=float(tp_computed),
        tz=periods["Tz"],
        t01=periods["T01"],
    )
