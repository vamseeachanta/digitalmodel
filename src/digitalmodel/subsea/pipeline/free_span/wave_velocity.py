"""
DNV RP F105 Section 3.4 — Wave-induced velocity at pipe elevation.

Computes the significant wave-induced flow velocity Uw and mean
zero-crossing period Tu from sea-state parameters using the JONSWAP
spectrum and linear wave theory transfer function.

Pipeline:
    1. JONSWAP spectrum  S(ω)  from Hs, Tp
    2. Dispersion relation  k(ω, h)  (iterative)
    3. Velocity transfer function  G(ω) = ω·cosh(k·z_pipe)/sinh(k·h)
    4. Velocity spectrum  Sv(ω) = S(ω) × G(ω)²
    5. Spectral moments  Mn = ∫ ωⁿ · Sv(ω) dω
    6. Significant velocity  Uw = 2√M₀
    7. Mean zero-crossing period  Tu = 2π√(M₀/M₂)

JONSWAP peak enhancement γ (DNV-RP-F105 Sec 3.4):
    φ = Tp / √Hs
    γ = 5.0               if φ ≤ 3.6
    γ = exp(5.75 − 1.15φ) if 3.6 < φ < 5.0
    γ = 1.0               if φ ≥ 5.0

Reference: DNV-RP-F105, "Free Spanning Pipelines", Section 3.4 (2006/2017).
"""
from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Optional

import numpy as np

# Gravitational acceleration [m/s²]
_G: float = 9.80665


# =========================================================================
# Data classes
# =========================================================================

@dataclass
class WaveVelocityResult:
    """Output of ``compute_wave_velocity``.

    Attributes
    ----------
    Uw_ms : float
        Significant wave-induced velocity at pipe elevation [m/s].
    Tu_s : float
        Mean zero-crossing period of the velocity process [s].
    gamma : float
        JONSWAP peak enhancement factor used.
    M0 : float
        Zeroth spectral moment of the velocity spectrum [m²/s²].
    M2 : float
        Second spectral moment of the velocity spectrum [m²/s⁴].
    """
    Uw_ms: float
    Tu_s: float
    gamma: float
    M0: float
    M2: float


# =========================================================================
# JONSWAP spectrum
# =========================================================================

def jonswap_gamma(Hs: float, Tp: float) -> float:
    """Auto-select JONSWAP peak enhancement factor γ per DNV-RP-F105.

    Parameters
    ----------
    Hs : float
        Significant wave height [m].
    Tp : float
        Peak spectral period [s].

    Returns
    -------
    float
        Peak enhancement factor γ.
    """
    phi = Tp / math.sqrt(Hs) if Hs > 0 else 5.0
    if phi <= 3.6:
        return 5.0
    elif phi >= 5.0:
        return 1.0
    else:
        return math.exp(5.75 - 1.15 * phi)


def jonswap_spectrum(
    omega: np.ndarray,
    Hs: float,
    Tp: float,
    gamma: Optional[float] = None,
) -> np.ndarray:
    """JONSWAP wave energy spectrum S(ω) [m²·s/rad].

    Parameters
    ----------
    omega : ndarray
        Angular frequencies [rad/s].  Must be > 0.
    Hs : float
        Significant wave height [m].
    Tp : float
        Peak spectral period [s].
    gamma : float, optional
        Peak enhancement factor.  Auto-computed from Hs/Tp if omitted.

    Returns
    -------
    ndarray
        Spectral density S(ω) [m²·s/rad].
    """
    if gamma is None:
        gamma = jonswap_gamma(Hs, Tp)

    wp = 2.0 * math.pi / Tp  # peak angular frequency
    g = _G

    # Spectral width parameter
    sigma = np.where(omega <= wp, 0.07, 0.09)

    # Peak enhancement exponent
    a = np.exp(-0.5 * ((omega - wp) / (sigma * wp)) ** 2)

    # Normalisation factor for gamma (DNV-RP-F105 / Goda)
    C_gamma = 1.0 - 0.287 * math.log(gamma) if gamma > 1.0 else 1.0

    # Alpha from Hs constraint
    alpha = (5.0 / 16.0) * Hs**2 * wp**4 / g**2

    with np.errstate(divide="ignore", invalid="ignore"):
        S_pm = (alpha * g**2 / omega**5) * np.exp(-1.25 * (wp / omega) ** 4)
        S = (S_pm / C_gamma) * gamma**a

    S = np.nan_to_num(S, nan=0.0, posinf=0.0, neginf=0.0)
    return S


# =========================================================================
# Dispersion relation
# =========================================================================

def solve_dispersion(omega: np.ndarray, depth: float, tol: float = 1e-8,
                     max_iter: int = 50) -> np.ndarray:
    """Solve the linear wave dispersion relation ω² = g·k·tanh(k·h).

    Uses Newton-Raphson iteration starting from the deep-water
    approximation k₀ = ω²/g.

    Parameters
    ----------
    omega : ndarray
        Angular frequencies [rad/s].
    depth : float
        Water depth h [m].
    tol : float
        Convergence tolerance on k·h.
    max_iter : int
        Maximum Newton iterations.

    Returns
    -------
    ndarray
        Wave numbers k [1/m].
    """
    g = _G
    h = depth

    # Deep-water initial guess
    k = omega**2 / g

    for _ in range(max_iter):
        kh = k * h
        # Avoid overflow in tanh for very large kh
        kh_clipped = np.minimum(kh, 50.0)
        f = omega**2 - g * k * np.tanh(kh_clipped)
        # df/dk = -g * [tanh(kh) + kh * sech²(kh)]
        tanh_kh = np.tanh(kh_clipped)
        sech2_kh = 1.0 - tanh_kh**2
        df = -g * (tanh_kh + kh_clipped * sech2_kh)
        dk = -f / df
        k = k + dk
        k = np.maximum(k, 1e-12)  # keep positive
        if np.max(np.abs(dk * h)) < tol:
            break

    return k


# =========================================================================
# Velocity transfer function
# =========================================================================

def velocity_transfer(omega: np.ndarray, k: np.ndarray,
                      depth: float, z_pipe: float) -> np.ndarray:
    """Horizontal velocity transfer function G(ω) per linear wave theory.

    G(ω) = ω · cosh(k · z_pipe) / sinh(k · h)

    where z_pipe is measured from the seabed (positive upward), and h is
    the total water depth.

    Parameters
    ----------
    omega : ndarray
        Angular frequencies [rad/s].
    k : ndarray
        Wave numbers [1/m].
    depth : float
        Water depth h [m].
    z_pipe : float
        Pipe elevation above seabed [m].  For a pipe on the seabed
        with gap *e*, use z_pipe = e + D/2 (pipe centre) or e (pipe bottom).

    Returns
    -------
    ndarray
        Velocity transfer function G(ω) [1/s].
    """
    kh = k * depth
    kz = k * z_pipe

    # Clip to avoid overflow
    kh_clipped = np.minimum(kh, 50.0)
    kz_clipped = np.minimum(kz, 50.0)

    with np.errstate(divide="ignore", invalid="ignore"):
        G = omega * np.cosh(kz_clipped) / np.sinh(kh_clipped)

    G = np.nan_to_num(G, nan=0.0, posinf=0.0, neginf=0.0)
    return G


# =========================================================================
# Spectral moments
# =========================================================================

def spectral_moments(omega: np.ndarray, Sv: np.ndarray) -> tuple[float, float]:
    """Compute 0th and 2nd spectral moments of velocity spectrum.

    Mn = ∫ ωⁿ · Sv(ω) dω

    Parameters
    ----------
    omega : ndarray
        Angular frequencies [rad/s].
    Sv : ndarray
        Velocity spectral density [m²/s].

    Returns
    -------
    (M0, M2) : tuple[float, float]
        Zeroth and second spectral moments.
    """
    _trapz = getattr(np, "trapezoid", None) or np.trapz  # numpy 2.x compat
    M0 = float(_trapz(Sv, omega))
    M2 = float(_trapz(omega**2 * Sv, omega))
    return M0, M2


# =========================================================================
# Main API
# =========================================================================

def compute_wave_velocity(
    Hs: float,
    Tp: float,
    water_depth_m: float,
    pipe_elevation_m: float,
    pipe_od_m: float = 0.0,
    gamma: Optional[float] = None,
    n_freq: int = 1024,
    omega_max_factor: float = 5.0,
) -> WaveVelocityResult:
    """Compute significant wave-induced velocity at pipe elevation.

    Follows the DNV-RP-F105 Section 3.4 procedure:
    JONSWAP → dispersion → velocity transfer → spectral moments → Uw, Tu.

    Parameters
    ----------
    Hs : float
        Significant wave height [m].
    Tp : float
        Peak spectral period [s].
    water_depth_m : float
        Water depth h [m].
    pipe_elevation_m : float
        Pipe bottom elevation above seabed [m].
        Use seabed_gap + OD/2 for pipe centre, or seabed_gap for bottom.
    pipe_od_m : float
        Pipe outer diameter [m].  If provided, elevation is adjusted to
        the point D/2 above the seabed gap (pipe centre) when
        pipe_elevation_m represents the gap.
    gamma : float, optional
        JONSWAP peak enhancement factor.  Auto-selected if omitted.
    n_freq : int
        Number of frequency bins for integration (default 1024).
    omega_max_factor : float
        Upper integration limit = omega_max_factor × ωp (default 5.0).

    Returns
    -------
    WaveVelocityResult
        Contains Uw_ms, Tu_s, gamma, M0, M2.

    Examples
    --------
    >>> result = compute_wave_velocity(Hs=5.0, Tp=8.0, water_depth_m=120.0,
    ...                                pipe_elevation_m=0.5, pipe_od_m=0.2731)
    >>> print(f"Uw = {result.Uw_ms:.3f} m/s, Tu = {result.Tu_s:.2f} s")
    """
    if Hs <= 0 or Tp <= 0:
        return WaveVelocityResult(Uw_ms=0.0, Tu_s=0.0, gamma=1.0, M0=0.0, M2=0.0)

    if gamma is None:
        gamma = jonswap_gamma(Hs, Tp)

    wp = 2.0 * math.pi / Tp

    # Pipe elevation: use centre of pipe if OD provided
    z_pipe = pipe_elevation_m
    if pipe_od_m > 0:
        z_pipe = pipe_elevation_m + pipe_od_m / 2.0

    # Frequency array (skip ω=0 to avoid division by zero)
    omega_max = omega_max_factor * wp
    omega = np.linspace(0.01 * wp, omega_max, n_freq)

    # 1. JONSWAP spectrum
    S = jonswap_spectrum(omega, Hs, Tp, gamma)

    # 2. Dispersion relation
    k = solve_dispersion(omega, water_depth_m)

    # 3. Velocity transfer function
    G = velocity_transfer(omega, k, water_depth_m, z_pipe)

    # 4. Velocity spectrum
    Sv = S * G**2

    # 5. Spectral moments
    M0, M2 = spectral_moments(omega, Sv)

    # 6. Significant velocity and zero-crossing period
    Uw = 2.0 * math.sqrt(M0) if M0 > 0 else 0.0
    Tu = 2.0 * math.pi * math.sqrt(M0 / M2) if M2 > 0 else 0.0

    return WaveVelocityResult(
        Uw_ms=Uw,
        Tu_s=Tu,
        gamma=gamma,
        M0=M0,
        M2=M2,
    )
