"""
Frequency-Domain (Spectral) Fatigue Analysis
=============================================

Methods for estimating fatigue damage from a stress power spectral density
(PSD), avoiding the need for time-domain simulation + rainflow counting.

Implemented methods:

1. **Narrow-band approximation** — Bendat (1964), upper bound for most
   real-world loading.
2. **Wirsching-Light correction** — empirical correction to narrow-band
   for wide-band processes (Wirsching & Light, 1980).
3. **Dirlik method** — semi-empirical closed-form, widely regarded as the
   most accurate single-method spectral approach (Dirlik, 1985).
4. **Benasciutti-Tovo method** — two-boundary method interpolating between
   narrow-band and wide-band estimates (Benasciutti & Tovo, 2005).

All methods take spectral moments (m₀, m₁, m₂, m₄) and an S-N curve
slope *m*, and return fatigue damage per unit time or an equivalent
stress range.

References
----------
- Dirlik, T. (1985), Application of computers in fatigue analysis, PhD thesis
- Wirsching, P.H. & Light, M.C. (1980), J. Struct. Div., ASCE, 106(6)
- Benasciutti, D. & Tovo, R. (2005), Int. J. Fatigue, 27(8), pp. 867-877
- DNV-RP-C203 (2021), Section 4.6 — Frequency domain fatigue analysis
"""

import math
from typing import Optional, Tuple, Union

import numpy as np
from scipy import special
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class SpectralMoments(BaseModel):
    """Spectral moments of a stress PSD.

    Attributes
    ----------
    m0 : float — zeroth moment (= variance σ²)
    m1 : float — first moment
    m2 : float — second moment
    m4 : float — fourth moment
    """
    m0: float
    m1: float
    m2: float
    m4: float

    @property
    def rms(self) -> float:
        """Root-mean-square stress (MPa)."""
        return math.sqrt(self.m0)

    @property
    def nu_p(self) -> float:
        """Expected number of peaks per second (peak rate)."""
        return math.sqrt(self.m4 / self.m2) / (2.0 * math.pi)

    @property
    def nu_0(self) -> float:
        """Expected zero-upcrossing rate per second."""
        return math.sqrt(self.m2 / self.m0) / (2.0 * math.pi)

    @property
    def bandwidth_parameter(self) -> float:
        """Vanmarcke bandwidth parameter ε = sqrt(1 − m2²/(m0·m4))."""
        ratio = self.m2 ** 2 / (self.m0 * self.m4)
        return math.sqrt(max(1.0 - ratio, 0.0))

    @property
    def irregularity_factor(self) -> float:
        """Irregularity factor α₂ = m2 / sqrt(m0·m4)."""
        return self.m2 / math.sqrt(self.m0 * self.m4)


class SpectralFatigueResult(BaseModel):
    """Result of spectral fatigue computation.

    Attributes
    ----------
    damage_rate : float
        Fatigue damage per second (1/s).
    damage_per_year : float
        Fatigue damage per year (assuming continuous operation).
    equivalent_stress_range : float
        Equivalent constant-amplitude stress range (MPa) that produces
        the same damage at the peak crossing rate.
    method : str
        Name of the method used.
    bandwidth_parameter : float
        ε of the PSD.
    peak_rate : float
        Expected peaks per second (Hz).
    """
    damage_rate: float
    damage_per_year: float = 0.0
    equivalent_stress_range: float = 0.0
    method: str = ""
    bandwidth_parameter: float = 0.0
    peak_rate: float = 0.0


# ---------------------------------------------------------------------------
# Helper: compute spectral moments from a PSD
# ---------------------------------------------------------------------------

def compute_spectral_moments(
    frequency: Union[np.ndarray, list],
    psd: Union[np.ndarray, list],
) -> SpectralMoments:
    """Compute spectral moments m0, m1, m2, m4 from a one-sided PSD.

    Parameters
    ----------
    frequency : array-like
        Frequency axis (Hz), length N.
    psd : array-like
        One-sided power spectral density of stress (MPa²/Hz), length N.

    Returns
    -------
    SpectralMoments
    """
    f = np.asarray(frequency, dtype=float)
    S = np.asarray(psd, dtype=float)
    if len(f) != len(S):
        raise ValueError("frequency and psd must have equal length")

    m0 = float(np.trapz(S, f))
    m1 = float(np.trapz(S * f, f))
    m2 = float(np.trapz(S * f ** 2, f))
    m4 = float(np.trapz(S * f ** 4, f))

    return SpectralMoments(m0=m0, m1=m1, m2=m2, m4=m4)


# ---------------------------------------------------------------------------
# Narrow-band approximation
# ---------------------------------------------------------------------------

def narrow_band_damage(
    moments: SpectralMoments,
    sn_slope: float,
    sn_intercept: float,
    duration: float = 1.0,
) -> SpectralFatigueResult:
    """Narrow-band fatigue damage estimate (Bendat, 1964).

    Assumes all cycles are Rayleigh-distributed::

        D = ν₀ · T · (2√2 · √m₀)^m · Γ(1 + m/2) / A

    where A = 10^(log_a), m = SN slope.

    Parameters
    ----------
    moments : SpectralMoments
    sn_slope : float
        S-N curve slope *m* (e.g. 3.0 for DNV D-curve).
    sn_intercept : float
        log10 of S-N intercept *a*.
    duration : float
        Duration in seconds (default 1.0).

    Returns
    -------
    SpectralFatigueResult
    """
    m = sn_slope
    A = 10.0 ** sn_intercept
    rms = moments.rms
    nu0 = moments.nu_0

    # E[S^m] for Rayleigh distribution with scale √(2·m0)
    gamma_m = special.gamma(1.0 + m / 2.0)
    E_Sm = (2.0 * math.sqrt(2.0 * moments.m0)) ** m * gamma_m

    damage_rate = nu0 * E_Sm / A
    damage = damage_rate * duration

    # Equivalent stress range
    s_eq = (E_Sm) ** (1.0 / m) if E_Sm > 0 else 0.0

    return SpectralFatigueResult(
        damage_rate=damage_rate,
        damage_per_year=damage_rate * 365.25 * 24 * 3600,
        equivalent_stress_range=round(s_eq, 3),
        method="Narrow-band (Rayleigh)",
        bandwidth_parameter=moments.bandwidth_parameter,
        peak_rate=moments.nu_p,
    )


# ---------------------------------------------------------------------------
# Wirsching-Light correction
# ---------------------------------------------------------------------------

def wirsching_light_damage(
    moments: SpectralMoments,
    sn_slope: float,
    sn_intercept: float,
    duration: float = 1.0,
) -> SpectralFatigueResult:
    """Wirsching-Light wide-band correction to narrow-band damage.

    DNV-RP-C203 (2021) Section 4.6.3.

    The correction factor λ is::

        λ = a(m) + [1 − a(m)] · (1 − ε)^b(m)

    where a(m) = 0.926 − 0.033·m, b(m) = 1.587·m − 2.323.

    Parameters
    ----------
    moments : SpectralMoments
    sn_slope : float
    sn_intercept : float
    duration : float

    Returns
    -------
    SpectralFatigueResult
    """
    nb = narrow_band_damage(moments, sn_slope, sn_intercept, duration)

    m = sn_slope
    eps = moments.bandwidth_parameter

    a_m = 0.926 - 0.033 * m
    b_m = 1.587 * m - 2.323

    lam = a_m + (1.0 - a_m) * (1.0 - eps) ** b_m

    damage_rate = nb.damage_rate * lam
    s_eq = nb.equivalent_stress_range * lam ** (1.0 / m) if lam > 0 else 0.0

    return SpectralFatigueResult(
        damage_rate=damage_rate,
        damage_per_year=damage_rate * 365.25 * 24 * 3600,
        equivalent_stress_range=round(s_eq, 3),
        method=f"Wirsching-Light (lambda={lam:.4f})",
        bandwidth_parameter=eps,
        peak_rate=moments.nu_p,
    )


# ---------------------------------------------------------------------------
# Dirlik method
# ---------------------------------------------------------------------------

def dirlik_damage(
    moments: SpectralMoments,
    sn_slope: float,
    sn_intercept: float,
    duration: float = 1.0,
) -> SpectralFatigueResult:
    """Dirlik closed-form spectral fatigue (Dirlik, 1985).

    The PDF of stress ranges is modelled as a mixture of one exponential
    and two Rayleigh distributions.  This is widely considered the most
    accurate single closed-form spectral method.

    Parameters
    ----------
    moments : SpectralMoments
    sn_slope : float
    sn_intercept : float
    duration : float

    Returns
    -------
    SpectralFatigueResult
    """
    m0 = moments.m0
    m1 = moments.m1
    m2 = moments.m2
    m4 = moments.m4
    m_sn = sn_slope
    A = 10.0 ** sn_intercept

    xm = m1 / m0 * math.sqrt(m2 / m4)
    eps = moments.bandwidth_parameter

    # Dirlik parameters
    D1 = 2.0 * (xm - eps ** 2) / (1.0 + eps ** 2)
    R = (eps - xm - D1 ** 2) / max(1.0 - eps - D1 + D1 ** 2, 1e-12)
    D2 = (1.0 - eps - D1 + D1 ** 2) / max(1.0 - R, 1e-12)
    D3 = 1.0 - D1 - D2
    Q = 1.25 * (eps - D3 - D2 * R) / max(D1, 1e-12)

    # Ensure physical parameters
    D1 = max(D1, 0.0)
    D2 = max(D2, 0.0)
    D3 = max(D3, 0.0)
    Q = max(Q, 1e-12)
    R = max(R, 1e-12)

    sqrt_m0 = math.sqrt(m0)

    # E[S^m] = integral of s^m * p_dirlik(s) ds  (s normalised by √m0)
    # Analytical: D1·Q^m·Γ(1+m) + (√2)^m·Γ(1+m/2)·(D2·|R|^m + D3)
    gamma_m = special.gamma(1.0 + m_sn)
    gamma_m2 = special.gamma(1.0 + m_sn / 2.0)

    E_Zm = (D1 * Q ** m_sn * gamma_m
            + (math.sqrt(2.0)) ** m_sn * gamma_m2 * (D2 * abs(R) ** m_sn + D3))

    E_Sm = E_Zm * (2.0 * sqrt_m0) ** m_sn

    nu_p = moments.nu_p
    damage_rate = nu_p * E_Sm / A

    s_eq = E_Sm ** (1.0 / m_sn) if E_Sm > 0 else 0.0

    return SpectralFatigueResult(
        damage_rate=damage_rate,
        damage_per_year=damage_rate * 365.25 * 24 * 3600,
        equivalent_stress_range=round(s_eq, 3),
        method="Dirlik (1985)",
        bandwidth_parameter=eps,
        peak_rate=nu_p,
    )


# ---------------------------------------------------------------------------
# Benasciutti-Tovo method
# ---------------------------------------------------------------------------

def benasciutti_tovo_damage(
    moments: SpectralMoments,
    sn_slope: float,
    sn_intercept: float,
    duration: float = 1.0,
) -> SpectralFatigueResult:
    """Benasciutti-Tovo two-boundary spectral fatigue method (2005).

    Interpolates between narrow-band and wide-band damage bounds
    using the bandwidth parameter.

    Parameters
    ----------
    moments : SpectralMoments
    sn_slope : float
    sn_intercept : float
    duration : float

    Returns
    -------
    SpectralFatigueResult
    """
    nb_result = narrow_band_damage(moments, sn_slope, sn_intercept, duration)
    nb_damage = nb_result.damage_rate

    alpha2 = moments.irregularity_factor
    m = sn_slope

    # Wide-band (range-counting) correction factor
    # Benasciutti-Tovo Eq. (13)
    b_bt = (alpha2 - 1.0) ** 2
    lam_wb = alpha2 ** (2.0 - m)

    # Interpolation weight
    # BT Eq. (14): ρ = min((α₁ − α₂)/(1 − α₁), 1) where α₁ = m1/√(m0·m2)
    alpha1 = moments.m1 / math.sqrt(moments.m0 * moments.m2)
    if alpha1 < 1.0:
        rho = min((alpha1 - alpha2) / max(1.0 - alpha1, 1e-12), 1.0)
    else:
        rho = 1.0
    rho = max(rho, 0.0)

    # Combined correction
    lam = rho * 1.0 + (1.0 - rho) * lam_wb  # 1.0 = narrow-band (no correction)
    lam = max(lam, lam_wb)  # never below wide-band

    damage_rate = nb_damage * lam
    s_eq = nb_result.equivalent_stress_range * lam ** (1.0 / m)

    return SpectralFatigueResult(
        damage_rate=damage_rate,
        damage_per_year=damage_rate * 365.25 * 24 * 3600,
        equivalent_stress_range=round(s_eq, 3),
        method=f"Benasciutti-Tovo (rho={rho:.3f})",
        bandwidth_parameter=moments.bandwidth_parameter,
        peak_rate=moments.nu_p,
    )
