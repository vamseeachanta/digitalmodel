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
from typing import Union

import numpy as np
from scipy import special
from pydantic import BaseModel

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class SpectralMoments(BaseModel):
    """Hz moments of a one-sided stress PSD: m_k = integral(f**k S(f) df).

    Frequency f is in Hz and S(f) in MPa²/Hz; m_k has units MPa² Hz**k.
    Direct constructors must use this convention. Convert angular moments
    explicitly with m_k = m_k_angular / (2*pi)**k before construction.
    See docs/domains/fatigue/spectral-frequency-units.md for the derivation.

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
        """Expected peaks per second: sqrt(m4/m2) for Hz moments."""
        return math.sqrt(self.m4 / self.m2)

    @property
    def nu_0(self) -> float:
        """Expected zero upcrossings per second: sqrt(m2/m0) for Hz moments."""
        return math.sqrt(self.m2 / self.m0)

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
    """Compute Hz moments m_k = integral(f**k S(f) df), k = 0, 1, 2, 4.

    The one-sided PSD includes the full variance at nonnegative frequencies.
    No angular-frequency conversion is applied.

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

    m0 = float(_trapezoid(S, f))
    m1 = float(_trapezoid(S * f, f))
    m2 = float(_trapezoid(S * f ** 2, f))
    m4 = float(_trapezoid(S * f ** 4, f))

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

        damage_rate = ν₀ · (2√2 · √m₀)^m · Γ(1 + m/2) / A

    where A = 10^(log_a), m = SN slope.

    Parameters
    ----------
    moments : SpectralMoments
    sn_slope : float
        S-N curve slope *m* (e.g. 3.0 for DNV D-curve).
    sn_intercept : float
        log10 of S-N intercept *a*.
    duration : float
        Retained for API compatibility (default 1.0 seconds); returned
        damage fields are rates independent of this argument. Multiply
        damage_rate by exposure in seconds to obtain accumulated damage.

    Returns
    -------
    SpectralFatigueResult
    """
    m = sn_slope
    A = 10.0 ** sn_intercept
    nu0 = moments.nu_0

    # E[S^m] for Rayleigh distribution with scale √(2·m0)
    gamma_m = special.gamma(1.0 + m / 2.0)
    E_Sm = (2.0 * math.sqrt(2.0 * moments.m0)) ** m * gamma_m

    damage_rate = nu0 * E_Sm / A

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
    """Benasciutti-Tovo correction using the fitted BT2 weight (2005).

    Metals 11 (2021), 1333, Eqs. (11), (13),
    https://doi.org/10.3390/met11091333, gives
    ``lambda = w + (1-w)*alpha2**(m-1)``. The method assumes a stationary
    Gaussian stress process and a single-slope S-N curve without a fatigue
    limit. Finite positive moments must satisfy
    ``0 < alpha2 <= alpha1 <= 1``; slopes below unity are rejected because
    the reduction bound then no longer follows from Eq. (11).

    Moment and rate conventions are inherited from ``narrow_band_damage``.
    This correction does not qualify their absolute frequency units.

    Parameters
    ----------
    moments : SpectralMoments
    sn_slope : float
        Finite S-N slope, at least unity (algebraic reduction domain).
    sn_intercept : float
        Finite log10 of the range-based S-N intercept.
    duration : float
        Retained for API compatibility; returned damage is a rate.

    Returns
    -------
    SpectralFatigueResult
    """
    if not all(math.isfinite(v) and v > 0 for v in (
        moments.m0, moments.m1, moments.m2, moments.m4,
    )):
        raise ValueError("moments must be finite and positive")
    if not math.isfinite(sn_slope) or sn_slope < 1.0:
        raise ValueError("sn_slope must be finite and at least unity")
    if not math.isfinite(sn_intercept):
        raise ValueError("sn_intercept must be finite")

    # Metals (2021), Eq. (2): dimensionless moment ratios. Separate square
    # roots avoid overflowing products of otherwise representable moments.
    alpha1 = moments.m1 / math.sqrt(moments.m0) / math.sqrt(moments.m2)
    alpha2 = moments.m2 / math.sqrt(moments.m0) / math.sqrt(moments.m4)
    # Accuracy and Stability of Numerical Algorithms, 2nd ed. (2002),
    # Sections 2.2 and 3.1, Lemma 3.1: two roots and two divisions give
    # alpha_hat/alpha=(1+d1)*(1+d2)/((1+d3)*(1+d4)), |di|<=u.
    # Hence |alpha_hat/alpha-1|<=gamma_4=4*u/(1-4*u). Ordering uses
    # both relative error bounds expressed in terms of the computed ratios.
    unit_roundoff = math.ulp(1.0) / 2.0
    roundoff = 4.0 * unit_roundoff / (1.0 - 4.0 * unit_roundoff)
    ordering_roundoff = roundoff / (1.0 - roundoff) * (alpha1 + alpha2)
    if not (0 < alpha2 <= 1.0 + roundoff
            and 0 < alpha1 <= 1.0 + roundoff
            and alpha2 - alpha1 <= ordering_roundoff):
        raise ValueError("moments must satisfy 0 < alpha2 <= alpha1 <= 1")
    # Normalize the same uncertainty interval on both sides of unity.
    if abs(alpha1 - 1.0) <= roundoff:
        alpha1 = 1.0
    if abs(alpha2 - 1.0) <= roundoff:
        alpha2 = 1.0
    alpha2 = min(alpha2, 1.0)
    alpha1 = min(max(alpha1, alpha2), 1.0)

    if alpha2 == 1.0:
        # Eq. (11) gives lambda=1 for a spectral line; Eq. (13) is 0/0.
        weight = 1.0
    else:
        # Eq. (13), factored using 1+a1*a2-a1-a2=(1-a1)*(1-a2).
        # Both fitted coefficients, 1.112 and 2.11, are from that equation.
        ratio = (alpha1 - alpha2) / (1.0 - alpha2)
        weight = ratio * (
            ratio + 1.112 * (1.0 - alpha1) * math.exp(2.11 * alpha2)
        )

    # Eq. (11): the convex mixture gives alpha2**(m-1) <= lambda <= 1.
    lam = weight + (1.0 - weight) * alpha2 ** (sn_slope - 1.0)
    nb_result = narrow_band_damage(moments, sn_slope, sn_intercept, duration)
    damage_rate = nb_result.damage_rate * lam
    # Eq. (4), N(range)=A/range**m: match damage at the reported peak rate.
    # Use unrounded damage rather than scaling an already rounded range.
    s_eq = (damage_rate * 10.0 ** sn_intercept / moments.nu_p) ** (1.0 / sn_slope)

    return SpectralFatigueResult(
        damage_rate=damage_rate,
        damage_per_year=damage_rate * 365.25 * 24 * 3600,
        equivalent_stress_range=round(s_eq, 3),
        method=f"Benasciutti-Tovo (BT2, rho={weight:.3f})",
        bandwidth_parameter=moments.bandwidth_parameter,
        peak_rate=moments.nu_p,
    )
