"""
Spectral fatigue analysis per DNV-RP-C203.

Stdlib-only implementation of narrow-band spectral fatigue damage
using trapezoidal integration for spectral moments.

Functions
---------
spectral_moment : Compute nth spectral moment from discrete spectrum.
zero_crossing_period : Zero-crossing period Tz from m0, m2.
bandwidth_parameter : Bandwidth epsilon from m0, m2, m4.
narrow_band_damage : Rayleigh-based narrow-band fatigue damage.
"""

from dataclasses import dataclass
import math


@dataclass
class FatigueDamageResult:
    """Result of a narrow-band fatigue damage calculation."""

    damage: float
    cycles: float
    stress_range_rms: float
    standard: str = "DNV-RP-C203"


def spectral_moment(freqs: list, spectrum: list, n: int = 0) -> float:
    """Compute the nth spectral moment using trapezoidal integration.

    mn = integral of f^n * S(f) df

    Parameters
    ----------
    freqs : list[float]
        Frequency values in Hz (ascending order).
    spectrum : list[float]
        Spectral density values corresponding to *freqs*.
    n : int
        Moment order (0, 1, 2, 4, ...).

    Returns
    -------
    float
        The nth spectral moment.

    Raises
    ------
    ValueError
        If inputs are empty or mismatched in length.
    """
    if len(freqs) == 0 or len(spectrum) == 0:
        raise ValueError("freqs and spectrum must be non-empty")
    if len(freqs) != len(spectrum):
        raise ValueError("freqs and spectrum must have same length")
    if len(freqs) < 2:
        raise ValueError("Need at least 2 points for integration")

    total = 0.0
    for i in range(len(freqs) - 1):
        f0, f1 = freqs[i], freqs[i + 1]
        s0, s1 = spectrum[i], spectrum[i + 1]
        y0 = (f0 ** n) * s0
        y1 = (f1 ** n) * s1
        total += 0.5 * (y0 + y1) * (f1 - f0)
    return total


def zero_crossing_period(m0: float, m2: float) -> float:
    """Zero-crossing period Tz = sqrt(m0 / m2).

    Parameters
    ----------
    m0 : float
        0th spectral moment (variance of process).
    m2 : float
        2nd spectral moment.

    Returns
    -------
    float
        Zero-crossing period in seconds.

    Raises
    ------
    ValueError
        If m0 <= 0 or m2 <= 0.
    """
    if m0 <= 0:
        raise ValueError("m0 must be positive")
    if m2 <= 0:
        raise ValueError("m2 must be positive")
    return math.sqrt(m0 / m2)


def bandwidth_parameter(m0: float, m2: float, m4: float) -> float:
    """Bandwidth parameter epsilon = sqrt(1 - m2^2 / (m0 * m4)).

    Parameters
    ----------
    m0 : float
        0th spectral moment.
    m2 : float
        2nd spectral moment.
    m4 : float
        4th spectral moment.

    Returns
    -------
    float
        Bandwidth parameter (0 = narrow-band, 1 = wide-band).
    """
    ratio = m2 ** 2 / (m0 * m4)
    # Clamp for numerical safety
    val = 1.0 - ratio
    if val < 0:
        val = 0.0
    return math.sqrt(val)


def narrow_band_damage(
    m0: float,
    tz_seconds: float,
    duration_seconds: float,
    sn_log_a: float,
    sn_m: float,
) -> FatigueDamageResult:
    """Narrow-band fatigue damage per DNV-RP-C203 Section 4.3.

    Uses Rayleigh distribution of stress ranges for a Gaussian
    narrow-band process.

    D = (N / a) * (2 * sqrt(2 * m0))^m * Gamma(1 + m/2)

    where N = duration / Tz, a = 10^sn_log_a.

    Parameters
    ----------
    m0 : float
        0th spectral moment of stress process (units: MPa^2).
    tz_seconds : float
        Zero-crossing period in seconds.
    duration_seconds : float
        Exposure duration in seconds.
    sn_log_a : float
        Log10 of S-N curve intercept (e.g. 12.164 for DNV D-curve).
    sn_m : float
        S-N curve inverse slope (e.g. 3.0).

    Returns
    -------
    FatigueDamageResult
        Damage, cycle count, and RMS stress range.
    """
    if m0 <= 0:
        return FatigueDamageResult(
            damage=0.0, cycles=0.0, stress_range_rms=0.0
        )

    # S-N curve intercept
    a = 10 ** sn_log_a

    # RMS stress range (Rayleigh parameter)
    stress_range_rms = 2.0 * math.sqrt(2.0 * m0)

    # Number of cycles in exposure
    n_cycles = duration_seconds / tz_seconds

    # Narrow-band damage using gamma function
    gamma_factor = math.gamma(1.0 + sn_m / 2.0)
    damage = (n_cycles / a) * (stress_range_rms ** sn_m) * gamma_factor

    return FatigueDamageResult(
        damage=damage,
        cycles=n_cycles,
        stress_range_rms=stress_range_rms,
    )
