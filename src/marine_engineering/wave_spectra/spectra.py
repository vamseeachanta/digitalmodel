"""
Wave Spectrum Models

Implements JONSWAP and Pierson-Moskowitz wave spectra with spectral
moment calculations for marine engineering applications.

Based on:
- DNV-RP-C205: Environmental conditions and environmental loads
- API RP 2A: Wave spectrum models for offshore structures
- ISO 19901-1: Metocean design and operating considerations
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Tuple

import numpy as np


@dataclass
class WaveSpectrumParameters:
    """
    Parameters defining wave spectrum.

    Attributes:
        Hs: Significant wave height [m]
        Tp: Peak period [s]
        gamma: JONSWAP peak enhancement factor (default: 3.3)
        freq_range: Frequency range [Hz] (default: 0.01 to 2.0 Hz)
        n_frequencies: Number of frequency bins (default: 100)
    """
    Hs: float
    Tp: float
    gamma: float = 3.3
    freq_range: Tuple[float, float] = (0.01, 2.0)
    n_frequencies: int = 100

    def __post_init__(self):
        """Validate parameters."""
        if self.Hs <= 0:
            raise ValueError(f"Hs must be positive, got {self.Hs}")
        if self.Tp <= 0:
            raise ValueError(f"Tp must be positive, got {self.Tp}")
        if self.gamma <= 0:
            raise ValueError(f"gamma must be positive, got {self.gamma}")
        if self.freq_range[0] >= self.freq_range[1]:
            raise ValueError(
                f"Invalid frequency range: {self.freq_range}. "
                "First value must be less than second value."
            )
        if self.n_frequencies < 10:
            raise ValueError(
                f"n_frequencies must be at least 10, got {self.n_frequencies}"
            )


class WaveSpectrum(ABC):
    """
    Abstract base class for wave spectrum models.

    Provides common functionality for spectral analysis including
    spectral moment calculations and derived wave parameters.
    """

    def __init__(self, params: WaveSpectrumParameters):
        """
        Initialize wave spectrum.

        Parameters:
            params: Wave spectrum parameters
        """
        self.params = params
        self.frequencies = np.linspace(
            params.freq_range[0],
            params.freq_range[1],
            params.n_frequencies
        )
        self.omega = 2 * np.pi * self.frequencies  # rad/s

    @abstractmethod
    def compute_spectrum(self) -> np.ndarray:
        """
        Compute spectral density S(ω) at all frequencies.

        Returns:
            S: Spectral density [m²s/rad] at each frequency
        """
        pass

    def spectral_moment(self, n: int = 0) -> float:
        """
        Calculate n-th spectral moment.

        mₙ = ∫₀^∞ ωⁿ S(ω) dω

        Parameters:
            n: Moment order (0, 1, 2, 4 commonly used)

        Returns:
            Spectral moment value

        Notes:
            - m₀: Total energy (variance)
            - m₁: Used for mean period
            - m₂: Used for zero-crossing period
            - m₄: Used for spectral bandwidth
        """
        if n < 0:
            raise ValueError(f"Moment order must be non-negative, got {n}")

        S = self.compute_spectrum()
        integrand = (self.omega ** n) * S

        # Use trapezoidal integration for numerical stability
        moment = np.trapz(integrand, self.omega)

        return float(moment)

    def significant_wave_height(self) -> float:
        """
        Calculate significant wave height from spectrum.

        Hs = 4√m₀

        Returns:
            Significant wave height [m]

        Notes:
            Hs represents the average height of the highest 1/3 of waves.
            Should match input Hs for properly calibrated spectra.
        """
        m0 = self.spectral_moment(0)
        return 4.0 * np.sqrt(m0)

    def zero_crossing_period(self) -> float:
        """
        Calculate zero-upcrossing period from spectrum.

        Tz = 2π√(m₀/m₂)

        Returns:
            Zero-crossing period [s]

        Notes:
            Tz is the average period between successive zero-upcrossings.
            Related to peak period by: Tz ≈ 0.71 * Tp (for JONSWAP)
        """
        m0 = self.spectral_moment(0)
        m2 = self.spectral_moment(2)

        if m2 <= 0:
            raise ValueError("Second moment m2 must be positive for Tz calculation")

        return 2.0 * np.pi * np.sqrt(m0 / m2)

    def mean_period(self) -> float:
        """
        Calculate mean period from spectrum.

        Tm = m₀/m₁

        Returns:
            Mean period [s]

        Notes:
            Mean period from spectral analysis.
            Alternative measure to zero-crossing period.
        """
        m0 = self.spectral_moment(0)
        m1 = self.spectral_moment(1)

        if m1 <= 0:
            raise ValueError("First moment m1 must be positive for Tm calculation")

        return m0 / m1

    def spectral_bandwidth(self) -> float:
        """
        Calculate spectral bandwidth parameter.

        ε = √(1 - m₂²/(m₀m₄))

        Returns:
            Spectral bandwidth parameter [0-1]

        Notes:
            - ε = 0: narrow-band spectrum (single frequency)
            - ε = 1: broad-band spectrum (white noise)
            - Typical sea states: ε ≈ 0.6-0.8
        """
        m0 = self.spectral_moment(0)
        m2 = self.spectral_moment(2)
        m4 = self.spectral_moment(4)

        if m0 <= 0 or m4 <= 0:
            raise ValueError("Moments m0 and m4 must be positive for bandwidth calculation")

        # Ensure numerical stability (ratio should be ≤ 1)
        ratio = (m2 ** 2) / (m0 * m4)
        ratio = min(ratio, 1.0)  # Clip to [0, 1] due to numerical errors

        return np.sqrt(1.0 - ratio)

    def get_spectral_statistics(self) -> dict:
        """
        Calculate all spectral statistics.

        Returns:
            Dictionary containing:
                - Hs: Significant wave height [m]
                - Tz: Zero-crossing period [s]
                - Tm: Mean period [s]
                - bandwidth: Spectral bandwidth parameter
                - m0, m1, m2, m4: Spectral moments
        """
        m0 = self.spectral_moment(0)
        m1 = self.spectral_moment(1)
        m2 = self.spectral_moment(2)
        m4 = self.spectral_moment(4)

        return {
            'Hs': self.significant_wave_height(),
            'Tz': self.zero_crossing_period(),
            'Tm': self.mean_period(),
            'bandwidth': self.spectral_bandwidth(),
            'm0': m0,
            'm1': m1,
            'm2': m2,
            'm4': m4,
        }


class JONSWAPSpectrum(WaveSpectrum):
    """
    JONSWAP wave spectrum implementation.

    Joint North Sea Wave Project spectrum - most widely used for
    offshore engineering. Represents fetch-limited seas with peak
    enhancement.

    Mathematical Form:
        S(ω) = (α g² / ω⁵) exp(-1.25(ωₚ/ω)⁴) γ^r

    where:
        r = exp(-(ω-ωₚ)²/(2σ²ωₚ²))
        α = 0.0081 (Phillips constant)
        ωₚ = peak frequency [rad/s]
        γ = peak enhancement factor (typically 3.3)
        σ = spectral width parameter (0.07 for ω ≤ ωₚ, 0.09 for ω > ωₚ)

    Reference:
        Based on Excel formulas with 15 references.
        DNV-RP-C205: Environmental conditions and environmental loads
    """

    GRAVITY = 9.8065  # m/s² (standard gravity)

    def compute_spectrum(self) -> np.ndarray:
        """
        Compute JONSWAP spectrum S(ω).

        Returns:
            S: Spectral density [m²s/rad] at each frequency

        Notes:
            Implements the standard JONSWAP formulation with:
            - Calibrated to match input Hs
            - Peak enhancement factor γ
            - Spectral width parameter σ

            Uses normalized formulation to ensure accurate Hs recovery.
        """
        # Peak frequency [rad/s]
        omega_p = 2.0 * np.pi / self.params.Tp

        # Spectral width parameter
        # σ = 0.07 for ω ≤ ωₚ, 0.09 for ω > ωₚ
        sigma = np.where(self.omega <= omega_p, 0.07, 0.09)

        # Peak enhancement exponent
        r = np.exp(-(self.omega - omega_p) ** 2 /
                   (2.0 * sigma ** 2 * omega_p ** 2))

        # Peak enhancement factor
        peak_enhancement = self.params.gamma ** r

        # Base P-M form using Hs and Tp (this already matches Hs correctly)
        S_pm_base = (5.0 / 16.0) * (self.params.Hs ** 2) * (omega_p ** 4) / \
                    (self.omega ** 5) * np.exp(-1.25 * (omega_p / self.omega) ** 4)

        # Apply JONSWAP peak enhancement
        S = S_pm_base * peak_enhancement

        # Normalize to preserve input Hs
        # Calculate current m0
        m0_current = np.trapz(S, self.omega)
        target_m0 = (self.params.Hs / 4.0) ** 2

        # Scale spectrum to match target Hs
        S = S * (target_m0 / m0_current)

        return S


class PiersonMoskowitzSpectrum(WaveSpectrum):
    """
    Pierson-Moskowitz fully-developed sea spectrum.

    Represents fully-developed seas with unlimited fetch.
    Simplified JONSWAP with γ = 1.0 (no peak enhancement).

    Mathematical Form (using Hs and Tp):
        S(ω) = (5/16) Hs² ωₚ⁴ / ω⁵ exp(-1.25(ωₚ/ω)⁴)

    Alternative form (using wind speed):
        S(ω) = (α g² / ω⁵) exp(-β(g/(Uω))⁴)
        where α = 0.0081, β = 0.74

    Reference:
        Based on Excel formulas with 12 references.
        Classical open-ocean spectrum for fully-developed seas.
    """

    GRAVITY = 9.8065  # m/s² (standard gravity)

    def compute_spectrum(self) -> np.ndarray:
        """
        Compute Pierson-Moskowitz spectrum S(ω).

        Returns:
            S: Spectral density [m²s/rad] at each frequency

        Notes:
            Uses the Hs-Tp formulation for consistency with JONSWAP.
            Equivalent to JONSWAP with γ = 1.0.
        """
        # Peak frequency [rad/s]
        omega_p = 2.0 * np.pi / self.params.Tp

        # Pierson-Moskowitz spectrum using Hs and Tp
        S = (5.0 / 16.0) * (self.params.Hs ** 2) * (omega_p ** 4) / \
            (self.omega ** 5) * np.exp(-1.25 * (omega_p / self.omega) ** 4)

        return S
