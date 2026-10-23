#!/usr/bin/env python3
"""
ABOUTME: Wave spectrum generation for offshore hydrodynamic analysis including
JONSWAP, Pierson-Moskowitz, Bretschneider, and ISSC spectra.
"""

import numpy as np
from typing import Tuple, Optional, Dict
from .models import (
    DEFAULT_FREQUENCY_POINTS,
    WaveParameters,
    WaveSpectrumType,
    wave_frequency_grid,
)

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


class WaveSpectra:
    """
    Wave spectrum generation and analysis

    Implements standard wave spectra used in offshore engineering:
    - JONSWAP (fetch-limited seas, North Sea)
    - Pierson-Moskowitz (fully developed seas)
    - Bretschneider (two-parameter spectrum)
    - ISSC (modified Pierson-Moskowitz)

    Reference: DNV-RP-C205 Environmental Conditions and Environmental Loads
    """

    def __init__(self):
        """Initialize wave spectra generator"""
        self.g = 9.81  # m/s²

    def _normalize_to_significant_height(
        self,
        frequencies: np.ndarray,
        spectrum: np.ndarray,
        hs: float
    ) -> np.ndarray:
        """Scale a discrete spectrum so its integrated energy matches the target Hs."""
        target_m0 = hs**2 / 16.0
        if target_m0 <= 0.0:
            return np.zeros_like(spectrum)

        current_m0 = self.spectral_moment(frequencies, spectrum, n=0)
        if current_m0 <= 0.0:
            return np.zeros_like(spectrum)

        return spectrum * (target_m0 / current_m0)

    def jonswap(
        self,
        hs: float,
        tp: float,
        gamma: float = 3.3,
        freq_min: float = 0.02,
        freq_max: Optional[float] = None,
        n_points: int = DEFAULT_FREQUENCY_POINTS
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate JONSWAP wave spectrum

        S(ω) = α·g²/ω⁵ · exp[-1.25(ω_p/ω)⁴] · γ^exp[-(ω-ω_p)²/(2σ²ω_p²)]

        Args:
            hs: Significant wave height (m)
            tp: Peak period (s)
            gamma: Peakedness parameter (typically 1-7, default 3.3)
            freq_min: Minimum frequency (rad/s)
            freq_max: Maximum frequency (rad/s); None uses 20 times the peak
            n_points: Number of uniformly spaced frequency points

        Returns:
            (frequencies, spectral_densities) in rad/s and m²·s

        Reference:
            DNV-RP-C205 Section 3.5.2
        """
        # Grid error budget: docs/domains/hydrodynamics/wave-spectra.md.
        if not np.isfinite(hs) or hs < 0:
            raise ValueError("Significant height must be finite and non-negative")
        omega = wave_frequency_grid(tp, freq_min, freq_max, n_points)

        if not np.isfinite(gamma) or gamma <= 0:
            raise ValueError("Gamma must be finite and positive")

        # Peak frequency
        omega_p = 2 * np.pi / tp

        # Calculate alpha (DNV-RP-C205)
        alpha = 5.0 / 16.0 * hs**2 * omega_p**4 / self.g**2

        # Orcina online help (accessed 2026), Environment: Data for JONSWAP
        # and ISSC spectra, "Spectral width parameters sigma1 and sigma2".
        # https://www.orcina.com/webhelp/OrcaFlex/Content/html/Environment,DataforJONSWAPandISSCspectra.htm
        # Sigma parameter (spectral width)
        sigma = np.where(omega <= omega_p, 0.07, 0.09)

        # Pierson-Moskowitz part
        S_PM = alpha * self.g**2 / omega**5 * np.exp(-1.25 * (omega_p / omega)**4)

        # JONSWAP peak enhancement factor
        A = np.exp(-(omega - omega_p)**2 / (2 * sigma**2 * omega_p**2))
        enhancement = gamma ** A

        # Complete JONSWAP spectrum
        S_jonswap = S_PM * enhancement

        return omega, self._normalize_to_significant_height(omega, S_jonswap, hs)

    def pierson_moskowitz(
        self,
        hs: float,
        tp: float,
        freq_min: float = 0.02,
        freq_max: Optional[float] = None,
        n_points: int = DEFAULT_FREQUENCY_POINTS
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate Pierson-Moskowitz wave spectrum (fully developed seas)

        S(ω) = α·g²/ω⁵ · exp[-β(ω_p/ω)⁴]

        Args:
            hs: Significant wave height (m)
            tp: Peak period (s)
            freq_min: Minimum frequency (rad/s)
            freq_max: Maximum frequency (rad/s); None uses 20 times the peak
            n_points: Number of uniformly spaced frequency points

        Returns:
            (frequencies, spectral_densities) in rad/s and m²·s

        Reference:
            DNV-RP-C205 Section 3.5.1
        """
        # Grid error budget: docs/domains/hydrodynamics/wave-spectra.md.
        if not np.isfinite(hs) or hs < 0:
            raise ValueError("Significant height must be finite and non-negative")
        omega = wave_frequency_grid(tp, freq_min, freq_max, n_points)

        # Peak frequency
        omega_p = 2 * np.pi / tp

        # PM parameters
        alpha = 5.0 / 16.0 * hs**2 * omega_p**4 / self.g**2
        beta = 1.25

        # PM spectrum
        S_pm = alpha * self.g**2 / omega**5 * np.exp(-beta * (omega_p / omega)**4)

        return omega, self._normalize_to_significant_height(omega, S_pm, hs)

    def bretschneider(
        self,
        hs: float,
        tp: float,
        freq_min: float = 0.02,
        freq_max: Optional[float] = None,
        n_points: int = DEFAULT_FREQUENCY_POINTS
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate Bretschneider two-parameter wave spectrum

        S(ω) = (5/16)·Hs²·ω_p⁴/ω⁵ · exp[-(5/4)(ω_p/ω)⁴]

        Args:
            hs: Significant wave height (m)
            tp: Peak period (s)
            freq_min: Minimum frequency (rad/s)
            freq_max: Maximum frequency (rad/s); None uses 20 times the peak
            n_points: Number of uniformly spaced frequency points

        Returns:
            (frequencies, spectral_densities) in rad/s and m²·s
        """
        # Grid error budget: docs/domains/hydrodynamics/wave-spectra.md.
        if not np.isfinite(hs) or hs < 0:
            raise ValueError("Significant height must be finite and non-negative")
        omega = wave_frequency_grid(tp, freq_min, freq_max, n_points)

        # Peak frequency
        omega_p = 2 * np.pi / tp

        # Bretschneider spectrum
        A = 5.0 / 16.0 * hs**2 * omega_p**4
        B = 5.0 / 4.0 * (omega_p / omega)**4

        S_bs = A / omega**5 * np.exp(-B)

        return omega, self._normalize_to_significant_height(omega, S_bs, hs)

    def issc(
        self,
        hs: float,
        tp: float,
        freq_min: float = 0.02,
        freq_max: Optional[float] = None,
        n_points: int = DEFAULT_FREQUENCY_POINTS
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate ISSC modified Pierson-Moskowitz spectrum

        S(f) = 0.11·Hs²·T₁⁻⁴·f⁻⁵ · exp[-0.44(T₁·f)⁻⁴]
        where T₁ = average period ≈ 0.772·Tp

        Args:
            hs: Significant wave height (m)
            tp: Peak period (s)
            freq_min: Minimum frequency (rad/s)
            freq_max: Maximum frequency (rad/s); None uses 20 times the peak
            n_points: Number of uniformly spaced frequency points

        Returns:
            (frequencies, spectral_densities) in rad/s and m²·s
        """
        # Grid error budget: docs/domains/hydrodynamics/wave-spectra.md.
        if not np.isfinite(hs) or hs < 0:
            raise ValueError("Significant height must be finite and non-negative")
        omega = wave_frequency_grid(tp, freq_min, freq_max, n_points)
        f = omega / (2 * np.pi)  # Convert to Hz

        # Average period
        T1 = 0.772 * tp

        # ISSC spectrum in Hz
        S_f = 0.11 * hs**2 * T1**(-4) * f**(-5) * np.exp(-0.44 * (T1 * f)**(-4))

        # Convert to rad/s: S(ω) = S(f) / (2π)
        S_omega = S_f / (2 * np.pi)

        return omega, self._normalize_to_significant_height(omega, S_omega, hs)

    def generate_spectrum(
        self,
        params: WaveParameters
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate wave spectrum based on WaveParameters

        Args:
            params: WaveParameters object

        Returns:
            (frequencies, spectral_densities) in rad/s and m²·s
        """
        if params.spectrum_type == WaveSpectrumType.JONSWAP:
            return self.jonswap(
                hs=params.significant_height,
                tp=params.peak_period,
                gamma=params.gamma,
                freq_min=params.freq_min,
                freq_max=params.freq_max,
                n_points=params.n_frequencies
            )
        elif params.spectrum_type == WaveSpectrumType.PIERSON_MOSKOWITZ:
            return self.pierson_moskowitz(
                hs=params.significant_height,
                tp=params.peak_period,
                freq_min=params.freq_min,
                freq_max=params.freq_max,
                n_points=params.n_frequencies
            )
        elif params.spectrum_type == WaveSpectrumType.BRETSCHNEIDER:
            return self.bretschneider(
                hs=params.significant_height,
                tp=params.peak_period,
                freq_min=params.freq_min,
                freq_max=params.freq_max,
                n_points=params.n_frequencies
            )
        elif params.spectrum_type == WaveSpectrumType.ISSC:
            return self.issc(
                hs=params.significant_height,
                tp=params.peak_period,
                freq_min=params.freq_min,
                freq_max=params.freq_max,
                n_points=params.n_frequencies
            )
        else:
            raise ValueError(f"Spectrum type {params.spectrum_type} not implemented")

    def spectral_moment(
        self,
        frequencies: np.ndarray,
        spectrum: np.ndarray,
        n: int = 0
    ) -> float:
        """
        Calculate n-th spectral moment

        m_n = ∫ ω^n · S(ω) dω

        Args:
            frequencies: Frequency array (rad/s)
            spectrum: Spectral density array (m²·s)
            n: Moment order (0, 1, 2, etc.)

        Returns:
            Spectral moment
        """
        integrand = frequencies**n * spectrum
        return float(_trapezoid(integrand, frequencies))

    def significant_height_from_spectrum(
        self,
        frequencies: np.ndarray,
        spectrum: np.ndarray
    ) -> float:
        """
        Calculate significant wave height from spectrum

        Hs = 4·√m₀

        Args:
            frequencies: Frequency array (rad/s)
            spectrum: Spectral density array (m²·s)

        Returns:
            Significant wave height (m)
        """
        m0 = self.spectral_moment(frequencies, spectrum, n=0)
        return 4.0 * np.sqrt(m0)

    def zero_crossing_period_from_spectrum(
        self,
        frequencies: np.ndarray,
        spectrum: np.ndarray
    ) -> float:
        """
        Calculate zero-crossing period from spectrum

        Tz = 2π·√(m₀/m₂)

        Args:
            frequencies: Frequency array (rad/s)
            spectrum: Spectral density array (m²·s)

        Returns:
            Zero-crossing period (s)
        """
        m0 = self.spectral_moment(frequencies, spectrum, n=0)
        m2 = self.spectral_moment(frequencies, spectrum, n=2)

        return 2 * np.pi * np.sqrt(m0 / m2)

    def energy_period_from_spectrum(
        self,
        frequencies: np.ndarray,
        spectrum: np.ndarray,
    ) -> float:
        """Return Te = 2*pi*m(-1)/m0 in seconds; NaN for a calm spectrum.

        Angular-frequency moments require the factor 2*pi. This is the
        energy-weighted mean period, derived by weighting T=2*pi/omega
        with S(omega)domega. See WEC-Sim Theory (accessed 2026), Irregular
        Waves: spectral moments, and the derivation in wave-spectra.md.
        """
        m0 = self.spectral_moment(frequencies, spectrum, n=0)
        if m0 == 0:
            return float("nan")
        return 2 * np.pi * self.spectral_moment(frequencies, spectrum, n=-1) / m0

    def deep_water_wave_power(
        self,
        frequencies: np.ndarray,
        spectrum: np.ndarray,
        *,
        water_density: float,
        gravity: float,
    ) -> float:
        """Return incident deep-water flux per crest width in W/m.

        Density (kg/m^3) and gravity (m/s^2) are required physical inputs.
        WEC-Sim Theory (accessed 2026), Irregular Wave Power gives
        P=rho*g*integral(S*cg); deep-water cg=g/(2*omega) gives
        P=rho*g**2*m(-1)/2. No finite-depth or directional correction is
        applied. This is available incident power, not extracted power.
        https://wec-sim.github.io/WEC-Sim/main/theory/theory.html#irregular-wave-power
        """
        if not np.isfinite(water_density) or water_density <= 0:
            raise ValueError("water_density must be finite and positive")
        if not np.isfinite(gravity) or gravity <= 0:
            raise ValueError("gravity must be finite and positive")
        return water_density * gravity**2 / 2 * self.spectral_moment(
            frequencies, spectrum, n=-1
        )

    def peak_frequency_from_spectrum(
        self,
        frequencies: np.ndarray,
        spectrum: np.ndarray
    ) -> float:
        """
        Find peak frequency from spectrum

        Args:
            frequencies: Frequency array (rad/s)
            spectrum: Spectral density array (m²·s)

        Returns:
            Peak frequency (rad/s)
        """
        peak_idx = np.argmax(spectrum)
        return frequencies[peak_idx]

    def spectrum_statistics(
        self,
        frequencies: np.ndarray,
        spectrum: np.ndarray
    ) -> Dict[str, float]:
        """
        Calculate comprehensive spectrum statistics

        Args:
            frequencies: Frequency array (rad/s)
            spectrum: Spectral density array (m²·s)

        Returns:
            Dictionary with angular-frequency moments, Hs, Tz, Te and peak.
            The fourth moment and spectral width depend on the finite cutoff:
            an ideal Pierson-Moskowitz/JONSWAP omega^-5 tail has a divergent
            fourth moment.
        """
        m_minus1 = self.spectral_moment(frequencies, spectrum, n=-1)
        m0 = self.spectral_moment(frequencies, spectrum, n=0)
        m1 = self.spectral_moment(frequencies, spectrum, n=1)
        m2 = self.spectral_moment(frequencies, spectrum, n=2)
        m4 = self.spectral_moment(frequencies, spectrum, n=4)

        hs = 4.0 * np.sqrt(m0)
        tz = 2 * np.pi * np.sqrt(m0 / m2)
        omega_p = self.peak_frequency_from_spectrum(frequencies, spectrum)
        tp = 2 * np.pi / omega_p

        # Spectral width parameter
        epsilon = np.sqrt(1 - m2**2 / (m0 * m4))

        return {
            'm_minus1': m_minus1,
            'Te_s': self.energy_period_from_spectrum(frequencies, spectrum),
            'm0': m0,
            'm1': m1,
            'm2': m2,
            'm4': m4,
            'Hs_m': hs,
            'Tz_s': tz,
            'Tp_s': tp,
            'omega_p_rad_s': omega_p,
            'spectral_width': epsilon,
        }
