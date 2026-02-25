#!/usr/bin/env python3
"""
ABOUTME: Wave spectrum generation for offshore hydrodynamic analysis including
JONSWAP, Pierson-Moskowitz, Bretschneider, and ISSC spectra.
"""

import numpy as np
from typing import Tuple, Optional, Dict
from .models import WaveParameters, WaveSpectrumType


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

    def jonswap(
        self,
        hs: float,
        tp: float,
        gamma: float = 3.3,
        freq_min: float = 0.02,
        freq_max: float = 2.0,
        n_points: int = 100
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate JONSWAP wave spectrum

        S(ω) = α·g²/ω⁵ · exp[-1.25(ω_p/ω)⁴] · γ^exp[-(ω-ω_p)²/(2σ²ω_p²)]

        Args:
            hs: Significant wave height (m)
            tp: Peak period (s)
            gamma: Peakedness parameter (typically 1-7, default 3.3)
            freq_min: Minimum frequency (rad/s)
            freq_max: Maximum frequency (rad/s)
            n_points: Number of frequency points

        Returns:
            (frequencies, spectral_densities) in rad/s and m²·s

        Reference:
            DNV-RP-C205 Section 3.5.2
        """
        # Frequency array
        omega = np.linspace(freq_min, freq_max, n_points)

        # Peak frequency
        omega_p = 2 * np.pi / tp

        # Calculate alpha (DNV-RP-C205)
        alpha = 5.0 / 16.0 * hs**2 * omega_p**4 / self.g**2

        # Sigma parameter (spectral width)
        sigma = np.where(omega <= omega_p, 0.07, 0.09)

        # Pierson-Moskowitz part
        S_PM = alpha * self.g**2 / omega**5 * np.exp(-1.25 * (omega_p / omega)**4)

        # JONSWAP peak enhancement factor
        A = np.exp(-(omega - omega_p)**2 / (2 * sigma**2 * omega_p**2))
        enhancement = gamma ** A

        # Complete JONSWAP spectrum
        S_jonswap = S_PM * enhancement

        return omega, S_jonswap

    def pierson_moskowitz(
        self,
        hs: float,
        tp: float,
        freq_min: float = 0.02,
        freq_max: float = 2.0,
        n_points: int = 100
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate Pierson-Moskowitz wave spectrum (fully developed seas)

        S(ω) = α·g²/ω⁵ · exp[-β(ω_p/ω)⁴]

        Args:
            hs: Significant wave height (m)
            tp: Peak period (s)
            freq_min: Minimum frequency (rad/s)
            freq_max: Maximum frequency (rad/s)
            n_points: Number of frequency points

        Returns:
            (frequencies, spectral_densities) in rad/s and m²·s

        Reference:
            DNV-RP-C205 Section 3.5.1
        """
        # Frequency array
        omega = np.linspace(freq_min, freq_max, n_points)

        # Peak frequency
        omega_p = 2 * np.pi / tp

        # PM parameters
        alpha = 5.0 / 16.0 * hs**2 * omega_p**4 / self.g**2
        beta = 1.25

        # PM spectrum
        S_pm = alpha * self.g**2 / omega**5 * np.exp(-beta * (omega_p / omega)**4)

        return omega, S_pm

    def bretschneider(
        self,
        hs: float,
        tp: float,
        freq_min: float = 0.02,
        freq_max: float = 2.0,
        n_points: int = 100
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate Bretschneider two-parameter wave spectrum

        S(ω) = (5/16)·Hs²·ω_p⁴/ω⁵ · exp[-(5/4)(ω_p/ω)⁴]

        Args:
            hs: Significant wave height (m)
            tp: Peak period (s)
            freq_min: Minimum frequency (rad/s)
            freq_max: Maximum frequency (rad/s)
            n_points: Number of frequency points

        Returns:
            (frequencies, spectral_densities) in rad/s and m²·s
        """
        # Frequency array
        omega = np.linspace(freq_min, freq_max, n_points)

        # Peak frequency
        omega_p = 2 * np.pi / tp

        # Bretschneider spectrum
        A = 5.0 / 16.0 * hs**2 * omega_p**4
        B = 5.0 / 4.0 * (omega_p / omega)**4

        S_bs = A / omega**5 * np.exp(-B)

        return omega, S_bs

    def issc(
        self,
        hs: float,
        tp: float,
        freq_min: float = 0.02,
        freq_max: float = 2.0,
        n_points: int = 100
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate ISSC modified Pierson-Moskowitz spectrum

        S(f) = 0.11·Hs²·T₁⁻⁴·f⁻⁵ · exp[-0.44(T₁·f)⁻⁴]
        where T₁ = average period ≈ 0.772·Tp

        Args:
            hs: Significant wave height (m)
            tp: Peak period (s)
            freq_min: Minimum frequency (rad/s)
            freq_max: Maximum frequency (rad/s)
            n_points: Number of frequency points

        Returns:
            (frequencies, spectral_densities) in rad/s and m²·s
        """
        # Frequency array
        omega = np.linspace(freq_min, freq_max, n_points)
        f = omega / (2 * np.pi)  # Convert to Hz

        # Average period
        T1 = 0.772 * tp

        # ISSC spectrum in Hz
        S_f = 0.11 * hs**2 * T1**(-4) * f**(-5) * np.exp(-0.44 * (T1 * f)**(-4))

        # Convert to rad/s: S(ω) = S(f) / (2π)
        S_omega = S_f / (2 * np.pi)

        return omega, S_omega

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
        if hasattr(np, "trapezoid"):
            return float(np.trapezoid(integrand, frequencies))
        return float(np.trapz(integrand, frequencies))

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
            Dictionary with spectrum statistics
        """
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
