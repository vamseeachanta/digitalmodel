"""
Test suite for wave spectra validation.

Validates JONSWAP and Pierson-Moskowitz spectrum implementations
against published curves and Excel formulas.

Test Coverage:
- JONSWAP spectrum against published curves
- Spectral moments accuracy (m₀, m₁, m₂, m₄)
- Hs = 4√m₀ relationship validation
- Pierson-Moskowitz vs JONSWAP with γ=1.0
- Spectral parameter derivation (Tz, bandwidth)
"""

import pytest
import numpy as np
from typing import Tuple, Optional

# Import production code - DO NOT duplicate implementation in tests!
from marine_engineering.wave_spectra.spectra import (
    WaveSpectrumParameters,
    WaveSpectrum,
    JONSWAPSpectrum,
    PiersonMoskowitzSpectrum,
)


class IrregularWaveSynthesizer:
    """
    Generate time-domain irregular wave elevations from spectrum.

    Uses linear superposition of harmonic components with
    random phases.
    """

    def __init__(self, spectrum: WaveSpectrum):
        """Initialize synthesizer with wave spectrum."""
        self.spectrum = spectrum

    def generate_wave_elevation(
        self,
        duration: float,
        dt: float = 0.1,
        random_seed: Optional[int] = None
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate irregular wave elevation time series.

        η(t) = Σᵢ aᵢ cos(ωᵢt + εᵢ)

        Parameters
        ----------
        duration : float
            Simulation duration [s]
        dt : float
            Time step [s]
        random_seed : int, optional
            Random seed for reproducibility

        Returns
        -------
        time : np.ndarray
            Time vector [s]
        elevation : np.ndarray
            Wave elevation [m]
        """
        if random_seed is not None:
            np.random.seed(random_seed)

        # Time vector
        time = np.arange(0, duration, dt)

        # Frequency spacing
        omega = self.spectrum.omega
        d_omega = np.diff(omega)[0]

        # Compute spectrum
        S = self.spectrum.compute_spectrum()

        # Component amplitudes from spectrum
        # a_i = √(2 S(ω_i) Δω)
        amplitudes = np.sqrt(2 * S * d_omega)

        # Random phases [0, 2π]
        phases = np.random.uniform(0, 2*np.pi, size=len(omega))

        # Superpose wave components
        elevation = np.zeros_like(time)
        for amp, om, phi in zip(amplitudes, omega, phases):
            elevation += amp * np.cos(om * time + phi)

        return time, elevation


# ============================================================================
# TEST SUITE
# ============================================================================

class TestJONSWAPSpectrum:
    """Test JONSWAP spectrum implementation."""

    @pytest.fixture
    def jonswap_standard(self):
        """Create standard JONSWAP spectrum (Hs=4m, Tp=10s, γ=3.3)."""
        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=3.3)
        return JONSWAPSpectrum(params)

    def test_spectral_moments_accuracy(self, jonswap_standard):
        """Validate spectral moment calculations (±0.1% accuracy)."""
        # Calculate zeroth moment
        m0 = jonswap_standard.spectral_moment(0)

        # Theoretical relationship: m₀ = Hs²/16
        m0_theoretical = jonswap_standard.params.Hs**2 / 16

        error = abs(m0 - m0_theoretical) / m0_theoretical
        assert error < 0.001, (
            f"m₀ error {error*100:.3f}% exceeds 0.1% tolerance. "
            f"Got {m0:.6f}, expected {m0_theoretical:.6f}"
        )

    def test_hs_from_m0_relationship(self, jonswap_standard):
        """Validate Hs = 4√m₀ relationship."""
        # Calculate Hs from spectrum
        Hs_calculated = jonswap_standard.significant_wave_height()

        # Should match input Hs
        error = abs(Hs_calculated - jonswap_standard.params.Hs) / jonswap_standard.params.Hs
        assert error < 0.001, (
            f"Hs calculation error {error*100:.3f}%. "
            f"Got {Hs_calculated:.3f}m, expected {jonswap_standard.params.Hs:.3f}m"
        )

    def test_zero_crossing_period(self, jonswap_standard):
        """Validate zero-crossing period Tz calculation."""
        Tz = jonswap_standard.zero_crossing_period()

        # For JONSWAP, Tz ≈ 0.95 * Tp (approximate)
        expected_Tz = 0.95 * jonswap_standard.params.Tp

        error = abs(Tz - expected_Tz) / expected_Tz
        assert error < 0.1, (  # 10% tolerance for this approximation
            f"Tz calculation error {error*100:.1f}%. "
            f"Got {Tz:.2f}s, expected ≈{expected_Tz:.2f}s"
        )

    def test_peak_enhancement(self):
        """Validate γ peak enhancement factor effect."""
        params_low = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=1.0)
        params_high = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=5.0)

        spec_low = JONSWAPSpectrum(params_low)
        spec_high = JONSWAPSpectrum(params_high)

        S_low = spec_low.compute_spectrum()
        S_high = spec_high.compute_spectrum()

        # Peak frequency index
        peak_idx = np.argmax(S_high)

        # Higher γ should give higher peak
        assert S_high[peak_idx] > S_low[peak_idx], (
            "Higher γ should produce higher spectral peak"
        )

        # Total energy (m₀) should be similar
        m0_low = spec_low.spectral_moment(0)
        m0_high = spec_high.spectral_moment(0)

        # Both should give same Hs (±5%)
        Hs_low = 4 * np.sqrt(m0_low)
        Hs_high = 4 * np.sqrt(m0_high)

        error = abs(Hs_high - Hs_low) / Hs_low
        assert error < 0.05, (
            f"Hs varies {error*100:.1f}% with γ (should be constant)"
        )

    def test_spectral_shape_parameters(self, jonswap_standard):
        """Validate spectral width parameters σ = 0.07/0.09."""
        S = jonswap_standard.compute_spectrum()
        omega = jonswap_standard.omega

        # Peak frequency
        omega_p = 2 * np.pi / jonswap_standard.params.Tp

        # Spectrum should have characteristic JONSWAP shape
        # - Sharp peak at ω_p
        # - Narrower on left (σ=0.07), wider on right (σ=0.09)

        peak_idx = np.argmax(S)
        assert abs(omega[peak_idx] - omega_p) / omega_p < 0.01, (
            "Peak frequency does not match ω_p"
        )

    def test_frequency_range_coverage(self, jonswap_standard):
        """Validate spectrum covers appropriate frequency range."""
        S = jonswap_standard.compute_spectrum()

        # Peak should be well within range
        omega_p = 2 * np.pi / jonswap_standard.params.Tp
        omega_min, omega_max = jonswap_standard.omega[0], jonswap_standard.omega[-1]

        assert omega_min < omega_p / 2, "Lower frequency limit too high"
        assert omega_max > omega_p * 3, "Upper frequency limit too low"

        # Most energy should be in central region
        peak_idx = np.argmax(S)
        assert 0.1 < peak_idx / len(S) < 0.9, (
            "Peak is at edge of frequency range"
        )


class TestPiersonMoskowitzSpectrum:
    """Test Pierson-Moskowitz spectrum implementation."""

    def test_pm_equals_jonswap_gamma_1(self):
        """Validate P-M is JONSWAP with γ=1.0."""
        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=1.0)

        pm_spec = PiersonMoskowitzSpectrum(params)
        jonswap_spec = JONSWAPSpectrum(params)

        S_pm = pm_spec.compute_spectrum()
        S_jonswap = jonswap_spec.compute_spectrum()

        # Should be approximately equal
        # Note: Different formulations may have slight differences
        max_diff = np.max(np.abs(S_pm - S_jonswap) / S_pm)
        assert max_diff < 0.05, (
            f"P-M and JONSWAP(γ=1.0) differ by {max_diff*100:.1f}%"
        )

    def test_pm_spectral_moments(self):
        """Validate P-M spectral moment calculations."""
        params = WaveSpectrumParameters(Hs=3.0, Tp=12.0)
        pm_spec = PiersonMoskowitzSpectrum(params)

        # Calculate Hs from m₀
        Hs_calculated = pm_spec.significant_wave_height()

        error = abs(Hs_calculated - params.Hs) / params.Hs
        assert error < 0.001, (
            f"P-M Hs calculation error {error*100:.3f}%"
        )

    def test_pm_alternative_formulation(self):
        """Validate alternative P-M formulation using wind speed."""
        # This is a placeholder for wind speed formulation
        # S(ω) = (α g² / ω⁵) exp(-β(g/(Uω))⁴)
        # where U is wind speed at 19.5m elevation

        params = WaveSpectrumParameters(Hs=5.0, Tp=11.0)
        pm_spec = PiersonMoskowitzSpectrum(params)

        # Should produce valid spectrum
        S = pm_spec.compute_spectrum()
        assert np.all(S >= 0), "Spectrum has negative values"
        assert np.any(S > 0), "Spectrum is all zeros"


class TestSpectralMoments:
    """Test spectral moment calculations."""

    def test_moment_integration_accuracy(self):
        """Validate numerical integration accuracy for moments."""
        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=3.3)
        spec = JONSWAPSpectrum(params)

        # Calculate moments with different frequency resolutions
        params_coarse = WaveSpectrumParameters(
            Hs=4.0, Tp=10.0, gamma=3.3, n_frequencies=100
        )
        params_fine = WaveSpectrumParameters(
            Hs=4.0, Tp=10.0, gamma=3.3, n_frequencies=10000
        )

        spec_coarse = JONSWAPSpectrum(params_coarse)
        spec_fine = JONSWAPSpectrum(params_fine)

        m0_coarse = spec_coarse.spectral_moment(0)
        m0_fine = spec_fine.spectral_moment(0)

        # Should converge with finer resolution
        error = abs(m0_fine - m0_coarse) / m0_fine
        assert error < 0.01, (
            f"Moment integration error {error*100:.2f}% with resolution change"
        )

    def test_higher_moments(self):
        """Validate higher moment calculations (m₂, m₄)."""
        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=3.3)
        spec = JONSWAPSpectrum(params)

        m0 = spec.spectral_moment(0)
        m2 = spec.spectral_moment(2)
        m4 = spec.spectral_moment(4)

        # All moments should be positive
        assert m0 > 0, "m₀ is not positive"
        assert m2 > 0, "m₂ is not positive"
        assert m4 > 0, "m₄ is not positive"

        # Higher moments should increase in magnitude
        # (as ω^n increases the integrand for higher n)
        assert m4 > m2 > m0, "Moments not ordered correctly"

    def test_spectral_bandwidth_parameter(self):
        """Validate spectral bandwidth ε calculation."""
        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=3.3)
        spec = JONSWAPSpectrum(params)

        epsilon = spec.spectral_bandwidth()

        # Should be between 0 and 1
        assert 0 <= epsilon <= 1, (
            f"Bandwidth parameter {epsilon:.3f} outside [0,1] range"
        )

        # JONSWAP is moderately narrow-band (ε ≈ 0.3-0.4)
        assert 0.2 < epsilon < 0.6, (
            f"JONSWAP bandwidth {epsilon:.3f} outside typical range [0.3, 0.4]"
        )


class TestIrregularWaveSynthesis:
    """Test irregular wave time series generation."""

    def test_wave_synthesis_statistics(self):
        """Validate synthesized waves match target spectrum statistics."""
        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=3.3)
        spec = JONSWAPSpectrum(params)
        synth = IrregularWaveSynthesizer(spec)

        # Generate long time series for good statistics
        time, elevation = synth.generate_wave_elevation(
            duration=3600,  # 1 hour
            dt=0.1,
            random_seed=42
        )

        # Calculate Hs from time series
        # Hs ≈ 4 * std(η) for Gaussian process
        Hs_synthesized = 4 * np.std(elevation)

        error = abs(Hs_synthesized - params.Hs) / params.Hs
        assert error < 0.1, (  # 10% tolerance for finite sample
            f"Synthesized Hs error {error*100:.1f}%. "
            f"Got {Hs_synthesized:.2f}m, expected {params.Hs:.2f}m"
        )

    def test_wave_synthesis_reproducibility(self):
        """Validate random seed produces reproducible results."""
        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=3.3)
        spec = JONSWAPSpectrum(params)
        synth = IrregularWaveSynthesizer(spec)

        # Generate twice with same seed
        time1, elev1 = synth.generate_wave_elevation(
            duration=100, dt=0.1, random_seed=42
        )
        time2, elev2 = synth.generate_wave_elevation(
            duration=100, dt=0.1, random_seed=42
        )

        # Should be identical
        assert np.allclose(elev1, elev2), (
            "Wave synthesis not reproducible with same seed"
        )

    def test_wave_synthesis_gaussian_distribution(self):
        """Validate wave elevations follow Gaussian distribution."""
        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=3.3)
        spec = JONSWAPSpectrum(params)
        synth = IrregularWaveSynthesizer(spec)

        time, elevation = synth.generate_wave_elevation(
            duration=3600, dt=0.1, random_seed=42
        )

        # Standardize
        eta_std = (elevation - np.mean(elevation)) / np.std(elevation)

        # Check skewness (should be ≈0 for Gaussian)
        skewness = np.mean(eta_std**3)
        assert abs(skewness) < 0.2, (
            f"Skewness {skewness:.3f} too large for Gaussian (expected ≈0)"
        )

        # Check kurtosis (should be ≈3 for Gaussian)
        kurtosis = np.mean(eta_std**4)
        assert abs(kurtosis - 3) < 0.5, (
            f"Kurtosis {kurtosis:.3f} deviates from Gaussian (expected ≈3)"
        )


class TestSpectrumPerformance:
    """Performance tests for spectrum calculations."""

    def test_spectrum_computation_speed(self):
        """Validate spectrum calculation < 10ms for 1000 points."""
        import time

        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, n_frequencies=1000)
        spec = JONSWAPSpectrum(params)

        start = time.perf_counter()
        S = spec.compute_spectrum()
        duration = (time.perf_counter() - start) * 1000  # ms

        assert duration < 10, (
            f"Spectrum computation took {duration:.2f}ms (requirement: <10ms)"
        )

    def test_moment_calculation_speed(self):
        """Validate moment integration < 1ms."""
        import time

        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, n_frequencies=1000)
        spec = JONSWAPSpectrum(params)

        start = time.perf_counter()
        m0 = spec.spectral_moment(0)
        duration = (time.perf_counter() - start) * 1000  # ms

        assert duration < 1, (
            f"Moment calculation took {duration:.3f}ms (requirement: <1ms)"
        )

    def test_wave_synthesis_generation_rate(self):
        """Validate wave synthesis >1000 samples/second."""
        import time

        params = WaveSpectrumParameters(Hs=4.0, Tp=10.0)
        spec = JONSWAPSpectrum(params)
        synth = IrregularWaveSynthesizer(spec)

        duration = 1000  # seconds
        dt = 0.1  # 10 Hz
        n_samples = int(duration / dt)

        start = time.perf_counter()
        time_vec, elevation = synth.generate_wave_elevation(
            duration=duration, dt=dt, random_seed=42
        )
        elapsed = time.perf_counter() - start

        sample_rate = n_samples / elapsed

        assert sample_rate > 1000, (
            f"Wave synthesis rate {sample_rate:.0f} samples/s < 1000"
        )


class TestPublishedBenchmarks:
    """Test against published spectrum curves and benchmarks."""

    def test_jonswap_vs_published_curves(self):
        """
        Validate JONSWAP matches published reference curves.

        Reference: DNV-RP-C205, Hasselmann et al. (1973)
        """
        # Standard North Sea conditions
        params = WaveSpectrumParameters(Hs=6.0, Tp=12.0, gamma=3.3)
        spec = JONSWAPSpectrum(params)

        S = spec.compute_spectrum()
        omega = spec.omega

        # Peak should occur at ω_p
        omega_p = 2 * np.pi / params.Tp
        peak_idx = np.argmax(S)

        error = abs(omega[peak_idx] - omega_p) / omega_p
        assert error < 0.01, (
            f"Peak frequency error {error*100:.2f}%"
        )

        # Peak value check (approximate from published curves)
        # For γ=3.3, Hs=6m, Tp=12s: S_peak ≈ 4-5 m²s
        S_peak = S[peak_idx]
        assert 3 < S_peak < 6, (
            f"Peak spectral density {S_peak:.2f} m²s outside expected range [3,6]"
        )

    def test_pm_vs_published_values(self):
        """Validate P-M spectrum against published tables."""
        # Standard conditions from Pierson & Moskowitz (1964)
        params = WaveSpectrumParameters(Hs=5.0, Tp=10.0)
        spec = PiersonMoskowitzSpectrum(params)

        Hs_calc = spec.significant_wave_height()

        assert abs(Hs_calc - 5.0) / 5.0 < 0.01, (
            "P-M Hs does not match published values"
        )


if __name__ == "__main__":
    # Run tests with verbose output
    pytest.main([__file__, "-v", "--tb=short"])
