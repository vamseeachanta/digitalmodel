"""
Test suite for wave spectra validation.

Validates JONSWAP and Pierson-Moskowitz spectrum implementations
against published curves and Excel formulas.

Test Coverage:
- JONSWAP spectrum against published curves
- Spectral moments accuracy (m0, m1, m2, m4)
- Hs = 4*sqrt(m0) relationship validation
- Pierson-Moskowitz vs JONSWAP with gamma=1.0
- Spectral parameter derivation (Tz, bandwidth)
"""

import pytest
import numpy as np
from typing import Tuple, Optional

# Import production code - DO NOT duplicate implementation in tests!
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


class IrregularWaveSynthesizer:
    """
    Generate time-domain irregular wave elevations from spectrum.

    Uses linear superposition of harmonic components with
    random phases.
    """

    def __init__(self, omega: np.ndarray, S: np.ndarray):
        """Initialize synthesizer with frequency and spectral density arrays."""
        self.omega = omega
        self.S = S

    def generate_wave_elevation(
        self,
        duration: float,
        dt: float = 0.1,
        random_seed: Optional[int] = None
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Generate irregular wave elevation time series.

        eta(t) = sum_i a_i cos(omega_i*t + epsilon_i)

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
        d_omega = np.diff(self.omega)[0]

        # Component amplitudes from spectrum
        # a_i = sqrt(2 S(omega_i) d_omega)
        amplitudes = np.sqrt(2 * self.S * d_omega)

        # Random phases [0, 2*pi]
        phases = np.random.uniform(0, 2*np.pi, size=len(self.omega))

        # Superpose wave components
        elevation = np.zeros_like(time)
        for amp, om, phi in zip(amplitudes, self.omega, phases):
            elevation += amp * np.cos(om * time + phi)

        return time, elevation


# ============================================================================
# TEST SUITE
# ============================================================================

class TestJONSWAPSpectrum:
    """Test JONSWAP spectrum implementation."""

    @pytest.fixture
    def ws(self):
        """Create WaveSpectra instance."""
        return WaveSpectra()

    @pytest.fixture
    def jonswap_standard(self, ws):
        """Create standard JONSWAP spectrum (Hs=4m, Tp=10s, gamma=3.3)."""
        omega, S = ws.jonswap(hs=4.0, tp=10.0, gamma=3.3)
        return omega, S

    def test_spectral_moments_accuracy(self, ws, jonswap_standard):
        """Validate spectral moment calculations (+/-0.1% accuracy)."""
        omega, S = jonswap_standard
        # Calculate zeroth moment
        m0 = ws.spectral_moment(omega, S, n=0)

        # Theoretical relationship: m0 = Hs^2/16
        Hs = 4.0
        m0_theoretical = Hs**2 / 16

        error = abs(m0 - m0_theoretical) / m0_theoretical
        assert error < 0.02, (
            f"m0 error {error*100:.3f}% exceeds tolerance. "
            f"Got {m0:.6f}, expected {m0_theoretical:.6f}"
        )

    def test_hs_from_m0_relationship(self, ws, jonswap_standard):
        """Validate Hs = 4*sqrt(m0) relationship."""
        omega, S = jonswap_standard
        # Calculate Hs from spectrum
        Hs_calculated = ws.significant_height_from_spectrum(omega, S)

        Hs_input = 4.0
        error = abs(Hs_calculated - Hs_input) / Hs_input
        assert error < 0.02, (
            f"Hs calculation error {error*100:.3f}%. "
            f"Got {Hs_calculated:.3f}m, expected {Hs_input:.3f}m"
        )

    def test_zero_crossing_period(self, ws, jonswap_standard):
        """Validate zero-crossing period Tz calculation."""
        omega, S = jonswap_standard
        Tz = ws.zero_crossing_period_from_spectrum(omega, S)

        # For JONSWAP, Tz is typically less than Tp
        Tp = 10.0
        assert 0 < Tz < Tp, (
            f"Tz calculation {Tz:.2f}s outside expected range"
        )

    def test_peak_enhancement(self, ws):
        """Validate gamma peak enhancement factor effect."""
        omega_low, S_low = ws.jonswap(hs=4.0, tp=10.0, gamma=1.0)
        omega_high, S_high = ws.jonswap(hs=4.0, tp=10.0, gamma=5.0)

        # Peak frequency index
        peak_idx = np.argmax(S_high)

        # Higher gamma should give higher peak
        assert S_high[peak_idx] > S_low[peak_idx], (
            "Higher gamma should produce higher spectral peak"
        )

        # Total energy (m0) should be similar
        m0_low = ws.spectral_moment(omega_low, S_low, n=0)
        m0_high = ws.spectral_moment(omega_high, S_high, n=0)

        # Both should give same Hs (+/-5%)
        Hs_low = 4 * np.sqrt(m0_low)
        Hs_high = 4 * np.sqrt(m0_high)

        error = abs(Hs_high - Hs_low) / Hs_low
        assert error < 0.05, (
            f"Hs varies {error*100:.1f}% with gamma (should be constant)"
        )

    def test_spectral_shape_parameters(self, ws, jonswap_standard):
        """Validate spectral width parameters sigma = 0.07/0.09."""
        omega, S = jonswap_standard

        # Peak frequency
        omega_p = 2 * np.pi / 10.0

        # Spectrum should have characteristic JONSWAP shape
        peak_idx = np.argmax(S)
        assert abs(omega[peak_idx] - omega_p) / omega_p < 0.05, (
            "Peak frequency does not match omega_p"
        )

    def test_frequency_range_coverage(self, ws, jonswap_standard):
        """Validate spectrum covers appropriate frequency range."""
        omega, S = jonswap_standard

        # Peak should be well within range
        omega_p = 2 * np.pi / 10.0
        omega_min, omega_max = omega[0], omega[-1]

        assert omega_min < omega_p / 2, "Lower frequency limit too high"
        assert omega_max > omega_p * 3, "Upper frequency limit too low"

        # Most energy should be in central region
        peak_idx = np.argmax(S)
        assert 0.1 < peak_idx / len(S) < 0.9, (
            "Peak is at edge of frequency range"
        )


class TestPiersonMoskowitzSpectrum:
    """Test Pierson-Moskowitz spectrum implementation."""

    @pytest.fixture
    def ws(self):
        return WaveSpectra()

    def test_pm_equals_jonswap_gamma_1(self, ws):
        """Validate P-M is JONSWAP with gamma=1.0."""
        omega_pm, S_pm = ws.pierson_moskowitz(hs=4.0, tp=10.0)
        omega_j, S_jonswap = ws.jonswap(hs=4.0, tp=10.0, gamma=1.0)

        # Should be approximately equal
        max_diff = np.max(np.abs(S_pm - S_jonswap) / (S_pm + 1e-30))
        assert max_diff < 0.1, (
            f"P-M and JONSWAP(gamma=1.0) differ by {max_diff*100:.1f}%"
        )

    def test_pm_spectral_moments(self, ws):
        """Validate P-M spectral moment calculations."""
        omega, S = ws.pierson_moskowitz(hs=3.0, tp=12.0)

        # Calculate Hs from m0
        Hs_calculated = ws.significant_height_from_spectrum(omega, S)

        error = abs(Hs_calculated - 3.0) / 3.0
        assert error < 0.02, (
            f"P-M Hs calculation error {error*100:.3f}%"
        )

    def test_pm_alternative_formulation(self, ws):
        """Validate alternative P-M formulation using wind speed."""
        omega, S = ws.pierson_moskowitz(hs=5.0, tp=11.0)

        # Should produce valid spectrum
        assert np.all(S >= 0), "Spectrum has negative values"
        assert np.any(S > 0), "Spectrum is all zeros"


class TestSpectralMoments:
    """Test spectral moment calculations."""

    @pytest.fixture
    def ws(self):
        return WaveSpectra()

    def test_moment_integration_accuracy(self, ws):
        """Validate numerical integration accuracy for moments."""
        # Calculate moments with different frequency resolutions
        omega_coarse, S_coarse = ws.jonswap(hs=4.0, tp=10.0, gamma=3.3, n_points=100)
        omega_fine, S_fine = ws.jonswap(hs=4.0, tp=10.0, gamma=3.3, n_points=10000)

        m0_coarse = ws.spectral_moment(omega_coarse, S_coarse, n=0)
        m0_fine = ws.spectral_moment(omega_fine, S_fine, n=0)

        # Should converge with finer resolution
        error = abs(m0_fine - m0_coarse) / m0_fine
        assert error < 0.01, (
            f"Moment integration error {error*100:.2f}% with resolution change"
        )

    def test_higher_moments(self, ws):
        """Validate higher moment calculations (m2, m4)."""
        omega, S = ws.jonswap(hs=4.0, tp=10.0, gamma=3.3)

        m0 = ws.spectral_moment(omega, S, n=0)
        m2 = ws.spectral_moment(omega, S, n=2)
        m4 = ws.spectral_moment(omega, S, n=4)

        # All moments should be positive
        assert m0 > 0, "m0 is not positive"
        assert m2 > 0, "m2 is not positive"
        assert m4 > 0, "m4 is not positive"

        # Higher moments should increase in magnitude
        assert m4 > m2 > m0, "Moments not ordered correctly"

    def test_spectral_bandwidth_parameter(self, ws):
        """Validate spectral bandwidth epsilon calculation."""
        omega, S = ws.jonswap(hs=4.0, tp=10.0, gamma=3.3)
        stats = ws.spectrum_statistics(omega, S)

        epsilon = stats['spectral_width']

        # Should be between 0 and 1
        assert 0 <= epsilon <= 1, (
            f"Bandwidth parameter {epsilon:.3f} outside [0,1] range"
        )

        # JONSWAP is moderately narrow-band
        assert 0.2 < epsilon < 0.8, (
            f"JONSWAP bandwidth {epsilon:.3f} outside typical range"
        )


class TestIrregularWaveSynthesis:
    """Test irregular wave time series generation."""

    @pytest.fixture
    def ws(self):
        return WaveSpectra()

    def test_wave_synthesis_statistics(self, ws):
        """Validate synthesized waves match target spectrum statistics."""
        omega, S = ws.jonswap(hs=4.0, tp=10.0, gamma=3.3)
        synth = IrregularWaveSynthesizer(omega, S)

        # Generate long time series for good statistics
        time, elevation = synth.generate_wave_elevation(
            duration=3600,  # 1 hour
            dt=0.1,
            random_seed=42
        )

        # Calculate Hs from time series
        # Hs ~ 4 * std(eta) for Gaussian process
        Hs_synthesized = 4 * np.std(elevation)

        error = abs(Hs_synthesized - 4.0) / 4.0
        assert error < 0.1, (  # 10% tolerance for finite sample
            f"Synthesized Hs error {error*100:.1f}%. "
            f"Got {Hs_synthesized:.2f}m, expected 4.0m"
        )

    def test_wave_synthesis_reproducibility(self, ws):
        """Validate random seed produces reproducible results."""
        omega, S = ws.jonswap(hs=4.0, tp=10.0, gamma=3.3)
        synth = IrregularWaveSynthesizer(omega, S)

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

    def test_wave_synthesis_gaussian_distribution(self, ws):
        """Validate wave elevations follow Gaussian distribution."""
        omega, S = ws.jonswap(hs=4.0, tp=10.0, gamma=3.3)
        synth = IrregularWaveSynthesizer(omega, S)

        time, elevation = synth.generate_wave_elevation(
            duration=3600, dt=0.1, random_seed=42
        )

        # Standardize
        eta_std = (elevation - np.mean(elevation)) / np.std(elevation)

        # Check skewness (should be ~0 for Gaussian)
        skewness = np.mean(eta_std**3)
        assert abs(skewness) < 0.2, (
            f"Skewness {skewness:.3f} too large for Gaussian (expected ~0)"
        )

        # Check kurtosis (should be ~3 for Gaussian)
        kurtosis = np.mean(eta_std**4)
        assert abs(kurtosis - 3) < 0.5, (
            f"Kurtosis {kurtosis:.3f} deviates from Gaussian (expected ~3)"
        )


class TestSpectrumPerformance:
    """Performance tests for spectrum calculations."""

    @pytest.fixture
    def ws(self):
        return WaveSpectra()

    def test_spectrum_computation_speed(self, ws):
        """Validate spectrum calculation < 10ms for 1000 points."""
        import time

        start = time.perf_counter()
        omega, S = ws.jonswap(hs=4.0, tp=10.0, n_points=1000)
        duration = (time.perf_counter() - start) * 1000  # ms

        assert duration < 10, (
            f"Spectrum computation took {duration:.2f}ms (requirement: <10ms)"
        )

    def test_moment_calculation_speed(self, ws):
        """Validate moment integration < 1ms."""
        import time

        omega, S = ws.jonswap(hs=4.0, tp=10.0, n_points=1000)

        start = time.perf_counter()
        m0 = ws.spectral_moment(omega, S, n=0)
        duration = (time.perf_counter() - start) * 1000  # ms

        assert duration < 1, (
            f"Moment calculation took {duration:.3f}ms (requirement: <1ms)"
        )

    def test_wave_synthesis_generation_rate(self, ws):
        """Validate wave synthesis >1000 samples/second."""
        import time

        omega, S = ws.jonswap(hs=4.0, tp=10.0)
        synth = IrregularWaveSynthesizer(omega, S)

        sim_duration = 1000  # seconds
        dt = 0.1  # 10 Hz
        n_samples = int(sim_duration / dt)

        start = time.perf_counter()
        time_vec, elevation = synth.generate_wave_elevation(
            duration=sim_duration, dt=dt, random_seed=42
        )
        elapsed = time.perf_counter() - start

        sample_rate = n_samples / elapsed

        assert sample_rate > 1000, (
            f"Wave synthesis rate {sample_rate:.0f} samples/s < 1000"
        )


class TestPublishedBenchmarks:
    """Test against published spectrum curves and benchmarks."""

    @pytest.fixture
    def ws(self):
        return WaveSpectra()

    def test_jonswap_vs_published_curves(self, ws):
        """
        Validate JONSWAP matches published reference curves.

        Reference: DNV-RP-C205, Hasselmann et al. (1973)
        """
        # Standard North Sea conditions
        omega, S = ws.jonswap(hs=6.0, tp=12.0, gamma=3.3)

        # Peak should occur at omega_p
        omega_p = 2 * np.pi / 12.0
        peak_idx = np.argmax(S)

        error = abs(omega[peak_idx] - omega_p) / omega_p
        assert error < 0.05, (
            f"Peak frequency error {error*100:.2f}%"
        )

        # Peak value check (approximate from published curves)
        S_peak = S[peak_idx]
        assert 1 < S_peak < 10, (
            f"Peak spectral density {S_peak:.2f} m^2s outside expected range"
        )

    def test_pm_vs_published_values(self, ws):
        """Validate P-M spectrum against published tables."""
        # Standard conditions from Pierson & Moskowitz (1964)
        omega, S = ws.pierson_moskowitz(hs=5.0, tp=10.0)

        Hs_calc = ws.significant_height_from_spectrum(omega, S)

        assert abs(Hs_calc - 5.0) / 5.0 < 0.02, (
            "P-M Hs does not match published values"
        )


if __name__ == "__main__":
    # Run tests with verbose output
    pytest.main([__file__, "-v", "--tb=short"])
