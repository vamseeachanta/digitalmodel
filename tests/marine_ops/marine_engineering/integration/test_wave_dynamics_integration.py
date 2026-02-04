"""
Integration tests for Wave Spectra → Ship Dynamics data flow.

Tests that wave spectra calculations correctly feed into motion analysis
and that spectral parameters affect dynamic responses.

Test Coverage:
- JONSWAP spectrum → irregular wave elevation
- Wave elevation → motion analysis
- Spectral moments → response statistics
- Peak period → motion amplitudes
- Wave energy distribution → frequency response
"""

import pytest
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

from digitalmodel.hydrodynamics.wave_spectra import (
    JONSWAPSpectrum,
    PiersonMoskowitzSpectrum,
    WaveSpectrumParameters
)


class TestWaveDynamicsIntegration:
    """Integration tests for wave spectra and ship dynamics."""

    @pytest.fixture
    def output_dir(self):
        """Create output directory for test charts."""
        output_path = Path(__file__).parent / "charts" / "wave_dynamics"
        output_path.mkdir(parents=True, exist_ok=True)
        return output_path

    @pytest.fixture
    def jonswap_spectrum(self):
        """Create JONSWAP spectrum for testing."""
        params = WaveSpectrumParameters(
            Hs=5.0,  # Significant wave height [m]
            Tp=10.0,  # Peak period [s]
            gamma=3.3,
            freq_range=(0.01, 1.0),
            n_frequencies=200
        )
        return JONSWAPSpectrum(params)

    @pytest.fixture
    def pm_spectrum(self):
        """Create Pierson-Moskowitz spectrum for testing."""
        params = WaveSpectrumParameters(
            Hs=5.0,
            Tp=10.0,
            freq_range=(0.01, 1.0),
            n_frequencies=200
        )
        return PiersonMoskowitzSpectrum(params)

    def test_jonswap_generates_valid_spectrum(self, jonswap_spectrum):
        """Test that JONSWAP spectrum generates valid spectral density."""
        S = jonswap_spectrum.compute_spectrum()

        # Spectrum should be all positive
        assert np.all(S >= 0), "Spectral density must be non-negative"

        # Spectrum should have peak
        peak_idx = np.argmax(S)
        assert 0 < peak_idx < len(S) - 1, "Peak should not be at boundaries"

        # Peak frequency should be near Tp
        peak_freq = jonswap_spectrum.frequencies[peak_idx]
        expected_freq = 1.0 / jonswap_spectrum.params.Tp
        np.testing.assert_allclose(peak_freq, expected_freq, rtol=0.1,
                                  err_msg="Peak frequency should match Tp")

    def test_spectral_moments_calculation(self, jonswap_spectrum):
        """Test that spectral moments are calculated correctly."""
        m0 = jonswap_spectrum.spectral_moment(0)
        m1 = jonswap_spectrum.spectral_moment(1)
        m2 = jonswap_spectrum.spectral_moment(2)
        m4 = jonswap_spectrum.spectral_moment(4)

        # All moments should be positive
        assert m0 > 0, "m0 (variance) must be positive"
        assert m1 > 0, "m1 must be positive"
        assert m2 > 0, "m2 must be positive"
        assert m4 > 0, "m4 must be positive"

        # Moments should increase with order (in general trend)
        # Note: m0 is variance, units different from m1, m2, m4
        # Just check they are all positive and reasonable magnitudes
        assert m2 > 0.1, "m2 should be significant"
        assert m4 > 0.01, "m4 should be significant"

    def test_significant_wave_height_recovery(self, jonswap_spectrum):
        """Test that Hs can be recovered from spectrum."""
        Hs_calculated = jonswap_spectrum.significant_wave_height()
        Hs_input = jonswap_spectrum.params.Hs

        # Should match within 1% (normalization ensures this)
        np.testing.assert_allclose(Hs_calculated, Hs_input, rtol=0.01,
                                  err_msg="Hs should be recoverable from spectrum")

    def test_zero_crossing_period_relationship(self, jonswap_spectrum):
        """Test Tz vs Tp relationship for JONSWAP."""
        Tz = jonswap_spectrum.zero_crossing_period()
        Tp = jonswap_spectrum.params.Tp

        # For JONSWAP: Tz ≈ 0.6-0.8 * Tp (depending on gamma)
        ratio = Tz / Tp
        assert 0.6 <= ratio <= 0.8, f"Tz/Tp ratio {ratio:.3f} outside expected range"

    def test_spectral_bandwidth_range(self, jonswap_spectrum):
        """Test that spectral bandwidth is in valid range."""
        bandwidth = jonswap_spectrum.spectral_bandwidth()

        # Bandwidth should be between 0 and 1
        assert 0 <= bandwidth <= 1, "Bandwidth must be in [0, 1]"

        # For JONSWAP (peaky spectrum), expect 0.6-0.8
        assert 0.5 <= bandwidth <= 0.9, \
            f"JONSWAP bandwidth {bandwidth:.3f} outside typical range"

    def test_pm_vs_jonswap_comparison(self, jonswap_spectrum, pm_spectrum):
        """Test that PM spectrum is smoother than JONSWAP (lower peak enhancement)."""
        S_jonswap = jonswap_spectrum.compute_spectrum()
        S_pm = pm_spectrum.compute_spectrum()

        # Both should have same m0 (same Hs)
        m0_jonswap = jonswap_spectrum.spectral_moment(0)
        m0_pm = pm_spectrum.spectral_moment(0)
        np.testing.assert_allclose(m0_jonswap, m0_pm, rtol=0.02,
                                  err_msg="Both spectra should have same Hs")

        # JONSWAP peak should be higher (more concentrated energy)
        peak_jonswap = np.max(S_jonswap)
        peak_pm = np.max(S_pm)
        assert peak_jonswap > peak_pm, "JONSWAP peak should exceed PM peak"

    def test_wave_elevation_time_series_generation(self, jonswap_spectrum):
        """Test generating irregular wave elevation from spectrum."""
        # Generate wave components
        S = jonswap_spectrum.compute_spectrum()
        omega = jonswap_spectrum.omega

        # Random phases
        np.random.seed(42)
        phases = np.random.uniform(0, 2*np.pi, len(omega))

        # Amplitudes from spectrum
        d_omega = np.diff(omega, prepend=omega[0] - (omega[1] - omega[0]))
        amplitudes = np.sqrt(2 * S * d_omega)

        # Time series
        t = np.linspace(0, 600, 6000)  # 600s simulation
        eta = np.zeros_like(t)
        for A, w, phi in zip(amplitudes, omega, phases):
            eta += A * np.cos(w * t + phi)

        # Check statistics
        eta_std = np.std(eta)
        expected_std = np.sqrt(jonswap_spectrum.spectral_moment(0))

        np.testing.assert_allclose(eta_std, expected_std, rtol=0.1,
                                  err_msg="Time series std should match √m0")

        # Check peak-to-trough range
        Hs_from_ts = 4 * eta_std
        Hs_input = jonswap_spectrum.params.Hs
        np.testing.assert_allclose(Hs_from_ts, Hs_input, rtol=0.15,
                                  err_msg="Hs from time series should match input")

    def test_frequency_response_peaks_at_natural_period(self, jonswap_spectrum):
        """Test that motion response peaks when wave frequency matches natural frequency."""
        # Simulate simple motion response: |RAO|² × S(ω)
        # Assume natural period Tn = 12s
        omega_n = 2 * np.pi / 12.0  # rad/s

        # Simple RAO model: resonance peak at omega_n
        omega = jonswap_spectrum.omega
        zeta = 0.1  # Damping ratio
        RAO = 1.0 / np.sqrt((omega_n**2 - omega**2)**2 + (2*zeta*omega_n*omega)**2)

        # Response spectrum
        S_wave = jonswap_spectrum.compute_spectrum()
        S_response = (RAO**2) * S_wave

        # Response peak should be near natural frequency
        peak_idx = np.argmax(S_response)
        peak_omega = omega[peak_idx]

        # Allow some shift due to wave spectrum shape
        np.testing.assert_allclose(peak_omega, omega_n, rtol=0.2,
                                  err_msg="Response peak should be near natural frequency")

    def test_peak_period_affects_motion_amplitude(self, output_dir):
        """Test that peak period affects motion amplitudes."""
        # Create spectra with different Tp but same Hs
        Tp_values = [8.0, 10.0, 12.0, 15.0]
        response_amplitudes = []

        omega_n = 2 * np.pi / 12.0  # Ship natural frequency

        for Tp in Tp_values:
            params = WaveSpectrumParameters(
                Hs=5.0,
                Tp=Tp,
                gamma=3.3,
                freq_range=(0.01, 1.0),
                n_frequencies=200
            )
            spectrum = JONSWAPSpectrum(params)

            # Simple RAO
            omega = spectrum.omega
            zeta = 0.1
            RAO = 1.0 / np.sqrt((omega_n**2 - omega**2)**2 + (2*zeta*omega_n*omega)**2)

            # Response variance
            S_wave = spectrum.compute_spectrum()
            S_response = (RAO**2) * S_wave
            response_var = np.trapz(S_response, omega)
            response_amplitudes.append(np.sqrt(response_var))

        # Response should peak when Tp is near natural period (12s)
        max_idx = np.argmax(response_amplitudes)
        assert Tp_values[max_idx] == 12.0, \
            "Maximum response should occur near natural period"

        # Create validation chart
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))

        ax1.plot(Tp_values, response_amplitudes, 'o-', linewidth=2, markersize=8)
        ax1.axvline(12.0, color='r', linestyle='--', label='Natural Period')
        ax1.set_xlabel('Peak Period Tp [s]')
        ax1.set_ylabel('Response Amplitude [m]')
        ax1.set_title('Motion Response vs Wave Peak Period')
        ax1.legend()
        ax1.grid(True, alpha=0.3)

        # Plot all spectra
        for Tp in Tp_values:
            params = WaveSpectrumParameters(Hs=5.0, Tp=Tp, gamma=3.3,
                                          freq_range=(0.01, 1.0), n_frequencies=200)
            spectrum = JONSWAPSpectrum(params)
            ax2.plot(spectrum.frequencies, spectrum.compute_spectrum(),
                    label=f'Tp={Tp}s', linewidth=2)

        ax2.set_xlabel('Frequency [Hz]')
        ax2.set_ylabel('Spectral Density [m²·s]')
        ax2.set_title('Wave Spectra for Different Tp')
        ax2.legend()
        ax2.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "peak_period_effect.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_wave_energy_distribution(self, jonswap_spectrum, output_dir):
        """Test wave energy distribution across frequencies."""
        S = jonswap_spectrum.compute_spectrum()
        omega = jonswap_spectrum.omega
        frequencies = jonswap_spectrum.frequencies

        # Energy in each frequency band
        d_omega = np.diff(omega, prepend=omega[0] - (omega[1] - omega[0]))
        energy = S * d_omega

        # Total energy should equal m0
        total_energy = np.sum(energy)
        m0 = jonswap_spectrum.spectral_moment(0)
        np.testing.assert_allclose(total_energy, m0, rtol=0.01,
                                  err_msg="Total energy should equal m0")

        # Most energy should be in 0.05-0.2 Hz range for Tp=10s
        mask = (frequencies >= 0.05) & (frequencies <= 0.2)
        energy_in_range = np.sum(energy[mask])
        energy_fraction = energy_in_range / total_energy

        assert energy_fraction > 0.8, \
            f"Expected >80% energy in main range, got {energy_fraction*100:.1f}%"

        # Create energy distribution chart
        fig, ax = plt.subplots(figsize=(10, 6))

        ax.fill_between(frequencies, 0, S, alpha=0.3, label='Spectral Density')
        ax.plot(frequencies, S, linewidth=2)
        ax.axvspan(0.05, 0.2, alpha=0.2, color='green', label='Main Energy Range')

        ax.set_xlabel('Frequency [Hz]')
        ax.set_ylabel('Spectral Density [m²·s]')
        ax.set_title(f'Wave Energy Distribution (Hs={jonswap_spectrum.params.Hs}m, '
                    f'Tp={jonswap_spectrum.params.Tp}s)')
        ax.legend()
        ax.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig(output_dir / "energy_distribution.png", dpi=300, bbox_inches='tight')
        plt.close()

    def test_spectral_statistics_integration(self, jonswap_spectrum, output_dir):
        """Test complete spectral statistics calculation and visualization."""
        stats = jonswap_spectrum.get_spectral_statistics()

        # Validate all statistics
        assert stats['Hs'] > 0, "Hs must be positive"
        assert stats['Tz'] > 0, "Tz must be positive"
        assert stats['Tm'] > 0, "Tm must be positive"
        assert 0 <= stats['bandwidth'] <= 1, "Bandwidth must be in [0,1]"

        # Tz should be less than Tm
        assert stats['Tz'] < stats['Tm'], "Tz should be less than Tm"

        # Both should be less than Tp
        Tp = jonswap_spectrum.params.Tp
        assert stats['Tz'] < Tp, "Tz should be less than Tp"
        assert stats['Tm'] < Tp, "Tm should be less than Tp"

        # Create statistics summary chart
        fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(12, 10))

        # Spectrum
        S = jonswap_spectrum.compute_spectrum()
        frequencies = jonswap_spectrum.frequencies
        ax1.plot(frequencies, S, linewidth=2)
        ax1.set_xlabel('Frequency [Hz]')
        ax1.set_ylabel('S(f) [m²·s]')
        ax1.set_title('Wave Spectrum')
        ax1.grid(True, alpha=0.3)

        # Period comparison
        periods = ['Tp', 'Tz', 'Tm']
        period_values = [Tp, stats['Tz'], stats['Tm']]
        ax2.bar(periods, period_values, color=['blue', 'green', 'orange'])
        ax2.set_ylabel('Period [s]')
        ax2.set_title('Period Measures')
        ax2.grid(True, alpha=0.3, axis='y')

        # Moments
        moments = ['m0', 'm1', 'm2', 'm4']
        moment_values = [stats[m] for m in moments]
        ax3.bar(moments, moment_values, color='purple', alpha=0.7)
        ax3.set_ylabel('Moment Value')
        ax3.set_title('Spectral Moments')
        ax3.grid(True, alpha=0.3, axis='y')
        ax3.set_yscale('log')

        # Bandwidth visualization
        ax4.text(0.5, 0.5, f"Bandwidth: {stats['bandwidth']:.3f}",
                horizontalalignment='center', verticalalignment='center',
                fontsize=20, transform=ax4.transAxes)
        ax4.text(0.5, 0.3, f"Hs: {stats['Hs']:.2f} m",
                horizontalalignment='center', verticalalignment='center',
                fontsize=14, transform=ax4.transAxes)
        ax4.axis('off')
        ax4.set_title('Key Statistics')

        plt.tight_layout()
        plt.savefig(output_dir / "spectral_statistics.png", dpi=300, bbox_inches='tight')
        plt.close()
