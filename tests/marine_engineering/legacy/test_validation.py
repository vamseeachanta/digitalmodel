"""Quick validation test for wave spectra implementation."""
import pytest
import numpy as np
from digitalmodel.modules.marine_analysis.wave_spectra import (
    WaveSpectrumParameters,
    JONSWAPSpectrum,
    PiersonMoskowitzSpectrum
)


@pytest.fixture
def spectrum_params():
    """Wave spectrum parameters for testing."""
    return WaveSpectrumParameters(Hs=3.0, Tp=10.0, gamma=3.3)


def test_jonswap_spectrum_creation(spectrum_params):
    """Test JONSWAP spectrum can be created successfully."""
    spectrum = JONSWAPSpectrum(spectrum_params)
    assert spectrum is not None


def test_jonswap_spectrum_computation(spectrum_params):
    """Test JONSWAP spectrum computation."""
    spectrum = JONSWAPSpectrum(spectrum_params)
    S = spectrum.compute_spectrum()

    assert len(S) > 0, "Spectrum should have data points"
    assert np.max(S) > 0, "Spectrum should have positive values"


def test_jonswap_hs_recovery(spectrum_params):
    """Test JONSWAP Hs recovery within 2% tolerance."""
    spectrum = JONSWAPSpectrum(spectrum_params)
    S = spectrum.compute_spectrum()

    Hs = spectrum.significant_wave_height()
    error_pct = abs(Hs - 3.0) / 3.0 * 100

    assert error_pct < 2.0, (
        f'JONSWAP Hs recovery error exceeds 2%: '
        f'Hs={Hs:.3f}m (expected 3.0m), error={error_pct:.2f}%'
    )


def test_jonswap_zero_crossing_period(spectrum_params):
    """Test JONSWAP zero-crossing period calculation."""
    spectrum = JONSWAPSpectrum(spectrum_params)
    Tz = spectrum.zero_crossing_period()

    # For Tp=10s, expect Tz around 7.1s (typical ratio Tz/Tp ~ 0.71)
    assert 6.0 < Tz < 9.0, f'Zero-crossing period {Tz:.3f}s outside expected range'


def test_jonswap_spectral_bandwidth(spectrum_params):
    """Test JONSWAP spectral bandwidth calculation."""
    spectrum = JONSWAPSpectrum(spectrum_params)
    bw = spectrum.spectral_bandwidth()

    # Bandwidth should be positive and less than 1
    assert 0 < bw < 1, f'Spectral bandwidth {bw:.3f} outside valid range'


def test_pierson_moskowitz_spectrum_computation(spectrum_params):
    """Test Pierson-Moskowitz spectrum computation."""
    pm = PiersonMoskowitzSpectrum(spectrum_params)
    S_pm = pm.compute_spectrum()

    assert len(S_pm) > 0, "P-M spectrum should have data points"


def test_pierson_moskowitz_hs_recovery(spectrum_params):
    """Test Pierson-Moskowitz Hs recovery within 2% tolerance."""
    pm = PiersonMoskowitzSpectrum(spectrum_params)
    S_pm = pm.compute_spectrum()

    Hs_pm = pm.significant_wave_height()
    error_pm = abs(Hs_pm - 3.0) / 3.0 * 100

    assert error_pm < 2.0, (
        f'P-M Hs recovery error exceeds 2%: '
        f'Hs={Hs_pm:.3f}m (expected 3.0m), error={error_pm:.2f}%'
    )


def test_spectral_statistics(spectrum_params):
    """Test spectral statistics computation."""
    spectrum = JONSWAPSpectrum(spectrum_params)
    stats = spectrum.get_spectral_statistics()

    # Verify all required statistics are present
    assert 'Hs' in stats
    assert 'Tz' in stats
    assert 'Tm' in stats
    assert 'bandwidth' in stats

    # Verify reasonable values
    assert stats['Hs'] > 0
    assert stats['Tz'] > 0
    assert stats['Tm'] > 0
    assert 0 < stats['bandwidth'] < 1


def test_combined_spectrum_validation(spectrum_params):
    """Test combined JONSWAP and P-M spectrum validation."""
    # JONSWAP spectrum
    spectrum = JONSWAPSpectrum(spectrum_params)
    Hs_jonswap = spectrum.significant_wave_height()
    error_pct = abs(Hs_jonswap - 3.0) / 3.0 * 100

    # Pierson-Moskowitz spectrum
    pm = PiersonMoskowitzSpectrum(spectrum_params)
    Hs_pm = pm.significant_wave_height()
    error_pm = abs(Hs_pm - 3.0) / 3.0 * 100

    # Both should pass 2% tolerance
    assert error_pct < 2.0 and error_pm < 2.0, (
        f'Spectrum validation failed: '
        f'JONSWAP error={error_pct:.2f}%, P-M error={error_pm:.2f}% (target: <2%)'
    )
