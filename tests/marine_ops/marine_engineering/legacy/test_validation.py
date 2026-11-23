"""Quick validation test for wave spectra implementation."""
import pytest
import numpy as np
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra


@pytest.fixture
def ws():
    """WaveSpectra instance for testing."""
    return WaveSpectra()


def test_jonswap_spectrum_creation(ws):
    """Test JONSWAP spectrum can be created successfully."""
    omega, S = ws.jonswap(hs=3.0, tp=10.0, gamma=3.3)
    assert omega is not None
    assert S is not None


def test_jonswap_spectrum_computation(ws):
    """Test JONSWAP spectrum computation."""
    omega, S = ws.jonswap(hs=3.0, tp=10.0, gamma=3.3)

    assert len(S) > 0, "Spectrum should have data points"
    assert np.max(S) > 0, "Spectrum should have positive values"


def test_jonswap_hs_recovery(ws):
    """Test JONSWAP Hs recovery within 2% tolerance."""
    omega, S = ws.jonswap(hs=3.0, tp=10.0, gamma=3.3)

    Hs = ws.significant_height_from_spectrum(omega, S)
    error_pct = abs(Hs - 3.0) / 3.0 * 100

    assert error_pct < 2.0, (
        f'JONSWAP Hs recovery error exceeds 2%: '
        f'Hs={Hs:.3f}m (expected 3.0m), error={error_pct:.2f}%'
    )


def test_jonswap_zero_crossing_period(ws):
    """Test JONSWAP zero-crossing period calculation."""
    omega, S = ws.jonswap(hs=3.0, tp=10.0, gamma=3.3)
    Tz = ws.zero_crossing_period_from_spectrum(omega, S)

    # For Tp=10s, expect Tz around 7.1s (typical ratio Tz/Tp ~ 0.71)
    assert 5.0 < Tz < 10.0, f'Zero-crossing period {Tz:.3f}s outside expected range'


def test_jonswap_spectral_bandwidth(ws):
    """Test JONSWAP spectral bandwidth calculation."""
    omega, S = ws.jonswap(hs=3.0, tp=10.0, gamma=3.3)
    stats = ws.spectrum_statistics(omega, S)
    bw = stats['spectral_width']

    # Bandwidth should be positive and less than 1
    assert 0 < bw < 1, f'Spectral bandwidth {bw:.3f} outside valid range'


def test_pierson_moskowitz_spectrum_computation(ws):
    """Test Pierson-Moskowitz spectrum computation."""
    omega, S_pm = ws.pierson_moskowitz(hs=3.0, tp=10.0)

    assert len(S_pm) > 0, "P-M spectrum should have data points"


def test_pierson_moskowitz_hs_recovery(ws):
    """Test Pierson-Moskowitz Hs recovery within 2% tolerance."""
    omega, S_pm = ws.pierson_moskowitz(hs=3.0, tp=10.0)

    Hs_pm = ws.significant_height_from_spectrum(omega, S_pm)
    error_pm = abs(Hs_pm - 3.0) / 3.0 * 100

    assert error_pm < 2.0, (
        f'P-M Hs recovery error exceeds 2%: '
        f'Hs={Hs_pm:.3f}m (expected 3.0m), error={error_pm:.2f}%'
    )


def test_spectral_statistics(ws):
    """Test spectral statistics computation."""
    omega, S = ws.jonswap(hs=3.0, tp=10.0, gamma=3.3)
    stats = ws.spectrum_statistics(omega, S)

    # Verify all required statistics are present
    assert 'Hs_m' in stats
    assert 'Tz_s' in stats
    assert 'Tp_s' in stats
    assert 'spectral_width' in stats

    # Verify reasonable values
    assert stats['Hs_m'] > 0
    assert stats['Tz_s'] > 0
    assert stats['Tp_s'] > 0
    assert 0 < stats['spectral_width'] < 1


def test_combined_spectrum_validation(ws):
    """Test combined JONSWAP and P-M spectrum validation."""
    # JONSWAP spectrum
    omega_j, S_j = ws.jonswap(hs=3.0, tp=10.0, gamma=3.3)
    Hs_jonswap = ws.significant_height_from_spectrum(omega_j, S_j)
    error_pct = abs(Hs_jonswap - 3.0) / 3.0 * 100

    # Pierson-Moskowitz spectrum
    omega_pm, S_pm = ws.pierson_moskowitz(hs=3.0, tp=10.0)
    Hs_pm = ws.significant_height_from_spectrum(omega_pm, S_pm)
    error_pm = abs(Hs_pm - 3.0) / 3.0 * 100

    # Both should pass 2% tolerance
    assert error_pct < 2.0 and error_pm < 2.0, (
        f'Spectrum validation failed: '
        f'JONSWAP error={error_pct:.2f}%, P-M error={error_pm:.2f}% (target: <2%)'
    )
