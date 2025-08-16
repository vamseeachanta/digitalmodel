"""
Tests for spectral analysis module
"""

import pytest
import numpy as np
import pandas as pd
import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent))

from src.digitalmodel.modules.signal_analysis.core.spectral import SpectralAnalyzer
from tests.modules.signal_analysis.fixtures.test_signals import (
    simple_sine_wave, multi_sine_wave, window_test_signal
)


class TestSpectralAnalyzer:
    """Test suite for SpectralAnalyzer class"""
    
    def test_initialization(self):
        """Test SpectralAnalyzer initialization"""
        analyzer = SpectralAnalyzer()
        assert analyzer.sampling_rate is None
        assert analyzer.method == 'fft'
        assert analyzer.last_spectrum is None
        
        analyzer = SpectralAnalyzer(sampling_rate=100, method='welch')
        assert analyzer.sampling_rate == 100
        assert analyzer.method == 'welch'
    
    def test_compute_fft(self, simple_sine_wave):
        """Test FFT computation"""
        analyzer = SpectralAnalyzer(sampling_rate=simple_sine_wave['sampling_rate'])
        signal = simple_sine_wave['signal']
        
        spectrum = analyzer.compute_spectrum(signal)
        
        assert isinstance(spectrum, pd.DataFrame)
        assert 'frequency' in spectrum.columns
        assert 'magnitude' in spectrum.columns
        assert 'power' in spectrum.columns
        assert 'phase' in spectrum.columns
        
        # Check that peak is at expected frequency
        peak_idx = spectrum['power'].idxmax()
        peak_freq = spectrum.loc[peak_idx, 'frequency']
        expected_freq = simple_sine_wave['frequency']
        
        assert abs(peak_freq - expected_freq) < 1.0  # Within 1 Hz
    
    def test_compute_welch(self, simple_sine_wave):
        """Test Welch's method"""
        analyzer = SpectralAnalyzer(method='welch')
        signal = simple_sine_wave['signal']
        fs = simple_sine_wave['sampling_rate']
        
        spectrum = analyzer.compute_spectrum(signal, sampling_rate=fs)
        
        assert isinstance(spectrum, pd.DataFrame)
        assert 'frequency' in spectrum.columns
        assert 'power' in spectrum.columns
        assert len(spectrum) > 0
    
    def test_window_averaged_fft(self, window_test_signal):
        """Test window-averaged FFT"""
        analyzer = SpectralAnalyzer()
        signal = window_test_signal['signal']
        fs = window_test_signal['sampling_rate']
        
        spectrum = analyzer.window_averaged_fft(
            signal, 
            window_size=256,
            overlap=0.5,
            sampling_rate=fs
        )
        
        assert isinstance(spectrum, pd.DataFrame)
        assert 'frequency' in spectrum.columns
        assert 'power' in spectrum.columns
        assert len(spectrum) > 0
    
    def test_find_peaks(self, multi_sine_wave):
        """Test peak finding"""
        analyzer = SpectralAnalyzer(sampling_rate=multi_sine_wave['sampling_rate'])
        signal = multi_sine_wave['signal']
        expected_freqs = multi_sine_wave['frequencies']
        
        spectrum = analyzer.compute_spectrum(signal)
        peaks = analyzer.find_peaks(n_peaks=3)
        
        assert isinstance(peaks, pd.DataFrame)
        assert len(peaks) <= 3
        
        # Check that peaks are near expected frequencies
        peak_freqs = peaks['frequency'].values
        for expected_freq in expected_freqs:
            # Find closest peak
            closest_peak = peak_freqs[np.argmin(np.abs(peak_freqs - expected_freq))]
            assert abs(closest_peak - expected_freq) < 2.0  # Within 2 Hz
    
    def test_filter_spectrum(self, multi_sine_wave):
        """Test spectrum filtering"""
        analyzer = SpectralAnalyzer(sampling_rate=multi_sine_wave['sampling_rate'])
        signal = multi_sine_wave['signal']
        
        spectrum = analyzer.compute_spectrum(signal)
        
        # Test bandpass filter
        filtered = analyzer.filter_spectrum(
            spectrum,
            filter_type='bandpass',
            low_freq=8,
            high_freq=12
        )
        
        # Check that power outside band is zero
        low_freq_power = filtered[filtered['frequency'] < 8]['power'].sum()
        high_freq_power = filtered[filtered['frequency'] > 12]['power'].sum()
        
        assert low_freq_power == 0
        assert high_freq_power == 0
        
        # Check that power inside band is preserved
        band_power = filtered[(filtered['frequency'] >= 8) & 
                             (filtered['frequency'] <= 12)]['power'].sum()
        assert band_power > 0
    
    def test_compute_psd(self, simple_sine_wave):
        """Test PSD computation"""
        analyzer = SpectralAnalyzer()
        signal = simple_sine_wave['signal']
        fs = simple_sine_wave['sampling_rate']
        
        psd = analyzer.compute_psd(signal, sampling_rate=fs, method='welch')
        
        assert isinstance(psd, pd.DataFrame)
        assert 'power' in psd.columns or 'psd' in psd.columns
        assert len(psd) > 0
    
    def test_inverse_transform(self, simple_sine_wave):
        """Test inverse FFT"""
        analyzer = SpectralAnalyzer(sampling_rate=simple_sine_wave['sampling_rate'])
        signal = simple_sine_wave['signal']
        
        # Compute spectrum
        spectrum = analyzer.compute_spectrum(signal)
        
        # Inverse transform
        reconstructed = analyzer.inverse_transform(spectrum)
        
        assert isinstance(reconstructed, np.ndarray)
        assert len(reconstructed) == len(signal)
        
        # Check reconstruction error is small
        error = np.mean(np.abs(signal - reconstructed))
        assert error < 0.01
    
    def test_empty_signal(self):
        """Test handling of empty signal"""
        analyzer = SpectralAnalyzer(sampling_rate=100)
        
        with pytest.raises(Exception):
            analyzer.compute_spectrum([])
    
    def test_missing_sampling_rate(self):
        """Test error when sampling rate is missing"""
        analyzer = SpectralAnalyzer()
        signal = np.random.randn(100)
        
        with pytest.raises(ValueError, match="Sampling rate"):
            analyzer.compute_spectrum(signal)
    
    def test_filter_types(self, simple_sine_wave):
        """Test different filter types"""
        analyzer = SpectralAnalyzer(sampling_rate=simple_sine_wave['sampling_rate'])
        signal = simple_sine_wave['signal']
        spectrum = analyzer.compute_spectrum(signal)
        
        # Test lowpass
        lowpass = analyzer.filter_spectrum(spectrum, 'lowpass', high_freq=10)
        assert all(lowpass[lowpass['frequency'] > 10]['power'] == 0)
        
        # Test highpass
        highpass = analyzer.filter_spectrum(spectrum, 'highpass', low_freq=3)
        assert all(highpass[highpass['frequency'] < 3]['power'] == 0)
        
        # Test bandstop
        bandstop = analyzer.filter_spectrum(spectrum, 'bandstop', 
                                          low_freq=4, high_freq=6)
        band_power = bandstop[(bandstop['frequency'] >= 4) & 
                              (bandstop['frequency'] <= 6)]['power'].sum()
        assert band_power == 0
    
    def test_detrending(self):
        """Test detrending options"""
        analyzer = SpectralAnalyzer(sampling_rate=100)
        
        # Signal with linear trend
        t = np.linspace(0, 1, 100)
        signal = np.sin(2 * np.pi * 5 * t) + 2 * t  # Sine + linear trend
        
        # Without detrending
        spectrum_no_detrend = analyzer._compute_fft(signal, 100, detrend='none')
        
        # With detrending
        spectrum_detrend = analyzer._compute_fft(signal, 100, detrend='linear')
        
        # DC component should be smaller with detrending
        dc_no_detrend = spectrum_no_detrend[spectrum_no_detrend['frequency'] == 0]['power'].values[0]
        dc_detrend = spectrum_detrend[spectrum_detrend['frequency'] == 0]['power'].values[0]
        
        assert dc_detrend < dc_no_detrend


if __name__ == "__main__":
    pytest.main([__file__, "-v"])