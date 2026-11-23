"""Time series module test fixtures."""
import pytest
import numpy as np

@pytest.fixture
def sample_time_series_data():
    """Generate sample time series data."""
    dt = 0.01  # 100 Hz sampling
    duration = 10  # seconds
    n_points = int(duration / dt)
    time = np.linspace(0, duration, n_points)
    
    # Multi-frequency signal
    signal = (
        10 * np.sin(2 * np.pi * 1 * time) +  # 1 Hz
        5 * np.sin(2 * np.pi * 5 * time) +   # 5 Hz
        2 * np.sin(2 * np.pi * 10 * time)    # 10 Hz
    )
    
    return time, signal

@pytest.fixture
def fft_parameters():
    """FFT analysis parameters."""
    return {
        'window': 'hanning',
        'overlap': 0.5,
        'nfft': 1024,
        'detrend': 'linear',
    }

@pytest.fixture
def window_parameters():
    """Window function parameters."""
    return {
        'window_size': 512,
        'window_type': 'hamming',
        'overlap_ratio': 0.5,
    }