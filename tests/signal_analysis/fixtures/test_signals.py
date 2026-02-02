"""
Test fixtures for signal analysis

Provides common test signals and expected results.
"""

import numpy as np
import pytest


@pytest.fixture
def simple_sine_wave():
    """Generate a simple sine wave signal"""
    fs = 100  # Sampling frequency
    duration = 1.0  # seconds
    frequency = 5.0  # Hz
    
    t = np.linspace(0, duration, int(fs * duration), endpoint=False)
    signal = np.sin(2 * np.pi * frequency * t)
    
    return {
        'signal': signal,
        'time': t,
        'sampling_rate': fs,
        'frequency': frequency
    }


@pytest.fixture
def multi_sine_wave():
    """Generate a multi-frequency sine wave"""
    fs = 100  # Sampling frequency
    duration = 2.0  # seconds
    frequencies = [5.0, 10.0, 15.0]  # Hz
    amplitudes = [1.0, 0.5, 0.25]
    
    t = np.linspace(0, duration, int(fs * duration), endpoint=False)
    signal = np.zeros_like(t)
    
    for freq, amp in zip(frequencies, amplitudes):
        signal += amp * np.sin(2 * np.pi * freq * t)
    
    return {
        'signal': signal,
        'time': t,
        'sampling_rate': fs,
        'frequencies': frequencies,
        'amplitudes': amplitudes
    }


@pytest.fixture
def noisy_signal():
    """Generate a noisy signal with outliers"""
    fs = 100  # Sampling frequency
    duration = 1.0  # seconds
    frequency = 5.0  # Hz
    
    t = np.linspace(0, duration, int(fs * duration), endpoint=False)
    clean_signal = np.sin(2 * np.pi * frequency * t)
    
    # Add noise
    noise = 0.1 * np.random.randn(len(t))
    signal = clean_signal + noise
    
    # Add outliers
    outlier_indices = [10, 30, 50, 70, 90]
    for idx in outlier_indices:
        signal[idx] += np.random.choice([-3, 3])
    
    return {
        'signal': signal,
        'clean_signal': clean_signal,
        'time': t,
        'sampling_rate': fs,
        'frequency': frequency,
        'outlier_indices': outlier_indices
    }


@pytest.fixture
def rainflow_test_signal():
    """
    ASTM E1049-85 test signal for rainflow counting
    
    Expected cycles (from standard):
    - Range 8, Mean 0.5, Count 0.5
    - Range 6, Mean -0.5, Count 0.5
    - Range 4, Mean 1, Count 1
    - Range 3, Mean -0.5, Count 0.5
    """
    signal = np.array([-2, 1, -3, 5, -1, 3, -4, 4, -2])
    
    expected_cycles = [
        {'range': 8, 'mean': 0.5, 'count': 0.5},
        {'range': 6, 'mean': -0.5, 'count': 0.5},
        {'range': 4, 'mean': 1, 'count': 1},
        {'range': 3, 'mean': -0.5, 'count': 0.5},
    ]
    
    return {
        'signal': signal,
        'expected_cycles': expected_cycles
    }


@pytest.fixture
def fatigue_signal():
    """Generate a signal for fatigue analysis"""
    fs = 100  # Sampling frequency
    duration = 10.0  # seconds
    
    t = np.linspace(0, duration, int(fs * duration), endpoint=False)
    
    # Variable amplitude loading
    signal = (
        10 * np.sin(2 * np.pi * 0.5 * t) +  # Low frequency, high amplitude
        5 * np.sin(2 * np.pi * 2 * t) +     # Medium frequency, medium amplitude
        2 * np.sin(2 * np.pi * 10 * t)      # High frequency, low amplitude
    )
    
    # Add mean stress
    signal += 15
    
    return {
        'signal': signal,
        'time': t,
        'sampling_rate': fs,
        'mean_stress': 15
    }


@pytest.fixture
def window_test_signal():
    """Generate a long signal for window averaging tests"""
    fs = 1000  # Sampling frequency
    duration = 10.0  # seconds
    
    t = np.linspace(0, duration, int(fs * duration), endpoint=False)
    
    # Time-varying frequency
    frequency = 5 + 2 * np.sin(2 * np.pi * 0.1 * t)
    phase = 2 * np.pi * np.cumsum(frequency) / fs
    signal = np.sin(phase)
    
    return {
        'signal': signal,
        'time': t,
        'sampling_rate': fs
    }