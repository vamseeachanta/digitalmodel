"""Fatigue analysis module test fixtures."""
import pytest
import numpy as np

@pytest.fixture
def sn_curve_data():
    """Sample S-N curve data."""
    return {
        'log_a': 12.164,
        'm': 3.0,
        'fatigue_limit': 52.0,  # MPa
        'curve_type': 'DNV-C'
    }

@pytest.fixture
def stress_history():
    """Generate sample stress history for fatigue analysis."""
    time = np.linspace(0, 3600, 36000)  # 1 hour at 10 Hz
    # Sinusoidal stress with varying amplitude
    mean_stress = 100  # MPa
    amplitude = 50 * (1 + 0.5 * np.sin(2 * np.pi * 0.01 * time))
    stress = mean_stress + amplitude * np.sin(2 * np.pi * 1 * time)
    return time, stress

@pytest.fixture
def load_spectrum():
    """Sample load spectrum for fatigue analysis."""
    return {
        'ranges': [50, 100, 150, 200, 250],
        'counts': [1000000, 100000, 10000, 1000, 100]
    }