"""RAO analysis module test fixtures."""
import pytest
import numpy as np

@pytest.fixture
def frequency_range():
    """Frequency range for RAO analysis."""
    return np.logspace(-1, 1, 50)  # 0.1 to 10 rad/s

@pytest.fixture
def wave_directions():
    """Wave directions for RAO analysis."""
    return np.arange(0, 360, 15)  # degrees

@pytest.fixture
def sample_rao_data():
    """Sample RAO data."""
    frequencies = np.array([0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0])
    surge_rao = np.array([10.0, 8.0, 5.0, 2.0, 0.5, 0.1, 0.01])
    heave_rao = np.array([1.0, 0.95, 0.8, 0.5, 0.2, 0.05, 0.01])
    pitch_rao = np.array([5.0, 4.5, 3.5, 2.0, 0.8, 0.2, 0.05])
    
    return {
        'frequencies': frequencies,
        'surge': surge_rao,
        'heave': heave_rao,
        'pitch': pitch_rao,
    }

@pytest.fixture
def wave_spectrum():
    """JONSWAP wave spectrum parameters."""
    return {
        'type': 'JONSWAP',
        'Hs': 3.0,  # m
        'Tp': 10.0,  # s
        'gamma': 2.5,
        'direction': 180,  # degrees
    }