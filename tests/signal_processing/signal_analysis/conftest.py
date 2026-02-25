"""Signal analysis module test fixtures."""
import pytest
import numpy as np
from pathlib import Path

@pytest.fixture
def signal_test_data_dir():
    """Return path to signal analysis test data directory."""
    return Path(__file__).parent / "test_data"

@pytest.fixture
def sample_time_series():
    """Generate sample time series data."""
    time = np.linspace(0, 10, 1000)
    signal = np.sin(2 * np.pi * 1 * time) + 0.5 * np.sin(2 * np.pi * 3 * time)
    return time, signal

@pytest.fixture
def sample_tension_data():
    """Generate sample tension time series."""
    time = np.linspace(0, 100, 10000)
    # Base tension with cyclic loading
    mean_tension = 1000  # kN
    amplitude = 200  # kN
    frequency = 0.1  # Hz
    tension = mean_tension + amplitude * np.sin(2 * np.pi * frequency * time)
    # Add some noise
    tension += np.random.normal(0, 10, len(time))
    return time, tension

@pytest.fixture
def rainflow_test_data():
    """Sample data for rainflow counting."""
    # Classic rainflow example sequence
    return np.array([0, 2, -1, 3, -2, 4, -3, 5, -4, 3, -2, 1, 0])

@pytest.fixture
def histogram_config():
    """Configuration for histogram analysis."""
    return {
        'bins': 20,
        'range': (0, 100),
        'density': True
    }