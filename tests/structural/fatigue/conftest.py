"""
Conftest for scatter fatigue tests -- self-contained fixtures.

Per D-03: do NOT modify root conftest. All fixtures local to this file.
"""

import numpy as np
import pytest

from digitalmodel.structural.fatigue.sn_curves import PowerLawSNCurve


@pytest.fixture
def simple_sn_curve():
    """Simple power-law SN curve: N = 1e12 * S^(-3)."""
    return PowerLawSNCurve(name="TestSN", A=1e12, m=3.0, fatigue_limit=0.0)


@pytest.fixture
def frequencies():
    """Frequency array in Hz (0.01 to 0.5 Hz, 200 points)."""
    return np.linspace(0.01, 0.5, 200)


@pytest.fixture
def flat_transfer_function():
    """Constant stress transfer function |H(f)| = 1.0 (returns stress = wave elevation)."""
    return lambda f: np.ones_like(f, dtype=float)
