"""Conftest for installation analysis tests -- self-contained fixtures.

Per D-03: do NOT modify root conftest. All fixtures local to this file.
"""
from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.models import (
    CraneCurve,
    CraneTipConfig,
    InstallationCase,
    InstallationCriteria,
    Structure,
    Vessel,
)


@pytest.fixture
def frequencies():
    """RAO frequencies [rad/s], 20 points from 0.1 to 2.0."""
    return np.linspace(0.1, 2.0, 20)


@pytest.fixture
def headings():
    """RAO headings [deg]."""
    return np.array([0.0, 45.0, 90.0, 135.0, 180.0])


def _make_rao(frequencies, headings, peak_amp, peak_freq=0.8):
    """Helper: synthetic Gaussian RAO peaked at peak_freq."""
    nf, nh = len(frequencies), len(headings)
    amp = peak_amp * np.exp(-2 * (frequencies - peak_freq) ** 2)
    amp_2d = np.tile(amp[:, None], (1, nh))
    phase_2d = np.zeros((nf, nh))
    return {"amplitude": amp_2d, "phase": phase_2d}


@pytest.fixture
def rao_data(frequencies, headings):
    """Synthetic 6-DOF RAO data dict."""
    return {
        "surge": _make_rao(frequencies, headings, 0.5),
        "sway": _make_rao(frequencies, headings, 0.3),
        "heave": _make_rao(frequencies, headings, 1.0),
        "roll": _make_rao(frequencies, headings, 3.0),  # deg/m
        "pitch": _make_rao(frequencies, headings, 2.0),  # deg/m
        "yaw": _make_rao(frequencies, headings, 0.5),  # deg/m
    }


@pytest.fixture
def crane_tip():
    """Crane tip at 30m fwd, 15m port, 25m above CoG."""
    return CraneTipConfig(x_m=30.0, y_m=15.0, z_m=25.0)


@pytest.fixture
def vessel(frequencies, headings, rao_data, crane_tip):
    """Test vessel with synthetic RAOs and crane configuration."""
    return Vessel(
        name="Test Vessel",
        rao_frequencies=frequencies,
        rao_headings=headings,
        rao_data=rao_data,
        crane_tip=crane_tip,
        displacement_te=5000.0,
    )


@pytest.fixture
def structure():
    """6m x 4m x 3m manifold, 25 tonnes."""
    return Structure(
        name="Test Manifold",
        length_m=6.0,
        width_m=4.0,
        height_m=3.0,
        mass_air_kg=25000.0,
        C_s=5.0,
    )


@pytest.fixture
def criteria():
    """Default installation criteria."""
    return InstallationCriteria(
        max_crane_tip_heave_m=2.0,
        max_crane_tip_velocity_m_s=0.5,
        max_hook_load_factor=1.3,
        max_tilt_deg=3.0,
    )


@pytest.fixture
def installation_case(structure, vessel, criteria):
    """Complete installation case: Hs=1.5m, Tp=8s, head sea."""
    return InstallationCase(
        structure=structure,
        vessel=vessel,
        criteria=criteria,
        wave_hs_m=1.5,
        wave_tp_s=8.0,
        wave_heading_deg=0.0,
    )


@pytest.fixture
def tp_range():
    """Peak period range for operability [s]."""
    return np.arange(5.0, 14.0, 1.0)


@pytest.fixture
def scatter_diagram():
    """Simple wave scatter diagram for operability tests."""
    hs = np.array([0.5, 1.0, 1.5, 2.0, 2.5, 3.0])
    tp = np.array([5.0, 7.0, 9.0, 11.0, 13.0])
    # Counts: most occurrences at low Hs
    counts = np.array([
        [50, 100, 80, 30, 10],   # Hs=0.5
        [30, 80, 100, 50, 20],   # Hs=1.0
        [10, 40, 60, 40, 15],    # Hs=1.5
        [5, 15, 25, 20, 10],     # Hs=2.0
        [2, 5, 10, 8, 5],        # Hs=2.5
        [1, 2, 3, 3, 2],         # Hs=3.0
    ])
    return hs, tp, counts
