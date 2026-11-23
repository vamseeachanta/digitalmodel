"""Shared fixtures for OrcaWave reporting tests.

Provides mock OrcFxAPI.Diffraction objects with realistic attributes
so that section builders can be tested without the real solver.
"""
from __future__ import annotations

from unittest.mock import MagicMock, PropertyMock

import numpy as np
import pytest


@pytest.fixture
def mock_diff_single_body():
    """Mock Diffraction object for a single-body model (6 DOFs)."""
    diff = MagicMock()

    n_freqs = 5
    n_headings = 3
    n_dofs = 6

    # Frequencies in Hz, descending (typical OrcaWave ordering)
    freqs_hz = np.array([0.5, 0.4, 0.3, 0.2, 0.1])
    diff.frequencies = freqs_hz

    # Headings in degrees
    diff.headings = [0.0, 90.0, 180.0]

    # displacementRAOs: complex, shape (nheadings, nfreq, 6*nbodies)
    rng = np.random.RandomState(42)
    raos = rng.rand(n_headings, n_freqs, n_dofs) + 1j * rng.rand(
        n_headings, n_freqs, n_dofs
    )
    # Make heave quasi-static at the longest period (lowest freq -> last sorted)
    # For heading 180 deg (index 2), last sorted freq -> index 4 in descending
    raos[2, 4, 2] = 1.0 + 0.0j  # heave ~ 1 at longest period
    diff.displacementRAOs = raos

    # addedMass: shape (nfreq, 6, 6)
    am = rng.rand(n_freqs, n_dofs, n_dofs) * 1e6
    diff.addedMass = am

    # damping: shape (nfreq, 6, 6), diagonal non-negative
    dm = np.abs(rng.rand(n_freqs, n_dofs, n_dofs)) * 1e4
    diff.damping = dm

    # Water depth and density
    diff.WaterDepth = 100.0
    diff.WaterDensity = 1025.0

    return diff


@pytest.fixture
def mock_diff_two_body():
    """Mock Diffraction object for a two-body model (12 DOFs)."""
    diff = MagicMock()

    n_freqs = 4
    n_headings = 2
    n_dofs = 12  # 2 bodies * 6 DOFs

    freqs_hz = np.array([0.4, 0.3, 0.2, 0.1])
    diff.frequencies = freqs_hz
    diff.headings = [0.0, 180.0]

    rng = np.random.RandomState(99)
    raos = rng.rand(n_headings, n_freqs, n_dofs) + 1j * rng.rand(
        n_headings, n_freqs, n_dofs
    )
    raos[1, 3, 2] = 1.0 + 0.0j  # heave quasi-static
    diff.displacementRAOs = raos

    am = rng.rand(n_freqs, n_dofs, n_dofs) * 1e6
    diff.addedMass = am

    dm = np.abs(rng.rand(n_freqs, n_dofs, n_dofs)) * 1e4
    diff.damping = dm

    diff.WaterDepth = 200.0
    diff.WaterDensity = 1025.0

    return diff


@pytest.fixture
def mock_panel_geometry():
    """Return a list of panel geometry dicts suitable for diff.panelGeometry."""
    return [
        {"area": 2.5, "objectName": "Hull"},
        {"area": 3.1, "objectName": "Hull"},
        {"area": 1.8, "objectName": "Skirt"},
        {"area": 4.0, "objectName": "Hull"},
    ]


@pytest.fixture
def mock_diff_with_qtf():
    """Mock Diffraction with QTF results."""
    diff = MagicMock()

    n_freqs = 4
    n_headings = 2

    freqs_hz = np.array([0.4, 0.3, 0.2, 0.1])
    diff.frequencies = freqs_hz
    diff.headings = [0.0, 180.0]

    # QTF angular frequencies (rad/s)
    qtf_omegas = np.array([0.5, 1.0, 1.5, 2.0])
    diff.QTFAngularFrequencies = qtf_omegas

    # QTF results: 4D shape (nheadings, n_qtf_freqs, n_qtf_freqs, 6)
    rng = np.random.RandomState(77)
    qtf = rng.rand(n_headings, 4, 4, 6) + 1j * rng.rand(n_headings, 4, 4, 6)
    diff.QTFResults = qtf

    return diff
