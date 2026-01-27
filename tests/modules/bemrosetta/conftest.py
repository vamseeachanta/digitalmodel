"""Pytest configuration and fixtures for BEMRosetta tests."""

import pytest
import numpy as np
from pathlib import Path


# ============================================================================
# Fixture Directory
# ============================================================================

FIXTURES_DIR = Path(__file__).parent / "fixtures"


# ============================================================================
# File-Based Fixtures
# ============================================================================


@pytest.fixture
def sample_aqwa_file():
    """Path to sample AQWA .LIS file.

    Returns:
        Path: Absolute path to the sample AQWA output file containing
        frequencies, headings, added mass, damping, and RAO data.
    """
    return FIXTURES_DIR / "sample_aqwa.lis"


@pytest.fixture
def sample_qtf_file():
    """Path to sample QTF file.

    Returns:
        Path: Absolute path to the sample QTF file containing
        difference-frequency second-order wave force data.
    """
    return FIXTURES_DIR / "sample.qtf"


@pytest.fixture
def sample_gdf_file():
    """Path to sample GDF mesh file.

    Returns:
        Path: Absolute path to the sample WAMIT GDF format mesh file
        containing a simple box geometry.
    """
    return FIXTURES_DIR / "sample_box.gdf"


@pytest.fixture
def sample_dat_file():
    """Path to sample DAT mesh file.

    Returns:
        Path: Absolute path to the sample AQWA/NEMOH DAT format mesh file
        containing the same box geometry as the GDF file.
    """
    return FIXTURES_DIR / "sample_box.dat"


# ============================================================================
# In-Memory Fixtures
# ============================================================================


@pytest.fixture
def sample_frequencies():
    """Sample frequency array for testing."""
    return np.linspace(0.1, 2.0, 10)


@pytest.fixture
def sample_headings():
    """Sample headings array for testing."""
    return np.array([0.0, 45.0, 90.0, 135.0, 180.0])


@pytest.fixture
def sample_qtf_data(sample_frequencies, sample_headings):
    """Sample QTF data for testing."""
    n_f1 = len(sample_frequencies)
    n_f2 = len(sample_frequencies)
    n_head = len(sample_headings)

    # Generate sample real and imaginary parts
    np.random.seed(42)
    real_part = np.random.randn(n_f1, n_f2, n_head, 6) * 100
    imag_part = np.random.randn(n_f1, n_f2, n_head, 6) * 50

    return {
        "frequencies_1": sample_frequencies,
        "frequencies_2": sample_frequencies,
        "headings": sample_headings,
        "real_part": real_part,
        "imag_part": imag_part,
    }


@pytest.fixture
def sample_panel_mesh():
    """Sample panel mesh for testing (simple cube below waterline)."""
    # Simple 4-panel mesh (bottom of a box)
    vertices = np.array([
        [0.0, 0.0, 0.0],
        [1.0, 0.0, 0.0],
        [1.0, 1.0, 0.0],
        [0.0, 1.0, 0.0],
        [0.0, 0.0, -1.0],
        [1.0, 0.0, -1.0],
        [1.0, 1.0, -1.0],
        [0.0, 1.0, -1.0],
    ])

    # Quad panels (bottom, front, back, left, right - no top)
    panels = np.array([
        [4, 5, 6, 7],  # bottom
        [0, 1, 5, 4],  # front
        [2, 3, 7, 6],  # back
        [0, 4, 7, 3],  # left
        [1, 2, 6, 5],  # right
    ])

    return {"vertices": vertices, "panels": panels}


@pytest.fixture
def sample_triangle_mesh():
    """Sample triangular panel mesh for testing."""
    vertices = np.array([
        [0.0, 0.0, 0.0],
        [1.0, 0.0, 0.0],
        [0.5, 1.0, 0.0],
    ])

    # Single triangle (use -1 for 4th vertex)
    panels = np.array([
        [0, 1, 2, -1],
    ])

    return {"vertices": vertices, "panels": panels}
