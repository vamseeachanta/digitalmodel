# ABOUTME: TDD tests for refactored reservoir/stratigraphic.py — #1633.
# ABOUTME: Tests verify function signatures, return types, error handling,
# ABOUTME: and importability using mock DataFrames (no real well data needed).
"""
Tests for digitalmodel.reservoir.stratigraphic module.

After refactoring, stratigraphic.py exposes importable functions:
  - create_cross_section(wells_list, df_logs, statdata, ...)
  - plot_gr_track(ax, df, ...)
  - plot_rt_track(ax, df, ...)
  - plot_rhob_nphi_track(ax, df, ...)
  - plot_facies_track(ax, df, ...)

Tests use mock DataFrames and mock matplotlib — no real well data needed.
"""
from __future__ import annotations

import pytest
import numpy as np
import pandas as pd
from unittest.mock import MagicMock, patch


@pytest.fixture(autouse=True)
def mock_matplotlib(monkeypatch):
    """Mock matplotlib to prevent import errors and actual rendering.

    Injects mock modules into sys.modules so that
    `import matplotlib.pyplot as plt` etc. work even without
    matplotlib installed.
    """
    import sys

    mock_mpl = MagicMock()
    mock_pyplot = MagicMock()
    mock_colors = MagicMock()

    # Make ListedColormap return a working mock
    mock_colors.ListedColormap.return_value = MagicMock()

    mock_mpl.pyplot = mock_pyplot
    mock_mpl.colors = mock_colors

    # Store originals and inject mocks
    monkeypatch.setitem(sys.modules, "matplotlib", mock_mpl)
    monkeypatch.setitem(sys.modules, "matplotlib.pyplot", mock_pyplot)
    monkeypatch.setitem(sys.modules, "matplotlib.colors", mock_colors)

    # Also invalidate any cached import of stratigraphic so it re-imports with mocks
    for key in list(sys.modules.keys()):
        if "stratigraphic" in key:
            monkeypatch.delitem(sys.modules, key, raising=False)

    return mock_pyplot


@pytest.fixture
def sample_df_logs():
    """Create a mock well log DataFrame with required columns."""
    np.random.seed(42)
    n = 50
    wells = ["WELL_A"] * n + ["WELL_B"] * n
    return pd.DataFrame({
        "UWI": wells,
        "Depth": list(np.linspace(1000, 1200, n)) * 2,
        "GR": np.random.uniform(10, 80, n * 2),
        "RT": np.random.uniform(1, 500, n * 2),
        "RHOB": np.random.uniform(2.0, 2.7, n * 2),
        "PHIN": np.random.uniform(0.05, 0.35, n * 2),
        "KMeans": np.random.choice([0, 1, 2, 3, 4], n * 2),
    })


@pytest.fixture
def sample_statdata():
    """Create a mock tops/formation data DataFrame."""
    return pd.DataFrame({
        "UWI": ["WELL_A", "WELL_B"],
        "TOP": [1020.0, 1030.0],
        "BASE": [1150.0, 1160.0],
    })


@pytest.fixture
def sample_wells_list():
    return ["WELL_A", "WELL_B"]


# ---------------------------------------------------------------------------
# 1. Module importability (the key acceptance criterion)
# ---------------------------------------------------------------------------

class TestModuleImport:
    """Verify stratigraphic.py is importable after refactoring."""

    def test_import_module(self):
        """Importing stratigraphic should not raise NameError."""
        from digitalmodel.reservoir.stratigraphic import create_cross_section
        assert callable(create_cross_section)

    def test_import_all_public_functions(self):
        """All expected public functions should be importable."""
        from digitalmodel.reservoir import stratigraphic
        assert hasattr(stratigraphic, "create_cross_section")
        assert hasattr(stratigraphic, "plot_gr_track")
        assert hasattr(stratigraphic, "plot_rt_track")
        assert hasattr(stratigraphic, "plot_rhob_nphi_track")
        assert hasattr(stratigraphic, "plot_facies_track")


# ---------------------------------------------------------------------------
# 2. Function signatures and return types
# ---------------------------------------------------------------------------

class TestFunctionSignatures:
    """Verify functions accept the expected arguments."""

    def test_create_cross_section_returns_figure(
        self, sample_wells_list, sample_df_logs, sample_statdata, mock_matplotlib
    ):
        """create_cross_section should return a matplotlib figure."""
        from digitalmodel.reservoir import stratigraphic

        mock_fig = MagicMock()
        n_wells = len(sample_wells_list)
        mock_axes = [MagicMock() for _ in range(4 * n_wells)]

        # Configure the mock pyplot that stratigraphic imported
        stratigraphic.plt.subplots.return_value = (mock_fig, mock_axes)
        stratigraphic.plt.Normalize.return_value = MagicMock()
        stratigraphic.plt.cm.rainbow.return_value = np.zeros((50, 4))

        fig = stratigraphic.create_cross_section(
            sample_wells_list, sample_df_logs, sample_statdata
        )
        assert fig is not None

    def test_plot_gr_track_accepts_ax_and_df(
        self, sample_df_logs
    ):
        """plot_gr_track should accept an axes object and a DataFrame."""
        from digitalmodel.reservoir.stratigraphic import plot_gr_track

        mock_ax = MagicMock()
        df_well = sample_df_logs[sample_df_logs["UWI"] == "WELL_A"].copy()
        df_well["Adj_Depth"] = df_well["Depth"] - 1020.0

        # Should not raise
        plot_gr_track(mock_ax, df_well)
        mock_ax.plot.assert_called()

    def test_plot_rt_track_accepts_ax_and_df(
        self, sample_df_logs, mock_matplotlib
    ):
        """plot_rt_track should accept an axes object and a DataFrame."""
        from digitalmodel.reservoir import stratigraphic

        mock_ax = MagicMock()
        df_well = sample_df_logs[sample_df_logs["UWI"] == "WELL_A"].copy()
        df_well["Adj_Depth"] = df_well["Depth"] - 1020.0

        # Configure mocks
        stratigraphic.plt.Normalize.return_value = MagicMock()
        stratigraphic.plt.cm.rainbow.return_value = np.zeros((50, 4))

        stratigraphic.plot_rt_track(mock_ax, df_well)
        mock_ax.plot.assert_called()


# ---------------------------------------------------------------------------
# 3. Error handling for bad inputs
# ---------------------------------------------------------------------------

class TestErrorHandling:
    """Verify functions handle bad inputs gracefully."""

    def test_create_cross_section_empty_wells_list(
        self, sample_df_logs, sample_statdata
    ):
        """Empty wells list should raise ValueError."""
        from digitalmodel.reservoir.stratigraphic import create_cross_section

        with pytest.raises(ValueError, match="wells_list"):
            create_cross_section([], sample_df_logs, sample_statdata)

    def test_create_cross_section_missing_columns(
        self, sample_wells_list, sample_statdata
    ):
        """DataFrame missing required columns should raise ValueError."""
        from digitalmodel.reservoir.stratigraphic import create_cross_section

        bad_df = pd.DataFrame({"UWI": ["WELL_A"], "Depth": [1000]})
        with pytest.raises(ValueError, match="column"):
            create_cross_section(sample_wells_list, bad_df, sample_statdata)

    def test_plot_facies_track_accepts_ax_and_df(
        self, sample_df_logs
    ):
        """plot_facies_track should accept an axes object and a DataFrame."""
        from digitalmodel.reservoir.stratigraphic import plot_facies_track

        mock_ax = MagicMock()
        df_well = sample_df_logs[sample_df_logs["UWI"] == "WELL_A"].copy()
        df_well["Adj_Depth"] = df_well["Depth"] - 1020.0

        plot_facies_track(mock_ax, df_well)
        mock_ax.imshow.assert_called()
