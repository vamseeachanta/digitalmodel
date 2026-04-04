"""
Tests for parametric_hull_analysis.charts module.

Covers all public plot functions, save_figure, and private helpers.
Uses Agg backend for headless operation.
"""

from __future__ import annotations

import matplotlib
matplotlib.use("Agg")  # Must be before any other matplotlib imports

import os
from dataclasses import dataclass, field
from pathlib import Path
from types import SimpleNamespace

import matplotlib.pyplot as plt
import numpy as np
import pytest
from matplotlib.figure import Figure

from digitalmodel.hydrodynamics.parametric_hull_analysis.models import (
    DepthClassification,
    PassingShipSweepEntry,
    SweepResultEntry,
    classify_depth,
)
from digitalmodel.hydrodynamics.parametric_hull_analysis.charts import (
    _force_unit,
    _short_label,
    operability_chart,
    passing_ship_contour,
    rao_comparison_grid,
    parameter_sensitivity_plot,
    save_figure,
)


# ---------------------------------------------------------------------------
# Helpers — minimal mock data structures
# ---------------------------------------------------------------------------


def _make_rao_result(
    n_periods: int = 10,
    n_headings: int = 3,
    dof_names: list[str] | None = None,
    scale: float = 1.0,
) -> SimpleNamespace:
    """Create a minimal object mimicking a RAO result."""
    if dof_names is None:
        dof_names = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    n_dofs = len(dof_names)
    periods = np.linspace(3.0, 25.0, n_periods)
    # Random but reproducible
    rng = np.random.default_rng(42)
    rao_amplitude = rng.random((n_periods, n_headings, n_dofs)) * scale
    return SimpleNamespace(
        periods=periods,
        dof_names=dof_names,
        rao_amplitude=rao_amplitude,
    )


def _make_sweep_result_entry(
    variation_id: str = "base__beam=32.0",
    param_val: float = 32.0,
    rao_scale: float = 1.0,
) -> SweepResultEntry:
    """Create a minimal SweepResultEntry with synthetic RAO data."""
    return SweepResultEntry(
        variation_id=variation_id,
        hull_params={"beam": param_val},
        length_bp=200.0,
        beam=param_val,
        draft=12.0,
        block_coefficient=0.82,
        bem_result=None,
        rao_result=_make_rao_result(scale=rao_scale),
        depth_class=DepthClassification.DEEP,
    )


def _make_passing_ship_entries(
    n_sep: int = 4,
    n_speed: int = 4,
    variation_id: str = "base__beam=32.0",
    water_depth: float = 15.0,
) -> list[PassingShipSweepEntry]:
    """Generate synthetic PassingShipSweepEntry list for chart tests."""
    seps = np.linspace(30, 120, n_sep)
    speeds = np.linspace(1, 6, n_speed)
    entries = []
    for sep in seps:
        for speed in speeds:
            entries.append(
                PassingShipSweepEntry(
                    variation_id=variation_id,
                    hull_params={"beam": 32.0},
                    separation_m=float(sep),
                    speed_ms=float(speed),
                    water_depth_m=water_depth,
                    peak_surge_N=1000 * speed**2 / sep,
                    peak_sway_N=2000 * speed**2 / sep,
                    peak_yaw_Nm=5000 * speed**2 / sep,
                    depth_class=classify_depth(water_depth, 12.0),
                )
            )
    return entries


# ===================================================================
# Tests for private helpers
# ===================================================================


class TestShortLabel:
    """Tests for the _short_label helper."""

    def test_with_double_underscore(self):
        assert _short_label("hull_v1__beam=32.0_draft=12.0") == "beam=32.0_draft=12.0"

    def test_without_double_underscore(self):
        assert _short_label("simple_name") == "simple_name"

    def test_truncation(self):
        long_suffix = "a" * 50
        result = _short_label(f"prefix__{long_suffix}")
        assert len(result) <= 30


class TestForceUnit:
    """Tests for _force_unit helper."""

    def test_yaw(self):
        assert _force_unit("yaw") == "N·m"

    def test_sway(self):
        assert _force_unit("sway") == "N"

    def test_surge(self):
        assert _force_unit("surge") == "N"


# ===================================================================
# Tests for rao_comparison_grid
# ===================================================================


class TestRaoComparisonGrid:
    """Tests for rao_comparison_grid."""

    def test_returns_figure(self):
        results = [_make_sweep_result_entry(f"hull__beam={b}", param_val=b)
                    for b in [30.0, 32.0, 34.0]]
        fig = rao_comparison_grid(results)
        assert isinstance(fig, Figure)
        plt.close(fig)

    def test_correct_number_of_subplots(self):
        results = [_make_sweep_result_entry()]
        fig = rao_comparison_grid(results, dofs=("Heave", "Roll", "Pitch"))
        axes = fig.get_axes()
        assert len(axes) == 3
        plt.close(fig)

    def test_axes_labels(self):
        results = [_make_sweep_result_entry()]
        fig = rao_comparison_grid(results, dofs=("Heave",))
        ax = fig.get_axes()[0]
        assert "period" in ax.get_xlabel().lower()
        assert "rao" in ax.get_ylabel().lower()
        plt.close(fig)

    def test_axes_titles_match_dofs(self):
        dofs = ("Heave", "Roll")
        results = [_make_sweep_result_entry()]
        fig = rao_comparison_grid(results, dofs=dofs)
        axes = fig.get_axes()
        for i, dof in enumerate(dofs):
            assert axes[i].get_title() == dof
        plt.close(fig)

    def test_legend_with_few_variants(self):
        """Legends should be shown when <= 10 variants."""
        results = [_make_sweep_result_entry(f"hull__beam={b}", b)
                    for b in [30.0, 32.0]]
        fig = rao_comparison_grid(results, dofs=("Heave",))
        ax = fig.get_axes()[0]
        legend = ax.get_legend()
        assert legend is not None
        plt.close(fig)

    def test_no_rao_data_gracefully_handled(self):
        """Entry with rao_result=None should not crash."""
        entry = SweepResultEntry(
            variation_id="null_rao",
            hull_params={},
            length_bp=200.0,
            beam=32.0,
            draft=12.0,
            block_coefficient=0.82,
            bem_result=None,
            rao_result=None,
            depth_class=DepthClassification.DEEP,
        )
        fig = rao_comparison_grid([entry])
        assert isinstance(fig, Figure)
        plt.close(fig)

    def test_empty_results(self):
        fig = rao_comparison_grid([])
        assert isinstance(fig, Figure)
        plt.close(fig)

    def test_custom_figsize(self):
        results = [_make_sweep_result_entry()]
        fig = rao_comparison_grid(results, figsize=(20, 8), dofs=("Heave",))
        w, h = fig.get_size_inches()
        assert w == pytest.approx(20.0, abs=0.5)
        assert h == pytest.approx(8.0, abs=0.5)
        plt.close(fig)


# ===================================================================
# Tests for parameter_sensitivity_plot
# ===================================================================


class TestParameterSensitivityPlot:
    """Tests for parameter_sensitivity_plot."""

    def test_returns_figure(self):
        results = [_make_sweep_result_entry(f"hull__beam={b}", b)
                    for b in [28.0, 30.0, 32.0, 34.0]]
        fig = parameter_sensitivity_plot(results, param_name="beam", dof="Heave")
        assert isinstance(fig, Figure)
        plt.close(fig)

    def test_xlabel_contains_param_name(self):
        results = [_make_sweep_result_entry(f"hull__beam={b}", b)
                    for b in [30.0, 32.0, 34.0]]
        fig = parameter_sensitivity_plot(results, param_name="beam")
        ax = fig.get_axes()[0]
        assert "beam" in ax.get_xlabel()
        plt.close(fig)

    def test_title_mentions_dof(self):
        results = [_make_sweep_result_entry(f"hull__beam={b}", b)
                    for b in [30.0, 32.0]]
        fig = parameter_sensitivity_plot(results, param_name="beam", dof="Roll")
        ax = fig.get_axes()[0]
        assert "Roll" in ax.get_title()
        plt.close(fig)

    def test_scatter_points_present(self):
        results = [_make_sweep_result_entry(f"hull__beam={b}", b)
                    for b in [30.0, 32.0, 34.0]]
        fig = parameter_sensitivity_plot(results, param_name="beam")
        ax = fig.get_axes()[0]
        # Check that scatter collections exist
        assert len(ax.collections) > 0 or len(ax.lines) > 0
        plt.close(fig)


# ===================================================================
# Tests for passing_ship_contour
# ===================================================================


class TestPassingShipContour:
    """Tests for passing_ship_contour."""

    def test_returns_figure(self):
        entries = _make_passing_ship_entries()
        fig = passing_ship_contour(entries, "base__beam=32.0")
        assert isinstance(fig, Figure)
        plt.close(fig)

    def test_axes_labels(self):
        entries = _make_passing_ship_entries()
        fig = passing_ship_contour(entries, "base__beam=32.0")
        ax = fig.get_axes()[0]
        assert "speed" in ax.get_xlabel().lower()
        assert "separation" in ax.get_ylabel().lower()
        plt.close(fig)

    def test_different_force_components(self):
        entries = _make_passing_ship_entries()
        for component in ("surge", "sway", "yaw"):
            fig = passing_ship_contour(
                entries, "base__beam=32.0", force_component=component
            )
            assert isinstance(fig, Figure)
            assert component.title() in fig.get_axes()[0].get_title()
            plt.close(fig)

    def test_nonexistent_variant_returns_figure(self):
        """Missing variant should still return a Figure (with 'No data' message)."""
        entries = _make_passing_ship_entries()
        fig = passing_ship_contour(entries, "nonexistent__x=0")
        assert isinstance(fig, Figure)
        plt.close(fig)

    def test_water_depth_filter(self):
        entries = _make_passing_ship_entries(water_depth=15.0)
        # Filter for a depth that doesn't match → empty
        fig = passing_ship_contour(
            entries, "base__beam=32.0", water_depth_m=999.0
        )
        assert isinstance(fig, Figure)
        plt.close(fig)

    def test_contour_title_contains_variant(self):
        entries = _make_passing_ship_entries()
        fig = passing_ship_contour(entries, "base__beam=32.0")
        ax = fig.get_axes()[0]
        assert "beam=32.0" in ax.get_title()
        plt.close(fig)


# ===================================================================
# Tests for operability_chart
# ===================================================================


class TestOperabilityChart:
    """Tests for operability_chart."""

    def test_returns_figure(self):
        entries = _make_passing_ship_entries()
        fig = operability_chart(entries, threshold_sway_N=500, threshold_yaw_Nm=2000)
        assert isinstance(fig, Figure)
        plt.close(fig)

    def test_suptitle_contains_thresholds(self):
        entries = _make_passing_ship_entries()
        fig = operability_chart(entries, threshold_sway_N=5000, threshold_yaw_Nm=10000)
        suptitle = fig._suptitle.get_text() if fig._suptitle else ""
        assert "5" in suptitle  # 5000 / 1e3 = 5
        assert "10" in suptitle  # 10000 / 1e3 = 10
        plt.close(fig)

    def test_multiple_variants(self):
        entries_a = _make_passing_ship_entries(variation_id="hull_a__beam=30")
        entries_b = _make_passing_ship_entries(variation_id="hull_b__beam=34")
        all_entries = entries_a + entries_b
        fig = operability_chart(all_entries, threshold_sway_N=500, threshold_yaw_Nm=2000)
        assert isinstance(fig, Figure)
        # Should have at least 2 visible axes for 2 variants
        visible_axes = [a for a in fig.get_axes() if a.get_visible()]
        assert len(visible_axes) >= 2
        plt.close(fig)


# ===================================================================
# Tests for save_figure
# ===================================================================


class TestSaveFigure:
    """Tests for save_figure to tmp_path."""

    def test_save_png(self, tmp_path):
        fig, ax = plt.subplots()
        ax.plot([0, 1], [0, 1])
        path = tmp_path / "test_chart.png"
        save_figure(fig, path)
        assert path.exists()
        assert path.stat().st_size > 0

    def test_save_pdf(self, tmp_path):
        fig, ax = plt.subplots()
        ax.plot([0, 1], [0, 1])
        path = tmp_path / "test_chart.pdf"
        save_figure(fig, path)
        assert path.exists()
        assert path.stat().st_size > 0

    def test_creates_parent_directories(self, tmp_path):
        fig, ax = plt.subplots()
        ax.plot([0, 1], [0, 1])
        path = tmp_path / "subdir" / "deep" / "chart.png"
        save_figure(fig, path)
        assert path.exists()

    def test_custom_dpi(self, tmp_path):
        fig, ax = plt.subplots(figsize=(2, 2))
        ax.plot([0, 1], [0, 1])
        path_low = tmp_path / "low_dpi.png"
        path_high = tmp_path / "high_dpi.png"
        save_figure(fig, path_low, dpi=50)
        fig2, ax2 = plt.subplots(figsize=(2, 2))
        ax2.plot([0, 1], [0, 1])
        save_figure(fig2, path_high, dpi=300)
        # Higher DPI should produce a larger file
        assert path_high.stat().st_size > path_low.stat().st_size

    def test_figure_closed_after_save(self, tmp_path):
        fig, ax = plt.subplots()
        ax.plot([0, 1], [0, 1])
        fig_num = fig.number
        path = tmp_path / "closed.png"
        save_figure(fig, path)
        # After save_figure, the figure should be closed
        assert fig_num not in plt.get_fignums()
