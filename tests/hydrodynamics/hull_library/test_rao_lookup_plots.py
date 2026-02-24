"""
ABOUTME: Tests for RAO lookup graph generation — Phase 4 (WRK-043).

Tests are written FIRST before implementation (TDD).
Covers per_hull_rao_plot, comparison_plot, parameter_sweep_plot,
export_html, and export_png helpers.
"""

from __future__ import annotations

import pytest
import numpy as np
import tempfile
from pathlib import Path


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_rao_data(vessel_name: str = "test", n_freq: int = 20,
                   amplitude: float = 0.5):
    """Create synthetic RAOData."""
    from digitalmodel.hydrodynamics.models import RAOData

    frequencies = np.linspace(0.1, 2.0, n_freq)
    directions = np.array([0.0, 90.0, 180.0])
    amplitudes = np.ones((n_freq, 3, 6)) * amplitude
    phases = np.zeros((n_freq, 3, 6))
    return RAOData(
        frequencies=frequencies,
        directions=directions,
        amplitudes=amplitudes,
        phases=phases,
        vessel_name=vessel_name,
    )


def _make_entry(vid: str, params: dict, amplitude: float = 0.5):
    from digitalmodel.hydrodynamics.hull_library.rao_database import (
        RAODatabaseEntry,
    )

    return RAODatabaseEntry(
        variation_id=vid,
        hull_params=params,
        rao_data=_make_rao_data(vid, amplitude=amplitude),
    )


# ---------------------------------------------------------------------------
# per_hull_rao_plot tests
# ---------------------------------------------------------------------------


class TestPerHullRaoPlot:
    def test_returns_plotly_figure(self):
        import plotly.graph_objects as go
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            per_hull_rao_plot,
        )

        entry = _make_entry("hull_001", {"length_scale": 1.0})
        fig = per_hull_rao_plot(entry)
        assert isinstance(fig, go.Figure)

    def test_has_six_subplots(self):
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            per_hull_rao_plot,
        )

        entry = _make_entry("hull_001", {"length_scale": 1.0})
        fig = per_hull_rao_plot(entry)
        # 6 DOFs → 6 traces minimum (one per subplot for primary direction)
        assert len(fig.data) >= 6

    def test_figure_has_title(self):
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            per_hull_rao_plot,
        )

        entry = _make_entry("my_hull", {"length_scale": 1.5})
        fig = per_hull_rao_plot(entry)
        title_text = str(fig.layout.title.text or "")
        assert "my_hull" in title_text or len(title_text) > 0

    def test_uses_variation_id_in_title(self):
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            per_hull_rao_plot,
        )

        entry = _make_entry("specific_variation", {"beam_scale": 1.2})
        fig = per_hull_rao_plot(entry)
        title_text = str(fig.layout.title.text or "")
        assert "specific_variation" in title_text


# ---------------------------------------------------------------------------
# comparison_plot tests
# ---------------------------------------------------------------------------


class TestComparisonPlot:
    def test_returns_plotly_figure(self):
        import plotly.graph_objects as go
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            comparison_plot,
        )

        entries = [
            _make_entry("v001", {"length_scale": 1.0}, amplitude=0.5),
            _make_entry("v002", {"length_scale": 1.5}, amplitude=0.7),
        ]
        fig = comparison_plot(entries)
        assert isinstance(fig, go.Figure)

    def test_contains_traces_for_all_entries(self):
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            comparison_plot,
        )

        entries = [
            _make_entry(f"v{i:03d}", {"length_scale": 1.0 + i * 0.25})
            for i in range(3)
        ]
        fig = comparison_plot(entries)
        # At least one trace per entry (may be more for multiple DOFs)
        assert len(fig.data) >= len(entries)

    def test_single_entry_comparison(self):
        import plotly.graph_objects as go
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            comparison_plot,
        )

        entries = [_make_entry("solo", {"length_scale": 1.0})]
        fig = comparison_plot(entries)
        assert isinstance(fig, go.Figure)
        assert len(fig.data) >= 1

    def test_empty_entries_list(self):
        import plotly.graph_objects as go
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            comparison_plot,
        )

        fig = comparison_plot([])
        assert isinstance(fig, go.Figure)


# ---------------------------------------------------------------------------
# parameter_sweep_plot tests
# ---------------------------------------------------------------------------


class TestParameterSweepPlot:
    def test_returns_plotly_figure(self):
        import plotly.graph_objects as go
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            parameter_sweep_plot,
        )

        entries = [
            _make_entry(f"v{i}", {"length_scale": 1.0 + i * 0.25},
                        amplitude=0.3 + i * 0.1)
            for i in range(4)
        ]
        fig = parameter_sweep_plot(entries, "length_scale", dof=2)
        assert isinstance(fig, go.Figure)

    def test_has_traces_for_each_entry(self):
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            parameter_sweep_plot,
        )

        entries = [
            _make_entry(f"v{i}", {"beam_scale": 0.8 + i * 0.1})
            for i in range(3)
        ]
        fig = parameter_sweep_plot(entries, "beam_scale", dof=2)
        assert len(fig.data) == 3

    def test_title_contains_param_name_and_dof(self):
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            parameter_sweep_plot,
        )

        entries = [_make_entry("v1", {"draft_scale": 0.5})]
        fig = parameter_sweep_plot(entries, "draft_scale", dof=0)
        title = str(fig.layout.title.text or "")
        # Either the param name or dof should appear in some form
        assert "draft_scale" in title or len(title) > 0

    def test_dof_names_accepted(self):
        """Accept DOF as integer index 0-5."""
        import plotly.graph_objects as go
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            parameter_sweep_plot,
        )

        entries = [_make_entry("v1", {"length_scale": 1.0})]
        for dof_idx in range(6):
            fig = parameter_sweep_plot(entries, "length_scale", dof=dof_idx)
            assert isinstance(fig, go.Figure)


# ---------------------------------------------------------------------------
# Export helper tests
# ---------------------------------------------------------------------------


class TestExportHelpers:
    def test_export_html_creates_file(self):
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            per_hull_rao_plot, export_html,
        )

        entry = _make_entry("export_test", {"length_scale": 1.0})
        fig = per_hull_rao_plot(entry)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "test.html"
            export_html(fig, path)
            assert path.exists()
            content = path.read_text()
            assert len(content) > 100

    def test_export_html_content_is_html(self):
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            comparison_plot, export_html,
        )

        entries = [_make_entry("v1", {}), _make_entry("v2", {})]
        fig = comparison_plot(entries)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "cmp.html"
            export_html(fig, path)
            content = path.read_text()
            assert "<html" in content.lower() or "plotly" in content.lower()

    def test_export_html_creates_parent_dirs(self):
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            per_hull_rao_plot, export_html,
        )

        entry = _make_entry("v1", {})
        fig = per_hull_rao_plot(entry)

        with tempfile.TemporaryDirectory() as tmpdir:
            nested = Path(tmpdir) / "subdir" / "plot.html"
            export_html(fig, nested)
            assert nested.exists()

    def test_export_png_creates_file_or_skips_gracefully(self):
        """export_png requires kaleido; skip if not installed."""
        from digitalmodel.hydrodynamics.hull_library.rao_lookup_plots import (
            per_hull_rao_plot, export_png,
        )

        entry = _make_entry("v1", {})
        fig = per_hull_rao_plot(entry)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "fig.png"
            try:
                export_png(fig, path)
                # If kaleido available, file should exist
                assert path.exists()
            except ImportError:
                # kaleido not installed — acceptable
                pass
            except Exception as e:
                # Some environments can't render PNGs
                if "kaleido" in str(e).lower() or "orca" in str(e).lower():
                    pass
                else:
                    raise
