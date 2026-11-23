"""Tests for benchmark_rao_plots module.

ABOUTME: Tests for plot_amplitude_overlay, plot_phase_overlay,
plot_combined_overlay, plot_difference, and plot_per_dof.
Verifies file creation, return types, HTML content, heading filters,
x-axis modes, and error handling.
"""
from __future__ import annotations

from pathlib import Path
from typing import Dict

import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_rao_plots import (
    plot_amplitude_overlay,
    plot_combined_overlay,
    plot_difference,
    plot_per_dof,
    plot_phase_overlay,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import DOF_ORDER
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)

# Re-use conftest helpers directly
from tests.hydrodynamics.diffraction.conftest import _make_solver_results


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _two_solvers() -> Dict[str, DiffractionResults]:
    return {
        "SolverA": _make_solver_results("SolverA", seed_offset=0),
        "SolverB": _make_solver_results("SolverB", seed_offset=0, magnitude_scale=1.02),
    }


def _three_solvers() -> Dict[str, DiffractionResults]:
    return {
        "AQWA": _make_solver_results("AQWA", seed_offset=0),
        "OrcaWave": _make_solver_results("OrcaWave", seed_offset=0, magnitude_scale=1.02),
        "BEMRosetta": _make_solver_results(
            "BEMRosetta", seed_offset=0, magnitude_scale=1.01, heave_bias=0.15,
        ),
    }


# ---------------------------------------------------------------------------
# 1. plot_amplitude_overlay
# ---------------------------------------------------------------------------


class TestPlotAmplitudeOverlay:
    """Tests for the amplitude overlay plot function."""

    def test_returns_path(self, tmp_path: Path):
        """Return value must be a Path object."""
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_amplitude_overlay(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert isinstance(path, Path)

    def test_creates_html_file(self, tmp_path: Path):
        """An HTML file must exist at the returned path."""
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_amplitude_overlay(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert path.exists()
        assert path.suffix == ".html"

    def test_html_contains_amplitude(self, tmp_path: Path):
        """HTML content should reference 'Amplitude'."""
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_amplitude_overlay(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        content = path.read_text(encoding="utf-8")
        assert "Amplitude" in content

    def test_period_x_axis(self, tmp_path: Path):
        """Period x-axis should produce a valid file."""
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_amplitude_overlay(
            results, names, tmp_path,
            x_axis="period", heading_x_axis=False,
        )
        assert path.exists()


# ---------------------------------------------------------------------------
# 2. plot_phase_overlay
# ---------------------------------------------------------------------------


class TestPlotPhaseOverlay:
    """Tests for the phase overlay plot function."""

    def test_returns_path(self, tmp_path: Path):
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_phase_overlay(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert isinstance(path, Path)

    def test_creates_html_file(self, tmp_path: Path):
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_phase_overlay(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert path.exists()
        assert path.suffix == ".html"

    def test_html_contains_phase(self, tmp_path: Path):
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_phase_overlay(
            results, names, tmp_path,
            x_axis="period", heading_x_axis=False,
        )
        content = path.read_text(encoding="utf-8")
        assert "Phase" in content


# ---------------------------------------------------------------------------
# 3. plot_combined_overlay
# ---------------------------------------------------------------------------


class TestPlotCombinedOverlay:
    """Tests for the combined (amplitude + phase) overlay plot."""

    def test_returns_path(self, tmp_path: Path):
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_combined_overlay(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert isinstance(path, Path)

    def test_creates_html_file(self, tmp_path: Path):
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_combined_overlay(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert path.exists()
        assert path.suffix == ".html"

    def test_with_heading_filter(self, tmp_path: Path):
        """Heading filter should still produce valid output."""
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_combined_overlay(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
            headings=[0.0, 90.0],
        )
        assert path.exists()

    def test_heading_x_axis_mode(self, tmp_path: Path):
        """heading_x_axis=True should work."""
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_combined_overlay(
            results, names, tmp_path,
            x_axis="period", heading_x_axis=True,
        )
        assert path.exists()


# ---------------------------------------------------------------------------
# 4. plot_difference
# ---------------------------------------------------------------------------


class TestPlotDifference:
    """Tests for the difference plot function."""

    def test_returns_path(self, tmp_path: Path):
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_difference(
            names[0], results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert isinstance(path, Path)

    def test_creates_html_file(self, tmp_path: Path):
        results = _two_solvers()
        names = sorted(results.keys())
        path = plot_difference(
            names[0], results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert path.exists()
        assert path.suffix == ".html"

    def test_invalid_reference_raises(self, tmp_path: Path):
        """Unknown reference solver must raise ValueError."""
        results = _two_solvers()
        names = sorted(results.keys())
        with pytest.raises(ValueError, match="not found"):
            plot_difference(
                "NonExistent", results, names, tmp_path,
                x_axis="frequency", heading_x_axis=False,
            )

    def test_three_solvers(self, tmp_path: Path):
        """Three solvers should produce a valid difference plot."""
        results = _three_solvers()
        names = sorted(results.keys())
        path = plot_difference(
            "AQWA", results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert path.exists()

    def test_html_contains_reference_solver_name(self, tmp_path: Path):
        results = _two_solvers()
        names = sorted(results.keys())
        ref = names[0]
        path = plot_difference(
            ref, results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        content = path.read_text(encoding="utf-8")
        assert ref in content


# ---------------------------------------------------------------------------
# 5. plot_per_dof
# ---------------------------------------------------------------------------


class TestPlotPerDof:
    """Tests for per-DOF individual plot generation."""

    def test_returns_dict(self, tmp_path: Path):
        """Return value must be a dict."""
        results = _two_solvers()
        names = sorted(results.keys())
        paths = plot_per_dof(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert isinstance(paths, dict)

    def test_six_keys(self, tmp_path: Path):
        """Returned dict must have exactly 6 entries (one per DOF)."""
        results = _two_solvers()
        names = sorted(results.keys())
        paths = plot_per_dof(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert len(paths) == 6

    def test_keys_are_lowercase_dof_names(self, tmp_path: Path):
        """Keys should be lowercase DOF names: surge, sway, ...."""
        results = _two_solvers()
        names = sorted(results.keys())
        paths = plot_per_dof(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        expected_keys = {dof.name.lower() for dof in DOF_ORDER}
        assert set(paths.keys()) == expected_keys

    def test_all_files_exist(self, tmp_path: Path):
        """All returned paths must point to existing files."""
        results = _two_solvers()
        names = sorted(results.keys())
        paths = plot_per_dof(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        for dof_name, path in paths.items():
            assert path.exists(), f"File missing for {dof_name}"
            assert path.suffix == ".html"

    def test_all_files_are_html(self, tmp_path: Path):
        """All generated files must contain HTML content."""
        results = _two_solvers()
        names = sorted(results.keys())
        paths = plot_per_dof(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        for dof_name, path in paths.items():
            content = path.read_text(encoding="utf-8")
            assert "plotly" in content.lower() or "div" in content.lower(), (
                f"No Plotly content in {dof_name}"
            )

    def test_period_x_axis(self, tmp_path: Path):
        """Period x-axis should produce valid per-DOF plots."""
        results = _two_solvers()
        names = sorted(results.keys())
        paths = plot_per_dof(
            results, names, tmp_path,
            x_axis="period", heading_x_axis=False,
        )
        assert len(paths) == 6

    def test_heading_filter(self, tmp_path: Path):
        """Heading filter should still produce 6 DOF plots."""
        results = _two_solvers()
        names = sorted(results.keys())
        paths = plot_per_dof(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
            headings=[0.0, 180.0],
        )
        assert len(paths) == 6
        for path in paths.values():
            assert path.exists()

    def test_heading_x_axis_mode(self, tmp_path: Path):
        """heading_x_axis=True should produce valid per-DOF plots."""
        results = _two_solvers()
        names = sorted(results.keys())
        paths = plot_per_dof(
            results, names, tmp_path,
            x_axis="period", heading_x_axis=True,
        )
        assert len(paths) == 6
        for path in paths.values():
            assert path.exists()

    def test_three_solvers_per_dof(self, tmp_path: Path):
        """Three solvers should produce valid per-DOF plots."""
        results = _three_solvers()
        names = sorted(results.keys())
        paths = plot_per_dof(
            results, names, tmp_path,
            x_axis="frequency", heading_x_axis=False,
        )
        assert len(paths) == 6
        for path in paths.values():
            assert path.exists()
