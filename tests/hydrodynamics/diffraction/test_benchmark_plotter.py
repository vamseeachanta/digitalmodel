"""Tests for BenchmarkPlotter (multi-solver overlay and difference plots)."""
from __future__ import annotations

from pathlib import Path
from typing import Dict

import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_plotter import (
    BenchmarkPlotter,
)
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    BenchmarkReport,
    MultiSolverComparator,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)


# ---------------------------------------------------------------------------
# 1. Initialization
# ---------------------------------------------------------------------------


class TestInitialization:
    """Validate constructor defaults and side-effects."""

    def test_init_creates_output_dir(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        out = tmp_path / "plots" / "nested"
        assert not out.exists()

        # Act
        BenchmarkPlotter(three_solver_results, output_dir=out)

        # Assert
        assert out.exists()
        assert out.is_dir()

    def test_init_default_x_axis_period(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Act
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Assert
        assert plotter.x_axis == "period"


# ---------------------------------------------------------------------------
# 2. Amplitude overlay
# ---------------------------------------------------------------------------


class TestAmplitudeOverlay:
    """Verify amplitude overlay plot generation."""

    def test_amplitude_overlay_creates_html(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act
        path = plotter.plot_amplitude_overlay()

        # Assert
        assert path.exists()
        assert path.is_file()
        assert path.suffix == ".html"

    def test_amplitude_overlay_contains_all_solver_names(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act
        path = plotter.plot_amplitude_overlay()
        content = path.read_text(encoding="utf-8")

        # Assert
        assert "AQWA" in content
        assert "OrcaWave" in content
        assert "BEMRosetta" in content


# ---------------------------------------------------------------------------
# 3. Phase overlay
# ---------------------------------------------------------------------------


class TestPhaseOverlay:
    """Verify phase overlay plot generation."""

    def test_phase_overlay_creates_html(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act
        path = plotter.plot_phase_overlay()

        # Assert
        assert path.exists()
        assert path.is_file()
        assert path.suffix == ".html"

    def test_phase_overlay_with_heading_filter(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act
        path = plotter.plot_phase_overlay(headings=[0.0, 90.0])

        # Assert
        assert path.exists()
        assert path.is_file()


# ---------------------------------------------------------------------------
# 4. Combined overlay
# ---------------------------------------------------------------------------


class TestCombinedOverlay:
    """Verify combined amplitude+phase overlay plot."""

    def test_combined_overlay_creates_html(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act
        path = plotter.plot_combined_overlay()

        # Assert
        assert path.exists()
        assert path.is_file()
        assert path.suffix == ".html"


# ---------------------------------------------------------------------------
# 5. Difference plot
# ---------------------------------------------------------------------------


class TestDifferencePlot:
    """Verify difference-from-reference plot generation."""

    def test_difference_plot_creates_html(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act
        path = plotter.plot_difference(reference_solver="AQWA")

        # Assert
        assert path.exists()
        assert path.is_file()
        assert path.suffix == ".html"

    def test_difference_plot_invalid_reference_raises(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act / Assert
        with pytest.raises(ValueError):
            plotter.plot_difference(reference_solver="UnknownSolver")


# ---------------------------------------------------------------------------
# 6. Pairwise correlation heatmap
# ---------------------------------------------------------------------------


class TestPairwiseHeatmap:
    """Verify heatmap generation from a BenchmarkReport."""

    def test_pairwise_heatmap_creates_html(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        comparator = MultiSolverComparator(three_solver_results)
        report = comparator.generate_report()
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act
        path = plotter.plot_pairwise_correlation_heatmap(report)

        # Assert
        assert path.exists()
        assert path.is_file()
        assert path.suffix == ".html"


# ---------------------------------------------------------------------------
# 7. plot_all
# ---------------------------------------------------------------------------


class TestPlotAll:
    """Verify plot_all orchestration method."""

    def test_plot_all_returns_multiple_paths(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act
        paths = plotter.plot_all()

        # Assert
        assert isinstance(paths, list)
        assert len(paths) >= 3
        for p in paths:
            assert isinstance(p, Path)
            assert p.exists()
            assert p.is_file()

    def test_plot_all_with_headings_filter(
        self,
        three_solver_results: Dict[str, DiffractionResults],
        tmp_path: Path,
    ) -> None:
        # Arrange
        plotter = BenchmarkPlotter(three_solver_results, output_dir=tmp_path)

        # Act
        paths = plotter.plot_all(headings=[0.0, 180.0])

        # Assert
        assert isinstance(paths, list)
        assert len(paths) >= 3
        for p in paths:
            assert p.exists()
            assert p.is_file()
