"""Tests for RAOPlotter (WRK-030)."""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
    RAOSet,
)
from digitalmodel.hydrodynamics.diffraction.rao_plotter import DOF_ORDER, RAOPlotter


class TestPlotAmplitude:
    """Test amplitude plot generation."""

    def test_creates_html_file(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        path = plotter.plot_amplitude()
        assert path.exists()
        assert path.suffix == ".html"

    def test_contains_plotly(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        path = plotter.plot_amplitude()
        content = path.read_text()
        assert "plotly" in content.lower()

    def test_heading_filter(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        path = plotter.plot_amplitude(headings=[0.0, 90.0])
        assert path.exists()


class TestPlotPhase:
    """Test phase plot generation."""

    def test_creates_html_file(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        path = plotter.plot_phase()
        assert path.exists()
        assert path.suffix == ".html"


class TestPlotCombined:
    """Test combined amplitude+phase plot."""

    def test_creates_html_file(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        path = plotter.plot_combined()
        assert path.exists()
        assert path.suffix == ".html"


class TestPlotSingleDof:
    """Test single-DOF plot."""

    def test_creates_html_for_heave(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        path = plotter.plot_single_dof(DOF.HEAVE)
        assert path.exists()

    def test_creates_html_for_roll(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        path = plotter.plot_single_dof(DOF.ROLL)
        assert path.exists()


class TestPlotAll:
    """Test plot_all generates multiple files."""

    def test_returns_list_of_paths(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        paths = plotter.plot_all()
        assert len(paths) == 3  # amplitude, phase, combined
        for p in paths:
            assert p.exists()


class TestXAxisSwitching:
    """Test period vs frequency x-axis."""

    def test_frequency_axis(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path, x_axis="frequency")
        path = plotter.plot_amplitude()
        content = path.read_text()
        assert "Frequency" in content

    def test_period_axis(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path, x_axis="period")
        path = plotter.plot_amplitude()
        content = path.read_text()
        assert "Period" in content


class TestDofOrder:
    """Test DOF ordering constant."""

    def test_dof_order_has_six_entries(self) -> None:
        assert len(DOF_ORDER) == 6

    def test_dof_order_starts_surge_ends_yaw(self) -> None:
        assert DOF_ORDER[0] == DOF.SURGE
        assert DOF_ORDER[-1] == DOF.YAW


class TestLayoutCorrectness:
    """Test plot layout properties."""

    def test_amplitude_contains_all_dof_names(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        path = plotter.plot_amplitude()
        content = path.read_text()
        for dof in DOF_ORDER:
            assert dof.name in content.upper() or dof.name.capitalize() in content

    def test_combined_has_twelve_subplots_hint(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        plotter = RAOPlotter(mock_diffraction_results, tmp_path)
        path = plotter.plot_combined()
        # Combined should have content for all 6 DOFs in both amp and phase
        content = path.read_text()
        assert "Surge" in content or "SURGE" in content
        assert "Yaw" in content or "YAW" in content
