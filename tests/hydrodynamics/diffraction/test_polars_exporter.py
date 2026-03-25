"""Tests for PolarsExporter (WRK-030)."""
from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults
from digitalmodel.hydrodynamics.diffraction.polars_exporter import (
    POLARS_AVAILABLE,
    PolarsExporter,
)


class TestRaosToPolars:
    """Test RAO DataFrame construction."""

    def test_returns_polars_dataframe(self, mock_diffraction_results: DiffractionResults) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        import polars as pl

        df = exporter.raos_to_polars()
        assert isinstance(df, pl.DataFrame)

    def test_columns_present(self, mock_diffraction_results: DiffractionResults) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        df = exporter.raos_to_polars()
        expected_cols = {"frequency", "period", "heading", "dof", "amplitude", "phase", "unit"}
        assert expected_cols == set(df.columns)

    def test_row_count(self, mock_diffraction_results: DiffractionResults) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        df = exporter.raos_to_polars()
        # 10 frequencies * 5 headings * 6 DOFs = 300
        assert len(df) == 10 * 5 * 6

    def test_dof_values(self, mock_diffraction_results: DiffractionResults) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        df = exporter.raos_to_polars()
        dofs = sorted(df["dof"].unique().to_list())
        assert dofs == ["HEAVE", "PITCH", "ROLL", "SURGE", "SWAY", "YAW"]

    def test_frequency_values_positive(self, mock_diffraction_results: DiffractionResults) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        df = exporter.raos_to_polars()
        assert (df["frequency"] > 0).all()

    def test_amplitude_non_negative(self, mock_diffraction_results: DiffractionResults) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        df = exporter.raos_to_polars()
        assert (df["amplitude"] >= 0).all()


class TestMatrixExport:
    """Test added mass and damping DataFrame construction."""

    def test_added_mass_columns(self, mock_diffraction_results: DiffractionResults) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        df = exporter.added_mass_to_polars()
        expected = {"frequency", "period", "dof_i", "dof_j", "value", "unit"}
        assert expected == set(df.columns)

    def test_added_mass_row_count(self, mock_diffraction_results: DiffractionResults) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        df = exporter.added_mass_to_polars()
        # 10 frequencies * 6 * 6 = 360
        assert len(df) == 10 * 6 * 6

    def test_damping_columns(self, mock_diffraction_results: DiffractionResults) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        df = exporter.damping_to_polars()
        expected = {"frequency", "period", "dof_i", "dof_j", "value", "unit"}
        assert expected == set(df.columns)


class TestCsvExport:
    """Test CSV file export."""

    def test_export_raos_csv_creates_file(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        path = exporter.export_raos_csv(tmp_path)
        assert path.exists()
        assert path.suffix == ".csv"

    def test_export_added_mass_csv(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        path = exporter.export_added_mass_csv(tmp_path)
        assert path.exists()

    def test_export_damping_csv(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        path = exporter.export_damping_csv(tmp_path)
        assert path.exists()

    def test_export_all_csv(
        self, mock_diffraction_results: DiffractionResults, tmp_path: Path
    ) -> None:
        exporter = PolarsExporter(mock_diffraction_results)
        files = exporter.export_all_csv(tmp_path)
        assert "raos" in files
        assert "added_mass" in files
        assert "damping" in files
        for path in files.values():
            assert path.exists()


class TestPandasFallback:
    """Test Pandas fallback path."""

    def test_raos_to_pandas_returns_dataframe(
        self, mock_diffraction_results: DiffractionResults
    ) -> None:
        import pandas as pd

        exporter = PolarsExporter(mock_diffraction_results)
        df = exporter.raos_to_pandas()
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 10 * 5 * 6
