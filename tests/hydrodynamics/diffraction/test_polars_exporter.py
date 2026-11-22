# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Tests for PolarsExporter (WRK-030)."""
from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
    DOF,
)
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

    @pytest.mark.parametrize(
        "fixture_name", ["mock_diffraction_results", "dense_diffraction_results"]
    )
    def test_row_count(self, request: pytest.FixtureRequest, fixture_name: str) -> None:
        """Count the Cartesian product independently of the exporter.

        Inputs: conftest.py's mock and dense result fixtures; DOF from
        output_schemas.py. Hand calculation for the current fixtures:
        mock: 40 frequencies * 5 headings * 6 DOFs = 1200 rows;
        dense: 24 frequencies * 13 headings * 6 DOFs = 1872 rows.
        Read the dimensions so an abscissa-contract change needs no new total.
        """
        results: DiffractionResults = request.getfixturevalue(fixture_name)
        exporter = PolarsExporter(results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        df = exporter.raos_to_polars()
        frequency_count = results.raos.surge.frequencies.count
        heading_count = results.raos.surge.headings.count
        assert len(df) == frequency_count * heading_count * len(DOF)

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

    @pytest.mark.parametrize(
        "fixture_name", ["mock_diffraction_results", "dense_diffraction_results"]
    )
    @pytest.mark.parametrize("matrix_type", ["added_mass", "damping"])
    def test_matrix_row_count(
        self, request: pytest.FixtureRequest, fixture_name: str, matrix_type: str
    ) -> None:
        """Count each frequency's DOF row/column pairs by hand.

        Inputs: conftest.py's mock and dense matrix sets; DOF from
        output_schemas.py. For both added mass and damping:
        mock: 40 frequencies * 6 row DOFs * 6 column DOFs = 1440 rows;
        dense: 24 frequencies * 6 row DOFs * 6 column DOFs = 864 rows.
        The expected count uses input dimensions, never exported records.
        """
        results: DiffractionResults = request.getfixturevalue(fixture_name)
        exporter = PolarsExporter(results)
        if not POLARS_AVAILABLE:
            pytest.skip("polars not installed")
        if matrix_type == "added_mass":
            matrix_set = results.added_mass
            df = exporter.added_mass_to_polars()
        else:
            matrix_set = results.damping
            df = exporter.damping_to_polars()
        # output_schemas.py: each matrix has a row and column for every DOF.
        assert len(df) == matrix_set.frequencies.count * len(DOF) * len(DOF)

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
        frequency_count = mock_diffraction_results.raos.surge.frequencies.count
        heading_count = mock_diffraction_results.raos.surge.headings.count
        assert len(df) == frequency_count * heading_count * len(DOF)
