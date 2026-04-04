"""Tests for orcaflex_exporter module.

ABOUTME: Tests for OrcaFlexExporter covering vessel type YAML, RAO CSV,
added mass CSV, damping CSV, Excel workbook, summary report, and helpers.
"""
from __future__ import annotations

import csv
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.orcaflex_exporter import OrcaFlexExporter
from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def exporter(mock_diffraction_results: DiffractionResults, tmp_path: Path):
    """OrcaFlexExporter pre-configured with mock results and tmp output."""
    return OrcaFlexExporter(mock_diffraction_results, tmp_path)


@pytest.fixture
def vessel_name(mock_diffraction_results: DiffractionResults) -> str:
    """Expected vessel name used in file naming."""
    return mock_diffraction_results.vessel_name.replace(" ", "_")


# ===========================================================================
# 1. Constructor / initialisation
# ===========================================================================


class TestExporterInit:
    def test_output_dir_created(self, mock_diffraction_results, tmp_path):
        out = tmp_path / "subdir" / "nested"
        exp = OrcaFlexExporter(mock_diffraction_results, out)
        assert out.is_dir()

    def test_vessel_name_spaces_replaced(self, mock_diffraction_results, tmp_path):
        exp = OrcaFlexExporter(mock_diffraction_results, tmp_path)
        assert " " not in exp.vessel_name

    def test_results_stored(self, exporter, mock_diffraction_results):
        assert exporter.results is mock_diffraction_results


# ===========================================================================
# 2. Vessel type YAML export
# ===========================================================================


class TestExportVesselType:
    def test_creates_yml_file(self, exporter, vessel_name):
        path = exporter.export_vessel_type()
        assert path.exists()
        assert path.suffix == ".yml"

    def test_yaml_loadable(self, exporter):
        path = exporter.export_vessel_type()
        data = yaml.safe_load(path.read_text())
        assert "VesselType" in data

    def test_yaml_contains_vessel_metadata(self, exporter, vessel_name):
        path = exporter.export_vessel_type()
        data = yaml.safe_load(path.read_text())
        vt = data["VesselType"]
        assert vt["Name"] == vessel_name
        assert vt["WaterDepth"] == exporter.results.water_depth
        assert "RAODataFile" in vt
        assert "AddedMassDataFile" in vt
        assert "DampingDataFile" in vt


# ===========================================================================
# 3. RAO CSV export
# ===========================================================================


class TestExportRaoCsv:
    def test_creates_csv_file(self, exporter):
        path = exporter.export_raos_csv()
        assert path.exists()
        assert path.suffix == ".csv"

    def test_csv_has_expected_rows(self, exporter):
        path = exporter.export_raos_csv()
        df = pd.read_csv(path)
        n_freq = exporter.results.raos.surge.frequencies.count
        assert len(df) == n_freq

    def test_csv_has_frequency_and_period_cols(self, exporter):
        path = exporter.export_raos_csv()
        df = pd.read_csv(path)
        assert "Frequency (rad/s)" in df.columns
        assert "Period (s)" in df.columns

    def test_csv_has_columns_for_all_dofs_and_headings(self, exporter):
        path = exporter.export_raos_csv()
        df = pd.read_csv(path)
        headings = exporter.results.raos.surge.headings.values
        dof_names = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        for h in headings:
            for dof in dof_names:
                assert f"{dof}_Mag_H{h:.1f}" in df.columns
                assert f"{dof}_Phase_H{h:.1f}" in df.columns

    def test_csv_frequencies_match_results(self, exporter):
        path = exporter.export_raos_csv()
        df = pd.read_csv(path)
        expected = exporter.results.raos.surge.frequencies.values
        np.testing.assert_allclose(df["Frequency (rad/s)"].values, expected, atol=1e-5)


# ===========================================================================
# 4. Added mass CSV export
# ===========================================================================


class TestExportAddedMassCsv:
    def test_creates_csv(self, exporter):
        path = exporter.export_added_mass_csv()
        assert path.exists()

    def test_long_form_columns(self, exporter):
        path = exporter.export_added_mass_csv()
        df = pd.read_csv(path)
        for col in ("Frequency (rad/s)", "Period (s)", "DOF_i", "DOF_j", "AddedMass", "Unit"):
            assert col in df.columns

    def test_row_count(self, exporter):
        """Should have n_freq * 6 * 6 rows."""
        path = exporter.export_added_mass_csv()
        df = pd.read_csv(path)
        n_freq = len(exporter.results.added_mass.matrices)
        assert len(df) == n_freq * 36


# ===========================================================================
# 5. Damping CSV export
# ===========================================================================


class TestExportDampingCsv:
    def test_creates_csv(self, exporter):
        path = exporter.export_damping_csv()
        assert path.exists()

    def test_row_count(self, exporter):
        path = exporter.export_damping_csv()
        df = pd.read_csv(path)
        n_freq = len(exporter.results.damping.matrices)
        assert len(df) == n_freq * 36

    def test_damping_column_present(self, exporter):
        path = exporter.export_damping_csv()
        df = pd.read_csv(path)
        assert "Damping" in df.columns


# ===========================================================================
# 6. Excel workbook export
# ===========================================================================


class TestExportExcelWorkbook:
    def test_creates_xlsx(self, exporter):
        path = exporter.export_excel_workbook()
        assert path.exists()
        assert path.suffix == ".xlsx"

    def test_excel_has_expected_sheets(self, exporter):
        path = exporter.export_excel_workbook()
        xl = pd.ExcelFile(path)
        for sheet in ("Summary", "RAOs", "AddedMass", "Damping", "Discretization"):
            assert sheet in xl.sheet_names


# ===========================================================================
# 7. Summary report export
# ===========================================================================


class TestExportSummaryReport:
    def test_creates_txt_file(self, exporter):
        path = exporter.export_summary_report()
        assert path.exists()
        assert path.suffix == ".txt"

    def test_summary_contains_vessel_info(self, exporter):
        path = exporter.export_summary_report()
        text = path.read_text()
        assert exporter.results.vessel_name in text
        assert exporter.results.analysis_tool in text

    def test_summary_mentions_orcaflex(self, exporter):
        path = exporter.export_summary_report()
        text = path.read_text()
        assert "OrcaFlex" in text


# ===========================================================================
# 8. export_all
# ===========================================================================


class TestExportAll:
    def test_export_all_returns_dict(self, exporter):
        outputs = exporter.export_all()
        assert isinstance(outputs, dict)
        expected_keys = {"vessel_type", "rao_csv", "added_mass_csv", "damping_csv", "excel", "summary"}
        assert expected_keys == set(outputs.keys())

    def test_export_all_files_exist(self, exporter):
        outputs = exporter.export_all()
        for key, path in outputs.items():
            assert Path(path).exists(), f"Missing output: {key} -> {path}"


# ===========================================================================
# 9. Helper / static methods
# ===========================================================================


class TestHelperMethods:
    def test_added_mass_unit_translation(self):
        assert OrcaFlexExporter._get_added_mass_unit(0, 1) == "kg"
        assert OrcaFlexExporter._get_added_mass_unit(2, 2) == "kg"

    def test_added_mass_unit_coupling(self):
        assert OrcaFlexExporter._get_added_mass_unit(0, 3) == "kg.m"
        assert OrcaFlexExporter._get_added_mass_unit(4, 1) == "kg.m"

    def test_added_mass_unit_rotation(self):
        assert OrcaFlexExporter._get_added_mass_unit(3, 5) == "kg.m^2"

    def test_damping_unit_translation(self):
        assert OrcaFlexExporter._get_damping_unit(0, 0) == "N.s/m"

    def test_damping_unit_coupling(self):
        unit = OrcaFlexExporter._get_damping_unit(1, 4)
        assert "N" in unit

    def test_damping_unit_rotation(self):
        assert OrcaFlexExporter._get_damping_unit(3, 5) == "N.m.s/rad"
