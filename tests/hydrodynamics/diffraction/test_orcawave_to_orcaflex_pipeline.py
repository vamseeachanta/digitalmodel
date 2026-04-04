"""
ABOUTME: Tests for the automated OrcaWave → OrcaFlex handoff pipeline.
Validates the single-command convert_orcawave_xlsx_to_orcaflex function
and CLI entry point using committed fixtures.

Traceability: #1768, #1592
"""
from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.orcawave_to_orcaflex import (
    convert_orcawave_xlsx_to_orcaflex,
    main as cli_main,
    rao_data_to_diffraction_results,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DiffractionResults,
)
from digitalmodel.hydrodynamics.hull_library.rao_extractor import (
    xlsx_to_hydro_coefficients,
    xlsx_to_rao_data,
)
from digitalmodel.hydrodynamics.models import RAOData

# ---------------------------------------------------------------------------
# Fixture paths
# ---------------------------------------------------------------------------

FIXTURES_DIR = Path(__file__).resolve().parents[2] / "fixtures" / "solver"
PIPELINE_XLSX = FIXTURES_DIR / "test01_unit_box.xlsx"
ELLIPSOID_XLSX = FIXTURES_DIR / "ellipsoid.xlsx"
NATIVE_XLSX = FIXTURES_DIR / "L00_test01.xlsx"


# ---------------------------------------------------------------------------
# convert_orcawave_xlsx_to_orcaflex — pipeline format
# ---------------------------------------------------------------------------


class TestConvertPipelineXlsx:
    """Single-command pipeline with pipeline-format xlsx."""

    @pytest.fixture(autouse=True)
    def _setup(self, tmp_path):
        self.output_dir = tmp_path / "orcaflex_output"
        self.outputs = convert_orcawave_xlsx_to_orcaflex(
            xlsx_path=PIPELINE_XLSX,
            output_dir=self.output_dir,
        )

    def test_returns_dict_of_paths(self):
        assert isinstance(self.outputs, dict)
        assert len(self.outputs) >= 5

    def test_all_output_files_exist(self):
        for key, path in self.outputs.items():
            assert path.exists(), f"Missing: {key} → {path}"

    def test_vessel_type_yaml_valid(self):
        yaml_path = self.outputs["vessel_type"]
        with open(yaml_path) as f:
            data = yaml.safe_load(f)
        assert "VesselType" in data
        assert data["VesselType"]["Name"] == "test01_unit_box"
        assert data["VesselType"]["WaterDepth"] == 100.0

    def test_rao_csv_has_data(self):
        import pandas as pd
        df = pd.read_csv(self.outputs["rao_csv"])
        assert len(df) >= 50

    def test_added_mass_csv_has_data(self):
        import pandas as pd
        df = pd.read_csv(self.outputs["added_mass_csv"])
        assert len(df) >= 50

    def test_damping_csv_has_data(self):
        import pandas as pd
        df = pd.read_csv(self.outputs["damping_csv"])
        assert len(df) >= 50

    def test_excel_workbook_created(self):
        assert self.outputs["excel"].exists()
        assert self.outputs["excel"].stat().st_size > 0

    def test_summary_report_mentions_vessel(self):
        content = self.outputs["summary"].read_text()
        assert "test01_unit_box" in content


# ---------------------------------------------------------------------------
# convert_orcawave_xlsx_to_orcaflex — ellipsoid
# ---------------------------------------------------------------------------


class TestConvertEllipsoidXlsx:
    """Pipeline with the ellipsoid fixture (1 freq, 18 headings)."""

    def test_converts_successfully(self, tmp_path):
        outputs = convert_orcawave_xlsx_to_orcaflex(
            xlsx_path=ELLIPSOID_XLSX,
            output_dir=tmp_path / "ell",
        )
        assert all(p.exists() for p in outputs.values())

    def test_vessel_name_from_filename(self, tmp_path):
        outputs = convert_orcawave_xlsx_to_orcaflex(
            xlsx_path=ELLIPSOID_XLSX,
            output_dir=tmp_path / "ell",
        )
        with open(outputs["vessel_type"]) as f:
            data = yaml.safe_load(f)
        assert data["VesselType"]["Name"] == "ellipsoid"


# ---------------------------------------------------------------------------
# convert_orcawave_xlsx_to_orcaflex — native format
# ---------------------------------------------------------------------------


class TestConvertNativeXlsx:
    """Pipeline with native OrcaWave format (L00_test01)."""

    def test_converts_successfully(self, tmp_path):
        outputs = convert_orcawave_xlsx_to_orcaflex(
            xlsx_path=NATIVE_XLSX,
            output_dir=tmp_path / "native",
        )
        assert all(p.exists() for p in outputs.values())

    def test_vessel_name_from_filename(self, tmp_path):
        outputs = convert_orcawave_xlsx_to_orcaflex(
            xlsx_path=NATIVE_XLSX,
            output_dir=tmp_path / "native",
        )
        with open(outputs["vessel_type"]) as f:
            data = yaml.safe_load(f)
        assert data["VesselType"]["Name"] == "L00_test01"


# ---------------------------------------------------------------------------
# Custom vessel name and water depth
# ---------------------------------------------------------------------------


class TestCustomParameters:
    """Override vessel name and water depth."""

    def test_custom_vessel_name(self, tmp_path):
        outputs = convert_orcawave_xlsx_to_orcaflex(
            xlsx_path=PIPELINE_XLSX,
            output_dir=tmp_path / "custom",
            vessel_name="MyBarge",
        )
        with open(outputs["vessel_type"]) as f:
            data = yaml.safe_load(f)
        assert data["VesselType"]["Name"] == "MyBarge"

    def test_custom_water_depth(self, tmp_path):
        outputs = convert_orcawave_xlsx_to_orcaflex(
            xlsx_path=PIPELINE_XLSX,
            output_dir=tmp_path / "custom",
            water_depth=200.0,
        )
        with open(outputs["vessel_type"]) as f:
            data = yaml.safe_load(f)
        assert data["VesselType"]["WaterDepth"] == 200.0

    def test_output_dir_created_if_missing(self, tmp_path):
        deep_dir = tmp_path / "a" / "b" / "c"
        outputs = convert_orcawave_xlsx_to_orcaflex(
            xlsx_path=PIPELINE_XLSX,
            output_dir=deep_dir,
        )
        assert deep_dir.exists()
        assert all(p.exists() for p in outputs.values())


# ---------------------------------------------------------------------------
# rao_data_to_diffraction_results (bridge)
# ---------------------------------------------------------------------------


class TestBridgeFunction:
    """Validate the RAOData → DiffractionResults bridge."""

    def test_produces_valid_results(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        coeffs = xlsx_to_hydro_coefficients(PIPELINE_XLSX)
        results = rao_data_to_diffraction_results(rao, coeffs)
        assert isinstance(results, DiffractionResults)

    def test_all_six_dofs_present(self):
        from digitalmodel.hydrodynamics.diffraction.output_schemas import DOF
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        results = rao_data_to_diffraction_results(rao)
        for dof in DOF:
            comp = results.raos.get_component(dof)
            assert comp.magnitude.shape[0] == 50

    def test_source_files_tracked(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        results = rao_data_to_diffraction_results(
            rao, source_file="/path/to/input.xlsx"
        )
        assert results.source_files == ["/path/to/input.xlsx"]

    def test_zero_fill_without_coefficients(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        results = rao_data_to_diffraction_results(rao, coeffs=None)
        assert results.added_mass is not None
        for mat in results.added_mass.matrices:
            np.testing.assert_array_equal(mat.matrix, np.zeros((6, 6)))


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------


class TestCli:
    """Test the CLI main() function."""

    def test_cli_success(self, tmp_path):
        out_dir = tmp_path / "cli_out"
        rc = cli_main([
            str(PIPELINE_XLSX),
            "-o", str(out_dir),
        ])
        assert rc == 0
        assert (out_dir / "test01_unit_box_vessel_type.yml").exists()

    def test_cli_custom_vessel_name(self, tmp_path):
        out_dir = tmp_path / "cli_out"
        rc = cli_main([
            str(PIPELINE_XLSX),
            "-o", str(out_dir),
            "--vessel-name", "TestBarge",
        ])
        assert rc == 0
        with open(out_dir / "TestBarge_vessel_type.yml") as f:
            data = yaml.safe_load(f)
        assert data["VesselType"]["Name"] == "TestBarge"

    def test_cli_missing_file(self):
        rc = cli_main(["/nonexistent/file.xlsx"])
        assert rc == 1

    def test_cli_with_water_depth(self, tmp_path):
        out_dir = tmp_path / "cli_out"
        rc = cli_main([
            str(PIPELINE_XLSX),
            "-o", str(out_dir),
            "--water-depth", "250.0",
        ])
        assert rc == 0
        with open(out_dir / "test01_unit_box_vessel_type.yml") as f:
            data = yaml.safe_load(f)
        assert data["VesselType"]["WaterDepth"] == 250.0


# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------


class TestErrorHandling:
    """Graceful failures."""

    def test_nonexistent_xlsx_raises(self, tmp_path):
        with pytest.raises(FileNotFoundError):
            convert_orcawave_xlsx_to_orcaflex(
                xlsx_path="/nonexistent.xlsx",
                output_dir=tmp_path,
            )
