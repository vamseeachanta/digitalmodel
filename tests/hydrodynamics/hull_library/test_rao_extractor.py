"""
ABOUTME: Tests for OrcaWave .xlsx → RAOData extraction and RAODatabase population.
Tests the xlsx_to_rao_data and populate_database_from_xlsx pipelines
using committed fixture files from licensed-win-1.

Traceability: #1765, #1597
"""
from __future__ import annotations

import re
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.models import RAOData
from digitalmodel.hydrodynamics.hull_library.rao_database import (
    RAODatabase,
    RAODatabaseEntry,
)
from digitalmodel.hydrodynamics.hull_library.rao_extractor import (
    XlsxFormatDetector,
    PipelineXlsxReader,
    NativeXlsxReader,
    xlsx_to_rao_data,
    xlsx_to_hydro_coefficients,
    populate_database_from_xlsx,
    HydroCoefficients,
)

# ---------------------------------------------------------------------------
# Fixture paths
# ---------------------------------------------------------------------------

FIXTURES_DIR = Path(__file__).resolve().parents[2] / "fixtures" / "solver"
PIPELINE_XLSX = FIXTURES_DIR / "test01_unit_box.xlsx"
PIPELINE_XLSX_ELLIPSOID = FIXTURES_DIR / "ellipsoid.xlsx"
NATIVE_XLSX = FIXTURES_DIR / "L00_test01.xlsx"
NATIVE_XLSX_SHIP = FIXTURES_DIR / "L01_001_ship_raos.xlsx"


# ---------------------------------------------------------------------------
# XlsxFormatDetector
# ---------------------------------------------------------------------------


class TestXlsxFormatDetector:
    """Detect whether an xlsx is pipeline or native OrcaWave format."""

    def test_pipeline_format_detected(self):
        fmt = XlsxFormatDetector.detect(PIPELINE_XLSX)
        assert fmt == "pipeline"

    def test_native_format_detected(self):
        fmt = XlsxFormatDetector.detect(NATIVE_XLSX)
        assert fmt == "native"

    def test_pipeline_ellipsoid_detected(self):
        fmt = XlsxFormatDetector.detect(PIPELINE_XLSX_ELLIPSOID)
        assert fmt == "pipeline"

    def test_native_ship_detected(self):
        fmt = XlsxFormatDetector.detect(NATIVE_XLSX_SHIP)
        assert fmt == "native"

    def test_nonexistent_file_raises(self):
        with pytest.raises(FileNotFoundError):
            XlsxFormatDetector.detect(Path("/nonexistent/file.xlsx"))


# ---------------------------------------------------------------------------
# PipelineXlsxReader — test01_unit_box
# ---------------------------------------------------------------------------


class TestPipelineXlsxReaderUnitBox:
    """Parse pipeline-format xlsx (test01_unit_box) into RAOData."""

    @pytest.fixture(autouse=True)
    def _load(self):
        self.reader = PipelineXlsxReader(PIPELINE_XLSX)
        self.rao_data = self.reader.read_rao_data()

    def test_returns_rao_data_instance(self):
        assert isinstance(self.rao_data, RAOData)

    def test_frequencies_sorted_ascending(self):
        freqs = self.rao_data.frequencies
        assert np.all(np.diff(freqs) > 0), "frequencies must be ascending"

    def test_frequency_count(self):
        assert len(self.rao_data.frequencies) == 50

    def test_heading_count(self):
        # test01_unit_box has 2 headings: 0.0 and 27.0
        assert len(self.rao_data.directions) == 2

    def test_headings_values(self):
        np.testing.assert_array_almost_equal(
            self.rao_data.directions, [0.0, 27.0]
        )

    def test_amplitudes_shape(self):
        # (n_freq, n_dir, 6)
        assert self.rao_data.amplitudes.shape == (50, 2, 6)

    def test_phases_shape(self):
        assert self.rao_data.phases.shape == (50, 2, 6)

    def test_amplitudes_non_negative(self):
        assert np.all(self.rao_data.amplitudes >= 0)

    def test_surge_amplitude_near_unity_low_freq(self):
        """At low frequency, surge RAO should be ~1.0 for a unit box."""
        surge_amp = self.rao_data.amplitudes[0, 0, 0]  # lowest freq, heading 0, surge
        assert abs(surge_amp - 1.0) < 0.01

    def test_heave_amplitude_near_unity_low_freq(self):
        """At low frequency, heave RAO should be ~1.0 for a unit box."""
        heave_amp = self.rao_data.amplitudes[0, 0, 2]  # lowest freq, heading 0, heave
        assert abs(heave_amp - 1.0) < 0.01

    def test_vessel_name_extracted(self):
        assert self.rao_data.vessel_name == "test01_unit_box"

    def test_frequency_units_are_rad_per_s(self):
        """Frequencies in the pipeline xlsx are already in rad/s."""
        # The lowest freq should be ~0.1 rad/s
        assert 0.05 < self.rao_data.frequencies[0] < 0.15


# ---------------------------------------------------------------------------
# PipelineXlsxReader — ellipsoid
# ---------------------------------------------------------------------------


class TestPipelineXlsxReaderEllipsoid:
    """Parse pipeline-format xlsx (ellipsoid) with many headings."""

    @pytest.fixture(autouse=True)
    def _load(self):
        self.reader = PipelineXlsxReader(PIPELINE_XLSX_ELLIPSOID)
        self.rao_data = self.reader.read_rao_data()

    def test_frequency_count(self):
        # Ellipsoid has 1 frequency
        assert len(self.rao_data.frequencies) == 1

    def test_heading_count(self):
        # 18 headings (0-340 deg step 20)
        assert len(self.rao_data.directions) == 18

    def test_amplitudes_shape(self):
        assert self.rao_data.amplitudes.shape == (1, 18, 6)


# ---------------------------------------------------------------------------
# HydroCoefficients extraction
# ---------------------------------------------------------------------------


class TestHydroCoefficients:
    """Extract added mass and damping matrices from pipeline xlsx."""

    @pytest.fixture(autouse=True)
    def _load(self):
        self.reader = PipelineXlsxReader(PIPELINE_XLSX)
        self.coeffs = self.reader.read_hydro_coefficients()

    def test_returns_hydro_coefficients(self):
        assert isinstance(self.coeffs, HydroCoefficients)

    def test_added_mass_shape(self):
        # (n_freq, 6, 6)
        assert self.coeffs.added_mass.shape == (50, 6, 6)

    def test_damping_shape(self):
        assert self.coeffs.damping.shape == (50, 6, 6)

    def test_frequencies_match(self):
        assert len(self.coeffs.frequencies) == 50

    def test_added_mass_diagonal_positive(self):
        """Diagonal added mass entries should be non-negative."""
        for i in range(6):
            diag = self.coeffs.added_mass[:, i, i]
            assert np.all(diag >= 0), f"DOF {i} diagonal has negative values"

    def test_damping_diagonal_non_negative(self):
        """Diagonal damping entries should be non-negative."""
        for i in range(6):
            diag = self.coeffs.damping[:, i, i]
            assert np.all(diag >= -1e-10), f"DOF {i} diagonal has negative values"


# ---------------------------------------------------------------------------
# Top-level convenience: xlsx_to_rao_data
# ---------------------------------------------------------------------------


class TestXlsxToRaoData:
    """Auto-detecting convenience function."""

    def test_pipeline_format(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        assert isinstance(rao, RAOData)
        assert rao.amplitudes.shape[2] == 6

    def test_pipeline_format_ellipsoid(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX_ELLIPSOID)
        assert isinstance(rao, RAOData)
        assert rao.amplitudes.shape == (1, 18, 6)

    def test_native_format(self):
        rao = xlsx_to_rao_data(NATIVE_XLSX)
        assert isinstance(rao, RAOData)
        assert rao.amplitudes.shape[2] == 6

    def test_custom_vessel_name(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX, vessel_name="MyVessel")
        assert rao.vessel_name == "MyVessel"


# ---------------------------------------------------------------------------
# xlsx_to_hydro_coefficients
# ---------------------------------------------------------------------------


class TestXlsxToHydroCoefficients:
    """Extract added mass and damping via auto-detecting convenience."""

    def test_pipeline_format(self):
        coeffs = xlsx_to_hydro_coefficients(PIPELINE_XLSX)
        assert isinstance(coeffs, HydroCoefficients)
        assert coeffs.added_mass.shape == (50, 6, 6)

    def test_native_format(self):
        coeffs = xlsx_to_hydro_coefficients(NATIVE_XLSX)
        assert isinstance(coeffs, HydroCoefficients)
        assert coeffs.added_mass.shape[1:] == (6, 6)


# ---------------------------------------------------------------------------
# Database population
# ---------------------------------------------------------------------------


class TestPopulateDatabaseFromXlsx:
    """Full pipeline: xlsx → RAOData → RAODatabaseEntry → RAODatabase."""

    def test_populate_single_entry(self):
        db = RAODatabase()
        entry = populate_database_from_xlsx(
            db=db,
            xlsx_path=PIPELINE_XLSX,
            variation_id="wamit_val_test01",
            hull_params={"length_scale": 1.0, "beam_scale": 1.0},
        )
        assert isinstance(entry, RAODatabaseEntry)
        assert entry.variation_id == "wamit_val_test01"

    def test_entry_stored_in_database(self):
        db = RAODatabase()
        populate_database_from_xlsx(
            db=db,
            xlsx_path=PIPELINE_XLSX,
            variation_id="wamit_val_test01",
            hull_params={"length_scale": 1.0},
        )
        retrieved = db.get_by_id("wamit_val_test01")
        assert retrieved.variation_id == "wamit_val_test01"

    def test_rao_data_in_entry_is_valid(self):
        db = RAODatabase()
        entry = populate_database_from_xlsx(
            db=db,
            xlsx_path=PIPELINE_XLSX,
            variation_id="v001",
            hull_params={"length_scale": 1.0},
        )
        rao = entry.rao_data
        assert isinstance(rao, RAOData)
        assert rao.amplitudes.shape == (50, 2, 6)

    def test_metadata_included(self):
        db = RAODatabase()
        entry = populate_database_from_xlsx(
            db=db,
            xlsx_path=PIPELINE_XLSX,
            variation_id="v001",
            hull_params={},
            metadata={"solver": "OrcaWave", "case": "L00_2.1"},
        )
        assert entry.metadata["solver"] == "OrcaWave"
        assert entry.metadata["case"] == "L00_2.1"
        assert "source_file" in entry.metadata

    def test_populate_multiple_entries(self):
        db = RAODatabase()
        populate_database_from_xlsx(
            db, PIPELINE_XLSX, "v001", {"length_scale": 1.0}
        )
        populate_database_from_xlsx(
            db, PIPELINE_XLSX_ELLIPSOID, "v002", {"length_scale": 0.5}
        )
        results = list(db.query({}))
        assert len(results) == 2

    def test_round_trip_persistence(self, tmp_path):
        """Populate → save to parquet → load → verify."""
        db = RAODatabase()
        populate_database_from_xlsx(
            db, PIPELINE_XLSX, "v001", {"length_scale": 1.0}
        )
        db_path = tmp_path / "test_rao.parquet"
        db.save_to_disk(db_path)

        db2 = RAODatabase()
        db2.load_from_disk(db_path)
        entry = db2.get_by_id("v001")
        assert entry.rao_data.amplitudes.shape == (50, 2, 6)
        np.testing.assert_array_almost_equal(
            entry.rao_data.frequencies,
            db.get_by_id("v001").rao_data.frequencies,
        )


# ---------------------------------------------------------------------------
# NativeXlsxReader
# ---------------------------------------------------------------------------


class TestNativeXlsxReader:
    """Parse native OrcaWave xlsx format (L00_test01)."""

    @pytest.fixture(autouse=True)
    def _load(self):
        self.reader = NativeXlsxReader(NATIVE_XLSX)
        self.rao_data = self.reader.read_rao_data()

    def test_returns_rao_data(self):
        assert isinstance(self.rao_data, RAOData)

    def test_frequency_count(self):
        # L00_test01 has 50 frequencies
        assert len(self.rao_data.frequencies) == 50

    def test_heading_count(self):
        # L00_test01 has 2 headings
        assert len(self.rao_data.directions) == 2

    def test_amplitudes_shape(self):
        assert self.rao_data.amplitudes.shape == (50, 2, 6)

    def test_phases_shape(self):
        assert self.rao_data.phases.shape == (50, 2, 6)

    def test_vessel_name(self):
        assert self.rao_data.vessel_name == "L00_test01"

    def test_hydro_coefficients(self):
        coeffs = self.reader.read_hydro_coefficients()
        assert isinstance(coeffs, HydroCoefficients)
        assert coeffs.added_mass.shape == (50, 6, 6)
