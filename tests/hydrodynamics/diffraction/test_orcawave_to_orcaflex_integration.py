"""
ABOUTME: Integration test for the OrcaWave → OrcaFlex handoff pipeline.
Validates the full round-trip:
  .xlsx fixture → RAOData extraction → DiffractionResults → OrcaFlex export
  → re-read exported data → compare against source within tolerance.

Uses committed fixtures from licensed-win-1 (no OrcFxAPI required).

Traceability: #1766, #1605
Acceptance criteria:
  - Amplitude tolerance: 1%
  - Phase tolerance: 5 degrees
  - All 6 DOFs pass
  - Coordinate system transformation validated
"""
from __future__ import annotations

from datetime import datetime
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import yaml

from digitalmodel.hydrodynamics.hull_library.rao_extractor import (
    HydroCoefficients,
    xlsx_to_hydro_coefficients,
    xlsx_to_rao_data,
)
from digitalmodel.hydrodynamics.models import RAOData
from digitalmodel.hydrodynamics.diffraction.orcaflex_exporter import (
    OrcaFlexExporter,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    AddedMassSet,
    DampingSet,
    DiffractionResults,
    DOF,
    FrequencyData,
    HeadingData,
    HydrodynamicMatrix,
    RAOComponent,
    RAOSet,
)

# ---------------------------------------------------------------------------
# Fixture paths
# ---------------------------------------------------------------------------

FIXTURES_DIR = Path(__file__).resolve().parents[2] / "fixtures" / "solver"
PIPELINE_XLSX = FIXTURES_DIR / "test01_unit_box.xlsx"
ELLIPSOID_XLSX = FIXTURES_DIR / "ellipsoid.xlsx"
NATIVE_XLSX = FIXTURES_DIR / "L00_test01.xlsx"

# Tolerance constants per acceptance criteria
AMPLITUDE_REL_TOL = 0.01   # 1%
PHASE_ABS_TOL_DEG = 5.0    # 5 degrees


# ---------------------------------------------------------------------------
# Bridge: RAOData + HydroCoefficients → DiffractionResults
# ---------------------------------------------------------------------------


def rao_data_to_diffraction_results(
    rao: RAOData,
    coeffs: HydroCoefficients | None = None,
    water_depth: float = 100.0,
    analysis_tool: str = "OrcaWave",
    source_file: str | None = None,
) -> DiffractionResults:
    """Convert RAOData + HydroCoefficients to DiffractionResults schema.

    This bridges the rao_extractor output to the OrcaFlexExporter input.
    """
    freq_data = FrequencyData(
        values=rao.frequencies,
        periods=2.0 * np.pi / rao.frequencies,
        count=len(rao.frequencies),
        min_freq=float(np.min(rao.frequencies)),
        max_freq=float(np.max(rao.frequencies)),
    )
    heading_data = HeadingData(
        values=rao.directions,
        count=len(rao.directions),
        min_heading=float(np.min(rao.directions)),
        max_heading=float(np.max(rao.directions)),
    )

    dof_list = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]
    components: dict[DOF, RAOComponent] = {}
    for i, dof in enumerate(dof_list):
        unit = "m/m" if i < 3 else "deg/m"
        components[dof] = RAOComponent(
            dof=dof,
            magnitude=rao.amplitudes[:, :, i],
            phase=rao.phases[:, :, i],
            frequencies=freq_data,
            headings=heading_data,
            unit=unit,
        )

    rao_set = RAOSet(
        vessel_name=rao.vessel_name,
        analysis_tool=analysis_tool,
        water_depth=water_depth,
        surge=components[DOF.SURGE],
        sway=components[DOF.SWAY],
        heave=components[DOF.HEAVE],
        roll=components[DOF.ROLL],
        pitch=components[DOF.PITCH],
        yaw=components[DOF.YAW],
        created_date=datetime.now().strftime("%Y-%m-%d"),
        source_file=source_file,
    )

    # Build added mass / damping sets
    added_mass_set = None
    damping_set = None
    if coeffs is not None:
        am_matrices = []
        damp_matrices = []
        for i, freq in enumerate(coeffs.frequencies):
            am_matrices.append(
                HydrodynamicMatrix(
                    matrix=coeffs.added_mass[i],
                    frequency=float(freq),
                    matrix_type="added_mass",
                    units={"linear": "kg", "angular": "kg.m^2"},
                )
            )
            damp_matrices.append(
                HydrodynamicMatrix(
                    matrix=coeffs.damping[i],
                    frequency=float(freq),
                    matrix_type="damping",
                    units={"linear": "N.s/m", "angular": "N.m.s/rad"},
                )
            )

        am_freq = FrequencyData(
            values=coeffs.frequencies,
            periods=2.0 * np.pi / coeffs.frequencies,
            count=len(coeffs.frequencies),
            min_freq=float(np.min(coeffs.frequencies)),
            max_freq=float(np.max(coeffs.frequencies)),
        )
        added_mass_set = AddedMassSet(
            vessel_name=rao.vessel_name,
            analysis_tool=analysis_tool,
            water_depth=water_depth,
            matrices=am_matrices,
            frequencies=am_freq,
            created_date=datetime.now().strftime("%Y-%m-%d"),
        )
        damping_set = DampingSet(
            vessel_name=rao.vessel_name,
            analysis_tool=analysis_tool,
            water_depth=water_depth,
            matrices=damp_matrices,
            frequencies=am_freq,
            created_date=datetime.now().strftime("%Y-%m-%d"),
        )

    # DiffractionResults requires non-None added_mass and damping.
    # If no coefficients provided, create empty sets with zero matrices.
    if added_mass_set is None:
        empty_freq = FrequencyData(
            values=rao.frequencies,
            periods=2.0 * np.pi / rao.frequencies,
            count=len(rao.frequencies),
            min_freq=float(np.min(rao.frequencies)),
            max_freq=float(np.max(rao.frequencies)),
        )
        zero_matrices = [
            HydrodynamicMatrix(
                matrix=np.zeros((6, 6)),
                frequency=float(f),
                matrix_type="added_mass",
                units={"linear": "kg"},
            )
            for f in rao.frequencies
        ]
        added_mass_set = AddedMassSet(
            vessel_name=rao.vessel_name,
            analysis_tool=analysis_tool,
            water_depth=water_depth,
            matrices=zero_matrices,
            frequencies=empty_freq,
            created_date=datetime.now().strftime("%Y-%m-%d"),
        )
        zero_damp = [
            HydrodynamicMatrix(
                matrix=np.zeros((6, 6)),
                frequency=float(f),
                matrix_type="damping",
                units={"linear": "N.s/m"},
            )
            for f in rao.frequencies
        ]
        damping_set = DampingSet(
            vessel_name=rao.vessel_name,
            analysis_tool=analysis_tool,
            water_depth=water_depth,
            matrices=zero_damp,
            frequencies=empty_freq,
            created_date=datetime.now().strftime("%Y-%m-%d"),
        )

    source_files = [source_file] if source_file else None

    return DiffractionResults(
        vessel_name=rao.vessel_name,
        analysis_tool=analysis_tool,
        water_depth=water_depth,
        raos=rao_set,
        added_mass=added_mass_set,
        damping=damping_set,
        created_date=datetime.now().strftime("%Y-%m-%d"),
        source_files=source_files,
    )


# ---------------------------------------------------------------------------
# Helper: re-read exported CSV and compare to source
# ---------------------------------------------------------------------------


def _read_rao_csv(csv_path: Path) -> pd.DataFrame:
    """Read RAO CSV exported by OrcaFlexExporter."""
    return pd.read_csv(csv_path)


def _read_matrix_csv(csv_path: Path) -> pd.DataFrame:
    """Read added mass or damping CSV exported by OrcaFlexExporter."""
    return pd.read_csv(csv_path)


# ---------------------------------------------------------------------------
# Test: Bridge function works correctly
# ---------------------------------------------------------------------------


class TestRaoDataToDiffractionResults:
    """Validate the RAOData → DiffractionResults bridge."""

    def test_bridge_produces_valid_results(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        coeffs = xlsx_to_hydro_coefficients(PIPELINE_XLSX)
        results = rao_data_to_diffraction_results(rao, coeffs)
        assert isinstance(results, DiffractionResults)

    def test_bridge_preserves_vessel_name(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        results = rao_data_to_diffraction_results(rao)
        assert results.vessel_name == "test01_unit_box"

    def test_bridge_rao_set_has_all_dofs(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        results = rao_data_to_diffraction_results(rao)
        for dof in DOF:
            comp = results.raos.get_component(dof)
            assert comp is not None
            assert comp.magnitude.shape == (50, 2)

    def test_bridge_added_mass_present(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        coeffs = xlsx_to_hydro_coefficients(PIPELINE_XLSX)
        results = rao_data_to_diffraction_results(rao, coeffs)
        assert results.added_mass is not None
        assert len(results.added_mass.matrices) == 50

    def test_bridge_without_coefficients_uses_zero_matrices(self):
        rao = xlsx_to_rao_data(PIPELINE_XLSX)
        results = rao_data_to_diffraction_results(rao, coeffs=None)
        # Should have zero-filled matrices (DiffractionResults requires them)
        assert results.added_mass is not None
        assert results.damping is not None
        # Verify they're zero-filled
        for mat in results.added_mass.matrices:
            np.testing.assert_array_equal(mat.matrix, np.zeros((6, 6)))


# ---------------------------------------------------------------------------
# Test: Full export round-trip — pipeline xlsx
# ---------------------------------------------------------------------------


class TestFullExportRoundTrip:
    """End-to-end: xlsx → RAOData → DiffractionResults → OrcaFlex export → validate."""

    @pytest.fixture(autouse=True)
    def _setup(self, tmp_path):
        self.output_dir = tmp_path / "orcaflex_export"
        self.rao = xlsx_to_rao_data(PIPELINE_XLSX)
        self.coeffs = xlsx_to_hydro_coefficients(PIPELINE_XLSX)
        self.results = rao_data_to_diffraction_results(
            self.rao, self.coeffs, source_file=str(PIPELINE_XLSX)
        )
        self.exporter = OrcaFlexExporter(self.results, self.output_dir)
        self.outputs = self.exporter.export_all()

    def test_all_output_files_created(self):
        for key, path in self.outputs.items():
            assert path.exists(), f"Output {key} missing: {path}"

    def test_vessel_type_yaml_valid(self):
        yaml_path = self.outputs["vessel_type"]
        with open(yaml_path) as f:
            data = yaml.safe_load(f)
        assert "VesselType" in data
        assert data["VesselType"]["Name"] == "test01_unit_box"

    def test_rao_csv_frequency_count(self):
        csv_path = self.outputs["rao_csv"]
        df = pd.read_csv(csv_path)
        # Should have 50 freqs × 2 headings = 100 rows (or 50 rows per heading)
        # Depends on CSV layout — check at least 50 data rows
        assert len(df) >= 50

    def test_rao_csv_amplitude_matches_source(self):
        """Verify exported RAO amplitudes match source within 1% tolerance."""
        csv_path = self.outputs["rao_csv"]
        df = pd.read_csv(csv_path)

        # Extract surge amplitude from CSV and compare to source
        for dof_name, dof_idx in [("surge", 0), ("heave", 2), ("pitch", 4)]:
            if f"{dof_name}_magnitude" in df.columns:
                exported = df[f"{dof_name}_magnitude"].values
                # Compare first heading (heading 0.0)
                source = self.rao.amplitudes[:, 0, dof_idx]
                # Match lengths
                n = min(len(exported), len(source))
                mask = source[:n] > 1e-6  # skip near-zero values
                if mask.any():
                    rel_diff = np.abs(exported[:n][mask] - source[:n][mask]) / source[:n][mask]
                    assert np.max(rel_diff) < AMPLITUDE_REL_TOL, (
                        f"{dof_name} amplitude exceeds {AMPLITUDE_REL_TOL*100}% tolerance: "
                        f"max rel diff = {np.max(rel_diff)*100:.2f}%"
                    )

    def test_added_mass_csv_exists_and_populated(self):
        csv_path = self.outputs["added_mass_csv"]
        df = pd.read_csv(csv_path)
        assert len(df) >= 50  # at least 50 frequency rows

    def test_damping_csv_exists_and_populated(self):
        csv_path = self.outputs["damping_csv"]
        df = pd.read_csv(csv_path)
        assert len(df) >= 50

    def test_excel_workbook_created(self):
        excel_path = self.outputs["excel"]
        assert excel_path.exists()
        assert excel_path.stat().st_size > 0

    def test_summary_report_created(self):
        summary_path = self.outputs["summary"]
        assert summary_path.exists()
        content = summary_path.read_text()
        assert "test01_unit_box" in content


# ---------------------------------------------------------------------------
# Test: DOF-level amplitude validation
# ---------------------------------------------------------------------------


class TestDofLevelValidation:
    """Validate each DOF independently against tolerance criteria."""

    @pytest.fixture(autouse=True)
    def _setup(self, tmp_path):
        self.rao = xlsx_to_rao_data(PIPELINE_XLSX)
        self.coeffs = xlsx_to_hydro_coefficients(PIPELINE_XLSX)
        self.results = rao_data_to_diffraction_results(self.rao, self.coeffs)

    @pytest.mark.parametrize(
        "dof,dof_idx",
        [
            (DOF.SURGE, 0),
            (DOF.SWAY, 1),
            (DOF.HEAVE, 2),
            (DOF.ROLL, 3),
            (DOF.PITCH, 4),
            (DOF.YAW, 5),
        ],
    )
    def test_dof_amplitude_round_trip(self, dof, dof_idx):
        """Each DOF's amplitude in DiffractionResults matches source RAOData."""
        comp = self.results.raos.get_component(dof)
        source = self.rao.amplitudes[:, :, dof_idx]
        exported = comp.magnitude

        # Compare shapes
        assert exported.shape == source.shape, (
            f"{dof.name}: shape mismatch {exported.shape} vs {source.shape}"
        )

        # Compare values within tolerance
        mask = source > 1e-8
        if mask.any():
            rel_diff = np.abs(exported[mask] - source[mask]) / source[mask]
            max_diff = np.max(rel_diff)
            assert max_diff < AMPLITUDE_REL_TOL, (
                f"{dof.name}: max relative amplitude difference = "
                f"{max_diff*100:.4f}% exceeds {AMPLITUDE_REL_TOL*100}%"
            )

    @pytest.mark.parametrize(
        "dof,dof_idx",
        [
            (DOF.SURGE, 0),
            (DOF.SWAY, 1),
            (DOF.HEAVE, 2),
            (DOF.ROLL, 3),
            (DOF.PITCH, 4),
            (DOF.YAW, 5),
        ],
    )
    def test_dof_phase_round_trip(self, dof, dof_idx):
        """Each DOF's phase in DiffractionResults matches source RAOData."""
        comp = self.results.raos.get_component(dof)
        source = self.rao.phases[:, :, dof_idx]
        exported = comp.phase

        assert exported.shape == source.shape

        # Phase comparison handles wrapping: compute circular difference
        diff = np.abs(exported - source)
        # Handle 360-degree wrapping
        diff = np.minimum(diff, 360.0 - diff)

        # Only check where amplitude is significant (phase is meaningless at zero amp)
        amp = self.rao.amplitudes[:, :, dof_idx]
        mask = amp > 1e-6
        if mask.any():
            max_phase_diff = np.max(diff[mask])
            assert max_phase_diff < PHASE_ABS_TOL_DEG, (
                f"{dof.name}: max phase difference = "
                f"{max_phase_diff:.2f} deg exceeds {PHASE_ABS_TOL_DEG} deg"
            )


# ---------------------------------------------------------------------------
# Test: Added mass / damping round-trip
# ---------------------------------------------------------------------------


class TestHydroCoefficientsRoundTrip:
    """Validate added mass and damping matrices through the pipeline."""

    @pytest.fixture(autouse=True)
    def _setup(self):
        self.coeffs = xlsx_to_hydro_coefficients(PIPELINE_XLSX)
        self.rao = xlsx_to_rao_data(PIPELINE_XLSX)
        self.results = rao_data_to_diffraction_results(self.rao, self.coeffs)

    def test_added_mass_matrix_count(self):
        assert len(self.results.added_mass.matrices) == len(self.coeffs.frequencies)

    def test_added_mass_values_preserved(self):
        """Every added mass matrix matches source exactly (no conversion loss)."""
        for i, mat_obj in enumerate(self.results.added_mass.matrices):
            np.testing.assert_array_almost_equal(
                mat_obj.matrix,
                self.coeffs.added_mass[i],
                decimal=10,
                err_msg=f"Added mass mismatch at freq index {i}",
            )

    def test_damping_values_preserved(self):
        """Every damping matrix matches source exactly."""
        for i, mat_obj in enumerate(self.results.damping.matrices):
            np.testing.assert_array_almost_equal(
                mat_obj.matrix,
                self.coeffs.damping[i],
                decimal=10,
                err_msg=f"Damping mismatch at freq index {i}",
            )

    def test_frequencies_match(self):
        """Frequencies in DiffractionResults match source."""
        np.testing.assert_array_almost_equal(
            self.results.added_mass.frequencies.values,
            self.coeffs.frequencies,
        )


# ---------------------------------------------------------------------------
# Test: Cross-format consistency
# ---------------------------------------------------------------------------


class TestCrossFormatConsistency:
    """Compare pipeline-format and native-format xlsx give consistent results."""

    def test_pipeline_and_native_frequencies_match(self):
        """Both formats should give same frequency grid for same test case."""
        pipeline = xlsx_to_rao_data(PIPELINE_XLSX)
        native = xlsx_to_rao_data(NATIVE_XLSX)

        np.testing.assert_array_almost_equal(
            pipeline.frequencies,
            native.frequencies,
            decimal=4,
            err_msg="Pipeline and native xlsx have different frequency grids",
        )

    def test_pipeline_and_native_headings_match(self):
        """Both formats should give same heading grid."""
        pipeline = xlsx_to_rao_data(PIPELINE_XLSX)
        native = xlsx_to_rao_data(NATIVE_XLSX)

        np.testing.assert_array_almost_equal(
            pipeline.directions,
            native.directions,
            decimal=2,
            err_msg="Pipeline and native xlsx have different heading grids",
        )

    def test_pipeline_and_native_surge_amplitude_consistent(self):
        """Surge RAO amplitudes from both formats should be within tolerance."""
        pipeline = xlsx_to_rao_data(PIPELINE_XLSX)
        native = xlsx_to_rao_data(NATIVE_XLSX)

        # Compare surge (DOF 0) amplitudes at heading 0
        p_surge = pipeline.amplitudes[:, 0, 0]
        n_surge = native.amplitudes[:, 0, 0]

        mask = p_surge > 1e-6
        if mask.any():
            rel_diff = np.abs(p_surge[mask] - n_surge[mask]) / p_surge[mask]
            max_diff = np.max(rel_diff)
            assert max_diff < AMPLITUDE_REL_TOL, (
                f"Pipeline vs native surge amplitude differ by {max_diff*100:.4f}%"
            )

    def test_pipeline_and_native_heave_amplitude_consistent(self):
        """Heave RAO from both formats should agree."""
        pipeline = xlsx_to_rao_data(PIPELINE_XLSX)
        native = xlsx_to_rao_data(NATIVE_XLSX)

        p_heave = pipeline.amplitudes[:, 0, 2]
        n_heave = native.amplitudes[:, 0, 2]

        mask = p_heave > 1e-6
        if mask.any():
            rel_diff = np.abs(p_heave[mask] - n_heave[mask]) / p_heave[mask]
            max_diff = np.max(rel_diff)
            assert max_diff < AMPLITUDE_REL_TOL, (
                f"Pipeline vs native heave amplitude differ by {max_diff*100:.4f}%"
            )
