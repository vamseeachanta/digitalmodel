"""Tests for OrcaWave input backend (WRK-059).

TDD: These tests define the expected behavior of the OrcaWaveBackend
that converts a DiffractionSpec into OrcaWave .yml project files.

Covers:
- Single-file output mode
- Modular (include) output mode
- Correct OrcaWave YAML structure and field mapping
- Period-to-frequency conversion
- Multi-body generation
- Body property mapping (mass, inertia, mesh)
"""

import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.diffraction.input_schemas import (
    DiffractionSpec,
    FrequencyInputType,
)

FIXTURES_DIR = Path(__file__).parent / "fixtures"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _load_orcawave_yml(path: Path) -> dict:
    """Load an OrcaWave YAML file and return as dict."""
    with open(path) as f:
        return yaml.safe_load(f)


def _ship_spec() -> DiffractionSpec:
    """Load the ship RAO spec fixture."""
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_ship_raos.yml")


def _semisub_spec() -> DiffractionSpec:
    """Load the semi-sub spec fixture."""
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_semisub.yml")


def _multibody_spec() -> DiffractionSpec:
    """Load the multi-body FPSO turret spec fixture."""
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_fpso_turret.yml")


# ---------------------------------------------------------------------------
# Tests: Single-file generation
# ---------------------------------------------------------------------------


class TestOrcaWaveBackendSingleMode:
    """Test single .yml file generation from DiffractionSpec."""

    def test_generates_single_yml_file(self, tmp_path: Path):
        """generate_single creates exactly one .yml file."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)

        assert result.exists()
        assert result.suffix == ".yml"

    def test_single_yml_contains_required_sections(self, tmp_path: Path):
        """Single output contains all required OrcaWave top-level keys."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        required_keys = [
            "UnitsSystem",
            "SolveType",
            "WaterDepth",
            "WaterDensity",
            "PeriodOrFrequency",
            "WaveHeading",
            "Bodies",
        ]
        for key in required_keys:
            assert key in data, f"Missing required key: {key}"

    def test_units_system_is_si(self, tmp_path: Path):
        """OrcaWave output uses SI units."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["UnitsSystem"] == "SI"

    def test_environment_mapping(self, tmp_path: Path):
        """Environment section maps water depth and density correctly."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["WaterDepth"] == 500.0
        # OrcaWave density in SI uses tonnes/m^3 (1.025)
        assert data["WaterDensity"] == 1.025

    def test_wave_headings_mapped(self, tmp_path: Path):
        """Wave headings list is correctly transcribed."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["WaveHeading"] == [0.0, 45.0, 90.0, 135.0, 180.0]

    def test_solve_type_set(self, tmp_path: Path):
        """SolveType is set to potential and source formulations."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["SolveType"] == "Potential and source formulations"

    def test_load_rao_method_mapping(self, tmp_path: Path):
        """LoadRAOCalculationMethod maps correctly from spec."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["LoadRAOCalculationMethod"] == "Both"


# ---------------------------------------------------------------------------
# Tests: Body property mapping
# ---------------------------------------------------------------------------


class TestBodyPropertyMapping:
    """Test that body properties from DiffractionSpec map to OrcaWave bodies."""

    def test_single_body_count(self, tmp_path: Path):
        """Single-vessel spec produces exactly one body."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert len(data["Bodies"]) == 1

    def test_body_name(self, tmp_path: Path):
        """Body name taken from vessel spec."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["Bodies"][0]["BodyName"] == "Ship_001"

    def test_body_mass_in_tonnes(self, tmp_path: Path):
        """BodyMass is in tonnes for SI (spec is in kg)."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        # spec mass is 84387300.0 kg -> 84387.3 tonnes
        expected_mass = 84387300.0 / 1000.0
        assert abs(data["Bodies"][0]["BodyMass"] - expected_mass) < 0.01

    def test_body_centre_of_mass(self, tmp_path: Path):
        """BodyCentreOfMass maps from vessel COG."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["Bodies"][0]["BodyCentreOfMass"] == [0.0, 0.0, -1.5]

    def test_body_mesh_file_reference(self, tmp_path: Path):
        """BodyMeshFileName references the mesh file from spec."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        # Should reference the mesh file (basename for portability)
        body = data["Bodies"][0]
        assert "BodyMeshFileName" in body
        assert body["BodyMeshFileName"] == "sample_box.gdf"

    def test_body_mesh_format_mapping(self, tmp_path: Path):
        """Mesh format maps to OrcaWave conventions."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        body = data["Bodies"][0]
        assert body["BodyMeshFormat"] == "Wamit gdf"

    def test_body_mesh_symmetry_mapping(self, tmp_path: Path):
        """Mesh symmetry maps to OrcaWave symmetry convention."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        body = data["Bodies"][0]
        # Ship spec has symmetry: xz
        assert body["BodyMeshSymmetry"] == "xz plane"

    def test_body_inertia_from_radii_of_gyration(self, tmp_path: Path):
        """Inertia tensor built from mass and radii of gyration."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        body = data["Bodies"][0]
        inertia_key = (
            "BodyInertiaTensorRx, "
            "BodyInertiaTensorRy, "
            "BodyInertiaTensorRz"
        )
        assert inertia_key in body

        # mass = 84387300 kg = 84387.3 t
        # radii = [15, 50, 50]
        # Ixx = m * kxx^2 = 84387.3 * 15^2 = 84387.3 * 225 = 18987142.5 t.m^2
        # In OrcaWave SI: mass in tonnes, inertia in t.m^2
        mass_t = 84387300.0 / 1000.0
        expected_ixx = mass_t * 15.0**2
        expected_iyy = mass_t * 50.0**2
        expected_izz = mass_t * 50.0**2

        tensor = body[inertia_key]
        assert len(tensor) == 3  # 3x3 matrix
        assert abs(tensor[0][0] - expected_ixx) < 1.0
        assert abs(tensor[1][1] - expected_iyy) < 1.0
        assert abs(tensor[2][2] - expected_izz) < 1.0
        # Off-diagonal should be 0
        assert tensor[0][1] == 0
        assert tensor[0][2] == 0
        assert tensor[1][0] == 0

    def test_body_inertia_from_tensor(self, tmp_path: Path):
        """Inertia tensor from explicit tensor dict."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _semisub_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        body = data["Bodies"][0]
        inertia_key = (
            "BodyInertiaTensorRx, "
            "BodyInertiaTensorRy, "
            "BodyInertiaTensorRz"
        )
        assert inertia_key in body

        tensor = body[inertia_key]
        # Ixx = 6.827e9 kg.m^2 -> 6.827e6 t.m^2
        assert abs(tensor[0][0] - 6.827e6) < 1.0
        assert abs(tensor[1][1] - 6.827e6) < 1.0
        assert abs(tensor[2][2] - 1.226e7) < 1.0

    def test_body_connection_parent_free(self, tmp_path: Path):
        """Single vessel body has connection parent = Free."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["Bodies"][0]["BodyConnectionParent"] == "Free"

    def test_body_included_in_analysis(self, tmp_path: Path):
        """Each body has BodyIncludedInAnalysis = Yes.

        Note: YAML 1.1 maps Yes/No to bool on load, so we check True/False.
        The actual .yml file written contains unquoted Yes/No for OrcaWave.
        """
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["Bodies"][0]["BodyIncludedInAnalysis"] is True

    def test_body_fixed_dofs_mapping(self, tmp_path: Path):
        """Fixed DOFs from spec map to BodyFixedDOF fields.

        Note: YAML 1.1 maps Yes/No to bool on load.
        """
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _multibody_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        # Turret body has fixed_dofs: [surge, sway, yaw]
        turret_body = data["Bodies"][1]
        assert turret_body["BodyFixedDOFx"] is True  # surge
        assert turret_body["BodyFixedDOFy"] is True  # sway
        assert turret_body["BodyFixedDOFRz"] is True  # yaw
        # Others should be No (False)
        assert turret_body["BodyFixedDOFz"] is False
        assert turret_body["BodyFixedDOFRx"] is False
        assert turret_body["BodyFixedDOFRy"] is False


# ---------------------------------------------------------------------------
# Tests: Frequency handling
# ---------------------------------------------------------------------------


class TestFrequencyMapping:
    """Test period/frequency conversion to OrcaWave format."""

    def test_frequencies_as_periods_in_orcawave(self, tmp_path: Path):
        """OrcaWave always uses periods; freq values must be converted."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["WavesReferredToBy"] == "period (s)"

        # Input is frequencies [0.1 .. 1.5] rad/s
        # Should convert to periods: T = 2*pi / omega
        # Sorted ascending: smallest period first (highest frequency)
        periods = data["PeriodOrFrequency"]
        assert len(periods) == 15

        # Highest freq 1.5 rad/s -> smallest period 4.19 s (first)
        expected_smallest = round(2.0 * math.pi / 1.5, 4)
        assert abs(periods[0] - expected_smallest) < 0.01

        # Lowest freq 0.1 rad/s -> largest period 62.83 s (last)
        expected_largest = round(2.0 * math.pi / 0.1, 4)
        assert abs(periods[-1] - expected_largest) < 0.01

    def test_period_input_stays_as_periods(self, tmp_path: Path):
        """If input is already periods, they pass through."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _semisub_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["WavesReferredToBy"] == "period (s)"
        periods = data["PeriodOrFrequency"]
        assert len(periods) == 50
        # First period should be close to 3.0 s
        assert abs(periods[0] - 3.0) < 0.1

    def test_periods_sorted_ascending(self, tmp_path: Path):
        """OrcaWave expects periods sorted in ascending order."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        periods = data["PeriodOrFrequency"]
        assert periods == sorted(periods)


# ---------------------------------------------------------------------------
# Tests: Multi-body
# ---------------------------------------------------------------------------


class TestMultiBodyGeneration:
    """Test multi-body spec generates correct body count and relationships."""

    def test_multi_body_count(self, tmp_path: Path):
        """Multi-body spec produces correct number of bodies."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _multibody_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert len(data["Bodies"]) == 2

    def test_multi_body_names(self, tmp_path: Path):
        """Multi-body body names match vessel names."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _multibody_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["Bodies"][0]["BodyName"] == "FPSO_Hull"
        assert data["Bodies"][1]["BodyName"] == "Turret"

    def test_multi_body_connection_parent(self, tmp_path: Path):
        """Connection parent maps from spec."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _multibody_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["Bodies"][0]["BodyConnectionParent"] == "Free"
        assert data["Bodies"][1]["BodyConnectionParent"] == "FPSO_Hull"

    def test_multi_body_position(self, tmp_path: Path):
        """Body mesh position maps from spec body position."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _multibody_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["Bodies"][0]["BodyMeshPosition"] == [0.0, 0.0, 0.0]
        assert data["Bodies"][1]["BodyMeshPosition"] == [100.0, 0.0, 0.0]

    def test_infinite_water_depth(self, tmp_path: Path):
        """Infinite water depth mapped to OrcaWave convention."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _multibody_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        # OrcaWave uses "Infinity" for infinite water depth
        assert data["WaterDepth"] == "Infinity"


# ---------------------------------------------------------------------------
# Tests: Solver settings
# ---------------------------------------------------------------------------


class TestSolverSettingsMapping:
    """Test solver options map to OrcaWave settings."""

    def test_remove_irregular_frequencies(self, tmp_path: Path):
        """remove_irregular_frequencies maps to interior surface panels.

        Note: YAML 1.1 maps Yes/No to bool on load.
        """
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        body = data["Bodies"][0]
        assert body["BodyAddInteriorSurfacePanels"] is True

    def test_qtf_settings_when_disabled(self, tmp_path: Path):
        """QTF fields present but disabled when qtf_calculation=False."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["QuadraticLoadPressureIntegration"] is False

    def test_qtf_settings_when_enabled(self, tmp_path: Path):
        """QTF fields enabled when qtf_calculation=True."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _semisub_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_single(spec, tmp_path)
        data = _load_orcawave_yml(result)

        assert data["QuadraticLoadPressureIntegration"] is True


# ---------------------------------------------------------------------------
# Tests: Modular output mode
# ---------------------------------------------------------------------------


class TestOrcaWaveBackendModularMode:
    """Test modular (include) file generation."""

    def test_modular_generates_master_file(self, tmp_path: Path):
        """generate_modular creates a master.yml."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        result = backend.generate_modular(spec, tmp_path)

        assert result.exists()
        assert result.name == "master.yml"

    def test_modular_generates_section_files(self, tmp_path: Path):
        """generate_modular creates separate section files."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        backend.generate_modular(spec, tmp_path)

        expected_files = [
            "01_general.yml",
            "02_units.yml",
            "03_environment.yml",
            "04_bodies.yml",
            "05_frequencies.yml",
            "06_headings.yml",
            "07_solver.yml",
            "08_outputs.yml",
        ]
        for fname in expected_files:
            fpath = tmp_path / fname
            assert fpath.exists(), f"Missing modular file: {fname}"

    def test_modular_environment_file_content(self, tmp_path: Path):
        """Environment modular file has correct water depth and density."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        backend.generate_modular(spec, tmp_path)

        env_data = _load_orcawave_yml(tmp_path / "03_environment.yml")
        assert env_data["WaterDepth"] == 500.0
        assert env_data["WaterDensity"] == 1.025

    def test_modular_bodies_file_content(self, tmp_path: Path):
        """Bodies modular file has correct body structure."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        backend.generate_modular(spec, tmp_path)

        bodies_data = _load_orcawave_yml(tmp_path / "04_bodies.yml")
        assert "Bodies" in bodies_data
        assert len(bodies_data["Bodies"]) == 1
        assert bodies_data["Bodies"][0]["BodyName"] == "Ship_001"

    def test_modular_frequencies_file_content(self, tmp_path: Path):
        """Frequencies modular file has period list."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        backend.generate_modular(spec, tmp_path)

        freq_data = _load_orcawave_yml(tmp_path / "05_frequencies.yml")
        assert "PeriodOrFrequency" in freq_data
        assert "WavesReferredToBy" in freq_data
        assert len(freq_data["PeriodOrFrequency"]) == 15

    def test_modular_headings_file_content(self, tmp_path: Path):
        """Headings modular file has correct heading list."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        backend.generate_modular(spec, tmp_path)

        heading_data = _load_orcawave_yml(tmp_path / "06_headings.yml")
        assert "WaveHeading" in heading_data
        assert heading_data["WaveHeading"] == [0.0, 45.0, 90.0, 135.0, 180.0]

    def test_modular_all_files_are_valid_yaml(self, tmp_path: Path):
        """All generated modular files are valid YAML."""
        from digitalmodel.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        spec = _ship_spec()
        backend = OrcaWaveBackend()
        backend.generate_modular(spec, tmp_path)

        for yml_file in tmp_path.glob("*.yml"):
            with open(yml_file) as f:
                data = yaml.safe_load(f)
            assert data is not None, f"Empty YAML: {yml_file.name}"
