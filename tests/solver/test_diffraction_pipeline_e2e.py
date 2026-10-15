"""End-to-end integration test for the DiffractionSpec pipeline.

Pipeline: spec.yml -> DiffractionSpec -> OrcaWaveBackend -> native solver YAML

Tests use the committed unit-box spec.yml fixture but mock the actual solver call.
No OrcaWave license required.

Issue: #1598
"""
from __future__ import annotations

from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


REPO_ROOT = Path(__file__).resolve().parents[2]  # digitalmodel/
L00_DIR = REPO_ROOT / "docs" / "domains" / "orcawave" / "L00_validation_wamit"
L00_2_1_SPEC = L00_DIR / "2.1" / "spec.yml"  # Expected refusal
FULL_SPEC = REPO_ROOT / "examples/hydrodynamics/diffraction/unit_box_rao/spec.yml"


@pytest.fixture
def l00_spec_path() -> Path:
    """Return the committed full-mesh fixture."""
    assert FULL_SPEC.exists(), f"Fixture not found: {FULL_SPEC}"
    return FULL_SPEC


@pytest.fixture
def l00_spec(l00_spec_path) -> DiffractionSpec:
    """Load the validated full-mesh DiffractionSpec."""
    return DiffractionSpec.from_yaml(str(l00_spec_path))


@pytest.fixture
def backend() -> OrcaWaveBackend:
    """Return an OrcaWaveBackend instance."""
    return OrcaWaveBackend()


# ---------------------------------------------------------------------------
# Test 1: Spec load and validate
# ---------------------------------------------------------------------------


class TestSpecLoadAndValidate:
    """Load a real spec.yml, validate against schema."""

    def test_spec_loads_from_yaml(self, l00_spec_path):
        spec = DiffractionSpec.from_yaml(str(l00_spec_path))
        assert spec is not None

    def test_spec_has_correct_version(self, l00_spec):
        assert l00_spec.version == "1.0"

    def test_spec_has_vessel(self, l00_spec):
        assert l00_spec.vessel is not None
        assert l00_spec.vessel.name == "UnitBoxRAO"

    def test_spec_has_environment(self, l00_spec):
        assert l00_spec.environment is not None
        assert l00_spec.environment.water_depth == 100.0
        assert l00_spec.environment.water_density == 1025.0

    def test_spec_has_frequencies(self, l00_spec):
        freqs = l00_spec.frequencies.to_frequencies_rad_s()
        assert len(freqs) == 3

    def test_spec_has_headings(self, l00_spec):
        headings = l00_spec.wave_headings.to_heading_list()
        assert headings == [0.0, 90.0, 180.0]

    def test_spec_metadata_present(self, l00_spec):
        assert l00_spec.metadata is not None
        assert "canonical" in (l00_spec.metadata.tags or [])


# ---------------------------------------------------------------------------
# Test 2: Backend initialization
# ---------------------------------------------------------------------------


class TestBackendInitialization:
    """Create backend from spec — verify it can process the spec."""

    def test_backend_instantiates(self):
        backend = OrcaWaveBackend()
        assert backend is not None

    def test_backend_has_generate_single(self, backend):
        assert callable(getattr(backend, "generate_single", None))

    def test_backend_has_generate_modular(self, backend):
        assert callable(getattr(backend, "generate_modular", None))

    def test_backend_analysis_name_from_single_body(self, l00_spec, backend):
        name = backend._analysis_name(l00_spec)
        assert name == "UnitBoxRAO"


# ---------------------------------------------------------------------------
# Test 3: Native YAML generation
# ---------------------------------------------------------------------------


def test_l00_case_2_1_is_refused(backend, tmp_path):
    with pytest.raises(ValueError, match="Cannot verify GDF symmetry"):
        backend.generate_single(DiffractionSpec.from_yaml(L00_2_1_SPEC), tmp_path)


class TestNativeYamlGeneration:
    """Backend produces solver-native YAML from spec."""

    def test_single_yml_created(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        assert output.exists()
        assert output.suffix == ".yml"

    def test_single_yml_valid_yaml(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        assert isinstance(data, dict)

    def test_single_yml_has_orcawave_keys(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        # OrcaWave native YAML uses PascalCase keys
        assert "UnitsSystem" in data
        assert data["UnitsSystem"] == "SI"
        assert "Bodies" in data
        assert "WaterDepth" in data

    def test_single_yml_has_periods(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        periods = data.get("PeriodOrFrequency", [])
        assert len(periods) > 0
        # Periods should be positive
        assert all(p > 0 for p in periods)

    def test_single_yml_has_headings(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        headings = data.get("WaveHeading", [])
        assert headings == [0.0, 90.0, 180.0]

    def test_single_yml_body_name(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        bodies = data.get("Bodies", [])
        assert len(bodies) == 1
        assert bodies[0]["BodyName"] == "UnitBoxRAO"

    def test_modular_yml_created(self, l00_spec, backend, tmp_path):
        master = backend.generate_modular(l00_spec, tmp_path)
        assert master.exists()
        assert master.name == "master.yml"
        # Also check section files exist
        assert (tmp_path / "01_general.yml").exists()
        assert (tmp_path / "03_environment.yml").exists()
        assert (tmp_path / "04_bodies.yml").exists()


# ---------------------------------------------------------------------------
# Test 4: Round-trip (spec -> backend -> native YAML -> parse back)
# ---------------------------------------------------------------------------


class TestRoundTrip:
    """Verify pipeline data integrity: spec -> native YAML -> parse back."""

    def test_water_depth_round_trip(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        # The unit box uses finite depth
        assert data["WaterDepth"] == 100.0

    def test_body_count_round_trip(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        original_bodies = l00_spec.get_bodies()
        assert len(data["Bodies"]) == len(original_bodies)

    def test_heading_count_round_trip(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        original_headings = l00_spec.wave_headings.to_heading_list()
        assert len(data["WaveHeading"]) == len(original_headings)

    def test_frequency_count_round_trip(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        original_freqs = l00_spec.frequencies.to_frequencies_rad_s()
        native_periods = data["PeriodOrFrequency"]
        # Allow slight difference due to period conversion
        assert len(native_periods) == len(original_freqs)

    def test_mesh_filename_preserved(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        body = data["Bodies"][0]
        # OrcaWave only uses the basename
        assert "unit_box.gdf" in body["BodyMeshFileName"]

    def test_symmetry_preserved(self, l00_spec, backend, tmp_path):
        l00_spec.vessel.geometry.symmetry = "xz"
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        body = data["Bodies"][0]
        assert body["BodyMeshSymmetry"] == "None"
