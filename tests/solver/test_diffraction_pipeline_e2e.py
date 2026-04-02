"""End-to-end integration test for the DiffractionSpec pipeline.

Pipeline: spec.yml -> DiffractionSpec -> OrcaWaveBackend -> native solver YAML

Tests use real L00 spec.yml fixture data but mock the actual solver call.
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
L00_2_1_SPEC = L00_DIR / "2.1" / "spec.yml"  # Simple cylinder case


@pytest.fixture
def l00_spec_path() -> Path:
    """Return path to L00 case 2.1 spec.yml."""
    assert L00_2_1_SPEC.exists(), f"Fixture not found: {L00_2_1_SPEC}"
    return L00_2_1_SPEC


@pytest.fixture
def l00_spec(l00_spec_path) -> DiffractionSpec:
    """Load and return a validated DiffractionSpec from L00 case 2.1."""
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
        assert l00_spec.vessel.name == "Test01_cylinder"

    def test_spec_has_environment(self, l00_spec):
        assert l00_spec.environment is not None
        assert l00_spec.environment.water_depth == "infinite"
        assert l00_spec.environment.water_density == 1000.0

    def test_spec_has_frequencies(self, l00_spec):
        freqs = l00_spec.frequencies.to_frequencies_rad_s()
        assert len(freqs) == 50  # 0.1 to 5.0 in steps of 0.1

    def test_spec_has_headings(self, l00_spec):
        headings = l00_spec.wave_headings.to_heading_list()
        assert headings == [0.0, 27.0]

    def test_spec_metadata_present(self, l00_spec):
        assert l00_spec.metadata is not None
        assert "validation" in (l00_spec.metadata.tags or [])


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
        assert name == "Test01_cylinder"


# ---------------------------------------------------------------------------
# Test 3: Native YAML generation
# ---------------------------------------------------------------------------


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
        assert headings == [0.0, 27.0]

    def test_single_yml_body_name(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        bodies = data.get("Bodies", [])
        assert len(bodies) == 1
        assert bodies[0]["BodyName"] == "Test01_cylinder"

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
        # L00 case 2.1 has infinite depth
        assert data["WaterDepth"] == "Infinity"

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
        assert "val_cylinder_r1_t05.gdf" in body["BodyMeshFileName"]

    def test_symmetry_preserved(self, l00_spec, backend, tmp_path):
        output = backend.generate_single(l00_spec, tmp_path)
        with open(output) as f:
            data = yaml.safe_load(f)
        body = data["Bodies"][0]
        assert body["BodyMeshSymmetry"] == "xz and yz planes"
