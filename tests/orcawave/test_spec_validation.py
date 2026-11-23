"""Tests for OrcaWave spec.yml validation against DiffractionSpec schema.

Validates that real spec.yml files from the L00 validation cases parse
correctly, and that malformed specs are rejected with clear errors.
"""
from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    AnalysisType,
    DiffractionSpec,
    EnvironmentSpec,
    FrequencyInputType,
    FrequencySpec,
    MeshFormatType,
    SymmetryType,
    VesselGeometry,
    VesselInertia,
    VesselSpec,
    WaveHeadingSpec,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

REPO_ROOT = Path(__file__).resolve().parents[2]  # digitalmodel/
L00_DIR = REPO_ROOT / "docs" / "domains" / "orcawave" / "L00_validation_wamit"

# Known L00 spec.yml files
L00_SPEC_FILES = sorted(L00_DIR.glob("*/spec.yml")) if L00_DIR.exists() else []


def _minimal_spec_data() -> dict:
    """Return the minimal valid DiffractionSpec as a dict."""
    return {
        "version": "1.0",
        "analysis_type": "diffraction",
        "vessel": {
            "name": "TestVessel",
            "geometry": {
                "mesh_file": "test.gdf",
                "mesh_format": "gdf",
            },
            "inertia": {
                "mass": 1000.0,
                "centre_of_gravity": [0.0, 0.0, 0.0],
                "radii_of_gyration": [1.0, 1.0, 1.0],
            },
        },
        "environment": {
            "water_depth": "infinite",
            "water_density": 1025.0,
        },
        "frequencies": {
            "input_type": "frequency",
            "values": [0.1, 0.5, 1.0],
        },
        "wave_headings": {
            "values": [0.0, 180.0],
        },
    }


# ---------------------------------------------------------------------------
# Real spec.yml validation
# ---------------------------------------------------------------------------


class TestRealSpecFiles:
    """Validate real L00 spec.yml files against DiffractionSpec schema."""

    @pytest.mark.parametrize(
        "spec_path",
        L00_SPEC_FILES,
        ids=[p.parent.name for p in L00_SPEC_FILES],
    )
    def test_l00_spec_parses(self, spec_path: Path):
        """Each L00 spec.yml should load and validate without error."""
        spec = DiffractionSpec.from_yaml(str(spec_path))
        assert spec.version == "1.0"
        assert spec.analysis_type in (
            AnalysisType.DIFFRACTION,
            AnalysisType.FULL_QTF,
            AnalysisType.RADIATION,
            AnalysisType.FREQUENCY_DOMAIN,
            AnalysisType.TIME_DOMAIN,
        )

    @pytest.mark.parametrize(
        "spec_path",
        L00_SPEC_FILES,
        ids=[p.parent.name for p in L00_SPEC_FILES],
    )
    def test_l00_spec_has_vessel(self, spec_path: Path):
        """Each L00 spec.yml should define a vessel."""
        spec = DiffractionSpec.from_yaml(str(spec_path))
        bodies = spec.get_bodies()
        assert len(bodies) >= 1
        assert bodies[0].vessel.name

    @pytest.mark.parametrize(
        "spec_path",
        L00_SPEC_FILES,
        ids=[p.parent.name for p in L00_SPEC_FILES],
    )
    def test_l00_spec_has_frequencies(self, spec_path: Path):
        """Each L00 spec.yml should define at least one frequency."""
        spec = DiffractionSpec.from_yaml(str(spec_path))
        freqs = spec.frequencies.to_frequencies_rad_s()
        assert len(freqs) > 0

    @pytest.mark.parametrize(
        "spec_path",
        L00_SPEC_FILES,
        ids=[p.parent.name for p in L00_SPEC_FILES],
    )
    def test_l00_spec_has_headings(self, spec_path: Path):
        """Each L00 spec.yml should define at least one heading."""
        spec = DiffractionSpec.from_yaml(str(spec_path))
        headings = spec.wave_headings.to_heading_list()
        assert len(headings) > 0


# ---------------------------------------------------------------------------
# Programmatic spec construction
# ---------------------------------------------------------------------------


class TestMinimalSpec:
    """Tests for constructing DiffractionSpec from dicts."""

    def test_minimal_spec_valid(self):
        data = _minimal_spec_data()
        spec = DiffractionSpec(**data)
        assert spec.vessel.name == "TestVessel"
        assert spec.environment.water_depth == "infinite"

    def test_get_bodies_wraps_single_vessel(self):
        data = _minimal_spec_data()
        spec = DiffractionSpec(**data)
        bodies = spec.get_bodies()
        assert len(bodies) == 1
        assert bodies[0].vessel.name == "TestVessel"

    def test_missing_vessel_and_bodies_rejected(self):
        data = _minimal_spec_data()
        del data["vessel"]
        with pytest.raises(Exception):
            DiffractionSpec(**data)

    def test_both_vessel_and_bodies_rejected(self):
        data = _minimal_spec_data()
        data["bodies"] = [
            {
                "vessel": data["vessel"],
                "position": [0, 0, 0],
                "attitude": [0, 0, 0],
            }
        ]
        with pytest.raises(Exception):
            DiffractionSpec(**data)


# ---------------------------------------------------------------------------
# Invalid spec rejection
# ---------------------------------------------------------------------------


class TestInvalidSpecs:
    """Tests ensuring malformed specs are rejected."""

    def test_negative_water_depth_rejected(self):
        data = _minimal_spec_data()
        data["environment"]["water_depth"] = -100.0
        with pytest.raises(Exception):
            DiffractionSpec(**data)

    def test_invalid_water_depth_string_rejected(self):
        data = _minimal_spec_data()
        data["environment"]["water_depth"] = "shallow"
        with pytest.raises(Exception):
            DiffractionSpec(**data)

    def test_zero_mass_rejected(self):
        data = _minimal_spec_data()
        data["vessel"]["inertia"]["mass"] = 0.0
        with pytest.raises(Exception):
            DiffractionSpec(**data)

    def test_bad_reference_point_length_rejected(self):
        data = _minimal_spec_data()
        data["vessel"]["geometry"]["reference_point"] = [0.0, 0.0]
        with pytest.raises(Exception):
            DiffractionSpec(**data)

    def test_both_freq_values_and_range_rejected(self):
        data = _minimal_spec_data()
        data["frequencies"]["range"] = {"start": 0.1, "end": 1.0, "count": 10}
        with pytest.raises(Exception):
            DiffractionSpec(**data)

    def test_water_density_out_of_range_rejected(self):
        data = _minimal_spec_data()
        data["environment"]["water_density"] = 50.0  # way too low
        with pytest.raises(Exception):
            DiffractionSpec(**data)


# ---------------------------------------------------------------------------
# YAML round-trip
# ---------------------------------------------------------------------------


class TestYamlRoundTrip:
    """Tests for to_yaml / from_yaml round-tripping."""

    def test_to_yaml_and_back(self, tmp_path):
        data = _minimal_spec_data()
        original = DiffractionSpec(**data)
        yml_path = tmp_path / "roundtrip.yml"
        original.to_yaml(yml_path)
        restored = DiffractionSpec.from_yaml(yml_path)
        assert restored.vessel.name == original.vessel.name
        assert restored.environment.water_depth == original.environment.water_depth
        assert (
            restored.frequencies.to_frequencies_rad_s()
            == original.frequencies.to_frequencies_rad_s()
        )
