"""Tests for reverse parsers: AQWA .dat and OrcaWave .yml to DiffractionSpec.

WRK-063: TDD tests for the reverse parsing direction. These parsers read
solver-specific input files and produce canonical DiffractionSpec objects.

Strategy:
- Use the FORWARD backends to generate solver-specific files from known specs.
- Parse those generated files back with the reverse parsers.
- Compare the round-trip result to the original spec.
"""

import math
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec

FIXTURES_DIR = Path(__file__).parent / "fixtures"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _load_ship_spec() -> DiffractionSpec:
    """Load the ship RAO fixture spec."""
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_ship_raos.yml")


def _load_semisub_spec() -> DiffractionSpec:
    """Load the semi-sub fixture spec."""
    return DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_semisub.yml")


def _generate_aqwa_dat(spec: DiffractionSpec, tmp_path: Path) -> Path:
    """Generate an AQWA .dat file from a spec using the forward backend."""
    from digitalmodel.modules.diffraction.aqwa_backend import AQWABackend

    backend = AQWABackend()
    return backend.generate_single(spec, tmp_path)


def _generate_orcawave_yml(spec: DiffractionSpec, tmp_path: Path) -> Path:
    """Generate an OrcaWave .yml file from a spec using the forward backend."""
    from digitalmodel.modules.diffraction.orcawave_backend import (
        OrcaWaveBackend,
    )

    backend = OrcaWaveBackend()
    return backend.generate_single(spec, tmp_path)


# ---------------------------------------------------------------------------
# AQWA Input Parser tests
# ---------------------------------------------------------------------------


class TestAQWAInputParserBasic:
    """Test that AQWAInputParser produces a valid DiffractionSpec."""

    def test_parse_returns_diffraction_spec(self, tmp_path: Path):
        """Parsing an AQWA .dat file returns a DiffractionSpec instance."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        assert isinstance(result, DiffractionSpec)

    def test_parsed_spec_has_vessel(self, tmp_path: Path):
        """Parsed spec should have a vessel (single-body case)."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        assert result.vessel is not None


class TestAQWAInputParserEnvironment:
    """Test environment extraction from AQWA .dat."""

    def test_water_depth(self, tmp_path: Path):
        """Parsed spec has correct water depth from DPTH card."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        assert result.environment.water_depth == pytest.approx(500.0, rel=1e-3)

    def test_water_density(self, tmp_path: Path):
        """Parsed spec has correct water density from DENS card."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        assert result.environment.water_density == pytest.approx(
            1025.0, rel=1e-3
        )

    def test_gravity(self, tmp_path: Path):
        """Parsed spec has correct gravity from ACCG card."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        assert result.environment.gravity == pytest.approx(9.80665, rel=1e-4)

    def test_deep_water_detection(self, tmp_path: Path):
        """AQWA deep-water depth (>=10000) maps to 'infinite'."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        # Create a spec with infinite water depth, generate .dat, parse back
        spec = _load_ship_spec()
        # Modify to use deep water by creating a modified spec
        spec_data = spec.model_dump(mode="json", exclude_none=True)
        spec_data["environment"]["water_depth"] = "infinite"
        deep_spec = DiffractionSpec.model_validate(spec_data)

        dat_path = _generate_aqwa_dat(deep_spec, tmp_path)
        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        assert result.environment.water_depth == "infinite"


class TestAQWAInputParserFrequencies:
    """Test frequency extraction from AQWA .dat HRTZ cards."""

    def test_frequency_count(self, tmp_path: Path):
        """Parsed spec has correct number of frequencies."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        assert result.frequencies.values is not None
        assert len(result.frequencies.values) == 15

    def test_frequency_values_converted_to_rad_s(self, tmp_path: Path):
        """AQWA stores Hz; parsed spec should have rad/s values."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        original_freqs = spec.frequencies.to_frequencies_rad_s()
        parsed_freqs = result.frequencies.to_frequencies_rad_s()

        for orig, parsed in zip(original_freqs, parsed_freqs):
            assert parsed == pytest.approx(orig, rel=1e-3)


class TestAQWAInputParserHeadings:
    """Test heading extraction from AQWA .dat DIRN cards."""

    def test_heading_count(self, tmp_path: Path):
        """Parsed spec has correct number of headings."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        headings = result.wave_headings.to_heading_list()
        assert len(headings) == 5

    def test_heading_values(self, tmp_path: Path):
        """Parsed headings match original spec values."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        original_headings = spec.wave_headings.to_heading_list()
        parsed_headings = result.wave_headings.to_heading_list()

        for orig, parsed in zip(original_headings, parsed_headings):
            assert parsed == pytest.approx(orig, abs=0.1)


class TestAQWAInputParserMass:
    """Test mass extraction from AQWA .dat deck 3."""

    def test_vessel_mass(self, tmp_path: Path):
        """Parsed spec has correct mass from MATE section."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        assert result.vessel.inertia.mass == pytest.approx(
            84387300.0, rel=1e-3
        )


class TestAQWAInputParserInertia:
    """Test inertia extraction from AQWA .dat deck 4."""

    def test_inertia_tensor_values(self, tmp_path: Path):
        """Parsed spec has correct inertia from PMAS section."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        # Original: mass=84387300, radii=[15, 50, 50]
        # Ixx = 84387300 * 15^2 = 18987142500
        # Iyy = 84387300 * 50^2 = 210968250000
        # Izz = 84387300 * 50^2 = 210968250000
        assert result.vessel.inertia.inertia_tensor is not None
        ixx = result.vessel.inertia.inertia_tensor["Ixx"]
        iyy = result.vessel.inertia.inertia_tensor["Iyy"]
        izz = result.vessel.inertia.inertia_tensor["Izz"]

        expected_ixx = 84387300.0 * 15.0**2
        expected_iyy = 84387300.0 * 50.0**2
        expected_izz = 84387300.0 * 50.0**2

        assert ixx == pytest.approx(expected_ixx, rel=1e-2)
        assert iyy == pytest.approx(expected_iyy, rel=1e-2)
        assert izz == pytest.approx(expected_izz, rel=1e-2)


class TestAQWAInputParserMetadata:
    """Test metadata extraction from AQWA .dat deck 0."""

    def test_project_name_from_title(self, tmp_path: Path):
        """Parsed spec extracts project name from TITLE card."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        spec = _load_ship_spec()
        dat_path = _generate_aqwa_dat(spec, tmp_path)

        parser = AQWAInputParser()
        result = parser.parse(dat_path)

        assert result.metadata.project is not None
        assert "test_ship_raos" in result.metadata.project


class TestAQWAInputParserRoundTrip:
    """Test round-trip: spec -> AQWA .dat -> parse -> compare."""

    def test_round_trip_key_fields(self, tmp_path: Path):
        """Round-trip preserves key fields: depth, density, freq count, heading count."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        original = _load_ship_spec()
        dat_path = _generate_aqwa_dat(original, tmp_path)

        parser = AQWAInputParser()
        parsed = parser.parse(dat_path)

        # Environment
        assert parsed.environment.water_depth == pytest.approx(
            500.0, rel=1e-3
        )
        assert parsed.environment.water_density == pytest.approx(
            1025.0, rel=1e-3
        )

        # Frequency count
        original_freqs = original.frequencies.to_frequencies_rad_s()
        parsed_freqs = parsed.frequencies.to_frequencies_rad_s()
        assert len(parsed_freqs) == len(original_freqs)

        # Heading count
        original_headings = original.wave_headings.to_heading_list()
        parsed_headings = parsed.wave_headings.to_heading_list()
        assert len(parsed_headings) == len(original_headings)

        # Mass
        assert parsed.vessel.inertia.mass == pytest.approx(
            original.vessel.inertia.mass, rel=1e-3
        )

    def test_round_trip_produces_valid_yaml(self, tmp_path: Path):
        """Round-trip result can be saved to YAML and reloaded."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        original = _load_ship_spec()
        dat_path = _generate_aqwa_dat(original, tmp_path)

        parser = AQWAInputParser()
        parsed = parser.parse(dat_path)

        yaml_path = tmp_path / "roundtrip_aqwa.yml"
        parsed.to_yaml(yaml_path)

        reloaded = DiffractionSpec.from_yaml(yaml_path)
        assert isinstance(reloaded, DiffractionSpec)


# ---------------------------------------------------------------------------
# OrcaWave Input Parser tests
# ---------------------------------------------------------------------------


class TestOrcaWaveInputParserBasic:
    """Test that OrcaWaveInputParser produces a valid DiffractionSpec."""

    def test_parse_returns_diffraction_spec(self, tmp_path: Path):
        """Parsing an OrcaWave .yml file returns a DiffractionSpec instance."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        assert isinstance(result, DiffractionSpec)

    def test_parsed_spec_has_vessel(self, tmp_path: Path):
        """Parsed spec should have a vessel (single-body case)."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        assert result.vessel is not None


class TestOrcaWaveInputParserEnvironment:
    """Test environment extraction from OrcaWave .yml."""

    def test_water_depth(self, tmp_path: Path):
        """Parsed spec has correct water depth."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        assert result.environment.water_depth == pytest.approx(500.0, rel=1e-3)

    def test_water_density(self, tmp_path: Path):
        """Parsed spec has correct water density (converted from t/m3 to kg/m3)."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        assert result.environment.water_density == pytest.approx(
            1025.0, rel=1e-3
        )

    def test_infinite_water_depth(self, tmp_path: Path):
        """OrcaWave 'Infinity' maps to spec 'infinite'."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        spec_data = spec.model_dump(mode="json", exclude_none=True)
        spec_data["environment"]["water_depth"] = "infinite"
        deep_spec = DiffractionSpec.model_validate(spec_data)

        yml_path = _generate_orcawave_yml(deep_spec, tmp_path)
        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        assert result.environment.water_depth == "infinite"


class TestOrcaWaveInputParserVessel:
    """Test vessel property extraction from OrcaWave .yml."""

    def test_vessel_name(self, tmp_path: Path):
        """Parsed spec has correct vessel name from BodyName."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        assert result.vessel.name == "Ship_001"

    def test_vessel_mass(self, tmp_path: Path):
        """Parsed spec has correct mass (converted from tonnes to kg)."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        assert result.vessel.inertia.mass == pytest.approx(
            84387300.0, rel=1e-3
        )

    def test_vessel_cog(self, tmp_path: Path):
        """Parsed spec has correct centre of gravity."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        assert result.vessel.inertia.centre_of_gravity == pytest.approx(
            [0.0, 0.0, -1.5], abs=0.01
        )


class TestOrcaWaveInputParserFrequencies:
    """Test frequency extraction from OrcaWave .yml."""

    def test_frequency_count(self, tmp_path: Path):
        """Parsed spec has correct number of frequencies."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        freqs = result.frequencies.to_frequencies_rad_s()
        assert len(freqs) == 15

    def test_frequency_values_from_periods(self, tmp_path: Path):
        """OrcaWave periods are correctly converted to rad/s in spec."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        original_freqs = sorted(spec.frequencies.to_frequencies_rad_s())
        parsed_freqs = sorted(result.frequencies.to_frequencies_rad_s())

        for orig, parsed in zip(original_freqs, parsed_freqs):
            assert parsed == pytest.approx(orig, rel=1e-2)


class TestOrcaWaveInputParserHeadings:
    """Test heading extraction from OrcaWave .yml."""

    def test_heading_count(self, tmp_path: Path):
        """Parsed spec has correct number of headings."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        headings = result.wave_headings.to_heading_list()
        assert len(headings) == 5

    def test_heading_values(self, tmp_path: Path):
        """Parsed headings match original spec values."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        original_headings = spec.wave_headings.to_heading_list()
        parsed_headings = result.wave_headings.to_heading_list()

        for orig, parsed in zip(original_headings, parsed_headings):
            assert parsed == pytest.approx(orig, abs=0.1)


class TestOrcaWaveInputParserInertia:
    """Test inertia extraction from OrcaWave .yml."""

    def test_inertia_tensor_from_matrix(self, tmp_path: Path):
        """Parsed spec has correct inertia tensor (converted from t.m2 to kg.m2)."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        spec = _load_ship_spec()
        yml_path = _generate_orcawave_yml(spec, tmp_path)

        parser = OrcaWaveInputParser()
        result = parser.parse(yml_path)

        assert result.vessel.inertia.inertia_tensor is not None

        # Original: mass=84387300 kg, radii=[15,50,50]
        # Forward: Ixx_t = (84387.3) * 15^2 = 18987142.5 t.m2
        # Reverse: Ixx_kg = 18987142.5 * 1000 = 18987142500 kg.m2
        expected_ixx = 84387300.0 * 15.0**2
        expected_iyy = 84387300.0 * 50.0**2

        ixx = result.vessel.inertia.inertia_tensor["Ixx"]
        iyy = result.vessel.inertia.inertia_tensor["Iyy"]

        assert ixx == pytest.approx(expected_ixx, rel=1e-2)
        assert iyy == pytest.approx(expected_iyy, rel=1e-2)


class TestOrcaWaveInputParserRoundTrip:
    """Test round-trip: spec -> OrcaWave .yml -> parse -> compare."""

    def test_round_trip_key_fields(self, tmp_path: Path):
        """Round-trip preserves key fields."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        original = _load_ship_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)

        parser = OrcaWaveInputParser()
        parsed = parser.parse(yml_path)

        # Environment
        assert parsed.environment.water_depth == pytest.approx(
            500.0, rel=1e-3
        )
        assert parsed.environment.water_density == pytest.approx(
            1025.0, rel=1e-3
        )

        # Frequency count
        original_freqs = original.frequencies.to_frequencies_rad_s()
        parsed_freqs = parsed.frequencies.to_frequencies_rad_s()
        assert len(parsed_freqs) == len(original_freqs)

        # Heading count
        original_headings = original.wave_headings.to_heading_list()
        parsed_headings = parsed.wave_headings.to_heading_list()
        assert len(parsed_headings) == len(original_headings)

        # Mass
        assert parsed.vessel.inertia.mass == pytest.approx(
            original.vessel.inertia.mass, rel=1e-3
        )

    def test_round_trip_produces_valid_yaml(self, tmp_path: Path):
        """Round-trip result can be saved to YAML and reloaded."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        original = _load_ship_spec()
        yml_path = _generate_orcawave_yml(original, tmp_path)

        parser = OrcaWaveInputParser()
        parsed = parser.parse(yml_path)

        yaml_path = tmp_path / "roundtrip_orcawave.yml"
        parsed.to_yaml(yaml_path)

        reloaded = DiffractionSpec.from_yaml(yaml_path)
        assert isinstance(reloaded, DiffractionSpec)


# ---------------------------------------------------------------------------
# Cross-conversion test
# ---------------------------------------------------------------------------


class TestCrossConversion:
    """Test full cross-conversion pipeline: AQWA .dat -> spec -> OrcaWave .yml."""

    def test_aqwa_to_orcawave_pipeline(self, tmp_path: Path):
        """AQWA .dat -> parse to spec -> generate OrcaWave .yml works end-to-end."""
        from digitalmodel.modules.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )
        from digitalmodel.modules.diffraction.reverse_parsers import (
            AQWAInputParser,
        )

        # Step 1: Generate an AQWA .dat from known spec
        original = _load_ship_spec()
        aqwa_dir = tmp_path / "aqwa"
        aqwa_dir.mkdir()
        dat_path = _generate_aqwa_dat(original, aqwa_dir)

        # Step 2: Parse the .dat back to spec
        aqwa_parser = AQWAInputParser()
        intermediate_spec = aqwa_parser.parse(dat_path)

        # Step 3: Generate OrcaWave .yml from the intermediate spec
        orcawave_dir = tmp_path / "orcawave"
        orcawave_dir.mkdir()
        orcawave_backend = OrcaWaveBackend()
        orcawave_path = orcawave_backend.generate_single(
            intermediate_spec, orcawave_dir
        )

        # Verify the OrcaWave .yml is valid
        assert orcawave_path.exists()
        with open(orcawave_path) as f:
            data = yaml.safe_load(f)

        assert "Bodies" in data
        assert "WaterDepth" in data
        assert "PeriodOrFrequency" in data

    def test_orcawave_to_aqwa_pipeline(self, tmp_path: Path):
        """OrcaWave .yml -> parse to spec -> generate AQWA .dat works end-to-end."""
        from digitalmodel.modules.diffraction.aqwa_backend import AQWABackend
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        # Step 1: Generate an OrcaWave .yml from known spec
        original = _load_ship_spec()
        orcawave_dir = tmp_path / "orcawave"
        orcawave_dir.mkdir()
        yml_path = _generate_orcawave_yml(original, orcawave_dir)

        # Step 2: Parse the .yml back to spec
        orcawave_parser = OrcaWaveInputParser()
        intermediate_spec = orcawave_parser.parse(yml_path)

        # Step 3: Generate AQWA .dat from the intermediate spec
        aqwa_dir = tmp_path / "aqwa"
        aqwa_dir.mkdir()
        aqwa_backend = AQWABackend()
        dat_path = aqwa_backend.generate_single(intermediate_spec, aqwa_dir)

        # Verify the .dat file is valid
        assert dat_path.exists()
        content = dat_path.read_text()
        assert "JOB AQWA" in content
        assert "DPTH" in content
        assert "HRTZ" in content
        assert "DIRN" in content


class TestParseExistingOrcaWaveFile:
    """Test parsing a real OrcaWave .yml example file."""

    def test_parse_l01_default_vessel(self):
        """Parse the L01 Default vessel example OrcaWave .yml file."""
        from digitalmodel.modules.diffraction.reverse_parsers import (
            OrcaWaveInputParser,
        )

        example_path = Path(
            "/mnt/github/workspace-hub/digitalmodel/docs/modules/orcawave"
            "/examples/L01_default_vessel/L01 Default vessel.yml"
        )
        if not example_path.exists():
            pytest.skip("L01 Default vessel example not available")

        parser = OrcaWaveInputParser()
        result = parser.parse(example_path)

        assert isinstance(result, DiffractionSpec)
        assert result.vessel is not None
        assert result.vessel.name == "Body1"
        assert result.environment.water_depth == pytest.approx(400.0, rel=1e-3)
        # OrcaWave density 1.025 t/m3 -> 1025 kg/m3
        assert result.environment.water_density == pytest.approx(
            1025.0, rel=1e-3
        )
        # 24 periods in the example
        freqs = result.frequencies.to_frequencies_rad_s()
        assert len(freqs) == 24
        # 9 headings
        headings = result.wave_headings.to_heading_list()
        assert len(headings) == 9
        # Body mass: 9017.95 tonnes -> 9017950 kg
        assert result.vessel.inertia.mass == pytest.approx(
            9017950.0, rel=1e-3
        )
