"""Tests for the canonical diffraction analysis input schema.

TDD: These tests define the expected behavior of the input_schemas module.
"""
import math
from pathlib import Path

import pytest
import yaml


# Fixtures directory
FIXTURES_DIR = Path(__file__).parent / "fixtures"


class TestDiffractionSpecLoading:
    """Test loading spec from YAML files."""

    def test_load_ship_raos_spec(self):
        """Load ship RAO spec from YAML fixture."""
        from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_ship_raos.yml")
        assert spec.vessel.name == "Ship_001"
        assert spec.analysis_type.value == "diffraction"
        assert spec.environment.water_depth == 500.0

    def test_load_semisub_spec(self):
        """Load semi-sub spec with frequency range."""
        from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_semisub.yml")
        assert spec.vessel.name == "OC4_Semisub"
        assert spec.analysis_type.value == "full_qtf"
        assert spec.solver_options.qtf_calculation is True

    def test_load_fpso_turret_multibody(self):
        """Load multi-body FPSO turret spec."""
        from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_fpso_turret.yml")
        assert spec.vessel is None
        assert spec.bodies is not None
        assert len(spec.bodies) == 2
        assert spec.bodies[0].vessel.name == "FPSO_Hull"
        assert spec.bodies[1].vessel.name == "Turret"
        assert spec.bodies[1].connection_parent == "FPSO_Hull"


class TestDiffractionSpecValidation:
    """Test schema validation catches errors."""

    def test_missing_vessel_and_bodies_raises(self):
        """Must specify either vessel or bodies."""
        from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec
        with pytest.raises(Exception):  # ValidationError
            DiffractionSpec(
                environment={"water_depth": 100.0},
                frequencies={"input_type": "frequency", "values": [0.5]},
                wave_headings={"values": [0.0]},
            )

    def test_both_vessel_and_bodies_raises(self):
        """Cannot specify both vessel and bodies."""
        from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec
        vessel_data = {
            "name": "Test",
            "geometry": {"mesh_file": "test.gdf"},
            "inertia": {"mass": 1000, "centre_of_gravity": [0, 0, 0], "radii_of_gyration": [1, 1, 1]},
        }
        with pytest.raises(Exception):
            DiffractionSpec(
                vessel=vessel_data,
                bodies=[{"vessel": vessel_data}],
                environment={"water_depth": 100.0},
                frequencies={"input_type": "frequency", "values": [0.5]},
                wave_headings={"values": [0.0]},
            )

    def test_negative_water_depth_raises(self):
        """Water depth must be positive or infinite."""
        from digitalmodel.modules.diffraction.input_schemas import EnvironmentSpec
        with pytest.raises(Exception):
            EnvironmentSpec(water_depth=-10.0)

    def test_infinite_water_depth_accepted(self):
        """Infinite water depth accepted as string."""
        from digitalmodel.modules.diffraction.input_schemas import EnvironmentSpec
        env = EnvironmentSpec(water_depth="infinite")
        assert env.water_depth == "infinite"

    def test_missing_mass_raises(self):
        """Vessel inertia must include mass."""
        from digitalmodel.modules.diffraction.input_schemas import VesselInertia
        with pytest.raises(Exception):
            VesselInertia(centre_of_gravity=[0, 0, 0], radii_of_gyration=[1, 1, 1])

    def test_missing_inertia_definition_raises(self):
        """Must specify either radii_of_gyration or inertia_tensor."""
        from digitalmodel.modules.diffraction.input_schemas import VesselInertia
        with pytest.raises(Exception):
            VesselInertia(mass=1000, centre_of_gravity=[0, 0, 0])

    def test_invalid_cog_length_raises(self):
        """Centre of gravity must have 3 values."""
        from digitalmodel.modules.diffraction.input_schemas import VesselInertia
        with pytest.raises(Exception):
            VesselInertia(mass=1000, centre_of_gravity=[0, 0], radii_of_gyration=[1, 1, 1])


class TestFrequencySpec:
    """Test frequency specification handling."""

    def test_explicit_frequency_values(self):
        """Explicit frequency list works."""
        from digitalmodel.modules.diffraction.input_schemas import FrequencySpec
        freq = FrequencySpec(input_type="frequency", values=[0.1, 0.5, 1.0])
        result = freq.to_frequencies_rad_s()
        assert result == [0.1, 0.5, 1.0]

    def test_period_to_frequency_conversion(self):
        """Period values converted to frequencies correctly."""
        from digitalmodel.modules.diffraction.input_schemas import FrequencySpec
        freq = FrequencySpec(input_type="period", values=[10.0])
        result = freq.to_frequencies_rad_s()
        expected = 2.0 * math.pi / 10.0
        assert abs(result[0] - expected) < 1e-10

    def test_frequency_to_period_conversion(self):
        """Frequencies converted to periods correctly."""
        from digitalmodel.modules.diffraction.input_schemas import FrequencySpec
        freq = FrequencySpec(input_type="frequency", values=[1.0])
        result = freq.to_periods_s()
        expected = 2.0 * math.pi
        assert abs(result[0] - expected) < 1e-10

    def test_frequency_range_linear(self):
        """Linear frequency range generates correct values."""
        from digitalmodel.modules.diffraction.input_schemas import FrequencySpec
        freq = FrequencySpec(
            input_type="frequency",
            range={"start": 0.1, "end": 1.0, "count": 10, "distribution": "linear"}
        )
        result = freq.to_frequencies_rad_s()
        assert len(result) == 10
        assert abs(result[0] - 0.1) < 1e-10
        assert abs(result[-1] - 1.0) < 1e-10

    def test_frequency_range_logarithmic(self):
        """Logarithmic frequency range generates correct values."""
        from digitalmodel.modules.diffraction.input_schemas import FrequencySpec
        freq = FrequencySpec(
            input_type="frequency",
            range={"start": 0.1, "end": 10.0, "count": 5, "distribution": "logarithmic"}
        )
        result = freq.to_frequencies_rad_s()
        assert len(result) == 5
        assert abs(result[0] - 0.1) < 1e-10
        assert abs(result[-1] - 10.0) < 1e-6

    def test_neither_values_nor_range_raises(self):
        """Must specify either values or range."""
        from digitalmodel.modules.diffraction.input_schemas import FrequencySpec
        with pytest.raises(Exception):
            FrequencySpec(input_type="frequency")

    def test_both_values_and_range_raises(self):
        """Cannot specify both values and range."""
        from digitalmodel.modules.diffraction.input_schemas import FrequencySpec
        with pytest.raises(Exception):
            FrequencySpec(
                input_type="frequency",
                values=[0.5],
                range={"start": 0.1, "end": 1.0, "count": 10}
            )


class TestWaveHeadingSpec:
    """Test wave heading specification handling."""

    def test_explicit_heading_values(self):
        """Explicit heading list works."""
        from digitalmodel.modules.diffraction.input_schemas import WaveHeadingSpec
        headings = WaveHeadingSpec(values=[0, 45, 90, 135, 180])
        result = headings.to_heading_list()
        assert result == [0, 45, 90, 135, 180]

    def test_heading_range(self):
        """Heading range generates correct values."""
        from digitalmodel.modules.diffraction.input_schemas import WaveHeadingSpec
        headings = WaveHeadingSpec(range={"start": 0, "end": 180, "increment": 45})
        result = headings.to_heading_list()
        assert result == [0.0, 45.0, 90.0, 135.0, 180.0]


class TestGetBodies:
    """Test body list retrieval for single and multi-body cases."""

    def test_single_vessel_wraps_to_body_list(self):
        """Single vessel spec wraps into body list."""
        from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_ship_raos.yml")
        bodies = spec.get_bodies()
        assert len(bodies) == 1
        assert bodies[0].vessel.name == "Ship_001"

    def test_multi_body_returns_all_bodies(self):
        """Multi-body spec returns all bodies."""
        from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_fpso_turret.yml")
        bodies = spec.get_bodies()
        assert len(bodies) == 2


class TestSpecRoundTrip:
    """Test YAML round-trip: load -> save -> load -> compare."""

    def test_ship_raos_round_trip(self, tmp_path: Path):
        """Ship RAO spec survives YAML round-trip."""
        from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec
        original = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_ship_raos.yml")
        output_path = tmp_path / "round_trip.yml"
        original.to_yaml(output_path)
        reloaded = DiffractionSpec.from_yaml(output_path)
        assert reloaded.vessel.name == original.vessel.name
        assert reloaded.environment.water_depth == original.environment.water_depth
        assert len(reloaded.frequencies.values) == len(original.frequencies.values)


class TestJsonSchema:
    """Test JSON Schema export."""

    def test_json_schema_export(self):
        """JSON Schema can be exported."""
        from digitalmodel.modules.diffraction.input_schemas import DiffractionSpec
        schema = DiffractionSpec.model_json_schema()
        assert "properties" in schema
        assert "environment" in schema["properties"]
        assert "frequencies" in schema["properties"]
