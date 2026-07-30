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
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_ship_raos.yml")
        assert spec.vessel.name == "Ship_001"
        assert spec.analysis_type.value == "diffraction"
        assert spec.environment.water_depth == 500.0

    def test_load_semisub_spec(self):
        """Load semi-sub spec with frequency range."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_semisub.yml")
        assert spec.vessel.name == "OC4_Semisub"
        assert spec.analysis_type.value == "full_qtf"
        assert spec.solver_options.qtf_calculation is True

    def test_load_fpso_turret_multibody(self):
        """Load multi-body FPSO turret spec."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
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
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
        with pytest.raises(Exception):  # ValidationError
            DiffractionSpec(
                environment={"water_depth": 100.0},
                frequencies={"input_type": "frequency", "values": [0.5]},
                wave_headings={"values": [0.0]},
            )

    def test_both_vessel_and_bodies_raises(self):
        """Cannot specify both vessel and bodies."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
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
        from digitalmodel.hydrodynamics.diffraction.input_schemas import EnvironmentSpec
        with pytest.raises(Exception):
            EnvironmentSpec(water_depth=-10.0)

    def test_infinite_water_depth_accepted(self):
        """Infinite water depth accepted as string."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import EnvironmentSpec
        env = EnvironmentSpec(water_depth="infinite")
        assert env.water_depth == "infinite"

    def test_water_density_rejects_air_density(self):
        """1.0 kg/m³ (air) must be rejected — likely a t/m³ unit error."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import EnvironmentSpec
        with pytest.raises(Exception, match="kg/m³"):
            EnvironmentSpec(water_depth=100.0, water_density=1.0)

    def test_water_density_rejects_tonnes_per_m3(self):
        """1.025 (seawater in t/m³) must be rejected — unit error."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import EnvironmentSpec
        with pytest.raises(Exception, match="kg/m³"):
            EnvironmentSpec(water_depth=100.0, water_density=1.025)

    def test_water_density_accepts_fresh_water(self):
        """1000.0 kg/m³ (fresh water) must be accepted."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import EnvironmentSpec
        env = EnvironmentSpec(water_depth=100.0, water_density=1000.0)
        assert env.water_density == 1000.0

    def test_water_density_accepts_seawater(self):
        """1025.0 kg/m³ (standard seawater) must be accepted."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import EnvironmentSpec
        env = EnvironmentSpec(water_depth=100.0, water_density=1025.0)
        assert env.water_density == 1025.0

    def test_missing_mass_raises(self):
        """Vessel inertia must include mass."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import VesselInertia
        with pytest.raises(Exception):
            VesselInertia(centre_of_gravity=[0, 0, 0], radii_of_gyration=[1, 1, 1])

    def test_missing_inertia_definition_raises(self):
        """Must specify either radii_of_gyration or inertia_tensor."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import VesselInertia
        with pytest.raises(Exception):
            VesselInertia(mass=1000, centre_of_gravity=[0, 0, 0])

    def test_invalid_cog_length_raises(self):
        """Centre of gravity must have 3 values."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import VesselInertia
        with pytest.raises(Exception):
            VesselInertia(mass=1000, centre_of_gravity=[0, 0], radii_of_gyration=[1, 1, 1])


class TestFrequencySpec:
    """Test frequency specification handling."""

    def test_explicit_frequency_values(self):
        """Explicit frequency list works."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import FrequencySpec
        freq = FrequencySpec(input_type="frequency", values=[0.1, 0.5, 1.0])
        result = freq.to_frequencies_rad_s()
        assert result == [0.1, 0.5, 1.0]

    def test_period_to_frequency_conversion(self):
        """Period values converted to frequencies correctly."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import FrequencySpec
        freq = FrequencySpec(input_type="period", values=[10.0])
        result = freq.to_frequencies_rad_s()
        expected = 2.0 * math.pi / 10.0
        assert abs(result[0] - expected) < 1e-10

    def test_frequency_to_period_conversion(self):
        """Frequencies converted to periods correctly."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import FrequencySpec
        freq = FrequencySpec(input_type="frequency", values=[1.0])
        result = freq.to_periods_s()
        expected = 2.0 * math.pi
        assert abs(result[0] - expected) < 1e-10

    def test_frequency_range_linear(self):
        """Linear frequency range generates correct values."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import FrequencySpec
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
        from digitalmodel.hydrodynamics.diffraction.input_schemas import FrequencySpec
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
        from digitalmodel.hydrodynamics.diffraction.input_schemas import FrequencySpec
        with pytest.raises(Exception):
            FrequencySpec(input_type="frequency")

    def test_both_values_and_range_raises(self):
        """Cannot specify both values and range."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import FrequencySpec
        with pytest.raises(Exception):
            FrequencySpec(
                input_type="frequency",
                values=[0.5],
                range={"start": 0.1, "end": 1.0, "count": 10}
            )

    @pytest.mark.parametrize(
        ("input_type", "values"),
        [
            ("period", [10.0, 0.0]),
            ("period", [10.0, -1.0]),
            ("period", [10.0, math.inf]),
            ("frequency", [1.0, 0.0]),
            ("frequency", [1.0, math.nan]),
        ],
    )
    def test_conversion_rejects_nonpositive_or_nonfinite_values(
        self, input_type, values
    ):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import FrequencySpec

        freq = FrequencySpec(input_type=input_type, values=values)
        with pytest.raises(ValueError):
            freq.to_frequencies_rad_s()


class TestWaveHeadingSpec:
    """Test wave heading specification handling."""

    def test_explicit_heading_values(self):
        """Explicit heading list works."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import WaveHeadingSpec
        headings = WaveHeadingSpec(values=[0, 45, 90, 135, 180])
        result = headings.to_heading_list()
        assert result == [0, 45, 90, 135, 180]

    def test_heading_range(self):
        """Heading range generates correct values."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import WaveHeadingSpec
        headings = WaveHeadingSpec(range={"start": 0, "end": 180, "increment": 45})
        result = headings.to_heading_list()
        assert result == [0.0, 45.0, 90.0, 135.0, 180.0]


class TestGetBodies:
    """Test body list retrieval for single and multi-body cases."""

    def test_single_vessel_wraps_to_body_list(self):
        """Single vessel spec wraps into body list."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_ship_raos.yml")
        bodies = spec.get_bodies()
        assert len(bodies) == 1
        assert bodies[0].vessel.name == "Ship_001"

    def test_multi_body_returns_all_bodies(self):
        """Multi-body spec returns all bodies."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_fpso_turret.yml")
        bodies = spec.get_bodies()
        assert len(bodies) == 2


class TestSpecRoundTrip:
    """Test YAML round-trip: load -> save -> load -> compare."""

    def test_ship_raos_round_trip(self, tmp_path: Path):
        """Ship RAO spec survives YAML round-trip."""
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
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
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
        schema = DiffractionSpec.model_json_schema()
        assert "properties" in schema
        assert "environment" in schema["properties"]
        assert "frequencies" in schema["properties"]


class TestVocabularyValidation:
    """Free-string fields with documented vocabularies must reject typos.

    Regression for the review sweep: 'mode' and 'solve_type' were plain str,
    so e.g. mode='free-floating' or solve_type='mean-drift' validated cleanly
    and silently generated the wrong model.
    """

    def _inertia_kwargs(self):
        return {
            "mass": 1000.0,
            "centre_of_gravity": [0.0, 0.0, 0.0],
            "radii_of_gyration": [1.0, 2.0, 3.0],
        }

    def test_inertia_mode_typo_rejected(self):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import VesselInertia
        with pytest.raises(Exception):  # ValidationError
            VesselInertia(mode="free-floating", **self._inertia_kwargs())

    def test_inertia_mode_valid_values_accepted(self):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import VesselInertia
        for mode in ("explicit", "free_floating"):
            assert VesselInertia(mode=mode, **self._inertia_kwargs()).mode == mode

    @pytest.mark.parametrize("bad", ["mean-drift", "meandrift", "full-qtf", ""])
    def test_solve_type_typo_rejected(self, bad):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import SolverOptions
        with pytest.raises(Exception):  # ValidationError
            SolverOptions(solve_type=bad)

    @pytest.mark.parametrize(
        "good",
        [
            "potential_only",
            "potential_and_source",
            "mean_drift",
            "diagonal_qtf",
            "full_qtf",
        ],
    )
    def test_solve_type_valid_values_accepted(self, good):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import SolverOptions
        assert SolverOptions(solve_type=good).solve_type == good


class TestLegacyQtfSerialization:
    """Legacy-flat QTF enablement must survive dump/reload (#623/#624)."""

    def test_flat_qtf_calculation_round_trips(self):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import SolverOptions
        options = SolverOptions(qtf_calculation=True)
        assert options.resolved_qtf().enabled is True
        dumped = options.model_dump(mode="json", exclude_none=True)
        assert dumped.get("qtf_calculation") is True
        reloaded = SolverOptions.model_validate(dumped)
        assert reloaded.resolved_qtf().enabled is True

    def test_qtf_disabled_not_serialized(self):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import SolverOptions
        dumped = SolverOptions().model_dump(mode="json", exclude_none=True)
        assert "qtf_calculation" not in dumped

    def test_nested_qtf_round_trips_without_flat_flag(self):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import SolverOptions
        options = SolverOptions(solve_type="full_qtf", qtf={"enabled": True})
        dumped = options.model_dump(mode="json", exclude_none=True)
        # Nested form carries the enablement itself; re-adding the flat flag
        # would trip the mutual-exclusion guard on reload.
        assert "qtf_calculation" not in dumped
        reloaded = SolverOptions.model_validate(dumped)
        assert reloaded.resolved_qtf().enabled is True

    def test_spec_to_yaml_round_trip_keeps_qtf_enabled(self, tmp_path: Path):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
        spec = DiffractionSpec.from_yaml(FIXTURES_DIR / "spec_semisub.yml")
        assert spec.solver_options.resolved_qtf().enabled is True
        out = tmp_path / "roundtrip.yml"
        spec.to_yaml(out)
        reloaded = DiffractionSpec.from_yaml(out)
        assert reloaded.solver_options.resolved_qtf().enabled is True


class TestControlSurfaceSpecValidation:
    """Control surfaces must be generatable (mesh file or auto parameters)."""

    def test_mesh_type_requires_mesh_file(self):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            ControlSurfaceSpec,
        )
        with pytest.raises(Exception):
            ControlSurfaceSpec(type="mesh")

    def test_auto_requires_panel_size_and_separation(self):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            ControlSurfaceSpec,
        )
        with pytest.raises(Exception):
            ControlSurfaceSpec(type="auto", panel_size=0.5)

    def test_auto_with_parameters_valid(self):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            ControlSurfaceSpec,
        )
        cs = ControlSurfaceSpec(type="auto", panel_size=0.5, separation=0.3)
        assert cs.mesh_file is None

    def test_mesh_with_file_valid_and_format_optional(self):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            ControlSurfaceSpec,
        )
        cs = ControlSurfaceSpec(type="mesh", mesh_file="cs.gdf")
        assert cs.mesh_format is None
