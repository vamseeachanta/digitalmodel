"""Tests for MonolithicExtractor.

Covers _sanitize_name, metadata extraction, environment extraction (water,
seabed, waves, current, wind), simulation extraction, generic model extraction
(list sections, variable data, singletons, general properties), and full
round-trip validation via ProjectInputSpec.
"""

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.extractor import (
    MonolithicExtractor,
    _sanitize_name,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import (
    ProjectInputSpec,
)


# ---------------------------------------------------------------------------
# _sanitize_name
# ---------------------------------------------------------------------------


class TestSanitizeName:
    """Tests for the _sanitize_name helper function."""

    def test_converts_spaces_to_underscores(self):
        assert _sanitize_name("A01 Catenary riser") == "a01_catenary_riser"

    def test_handles_hyphens(self):
        assert _sanitize_name("my-model-file") == "my_model_file"

    def test_collapses_multiple_underscores(self):
        assert _sanitize_name("foo___bar") == "foo_bar"

    def test_handles_special_chars(self):
        assert _sanitize_name("model (v2.1)") == "model_v2_1"

    def test_strips_leading_trailing_underscores(self):
        assert _sanitize_name("_test_") == "test"

    def test_lowercases_result(self):
        assert _sanitize_name("MyModel") == "mymodel"

    def test_mixed_separators(self):
        assert _sanitize_name("A01 - Catenary_Riser (v2)") == "a01_catenary_riser_v2"

    def test_pure_alphanumeric_unchanged(self):
        assert _sanitize_name("simplemodel") == "simplemodel"


# ---------------------------------------------------------------------------
# _extract_metadata
# ---------------------------------------------------------------------------


class TestExtractMetadata:
    """Tests for metadata extraction from filename."""

    def test_generates_metadata_from_filename(self, tmp_path):
        yml = tmp_path / "A01 Catenary Riser.yml"
        yml.write_text("Environment:\n  WaterDepth: 100\n")
        ext = MonolithicExtractor(yml)
        meta = ext._extract_metadata()

        assert meta["name"] == "a01_catenary_riser"
        assert "A01 Catenary Riser.yml" in meta["description"]
        assert meta["structure"] == "generic"
        assert meta["operation"] == "generic"
        assert meta["project"] == "model_library"


# ---------------------------------------------------------------------------
# _extract_environment
# ---------------------------------------------------------------------------


class TestExtractEnvironment:
    """Tests for environment extraction."""

    def test_extracts_water_depth_and_density(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("Environment:\n  WaterDepth: 150\n  Density: 1.030\n")
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert env["water"]["depth"] == 150
        assert env["water"]["density"] == 1.030

    def test_defaults_when_keys_missing(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("General:\n  StageDuration: [0, 10]\n")
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert env["water"]["depth"] == 100.0
        assert env["water"]["density"] == 1.025
        assert env["seabed"]["stiffness"]["normal"] == 100.0
        assert env["seabed"]["stiffness"]["shear"] == 100.0

    def test_extracts_seabed(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  SeabedNormalStiffness: 50\n"
            "  SeabedShearStiffness: 25\n"
            "  SeabedSlope: 5\n"
        )
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert env["seabed"]["stiffness"]["normal"] == 50
        assert env["seabed"]["stiffness"]["shear"] == 25
        assert env["seabed"]["slope"] == 5

    def test_extracts_waves_from_wave_trains(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "  WaveTrains:\n"
            "    - WaveType: JONSWAP\n"
            "      WaveHs: 3.5\n"
            "      WaveTz: 8.0\n"
            "      WaveDirection: 180\n"
        )
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        # Extractor emits a trains list (multi-train support); the Waves
        # schema exposes trains[0] via backward-compat properties.
        train = env["waves"]["trains"][0]
        assert train["type"] == "JONSWAP"
        assert train["height"] == 3.5
        assert train["period"] == 8.0
        assert train["direction"] == 180

    def test_extracts_all_wave_trains(self, tmp_path):
        """Multi-train models (wind sea + swell) keep ALL trains (story 496)."""
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "  WaveTrains:\n"
            "    - Name: WindSea\n"
            "      WaveType: JONSWAP\n"
            "      WaveHs: 3.5\n"
            "      WaveTz: 8.0\n"
            "      WaveDirection: 180\n"
            "    - Name: Swell\n"
            "      WaveType: Airy\n"
            "      WaveHeight: 1.5\n"
            "      WavePeriod: 14.0\n"
            "      WaveDirection: 90\n"
        )
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        trains = env["waves"]["trains"]
        assert len(trains) == 2
        assert trains[0]["name"] == "WindSea"
        assert trains[0]["type"] == "JONSWAP"
        assert trains[1]["name"] == "Swell"
        assert trains[1]["type"] == "Airy"
        assert trains[1]["height"] == 1.5
        assert trains[1]["period"] == 14.0
        assert trains[1]["direction"] == 90

    def test_extracts_wave_gamma(self, tmp_path):
        """WaveGamma is captured so it round-trips (was dropped -> 3.3)."""
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "  WaveTrains:\n"
            "    - WaveType: JONSWAP\n"
            "      WaveJONSWAPParameters: Fully specified\n"
            "      WaveHs: 3.0\n"
            "      WaveTz: 8.0\n"
            "      WaveGamma: 5.0\n"
        )
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert env["waves"]["trains"][0]["gamma"] == 5.0

    def test_wave_gamma_outside_schema_range_left_to_raw_layer(self, tmp_path):
        """WaveGamma: 1 (real exports, e.g. mudmat SUT_MM_SZ.yml) is outside
        the typed schema range (gt=1) and must not be typed — it survives via
        raw_properties instead of failing spec validation."""
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "  WaveTrains:\n"
            "    - WaveType: JONSWAP\n"
            "      WaveHs: 3.0\n"
            "      WaveTz: 8.0\n"
            "      WaveGamma: 1\n"
        )
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert "gamma" not in env["waves"]["trains"][0]
        assert env["raw_properties"]["WaveTrains"][0]["WaveGamma"] == 1

    def test_zero_wave_height_not_treated_as_missing(self, tmp_path):
        """WaveHs: 0 is a legitimate value; the old or-chain skipped it."""
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "  WaveTrains:\n"
            "    - WaveType: JONSWAP\n"
            "      WaveHs: 0\n"
            "      WaveHeight: 5.0\n"
            "      WaveTz: 8.0\n"
        )
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert env["waves"]["trains"][0]["height"] == 0

    def test_no_waves_when_absent(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("Environment:\n  WaterDepth: 100\n")
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert "waves" not in env

    def test_extracts_current(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "  RefCurrentSpeed: 0.5\n"
            "  RefCurrentDirection: 90\n"
        )
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert env["current"]["speed"] == 0.5
        assert env["current"]["direction"] == 90

    def test_no_current_when_absent(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("Environment:\n  WaterDepth: 100\n")
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert "current" not in env

    def test_extracts_wind(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "  WindSpeed: 15.0\n"
            "  WindDirection: 270\n"
        )
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert env["wind"]["speed"] == 15.0
        assert env["wind"]["direction"] == 270

    def test_no_wind_when_absent(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("Environment:\n  WaterDepth: 100\n")
        ext = MonolithicExtractor(yml)
        env = ext._extract_environment()

        assert "wind" not in env


# ---------------------------------------------------------------------------
# _extract_simulation
# ---------------------------------------------------------------------------


class TestExtractSimulation:
    """Tests for simulation parameter extraction."""

    def test_extracts_stage_duration(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("General:\n  StageDuration: [0, 10, 60]\n")
        ext = MonolithicExtractor(yml)
        sim = ext._extract_simulation()

        assert sim["stages"] == [0, 10, 60]

    def test_extracts_time_step(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("General:\n  ImplicitConstantTimeStep: 0.01\n")
        ext = MonolithicExtractor(yml)
        sim = ext._extract_simulation()

        assert sim["time_step"] == 0.01

    def test_empty_when_no_general(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("Environment:\n  WaterDepth: 100\n")
        ext = MonolithicExtractor(yml)
        sim = ext._extract_simulation()

        assert sim == {}


# ---------------------------------------------------------------------------
# _extract_generic_model
# ---------------------------------------------------------------------------


class TestExtractGenericModel:
    """Tests for generic model extraction."""

    def test_extracts_line_types(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "LineTypes:\n"
            "  - Name: Pipe1\n"
            "    Category: General\n"
            "    OD: 0.3\n"
            "    ID: 0.2\n"
            "    MassPerUnitLength: 0.1\n"
            "    EI: 100\n"
            "    EA: 500000\n"
            "    Drag: 1.2\n"
        )
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        assert "line_types" in generic
        lt = generic["line_types"][0]
        assert lt["name"] == "Pipe1"
        assert lt["category"] == "General"
        assert lt["outer_diameter"] == 0.3
        assert lt["inner_diameter"] == 0.2
        assert lt["mass_per_length"] == 0.1
        assert lt["bending_stiffness"] == 100
        assert lt["axial_stiffness"] == 500000
        assert lt["properties"]["Drag"] == 1.2

    def test_extracts_vessels(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Vessels:\n"
            "  - Name: Vessel1\n"
            "    VesselType: Barge\n"
            "    Connection: Fixed\n"
            "    InitialPosition: [0, 0, 0]\n"
        )
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        assert "vessels" in generic
        v = generic["vessels"][0]
        assert v["name"] == "Vessel1"
        assert v["vessel_type"] == "Barge"
        assert v["connection"] == "Fixed"
        assert v["initial_position"] == [0, 0, 0]

    def test_splits_typed_fields_from_properties(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "LineTypes:\n"
            "  - Name: Pipe1\n"
            "    OD: 0.3\n"
            "    Drag: 1.2\n"
            "    AddedMass: 0.5\n"
        )
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        lt = generic["line_types"][0]
        # Typed fields extracted
        assert lt["name"] == "Pipe1"
        assert lt["outer_diameter"] == 0.3
        # Non-typed go to properties
        assert lt["properties"]["Drag"] == 1.2
        assert lt["properties"]["AddedMass"] == 0.5
        # Typed fields not duplicated in properties
        assert "OD" not in lt["properties"]
        assert "Name" not in lt["properties"]

    def test_stiffener_type_keys_stay_in_properties(self, tmp_path):
        """Keys mapped by ORCAFLEX_TO_TYPED but absent from the section's
        model class (StiffenerTypes has no OD/ID/EI/EA/mass typed fields)
        must stay in the properties bag — typing them would be silently
        dropped by Pydantic (extra='ignore') and lost from the model."""
        yml = tmp_path / "test.yml"
        yml.write_text(
            "StiffenerTypes:\n"
            "  - Name: BS1\n"
            "    OD: 0.35\n"
            "    ID: 0.2\n"
            "    MassPerUnitLength: 0.1\n"
            "    EI: 500\n"
            "    EA: 100000\n"
            "    Pen: 3\n"
        )
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        st = generic["stiffener_types"][0]
        assert st["name"] == "BS1"
        for key, value in [
            ("OD", 0.35), ("ID", 0.2), ("MassPerUnitLength", 0.1),
            ("EI", 500), ("EA", 100000), ("Pen", 3),
        ]:
            assert st["properties"][key] == value

        # Round-trip through the typed spec must preserve them too
        spec = ProjectInputSpec(**{
            "metadata": {
                "name": "t", "description": "t", "structure": "generic",
                "operation": "generic", "project": "t",
            },
            "environment": {
                "water": {"depth": 100},
                "seabed": {"stiffness": {"normal": 100, "shear": 100}},
            },
            "generic": generic,
        })
        props = spec.generic.stiffener_types[0].properties
        assert props["OD"] == 0.35
        assert props["EA"] == 100000

    def test_flex_joint_connection_stays_in_properties(self, tmp_path):
        """GenericFlexJoint defines no 'connection' field; Connection must
        pass through the properties bag instead of being typed and dropped."""
        yml = tmp_path / "test.yml"
        yml.write_text(
            "FlexJoints:\n"
            "  - Name: FJ1\n"
            "    Connection: Vessel1\n"
        )
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        fj = generic["flex_joints"][0]
        assert fj["name"] == "FJ1"
        assert fj["properties"]["Connection"] == "Vessel1"

    def test_handles_variable_data_nested_structure(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "VariableData:\n"
            "  Dragcoefficient:\n"
            "    - Name: GenericDrag\n"
            "      Value: 1.0\n"
            "  Coatingsorlinings:\n"
            "    - Name: CWC120\n"
            "      Thickness: 0.12\n"
        )
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        assert "variable_data_sources" in generic
        vds = generic["variable_data_sources"]
        assert len(vds) == 2

        names = [v["name"] for v in vds]
        assert "GenericDrag" in names
        assert "CWC120" in names

        drag = next(v for v in vds if v["name"] == "GenericDrag")
        assert drag["data_type"] == "Dragcoefficient"
        assert drag["properties"]["Value"] == 1.0

    def test_handles_singleton_sections(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "SolidFrictionCoefficients:\n"
            "  StaticFriction: 0.3\n"
            "  DynamicFriction: 0.2\n"
        )
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        assert "friction_coefficients" in generic
        assert generic["friction_coefficients"]["data"]["StaticFriction"] == 0.3

    def test_handles_general_properties_excluding_sim_keys(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "General:\n"
            "  StageDuration: [0, 10]\n"
            "  ImplicitConstantTimeStep: 0.01\n"
            "  StaticsMethod: Full statics\n"
            "  DynamicsSolutionMethod: Implicit\n"
        )
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        assert "general_properties" in generic
        gp = generic["general_properties"]
        # Simulation keys excluded
        assert "StageDuration" not in gp
        assert "ImplicitConstantTimeStep" not in gp
        # Other keys included
        assert gp["StaticsMethod"] == "Full statics"
        assert gp["DynamicsSolutionMethod"] == "Implicit"

    def test_empty_model_when_no_sections(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("Environment:\n  WaterDepth: 100\n")
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        assert generic == {}

    def test_multiple_line_types(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "LineTypes:\n"
            "  - Name: Pipe1\n"
            "    OD: 0.3\n"
            "  - Name: Pipe2\n"
            "    OD: 0.5\n"
        )
        ext = MonolithicExtractor(yml)
        generic = ext._extract_generic_model()

        assert len(generic["line_types"]) == 2
        names = [lt["name"] for lt in generic["line_types"]]
        assert names == ["Pipe1", "Pipe2"]


# ---------------------------------------------------------------------------
# Multi-document YAML loading
# ---------------------------------------------------------------------------


class TestMultiDocumentYaml:
    """Tests for multi-document YAML handling."""

    def test_merges_multiple_documents(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 200\n"
            "---\n"
            "LineTypes:\n"
            "  - Name: Pipe1\n"
            "    OD: 0.3\n"
        )
        ext = MonolithicExtractor(yml)

        assert ext._raw.get("Environment") is not None
        assert ext._raw.get("LineTypes") is not None

    def test_later_documents_override_earlier(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "---\n"
            "Environment:\n"
            "  WaterDepth: 200\n"
        )
        ext = MonolithicExtractor(yml)

        assert ext._raw["Environment"]["WaterDepth"] == 200


# ---------------------------------------------------------------------------
# Public helpers
# ---------------------------------------------------------------------------


class TestPublicHelpers:
    """Tests for public helper methods on MonolithicExtractor."""

    def test_raw_returns_copy(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("Environment:\n  WaterDepth: 100\n")
        ext = MonolithicExtractor(yml)

        raw = ext.raw
        raw["NewKey"] = "should not affect internal"
        assert "NewKey" not in ext._raw

    def test_get_section_keys(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "Environment:\n  WaterDepth: 100\n"
            "LineTypes:\n  - Name: Pipe1\n"
        )
        ext = MonolithicExtractor(yml)

        keys = ext.get_section_keys()
        assert "Environment" in keys
        assert "LineTypes" in keys
        assert keys == sorted(keys)

    def test_get_object_names(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text(
            "LineTypes:\n"
            "  - Name: Pipe1\n"
            "  - Name: Pipe2\n"
        )
        ext = MonolithicExtractor(yml)

        names = ext.get_object_names("LineTypes")
        assert names == ["Pipe1", "Pipe2"]

    def test_get_object_names_missing_section(self, tmp_path):
        yml = tmp_path / "test.yml"
        yml.write_text("Environment:\n  WaterDepth: 100\n")
        ext = MonolithicExtractor(yml)

        assert ext.get_object_names("LineTypes") == []


# ---------------------------------------------------------------------------
# Full round-trip: extract -> validate
# ---------------------------------------------------------------------------


class TestFullRoundTrip:
    """Tests for extracting a YAML and validating through ProjectInputSpec."""

    def test_round_trip_with_line_types(self, tmp_path):
        yml = tmp_path / "test_model.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 150\n"
            "  Density: 1.025\n"
            "  SeabedNormalStiffness: 50\n"
            "  SeabedShearStiffness: 50\n"
            "LineTypes:\n"
            "  - Name: Pipe1\n"
            "    Category: General\n"
            "    OD: 0.3\n"
            "    ID: 0.2\n"
            "    MassPerUnitLength: 0.1\n"
            "    EI: 100\n"
            "    EA: 500000\n"
            "    Drag: 1.2\n"
        )
        ext = MonolithicExtractor(yml)
        result = ext.extract()

        spec = ProjectInputSpec(**result)

        assert spec.metadata.name == "test_model"
        assert spec.environment.water.depth == 150
        assert spec.is_generic() is True
        assert spec.is_pipeline() is False
        assert spec.is_riser() is False
        assert len(spec.generic.line_types) == 1
        assert spec.generic.line_types[0].name == "Pipe1"
        assert spec.generic.line_types[0].outer_diameter == 0.3
        assert spec.generic.line_types[0].properties["Drag"] == 1.2

    def test_round_trip_with_multiple_sections(self, tmp_path):
        yml = tmp_path / "multi_section.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "  Density: 1.025\n"
            "  SeabedNormalStiffness: 100\n"
            "  SeabedShearStiffness: 100\n"
            "LineTypes:\n"
            "  - Name: Riser\n"
            "    OD: 0.25\n"
            "Vessels:\n"
            "  - Name: FPSO\n"
            "    VesselType: Tanker\n"
            "    Connection: Free\n"
            "Lines:\n"
            "  - Name: Riser1\n"
            "    EndAConnection: FPSO\n"
            "General:\n"
            "  StageDuration: [8, 60]\n"
            "  StaticsMethod: Full statics\n"
        )
        ext = MonolithicExtractor(yml)
        result = ext.extract()

        spec = ProjectInputSpec(**result)

        assert len(spec.generic.line_types) == 1
        assert len(spec.generic.vessels) == 1
        assert len(spec.generic.lines) == 1
        assert spec.generic.general_properties["StaticsMethod"] == "Full statics"
        assert spec.simulation.stages == [8, 60]

    def test_round_trip_empty_model(self, tmp_path):
        yml = tmp_path / "empty.yml"
        yml.write_text(
            "Environment:\n"
            "  WaterDepth: 100\n"
            "  Density: 1.025\n"
            "  SeabedNormalStiffness: 100\n"
            "  SeabedShearStiffness: 100\n"
        )
        ext = MonolithicExtractor(yml)
        result = ext.extract()

        # Generic model will be empty dict -> GenericModel() with all defaults
        spec = ProjectInputSpec(**result)

        assert spec.is_generic() is True
        assert spec.generic.line_types == []
        assert spec.generic.vessels == []
