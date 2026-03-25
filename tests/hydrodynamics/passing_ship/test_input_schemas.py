"""Tests for PassingShipSpec Pydantic schema (WRK-131 Phase 2).

Tests cover:
- PassingShipSpec construction, YAML round-trip, dict round-trip
- MooredVesselSpec field validation
- PassingTrack validators (speed > 0, separation > 0)
- EnvironmentSpec: 'infinite' water depth accepted
- AnalysisCase override fields
- HullSpec displacement derivation
- Error cases (missing required fields, invalid ranges)
"""

import tempfile
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.passing_ship.input_schemas import (
    AnalysisCase,
    EnvironmentSpec,
    HullSpec,
    MooredVesselSpec,
    MooringLineSpec,
    OutputSpec,
    PassingShipDimensions,
    PassingShipScenario,
    PassingShipSpec,
    PassingTrack,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def minimal_hull():
    return HullSpec(loa=200.0, beam=32.0, draft=12.0)


@pytest.fixture()
def minimal_moored_vessel(minimal_hull):
    return MooredVesselSpec(hull=minimal_hull)


@pytest.fixture()
def minimal_passing_track():
    return PassingTrack(speed_knots=5.0, separation_m=100.0)


@pytest.fixture()
def minimal_passing_dimensions():
    return PassingShipDimensions(loa=250.0, beam=40.0, draft=14.0)


@pytest.fixture()
def minimal_passing_scenario(minimal_passing_dimensions, minimal_passing_track):
    return PassingShipScenario(
        vessel_type="tanker",
        dimensions=minimal_passing_dimensions,
        track=minimal_passing_track,
    )


@pytest.fixture()
def minimal_environment():
    return EnvironmentSpec()


@pytest.fixture()
def minimal_spec(
    minimal_moored_vessel, minimal_passing_scenario, minimal_environment
):
    return PassingShipSpec(
        name="test_spec",
        moored_vessel=minimal_moored_vessel,
        passing_ship=minimal_passing_scenario,
        environment=minimal_environment,
    )


@pytest.fixture()
def full_spec_dict():
    """A complete spec dict as would be loaded from YAML."""
    return {
        "name": "tanker_passing_tanker",
        "description": "Tanker passing a moored VLCC at 5 knots",
        "moored_vessel": {
            "hull": {
                "loa": 320.0,
                "beam": 60.0,
                "draft": 20.0,
                "block_coefficient": 0.82,
            },
            "loading_condition": "full_load",
            "mooring_lines": [
                {
                    "line_id": "L1",
                    "line_type": "chain",
                    "length": 200.0,
                    "pretension": 500.0,
                    "fairlead_x": 150.0,
                    "fairlead_y": 30.0,
                    "anchor_x": 300.0,
                    "anchor_y": 100.0,
                }
            ],
        },
        "passing_ship": {
            "vessel_type": "tanker",
            "dimensions": {
                "loa": 250.0,
                "beam": 42.0,
                "draft": 16.0,
                "block_coefficient": 0.78,
            },
            "track": {
                "speed_knots": 5.0,
                "separation_m": 120.0,
                "angle_deg": 0.0,
                "closest_approach_offset_m": 0.0,
            },
        },
        "environment": {
            "water_depth": 35.0,
            "water_density": 1025.0,
        },
        "analysis_cases": [
            {
                "name": "base",
                "include_wind": False,
                "include_passing_ship": True,
            },
            {
                "name": "wind_passing",
                "include_wind": True,
                "include_passing_ship": True,
                "passing_speed_knots": 3.7,
                "separation_m": 80.0,
            },
        ],
        "outputs": {
            "line_tensions": True,
            "vessel_motions": True,
            "fender_loads": False,
            "force_time_histories": True,
        },
    }


# ---------------------------------------------------------------------------
# HullSpec tests
# ---------------------------------------------------------------------------


class TestHullSpec:
    def test_hull_spec_basic_construction(self):
        hull = HullSpec(loa=200.0, beam=32.0, draft=12.0)
        assert hull.loa == 200.0
        assert hull.beam == 32.0
        assert hull.draft == 12.0
        assert hull.block_coefficient == 0.75  # default

    def test_hull_spec_displacement_derived_when_not_given(self):
        hull = HullSpec(loa=100.0, beam=20.0, draft=10.0, block_coefficient=0.5)
        expected = 100.0 * 20.0 * 10.0 * 0.5 * 1.025
        assert abs(hull.displacement_tonnes - expected) < 1e-6

    def test_hull_spec_explicit_displacement_preserved(self):
        hull = HullSpec(loa=100.0, beam=20.0, draft=10.0, displacement_tonnes=9999.0)
        assert hull.displacement_tonnes == 9999.0

    def test_hull_spec_requires_positive_loa(self):
        with pytest.raises(Exception):
            HullSpec(loa=-10.0, beam=32.0, draft=12.0)

    def test_hull_spec_requires_positive_beam(self):
        with pytest.raises(Exception):
            HullSpec(loa=200.0, beam=0.0, draft=12.0)

    def test_hull_spec_requires_positive_draft(self):
        with pytest.raises(Exception):
            HullSpec(loa=200.0, beam=32.0, draft=-1.0)

    def test_hull_spec_block_coefficient_bounds(self):
        with pytest.raises(Exception):
            HullSpec(loa=200.0, beam=32.0, draft=12.0, block_coefficient=1.1)


# ---------------------------------------------------------------------------
# MooredVesselSpec tests
# ---------------------------------------------------------------------------


class TestMooredVesselSpec:
    def test_moored_vessel_defaults(self, minimal_hull):
        mv = MooredVesselSpec(hull=minimal_hull)
        assert mv.loading_condition == "full_load"
        assert mv.draft is None
        assert mv.mooring_lines == []

    def test_moored_vessel_effective_draft_uses_hull_when_no_override(
        self, minimal_hull
    ):
        mv = MooredVesselSpec(hull=minimal_hull)
        assert mv.effective_draft() == minimal_hull.draft

    def test_moored_vessel_effective_draft_uses_override(self, minimal_hull):
        mv = MooredVesselSpec(hull=minimal_hull, draft=10.0)
        assert mv.effective_draft() == 10.0

    def test_moored_vessel_invalid_loading_condition(self, minimal_hull):
        with pytest.raises(Exception):
            MooredVesselSpec(hull=minimal_hull, loading_condition="laden")

    def test_moored_vessel_ballast_loading_condition(self, minimal_hull):
        mv = MooredVesselSpec(hull=minimal_hull, loading_condition="ballast")
        assert mv.loading_condition == "ballast"

    def test_mooring_line_spec_construction(self):
        line = MooringLineSpec(
            line_id="L1",
            line_type="chain",
            length=200.0,
            pretension=500.0,
            fairlead_x=100.0,
            fairlead_y=15.0,
            anchor_x=200.0,
            anchor_y=80.0,
        )
        assert line.line_id == "L1"
        assert line.line_type == "chain"

    def test_mooring_line_invalid_type(self):
        with pytest.raises(Exception):
            MooringLineSpec(
                line_id="L1",
                line_type="rope",
                length=200.0,
                pretension=500.0,
                fairlead_x=100.0,
                fairlead_y=15.0,
                anchor_x=200.0,
                anchor_y=80.0,
            )


# ---------------------------------------------------------------------------
# PassingTrack tests
# ---------------------------------------------------------------------------


class TestPassingTrack:
    def test_passing_track_basic_construction(self):
        track = PassingTrack(speed_knots=5.0, separation_m=100.0)
        assert track.speed_knots == 5.0
        assert track.separation_m == 100.0
        assert track.angle_deg == 0.0

    def test_passing_track_speed_must_be_positive(self):
        with pytest.raises(Exception):
            PassingTrack(speed_knots=0.0, separation_m=100.0)

    def test_passing_track_speed_negative_rejected(self):
        with pytest.raises(Exception):
            PassingTrack(speed_knots=-5.0, separation_m=100.0)

    def test_passing_track_separation_must_be_positive(self):
        with pytest.raises(Exception):
            PassingTrack(speed_knots=5.0, separation_m=0.0)

    def test_passing_track_speed_ms_property(self):
        track = PassingTrack(speed_knots=1.0, separation_m=50.0)
        assert abs(track.speed_ms - 0.514444) < 1e-4

    def test_passing_track_5_knots_in_ms(self):
        track = PassingTrack(speed_knots=5.0, separation_m=100.0)
        assert abs(track.speed_ms - 5 * 0.514444) < 1e-4


# ---------------------------------------------------------------------------
# EnvironmentSpec tests
# ---------------------------------------------------------------------------


class TestEnvironmentSpec:
    def test_environment_default_infinite(self):
        env = EnvironmentSpec()
        assert env.water_depth == "infinite"
        assert env.is_infinite_depth is True

    def test_environment_finite_depth_accepted(self):
        env = EnvironmentSpec(water_depth=30.0)
        assert env.water_depth == 30.0
        assert env.is_infinite_depth is False

    def test_environment_infinite_string_accepted(self):
        env = EnvironmentSpec(water_depth="infinite")
        assert env.water_depth == "infinite"

    def test_environment_inf_string_normalised(self):
        env = EnvironmentSpec(water_depth="inf")
        assert env.water_depth == "infinite"

    def test_environment_deep_string_normalised(self):
        env = EnvironmentSpec(water_depth="deep")
        assert env.water_depth == "infinite"

    def test_environment_zero_depth_rejected(self):
        with pytest.raises(Exception):
            EnvironmentSpec(water_depth=0.0)

    def test_environment_negative_depth_rejected(self):
        with pytest.raises(Exception):
            EnvironmentSpec(water_depth=-5.0)

    def test_environment_invalid_string_rejected(self):
        with pytest.raises(Exception):
            EnvironmentSpec(water_depth="very_deep")

    def test_environment_optional_wind_and_current(self):
        env = EnvironmentSpec(
            wind_speed=10.0,
            wind_direction_deg=270.0,
            current_speed=0.5,
            current_direction_deg=90.0,
        )
        assert env.wind_speed == 10.0
        assert env.current_speed == 0.5


# ---------------------------------------------------------------------------
# AnalysisCase tests
# ---------------------------------------------------------------------------


class TestAnalysisCase:
    def test_analysis_case_defaults(self):
        case = AnalysisCase(name="base")
        assert case.include_wind is False
        assert case.include_passing_ship is True
        assert case.passing_speed_knots is None
        assert case.separation_m is None

    def test_analysis_case_speed_override(self):
        case = AnalysisCase(name="slow", passing_speed_knots=3.0)
        assert case.passing_speed_knots == 3.0

    def test_analysis_case_separation_override(self):
        case = AnalysisCase(name="close", separation_m=50.0)
        assert case.separation_m == 50.0

    def test_analysis_case_zero_speed_rejected(self):
        with pytest.raises(Exception):
            AnalysisCase(name="bad", passing_speed_knots=0.0)


# ---------------------------------------------------------------------------
# PassingShipSpec top-level tests
# ---------------------------------------------------------------------------


class TestPassingShipSpec:
    def test_spec_construction_minimal(self, minimal_spec):
        assert minimal_spec.name == "test_spec"
        assert minimal_spec.description == ""
        assert isinstance(minimal_spec.outputs, OutputSpec)

    def test_spec_from_dict(self, full_spec_dict):
        spec = PassingShipSpec.from_dict(full_spec_dict)
        assert spec.name == "tanker_passing_tanker"
        assert spec.passing_ship.track.speed_knots == 5.0
        assert spec.environment.water_depth == 35.0
        assert len(spec.analysis_cases) == 2

    def test_spec_to_dict_roundtrip(self, full_spec_dict):
        spec = PassingShipSpec.from_dict(full_spec_dict)
        d = spec.to_dict()
        assert d["name"] == full_spec_dict["name"]
        assert (
            d["passing_ship"]["track"]["speed_knots"]
            == full_spec_dict["passing_ship"]["track"]["speed_knots"]
        )

    def test_spec_yaml_roundtrip(self, minimal_spec):
        with tempfile.TemporaryDirectory() as tmp:
            yaml_path = Path(tmp) / "spec.yml"
            minimal_spec.to_yaml(yaml_path)
            loaded = PassingShipSpec.from_yaml(yaml_path)
        assert loaded.name == minimal_spec.name
        assert (
            loaded.moored_vessel.hull.loa
            == minimal_spec.moored_vessel.hull.loa
        )

    def test_spec_yaml_file_content_is_valid_yaml(self, minimal_spec):
        with tempfile.TemporaryDirectory() as tmp:
            yaml_path = Path(tmp) / "spec.yml"
            minimal_spec.to_yaml(yaml_path)
            with open(yaml_path) as fh:
                data = yaml.safe_load(fh)
        assert isinstance(data, dict)
        assert "name" in data

    def test_spec_full_yaml_roundtrip(self, full_spec_dict):
        spec = PassingShipSpec.from_dict(full_spec_dict)
        with tempfile.TemporaryDirectory() as tmp:
            yaml_path = Path(tmp) / "spec.yml"
            spec.to_yaml(yaml_path)
            loaded = PassingShipSpec.from_yaml(yaml_path)
        assert loaded.name == spec.name
        assert len(loaded.analysis_cases) == len(spec.analysis_cases)
        assert loaded.moored_vessel.hull.block_coefficient == pytest.approx(
            spec.moored_vessel.hull.block_coefficient
        )

    def test_spec_missing_name_rejected(self, minimal_moored_vessel, minimal_passing_scenario, minimal_environment):
        with pytest.raises(Exception):
            PassingShipSpec(
                moored_vessel=minimal_moored_vessel,
                passing_ship=minimal_passing_scenario,
                environment=minimal_environment,
            )

    def test_spec_vessel_type_validated(self, minimal_hull, minimal_passing_track):
        with pytest.raises(Exception):
            PassingShipScenario(
                vessel_type="ferry",
                dimensions=PassingShipDimensions(loa=100.0, beam=20.0, draft=5.0),
                track=minimal_passing_track,
            )

    def test_spec_outputs_defaults(self, minimal_spec):
        assert minimal_spec.outputs.line_tensions is True
        assert minimal_spec.outputs.vessel_motions is True
        assert minimal_spec.outputs.fender_loads is False
        assert minimal_spec.outputs.force_time_histories is True

    def test_spec_analysis_cases_list_defaults_empty(self, minimal_spec):
        assert minimal_spec.analysis_cases == []

    def test_spec_mooring_lines_in_full_spec(self, full_spec_dict):
        spec = PassingShipSpec.from_dict(full_spec_dict)
        assert len(spec.moored_vessel.mooring_lines) == 1
        assert spec.moored_vessel.mooring_lines[0].line_id == "L1"

    def test_spec_environment_infinite_depth_from_dict(self):
        d = {
            "name": "deep_water",
            "moored_vessel": {
                "hull": {"loa": 200.0, "beam": 32.0, "draft": 12.0}
            },
            "passing_ship": {
                "dimensions": {"loa": 200.0, "beam": 32.0, "draft": 12.0},
                "track": {"speed_knots": 5.0, "separation_m": 100.0},
            },
            "environment": {"water_depth": "infinite"},
        }
        spec = PassingShipSpec.from_dict(d)
        assert spec.environment.is_infinite_depth is True
