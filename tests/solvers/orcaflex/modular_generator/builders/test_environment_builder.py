"""Tests for EnvironmentBuilder property mapping and edge cases."""
from __future__ import annotations

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.builders.context import (
    BuilderContext,
)
from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
    EnvironmentBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_env_spec(
    waves=None,
    current=None,
    wind=None,
    water=None,
    seabed=None,
    raw_properties=None,
):
    """Create a minimal pipeline spec with configurable environment."""
    env = {
        "water": water or {"depth": 100, "density": 1.025},
        "seabed": seabed or {"slope": 0, "stiffness": {"normal": 100, "shear": 50}},
    }
    if waves is not None:
        env["waves"] = waves
    if current is not None:
        env["current"] = current
    if wind is not None:
        env["wind"] = wind
    if raw_properties is not None:
        env["raw_properties"] = raw_properties

    return ProjectInputSpec(**{
        "metadata": {
            "name": "test",
            "description": "test",
            "structure": "pipeline",
            "operation": "installation",
            "project": "TEST",
        },
        "environment": env,
        "pipeline": {
            "name": "test_pipe",
            "material": "X65",
            "dimensions": {"outer_diameter": 0.3, "wall_thickness": 0.015},
            "coatings": {"corrosion": {"thickness": 0.003, "density": 1.1}},
            "segments": [{"type": "test", "length": 1000, "segment_length": 0.5}],
        },
    })


def _build(spec):
    """Run the builder and return the Environment dict."""
    builder = EnvironmentBuilder(spec, BuilderContext())
    result = builder.build()
    return result["Environment"]


# ---------------------------------------------------------------------------
# Water depth and density
# ---------------------------------------------------------------------------

class TestWaterProperties:
    def test_water_depth_default_key(self):
        """Without raw WaterDepth, depth is emitted as SeabedOriginDepth."""
        env = _build(_make_env_spec())
        assert env["SeabedOriginDepth"] == 100
        assert "WaterDepth" not in env

    def test_water_depth_with_raw_water_depth(self):
        """When raw_properties has WaterDepth, builder uses WaterDepth key."""
        env = _build(_make_env_spec(raw_properties={"WaterDepth": 100}))
        assert env["WaterDepth"] == 100
        assert "SeabedOriginDepth" not in env

    def test_water_density(self):
        env = _build(_make_env_spec(water={"depth": 50, "density": 1.03}))
        assert env["Density"] == 1.03


# ---------------------------------------------------------------------------
# Seabed
# ---------------------------------------------------------------------------

class TestSeabedProperties:
    def test_seabed_slope(self):
        env = _build(_make_env_spec(seabed={"slope": 2.5, "stiffness": {"normal": 200, "shear": 80}}))
        assert env["SeabedSlope"] == 2.5

    def test_seabed_stiffness(self):
        env = _build(_make_env_spec(seabed={"slope": 0, "stiffness": {"normal": 300, "shear": 150}}))
        assert env["SeabedNormalStiffness"] == 300
        assert env["SeabedShearStiffness"] == 150


# ---------------------------------------------------------------------------
# Waves — single train
# ---------------------------------------------------------------------------

class TestSingleWaveTrain:
    def test_jonswap_uses_hs_tz(self):
        """JONSWAP wave type emits WaveHs and WaveTz (spectral params)."""
        env = _build(_make_env_spec(waves={"type": "jonswap", "height": 3.0, "period": 8.0, "direction": 180}))
        trains = env["WaveTrains"]
        assert len(trains) == 1
        assert trains[0]["WaveType"] == "JONSWAP"
        assert trains[0]["WaveHs"] == 3.0
        assert trains[0]["WaveTz"] == 8.0
        assert trains[0]["WaveDirection"] == 180
        assert "WaveHeight" not in trains[0]
        assert "WavePeriod" not in trains[0]

    def test_dean_stream_uses_height_period(self):
        """Dean stream emits WaveHeight and WavePeriod (deterministic params)."""
        env = _build(_make_env_spec(waves={"type": "dean_stream", "height": 5.0, "period": 10.0, "direction": 0}))
        trains = env["WaveTrains"]
        assert len(trains) == 1
        assert trains[0]["WaveType"] == "Dean stream"
        assert trains[0]["WaveHeight"] == 5.0
        assert trains[0]["WavePeriod"] == 10.0
        assert trains[0]["WaveStreamFunctionOrder"] == 5
        assert "WaveHs" not in trains[0]
        assert "WaveTz" not in trains[0]

    def test_airy_uses_height_period(self):
        env = _build(_make_env_spec(waves={"type": "airy", "height": 2.0, "period": 6.0, "direction": 90}))
        trains = env["WaveTrains"]
        assert trains[0]["WaveType"] == "Airy"
        assert trains[0]["WaveHeight"] == 2.0
        assert trains[0]["WavePeriod"] == 6.0

    def test_stokes_5th_mapping(self):
        env = _build(_make_env_spec(waves={"type": "stokes_5th", "height": 4.0, "period": 9.0, "direction": 0}))
        trains = env["WaveTrains"]
        assert trains[0]["WaveType"] == "Stokes' 5th"
        assert trains[0]["WaveHeight"] == 4.0

    def test_pierson_moskowitz_spectral(self):
        env = _build(_make_env_spec(waves={"type": "pierson_moskowitz", "height": 2.5, "period": 7.0, "direction": 270}))
        trains = env["WaveTrains"]
        assert trains[0]["WaveType"] == "Pierson-Moskowitz"
        assert trains[0]["WaveHs"] == 2.5
        assert trains[0]["WaveTz"] == 7.0

    def test_default_waves(self):
        """No wave spec produces default airy train with height=0."""
        env = _build(_make_env_spec())
        trains = env["WaveTrains"]
        assert len(trains) == 1
        assert trains[0]["WaveType"] == "Airy"
        assert trains[0]["WaveHeight"] == 0


# ---------------------------------------------------------------------------
# Waves — multi-train
# ---------------------------------------------------------------------------

class TestMultiWaveTrain:
    def test_two_trains(self):
        """Multiple wave trains produce multiple WaveTrains entries."""
        env = _build(_make_env_spec(waves={
            "trains": [
                {"name": "Wind sea", "type": "jonswap", "height": 3.0, "period": 8, "direction": 0},
                {"name": "Swell", "type": "airy", "height": 1.5, "period": 14, "direction": 180},
            ]
        }))
        trains = env["WaveTrains"]
        assert len(trains) == 2
        assert trains[0]["WaveType"] == "JONSWAP"
        assert trains[0]["WaveHs"] == 3.0
        assert trains[0]["Name"] == "Wind sea"
        assert trains[1]["WaveType"] == "Airy"
        assert trains[1]["WaveHeight"] == 1.5
        assert trains[1]["WaveDirection"] == 180

    def test_three_trains_all_types(self):
        """Mix of spectral and deterministic trains."""
        env = _build(_make_env_spec(waves={
            "trains": [
                {"name": "A", "type": "jonswap", "height": 2.0, "period": 7, "direction": 0},
                {"name": "B", "type": "airy", "height": 1.0, "period": 5, "direction": 90},
                {"name": "C", "type": "dean_stream", "height": 4.0, "period": 11, "direction": 45},
            ]
        }))
        trains = env["WaveTrains"]
        assert len(trains) == 3
        assert trains[0]["WaveHs"] == 2.0
        assert trains[1]["WaveHeight"] == 1.0
        assert trains[2]["WaveHeight"] == 4.0
        assert trains[2]["WaveStreamFunctionOrder"] == 5


# ---------------------------------------------------------------------------
# Current profile
# ---------------------------------------------------------------------------

class TestCurrentProfile:
    def test_multi_level_profile(self):
        """Profile with 3 levels emits depth/factor/rotation triplets."""
        env = _build(_make_env_spec(current={
            "speed": 1.5,
            "direction": 180,
            "profile": [[0, 1.0], [50, 0.7], [100, 0.3]],
        }))
        assert env["RefCurrentSpeed"] == 1.5
        assert env["RefCurrentDirection"] == 180
        profile = env["CurrentDepth, CurrentFactor, CurrentRotation"]
        assert len(profile) == 3
        assert profile[0] == [0, 1.0, 0]
        assert profile[1] == [50, 0.7, 0]
        assert profile[2] == [100, 0.3, 0]

    def test_single_level_padded_to_two(self):
        """OrcaFlex requires >= 2 current levels; single level gets padded."""
        env = _build(_make_env_spec(current={
            "speed": 0.5,
            "direction": 90,
            "profile": [[0, 1.0]],
        }))
        profile = env["CurrentDepth, CurrentFactor, CurrentRotation"]
        assert len(profile) == 2
        # Padded point at water depth (100) with same factor
        assert profile[0] == [0, 1.0, 0]
        assert profile[1][0] == 100  # water depth
        assert profile[1][1] == 1.0
        assert profile[1][2] == 0

    def test_single_level_at_seabed_padded_with_surface(self):
        """Single point at seabed depth gets surface point inserted."""
        env = _build(_make_env_spec(current={
            "speed": 0.5,
            "direction": 0,
            "profile": [[100, 0.5]],
        }))
        profile = env["CurrentDepth, CurrentFactor, CurrentRotation"]
        assert len(profile) == 2
        assert profile[0] == [0, 0.5, 0]  # Surface point inserted
        assert profile[1] == [100, 0.5, 0]

    def test_two_level_profile_not_padded(self):
        """Two-level profile passes through without padding."""
        env = _build(_make_env_spec(current={
            "speed": 1.0,
            "direction": 0,
            "profile": [[0, 1.0], [100, 0.5]],
        }))
        profile = env["CurrentDepth, CurrentFactor, CurrentRotation"]
        assert len(profile) == 2

    def test_default_current_profile(self):
        """Default current has a single-level profile that gets padded."""
        env = _build(_make_env_spec())
        profile = env["CurrentDepth, CurrentFactor, CurrentRotation"]
        assert len(profile) == 2


# ---------------------------------------------------------------------------
# Wind
# ---------------------------------------------------------------------------

class TestWindProperties:
    def test_wind_speed_and_direction(self):
        env = _build(_make_env_spec(wind={"speed": 15, "direction": 225}))
        assert env["WindSpeed"] == 15
        assert env["WindDirection"] == 225

    def test_default_wind(self):
        env = _build(_make_env_spec())
        assert env["WindSpeed"] == 0
        assert env["WindDirection"] == 0

    def test_wind_speed_dormant_for_full_field(self):
        """WindSpeed should not be emitted for Full field wind type."""
        env = _build(_make_env_spec(
            raw_properties={"WindType": "Full field"},
            wind={"speed": 10, "direction": 180},
        ))
        assert "WindSpeed" not in env
        assert env["WindDirection"] == 180
        assert env["WindType"] == "Full field"


# ---------------------------------------------------------------------------
# Raw properties overlay
# ---------------------------------------------------------------------------

class TestRawPropertiesOverlay:
    def test_safe_key_overlay(self):
        """Safe keys from raw_properties are overlaid into output."""
        env = _build(_make_env_spec(raw_properties={
            "KinematicViscosity": 1.5e-06,
            "SeaTemperature": 15,
        }))
        assert env["KinematicViscosity"] == 1.5e-06
        assert env["SeaTemperature"] == 15

    def test_unsafe_key_not_overlaid(self):
        """Keys not in _SAFE_RAW_OVERLAY_KEYS should not appear from raw."""
        env = _build(_make_env_spec(raw_properties={
            "SomeUnsafeKey": "value",
        }))
        assert "SomeUnsafeKey" not in env

    def test_spec_values_override_raw(self):
        """Spec-derived values always take precedence over raw overlay."""
        env = _build(_make_env_spec(
            water={"depth": 200, "density": 1.03},
            raw_properties={"WaterDepth": 999},
        ))
        # Even though raw has WaterDepth=999, spec water.depth=200 wins
        assert env["WaterDepth"] == 200

    def test_wind_type_dependent_props(self):
        """Wind-type-dependent properties are emitted when type matches."""
        env = _build(_make_env_spec(raw_properties={
            "WindType": "NPD spectrum",
            "WindSpectrumElevation": 10,
            "NumberOfWindComponents": 200,
        }))
        assert env["WindType"] == "NPD spectrum"
        assert env["WindSpectrumElevation"] == 10
        assert env["NumberOfWindComponents"] == 200


# ---------------------------------------------------------------------------
# Defaults
# ---------------------------------------------------------------------------

class TestDefaults:
    def test_default_seabed_model(self):
        env = _build(_make_env_spec())
        assert env["SeabedModel"] == "Elastic"

    def test_default_current_model(self):
        env = _build(_make_env_spec())
        assert env["CurrentModel"] == "Variation scheme"

    def test_default_wind_type(self):
        env = _build(_make_env_spec())
        assert env["WindType"] == "Constant"

    def test_default_air_density(self):
        env = _build(_make_env_spec())
        assert env["AirDensity"] == pytest.approx(0.00128)
