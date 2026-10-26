# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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
from pathlib import Path


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
# Waves — raw base layer fidelity
# ---------------------------------------------------------------------------

class TestRawWaveTrainFidelity:
    def test_raw_trains_beyond_spec_count_pass_through(self):
        """Raw trains beyond the spec's train count must not be dropped."""
        swell_raw = {
            "Name": "Swell",
            "WaveType": "Airy",
            "WaveHeight": 1.5,
            "WavePeriod": 14.0,
            "WaveDirection": 180,
            "WaveOrigin": [0, 0],
            "WaveTimeOrigin": 0,
        }
        env = _build(_make_env_spec(
            waves={"type": "jonswap", "height": 3.0, "period": 8.0, "direction": 0},
            raw_properties={"WaveTrains": [
                {
                    "Name": "WindSea",
                    "WaveType": "JONSWAP",
                    "WaveHs": 3.0,
                    "WaveTz": 8.0,
                    "WaveDirection": 0,
                },
                swell_raw,
            ]},
        ))
        trains = env["WaveTrains"]
        assert len(trains) == 2
        assert trains[1] == swell_raw

    def test_tp_specified_raw_train_keeps_tp_key(self):
        """A Tp-only raw JONSWAP train (e.g. docs/domains/orcaflex/risers/
        drilling/gom/app_vertical_riser_2500ft_WT1000_064pcf_Wave1_FE.yml)
        must be re-emitted as WaveTp, not WaveTz (which would shift Tp)."""
        env = _build(_make_env_spec(
            waves={"type": "jonswap", "height": 3.0, "period": 10.0, "direction": 0},
            raw_properties={"WaveTrains": [
                {
                    "Name": "Wave1",
                    "WaveType": "JONSWAP",
                    "WaveHs": 3.0,
                    "WaveTp": 10.0,
                    "WaveDirection": 0,
                },
            ]},
        ))
        train = env["WaveTrains"][0]
        assert train["WaveTp"] == 10.0
        assert "WaveTz" not in train

    def test_tz_specified_raw_train_drops_stale_tp(self):
        """When emitting WaveTz, any stale WaveTp from the raw base layer is
        removed so the output never has contradictory period keys."""
        env = _build(_make_env_spec(
            waves={"type": "jonswap", "height": 3.0, "period": 8.0, "direction": 0},
            raw_properties={"WaveTrains": [
                {
                    "Name": "Wave1",
                    "WaveType": "JONSWAP",
                    "WaveHs": 3.0,
                    "WaveTz": 8.0,
                    "WaveTp": 10.3,
                    "WaveDirection": 0,
                },
            ]},
        ))
        train = env["WaveTrains"][0]
        assert train["WaveTz"] == 8.0
        assert "WaveTp" not in train

    def test_raw_wave_gamma_preserved_when_spec_gamma_unset(self):
        """Raw WaveGamma (e.g. 5.0, 'Fully specified') must not be clobbered
        by the spec default 3.3 when the spec never set gamma."""
        env = _build(_make_env_spec(
            waves={"type": "jonswap", "height": 3.0, "period": 8.0, "direction": 0},
            raw_properties={"WaveTrains": [
                {
                    "Name": "Wave1",
                    "WaveType": "JONSWAP",
                    "WaveJONSWAPParameters": "Fully specified",
                    "WaveHs": 3.0,
                    "WaveTz": 8.0,
                    "WaveGamma": 5.0,
                },
            ]},
        ))
        assert env["WaveTrains"][0]["WaveGamma"] == 5.0

    def test_explicit_spec_gamma_overrides_raw(self):
        """An explicitly set spec gamma remains authoritative over raw."""
        env = _build(_make_env_spec(
            waves={
                "type": "jonswap", "height": 3.0, "period": 8.0,
                "direction": 0, "gamma": 2.5,
            },
            raw_properties={"WaveTrains": [
                {
                    "Name": "Wave1",
                    "WaveType": "JONSWAP",
                    "WaveJONSWAPParameters": "Fully specified",
                    "WaveHs": 3.0,
                    "WaveTz": 8.0,
                    "WaveGamma": 5.0,
                },
            ]},
        ))
        assert env["WaveTrains"][0]["WaveGamma"] == 2.5

    def test_default_gamma_emitted_without_raw_train(self):
        """Hand-crafted specs (no raw base) still emit the default gamma."""
        env = _build(_make_env_spec(
            waves={"type": "jonswap", "height": 3.0, "period": 8.0, "direction": 0},
        ))
        assert env["WaveTrains"][0]["WaveGamma"] == 3.3

    def test_dean_stream_raw_values_preserved(self):
        """Raw Dean stream order/current-speed must round-trip, not be
        overwritten with the hardcoded defaults (5 / None)."""
        env = _build(_make_env_spec(
            waves={"type": "dean_stream", "height": 5.0, "period": 10.0, "direction": 0},
            raw_properties={"WaveTrains": [
                {
                    "Name": "Wave1",
                    "WaveType": "Dean stream",
                    "WaveHeight": 5.0,
                    "WavePeriod": 10.0,
                    "WaveStreamFunctionOrder": 9,
                    "WaveCurrentSpeedInWaveDirectionAtMeanWaterLevel": 1.5,
                },
            ]},
        ))
        train = env["WaveTrains"][0]
        assert train["WaveStreamFunctionOrder"] == 9
        assert train["WaveCurrentSpeedInWaveDirectionAtMeanWaterLevel"] == 1.5


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

    def test_raw_rotation_preserved(self):
        """Non-zero CurrentRotation from the raw profile must round-trip
        instead of being re-emitted as 0."""
        env = _build(_make_env_spec(
            current={
                "speed": 1.0,
                "direction": 0,
                "profile": [[0, 1.0], [50, 0.8], [100, 0.3]],
            },
            raw_properties={
                "CurrentDepth, CurrentFactor, CurrentRotation": [
                    [0, 1.0, 0], [50, 0.8, 30], [100, 0.3, 45],
                ],
            },
        ))
        profile = env["CurrentDepth, CurrentFactor, CurrentRotation"]
        assert profile == [[0, 1.0, 0], [50, 0.8, 30], [100, 0.3, 45]]

    def test_raw_rotation_ignored_when_profile_mismatch(self):
        """If the spec profile no longer matches the raw rows (user edited
        the spec), rotation falls back to 0 rather than mispairing."""
        env = _build(_make_env_spec(
            current={
                "speed": 1.0,
                "direction": 0,
                "profile": [[0, 1.0], [60, 0.5]],
            },
            raw_properties={
                "CurrentDepth, CurrentFactor, CurrentRotation": [
                    [0, 1.0, 0], [50, 0.8, 30],
                ],
            },
        ))
        profile = env["CurrentDepth, CurrentFactor, CurrentRotation"]
        assert profile == [[0, 1.0, 0], [60, 0.5, 0]]


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


# Literal membership pins copied from the pre-change builder declarations.
_PRE_CHANGE_OVERLAY_KEYS = {
    "WaterSurfaceZ", "KinematicViscosity", "SeaTemperature",
    "ReynoldsNumberCalculation", "HorizontalWaterDensityFactor",
    "VerticalDensityVariation", "SeabedType", "SeabedOrigin", "WaterDepth",
    "SeabedSlopeDirection", "SeabedSlope", "SeabedModel",
    "SeabedNormalStiffness", "SeabedShearStiffness", "KinematicStretchingMethod",
    "UserSpecifiedRandomWaveSeeds", "WaveFrequencySpectrumDiscretisationMethod",
    "WaveKinematicsCutoffDepth", "WaveCalculationMethod",
    "WaveCalculationTimeInterval", "WaveCalculationSpatialInterval",
    "MultipleCurrentDataCanBeDefined", "CurrentModel", "CurrentRamped",
    "CurrentApplyVerticalStretching", "HorizontalCurrentFactor",
    "VerticalCurrentVariationMethod", "IncludeVesselWindLoads",
    "IncludeLineWindLoads", "IncludeBuoyWindLoads", "IncludeBuoyWingWindLoads",
    "WindRamping", "AirDensity", "AirSpeedOfSound",
}
_PRE_CHANGE_WIND_KEYS = {
    "API spectrum": {
        "VerticalWindVariationFactor", "WindSpectrumElevation",
        "WindSpectrumFMin", "WindSpectrumFMax", "WindTimeOrigin", "WindSeed",
        "NumberOfWindComponents",
    },
    "NPD spectrum": {
        "VerticalWindVariationFactor", "WindSpectrumElevation",
        "WindSpectrumFMin", "WindSpectrumFMax", "WindTimeOrigin", "WindSeed",
        "NumberOfWindComponents",
    },
    "ESDU spectrum": {
        "VerticalWindVariationFactor", "WindSpectrumElevation",
        "WindSpectrumFMin", "WindSpectrumFMax", "WindTimeOrigin", "WindSeed",
        "NumberOfWindComponents",
    },
    "Full field": {
        "WindFullFieldFormat", "WindFullFieldTurbSimFileName", "WindTimeOrigin",
        "WindOrigin",
    },
}


def _hash_seed_raw_properties():
    """Exercise appended overlay and API-spectrum keys in reversed input order."""
    return {
        "NumberOfWindComponents": 100, "WindSeed": 123, "WindTimeOrigin": 0,
        "WindSpectrumFMax": 1, "WindSpectrumFMin": 0.01,
        "WindSpectrumElevation": 10, "VerticalWindVariationFactor": 1,
        "WindType": "API spectrum",
        "WaveFrequencySpectrumDiscretisationMethod": "Equal energy",
        "UserSpecifiedRandomWaveSeeds": "Yes",
        "KinematicStretchingMethod": "Vertical stretching",
        "SeabedShearStiffness": 50, "SeabedNormalStiffness": 100,
        "SeabedSlope": 0, "WaterDepth": 100,
    }


def test_environment_membership_regression_pin():
    """Pin overlay membership, wind modes and absence of duplicate entries."""
    overlay = EnvironmentBuilder._SAFE_RAW_OVERLAY_KEYS
    assert set(overlay) == _PRE_CHANGE_OVERLAY_KEYS
    assert len(overlay) == len(set(overlay))
    wind_props = EnvironmentBuilder._WIND_TYPE_PROPS
    assert set(wind_props) == set(_PRE_CHANGE_WIND_KEYS)
    for wind_type, expected in _PRE_CHANGE_WIND_KEYS.items():
        assert set(wind_props[wind_type]) == expected
        assert len(wind_props[wind_type]) == len(set(wind_props[wind_type]))


def test_environment_overlay_collections_are_ordered_tuples():
    """Require explicit ordering for both iterated overlay collections."""
    assert isinstance(EnvironmentBuilder._SAFE_RAW_OVERLAY_KEYS, tuple)
    assert all(isinstance(keys, tuple)
               for keys in EnvironmentBuilder._WIND_TYPE_PROPS.values())


def test_environment_appended_overlay_key_order():
    """Append non-default raw keys in declaration order, before wind fields."""
    raw = _hash_seed_raw_properties()
    env = _build(_make_env_spec(raw_properties=raw))
    appended_raw_keys = [key for key in env
                         if key in raw and key not in EnvironmentBuilder._DEFAULTS]
    assert appended_raw_keys == [
        "WaterDepth", "SeabedSlope", "SeabedNormalStiffness",
        "SeabedShearStiffness", "KinematicStretchingMethod",
        "UserSpecifiedRandomWaveSeeds", "WaveFrequencySpectrumDiscretisationMethod",
        "VerticalWindVariationFactor", "WindSpectrumElevation",
        "WindSpectrumFMin", "WindSpectrumFMax", "WindTimeOrigin", "WindSeed",
        "NumberOfWindComponents",
    ]
    keys = list(env)
    assert keys.index("WindType") < keys.index("VerticalWindVariationFactor")
    assert keys.index("SeabedType") < keys.index("WaterDepth")
    assert keys.index("SeabedModel") < keys.index("SeabedNormalStiffness")


def test_environment_serialized_bytes_across_hash_seeds(tmp_path):
    """Compare production YAML bytes across five separate Python processes.

    At BASE, seeds 1, 2, 42 and 12345 differed from seed 0.
    """
    import os
    from pathlib import Path
    import subprocess
    import sys

    repo_root = Path(__file__).resolve().parents[5]
    spec = _make_env_spec(raw_properties=_hash_seed_raw_properties())
    spec_input = spec.model_dump_json().encode("utf-8")
    assert ProjectInputSpec.model_validate_json(spec_input).environment == spec.environment
    child = """
import sys
import yaml
from digitalmodel.solvers.orcaflex.modular_generator import (
    BuilderContext, EnvironmentBuilder, ProjectInputSpec, _NoAliasDumper,
)
spec = ProjectInputSpec.model_validate_json(sys.stdin.buffer.read())
data = EnvironmentBuilder(spec, BuilderContext()).build()
text = yaml.dump(data, Dumper=_NoAliasDumper, default_flow_style=False,
                 allow_unicode=True, sort_keys=False)
sys.stdout.buffer.write(text.encode('utf-8'))
"""
    outputs = {}
    for seed in (0, 1, 2, 42, 12345):
        child_env = {**os.environ, "PYTHONHASHSEED": str(seed),
                     "PYTHONDONTWRITEBYTECODE": "1",
                     "PYTHONPATH": str(repo_root / "src")}
        for binding in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
            child_env.pop(binding, None)
        result = subprocess.run(
            [sys.executable, "-c", child], env=child_env, input=spec_input,
            cwd=tmp_path, capture_output=True, timeout=600, check=True,
            creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0),
        )
        outputs[seed] = result.stdout
    differing = [seed for seed, data in outputs.items() if data != outputs[0]]
    assert not differing, f"Serialized bytes differ from seed 0 for seeds {differing}"


@pytest.mark.parametrize("wave_type,raw_train", [
    ("jonswap", {"WaveTp": 12.9, "WaveGamma": 3.3,
                 "WaveJONSWAPParameters": "Partially specified", "WaveType": "JONSWAP",
                 "WaveHs": 11.2}),
    ("jonswap", {"WaveTp": 12.9, "WaveGamma": 3.3, "WaveType": "JONSWAP"}),
    ("jonswap", {"WaveTz": 12.9, "WaveGamma": 3.3, "WaveType": "JONSWAP"}),
    ("jonswap", {"WaveTz": 12.9, "WaveGamma": 3.3,
                 "WaveJONSWAPParameters": "Partially specified", "WaveType": "JONSWAP"}),
    ("airy", {"WavePeriod": 12.9, "WaveHeight": 11.2, "WaveType": "Airy"}),
    ("dean_stream", {"WaveStreamFunctionOrder": 5, "WavePeriod": 12.9,
                     "WaveHeight": 11.2, "WaveType": "Dean stream"}),
])
def test_wave_selectors_precede_raw_parameters_in_fresh_process(tmp_path, wave_type, raw_train):
    """Typed and extra raw trains shall place wave selectors before parameters.

    Replace Fully specified with Hs by Partially specified with gamma and Tp
    in inverted raw order to exercise the gamma-before-Tp dependency.
    """
    import os
    import subprocess
    import sys

    import yaml

    spec = _make_env_spec(
        waves={"type": wave_type, "height": 11.2, "period": 12.9},
        raw_properties={"WaveTrains": [raw_train, raw_train]},
    )
    child = """
import sys
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.materials import load_catalog, use_catalog
spec = ProjectInputSpec.model_validate_json(sys.stdin.buffer.read())
with use_catalog(load_catalog(sys.argv[2]), allow_synthetic=True):
    ModularModelGenerator.from_spec(spec).generate(sys.argv[1])
"""
    environment = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        environment.pop(key, None)
    subprocess.run(
        [sys.executable, "-c", child, str(tmp_path), str(
            Path(__file__).resolve().parents[4] / "materials/fixtures/catalog.json"
        )], env=environment,
        input=spec.model_dump_json().encode(), capture_output=True, check=True,
    )
    data = yaml.safe_load((tmp_path / "includes/03_environment.yml").read_text())
    trains = data["Environment"]["WaveTrains"]
    assert len(trains) == 2
    for train in trains:
        keys = list(train)
        assert keys.index("WaveType") < min(
            keys.index(key) for key in keys if key.startswith("Wave") and key != "WaveType"
        )
        if "WaveJONSWAPParameters" in keys:
            assert keys.index("WaveJONSWAPParameters") < keys.index("WaveGamma")
        if train["WaveType"] == "JONSWAP" and "WaveGamma" in keys:
            assert train["WaveJONSWAPParameters"] == "Partially specified"
            assert train["WaveGamma"] == raw_train["WaveGamma"]
            period = "WaveTp" if "WaveTp" in raw_train else "WaveTz"
            assert train[period] == raw_train[period]
            assert keys.index("WaveGamma") < keys.index(period)
        if "WaveTp" in keys and "WaveGamma" in keys:
            assert keys.index("WaveGamma") < keys.index("WaveTp")


def test_schema_rejects_no_waves_type():
    """Reject the native-invalid selector before generation."""
    with pytest.raises(ValueError, match="No waves"):
        _make_env_spec(waves={"type": "No waves", "height": 0, "period": 8})


def test_validator_rejects_invalid_extra_raw_wave_train():
    """Raw extra trains remain pass-through but must fail post-validation."""
    from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
        PostGenerationValidator,
    )

    spec = _make_env_spec(
        waves={"type": "Airy", "height": 0, "period": 8},
        raw_properties={"WaveTrains": [
            {"Name": "Wave 1", "WaveType": "Airy"},
            {"Name": "Wave 2", "WaveType": "No waves"},
        ]},
    )
    environment = _build(spec)
    findings = PostGenerationValidator().validate_data({
        "environment.yml": {"Environment": environment},
    })
    assert len(findings) == 1
    assert findings[0].level == "error"
    assert findings[0].category == "wave_type"
    assert findings[0].object_name == "Wave 2"
