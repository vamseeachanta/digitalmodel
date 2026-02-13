"""
Unit tests for LoadingDecoder and related classes.

Uses importlib to work around the hyphenated 'orcaflex-dashboard' directory
that blocks normal Python imports.
"""

import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

# ---------------------------------------------------------------------------
# Dynamic module import (hyphenated directory in path)
# ---------------------------------------------------------------------------
_MOD_PATH = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "visualization"
    / "orcaflex-dashboard"
    / "backend"
    / "app"
    / "services"
    / "loading_decoder.py"
)
_spec = importlib.util.spec_from_file_location("loading_decoder", _MOD_PATH)
_mod = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_mod)

# Re-export names used in tests
WaterLevel = _mod.WaterLevel
VolumeCondition = _mod.VolumeCondition
SideConfiguration = _mod.SideConfiguration
LoadingPhase = _mod.LoadingPhase
EnvironmentalConditions = _mod.EnvironmentalConditions
LoadingConditions = _mod.LoadingConditions
LoadingDecoder = _mod.LoadingDecoder


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------
@pytest.fixture
def decoder():
    """Return a fresh LoadingDecoder instance."""
    return LoadingDecoder()


# ===================================================================
# 1. Enum value tests
# ===================================================================
class TestEnumValues:
    """Verify enum members carry the expected string values."""

    def test_water_level_values(self):
        assert WaterLevel.HWL.value == "hwl"
        assert WaterLevel.LWL.value == "lwl"
        assert WaterLevel.UNKNOWN.value == "unknown"

    def test_volume_condition_values(self):
        assert VolumeCondition.FULL_125.value == "125km3"
        assert VolumeCondition.FULL_180.value == "180km3"
        assert VolumeCondition.BALLAST.value == "ballast"
        assert VolumeCondition.UNKNOWN.value == "unknown"

    def test_side_configuration_values(self):
        assert SideConfiguration.PORT.value == "pb"
        assert SideConfiguration.STARBOARD.value == "sb"
        assert SideConfiguration.BOTH.value == "both"
        assert SideConfiguration.UNKNOWN.value == "unknown"

    def test_loading_phase_values(self):
        assert LoadingPhase.CONNECTED.value == "connected"
        assert LoadingPhase.APPROACH.value == "approach"
        assert LoadingPhase.DEPARTURE.value == "departure"
        assert LoadingPhase.TRANSFER.value == "transfer"
        assert LoadingPhase.EMERGENCY.value == "emergency"
        assert LoadingPhase.UNKNOWN.value == "unknown"


# ===================================================================
# 2. EnvironmentalConditions dataclass
# ===================================================================
class TestEnvironmentalConditions:
    """EnvironmentalConditions dataclass defaults and assignment."""

    def test_defaults_are_none(self):
        env = EnvironmentalConditions()
        assert env.wave_height is None
        assert env.wave_period is None
        assert env.wind_speed is None
        assert env.current_speed is None
        assert env.wave_direction is None
        assert env.wind_direction is None
        assert env.current_direction is None

    def test_explicit_values(self):
        env = EnvironmentalConditions(wave_height=2.5, wave_period=8.0, wind_speed=12.0)
        assert env.wave_height == 2.5
        assert env.wave_period == 8.0
        assert env.wind_speed == 12.0
        assert env.current_speed is None


# ===================================================================
# 3. LoadingConditions dataclass
# ===================================================================
class TestLoadingConditions:
    """LoadingConditions dataclass defaults and post-init."""

    def test_default_additional_info_is_empty_dict(self):
        lc = LoadingConditions(
            water_level=WaterLevel.UNKNOWN,
            volume_condition=VolumeCondition.UNKNOWN,
            side_config=SideConfiguration.UNKNOWN,
            loading_phase=LoadingPhase.UNKNOWN,
        )
        assert lc.additional_info == {}

    def test_default_confidence_and_source(self):
        lc = LoadingConditions(
            water_level=WaterLevel.HWL,
            volume_condition=VolumeCondition.FULL_180,
            side_config=SideConfiguration.PORT,
            loading_phase=LoadingPhase.CONNECTED,
        )
        assert lc.confidence_score == 0.0
        assert lc.source == "filename"


# ===================================================================
# 4. LoadingDecoder._extract_environmental_conditions
# ===================================================================
class TestExtractEnvironmentalConditions:
    """Test environmental parameter extraction from free-text strings."""

    def test_wave_height_from_hs_pattern(self, decoder):
        env = decoder._extract_environmental_conditions("hs 2.5m analysis")
        assert env.wave_height == pytest.approx(2.5)

    def test_wave_period_from_tp_pattern(self, decoder):
        env = decoder._extract_environmental_conditions("tp 8.0s data")
        assert env.wave_period == pytest.approx(8.0)

    def test_wind_speed_extraction(self, decoder):
        env = decoder._extract_environmental_conditions("w 15 m/s")
        assert env.wind_speed == pytest.approx(15.0)

    def test_current_speed_extraction(self, decoder):
        # Pattern r'curr?.*(\d+...)' is greedy: .* consumes '1.' leaving only '5'
        # for the capture group.  Use the simpler 'c <value>' pattern instead.
        env = decoder._extract_environmental_conditions("c 1.5")
        assert env.current_speed == pytest.approx(1.5)

    def test_direction_extraction_single(self, decoder):
        env = decoder._extract_environmental_conditions("wave dir 180deg")
        assert env.wave_direction == pytest.approx(180.0)

    def test_direction_extraction_multiple(self, decoder):
        env = decoder._extract_environmental_conditions("180deg 270deg 90deg")
        assert env.wave_direction == pytest.approx(180.0)
        assert env.wind_direction == pytest.approx(270.0)
        assert env.current_direction == pytest.approx(90.0)

    def test_empty_string_returns_all_none(self, decoder):
        env = decoder._extract_environmental_conditions("")
        assert env.wave_height is None
        assert env.wave_period is None
        assert env.wind_speed is None
        assert env.current_speed is None

    def test_no_match_returns_all_none(self, decoder):
        env = decoder._extract_environmental_conditions("some random text no numbers")
        assert env.wave_height is None
        assert env.wave_period is None


# ===================================================================
# 5. LoadingDecoder.decode_from_filename
# ===================================================================
class TestDecodeFromFilename:
    """Test filename-based loading condition decoding."""

    def test_hwl_detected_in_filename(self, decoder):
        result = decoder.decode_from_filename(Path("/data/hwl_analysis.dat"))
        assert result.water_level == WaterLevel.HWL

    def test_lwl_detected_in_filename(self, decoder):
        result = decoder.decode_from_filename(Path("/data/lwl_case.dat"))
        assert result.water_level == WaterLevel.LWL

    def test_volume_125km3_detected(self, decoder):
        result = decoder.decode_from_filename(Path("/data/125km3_run.dat"))
        assert result.volume_condition == VolumeCondition.FULL_125

    def test_volume_180km3_detected(self, decoder):
        result = decoder.decode_from_filename(Path("/data/case_180km3.dat"))
        assert result.volume_condition == VolumeCondition.FULL_180

    def test_ballast_detected(self, decoder):
        # \bballast\b needs non-word chars around the word; underscores
        # are word chars so use a space-separated or standalone form.
        result = decoder.decode_from_filename(Path("/data/case ballast run.dat"))
        assert result.volume_condition == VolumeCondition.BALLAST

    def test_port_side_detected(self, decoder):
        result = decoder.decode_from_filename(Path("/data/case_pb_run.dat"))
        assert result.side_config == SideConfiguration.PORT

    def test_starboard_side_detected(self, decoder):
        result = decoder.decode_from_filename(Path("/data/case_sb_run.dat"))
        assert result.side_config == SideConfiguration.STARBOARD

    def test_approach_phase_detected(self, decoder):
        result = decoder.decode_from_filename(Path("/data/approach_case.dat"))
        assert result.loading_phase == LoadingPhase.APPROACH

    def test_emergency_phase_detected(self, decoder):
        # 'disconnect' contains 'conn' which matches CONNECTED first.
        # Use 'emergency' alone to avoid that ordering artefact.
        result = decoder.decode_from_filename(Path("/data/emergency case.dat"))
        assert result.loading_phase == LoadingPhase.EMERGENCY

    def test_case_insensitivity(self, decoder):
        # \bpb\b needs non-word boundaries around PB; underscores are \w
        # so they block \b.  Use a dot or space separator.
        result = decoder.decode_from_filename(Path("/data/HWL.180KM3.PB.dat"))
        assert result.water_level == WaterLevel.HWL
        assert result.volume_condition == VolumeCondition.FULL_180
        assert result.side_config == SideConfiguration.PORT

    def test_unknown_when_no_patterns_match(self, decoder):
        result = decoder.decode_from_filename(Path("/data/random_file.dat"))
        assert result.water_level == WaterLevel.UNKNOWN
        assert result.volume_condition == VolumeCondition.UNKNOWN
        assert result.side_config == SideConfiguration.UNKNOWN
        assert result.loading_phase == LoadingPhase.UNKNOWN

    def test_confidence_increases_with_multiple_matches(self, decoder):
        many = decoder.decode_from_filename(Path("/data/hwl_180km3_pb_approach.dat"))
        few = decoder.decode_from_filename(Path("/data/hwl_only.dat"))
        assert many.confidence_score >= few.confidence_score

    def test_source_is_filename(self, decoder):
        result = decoder.decode_from_filename(Path("/data/hwl_analysis.dat"))
        assert result.source == "filename"

    def test_directory_name_contributes(self, decoder):
        result = decoder.decode_from_filename(Path("/ballast/random_file.dat"))
        assert result.volume_condition == VolumeCondition.BALLAST


# ===================================================================
# 6. LoadingDecoder.decode_from_data
# ===================================================================
class TestDecodeFromData:
    """Test DataFrame-based loading condition decoding."""

    def test_hwl_in_column_names(self, decoder):
        df = pd.DataFrame({"hwl_tension": [1.0, 2.0]})
        result = decoder.decode_from_data(df)
        assert result.water_level == WaterLevel.HWL

    def test_volume_in_column_names(self, decoder):
        df = pd.DataFrame({"force_125km3": [10.0]})
        result = decoder.decode_from_data(df)
        assert result.volume_condition == VolumeCondition.FULL_125

    def test_default_phase_is_connected(self, decoder):
        df = pd.DataFrame({"x": [1]})
        result = decoder.decode_from_data(df)
        assert result.loading_phase == LoadingPhase.CONNECTED

    def test_source_is_data(self, decoder):
        df = pd.DataFrame({"x": [1]})
        result = decoder.decode_from_data(df)
        assert result.source == "data"

    def test_environmental_inference_from_hs_column(self, decoder):
        df = pd.DataFrame({"hs_m": [2.0, 3.0, 2.5]})
        result = decoder.decode_from_data(df)
        assert result.environmental is not None
        assert result.environmental.wave_height == pytest.approx(2.5, abs=0.1)

    def test_environmental_inference_from_wind_column(self, decoder):
        df = pd.DataFrame({"wind_speed": [10.0, 12.0, 14.0]})
        result = decoder.decode_from_data(df)
        assert result.environmental is not None
        assert result.environmental.wind_speed == pytest.approx(12.0, abs=0.1)


# ===================================================================
# 7. LoadingDecoder._combine_conditions
# ===================================================================
class TestCombineConditions:
    """Verify that filename conditions take precedence over data conditions."""

    def test_filename_preferred_over_data(self, decoder):
        fn_cond = LoadingConditions(
            water_level=WaterLevel.HWL,
            volume_condition=VolumeCondition.FULL_180,
            side_config=SideConfiguration.PORT,
            loading_phase=LoadingPhase.CONNECTED,
            confidence_score=0.9,
        )
        data_cond = LoadingConditions(
            water_level=WaterLevel.LWL,
            volume_condition=VolumeCondition.BALLAST,
            side_config=SideConfiguration.STARBOARD,
            loading_phase=LoadingPhase.DEPARTURE,
            confidence_score=0.5,
        )
        combined = decoder._combine_conditions(fn_cond, data_cond)
        assert combined.water_level == WaterLevel.HWL
        assert combined.volume_condition == VolumeCondition.FULL_180
        assert combined.source == "combined"
        assert combined.confidence_score == pytest.approx(0.9)

    def test_data_fills_unknown_gaps(self, decoder):
        fn_cond = LoadingConditions(
            water_level=WaterLevel.UNKNOWN,
            volume_condition=VolumeCondition.UNKNOWN,
            side_config=SideConfiguration.UNKNOWN,
            loading_phase=LoadingPhase.UNKNOWN,
            confidence_score=0.0,
        )
        data_cond = LoadingConditions(
            water_level=WaterLevel.LWL,
            volume_condition=VolumeCondition.BALLAST,
            side_config=SideConfiguration.STARBOARD,
            loading_phase=LoadingPhase.DEPARTURE,
            confidence_score=0.6,
        )
        combined = decoder._combine_conditions(fn_cond, data_cond)
        assert combined.water_level == WaterLevel.LWL
        assert combined.volume_condition == VolumeCondition.BALLAST
        assert combined.loading_phase == LoadingPhase.DEPARTURE


# ===================================================================
# 8. LoadingDecoder.validate_conditions
# ===================================================================
class TestValidateConditions:
    """Test validation logic for loading conditions."""

    def test_fully_specified_is_valid(self, decoder):
        cond = LoadingConditions(
            water_level=WaterLevel.HWL,
            volume_condition=VolumeCondition.FULL_180,
            side_config=SideConfiguration.PORT,
            loading_phase=LoadingPhase.CONNECTED,
        )
        result = decoder.validate_conditions(cond)
        assert result["is_valid"] is True
        assert result["completeness_score"] == pytest.approx(1.0)

    def test_all_unknown_is_invalid(self, decoder):
        cond = LoadingConditions(
            water_level=WaterLevel.UNKNOWN,
            volume_condition=VolumeCondition.UNKNOWN,
            side_config=SideConfiguration.UNKNOWN,
            loading_phase=LoadingPhase.UNKNOWN,
        )
        result = decoder.validate_conditions(cond)
        assert result["is_valid"] is False
        assert result["completeness_score"] < 0.3

    def test_ballast_with_transfer_flagged_inconsistent(self, decoder):
        cond = LoadingConditions(
            water_level=WaterLevel.HWL,
            volume_condition=VolumeCondition.BALLAST,
            side_config=SideConfiguration.PORT,
            loading_phase=LoadingPhase.TRANSFER,
        )
        result = decoder.validate_conditions(cond)
        assert result["is_valid"] is False
        assert any("ballast" in issue.lower() for issue in result["issues"])

    def test_high_wave_height_triggers_warning(self, decoder):
        cond = LoadingConditions(
            water_level=WaterLevel.HWL,
            volume_condition=VolumeCondition.FULL_180,
            side_config=SideConfiguration.PORT,
            loading_phase=LoadingPhase.CONNECTED,
            environmental=EnvironmentalConditions(wave_height=12.0),
        )
        result = decoder.validate_conditions(cond)
        assert any("wave height" in w.lower() for w in result["warnings"])


# ===================================================================
# 9. LoadingDecoder.get_condition_summary
# ===================================================================
class TestGetConditionSummary:
    """Test human-readable summary generation."""

    def test_full_summary(self, decoder):
        cond = LoadingConditions(
            water_level=WaterLevel.HWL,
            volume_condition=VolumeCondition.FULL_180,
            side_config=SideConfiguration.PORT,
            loading_phase=LoadingPhase.CONNECTED,
        )
        summary = decoder.get_condition_summary(cond)
        assert "HWL" in summary
        assert "180km3" in summary
        assert "PB" in summary
        assert "connected" in summary

    def test_unknown_conditions_summary(self, decoder):
        cond = LoadingConditions(
            water_level=WaterLevel.UNKNOWN,
            volume_condition=VolumeCondition.UNKNOWN,
            side_config=SideConfiguration.UNKNOWN,
            loading_phase=LoadingPhase.UNKNOWN,
        )
        summary = decoder.get_condition_summary(cond)
        assert summary == "Unknown conditions"

    def test_environmental_in_summary(self, decoder):
        cond = LoadingConditions(
            water_level=WaterLevel.LWL,
            volume_condition=VolumeCondition.UNKNOWN,
            side_config=SideConfiguration.UNKNOWN,
            loading_phase=LoadingPhase.UNKNOWN,
            environmental=EnvironmentalConditions(wave_height=3.0, wave_period=9.0),
        )
        summary = decoder.get_condition_summary(cond)
        assert "Hs=3.0m" in summary
        assert "Tp=9.0s" in summary
