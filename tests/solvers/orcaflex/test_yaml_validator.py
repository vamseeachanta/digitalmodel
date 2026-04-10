"""Tests for OrcaFlex YAML format validator.

Validates that the validator correctly identifies format issues
documented in CRITICAL_YAML_FORMAT_FINDINGS.md.
"""

from __future__ import annotations

import textwrap
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.yaml_validator import (
    OrcaFlexYAMLValidator,
    Severity,
    ValidationResult,
    validate_orcaflex_yaml,
)


@pytest.fixture
def validator() -> OrcaFlexYAMLValidator:
    return OrcaFlexYAMLValidator()


@pytest.fixture
def tmp_yaml(tmp_path):
    """Helper to write YAML content to a temp file and return its path."""
    def _write(content: str, name: str = "test.yml") -> Path:
        p = tmp_path / name
        p.write_text(textwrap.dedent(content))
        return p
    return _write


# -----------------------------------------------------------------------
# Valid flat YAML
# -----------------------------------------------------------------------


class TestValidFlatYAML:
    def test_minimal_valid_flat(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            General:
              UnitsSystem: SI
            Environment:
              WaterDepth: 300
        """)
        result = validator.validate_file(path)
        assert result.valid
        assert result.format_detected == "flat"
        assert len(result.errors) == 0

    def test_complete_flat_model(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            General:
              UnitsSystem: SI
              StageDuration:
                - 10
                - 100
            Environment:
              WaterDepth: 300
              WaveTrains:
                - Name: Wave 1
                  WaveType: JONSWAP
                  WaveDirection: 0
                  WaveHs: 2.5
                  WaveTz: 5.3
            VesselTypes:
              - Name: Simple_Buoy
                Length: 10
            LineTypes:
              - Name: Chain
                OD: 0.12
                EA: 407000e3
            Vessels:
              - Name: CALM_Buoy
                VesselType: Simple_Buoy
                Connection: Free
                InitialPosition: [0, 0, 0]
            Lines:
              - Name: Mooring1
                Connection: [CALM_Buoy, Anchored]
                LineType: [Chain]
                Length: [350]
        """)
        result = validator.validate_file(path)
        assert result.valid
        assert result.format_detected == "flat"
        assert len(result.errors) == 0


# -----------------------------------------------------------------------
# Valid include-based YAML
# -----------------------------------------------------------------------


class TestValidIncludeYAML:
    def test_include_based_format_detected(self, validator, tmp_yaml, tmp_path):
        # Create the referenced include files
        (tmp_path / "01_general.yml").write_text("StageDuration:\n  - 10\n  - 100\n")
        (tmp_path / "02_environment.yml").write_text("Environment:\n  WaterDepth: 300\n")

        path = tmp_yaml("""\
            - includefile: 01_general.yml
            - includefile: 02_environment.yml
        """)
        result = validator.validate_file(path)
        assert result.valid
        assert result.format_detected == "include-based"
        assert len(result.errors) == 0

    def test_missing_include_file_error(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            - includefile: nonexistent_file.yml
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("not found" in i.message for i in result.errors)


# -----------------------------------------------------------------------
# Hybrid format detection (ERROR case)
# -----------------------------------------------------------------------


class TestHybridFormatDetection:
    def test_hybrid_format_is_error(self, validator, tmp_yaml):
        """Mixed flat + includes must be flagged as error."""
        path = tmp_yaml("""\
            - includefile: 01_general.yml
            - General:
                UnitsSystem: SI
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert result.format_detected == "hybrid"
        assert any("Hybrid format" in i.message for i in result.errors)

    def test_list_with_non_include_items(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            - includefile: 01_general.yml
            - some_other_key: value
        """)
        result = validator.validate_file(path)
        assert not result.valid


# -----------------------------------------------------------------------
# Invalid properties
# -----------------------------------------------------------------------


class TestInvalidProperties:
    def test_implicit_variable_max_time_step(self, validator, tmp_yaml):
        """ImplicitVariableMaxTimeStep does not exist in OrcaFlex."""
        path = tmp_yaml("""\
            General:
              StageDuration:
                - 10
                - 100
              ImplicitVariableMaxTimeStep: ~
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("ImplicitVariableMaxTimeStep" in i.message for i in result.errors)

    def test_category_type_invalid(self, validator, tmp_yaml):
        """CategoryType is not a valid LineType property."""
        path = tmp_yaml("""\
            LineTypes:
              - Name: MooringChain
                CategoryType: General
                OD: 0.084
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("CategoryType" in i.message for i in result.errors)

    def test_num_mooring_lines_invalid(self, validator, tmp_yaml):
        """NumMooringLines is not a valid OrcaFlex variable."""
        path = tmp_yaml("""\
            General:
              NumMooringLines: 6
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("NumMooringLines" in i.message for i in result.errors)


# -----------------------------------------------------------------------
# Wave properties at wrong level
# -----------------------------------------------------------------------


class TestWavePropertyPlacement:
    def test_wave_type_at_environment_level(self, validator, tmp_yaml):
        """WaveType must be inside WaveTrains, not at Environment level."""
        path = tmp_yaml("""\
            Environment:
              WaterDepth: 300
              WaveType: JONSWAP
              WaveHs: 2.5
        """)
        result = validator.validate_file(path)
        assert not result.valid
        errs = [i for i in result.errors if i.property in ("WaveType", "WaveHs")]
        assert len(errs) >= 2

    def test_wave_direction_at_environment_level(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            Environment:
              WaterDepth: 300
              WaveDirection: 0
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("WaveDirection" in i.message for i in result.errors)

    def test_correct_wave_trains_passes(self, validator, tmp_yaml):
        """Properly structured WaveTrains should pass."""
        path = tmp_yaml("""\
            Environment:
              WaterDepth: 300
              WaveTrains:
                - Name: Wave 1
                  WaveType: JONSWAP
                  WaveDirection: 0
                  WaveHs: 2.5
                  WaveTz: 5.3
                  WaveGamma: 3.3
        """)
        result = validator.validate_file(path)
        assert result.valid


# -----------------------------------------------------------------------
# VesselTypes / Vessels structure validation
# -----------------------------------------------------------------------


class TestVesselStructure:
    def test_inline_vessel_type_in_vessel(self, validator, tmp_yaml):
        """Vessel with inline VesselType dict is invalid."""
        path = tmp_yaml("""\
            Vessels:
              - Name: Vessel1
                VesselType:
                  Name: crowley650_atb
                  Length: 195.0
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("inline dict" in i.message.lower() or "string reference" in i.message.lower()
                    for i in result.errors)

    def test_vessel_type_as_string_passes(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            VesselTypes:
              - Name: Simple_Buoy
                Length: 10
            Vessels:
              - Name: CALM_Buoy
                VesselType: Simple_Buoy
        """)
        result = validator.validate_file(path)
        assert result.valid

    def test_singular_vessel_key_flagged(self, validator, tmp_yaml):
        """'Vessel:' (singular) should be flagged — OrcaFlex expects 'Vessels:'."""
        path = tmp_yaml("""\
            Vessel:
              - Name: Vessel1
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("Vessels" in i.message and "plural" in i.message for i in result.errors)

    def test_singular_line_type_key_flagged(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            LineType:
              - Name: Chain
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("LineTypes" in i.message for i in result.errors)

    def test_vessel_types_must_be_list(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            VesselTypes:
              Name: Single
              Length: 10
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("list" in i.message.lower() for i in result.errors)


# -----------------------------------------------------------------------
# Lines validation
# -----------------------------------------------------------------------


class TestLinesValidation:
    def test_lines_must_be_list(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            Lines:
              Name: Line1
        """)
        result = validator.validate_file(path)
        assert not result.valid

    def test_line_type_cross_reference_warning(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            LineTypes:
              - Name: Chain
                OD: 0.1
            Lines:
              - Name: Mooring1
                LineType: [WrongType]
        """)
        result = validator.validate_file(path)
        # Should warn (not error) since the type might be in an include
        assert any("WrongType" in i.message for i in result.warnings)


# -----------------------------------------------------------------------
# WaveTrains structure validation
# -----------------------------------------------------------------------


class TestWaveTrainsStructure:
    def test_wave_trains_not_list(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            Environment:
              WaterDepth: 300
              WaveTrains:
                WaveType: JONSWAP
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("WaveTrains must be a list" in i.message for i in result.errors)

    def test_wave_train_missing_name_warns(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            Environment:
              WaterDepth: 300
              WaveTrains:
                - WaveType: JONSWAP
                  WaveHs: 2.5
        """)
        result = validator.validate_file(path)
        assert result.valid  # Missing name is a warning, not an error
        assert any("Name" in i.message for i in result.warnings)


# -----------------------------------------------------------------------
# Edge cases
# -----------------------------------------------------------------------


class TestEdgeCases:
    def test_empty_file(self, validator, tmp_yaml):
        path = tmp_yaml("")
        result = validator.validate_file(path)
        assert not result.valid

    def test_nonexistent_file(self, validator, tmp_path):
        result = validator.validate_file(tmp_path / "does_not_exist.yml")
        assert not result.valid

    def test_invalid_yaml_syntax(self, validator, tmp_yaml):
        path = tmp_yaml("foo: [unclosed bracket")
        result = validator.validate_file(path)
        assert not result.valid
        assert any("parse error" in i.message.lower() for i in result.errors)

    def test_basefile_key_accepted(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            General:
              UnitsSystem: SI
            BaseFile: ../base_files/calm_buoy_simple_base.yml
        """)
        result = validator.validate_file(path)
        assert result.valid

    def test_negative_stage_duration_warns(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            General:
              StageDuration:
                - -10
                - 100
        """)
        result = validator.validate_file(path)
        assert any("negative" in i.message.lower() for i in result.warnings)

    def test_water_depth_non_numeric(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            Environment:
              WaterDepth: deep
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("numeric" in i.message.lower() for i in result.errors)


# -----------------------------------------------------------------------
# Directory validation
# -----------------------------------------------------------------------


class TestDirectoryValidation:
    def test_validate_directory(self, validator, tmp_path):
        (tmp_path / "good.yml").write_text(
            "General:\n  UnitsSystem: SI\nEnvironment:\n  WaterDepth: 300\n"
        )
        (tmp_path / "bad.yml").write_text(
            "Environment:\n  WaveType: JONSWAP\n"
        )
        result = validator.validate_directory(tmp_path)
        assert not result.valid
        assert any("WaveType" in i.message for i in result.errors)

    def test_validate_empty_directory(self, validator, tmp_path):
        result = validator.validate_directory(tmp_path)
        assert any("No YAML files" in i.message for i in result.warnings)


# -----------------------------------------------------------------------
# Convenience function
# -----------------------------------------------------------------------


class TestConvenienceFunction:
    def test_validate_file(self, tmp_yaml):
        path = tmp_yaml("""\
            General:
              UnitsSystem: SI
            Environment:
              WaterDepth: 100
        """)
        result = validate_orcaflex_yaml(path)
        assert result.valid

    def test_validate_directory(self, tmp_path):
        (tmp_path / "model.yml").write_text(
            "General:\n  UnitsSystem: SI\n"
        )
        result = validate_orcaflex_yaml(tmp_path)
        assert result.valid


# -----------------------------------------------------------------------
# Fragment validation
# -----------------------------------------------------------------------


class TestFragmentValidation:
    def test_bare_wave_properties_at_top_level(self, validator, tmp_yaml):
        """Fragment files with bare wave properties should error."""
        path = tmp_yaml("""\
            WaveType: JONSWAP
            WaveDirection: 0
            WaveHs: 3.5
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("WaveType" in i.message for i in result.errors)

    def test_valid_fragment(self, validator, tmp_yaml):
        """A valid stages fragment should pass."""
        path = tmp_yaml("""\
            StageDuration:
              - 10
              - 100
            StaticsMinDamping: 5
            DynamicsSolutionMethod: Implicit time domain
        """)
        result = validator.validate_file(path)
        assert result.valid
        assert result.format_detected == "fragment"

    def test_fragment_with_invalid_property(self, validator, tmp_yaml):
        path = tmp_yaml("""\
            StageDuration:
              - 10
              - 100
            ImplicitVariableMaxTimeStep: ~
        """)
        result = validator.validate_file(path)
        assert not result.valid
        assert any("ImplicitVariableMaxTimeStep" in i.message for i in result.errors)


# -----------------------------------------------------------------------
# ValidationResult properties
# -----------------------------------------------------------------------


class TestValidationResultAPI:
    def test_errors_property(self):
        result = ValidationResult(valid=False)
        result.add(Severity.ERROR, "err1")
        result.add(Severity.WARNING, "warn1")
        result.add(Severity.INFO, "info1")
        assert len(result.errors) == 1
        assert len(result.warnings) == 1

    def test_str_representation(self):
        from digitalmodel.solvers.orcaflex.yaml_validator import ValidationIssue
        issue = ValidationIssue(
            Severity.ERROR, "Bad property", file="test.yml", property="Foo"
        )
        s = str(issue)
        assert "[ERROR]" in s
        assert "test.yml" in s
        assert "Foo" in s
