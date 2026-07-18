"""Regression tests for modular_input_validation review fixes.

Covers:
- level_1_yaml: missing include files are FAIL (not WARN) and are not
  double-reported as both a syntax error and a missing include.
- level_3_physical: parameter probes use real OrcaFlex YAML layouts
  (Environment.WaveTrains list, 6DBuoys list) instead of dot-paths that
  never exist in real models.
- cli: --no-orcaflex actually skips Level 2 (the OrcaFlex API level).

No OrcaFlex license / OrcFxAPI required.
"""

from pathlib import Path

import pytest

from digitalmodel.solvers.orcaflex.modular_input_validation.cli import (
    create_config_from_args,
    create_parser,
    main as cli_main,
)
from digitalmodel.solvers.orcaflex.modular_input_validation.config import (
    ValidationConfig,
)
from digitalmodel.solvers.orcaflex.modular_input_validation.data_loader import (
    ParameterRange,
)
from digitalmodel.solvers.orcaflex.modular_input_validation.level_1_yaml import (
    Level1YAMLValidator,
)
from digitalmodel.solvers.orcaflex.modular_input_validation.level_3_physical import (
    Level3PhysicalValidator,
    _extract_first,
    _extract_path,
)
from digitalmodel.solvers.orcaflex.modular_input_validation.models import (
    ValidationStatus,
)
from digitalmodel.solvers.orcaflex.modular_input_validation.validator import (
    ModularInputValidator,
)


# ---------------------------------------------------------------------------
# Level 1: missing include files
# ---------------------------------------------------------------------------


@pytest.fixture
def master_with_missing_include(tmp_path: Path) -> Path:
    master = tmp_path / "master.yml"
    master.write_text("- includefile: includes/03_environment.yml\n")
    return master


class TestLevel1MissingInclude:
    def test_missing_include_is_fail_not_warn(
        self, master_with_missing_include: Path
    ):
        """A missing include is a guaranteed OrcaFlex load failure -> FAIL."""
        result = Level1YAMLValidator().validate(master_with_missing_include)
        assert result.missing_includes == ["includes/03_environment.yml"]
        assert not result.includes_resolved
        assert result.status == ValidationStatus.FAIL

    def test_missing_include_not_duplicated_in_syntax_errors(
        self, master_with_missing_include: Path
    ):
        """The same message must not appear in BOTH error lists."""
        result = Level1YAMLValidator().validate(master_with_missing_include)
        assert not any(
            "include" in e.lower() for e in result.syntax_errors
        )

    def test_all_includes_present_still_passes(self, tmp_path: Path):
        includes = tmp_path / "includes"
        includes.mkdir()
        (includes / "01_general.yml").write_text(
            "General:\n  UnitsSystem: SI\n"
        )
        master = tmp_path / "master.yml"
        master.write_text("- includefile: includes/01_general.yml\n")

        result = Level1YAMLValidator().validate(master)
        assert result.status == ValidationStatus.PASS

    def test_validator_reports_issue_once_and_fails_overall(
        self, master_with_missing_include: Path, tmp_path: Path
    ):
        """Orchestrator emits ONE issue and an overall FAIL status."""
        config = ValidationConfig(
            skip_levels=[2, 3],
            generate_reports=False,
            reports_dir=tmp_path / "reports",
            results_dir=tmp_path / "results",
            calm_buoy_data_dir=tmp_path / "data",
        )
        validator = ModularInputValidator(config)
        result = validator.validate_file(master_with_missing_include)

        assert result.overall_status == ValidationStatus.FAIL
        include_issues = [
            i for i in result.issues if "include" in i.message.lower()
        ]
        assert len(include_issues) == 1

    def test_cli_exits_nonzero_for_missing_include(
        self, master_with_missing_include: Path, tmp_path, monkeypatch
    ):
        """CLI must not exit 0 for a model that cannot load in OrcaFlex."""
        monkeypatch.chdir(tmp_path)
        rc = cli_main([
            str(master_with_missing_include),
            "--no-orcaflex",
            "--skip-level", "3",
            "--no-reports",
            "--reports-dir", str(tmp_path / "reports"),
            "--data-dir", str(tmp_path / "data"),
        ])
        assert rc == 1


# ---------------------------------------------------------------------------
# Level 3: real-model parameter paths
# ---------------------------------------------------------------------------


REAL_MODEL_CONTENT = {
    # Layout mirrors docs/domains/orcaflex/examples/modular/C06/
    # 'C06 CALM buoy'/includes/03_environment.yml and 08_buoys.yml
    "Environment": {
        "WaterDepth": 100,
        "WaveTrains": [
            {
                "Name": "Wave 1",
                "WaveType": "JONSWAP",
                "WaveHs": 25.0,   # absurd on purpose
                "WaveTz": 8.0,
                "WaveDirection": 180,
            }
        ],
        "RefCurrentSpeed": 0.5,
    },
    "6DBuoys": [
        {
            "Name": "CALM Base",
            "Mass": 250,
            "Cylinders": [
                {"CylinderOuterDiameter": 11, "CylinderLength": 1}
            ],
        }
    ],
}


class TestExtractPath:
    def test_list_index_traversal(self):
        assert _extract_path(
            REAL_MODEL_CONTENT, "Environment.WaveTrains[0].WaveHs"
        ) == 25.0
        assert _extract_path(
            REAL_MODEL_CONTENT,
            "6DBuoys[0].Cylinders[0].CylinderOuterDiameter",
        ) == 11

    def test_plain_dict_walk_still_works(self):
        assert _extract_path(
            REAL_MODEL_CONTENT, "Environment.WaterDepth"
        ) == 100

    def test_out_of_range_index_returns_default(self):
        assert _extract_path(
            REAL_MODEL_CONTENT, "Environment.WaveTrains[3].WaveHs"
        ) is None

    def test_extract_first_falls_back(self):
        assert _extract_first(
            REAL_MODEL_CONTENT,
            ["Environment.WaveTp", "Environment.WaveTrains[0].WaveTz"],
        ) == 8.0


class TestLevel3RealModelPaths:
    """Regression: checks used to probe Environment.WaveHs / Buoy.* dot
    paths that never exist in real models, so every check silently no-oped
    (parameters_validated=0, status UNKNOWN)."""

    @pytest.fixture
    def validator(self, tmp_path) -> Level3PhysicalValidator:
        config = ValidationConfig(
            generate_reports=False,
            reports_dir=tmp_path / "reports",
            results_dir=tmp_path / "results",
            calm_buoy_data_dir=tmp_path / "data",
        )
        return Level3PhysicalValidator(config)

    def test_metocean_hs_out_of_range_flagged(self, validator):
        ranges = {
            "operating_hs_max": ParameterRange(
                parameter="operating_hs",
                min_value=0.0,
                max_value=10.0,
                reference_basis="test",
                notes="",
            )
        }
        checks = validator._validate_metocean(REAL_MODEL_CONTENT, ranges)
        hs_checks = [c for c in checks if c.parameter == "operating_hs"]
        assert len(hs_checks) == 1
        assert not hs_checks[0].within_range  # 25.0 > 10.0

    def test_metocean_current_uses_ref_current_speed(self, validator):
        ranges = {
            "operating_surface_current": ParameterRange(
                parameter="operating_surface_current",
                min_value=0.0,
                max_value=2.0,
                reference_basis="test",
                notes="",
            )
        }
        checks = validator._validate_metocean(REAL_MODEL_CONTENT, ranges)
        cur = [c for c in checks if c.parameter == "operating_surface_current"]
        assert len(cur) == 1
        assert cur[0].within_range  # 0.5 within [0, 2]

    def test_hull_geometry_from_6dbuoys_list(self, validator):
        ranges = {
            "buoy_diameter": ParameterRange(
                parameter="buoy_diameter",
                min_value=5.0,
                max_value=20.0,
                reference_basis="test",
                notes="",
            ),
            "buoy_mass": ParameterRange(
                parameter="buoy_mass",
                min_value=100.0,
                max_value=500.0,
                reference_basis="test",
                notes="",
            ),
        }
        checks = validator._validate_hull_geometry(REAL_MODEL_CONTENT, ranges)
        params = {c.parameter for c in checks}
        assert "buoy_diameter" in params  # from Cylinders[0]
        assert "buoy_mass" in params      # from 6DBuoys[0].Mass

    def test_legacy_flat_paths_still_work(self, validator):
        """Synthetic flat project files keep working (backward compat)."""
        flat_content = {"Environment": {"WaveHs": 2.5}}
        ranges = {
            "operating_hs": ParameterRange(
                parameter="operating_hs",
                min_value=0.0,
                max_value=10.0,
                reference_basis="test",
                notes="",
            )
        }
        checks = validator._validate_metocean(flat_content, ranges)
        assert len(checks) == 1
        assert checks[0].within_range


# ---------------------------------------------------------------------------
# CLI: --no-orcaflex must skip Level 2
# ---------------------------------------------------------------------------


class TestNoOrcaflexFlag:
    def test_no_orcaflex_skips_level_2(self):
        """Regression: enable_orcaflex was never consulted, so Level 2
        still ran LoadData/CalculateStatics on licensed machines."""
        parser = create_parser()
        args = parser.parse_args(["dummy.yml", "--no-orcaflex"])
        config = create_config_from_args(args)
        assert config.enable_orcaflex is False
        assert 2 in config.skip_levels
        assert config.should_run_level(2) is False

    def test_default_runs_level_2(self):
        parser = create_parser()
        args = parser.parse_args(["dummy.yml"])
        config = create_config_from_args(args)
        assert config.enable_orcaflex is True
        assert config.should_run_level(2) is True
