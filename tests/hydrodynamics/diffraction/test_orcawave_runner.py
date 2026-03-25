"""Tests for OrcaWave diffraction analysis runner (WRK-029).

TDD test suite for OrcaWaveRunner, covering:
- Data models (RunStatus, RunConfig, RunResult)
- File preparation (generate input files, copy mesh, validate)
- Solver execution (subprocess invocation, timeout, dry-run)
- Full pipeline (run = prepare + execute)
- Executable detection (platform-aware search)
- Module-level convenience function
"""

from __future__ import annotations

import subprocess
from dataclasses import fields as dataclass_fields
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_runner import (
    OrcaWaveRunner,
    RunConfig,
    RunResult,
    RunStatus,
    run_orcawave,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _load_spec(path: Path) -> DiffractionSpec:
    """Load a DiffractionSpec from a YAML fixture file."""
    return DiffractionSpec.from_yaml(path)


# ---------------------------------------------------------------------------
# TestRunStatus
# ---------------------------------------------------------------------------

class TestRunStatus:
    """Enum values and string representation."""

    def test_all_values_present(self):
        expected = {
            "pending", "preparing", "running", "completed",
            "failed", "dry_run", "cancelled",
        }
        actual = {s.value for s in RunStatus}
        assert actual == expected

    def test_string_enum_comparison(self):
        assert RunStatus.COMPLETED == "completed"
        assert RunStatus.DRY_RUN == "dry_run"


# ---------------------------------------------------------------------------
# TestRunConfig
# ---------------------------------------------------------------------------

class TestRunConfig:
    """Pydantic v2 model defaults and validation."""

    def test_defaults(self):
        config = RunConfig()
        assert config.executable_path is None
        assert config.dry_run is False
        assert config.timeout_seconds == 7200
        assert config.generate_modular is True
        assert config.copy_mesh_files is True
        assert config.extra_args == []

    def test_output_dir_default_none(self):
        config = RunConfig()
        assert config.output_dir is None

    def test_custom_values(self):
        config = RunConfig(
            executable_path=Path("C:/Orcina/OrcaWave/orcawave.exe"),
            output_dir=Path("/tmp/out"),
            dry_run=True,
            timeout_seconds=3600,
            generate_modular=False,
            copy_mesh_files=False,
            extra_args=["--verbose"],
        )
        assert config.executable_path == Path("C:/Orcina/OrcaWave/orcawave.exe")
        assert config.output_dir == Path("/tmp/out")
        assert config.dry_run is True
        assert config.timeout_seconds == 3600
        assert config.generate_modular is False
        assert config.copy_mesh_files is False
        assert config.extra_args == ["--verbose"]

    def test_timeout_must_be_positive(self):
        from pydantic import ValidationError

        with pytest.raises(ValidationError):
            RunConfig(timeout_seconds=0)

    def test_path_coercion(self):
        config = RunConfig(executable_path="orcawave.exe")
        assert isinstance(config.executable_path, Path)


# ---------------------------------------------------------------------------
# TestRunResult
# ---------------------------------------------------------------------------

class TestRunResult:
    """Dataclass properties and defaults."""

    def test_default_values(self):
        result = RunResult(status=RunStatus.PENDING)
        assert result.status == RunStatus.PENDING
        assert result.input_file is None
        assert result.modular_files == []
        assert result.output_dir is None
        assert result.mesh_files == []
        assert result.log_file is None
        assert result.stdout == ""
        assert result.stderr == ""
        assert result.return_code is None
        assert result.duration_seconds is None
        assert result.error_message is None
        assert result.spec_name is None

    def test_custom_values(self):
        result = RunResult(
            status=RunStatus.COMPLETED,
            input_file=Path("project.yml"),
            spec_name="Ship_001",
            return_code=0,
            duration_seconds=45.3,
        )
        assert result.status == RunStatus.COMPLETED
        assert result.input_file == Path("project.yml")
        assert result.spec_name == "Ship_001"
        assert result.return_code == 0
        assert result.duration_seconds == 45.3

    def test_is_dataclass(self):
        field_names = {f.name for f in dataclass_fields(RunResult)}
        expected_fields = {
            "status", "input_file", "modular_files", "output_dir",
            "mesh_files", "log_file", "stdout", "stderr", "return_code",
            "duration_seconds", "error_message", "spec_name",
        }
        assert expected_fields.issubset(field_names)


# ---------------------------------------------------------------------------
# TestOrcaWaveRunnerPrepare
# ---------------------------------------------------------------------------

class TestOrcaWaveRunnerPrepare:
    """File generation, mesh copying, and validation in prepare()."""

    def test_prepare_creates_output_dir(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path / "new_dir", dry_run=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec)
        assert (tmp_path / "new_dir").exists()
        assert result.status in (RunStatus.DRY_RUN, RunStatus.COMPLETED, RunStatus.PREPARING)

    def test_prepare_generates_single_input_file(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True, generate_modular=False)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec)
        assert result.input_file is not None
        assert result.input_file.exists()
        assert result.input_file.suffix == ".yml"

    def test_prepare_generates_modular_files(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True, generate_modular=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec)
        assert result.input_file is not None
        assert len(result.modular_files) > 0
        for mf in result.modular_files:
            assert mf.exists()

    def test_prepare_copies_mesh_files(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True, copy_mesh_files=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec, spec_path=ship_raos_spec_path)
        assert len(result.mesh_files) > 0
        for mf in result.mesh_files:
            assert mf.exists()

    def test_prepare_skips_mesh_copy_when_disabled(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True, copy_mesh_files=False)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec)
        assert result.mesh_files == []

    def test_prepare_sets_spec_name(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec)
        assert result.spec_name is not None
        assert len(result.spec_name) > 0

    def test_prepare_with_multibody_spec(self, fpso_turret_spec_path, tmp_path):
        spec = _load_spec(fpso_turret_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec)
        assert result.input_file is not None
        assert result.input_file.exists()

    def test_prepare_multibody_copies_all_meshes(self, fpso_turret_spec_path, tmp_path):
        spec = _load_spec(fpso_turret_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True, copy_mesh_files=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec, spec_path=fpso_turret_spec_path)
        assert len(result.mesh_files) >= 2

    def test_prepare_validates_mesh_references(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True, copy_mesh_files=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec, spec_path=ship_raos_spec_path)
        assert result.error_message is None

    def test_prepare_stores_output_dir(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec)
        assert result.output_dir == tmp_path


# ---------------------------------------------------------------------------
# TestOrcaWaveRunnerExecute
# ---------------------------------------------------------------------------

class TestOrcaWaveRunnerExecute:
    """Subprocess mocking, timeout, error handling in execute()."""

    def test_execute_dry_run_skips_subprocess(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True)
        runner = OrcaWaveRunner(config)
        runner.prepare(spec)
        result = runner.execute()
        assert result.status == RunStatus.DRY_RUN
        assert result.return_code is None

    def test_execute_invokes_subprocess(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(
            output_dir=tmp_path,
            executable_path=Path("orcawave.exe"),
            dry_run=False,
            use_api=False,
        )
        runner = OrcaWaveRunner(config)
        runner.prepare(spec)

        mock_result = MagicMock()
        mock_result.returncode = 0
        mock_result.stdout = "OrcaWave completed"
        mock_result.stderr = ""

        with patch.object(runner, "_detect_executable", return_value=Path("orcawave.exe")):
            with patch("subprocess.run", return_value=mock_result) as mock_run:
                result = runner.execute()
                mock_run.assert_called_once()
                assert result.status == RunStatus.COMPLETED
                assert result.return_code == 0

    def test_execute_handles_timeout(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(
            output_dir=tmp_path,
            executable_path=Path("orcawave.exe"),
            dry_run=False,
            use_api=False,
            timeout_seconds=10,
        )
        runner = OrcaWaveRunner(config)
        runner.prepare(spec)

        with patch.object(runner, "_detect_executable", return_value=Path("orcawave.exe")):
            with patch(
                "subprocess.run",
                side_effect=subprocess.TimeoutExpired(cmd="orcawave.exe", timeout=10),
            ):
                result = runner.execute()
                assert result.status == RunStatus.FAILED
                assert "timeout" in result.error_message.lower()

    def test_execute_handles_nonzero_exit(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(
            output_dir=tmp_path,
            executable_path=Path("orcawave.exe"),
            dry_run=False,
            use_api=False,
        )
        runner = OrcaWaveRunner(config)
        runner.prepare(spec)

        mock_result = MagicMock()
        mock_result.returncode = 1
        mock_result.stdout = ""
        mock_result.stderr = "License error"

        with patch.object(runner, "_detect_executable", return_value=Path("orcawave.exe")):
            with patch("subprocess.run", return_value=mock_result):
                result = runner.execute()
                assert result.status == RunStatus.FAILED
                assert result.return_code == 1
                assert "License error" in result.stderr

    def test_execute_handles_file_not_found(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(
            output_dir=tmp_path,
            executable_path=Path("nonexistent_orcawave.exe"),
            dry_run=False,
            use_api=False,
        )
        runner = OrcaWaveRunner(config)
        runner.prepare(spec)

        with patch.object(runner, "_detect_executable", return_value=Path("nonexistent_orcawave.exe")):
            with patch("subprocess.run", side_effect=FileNotFoundError("not found")):
                result = runner.execute()
                assert result.status == RunStatus.FAILED
                assert result.error_message is not None

    def test_execute_records_duration(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True)
        runner = OrcaWaveRunner(config)
        runner.prepare(spec)
        result = runner.execute()
        assert result.duration_seconds is not None
        assert result.duration_seconds >= 0


# ---------------------------------------------------------------------------
# TestOrcaWaveRunnerRun
# ---------------------------------------------------------------------------

class TestOrcaWaveRunnerRun:
    """Full pipeline: run = prepare + execute."""

    def test_run_dry_run_ship(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True)
        runner = OrcaWaveRunner(config)
        result = runner.run(spec)
        assert result.status == RunStatus.DRY_RUN
        assert result.input_file is not None
        assert result.input_file.exists()

    def test_run_dry_run_semisub(self, semisub_spec_path, tmp_path):
        spec = _load_spec(semisub_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True)
        runner = OrcaWaveRunner(config)
        result = runner.run(spec)
        assert result.status == RunStatus.DRY_RUN
        assert result.input_file is not None

    def test_run_dry_run_fpso_turret(self, fpso_turret_spec_path, tmp_path):
        spec = _load_spec(fpso_turret_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True)
        runner = OrcaWaveRunner(config)
        result = runner.run(spec)
        assert result.status == RunStatus.DRY_RUN
        assert result.input_file is not None

    def test_run_stores_result_property(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True)
        runner = OrcaWaveRunner(config)
        result = runner.run(spec)
        assert runner.result is result

    def test_run_with_no_executable_falls_back_to_dry_run(
        self, ship_raos_spec_path, tmp_path
    ):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=False, use_api=False)
        runner = OrcaWaveRunner(config)

        with patch.object(runner, "_detect_executable", return_value=None):
            result = runner.run(spec)
            assert result.status == RunStatus.DRY_RUN

    def test_run_with_mock_executable(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(
            output_dir=tmp_path,
            executable_path=Path("orcawave.exe"),
            dry_run=False,
            use_api=False,
        )
        runner = OrcaWaveRunner(config)

        mock_result = MagicMock()
        mock_result.returncode = 0
        mock_result.stdout = "done"
        mock_result.stderr = ""

        with patch.object(runner, "_detect_executable", return_value=Path("orcawave.exe")):
            with patch("subprocess.run", return_value=mock_result):
                result = runner.run(spec)
                assert result.status == RunStatus.COMPLETED
                assert result.input_file.exists()


# ---------------------------------------------------------------------------
# TestOrcaWaveRunnerDetectExecutable
# ---------------------------------------------------------------------------

class TestOrcaWaveRunnerDetectExecutable:
    """Platform-aware executable search."""

    def test_explicit_path_takes_priority(self, tmp_path):
        exe = tmp_path / "orcawave.exe"
        exe.touch()
        config = RunConfig(executable_path=exe)
        runner = OrcaWaveRunner(config)
        detected = runner._detect_executable()
        assert detected == exe

    def test_env_variable_fallback(self):
        config = RunConfig()
        runner = OrcaWaveRunner(config)
        with patch.dict("os.environ", {"ORCAWAVE_PATH": "/usr/bin/orcawave"}):
            with patch("pathlib.Path.exists", return_value=True):
                detected = runner._detect_executable()
                assert detected == Path("/usr/bin/orcawave")

    def test_shutil_which_fallback(self):
        config = RunConfig()
        runner = OrcaWaveRunner(config)
        with patch.dict("os.environ", {}, clear=True):
            with patch("shutil.which", return_value="/usr/local/bin/orcawave"):
                detected = runner._detect_executable()
                assert detected == Path("/usr/local/bin/orcawave")

    def test_returns_none_when_not_found(self):
        config = RunConfig()
        runner = OrcaWaveRunner(config)
        with patch.dict("os.environ", {}, clear=True):
            with patch("shutil.which", return_value=None):
                detected = runner._detect_executable()
                assert detected is None


# ---------------------------------------------------------------------------
# TestRunOrcawaveConvenience
# ---------------------------------------------------------------------------

class TestRunOrcawaveConvenience:
    """Module-level convenience function."""

    def test_convenience_dry_run(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        result = run_orcawave(spec, output_dir=tmp_path, dry_run=True)
        assert result.status == RunStatus.DRY_RUN
        assert result.input_file is not None

    def test_convenience_returns_run_result(self, semisub_spec_path, tmp_path):
        spec = _load_spec(semisub_spec_path)
        result = run_orcawave(spec, output_dir=tmp_path, dry_run=True)
        assert isinstance(result, RunResult)

    def test_convenience_accepts_timeout(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        result = run_orcawave(
            spec, output_dir=tmp_path, dry_run=True, timeout_seconds=300
        )
        assert result.status == RunStatus.DRY_RUN
