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

import numpy as np
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


def _make_fake_diffraction(
    n_freq: int = 24,
    n_head: int = 13,
    populate_live: bool = True,
    diagonal_value: float = 500.0,
    negative_heave: bool = False,
):
    """Build a stand-in for a live ``OrcFxAPI.Diffraction`` object.

    Mirrors the attribute conventions consumed by the adapter
    (``frequencies`` in Hz descending; ``addedMass``/``damping`` shaped
    ``(nfreq, 6, 6)``; ``displacementRAOs`` shaped ``(nheading, nfreq, 6)``).

    Args:
        populate_live: When False the live result arrays are empty, forcing the
            adapter's ``LoadResults(.owr)`` fallback. ``load_results_payload``
            on the returned object is what ``LoadResults`` will install.
        diagonal_value: Diagonal magnitude for the symmetric AM/damping matrices.
        negative_heave: Inject a negative heave added-mass diagonal -> FAIL.
    """
    obj = MagicMock()

    # Frequencies in Hz, descending (matches OrcFxAPI). 0.05..2.5 rad/s span.
    freq_rad = np.linspace(0.05, 2.5, n_freq)
    freq_hz = (freq_rad / (2.0 * np.pi))[::-1]  # descending Hz
    headings = np.linspace(0.0, 360.0, n_head)

    am = np.stack([np.eye(6) * diagonal_value for _ in range(n_freq)])
    dp = np.stack([np.eye(6) * diagonal_value for _ in range(n_freq)])
    if negative_heave:
        am[0, 2, 2] = -10.0  # negative heave added-mass diagonal

    # Displacement RAOs (nheading, nfreq, 6) complex; modest magnitudes.
    # Rotation DOFs (cols 3-5) are in rad/m and get converted to deg/m by the
    # adapter, so keep them small (0.05 rad ~ 2.9 deg/m, well under 15 deg/m).
    raos = np.zeros((n_head, n_freq, 6), dtype=complex)
    raos[:, :, 0:3] = 1.0 + 0.0j   # translation m/m
    raos[:, :, 3:6] = 0.05 + 0.0j  # rotation rad/m

    payload = {
        "frequencies": freq_hz,
        "headings": headings,
        "addedMass": am,
        "damping": dp,
        "displacementRAOs": raos,
    }

    def _install(d, data):
        d.frequencies = data["frequencies"]
        d.headings = data["headings"]
        d.addedMass = data["addedMass"]
        d.damping = data["damping"]
        d.displacementRAOs = data["displacementRAOs"]

    if populate_live:
        _install(obj, payload)
    else:
        # Empty live arrays -> adapter probe raises -> LoadResults fallback.
        obj.frequencies = []
        obj.headings = []
        obj.addedMass = []
        obj.damping = []
        obj.displacementRAOs = []

        def _load_results(_path, _data=payload):
            _install(obj, _data)

        obj.LoadResults.side_effect = _load_results

    # No hydrostatics by default (optional sub-schema).
    obj.hydrostaticResults = []
    return obj


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


# ---------------------------------------------------------------------------
# #611 RunResult contract + #625 validation contract (Phase-1a)
# ---------------------------------------------------------------------------

class TestRunResultContract611:
    """New #611/#625 fields on RunResult: defaults and presence."""

    def test_new_contract_fields_present(self):
        field_names = {f.name for f in dataclass_fields(RunResult)}
        expected = {
            "owr_path", "data_file", "xlsx_path", "report_path",
            "validation_report_path", "validation_verdict", "validation_issues",
            "validation_report", "diffraction_results", "orcf_api_version",
            "thread_count",
        }
        assert expected.issubset(field_names)

    def test_contract_defaults(self):
        result = RunResult(status=RunStatus.PENDING)
        assert result.owr_path is None
        assert result.data_file is None
        # D2: xlsx/report path fields exist but stay None this phase.
        assert result.xlsx_path is None
        assert result.report_path is None
        assert result.validation_report_path is None
        assert result.validation_verdict == "SKIPPED"
        assert result.validation_issues == []
        assert result.validation_report is None
        assert result.diffraction_results is None
        assert result.orcf_api_version is None
        assert result.thread_count is None


class TestRunConfigValidationToggles:
    """#625 validation toggles on RunConfig."""

    def test_defaults(self):
        config = RunConfig()
        assert config.validate_outputs is True
        assert config.validation_strict is False

    def test_overrides(self):
        config = RunConfig(validate_outputs=False, validation_strict=True)
        assert config.validate_outputs is False
        assert config.validation_strict is True


class TestSkippedWiring:
    """Dry-run / no-executable fallback produce SKIPPED verdict + reason."""

    def test_dry_run_skipped(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        result = run_orcawave(spec, output_dir=tmp_path, dry_run=True)
        assert result.status == RunStatus.DRY_RUN
        assert result.validation_verdict == "SKIPPED"
        assert result.validation_issues  # carries a reason
        assert "dry-run" in result.validation_issues[0].lower()

    def test_no_executable_fallback_skipped(self, ship_raos_spec_path, tmp_path):
        # No OrcFxAPI here and no executable -> run() falls back to DRY_RUN.
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=False, use_api=False)
        with patch.object(OrcaWaveRunner, "_detect_executable", return_value=None):
            result = OrcaWaveRunner(config).run(spec)
        assert result.status == RunStatus.DRY_RUN
        assert result.validation_verdict == "SKIPPED"
        assert result.validation_issues


class TestLogFileNoLongerOverloaded:
    """#611: the .owr -> log_file overload is removed on the API success path."""

    def test_api_success_sets_owr_path_not_log_file(
        self, ship_raos_spec_path, tmp_path
    ):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(
            output_dir=tmp_path,
            use_api=True,
            thread_count=2,
            validate_outputs=False,  # isolate the #611 path-contract assertions
        )
        runner = OrcaWaveRunner(config)
        runner.prepare(spec)

        fake_diff = _make_fake_diffraction()
        fake_module = MagicMock()
        fake_module.Diffraction.return_value = fake_diff

        with patch.object(OrcaWaveRunner, "_check_api_available", return_value=True), \
                patch.dict("sys.modules", {"OrcFxAPI": fake_module}):
            result = runner.execute()

        assert result.status == RunStatus.COMPLETED
        # The .owr lands in owr_path, NOT log_file (the old overload).
        assert result.owr_path is not None
        assert result.owr_path.suffix == ".owr"
        assert result.data_file is not None
        assert result.data_file.name.endswith("_data.dat")
        assert result.log_file is None
        assert result.thread_count == 2
        # validate_outputs=False -> verdict stays SKIPPED.
        assert result.validation_verdict == "SKIPPED"


# ---------------------------------------------------------------------------
# #625: OrcaWave adapter + runtime validation wiring
# ---------------------------------------------------------------------------


def _run_api(runner, fake_diff):
    """Execute the runner with a fake OrcFxAPI module installed."""
    fake_module = MagicMock()
    fake_module.Diffraction.return_value = fake_diff
    with patch.object(OrcaWaveRunner, "_check_api_available", return_value=True), \
            patch.dict("sys.modules", {"OrcFxAPI": fake_module}):
        return runner.execute()


class TestOrcaWaveAdapterSuccess:
    """Mocked-OrcFxAPI success builds valid DiffractionResults + PASS."""

    def test_adapter_builds_valid_results_and_passes(
        self, ship_raos_spec_path, tmp_path
    ):
        spec = _load_spec(ship_raos_spec_path)
        runner = OrcaWaveRunner(RunConfig(output_dir=tmp_path, use_api=True))
        runner.prepare(spec)
        result = _run_api(runner, _make_fake_diffraction())

        assert result.status == RunStatus.COMPLETED
        assert result.validation_verdict == "PASS"
        dr = result.diffraction_results
        assert dr is not None
        # Mandatory sub-schema metadata.
        assert dr.raos.analysis_tool == "OrcaWave"
        assert dr.added_mass.analysis_tool == "OrcaWave"
        assert dr.damping.analysis_tool == "OrcaWave"
        assert dr.raos.vessel_name == result.spec_name
        for s in (dr.raos, dr.added_mass, dr.damping):
            assert s.created_date
            assert s.water_depth is not None
        # 6x6 matrices with units.
        for m in dr.added_mass.matrices:
            assert m.matrix.shape == (6, 6)
            assert m.units
        # Validation report file written.
        assert result.validation_report_path is not None
        assert result.validation_report_path.exists()

    def test_water_depth_retained_from_spec(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        runner = OrcaWaveRunner(RunConfig(output_dir=tmp_path, use_api=True))
        runner.prepare(spec)
        result = _run_api(runner, _make_fake_diffraction())
        expected = float(spec.environment.water_depth) \
            if isinstance(spec.environment.water_depth, (int, float)) else 0.0
        assert result.diffraction_results.water_depth == expected


class TestOrcaWaveLoadResultsFallback:
    """Empty live attrs -> mandatory d.LoadResults(.owr) fallback fires."""

    def test_fallback_invoked_and_builds_results(
        self, ship_raos_spec_path, tmp_path
    ):
        spec = _load_spec(ship_raos_spec_path)
        runner = OrcaWaveRunner(RunConfig(output_dir=tmp_path, use_api=True))
        runner.prepare(spec)
        fake_diff = _make_fake_diffraction(populate_live=False)
        result = _run_api(runner, fake_diff)

        # LoadResults was called with the saved .owr path.
        fake_diff.LoadResults.assert_called_once()
        called_arg = fake_diff.LoadResults.call_args[0][0]
        assert called_arg.endswith(".owr")
        assert result.status == RunStatus.COMPLETED
        assert result.validation_verdict == "PASS"
        assert result.diffraction_results is not None


class TestOrcaWaveStrictMode:
    """Strict-mode escalation of FAIL verdicts."""

    def test_strict_fail_marks_run_failed(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        runner = OrcaWaveRunner(
            RunConfig(output_dir=tmp_path, use_api=True, validation_strict=True)
        )
        runner.prepare(spec)
        result = _run_api(runner, _make_fake_diffraction(negative_heave=True))

        assert result.validation_verdict == "FAIL"
        assert result.status == RunStatus.FAILED
        assert result.return_code == -2

    def test_non_strict_fail_stays_completed(
        self, ship_raos_spec_path, tmp_path
    ):
        spec = _load_spec(ship_raos_spec_path)
        runner = OrcaWaveRunner(
            RunConfig(output_dir=tmp_path, use_api=True, validation_strict=False)
        )
        runner.prepare(spec)
        result = _run_api(runner, _make_fake_diffraction(negative_heave=True))

        assert result.validation_verdict == "FAIL"
        assert result.status == RunStatus.COMPLETED
        assert result.return_code == 0


# --- host-aware thread-count resolution (dm#1555) ----------------------------


def test_default_thread_count_is_90_percent_of_cores_floored() -> None:
    from digitalmodel.hydrodynamics.diffraction import orcawave_runner as runner

    assert runner.default_thread_count(64) == 57
    assert runner.default_thread_count(4) == 3
    assert runner.default_thread_count(1) == 1  # never below 1


def test_resolve_thread_count_explicit_wins_and_none_uses_host_default(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    from digitalmodel.hydrodynamics.diffraction import orcawave_runner as runner

    monkeypatch.setattr(runner.os, "cpu_count", lambda: 64)
    assert runner.resolve_thread_count(None) == 57
    assert runner.resolve_thread_count(8) == 8


def test_run_orcawave_dry_run_resolves_thread_count_into_config(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    from digitalmodel.hydrodynamics.diffraction import orcawave_runner as runner

    monkeypatch.setattr(runner.os, "cpu_count", lambda: 64)
    captured: dict = {}

    class _FakeRunner:
        def __init__(self, config):
            captured["thread_count"] = config.thread_count

        def run(self, spec, spec_path=None, assumption_ledger=None):
            from types import SimpleNamespace

            return SimpleNamespace(status="dry_run")

    monkeypatch.setattr(runner, "OrcaWaveRunner", _FakeRunner)
    spec = MagicMock()
    runner.run_orcawave(spec, output_dir=tmp_path, dry_run=True)
    assert captured["thread_count"] == 57
    runner.run_orcawave(spec, output_dir=tmp_path, dry_run=True, thread_count=8)
    assert captured["thread_count"] == 8


# ---------------------------------------------------------------------------
# Pre-flight covers auxiliary meshes (#500/#605 review sweep)
# ---------------------------------------------------------------------------


class TestAuxiliaryMeshPreflight:
    """_validate_mesh_references must cover damping-lid / control-surface /
    free-surface-zone meshes, not just body meshes: packaging silently skips
    missing source files, so the pre-flight is the only guard against
    shipping a package with dangling auxiliary references."""

    def test_missing_damping_lid_mesh_warns(self, ship_raos_spec_path, tmp_path):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            DampingLidSpec,
        )

        spec = _load_spec(ship_raos_spec_path)
        spec.damping_lid = DampingLidSpec(
            mesh_file="lid_missing.gdf", damping_factor=0.05
        )
        config = RunConfig(output_dir=tmp_path, dry_run=True, copy_mesh_files=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec, spec_path=ship_raos_spec_path)
        assert result.error_message is not None
        assert "Damping lid mesh" in result.error_message
        assert "lid_missing.gdf" in result.error_message

    def test_missing_control_surface_mesh_warns(
        self, ship_raos_spec_path, tmp_path
    ):
        from digitalmodel.hydrodynamics.diffraction.input_schemas import (
            ControlSurfaceSpec,
        )

        spec = _load_spec(ship_raos_spec_path)
        spec.vessel.control_surface = ControlSurfaceSpec(
            type="mesh", mesh_file="cs_missing.gdf"
        )
        config = RunConfig(output_dir=tmp_path, dry_run=True, copy_mesh_files=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec, spec_path=ship_raos_spec_path)
        assert result.error_message is not None
        assert "control surface mesh" in result.error_message
        assert "cs_missing.gdf" in result.error_message

    def test_all_meshes_present_no_warning(self, ship_raos_spec_path, tmp_path):
        spec = _load_spec(ship_raos_spec_path)
        config = RunConfig(output_dir=tmp_path, dry_run=True, copy_mesh_files=True)
        runner = OrcaWaveRunner(config)
        result = runner.prepare(spec, spec_path=ship_raos_spec_path)
        assert result.error_message is None
