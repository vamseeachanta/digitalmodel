"""OrcaWave doctor diagnostics (#613).

Covers the unlicensed-Linux behavior (real on this host) and mocked-OrcFxAPI
behavior, plus the documented exit-code policy.
"""

from __future__ import annotations

from pathlib import Path
from unittest.mock import patch

from click.testing import CliRunner

from digitalmodel.hydrodynamics.diffraction import orcawave_doctor
from digitalmodel.hydrodynamics.diffraction.cli import cli
from digitalmodel.hydrodynamics.diffraction.orcawave_doctor import run_doctor


def _by_name(checks, name):
    return next(c for c in checks if c.name == name)


class TestRunDoctorUnlicensedHost:
    """Real behavior on this Linux box (no OrcFxAPI, no OrcaWave.exe)."""

    def test_no_stack_trace_and_dry_run_capability(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORCAWAVE_PATH", raising=False)
        checks, capability = run_doctor(output_dir=tmp_path)
        assert capability == "dry-run-only"
        api = _by_name(checks, "OrcFxAPI binding")
        assert api.status == "WARN"
        assert "not importable" in api.detail

    def test_capability_check_mentions_dry_run(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORCAWAVE_PATH", raising=False)
        checks, _ = run_doctor(output_dir=tmp_path)
        host = _by_name(checks, "Host capability")
        assert host.status == "WARN"
        assert "dry-run only" in host.detail

    def test_output_dir_writable_pass(self, tmp_path):
        checks, _ = run_doctor(output_dir=tmp_path / "new")
        assert _by_name(checks, "Output directory").status == "PASS"

    def test_orcawave_path_env_reported(self, tmp_path, monkeypatch):
        exe = tmp_path / "orcawave"
        exe.write_text("")
        monkeypatch.setenv("ORCAWAVE_PATH", str(exe))
        checks, capability = run_doctor(output_dir=tmp_path)
        env = _by_name(checks, "ORCAWAVE_PATH")
        assert env.status == "PASS"
        assert str(exe) in env.detail
        assert capability == "subprocess-solve"

    def test_explicit_executable_missing_is_fail(self, tmp_path):
        checks, _ = run_doctor(
            output_dir=tmp_path, executable=tmp_path / "nope.exe"
        )
        assert _by_name(checks, "Explicit executable").status == "FAIL"


class TestRunDoctorMockedApi:
    def test_api_present_gives_api_solve(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORCAWAVE_PATH", raising=False)
        with patch.object(
            orcawave_doctor,
            "_orcfxapi_info",
            return_value=(True, "OrcFxAPI importable (DLL version 11.6)"),
        ):
            checks, capability = run_doctor(output_dir=tmp_path)
        assert capability == "api-solve"
        assert _by_name(checks, "OrcFxAPI binding").status == "PASS"
        license_check = _by_name(checks, "OrcaWave license")
        assert "solve time" in license_check.detail


class TestCliExitPolicy:
    def test_default_exit_zero_on_dry_run_host(self, tmp_path, monkeypatch):
        monkeypatch.delenv("ORCAWAVE_PATH", raising=False)
        result = CliRunner().invoke(
            cli, ["orcawave-doctor", "-o", str(tmp_path)]
        )
        assert result.exit_code == 0, result.output
        assert "dry-run only" in result.output
        assert "[OK] Host usable (dry-run-only)" in result.output

    def test_require_solver_exits_nonzero_on_dry_run_host(
        self, tmp_path, monkeypatch
    ):
        monkeypatch.delenv("ORCAWAVE_PATH", raising=False)
        result = CliRunner().invoke(
            cli, ["orcawave-doctor", "-o", str(tmp_path), "--require-solver"]
        )
        assert result.exit_code == 1
        assert "--require-solver" in result.output

    def test_explicit_missing_executable_exits_nonzero(self, tmp_path):
        result = CliRunner().invoke(
            cli,
            [
                "orcawave-doctor",
                "-o",
                str(tmp_path),
                "--executable",
                str(tmp_path / "nope.exe"),
            ],
        )
        assert result.exit_code == 1
        assert "Hard failures" in result.output
