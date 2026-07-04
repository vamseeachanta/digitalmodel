"""ANSYS MAPDL solver runner — fail-closed, subprocess-based (digitalmodel #940)."""

from __future__ import annotations

from pathlib import Path
from types import SimpleNamespace

from digitalmodel.ansys.runner import (
    ANSYSRunConfig,
    ANSYSRunner,
    ANSYSRunStatus,
    run_ansys,
)


def _script(tmp_path: Path) -> Path:
    script = tmp_path / "model.inp"
    script.write_text("/PREP7\nFINISH\n")
    return script


def _completed(returncode: int):
    return SimpleNamespace(returncode=returncode, stdout="ok", stderr="")


def test_dry_run_requested_returns_dry_run(tmp_path: Path):
    result = run_ansys(_script(tmp_path), output_dir=tmp_path / "out", dry_run=True)
    assert result.status == ANSYSRunStatus.DRY_RUN


def test_missing_script_fails_closed(tmp_path: Path):
    result = run_ansys(tmp_path / "nope.inp", output_dir=tmp_path / "out")
    assert result.status == ANSYSRunStatus.FAILED
    assert "not found" in result.error_message


def test_no_executable_falls_back_to_dry_run(tmp_path: Path, monkeypatch):
    monkeypatch.setattr(ANSYSRunner, "_detect_executable", lambda self: None)
    result = run_ansys(_script(tmp_path), output_dir=tmp_path / "out", dry_run=False)
    assert result.status == ANSYSRunStatus.DRY_RUN
    assert "No ANSYS/MAPDL executable" in result.error_message


def test_successful_solve_marks_completed(tmp_path: Path, monkeypatch):
    monkeypatch.setattr(
        ANSYSRunner, "_detect_executable", lambda self: Path("/fake/mapdl")
    )
    monkeypatch.setattr(
        "digitalmodel.ansys.runner.subprocess.run",
        lambda *a, **k: _completed(0),
    )
    result = run_ansys(_script(tmp_path), output_dir=tmp_path / "out")
    assert result.status == ANSYSRunStatus.COMPLETED
    assert result.return_code == 0


def test_nonzero_exit_marks_failed(tmp_path: Path, monkeypatch):
    monkeypatch.setattr(
        ANSYSRunner, "_detect_executable", lambda self: Path("/fake/mapdl")
    )
    monkeypatch.setattr(
        "digitalmodel.ansys.runner.subprocess.run",
        lambda *a, **k: _completed(8),
    )
    result = run_ansys(_script(tmp_path), output_dir=tmp_path / "out")
    assert result.status == ANSYSRunStatus.FAILED
    assert "non-zero exit code 8" in result.error_message


def test_solver_error_in_log_marks_failed(tmp_path: Path, monkeypatch):
    out_dir = tmp_path / "out"
    out_dir.mkdir()
    (out_dir / "model.out").write_text("... *** ERROR *** element distorted ...")
    monkeypatch.setattr(
        ANSYSRunner, "_detect_executable", lambda self: Path("/fake/mapdl")
    )
    monkeypatch.setattr(
        "digitalmodel.ansys.runner.subprocess.run",
        lambda *a, **k: _completed(0),  # rc=0 but the log has an error
    )
    result = run_ansys(_script(tmp_path), output_dir=out_dir)
    assert result.status == ANSYSRunStatus.FAILED
    assert "*** ERROR ***" in result.error_message


def test_invocation_exception_fails_closed(tmp_path: Path, monkeypatch):
    monkeypatch.setattr(
        ANSYSRunner, "_detect_executable", lambda self: Path("/fake/mapdl")
    )

    def boom(*a, **k):
        raise OSError("exec format error")

    monkeypatch.setattr("digitalmodel.ansys.runner.subprocess.run", boom)
    result = run_ansys(_script(tmp_path), output_dir=tmp_path / "out")
    assert result.status == ANSYSRunStatus.FAILED
    assert "MAPDL invocation failed" in result.error_message


def test_config_executable_path_used_when_present(tmp_path: Path):
    exe = tmp_path / "mapdl"
    exe.write_text("#!/bin/sh\n")
    runner = ANSYSRunner(
        ANSYSRunConfig(output_dir=tmp_path / "out", executable_path=exe)
    )
    assert runner._detect_executable() == exe
