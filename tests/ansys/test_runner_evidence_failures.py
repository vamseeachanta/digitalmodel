"""Offline stale-log and filesystem failure regressions for the result contract."""

from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.ansys.runner import ANSYSRunner, ANSYSRunStatus, run_ansys


def _setup(tmp_path, monkeypatch, write_log=True):
    script = tmp_path / "model.inp"
    script.write_text("FINISH\n")
    output = tmp_path / "output"
    output.mkdir()
    calls = []

    def simulate(*args, **kwargs):
        calls.append(True)
        if write_log:
            (output / "model.out").write_text("completed")
        return SimpleNamespace(returncode=0, stdout="solver stdout", stderr="solver stderr")

    monkeypatch.setattr(ANSYSRunner, "_detect_executable", lambda self: Path("fake"))
    monkeypatch.setattr("digitalmodel.ansys.runner.subprocess.run", simulate)
    return script, output, calls


@pytest.mark.parametrize("old_log", [None, "completed", "*** ERROR *** old run"])
def test_missing_or_unchanged_log_cannot_complete(tmp_path, monkeypatch, old_log):
    script, output, _ = _setup(tmp_path, monkeypatch, write_log=False)
    if old_log is not None:
        (output / "model.out").write_text(old_log)
    result = run_ansys(script, output_dir=output)
    assert result.status == ANSYSRunStatus.FAILED
    assert result.log_file is None
    assert "fresh" in result.error_message.lower()
    assert result.return_code == 0
    assert result.stdout == "solver stdout"


@pytest.mark.parametrize("phase", ["before", "after"])
def test_snapshot_permission_failure_returns_record(tmp_path, monkeypatch, phase):
    script, output, calls = _setup(tmp_path, monkeypatch)
    original = ANSYSRunner._snapshot_result_files

    def snapshot(directory):
        if bool(calls) == (phase == "after"):
            raise PermissionError("snapshot locked")
        return original(directory)

    monkeypatch.setattr(ANSYSRunner, "_snapshot_result_files", staticmethod(snapshot))
    result = run_ansys(script, output_dir=output)
    assert result.status == ANSYSRunStatus.FAILED
    assert "snapshot locked" in result.error_message
    assert result.return_code == (0 if phase == "after" else None)
    assert result.stdout == ("solver stdout" if phase == "after" else "")
    assert result.stderr == ("solver stderr" if phase == "after" else "")
    assert bool(calls) == (phase == "after")
    assert result.duration_seconds >= 0


def test_file_disappearing_after_enumeration_returns_record(tmp_path, monkeypatch):
    script, output, calls = _setup(tmp_path, monkeypatch)
    original = ANSYSRunner._capture_result_files
    monkeypatch.setattr(
        ANSYSRunner, "_capture_result_files",
        staticmethod(lambda path: [output / "vanished.csv"] if calls else original(path)),
    )
    result = run_ansys(script, output_dir=output)
    assert result.status == ANSYSRunStatus.FAILED
    assert result.return_code == 0
    assert result.stdout == "solver stdout"
    assert result.stderr == "solver stderr"
    assert result.log_file is None


def test_unreadable_fresh_log_returns_record(tmp_path, monkeypatch):
    script, output, _ = _setup(tmp_path, monkeypatch)
    original = Path.open

    def open_file(path, *args, **kwargs):
        mode = args[0] if args else kwargs.get("mode", "r")
        if path == output / "model.out" and "w" not in mode:
            raise PermissionError("log locked")
        return original(path, *args, **kwargs)

    monkeypatch.setattr(Path, "open", open_file)
    result = run_ansys(script, output_dir=output)
    assert result.status == ANSYSRunStatus.FAILED
    assert "log locked" in result.error_message
    assert result.log_file is None
    assert result.return_code == 0
    assert result.stdout == "solver stdout"
    assert result.stderr == "solver stderr"


def test_log_error_scan_streams_across_chunk_boundary(tmp_path, monkeypatch):
    log = tmp_path / "model.out"
    log.write_text("x" * 65530 + "*** ERROR ***" + "x" * 70000)
    monkeypatch.setattr(Path, "read_text", lambda *a, **k: pytest.fail("unbounded read"))
    assert "*** ERROR ***" in ANSYSRunner()._detect_error(0, log)
