"""Process-tree tests use synthetic supervisors or the Python interpreter only.

Timeouts and delays below are test inputs, not engineering acceptance limits.
Expected failure is derived independently: a fatal log plus a non-exiting child
cannot satisfy the runner's fresh-log, successful-exit completion contract.
"""

import os
import subprocess
import sys
import time

import psutil
import pytest

from digitalmodel.ansys.runner_process import ProcessCleanupError, run_process


@pytest.mark.skipif(os.name == "nt", reason="POSIX process-group integration")
def test_timeout_kills_child_that_inherits_streams(tmp_path):
    """A child retains the streams after its parent exits; capture must not hang."""
    code = (
        "import pathlib, subprocess, sys; "
        "p = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)']); "
        "pathlib.Path('child.pid').write_text(str(p.pid), encoding='ascii'); "
        "print('*** FATAL ***', flush=True)"
    )
    start = time.monotonic()
    with pytest.raises(subprocess.TimeoutExpired) as caught:
        run_process([sys.executable, "-c", code], cwd=tmp_path, timeout=0.5)
    assert b"*** FATAL ***" in caught.value.stdout
    assert time.monotonic() - start < 10  # Test watchdog, not a solver limit.
    pid = int((tmp_path / "child.pid").read_text(encoding="ascii"))
    assert not psutil.pid_exists(pid) or psutil.Process(pid).status() == psutil.STATUS_ZOMBIE


def test_timeout_retains_partial_streams_and_closes_supervisor(tmp_path, monkeypatch):
    events = []

    class Supervisor:
        def start(self, argv, cwd, stdout, stderr):
            stdout.write(b"partial stdout\xff")
            stderr.write(b"partial stderr")

        def wait(self, timeout):
            raise subprocess.TimeoutExpired("synthetic", timeout)

        def terminate(self):
            events.append("terminate")

        def active(self):
            return 0

        def close(self):
            events.append("close")

    monkeypatch.setattr("digitalmodel.ansys.runner_process.supervisor_factory", Supervisor)
    with pytest.raises(subprocess.TimeoutExpired) as caught:
        run_process(["synthetic"], cwd=tmp_path, timeout=1)
    assert events == ["terminate", "close"]
    assert caught.value.stdout == b"partial stdout\xff"
    assert caught.value.stderr == b"partial stderr"


@pytest.mark.parametrize("failure", ["start", "wait", "active", "terminate", "close"])
def test_supervision_failure_cannot_return_success(tmp_path, monkeypatch, failure):
    """Every ownership operation must succeed before completion can be returned."""
    class Supervisor:
        def start(self, argv, cwd, stdout, stderr):
            stdout.write(b"partial")
            if failure == "start":
                raise OSError("synthetic start failure")

        def wait(self, timeout):
            if failure == "wait":
                raise OSError("synthetic wait failure")
            if failure == "terminate":
                raise subprocess.TimeoutExpired("synthetic", timeout)
            return 0

        def active(self):
            if failure == "active":
                raise OSError("synthetic active failure")
            return 0

        def terminate(self):
            if failure == "terminate":
                raise OSError("synthetic terminate failure")

        def close(self):
            if failure == "close":
                raise OSError("synthetic close failure")

    retained = []
    monkeypatch.setattr("digitalmodel.ansys.runner_process._UNSETTLED", retained)
    monkeypatch.setattr("digitalmodel.ansys.runner_process.supervisor_factory", Supervisor)
    with pytest.raises(OSError):
        run_process(["synthetic"], cwd=tmp_path, timeout=1)
    assert bool(retained) == (failure in ("active", "terminate", "close"))


def test_completed_process_decodes_utf8_independently_of_locale(tmp_path, monkeypatch):
    """Literal UTF-8 bytes give the independently specified text, replacing bad bytes."""
    class Supervisor:
        def start(self, argv, cwd, stdout, stderr):
            stdout.write("résumé".encode("utf-8") + b"\xff")
            stderr.write(b"warning")

        def wait(self, timeout):
            return 0

        def active(self):
            return 0

        def close(self):
            pass

    monkeypatch.setattr("digitalmodel.ansys.runner_process.supervisor_factory", Supervisor)
    result = run_process(["synthetic"], cwd=tmp_path, timeout=1)
    assert result.returncode == 0
    assert result.stdout == "résumé\ufffd"
    assert result.stderr == "warning"


def test_cleanup_failure_retains_runner_evidence(tmp_path, monkeypatch):
    from pathlib import Path

    from digitalmodel.ansys.runner import ANSYSRunner, ANSYSRunStatus, run_ansys

    script = tmp_path / "model.inp"
    script.write_text("FINISH", encoding="utf-8")
    monkeypatch.setattr(ANSYSRunner, "_detect_executable", lambda self: Path("synthetic"))

    def fail(argv, **kwargs):
        (Path(kwargs["cwd"]) / "model.out").write_text("*** FATAL ***", encoding="utf-8")
        raise ProcessCleanupError("cleanup uncertain", b"partial", b"stderr")

    monkeypatch.setattr("digitalmodel.ansys.runner._run_solver_process", fail)
    result = run_ansys(script, output_dir=tmp_path / "output")
    assert result.status == ANSYSRunStatus.FAILED
    assert result.error_message == "cleanup uncertain"
    assert result.return_code is None
    assert result.stdout == "partial"
    assert result.stderr == "stderr"
    assert result.log_file == tmp_path / "output/model.out"


def test_stream_allocation_failure_does_not_allocate_supervisor(tmp_path, monkeypatch):
    """A temporary-file failure must occur before a process ownership handle exists."""
    def no_stream():
        raise OSError("temporary stream unavailable")

    monkeypatch.setattr("digitalmodel.ansys.runner_process.tempfile.TemporaryFile", no_stream)
    monkeypatch.setattr(
        "digitalmodel.ansys.runner_process.supervisor_factory",
        lambda: pytest.fail("supervisor allocated before streams"),
    )
    with pytest.raises(OSError, match="temporary stream unavailable"):
        run_process(["synthetic"], cwd=tmp_path, timeout=1)


def test_cleanup_and_evidence_errors_are_both_reported(tmp_path, monkeypatch):
    from pathlib import Path

    from digitalmodel.ansys.runner import ANSYSRunner, ANSYSRunStatus, run_ansys

    script = tmp_path / "model.inp"
    script.write_text("FINISH", encoding="utf-8")
    monkeypatch.setattr(ANSYSRunner, "_detect_executable", lambda self: Path("synthetic"))

    def fail(*args, **kwargs):
        raise ProcessCleanupError("cleanup uncertain", b"partial", b"")

    def no_evidence(*args, **kwargs):
        raise OSError("evidence unavailable")

    monkeypatch.setattr("digitalmodel.ansys.runner._run_solver_process", fail)
    monkeypatch.setattr(ANSYSRunner, "_collect_evidence", no_evidence)
    result = run_ansys(script, output_dir=tmp_path / "output")
    assert result.status == ANSYSRunStatus.FAILED
    assert "cleanup uncertain" in result.error_message
    assert "evidence unavailable" in result.error_message
    assert "timeout" not in result.error_message.lower()
    assert result.stdout == "partial"


@pytest.mark.parametrize("return_code", [0, 7])
def test_real_python_exit_metadata(tmp_path, return_code):
    """Exit codes and text are literal synthetic inputs, independent of the runner."""
    result = run_process(
        [sys.executable, "-c", "import sys; print('done'); sys.exit(int(sys.argv[1]))",
         str(return_code)],
        cwd=tmp_path, timeout=5,
    )
    assert result.returncode == return_code
    assert result.stdout.strip() == "done"
    assert result.stderr == ""
