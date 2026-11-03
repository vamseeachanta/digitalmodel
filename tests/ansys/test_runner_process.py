"""Process-tree tests use synthetic supervisors or the Python interpreter only.

Timeouts and delays below are test inputs, not engineering acceptance limits.
Expected failure is derived independently: a fatal log plus a non-exiting child
cannot satisfy the runner's fresh-log, successful-exit completion contract.
"""

import subprocess
import sys
import time

import psutil
import pytest

from digitalmodel.ansys.runner_process import ProcessCleanupError, ProcessSettlementError, run_process


def test_descendant_grace_kills_child_that_inherits_streams(tmp_path):
    """A child retains the streams after its parent exits; capture must not hang."""
    code = (
        "import pathlib, subprocess, sys; "
        "p = subprocess.Popen([sys.executable, '-c', 'import time; time.sleep(60)']); "
        "pathlib.Path('child.pid').write_text(str(p.pid), encoding='ascii'); "
        "print('*** FATAL ***', flush=True)"
    )
    start = time.monotonic()
    with pytest.raises(ProcessSettlementError) as caught:
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

    retained = {}
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


@pytest.mark.parametrize("budget,expected_grace", [(7200, 5), (2, 2)])
@pytest.mark.parametrize("return_code", [0, 7])
def test_leader_exit_bounds_descendant_grace(tmp_path, monkeypatch, budget, expected_grace, return_code):
    """A finished leader gets at most five seconds, preserving its observed code."""
    from digitalmodel.ansys import runner_process as module

    clock = [0.0]
    terminated = []

    class Supervisor:
        def start(self, argv, cwd, stdout, stderr):
            stdout.write(b"leader output")

        def wait(self, timeout):
            return return_code

        def active(self):
            return not terminated

        def terminate(self):
            terminated.append(clock[0])

        def close(self):
            pass

    monkeypatch.setattr(module, "supervisor_factory", Supervisor)
    monkeypatch.setattr(module.time, "monotonic", lambda: clock[0])
    monkeypatch.setattr(module.time, "sleep", lambda duration: clock.__setitem__(0, clock[0] + duration))
    with pytest.raises(OSError, match="descendant") as caught:
        run_process(["synthetic"], cwd=tmp_path, timeout=budget)
    assert type(caught.value).__name__ == "ProcessSettlementError"
    assert caught.value.return_code == return_code
    assert caught.value.stdout == b"leader output"
    assert terminated == [expected_grace]


def test_retained_supervisor_can_be_retried(tmp_path, monkeypatch):
    """The exact retained ownership handle is released after a successful retry."""
    from digitalmodel.ansys import runner_process as module

    broken = [True]
    closed = []

    class Supervisor:
        def start(self, *args):
            pass

        def wait(self, timeout):
            raise subprocess.TimeoutExpired("synthetic", timeout)

        def terminate(self):
            if broken[0]:
                raise OSError("synthetic termination failure")

        def active(self):
            return 0

        def close(self):
            closed.append(True)

    retained = {}
    monkeypatch.setattr(module, "_UNSETTLED", retained)
    monkeypatch.setattr(module, "supervisor_factory", Supervisor)
    with pytest.raises(ProcessCleanupError) as caught:
        run_process(["synthetic"], cwd=tmp_path, timeout=1)
    token = caught.value.retained_supervisor_token
    assert token in retained
    assert "timeout after 1 seconds" in str(caught.value)
    retry = module.settle_retained(token)
    assert retry["retained_supervisor_token"] == token
    assert len(retained) == 1
    broken[0] = False
    retry = module.settle_retained(token)
    assert retry["retained_supervisor_token"] is None
    assert not retained
    assert closed == [True]


@pytest.mark.parametrize("interruption", [KeyboardInterrupt, SystemExit])
def test_cleanup_failure_preserves_control_exception(tmp_path, monkeypatch, interruption):
    from digitalmodel.ansys import runner_process as module

    class Supervisor:
        def start(self, *args):
            raise interruption("synthetic interruption")

        def terminate(self):
            raise OSError("synthetic cleanup failure")

    monkeypatch.setattr(module, "_UNSETTLED", {})
    monkeypatch.setattr(module, "supervisor_factory", Supervisor)
    with pytest.raises(interruption):
        run_process(["synthetic"], cwd=tmp_path, timeout=1)


def test_settlement_failure_metadata_reaches_runner(tmp_path, monkeypatch):
    from pathlib import Path
    from digitalmodel.ansys import runner_process as module
    from digitalmodel.ansys.runner import ANSYSRunner, ANSYSRunStatus, run_ansys

    script = tmp_path / "model.inp"
    script.write_text("FINISH", encoding="utf-8")
    monkeypatch.setattr(ANSYSRunner, "_detect_executable", lambda self: Path("synthetic"))

    def fail(*args, **kwargs):
        raise module.ProcessSettlementError(
            "owned descendants remain", b"partial", b"", return_code=7,
            retained_supervisor_token="synthetic-token",
        )

    monkeypatch.setattr("digitalmodel.ansys.runner._run_solver_process", fail)
    result = run_ansys(script, output_dir=tmp_path / "output")
    assert result.status == ANSYSRunStatus.FAILED
    assert result.return_code == 7
    assert result.retained_supervisor_token == "synthetic-token"
    assert "descendants" in result.error_message
    assert "timeout" not in result.error_message.lower()


def test_cylinder_uses_shared_settlement_policy():
    from digitalmodel.ansys import cylinder_runner, runner_process

    assert cylinder_runner._CLEANUP_SECONDS == runner_process._CLEANUP_SECONDS
    assert cylinder_runner._POLL_SECONDS == runner_process._POLL_SECONDS


@pytest.mark.parametrize("unreadable", [("stdout",), ("stderr",), ("stdout", "stderr")])
@pytest.mark.parametrize("cleanup_fails", [False, True])
@pytest.mark.parametrize("cause", ["timeout", "wait"])
def test_capture_failure_preserves_cause_and_cleanup(tmp_path, monkeypatch, unreadable,
                                                    cleanup_fails, cause):
    """Independent read failures cannot hide the invocation or its ownership handle."""
    from digitalmodel.ansys import runner_process as module

    original = (subprocess.TimeoutExpired("synthetic", 1) if cause == "timeout"
                else OSError("synthetic wait failure"))
    streams, read_attempts, retained = {}, [], {}
    read = module._read

    class Supervisor:
        def start(self, argv, cwd, stdout, stderr):
            streams.update(stdout=stdout, stderr=stderr)
            stdout.write(b"partial stdout")
            stderr.write(b"partial stderr")

        def wait(self, timeout):
            raise original

        def terminate(self):
            if cleanup_fails:
                raise OSError("synthetic termination failure")

        def active(self):
            return 0

        def close(self):
            pass

    def read_capture(stream):
        name = next(name for name, value in streams.items() if value is stream)
        read_attempts.append(name)
        if name in unreadable:
            raise OSError(f"synthetic {name} read failure")
        return read(stream)

    monkeypatch.setattr(module, "_read", read_capture)
    monkeypatch.setattr(module, "_UNSETTLED", retained)
    monkeypatch.setattr(module, "supervisor_factory", Supervisor)
    with pytest.raises(ProcessCleanupError) as caught:
        run_process(["synthetic"], cwd=tmp_path, timeout=1)
    error = caught.value
    assert error.__cause__ is original
    assert ("timeout after 1 seconds" if cause == "timeout" else str(original)) in str(error)
    assert read_attempts == ["stdout", "stderr"]
    assert error.stdout == (b"" if "stdout" in unreadable else b"partial stdout")
    assert error.stderr == (b"" if "stderr" in unreadable else b"partial stderr")
    assert all(f"synthetic {name} read failure" in str(error) for name in unreadable)
    assert (error.retained_supervisor_token in retained) == cleanup_fails
    assert len(retained) == int(cleanup_fails)
    if not cleanup_fails:
        assert error.retained_supervisor_token is None
    assert ("synthetic termination failure" in str(error)) == cleanup_fails
