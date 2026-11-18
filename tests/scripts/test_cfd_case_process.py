# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise recorded-PID checks with ordinary Python children, never a solver.

Expected outcomes follow independent fixture facts: a child is started in a
known directory with the current interpreter; a sibling directory is a distinct
inode; closing its input ends it. No engineering coefficients or acceptance
limits apply. The subprocess timeouts are test-harness budgets only.

Sources: Linux man-pages 6.19 (2026), proc_pid_cwd(5), proc_pid_exe(5),
proc_pid_status(5), pidfd_send_signal(2); Python 3 Library Reference (2026),
os.pidfd_open and signal.pidfd_send_signal. See the process-supervision runbook
for links and the scope of the identity checks.
"""

from __future__ import annotations

import importlib.util
import os
import select
import signal
import subprocess
import sys
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/cfd/case_process.py"
spec = importlib.util.spec_from_file_location("case_process", SCRIPT)
case_process = importlib.util.module_from_spec(spec)
spec.loader.exec_module(case_process)


@pytest.fixture
def child(tmp_path):
    """The ready message proves that exec and initialisation have completed."""
    process = subprocess.Popen(
        [
            sys.executable,
            "-c",
            'import sys; print("READY", flush=True); sys.stdin.read()',
        ],
        cwd=tmp_path,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        assert select.select([process.stdout], [], [], 10)[0], "child did not start"
        assert process.stdout.readline().strip() == "READY"
        yield process
    finally:
        if process.poll() is None:
            process.terminate()
        process.communicate(timeout=10)


def check(child, tmp_path, **kwargs):
    return case_process.check_process(
        child.pid, tmp_path, Path(sys.executable), **kwargs
    )


def test_inspection_does_not_terminate_child(child, tmp_path):
    """Known cwd and interpreter match; the child remains blocked on input."""
    assert check(child, tmp_path) == "VALIDATED"
    assert child.poll() is None


def test_directory_alias_is_same_case(child, tmp_path):
    """A symlink names the same directory inode, independently of its spelling."""
    alias = tmp_path / "alias"
    alias.symlink_to(tmp_path, target_is_directory=True)
    assert check(child, alias) == "VALIDATED"


@pytest.mark.parametrize("name", ["other", "case-extra"])
def test_different_directory_is_rejected(child, tmp_path, name):
    """A directory that merely shares a path prefix is not the child's cwd."""
    other = tmp_path / name
    other.mkdir()
    with pytest.raises(case_process.ValidationError, match="working directory"):
        check(child, other, terminate=True)
    assert child.poll() is None


def test_wrong_executable_is_rejected(child, tmp_path):
    """The test source is a different inode from the running interpreter."""
    with pytest.raises(case_process.ValidationError, match="executable"):
        case_process.check_process(child.pid, tmp_path, Path(__file__), terminate=True)
    assert child.poll() is None


def test_parent_directory_does_not_include_descendant_process(child, tmp_path):
    """Exact cwd matching excludes a child working below the expected directory."""
    with pytest.raises(case_process.ValidationError, match="working directory"):
        check(child, tmp_path.parent, terminate=True)
    assert child.poll() is None


def test_full_command_pattern_matches_wrapper(tmp_path):
    """pgrep(1), OPTIONS -f: the wrapper argv contains the literal search token.

    The final shell builtin keeps bash alive while pgrep executes. Its printed
    PID independently identifies the control process that must not be a target.
    No signal is sent and no solver is started.
    """
    token = f"recorded-pid-regression-{tmp_path.name}-{os.getpid()}"
    result = subprocess.run(
        ["bash", "-c", 'echo "$$"; pgrep -f -- "$1"; :', "wrapper", token],
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert result.returncode == 0
    control, *matched = result.stdout.splitlines()
    assert control in matched


@pytest.mark.parametrize("pid", [0, -1])
def test_nonpositive_pid_is_rejected(pid, tmp_path):
    """A candidate must be an individual positive PID, never a process group."""
    with pytest.raises(case_process.ValidationError, match="positive PID"):
        case_process.check_process(pid, tmp_path, Path(sys.executable), terminate=True)


@pytest.mark.parametrize("pid_source", [os.getpid, os.getppid])
def test_self_and_controlling_parent_are_rejected(pid_source, tmp_path):
    """Even an explicitly supplied control-process PID cannot receive TERM."""
    with pytest.raises(case_process.ValidationError, match="ancestor"):
        case_process.check_process(
            pid_source(), tmp_path, Path(sys.executable), terminate=True
        )


def test_reaped_pid_is_not_evaluated(child, tmp_path):
    """EOF and wait prove the recorded child has exited before inspection."""
    child.communicate(timeout=10)
    with pytest.raises(case_process.ValidationError, match="unavailable"):
        check(child, tmp_path, terminate=True)


def test_term_reaches_only_validated_child(child, tmp_path):
    """POSIX subprocess returncode is minus the terminating signal number."""
    assert check(child, tmp_path, terminate=True) == "TERM_SENT"
    assert child.wait(timeout=10) == -signal.SIGTERM


@pytest.mark.parametrize(
    "module, attribute", [(os, "pidfd_open"), (signal, "pidfd_send_signal")]
)
def test_missing_pidfd_support_fails_closed(
    child, tmp_path, monkeypatch, module, attribute
):
    """There is no numeric-PID signalling fallback when the API is absent."""
    monkeypatch.delattr(module, attribute)
    with pytest.raises(case_process.ValidationError, match="pidfd"):
        check(child, tmp_path, terminate=True)
    assert child.poll() is None


def test_permission_error_closes_descriptor(child, tmp_path, monkeypatch):
    """An unreadable cwd is unknown, and every opened pidfd is closed."""
    opened = []
    real_open = os.pidfd_open

    def record_open(pid):
        descriptor = real_open(pid)
        opened.append(descriptor)
        return descriptor

    def deny_samefile(*args):
        raise PermissionError("metadata unreadable")

    monkeypatch.setattr(os, "pidfd_open", record_open)
    monkeypatch.setattr(os.path, "samefile", deny_samefile)
    with pytest.raises(case_process.ValidationError, match="unavailable"):
        check(child, tmp_path, terminate=True)
    assert child.poll() is None
    assert opened
    for descriptor in opened:
        with pytest.raises(OSError):
            os.fstat(descriptor)


def test_exit_after_validation_uses_pinned_descriptor(child, tmp_path, monkeypatch):
    """Exit after cwd/exe reads makes the pinned handle stale, never retargeted."""
    real_send = signal.pidfd_send_signal
    sent = []

    def exit_before_probe(descriptor, sig):
        child.communicate(timeout=10)
        sent.append(sig)
        return real_send(descriptor, sig)

    monkeypatch.setattr(signal, "pidfd_send_signal", exit_before_probe)
    with pytest.raises(case_process.ValidationError, match="unavailable"):
        check(child, tmp_path, terminate=True)
    assert signal.SIGTERM not in sent


def test_cli_defaults_to_inspection(child, tmp_path):
    result = subprocess.run(
        [sys.executable, str(SCRIPT), str(child.pid), str(tmp_path), sys.executable],
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert result.returncode == 0
    assert result.stdout.strip() == "VALIDATED"
    assert child.poll() is None


def test_cli_reports_unknown_without_paths(tmp_path):
    """A rejected group PID prints a reason, with no environment-specific path."""
    result = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "0",
            str(tmp_path),
            sys.executable,
            "--terminate",
        ],
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert result.returncode != 0
    assert result.stdout == ""
    assert result.stderr.startswith("NOT_EVALUATED:")
    assert str(tmp_path) not in result.stderr


def test_cli_refuses_grandparent_even_with_matching_metadata(tmp_path):
    """A shell wrapper makes this test process an ancestor beyond the parent."""
    result = subprocess.run(
        [
            "bash",
            "-c",
            '"$@"; result=$?; exit "$result"',
            "wrapper",
            sys.executable,
            str(SCRIPT),
            str(os.getpid()),
            str(Path.cwd()),
            sys.executable,
            "--terminate",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert result.returncode != 0
    assert "controlling ancestor" in result.stderr
