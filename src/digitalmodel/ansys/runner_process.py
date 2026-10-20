"""Bounded execution with owned process-tree cleanup and binary stream capture.

Python 3.11 subprocess documentation, Popen.communicate: a timeout alone does
not kill children. File-backed streams avoid waiting for inherited pipe handles.
The existing cylinder_process supervisors provide Windows Job Objects and POSIX
process groups; POSIX descendants that create a new session remain uncontained.
"""

import subprocess
import tempfile
import time

from digitalmodel.ansys.cylinder_process import supervisor_factory

# Execution policy follows cylinder_runner._settle, not an engineering limit.
_CLEANUP_SECONDS = 5.0
_POLL_SECONDS = 0.01
_UNSETTLED = []  # Retain ownership handles if settlement cannot be established.


class ProcessCleanupError(OSError):
    """Cleanup uncertainty, carrying streams without claiming a successful run."""

    def __init__(self, message, stdout, stderr):
        super().__init__(message)
        self.stdout, self.stderr = stdout, stderr


def _wait_empty(supervisor, deadline):
    while supervisor.active():
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise subprocess.TimeoutExpired("owned process tree", 0)
        time.sleep(min(_POLL_SECONDS, remaining))


def _cleanup(supervisor):
    """Only the current invocation's owned tree is terminated; no name search."""
    try:
        supervisor.terminate()
        _wait_empty(supervisor, time.monotonic() + _CLEANUP_SECONDS)
        supervisor.close()
    except Exception as exc:
        _UNSETTLED.append(supervisor)
        return f"owned process cleanup could not be established: {exc}"
    return None


def _read(stream):
    stream.seek(0)
    return stream.read()


def run_process(argv, *, cwd, timeout):
    """Return decoded streams, or raise with partial bytes after bounded cleanup.

    The supplied wall-clock budget covers launch, leader wait and descendant
    settlement. Failure cleanup has a separate bounded grace period. No solver
    discovery, license query or executable-name termination occurs here.
    """
    deadline = time.monotonic() + timeout
    with tempfile.TemporaryFile() as stdout, tempfile.TemporaryFile() as stderr:
        supervisor = supervisor_factory()
        try:
            supervisor.start(argv, cwd, stdout, stderr)
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise subprocess.TimeoutExpired(argv, timeout)
            return_code = supervisor.wait(remaining)
            _wait_empty(supervisor, deadline)
            supervisor.close()
        except BaseException as exc:
            cleanup_error = _cleanup(supervisor)
            output, errors = _read(stdout), _read(stderr)
            if cleanup_error:
                raise ProcessCleanupError(cleanup_error, output, errors) from exc
            if isinstance(exc, subprocess.TimeoutExpired):
                raise subprocess.TimeoutExpired(argv, timeout, output, errors) from exc
            raise
        return subprocess.CompletedProcess(
            argv, return_code,
            _read(stdout).decode("utf-8", errors="replace"),
            _read(stderr).decode("utf-8", errors="replace"),
        )
