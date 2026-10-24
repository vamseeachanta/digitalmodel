"""Bounded execution with owned process-tree cleanup and binary stream capture.

Python 3.11 subprocess documentation, Popen.communicate: a timeout alone does
not kill children. File-backed streams avoid waiting for inherited pipe handles.
The existing cylinder_process supervisors provide Windows Job Objects and POSIX
process groups; POSIX descendants that create a new session remain uncontained.
"""

import subprocess
import tempfile
import time
import uuid

from digitalmodel.ansys.cylinder_process import supervisor_factory

# Execution policy follows cylinder_runner._settle, not an engineering limit.
_CLEANUP_SECONDS = 5.0
_POLL_SECONDS = 0.01
_UNSETTLED = {}  # Exact ownership handles remain available for settlement retry.


class ProcessCleanupError(OSError):
    """Cleanup uncertainty, carrying streams without claiming a successful run."""

    def __init__(self, message, stdout, stderr, *, return_code=None,
                 retained_supervisor_token=None):
        super().__init__(message)
        self.stdout, self.stderr = stdout, stderr
        self.return_code = return_code
        self.retained_supervisor_token = retained_supervisor_token


class ProcessSettlementError(ProcessCleanupError):
    """The leader exited, but owned descendants exceeded their natural grace."""


def _wait_empty(supervisor, deadline):
    while supervisor.active():
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise subprocess.TimeoutExpired("owned process tree", 0)
        time.sleep(min(_POLL_SECONDS, remaining))


def _cleanup(supervisor, token=None):
    """Only the current invocation's owned tree is terminated; no name search."""
    try:
        supervisor.terminate()
        _wait_empty(supervisor, time.monotonic() + _CLEANUP_SECONDS)
        supervisor.close()
    except Exception as exc:
        token = token or uuid.uuid4().hex
        _UNSETTLED[token] = supervisor
        return f"owned process cleanup could not be established: {exc}", token
    if token is not None:
        del _UNSETTLED[token]
    return None, None


def settle_retained(token):
    """Retry cleanup of an exact retained ownership handle, releasing it on success."""
    error, retained = _cleanup(_UNSETTLED[token], token)
    return {"error": error, "retained_supervisor_token": retained}


def _read(stream):
    stream.seek(0)
    return stream.read()


def _read_captures(stdout, stderr):
    """Read each capture independently so one I/O failure cannot hide the other."""
    captures, errors = [], []
    for name, stream in (("stdout", stdout), ("stderr", stderr)):
        try:
            captures.append(_read(stream))
        except (OSError, ValueError) as exc:
            captures.append(b"")
            errors.append(f"{name} capture readback failed: {exc}")
    return captures[0], captures[1], errors


def run_process(argv, *, cwd, timeout):
    """Return decoded streams, or raise with partial bytes after bounded cleanup.

    The supplied wall-clock budget covers launch, leader wait and descendant
    settlement. After leader exit, natural settlement is capped at the shared
    cleanup grace. Failure cleanup has a separate bounded grace period. No solver
    discovery, license query or executable-name termination occurs here.
    """
    deadline = time.monotonic() + timeout
    with tempfile.TemporaryFile() as stdout, tempfile.TemporaryFile() as stderr:
        supervisor = supervisor_factory()
        return_code = None
        try:
            supervisor.start(argv, cwd, stdout, stderr)
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise subprocess.TimeoutExpired(argv, timeout)
            return_code = supervisor.wait(remaining)
            try:
                _wait_empty(supervisor, min(deadline, time.monotonic() + _CLEANUP_SECONDS))
            except subprocess.TimeoutExpired as exc:
                raise ProcessSettlementError(
                    f"owned descendants did not settle after leader exit (return code {return_code})",
                    b"", b"", return_code=return_code,
                ) from exc
            supervisor.close()
        except BaseException as exc:
            cleanup_error, token = _cleanup(supervisor)
            if not isinstance(exc, Exception):
                if cleanup_error:
                    exc.add_note(cleanup_error)
                    exc.retained_supervisor_token = token
                raise
            output, errors, read_errors = _read_captures(stdout, stderr)
            if cleanup_error or read_errors:
                cause = (f"MAPDL timeout after {timeout} seconds"
                         if isinstance(exc, subprocess.TimeoutExpired) else str(exc))
                details = [cause, *([cleanup_error] if cleanup_error else []), *read_errors]
                raise ProcessCleanupError(
                    "; ".join(details), output, errors,
                    return_code=return_code, retained_supervisor_token=token,
                ) from exc
            if isinstance(exc, ProcessSettlementError):
                exc.stdout, exc.stderr = output, errors
                raise
            if isinstance(exc, subprocess.TimeoutExpired):
                raise subprocess.TimeoutExpired(argv, timeout, output, errors) from exc
            raise
        return subprocess.CompletedProcess(
            argv, return_code,
            _read(stdout).decode("utf-8", errors="replace"),
            _read(stderr).decode("utf-8", errors="replace"),
        )
