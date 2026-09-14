"""Opt-in binary MAPDL wrapper; native qualification belongs to the operator.

Existing ANSYSRunner behavior is unchanged. This API does not grant approval or
license ownership. Caller pins executable/deck hashes and retains the seat until
settlement is established. All injected process APIs in B1 tests are synthetic.
"""
import hashlib
import math
from pathlib import Path
import re
import subprocess
import time
import uuid

from digitalmodel.ansys.runner import ANSYSRunner
from digitalmodel.ansys.cylinder_process import supervisor_factory as _factory

_UNSETTLED = {}


def _paths(case, directory, timeout, executable):
    if isinstance(timeout, bool) or not isinstance(timeout, (int, float)):
        raise ValueError("finite positive timeout required")
    if not math.isfinite(timeout) or not 0 < timeout <= 300:
        raise ValueError("timeout must be within the approved 300 seconds")
    name = case.get("deck_basename", case.get("deck"))
    if not isinstance(name, str) or not re.fullmatch(r"[A-Za-z0-9_-]+\.(inp|ans)", name):
        raise ValueError("deck must be a direct .inp/.ans basename")
    directory, executable = Path(directory).resolve(), Path(executable).resolve()
    deck = directory / name
    if not directory.is_dir() or not executable.is_file() or not deck.is_file():
        raise ValueError("existing executable, attempt directory and deck required")
    if deck.is_symlink() or deck.resolve().parent != directory:
        raise ValueError("deck must reside in the attempt directory")
    output = directory / (deck.stem + ".out")
    if output.exists():
        raise FileExistsError(output)
    for name in ("stdout.bin", "stderr.bin"):
        if (directory / name).exists():
            raise FileExistsError(directory / name)
    return directory, executable, deck, output


def _poll_settlement(supervisor, remaining, deadline):
    while remaining:
        allowance = deadline - time.monotonic()
        if allowance <= 0:
            break
        time.sleep(min(0.01, allowance))
        remaining = supervisor.active()
    return remaining


def _settle(supervisor, result):
    try:
        deadline = time.monotonic() + 5
        remaining = supervisor.active()
        clean_exit = (result.get("return_code") == 0 and not result["error"]
                      and not result["timed_out"])
        if clean_exit:
            remaining = _poll_settlement(supervisor, remaining, deadline)
        if not clean_exit or remaining:
            result["settlement_required"] = True
            supervisor.terminate()
            remaining = supervisor.active()
            remaining = _poll_settlement(supervisor, remaining, deadline)
        result["owned_processes_remaining"] = remaining
        if remaining == 0:
            supervisor.close()
            return
        result["error"].append("owned process settlement timed out")
    except Exception as exc:
        result["settlement_required"] = True
        result["error"].append(f"settlement: {type(exc).__name__}: {exc}")
        result["owned_processes_remaining"] = None
        try:
            supervisor.terminate()
        except Exception as cleanup_error:
            result["error"].append(f"termination: {type(cleanup_error).__name__}: {cleanup_error}")
    token = uuid.uuid4().hex
    _UNSETTLED[token] = supervisor  # retain job handle; owner must retain seat
    result["retained_supervisor_token"] = token


def _invoke(supervisor, argv, directory, timeout, stdout, stderr, result):
    try:
        supervisor.start(argv, directory, stdout, stderr)
        result["containment_verified"] = bool(supervisor.containment_verified)
        result["return_code"] = supervisor.wait(timeout)
    except subprocess.TimeoutExpired:
        result["timed_out"] = True
    except Exception as exc:
        result["error"].append(f"launch/wait: {type(exc).__name__}: {exc}")
    finally:
        _settle(supervisor, result)


def _read_streams(directory, result):
    for name in ("stdout", "stderr"):
        result[name] = result[name + "_sha256"] = None
        try:
            raw = (directory / (name + ".bin")).read_bytes()
            result[name] = raw
            result[name + "_sha256"] = hashlib.sha256(raw).hexdigest()
        except OSError as exc:
            result["error"].append(f"{name} readback: {type(exc).__name__}: {exc}")
    result["streams_finalized"] = result["owned_processes_remaining"] == 0


def launch_case(case, directory, timeout, executable, *, supervisor_factory=None):
    """Run one already-approved case; return raw bytes and explicit uncertainty."""
    directory, executable, deck, output = _paths(case, directory, timeout, executable)
    result = {"return_code": None, "timed_out": False, "owned_processes_remaining": None,
              "containment_verified": False, "settlement_required": False,
              "evidence_complete": False, "error": [], "retained_supervisor_token": None}
    start = time.monotonic()
    argv = [str(executable), "-b", "-i", str(deck), "-o", str(output), "-np", "1", "-smp"]
    with (directory / "stdout.bin").open("xb") as stdout:
        with (directory / "stderr.bin").open("xb") as stderr:
            try:
                supervisor = (supervisor_factory or _factory)()
            except Exception as exc:
                result["error"].append(f"supervisor creation: {type(exc).__name__}: {exc}")
                result["owned_processes_remaining"] = 0  # no process was created
            else:
                _invoke(supervisor, argv, directory, timeout, stdout, stderr, result)
    result["duration_seconds"] = time.monotonic() - start
    _read_streams(directory, result)
    result["evidence_complete"] = bool(result["containment_verified"] and
        result["return_code"] == 0 and result["owned_processes_remaining"] == 0 and not result["error"] and
        not result["timed_out"] and not result["settlement_required"])
    return result


def settle_retained(token):
    """Retry settlement of an exact retained supervisor; never search by PID/name."""
    supervisor = _UNSETTLED[token]
    result = {"error": [], "timed_out": True, "settlement_required": True,
              "owned_processes_remaining": None, "retained_supervisor_token": None}
    _settle(supervisor, result)
    del _UNSETTLED[token]
    return result


class BinaryANSYSRunner(ANSYSRunner):
    """Explicit opt-in method; inherited default run/execute remain unchanged."""

    def launch_case(self, case, directory, timeout, executable):
        return launch_case(case, directory, timeout, executable)
