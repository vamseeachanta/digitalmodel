# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Inspect or send TERM to one recorded process after case/executable checks.

No process discovery or solver imports occur here. See
docs/domains/openfoam/process_supervision_runbook.md for sources and limitations.
"""

from __future__ import annotations

import argparse
import os
import signal
import sys
from pathlib import Path


class ValidationError(Exception):
    """The supplied process cannot be validated; no termination is attempted."""


def _reject_ancestor(pid: int) -> None:
    """Exclude the helper and every parent in its control chain."""
    current = os.getpid()
    seen = set()
    # PPid is the parent PID: Linux man-pages 6.19 (2026),
    # proc_pid_status(5), DESCRIPTION. Zero ends the visible ancestry.
    while current:
        if current == pid:
            raise ValidationError("candidate is self or a controlling ancestor")
        if current in seen:
            raise ValidationError("process ancestry is inconsistent")
        seen.add(current)
        for line in Path(f"/proc/{current}/status").read_text().splitlines():
            if line.startswith("PPid:"):
                current = int(line.removeprefix("PPid:").strip())
                break
        else:
            raise ValidationError("process ancestry is unavailable")


def check_process(
    pid: int, case_dir: Path, executable: Path, *, terminate: bool = False
) -> str:
    """Validate exact directory/executable identity and optionally send TERM.

    The pidfd is opened before reading metadata and used for every signal.
    It prevents a recycled numeric PID from receiving the signal. Metadata
    remains a snapshot: this is not a process ownership or containment service.
    """
    # Positive PIDs name individual processes; zero and negative values have
    # group meanings in kill(2), DESCRIPTION, Linux man-pages 6.19 (2026).
    if pid <= 0:
        raise ValidationError("candidate must be a positive PID")
    if not hasattr(os, "pidfd_open") or not hasattr(signal, "pidfd_send_signal"):
        raise ValidationError("pidfd support is unavailable")
    try:
        _reject_ancestor(pid)
        # Python 3 Library Reference (2026), os.pidfd_open: default flags.
        descriptor = os.pidfd_open(pid)
        try:
            proc = Path("/proc") / str(pid)
            # Linux man-pages 6.19 (2026), proc_pid_cwd(5) and proc_pid_exe(5),
            # DESCRIPTION: these links identify cwd and the executed program.
            if not os.path.samefile(proc / "cwd", case_dir):
                raise ValidationError("working directory does not match the case")
            if not os.path.samefile(proc / "exe", executable):
                raise ValidationError("executable does not match the expected program")
            # Signal zero only checks existence/permission: kill(2), DESCRIPTION;
            # pidfd_send_signal(2), NOTES (Linux man-pages 6.19, 2026) explains
            # why a pinned descriptor cannot signal a recycled PID.
            signal.pidfd_send_signal(descriptor, 0)
            if terminate:
                signal.pidfd_send_signal(descriptor, signal.SIGTERM)
                return "TERM_SENT"
            return "VALIDATED"
        finally:
            os.close(descriptor)
    except (OSError, ValueError, OverflowError) as exc:
        # Avoid leaking runtime paths from operating-system exception messages.
        raise ValidationError("process metadata or signalling is unavailable") from exc


def main(argv: list[str] | None = None) -> int:
    """Provide a read-only default and explicit termination opt-in."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("pid", type=int, help="PID recorded by the launcher")
    parser.add_argument("case_dir", type=Path, help="expected exact working directory")
    parser.add_argument("executable", type=Path, help="expected executable file")
    parser.add_argument(
        "--terminate", action="store_true", help="send TERM after checks"
    )
    args = parser.parse_args(argv)
    try:
        result = check_process(
            args.pid, args.case_dir, args.executable, terminate=args.terminate
        )
    except ValidationError as exc:
        print(f"NOT_EVALUATED: {exc}", file=sys.stderr)
        return 1
    print(result)
    return 0


if __name__ == "__main__":
    sys.exit(main())
