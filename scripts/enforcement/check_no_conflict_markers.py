# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Scan staged bytes without command substitution or binary-to-text conversion.

The policy is documented in check-no-conflict-markers.sh. Binary data is not
exempt: literal marker lines still count, but NUL cannot manufacture anchors
or forensic sentinels. Python 3.11 subprocess documentation, Popen.stdout and
Frequently Used Arguments, specifies binary pipes without newline conversion:
https://docs.python.org/3.11/library/subprocess.html
"""

import io
import os
import re
import subprocess
import sys
from typing import BinaryIO

PATH_RESTRICTED_PREFIXES = (
    b"docs/plans/",
    b"docs/governance/",
    b"docs/standards/",
    b"scripts/review/results/",
    b"scripts/review/prompts/",
)
FILE_SENTINEL = b"<!-- CONFLICT_MARKER_FORENSIC_FILE_OK -->"
LINE_SENTINELS = (
    b"# CONFLICT_MARKER_FORENSIC_OK",
    b"// CONFLICT_MARKER_FORENSIC_OK",
    b"<!-- CONFLICT_MARKER_FORENSIC_OK -->",
)
# Literal anchors and ASCII whitespace preserve the shell hook's text policy.
# These are syntax from check-no-conflict-markers.sh, not engineering limits.
MARKER = re.compile(rb"^(<<<<<<<|>>>>>>>)(?:\s|$)")


def scan_blob(stream: BinaryIO, restricted: bool) -> list[tuple[int, bytes]]:
    """Retain candidate diagnostics, never the complete staged blob.

    Both anchors count even when individually exempt, as in the shell policy.
    Ordinary lines use bounded reads; memory otherwise scales with marker
    diagnostics. The default buffer is defined by Python 3.11 io documentation:
    https://docs.python.org/3.11/library/io.html#io.DEFAULT_BUFFER_SIZE
    """
    anchors = set()
    violations = []
    file_exempt = False
    lineno = 1
    at_line_start = True
    candidate = None
    sentinel_tail = b""
    while fragment := stream.readline(io.DEFAULT_BUFFER_SIZE):
        if restricted and not file_exempt:
            window = sentinel_tail + fragment
            file_exempt = FILE_SENTINEL in window
            sentinel_tail = window[-len(FILE_SENTINEL) :]
        if file_exempt:
            candidate = None
            violations.clear()
            # Drain stdout before waiting for Git, including after exemption.
            continue
        if at_line_start:
            match = MARKER.match(fragment)
            if match:
                anchors.add(match.group(1))
                candidate = bytearray()
        if candidate is not None:
            candidate.extend(fragment)
        at_line_start = fragment.endswith(b"\n")
        if at_line_start:
            if candidate is not None:
                # Remove only LF. A CR remains whitespace for anchors and a
                # suffix byte for sentinels, as in the original read -r policy.
                line = bytes(candidate).removesuffix(b"\n")
                if not line.endswith(LINE_SENTINELS):
                    violations.append((lineno, line))
            candidate = None
            sentinel_tail = b""
            lineno += 1
    # A final unterminated marker line is still subject to enforcement.
    if candidate is not None:
        line = bytes(candidate)
        if not line.endswith(LINE_SENTINELS):
            violations.append((lineno, line))
    if file_exempt or not {b"<<<<<<<", b">>>>>>>"}.issubset(anchors):
        return []
    return violations


def display_bytes(value: bytes) -> str:
    """Keep diagnostics readable without emitting NUL or invalid UTF-8."""
    return (
        value.decode("utf-8", errors="backslashreplace")
        .replace("\x00", r"\x00")
        .replace("\r", r"\r")
        .replace("\t", r"\t")
        .replace("\n", r"\n")
    )


def check_index() -> int:
    """Read each selected blob once and fail closed if Git cannot read it."""
    paths = subprocess.run(
        ["git", "diff", "--cached", "--name-only", "--diff-filter=ACMR", "-z"],
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        check=True,
    ).stdout.split(b"\x00")
    found = False
    for path in filter(None, paths):
        with subprocess.Popen(
            ["git", "show", ":" + os.fsdecode(path)],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        ) as process:
            violations = scan_blob(
                process.stdout, path.startswith(PATH_RESTRICTED_PREFIXES)
            )
            if process.wait():
                raise RuntimeError(f"Cannot read staged blob: {display_bytes(path)}")
        for lineno, line in violations:
            if not found:
                print(
                    "ERROR: Unresolved merge-conflict markers detected in staged content:",
                    file=sys.stderr,
                )
                found = True
            print(
                f"  {display_bytes(path)}:{lineno}:{display_bytes(line)}",
                file=sys.stderr,
            )
    if found:
        print(
            "\nRemove conflict markers, keep the intended content, and re-stage.\n"
            "For forensic documentation, append an existing per-line sentinel or\n"
            "use the whole-file sentinel within a documented restricted path.\n"
            "See scripts/enforcement/check-no-conflict-markers.sh for the policy.",
            file=sys.stderr,
        )
    return int(found)


def main() -> int:
    """Return the hook's failure status for violations or unreadable content."""
    try:
        return check_index()
    except (OSError, subprocess.CalledProcessError, RuntimeError):
        print(
            "ERROR: Unable to read staged content; conflict scan incomplete.",
            file=sys.stderr,
        )
        return 1


if __name__ == "__main__":
    sys.exit(main())
