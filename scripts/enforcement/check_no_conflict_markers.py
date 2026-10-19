# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inspect staged bytes without passing blob content through a shell variable.

Policy source: check-no-conflict-markers.sh, inherited anchor/co-occurrence and
forensic exemption rules. Binary blobs are inspected using the same ASCII
syntax as text; NUL bytes are preserved and never create an anchor or sentinel.
Only paths and line numbers are reported, avoiding raw binary diagnostics.
"""

from __future__ import annotations

import os
import subprocess
import sys
from collections.abc import Iterator
from typing import BinaryIO

PATH_PREFIXES = (
    b"docs/plans/",
    b"docs/governance/",
    b"docs/standards/",
    b"scripts/review/results/",
    b"scripts/review/prompts/",
)
EOL_SENTINELS = (
    b"# CONFLICT_MARKER_FORENSIC_OK",
    b"// CONFLICT_MARKER_FORENSIC_OK",
    b"<!-- CONFLICT_MARKER_FORENSIC_OK -->",
)
FILE_SENTINEL = b"<!-- CONFLICT_MARKER_FORENSIC_FILE_OK -->"
OPEN = b"<<<<<<<"
CLOSE = b">>>>>>>"
# RFC 3629 (2003), section 3: UTF-8 characters occupy at most four octets.
# Keep enough bytes to classify the complete character following an anchor.
PREFIX_BYTES = len(OPEN) + 4
TAIL_BYTES = max(map(len, (*EOL_SENTINELS, FILE_SENTINEL)))
# Implementation buffer choice, not a content-size or acceptance limit.
CHUNK_BYTES = 64 * 1024


def line_summaries(stream: BinaryIO) -> Iterator[tuple[bytes, bytes, bool]]:
    """Retain bounded prefix/suffix state even for a newline-free binary blob."""
    prefix = tail = b""
    file_sentinel = False
    while chunk := stream.read(CHUNK_BYTES):
        parts = chunk.split(b"\n")
        for index, part in enumerate(parts):
            prefix = (prefix + part[:PREFIX_BYTES])[:PREFIX_BYTES]
            window = tail + part
            file_sentinel |= FILE_SENTINEL in window
            tail = window[-TAIL_BYTES:]
            if index < len(parts) - 1:
                yield prefix, tail, file_sentinel
                prefix = tail = b""
                file_sentinel = False
    if prefix or tail:
        yield prefix, tail, file_sentinel


def scan_blob(stream: BinaryIO, path: bytes) -> list[int]:
    """Return unexempt line numbers only when both anchors occur in this blob."""
    opening = closing = file_exempt = False
    findings = []
    for lineno, (prefix, tail, file_sentinel) in enumerate(line_summaries(stream), 1):
        file_exempt |= file_sentinel and path.startswith(PATH_PREFIXES)
        anchor = prefix[: len(OPEN)]
        if anchor not in (OPEN, CLOSE):
            continue
        suffix = prefix[len(OPEN) :].decode("utf-8", errors="surrogateescape")
        # Python 3.11 Library Reference, str.isspace(): Unicode whitespace.
        # This includes non-ASCII spaces recognized by the old UTF-8 locale
        # regex, with consistent conservative detection across host locales.
        if suffix and not suffix[0].isspace():
            continue
        opening |= anchor == OPEN
        closing |= anchor == CLOSE
        if not tail.endswith(EOL_SENTINELS):
            findings.append(lineno)
    if file_exempt or not (opening and closing):
        return []
    return findings


def main() -> int:
    try:
        staged = subprocess.run(
            ["git", "diff", "--cached", "--name-only", "--diff-filter=ACMR", "-z"],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            check=True,
        ).stdout
        violations = []
        for path in staged.split(b"\0"):
            if not path:
                continue
            # One read per staged blob; the working tree is never consulted.
            with subprocess.Popen(
                ["git", "show", b":" + path],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
            ) as process:
                findings = scan_blob(process.stdout, path)
                if process.wait():
                    raise OSError("staged blob unavailable")
            violations.extend((path, lineno) for lineno in findings)
    except (OSError, subprocess.CalledProcessError):
        print("ERROR: Could not inspect staged content.", file=sys.stderr)
        return 2

    if not violations:
        return 0
    print(
        "ERROR: Unresolved merge-conflict markers detected in staged content:",
        file=sys.stderr,
    )
    for path, lineno in violations:
        # Escape control characters in filenames and never echo blob bytes.
        display = ascii(os.fsdecode(path))[1:-1]
        print(f"  {display}:{lineno}: conflict marker", file=sys.stderr)
    print(
        "\nResolve the markers, re-stage, and retry the commit.\n"
        "For forensic documentation, append a recognized per-line sentinel,\n"
        "or use the file sentinel in a permitted documentation directory.\n"
        "See check-no-conflict-markers.sh for the exemption syntax.",
        file=sys.stderr,
    )
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
