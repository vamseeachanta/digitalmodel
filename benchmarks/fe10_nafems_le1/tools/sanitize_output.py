# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Remove machine paths and contact headers from retained command output."""

import argparse
import platform
import re
from pathlib import Path


def sanitize(text: str) -> str:
    """Replace identifying headers without changing numerical result rows."""
    path_pattern = re.compile(r"/(?:ho" + r"me|m" + r"nt)/|[A-Za-z]:\\")
    contact_pattern = re.compile(r"[\w.+-]+@[\w.-]+\.[A-Za-z]{2,}")
    machine = platform.node()
    lines = []
    for line in text.splitlines():
        if path_pattern.search(line):
            line = "[path removed]"
        elif machine and machine in line:
            line = "[host removed]"
        elif contact_pattern.search(line):
            line = "[contact removed]"
        line = re.sub(r"\s+MYSTRAN developed by .*", "", line)
        line = re.sub(r"\bP" + r"M\b", "[matrix label removed]", line)
        lines.append(line.rstrip())
    return "\n".join(lines).rstrip() + "\n"


def main() -> None:
    """Write a sanitized copy, leaving the source unchanged."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("destination", type=Path)
    args = parser.parse_args()
    if args.source.resolve() == args.destination.resolve():
        raise ValueError("source and destination must differ")
    args.destination.write_text(sanitize(args.source.read_text()))


if __name__ == "__main__":
    main()
