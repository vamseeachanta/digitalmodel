# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check that a source change only prepends the repository licence header."""

import argparse
from pathlib import Path


def header_only(before: Path, after: Path, license_file: Path) -> bool:
    """Compare every source byte after the standard commented header."""
    header = "".join("# " + line + "\n" for line in
                     license_file.read_text().splitlines()).encode() + b"\n"
    return after.read_bytes() == header + before.read_bytes()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("before", type=Path)
    parser.add_argument("after", type=Path)
    parser.add_argument("--license-file", type=Path, default=Path(".license-header.txt"))
    args = parser.parse_args()
    if not header_only(args.before, args.after, args.license_file):
        parser.exit(1, "Source differs beyond the standard licence header\n")
    print("Only the standard licence header was added")
