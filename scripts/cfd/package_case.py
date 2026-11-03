"""Package a quiescent OpenFOAM case while preserving initial-field templates.

Usage: python scripts/cfd/package_case.py CASE ARCHIVE.tar.gz

Only real numeric time directories directly beneath CASE are omitted. Nested
results (including processor directories) remain included. No solver is invoked.
"""

from __future__ import annotations

import argparse
from pathlib import Path, PurePosixPath
import re
import tarfile


# Repository policy: scripts/cfd/prune_case.sh, numeric-time directory selector.
# This is a full-name grammar, not tar's digit-prefix wildcard (issue 2057).
_TIME_NAME = re.compile(r"[0-9]+(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?")


def _archive_member(member: tarfile.TarInfo) -> tarfile.TarInfo | None:
    """Omit root time directories; retain named templates and nested data."""
    parts = PurePosixPath(member.name).parts
    if member.isdir() and len(parts) == 1 and _TIME_NAME.fullmatch(parts[0]):
        return None
    # Avoid embedding local account metadata in the portable archive.
    member.uname = member.gname = ""
    member.uid = member.gid = 0
    return member


def package_case(case: Path, destination: Path) -> None:
    """Create a new gzip tar outside CASE without altering its source tree.

    Source semantics: Python 3.11 Library Reference, tarfile.open ('x:gz'),
    TarFile.add (filter returning None omits a directory and its children),
    and TarFile.dereference (False stores symbolic links as links):
    https://docs.python.org/3.11/library/tarfile.html#tarfile.TarFile.add

    The case must remain unchanged during packaging. Links are preserved and
    may reference external locations; inspect links before extracting elsewhere.
    An I/O failure can leave an incomplete destination; use a fresh destination
    for a retry. No existing archive is replaced or removed.
    """
    case = Path(case).resolve()
    destination = Path(destination).resolve()
    if not case.is_dir():
        raise NotADirectoryError("case must be an existing directory")
    if destination.is_relative_to(case):
        raise ValueError("archive destination must be outside the case")
    with tarfile.open(destination, "x:gz", dereference=False) as archive:
        for entry in sorted(case.iterdir()):
            archive.add(entry, arcname=entry.name, filter=_archive_member)


def main() -> None:
    """Parse the case and new archive paths and package without a solver."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("case", type=Path)
    parser.add_argument("archive", type=Path)
    args = parser.parse_args()
    try:
        package_case(args.case, args.archive)
    except (OSError, ValueError, tarfile.TarError) as error:
        parser.exit(status=1, message=f"Packaging failed: {error}\n")


if __name__ == "__main__":
    main()
