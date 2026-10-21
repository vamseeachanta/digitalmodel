"""Preflight destination checks for multi-artifact writers."""

from itertools import combinations
from pathlib import Path


def validate_output_paths(*paths: Path) -> None:
    """Reject destinations that identify the same file before any write.

    Python 3.11 pathlib documentation, ``Path.resolve(strict=False)``:
    resolve symlinks and ``..`` even when the final file does not exist.
    ``Path.samefile`` additionally detects existing hard-link aliases.
    https://docs.python.org/3.11/library/pathlib.html#pathlib.Path.resolve
    https://docs.python.org/3.11/library/pathlib.html#pathlib.Path.samefile

    This preflight does not lock paths against concurrent filesystem changes.
    Case-only aliases are rejected conservatively on every filesystem.
    It preserves the caller's policy for replacing distinct existing files.
    """
    resolved = [Path(path).resolve() for path in paths]
    if len({str(path).casefold() for path in resolved}) != len(resolved):
        raise ValueError("Output destinations must be distinct")
    existing = [path for path in resolved if path.exists()]
    for first, second in combinations(existing, 2):
        if first.samefile(second):
            raise ValueError("Output destinations must be distinct")
