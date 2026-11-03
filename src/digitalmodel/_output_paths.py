"""Preflight destination checks for multi-artifact writers."""

from itertools import combinations
import os
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


def relative_artifact_path(artifact: Path, document: Path) -> str:
    """Return a filesystem link relative to the document's actual directory.

    Resolve both endpoints so a symlinked manifest or report still links to its
    artifacts. Source: Python 3.11 pathlib Path.resolve (above) and os.path.relpath:
    https://docs.python.org/3.11/library/os.path.html#os.path.relpath
    URL consumers must percent-encode this filesystem path separately.
    Different filesystem drives cannot be represented by a relative link and
    raise ValueError before the calling writer emits its document.
    """
    relative = os.path.relpath(
        Path(artifact).resolve(), Path(document).resolve().parent,
    )
    return Path(relative).as_posix()
