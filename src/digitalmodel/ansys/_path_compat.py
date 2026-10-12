"""Path helpers that behave the same on Python 3.11 and 3.12+.

``pathlib.Path.is_junction`` exists only from Python 3.12. Several ANSYS host
modules call it directly; those modules are pinned by digest in retained
evidence and are not edited. New or unpinned code should call
:func:`is_junction` here instead, so it also imports and runs on 3.11.
"""
from __future__ import annotations

import os
import stat
from pathlib import Path

_WINDOWS = os.name == 'nt'
_FILE_ATTRIBUTE_REPARSE_POINT = 0x400
_IO_REPARSE_TAG_MOUNT_POINT = getattr(stat, 'IO_REPARSE_TAG_MOUNT_POINT', 0xA0000003)


def is_junction(path: str | os.PathLike[str]) -> bool:
    """Return True only for a Windows directory junction (mount-point reparse tag).

    Matches ``Path.is_junction`` semantics: missing or unreadable paths and
    symbolic links return False, and POSIX systems never have junctions.
    """
    candidate = Path(path)
    native = getattr(candidate, 'is_junction', None)
    if native is not None:
        return bool(native())
    if not _WINDOWS:
        return False
    try:
        info = os.lstat(candidate)
    except (OSError, ValueError):
        return False
    attributes = getattr(info, 'st_file_attributes', 0)
    return bool(attributes & _FILE_ATTRIBUTE_REPARSE_POINT) and (
        getattr(info, 'st_reparse_tag', None) == _IO_REPARSE_TAG_MOUNT_POINT)
