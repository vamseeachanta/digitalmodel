# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Safe regeneration of licensed-run package folders.

A run package folder is written by a generator script and later receives
files the generator does not write, such as solver results saved beside the
model. Regenerating a package must never delete those files.

``record_case_folder`` writes a manifest of every file in the folder with its
SHA-256 digest after generation. ``prepare_case_folder`` clears a folder for
regeneration only when every file present is listed in that manifest with an
unchanged digest; it then removes exactly those files. Any other content
(a file the generator did not write, a generated file edited since, a
symbolic link, a folder without a manifest or with an unreadable one) raises
``FileExistsError`` and leaves the folder untouched.
"""

from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path

MANIFEST_NAME = ".generated-files.json"


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _walk(folder: Path) -> tuple[list[str], list[str], list[Path]]:
    """Return (files, links, directories) below ``folder`` without following links."""
    files: list[str] = []
    links: list[str] = []
    dirs: list[Path] = []
    for current, dirnames, filenames in os.walk(folder, followlinks=False):
        base = Path(current)
        for name in dirnames:
            path = base / name
            if path.is_symlink():
                links.append(path.relative_to(folder).as_posix())
            else:
                dirs.append(path)
        for name in filenames:
            path = base / name
            rel = path.relative_to(folder).as_posix()
            (links if path.is_symlink() else files).append(rel)
    return sorted(files), sorted(links), dirs


def record_case_folder(folder: Path) -> None:
    """Write the manifest of the generated files currently in ``folder``."""
    files, links, _ = _walk(folder)
    if links:
        raise FileExistsError(f"{folder}: generated folder holds symbolic links {links}")
    digests = {rel: _sha256(folder / rel) for rel in files if rel != MANIFEST_NAME}
    (folder / MANIFEST_NAME).write_text(json.dumps(digests, indent=1, sort_keys=True) + "\n",
                                        encoding="utf-8", newline="\n")


def prepare_case_folder(folder: Path) -> None:
    """Make ``folder`` an empty directory without deleting non-generated files.

    Raises ``FileExistsError`` when the folder holds anything other than
    unchanged files listed in its generator manifest.
    """
    if not folder.exists():
        folder.mkdir(parents=True)
        return
    if folder.is_symlink() or not folder.is_dir():
        raise FileExistsError(f"{folder}: exists and is not a plain directory; refusing to write")
    files, links, dirs = _walk(folder)
    if not files and not links:
        return
    if links:
        raise FileExistsError(f"{folder}: holds symbolic links {links}; refusing to regenerate")
    manifest = folder / MANIFEST_NAME
    if MANIFEST_NAME not in files:
        raise FileExistsError(
            f"{folder}: not empty and holds no generator manifest; move its contents "
            "or choose another output folder")
    try:
        recorded = json.loads(manifest.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        raise FileExistsError(f"{folder}: generator manifest is unreadable ({exc})") from exc
    if not isinstance(recorded, dict) or not all(
            isinstance(k, str) and isinstance(v, str) for k, v in recorded.items()):
        raise FileExistsError(f"{folder}: generator manifest has an unexpected form")
    unexpected = [rel for rel in files if rel != MANIFEST_NAME
                  and (rel not in recorded or _sha256(folder / rel) != recorded[rel])]
    if unexpected:
        raise FileExistsError(
            f"{folder}: holds files the generator did not write or that changed since "
            f"generation: {unexpected}; move them or choose another output folder")
    for rel in files:
        (folder / rel).unlink()
    for directory in sorted(dirs, key=lambda p: len(p.parts), reverse=True):
        directory.rmdir()
