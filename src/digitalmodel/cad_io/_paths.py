# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Enumerate writable CAD artifacts and reject filesystem aliases."""

from __future__ import annotations

from collections.abc import Iterable
from pathlib import Path


def json_outputs(path: Path) -> tuple[Path, Path]:
    """Return the final JSON record and its pending write destination."""
    return path, path.with_suffix(".json.pending")


def converter_outputs(target: Path, tool: str) -> tuple[Path, Path]:
    """Return the converter output and the log opened before conversion."""
    return target, target.parent / f"{tool}.log"


def roundtrip_outputs(folder: Path) -> list[Path]:
    """Reserve both encoder and fallback paths before starting a round trip."""
    return [
        *converter_outputs(folder / "first" / "drawing.dxf", "dwg2dxf"),
        *converter_outputs(folder / "encoded" / "drawing.dwg", "dxf2dwg"),
        *converter_outputs(folder / "last" / "drawing.dxf", "dwg2dxf"),
        folder / "resaved.dxf",
    ]


def inspection_outputs(folder: Path) -> list[Path]:
    """Reserve every drawing and solid artifact in a batch file's folder."""
    return [
        *json_outputs(folder / "drawing.json"), folder / "drawing.svg",
        *converter_outputs(folder / "conversion" / "drawing.dxf", "dwg2dxf"),
        *roundtrip_outputs(folder / "roundtrip"),
        folder / "part.glb", folder / "part.stl", folder / "part.inp",
    ]


def validate_write_paths(sources: Iterable[Path], targets: Iterable[Path]) -> None:
    """Reject source/output and output/output aliases before any writes.

    Resolved paths include symlinked parents; device/inode pairs include
    existing hardlinks. Paths must remain unchanged after this preflight.
    """
    def identities(path: Path) -> set[Path | tuple[int, int]]:
        keys: set[Path | tuple[int, int]] = {path.resolve()}
        if path.exists():
            stat = path.stat()
            keys.add((stat.st_dev, stat.st_ino))
        return keys

    seen: set[Path | tuple[int, int]] = set()
    for source in sources:
        seen.update(identities(source))
    for target in targets:
        keys = identities(target)
        if seen.intersection(keys):
            raise ValueError("Input and output artifact paths must not alias")
        seen.update(keys)
