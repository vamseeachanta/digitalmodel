# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Batch CAD inspection with per-file failures and digest-based resumption."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from typing import Any, Callable

from ._paths import inspection_outputs, json_outputs, validate_write_paths
from .drawings import dwg_to_dxf, dxf_to_json, dxf_to_svg, read_dwg_version, roundtrip_check
from .solids import step_checks

SUFFIXES = {".dwg", ".dxf", ".step", ".stp"}
SCHEMA_VERSION = 1


def _write_json(path: Path, data: Any) -> None:
    """Write a complete checkpoint, replacing only its own previous checkpoint."""
    _, temporary = json_outputs(path)
    temporary.write_text(
        json.dumps(data, indent=2, allow_nan=False) + "\n", encoding="utf-8"
    )
    temporary.replace(path)


def _attempt(steps: dict[str, Any], name: str, action: Callable[[], Any]) -> Any:
    """Record a step outcome without aborting independent steps or files."""
    try:
        value = action()
        steps[name] = {"status": "ok", "result": value}
        return value
    except Exception as exc:
        steps[name] = {"status": "failed", "reason": f"{type(exc).__name__}: {exc}"}
        return None


def _inspect(path: Path, folder: Path, code: str, digest: str) -> dict[str, Any]:
    """Run all applicable checks, explicitly logging unavailable prerequisites."""
    folder.mkdir(parents=True, exist_ok=True)
    row: dict[str, Any] = {
        "code": code, "sha256": digest, "format": path.suffix.lower(),
        "status": "failed", "converted": False, "steps": {}, "reason": "",
    }
    steps = row["steps"]
    suffix = path.suffix.lower()
    if suffix in {".dwg", ".dxf"}:
        drawing: Path | None = path
        if suffix == ".dwg":
            _attempt(steps, "version", lambda: read_dwg_version(path))

            def convert() -> str:
                return dwg_to_dxf(path, folder / "conversion").relative_to(folder).as_posix()

            converted = _attempt(steps, "conversion", convert)
            drawing = folder / converted if converted is not None else None
        if drawing is not None:

            def metadata() -> dict[str, Any]:
                data = dxf_to_json(drawing)
                _write_json(folder / "drawing.json", data)
                return data

            data = _attempt(steps, "metadata", metadata)
            row["converted"] = data is not None

            def render() -> str:
                return dxf_to_svg(drawing, folder / "drawing.svg").name

            _attempt(steps, "svg", render)
        else:
            for name in ("metadata", "svg"):
                steps[name] = {"status": "skipped", "reason": "DWG conversion failed"}
        if suffix == ".dwg":
            if row["converted"]:
                _attempt(steps, "roundtrip", lambda: roundtrip_check(path, folder / "roundtrip"))
            else:
                steps["roundtrip"] = {"status": "skipped", "reason": "readable DXF unavailable"}
    elif suffix in {".step", ".stp"}:
        solid = _attempt(steps, "solid", lambda: step_checks(path, folder))
        if solid is not None:
            steps.update(solid["steps"])
            row["converted"] = all(steps[name]["status"] == "ok" for name in ("gltf", "stl"))
    else:
        row.update(status="skipped", reason="unsupported file extension")
        return row
    failures = [f"{name}: {step['reason']}" for name, step in steps.items()
                if step["status"] == "failed"]
    row["reason"] = "; ".join(failures)
    row["status"] = "partial" if failures and row["converted"] else "failed" if failures else "ok"
    return row


def _digest(path: Path) -> str:
    """Hash input bytes in bounded chunks for resumable file checkpoints."""
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _validate_output_paths(files: list[Path], out: Path) -> None:
    """Reject aliases for every artifact, pending file and log before writes."""
    targets = list(json_outputs(out / "results.json"))
    for index in range(1, len(files) + 1):
        folder = out / f"F{index:02d}"
        targets.extend(json_outputs(folder / "checkpoint.json"))
        targets.extend(inspection_outputs(folder))
    validate_write_paths(files, targets)


def main(argv: list[str] | None = None) -> int:
    """Inspect one file or a folder; return 1 for file failures and 2 for usage errors."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args(argv)
    source, out = args.input.resolve(), args.out.resolve()
    if not source.exists():
        print("Input does not exist")
        return 2
    if source.is_dir() and (out == source or source in out.parents):
        print("Output directory must be outside the input folder")
        return 2
    files = (sorted(p for p in source.rglob("*") if p.is_file() and p.suffix.lower() in SUFFIXES)
             if source.is_dir() else [source])
    try:
        _validate_output_paths(files, out)
    except (OSError, ValueError) as exc:
        print(f"Invalid input/output paths: {exc}")
        return 2
    out.mkdir(parents=True, exist_ok=True)
    rows = []
    for index, path in enumerate(files, 1):
        code = f"F{index:02d}"
        folder = out / code
        checkpoint = folder / "checkpoint.json"
        try:
            digest = _digest(path)
            if checkpoint.exists():
                previous = json.loads(checkpoint.read_text(encoding="utf-8"))
                if (previous.get("schema_version") != SCHEMA_VERSION
                        or previous["file"]["sha256"] != digest
                        or previous["file"]["format"] != path.suffix.lower()):
                    raise ValueError("input or schema changed; use a fresh output directory")
                row = previous["file"]
            else:
                row = _inspect(path, folder, code, digest)
                _write_json(checkpoint, {"schema_version": SCHEMA_VERSION, "file": row})
        except Exception as exc:
            row = {"code": code, "status": "failed", "converted": False,
                   "steps": {}, "reason": f"{type(exc).__name__}: {exc}"}
        rows.append(row)
        _write_json(out / "results.json", {"schema_version": SCHEMA_VERSION, "files": rows})
    converted = sum(row["converted"] for row in rows)
    fraction = converted / len(rows) if rows else 0.0
    summary = {"tried": len(rows), "converted": converted, "converted_fraction": fraction,
               "failed": sum(row["status"] in {"failed", "partial"} for row in rows)}
    _write_json(out / "results.json", {"schema_version": SCHEMA_VERSION, "files": rows,
                                      "summary": summary})
    print(f"Converted {converted}/{len(rows)} ({fraction:.1%}); {summary['failed']} with failed checks")
    return 1 if summary["failed"] else 0
