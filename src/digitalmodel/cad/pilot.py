# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Headless, local DXF batch exports with a check record for every input."""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
import time
from dataclasses import asdict
from importlib.metadata import PackageNotFoundError, version
from pathlib import Path
from typing import Any, Sequence

from .models import CadGeometry, LengthUnit
from .reader import read_cad

_CHECKS = ("input", "geometry_and_units", "json", "svg", "artifacts")
_SVG = "{http://www.w3.org/2000/svg}"


def _write_svg(geometry: CadGeometry, destination: Path) -> None:
    """Render an XY preview of accepted metre geometry, without source styling."""
    import ezdxf
    from ezdxf.addons.drawing import Frontend, RenderContext, layout, svg
    from ezdxf.addons.drawing.config import BackgroundPolicy, ColorPolicy, Configuration

    # Rebuild only accepted primitives: no second source read, block expansion,
    # hidden layers or unsupported entities can change the preview's geometry.
    doc = ezdxf.new()
    doc.units = 6  # Metres, as established by the existing CAD reader.
    model = doc.modelspace()
    for entity in geometry.entities:
        points = [geometry.points[index] for index in entity.indices]
        if entity.kind == "point":
            model.add_point(points[0])
        elif entity.kind == "line":
            model.add_line(*points)
        elif entity.kind == "polyline":
            model.add_polyline3d(points)
        else:
            model.add_3dface(points)
    backend = svg.SVGBackend()
    config = Configuration(
        background_policy=BackgroundPolicy.WHITE,
        color_policy=ColorPolicy.BLACK,
        pdmode=3,
        pdsize=0.01,  # Display symbol only; JSON retains point coordinates.
    )
    Frontend(RenderContext(doc), backend, config=config).draw_layout(model)
    destination.write_text(backend.get_string(layout.Page(200, 200)), encoding="utf-8")


def _validate_artifacts(directory: Path, payload: str) -> None:
    """Check JSON round-trip and a parsed SVG containing graphical elements."""
    from defusedxml import ElementTree as ET

    saved = json.loads((directory / "geometry.json").read_text(encoding="utf-8"))
    if saved != json.loads(payload):
        raise ValueError("JSON round-trip failed")
    root = ET.parse(directory / "preview.svg").getroot()
    graphics = {
        f"{_SVG}{tag}"
        for tag in ("path", "line", "polyline", "polygon", "circle", "ellipse")
    }
    if root.tag != f"{_SVG}svg" or not any(
        node.tag in graphics for node in root.iter()
    ):
        raise ValueError("SVG contains no graphical elements")


def _convert(
    source: Path, index: int, output: Path, units: LengthUnit | None
) -> dict[str, Any]:
    started = time.perf_counter()
    row: dict[str, Any] = {
        "index": index,
        "input_sha256": None,
        "status": "fail",
        "checks": dict.fromkeys(_CHECKS, "not_run"),
        "outputs": {},
    }
    stage = "input"
    try:
        if source.suffix.casefold() != ".dxf" or not source.is_file():
            raise ValueError("A regular DXF input is required")
        content = source.read_bytes()
        row["input_sha256"] = hashlib.sha256(content).hexdigest()
        directory = output / f"{index:04d}"
        directory.mkdir()
        snapshot = directory / "source.dxf"
        snapshot.write_bytes(content)
        row["checks"][stage] = "pass"
        stage = "geometry_and_units"
        geometry = read_cad(snapshot, units=units)
        row["checks"][stage] = "pass"
        stage = "json"
        payload = json.dumps(asdict(geometry), indent=2, allow_nan=False) + "\n"
        (directory / "geometry.json").write_text(payload, encoding="utf-8")
        row["checks"][stage] = "pass"
        stage = "svg"
        _write_svg(geometry, directory / "preview.svg")
        row["checks"][stage] = "pass"
        stage = "artifacts"
        _validate_artifacts(directory, payload)
        row["checks"][stage] = "pass"
        row["outputs"] = {
            "json": f"{index:04d}/geometry.json",
            "svg": f"{index:04d}/preview.svg",
        }
        row["status"] = "pass"
    except Exception as error:
        # Each input must leave a result and later inputs must still be checked.
        # Raw library exceptions can expose source content and local paths.
        row["checks"][stage] = "fail"
        row["error"] = (
            "dependency_unavailable: install digitalmodel[drawings]"
            if isinstance(error, ImportError)
            else f"{stage}_failed"
        )
    row["elapsed_seconds"] = round(time.perf_counter() - started, 1)
    return row


def run_dxf_pilot(
    paths: Sequence[str | Path],
    output: str | Path,
    *,
    units: LengthUnit | None = None,
) -> dict[str, Any]:
    """Export a nonempty DXF batch into a new directory and retain report.json.

    Only inputs passing all five checks count toward the issue's 90% target.
    Partial files from failures are retained but are absent from ``outputs``.
    The report uses indices and digests rather than source names or locations.
    An existing destination is refused; no input or earlier output is replaced.
    """
    started = time.perf_counter()
    sources = [Path(path) for path in paths]
    if not sources:
        raise ValueError("Supply at least one DXF input")
    destination = Path(output)
    destination.mkdir(parents=True, exist_ok=False)
    rows = [
        _convert(path, index, destination, units)
        for index, path in enumerate(sources, 1)
    ]
    succeeded = sum(row["status"] == "pass" for row in rows)
    try:
        ezdxf_version = version("ezdxf")
    except PackageNotFoundError:
        ezdxf_version = "unavailable"
    report = {
        "schema_version": 1,
        "toolchain": {"python": sys.version.split()[0], "ezdxf": ezdxf_version},
        "scope": "DXF strict primitives; normalized JSON; XY SVG preview",
        "total": len(rows),
        "succeeded": succeeded,
        "success_percent": round(100 * succeeded / len(rows), 1),
        "required_percent": 90.0,
        "accepted": succeeded * 10 >= len(rows) * 9,
        "elapsed_seconds": round(time.perf_counter() - started, 1),
        "files": rows,
    }
    (destination / "report.json").write_text(
        json.dumps(report, indent=2, allow_nan=False) + "\n", encoding="utf-8"
    )
    return report


def main(argv: Sequence[str] | None = None) -> int:
    """Return 0 at target, 1 below target and 2 for setup errors."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("inputs", nargs="+", type=Path)
    parser.add_argument("--output", required=True, type=Path, help="new output directory")
    parser.add_argument("--units", choices=("m", "mm", "cm", "in", "ft"))
    args = parser.parse_args(argv)
    try:
        report = run_dxf_pilot(args.inputs, args.output, units=args.units)
    except (OSError, ValueError):
        print(
            "Cannot create pilot output; supply inputs and a new writable directory.",
            file=sys.stderr,
        )
        return 2
    print(
        f"DXF exports: {report['succeeded']}/{report['total']}; "
        f"{report['success_percent']:.1f}%"
    )
    return 0 if report["accepted"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
