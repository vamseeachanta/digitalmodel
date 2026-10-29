# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Draw proportional SCR/SLWR elevations from configuration-screen geometry."""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path
import re
from typing import Any

from .plotting import render_artifact, row, write_json
from .results_exchange import portable_path


def build_configuration(geometry: dict[str, Any], output: Path, *,
                        reference_pair: str, source_file: str) -> dict[str, Any]:
    """Render the zero-offset screen geometry with equal horizontal/depth scales.

    Args:
        geometry: Inputs accepted by riser_configuration_screen.static, in SI.
        output: Destination for SCH-01.svg, CSV and its separate manifest.
        reference_pair: Diameter/wall identifier shared by both configurations.
        source_file: Portable label for the input geometry, never an absolute path.

    Returns:
        Schematic manifest with every point and its material arc coordinate.

    Raises:
        ValueError: If geometry or its reference is invalid.
    """
    from digitalmodel.riser_configuration_screen.static import lazy_wave_offsets, scr_offsets

    if not re.fullmatch(r"R(?:08|10|12)-W[12]", reference_pair):
        raise ValueError("invalid reference pair")
    portable_path(source_file)
    if not all(isinstance(value, (int, float)) and not isinstance(value, bool)
               and math.isfinite(value) for value in geometry.values()):
        raise ValueError("geometry parameters must be finite numbers")
    # Zero offset is the nominal configuration, by definition of the screen.
    profiles = {"SCR": scr_offsets(geometry, [0.0])[0],
                "SLWR": lazy_wave_offsets(geometry, [0.0])[0]}
    rows, annotations = [], []
    depth = geometry["water_depth_m"]
    for kind, profile in profiles.items():
        case = reference_pair + "-" + kind
        points = profile["shape"]
        for arc, x, elevation in points:
            datum = row("SCH-01", case, "profile", x, depth-elevation, "m", "m", source_file)
            datum["arc_length_m"] = arc
            rows.append(datum)
        annotations.append({"x": points[0][1], "y": depth-points[0][2], "label": kind+" Hang-off"})
        annotations.append({"x": points[-1][1], "y": depth-points[-1][2], "label": kind+" Touchdown"})
        if kind == "SLWR":
            # The screen samples exact segment boundaries, including both bends.
            for previous, point, following in zip(points, points[1:], points[2:]):
                if previous[2] > point[2] < following[2]:
                    annotations.append({"x": point[1], "y": depth-point[2], "label": "Sag bend"})
                if previous[2] < point[2] > following[2]:
                    annotations.append({"x": point[1], "y": depth-point[2], "label": "Hog bend"})
            start = profile["buoyed_start_m"]
            end = start + profile["buoyed_length_m"]
            middle = min(points, key=lambda p: abs(p[0]-(start+end)/2))
            annotations.append({"x": middle[1], "y": depth-middle[2], "label": "Buoyancy section"})
            for datum in rows:
                if datum["case_id"] == case:
                    datum["buoyed"] = start <= datum["arc_length_m"] <= end
    for datum in rows:
        datum.setdefault("buoyed", False)
    xmax = max(r["x"] for r in rows)
    for series, y in (("Water line", 0.0), ("Seabed", depth)):
        for x in (0.0, xmax):
            datum = row("SCH-01", "", series, x, y, "m", "m", source_file)
            datum.update(arc_length_m="", buoyed=False)
            rows.append(datum)
        annotations.append({"x": xmax, "y": y, "label": series})
    artifact = render_artifact(Path(output), "SCH-01", rows,
        title="SCR and SLWR configuration elevation", x_title="Horizontal distance from hang-off (m)",
        y_title="Depth below water line (m)", equal_scale=True, annotations=annotations,
        note=f"Static configuration screen; equal horizontal and vertical scale; input: {source_file}.")
    manifest = {"artifacts": [artifact], "reference_pair": reference_pair,
                "geometry": geometry, "source_file": source_file,
                "method": "riser_configuration_screen.static; nominal zero offset; depth = water depth - elevation",
                "limitations": "Inextensible static screen; frictionless grounded tail; no bending, pressure or dynamics."}
    write_json(Path(output) / "SCH-01.manifest.json", manifest)
    return manifest


def main() -> None:
    """Render a supplied JSON geometry file without a native solver."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("geometry", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--reference-pair", required=True)
    args = parser.parse_args()
    geometry = json.loads(args.geometry.read_text(encoding="utf-8"))
    manifest = build_configuration(geometry, args.output, reference_pair=args.reference_pair,
                                   source_file=args.geometry.name)
    manifest["input_sha256"] = hashlib.sha256(args.geometry.read_bytes()).hexdigest()
    write_json(args.output / "SCH-01.manifest.json", manifest)


if __name__ == "__main__":
    main()
