# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Read every case strength export and build figures, tables and value tokens.

This module is a post-processor only. It neither runs nor imports a licensed
solver. All response values and engineering bases must be present in exports.
"""

from __future__ import annotations

import argparse
import hashlib
import math
from pathlib import Path
import re
from typing import Any

from .plotting import render_artifact, row, write_json
from .results_exchange import load_result

# Definition of a normalised stress / supplied allowable, not a code limit.
NORMALISED_LIMIT = 1.0


def build_strength(results: Path, output: Path, *, reference_pair: str,
                   touchdown_window_m: float) -> dict[str, Any]:
    """Validate all exports before writing the strength artifact collection.

    Args:
        results: Directory containing case_id/strength.json exports.
        output: Destination for SVG, CSV, manifest.json and tokens.json.
        reference_pair: Explicit diameter/wall pair, for example R08-W1.
        touchdown_window_m: Half-width about each exported touchdown arc.

    Returns:
        Manifest with all numeric marks and source-byte digests.

    Raises:
        ValueError: If any export or requested reference is invalid.
    """
    results, output = Path(results), Path(output)
    if (not math.isfinite(touchdown_window_m) or touchdown_window_m <= 0
            or not re.fullmatch(r"R(?:08|10|12)-W[12]", reference_pair)):
        raise ValueError("explicit reference pair and positive touchdown window required")
    paths = sorted(results.rglob("strength.json"))
    if not paths:
        raise ValueError("no strength.json exports found")
    records = []
    sources = []
    seen = set()
    for path in paths:
        data = load_result(path, "strength")
        case = data["case_id"]
        if path.parent.name != case or case in seen:
            raise ValueError("case directory mismatch or duplicate case export")
        seen.add(case)
        source = "results/" + path.relative_to(results).as_posix()
        records.append((data, source))
        sources.append({"file": source, "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
                        "software": data["software"], "load_case": data["load_case"],
                        "method": data["method"], "allowable": data["allowable"], "run": data["run"]})
    for kind in ("SCR", "SLWR"):
        if reference_pair + "-" + kind not in seen:
            raise ValueError("reference pair requires both SCR and SLWR exports")
    records.sort(key=lambda pair: pair[0]["case_id"])
    artifacts = []
    synthetic = all("synthetic" in data["software"]["name"].casefold() for data, _ in records)
    note = ("SYNTHETIC fixture results" if synthetic else "Exported results")
    note += "; method and allowable recorded in manifest."
    for token, field, title, unit in [
        ("FIG-STR-01", "effective_tension_kN", "Effective tension along the riser", "kN"),
        ("FIG-STR-02", "bending_moment_kNm", "Bending moment along the riser", "kN.m"),
        ("FIG-STR-04", "von_mises_utilisation", "Utilisation near touchdown", "-")]:
        rows = []
        for data, source in records:
            tdp = data["touchdown"]["arc_length_m"]
            rows.extend(row(token, data["case_id"], "profile", s, y, "m", unit, source)
                        for s, y in zip(data["arc_length_m"], data[field])
                        if token != "FIG-STR-04" or abs(s-tdp) <= touchdown_window_m)
            if token == "FIG-STR-04":
                rows.append(row(token, data["case_id"], "touchdown", tdp,
                                data["touchdown"]["utilisation"], "m", unit, source))
        artifacts.append(render_artifact(output, token, rows, title=title,
            x_title="Arc length from hang-off (m)", y_title=f"{title.split(' along')[0]} ({unit})", note=note))
    for token, key, series, title, unit in [
        ("FIG-STR-03", "max_utilisation", ("value",), "Maximum strength utilisation", "-"),
        ("FIG-STR-05", "top_angle_deg", ("static", "dynamic_min", "dynamic_max"), "Top angle from vertical", "deg"),
        ("FIG-STR-06", "hang_off_tension_kN", ("static", "dynamic_max"), "Hang-off effective tension", "kN")]:
        rows = [row(token, data["case_id"], name, index, data[key][name], "case index", unit, source)
                for index, (data, source) in enumerate(records) for name in series]
        if token == "FIG-STR-03":
            rows.extend(row(token, "", "limit", index, NORMALISED_LIMIT, "case index", "-", "definition: stress / allowable")
                        for index in (0, len(records)-1))
        artifacts.append(render_artifact(output, token, rows, title=title, categorical=True,
            x_title="Case", y_title=f"{title} ({unit})", note=note))
    def token_value(token: str, value: Any, source: str | list[str]) -> dict[str, Any]:
        return {"id": token, "value": value, "unit": "-", "source_file": source}
    governing, source = max(records, key=lambda pair: pair[0]["max_utilisation"]["value"])
    tokens = [token_value("V-STR-GOV-CASE", governing["case_id"], source),
              token_value("V-STR-UMAX-GOV", governing["max_utilisation"]["value"], source)]
    for kind in ("SCR", "SLWR"):
        data, source = max((r for r in records if r[0]["case_id"].endswith("-"+kind)),
                           key=lambda pair: pair[0]["max_utilisation"]["value"])
        tokens.append(token_value("V-STR-UMAX-"+kind, data["max_utilisation"]["value"], source))
        data, source = next(r for r in records if r[0]["case_id"] == reference_pair+"-"+kind)
        tokens.append(token_value("V-STR-TDP-"+kind+"-REF", data["touchdown"]["utilisation"], source))
    versions = sorted({r[0]["software"]["name"]+" "+r[0]["software"]["version"] for r in records})
    tokens.append(token_value("V-SW-VERSION", "; ".join(versions), [source for _, source in records]))
    manifest = {"artifacts": sorted(artifacts, key=lambda a: a["id"]), "sources": sources,
                "reference_pair": reference_pair, "touchdown_window_m": touchdown_window_m,
                "utilisation_limit": {"value": NORMALISED_LIMIT, "source": "definition: stress / supplied allowable"},
                "draft_tokens": {"FIG-STR-05": "top angle figure and table", "FIG-STR-06": "hang-off tension figure and table"}}
    write_json(output / "tokens.json", tokens)
    write_json(output / "manifest.json", manifest)
    return manifest


def main() -> None:
    """Run the exported-result post-processor from the command line."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--reference-pair", required=True)
    parser.add_argument("--touchdown-window-m", required=True, type=float)
    args = parser.parse_args()
    build_strength(args.results, args.output, reference_pair=args.reference_pair,
                   touchdown_window_m=args.touchdown_window_m)


if __name__ == "__main__":
    main()
