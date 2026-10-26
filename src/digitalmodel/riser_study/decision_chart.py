# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Join exported strength and fatigue sensitivity results against file criteria.

Run with ``python -m digitalmodel.riser_study.decision_chart --help``.
No response, stress, S-N damage or acceptance criterion is calculated here.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import math
import re
from pathlib import Path
from typing import Any

from matplotlib.backends.backend_agg import FigureCanvasAgg
from matplotlib.figure import Figure
from matplotlib.patches import Rectangle

from digitalmodel.riser_study.results_exchange import load_json, validate_result


ROW_FIELDS = (
    "case_id", "configuration", "max_utilisation", "max_utilisation_arc_length_m",
    "max_utilisation_location", "shortest_life_years", "governing_sea_state",
    "meets_strength", "meets_fatigue", "meets_both",
)
CRITERIA_FIELDS = ("key", "criterion", "value", "unit", "basis_label", "source")
ARTIFACTS = (
    "decision.svg", "results.csv", "results.md", "criteria.csv", "criteria.md",
    "tokens.json", "manifest.json",
)
CASE_ID = re.compile(r"R(08|10|12)-W[12]-(SCR|SLWR)")
STATE_ID = re.compile(r"[A-Za-z0-9][A-Za-z0-9_-]*")


def _digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _criteria(path: Path) -> dict[str, dict[str, Any]]:
    criteria = load_json(path)
    if not isinstance(criteria, dict) or set(criteria) != {"strength", "fatigue"}:
        raise ValueError("criteria must specify strength and fatigue")
    for key, unit in (("strength", "-"), ("fatigue", "years")):
        item = criteria[key]
        if not isinstance(item, dict):
            raise ValueError("each criterion must be an object")
        for field in ("criterion", "unit", "basis_label", "source"):
            if not isinstance(item.get(field), str) or not item[field].strip():
                raise ValueError("each criterion needs a label, unit, basis_label and source")
        value = item.get("value")
        if (isinstance(value, bool) or not isinstance(value, (int, float))
                or not math.isfinite(value) or value <= 0 or item["unit"] != unit):
            raise ValueError("criteria require finite positive values and supported units")
    return {key: criteria[key] for key in ("strength", "fatigue")}


def _expected(path: Path) -> tuple[list[str], list[str], str]:
    manifest = load_json(path)
    if not isinstance(manifest, dict) or not isinstance(manifest.get("cases"), list):
        raise ValueError("case manifest must contain a cases array")
    cases = []
    for row in manifest["cases"]:
        case = row.get("case_id") if isinstance(row, dict) else None
        if not isinstance(case, str) or not CASE_ID.fullmatch(case):
            raise ValueError("case manifest contains an invalid case id")
        if "configuration" in row and row["configuration"] != case.rsplit("-", 1)[1]:
            raise ValueError("case configuration conflicts with case id")
        cases.append(case)
    states = manifest.get("fatigue_sea_state_ids")
    if (not cases or len(cases) != len(set(cases)) or not isinstance(states, list)
            or not states or any(not isinstance(s, str) or not STATE_ID.fullmatch(s) for s in states)
            or len(states) != len(set(states))):
        raise ValueError("case and sea-state lists must be nonempty and unique")
    basis = manifest.get("basis_label")
    if not isinstance(basis, str) or not basis.strip():
        raise ValueError("case manifest requires a basis_label")
    return sorted(cases), sorted(states), basis


def _collect(results: Path, manifest: Path, criteria_file: Path) -> dict[str, Any]:
    criteria = _criteria(criteria_file)
    cases, states, basis = _expected(manifest)
    actual = {p.parent.name for p in results.glob("*/strength.json")}
    fatigue_cases = {p.parent.name for p in results.glob("*/fatigue-*.json")}
    if actual != set(cases) or fatigue_cases != set(cases):
        raise ValueError("strength exports must match all manifest cases exactly")
    inputs: list[dict[str, str]] = []

    def record(path: Path, role: str) -> None:
        try:
            locator = path.relative_to(manifest.parent).as_posix()
        except ValueError:
            locator = path.name
        inputs.append({"role": role, "path": locator, "sha256": _digest(path)})

    record(manifest, "case_manifest")
    record(criteria_file, "criteria")
    rows = []
    sea_state_metadata: dict[str, dict[str, Any]] = {}
    for case in cases:
        folder = results / case
        expected_files = {f"fatigue-{state}.json" for state in states}
        if {p.name for p in folder.glob("fatigue-*.json")} != expected_files:
            raise ValueError("fatigue exports must match all manifest sea states exactly")
        strength_file = folder / "strength.json"
        strength = load_json(strength_file)
        validate_result(strength, "strength")
        if strength["case_id"] != case:
            raise ValueError("strength case id conflicts with its directory")
        record(strength_file, "strength")
        fatigue = []
        for state in states:
            fatigue_file = folder / f"fatigue-{state}.json"
            result = load_json(fatigue_file)
            validate_result(result, "fatigue")
            if result["case_id"] != case or result["sea_state"]["id"] != state:
                raise ValueError("fatigue identifiers conflict with their file location")
            sea = {key: result["sea_state"][key]
                   for key in ("id", "Hs_m", "Tp_s", "direction_deg", "occurrence")}
            if state in sea_state_metadata and sea_state_metadata[state] != sea:
                raise ValueError("fatigue sea-state parameters differ between cases")
            sea_state_metadata[state] = sea
            fatigue.append(result)
            record(fatigue_file, "fatigue")
        shortest = min(fatigue, key=lambda f: (f["life_years"], f["sea_state"]["id"]))
        peak = strength["max_utilisation"]
        strength_pass = peak["value"] <= criteria["strength"]["value"]
        fatigue_pass = shortest["life_years"] >= criteria["fatigue"]["value"]
        rows.append({
            "case_id": case, "configuration": case.rsplit("-", 1)[1],
            "max_utilisation": peak["value"],
            "max_utilisation_arc_length_m": peak["arc_length_m"],
            "max_utilisation_location": peak["location"],
            "shortest_life_years": shortest["life_years"],
            "governing_sea_state": shortest["sea_state"]["id"],
            "meets_strength": strength_pass, "meets_fatigue": fatigue_pass,
            "meets_both": strength_pass and fatigue_pass,
        })
    return {
        "basis_label": basis, "criteria": criteria, "fatigue_sea_state_ids": states,
        "rows": rows, "count_both": sum(row["meets_both"] for row in rows), "inputs": inputs,
        "method": "Maximum exported utilisation versus minimum exported sea-state life; inclusive criteria",
        "limitations": "Screening sensitivity, not a full fatigue analysis; no occurrence reweighting or damage summation",
    }


def build_chart(rows: list[dict[str, Any]], criteria: dict[str, Any]) -> tuple[Figure, Any]:
    """Plot unrounded rows with file-supplied criterion lines and shaded region."""
    # Figure size, marker size, opacity and padding are presentation settings
    # only; no engineering threshold is embedded in these settings.
    fig = Figure(figsize=(12, 8), layout="constrained")
    FigureCanvasAgg(fig)
    ax = fig.subplots()
    strength_limit = criteria["strength"]["value"]
    fatigue_target = criteria["fatigue"]["value"]
    x_upper = max(strength_limit, *(r["max_utilisation"] for r in rows)) * 1.15
    y_lower = min(fatigue_target, *(r["shortest_life_years"] for r in rows)) / 2
    y_upper = max(fatigue_target, *(r["shortest_life_years"] for r in rows)) * 2
    ax.set(xlim=(0, x_upper), ylim=(y_lower, y_upper), yscale="log",
           xlabel="Maximum utilisation (-)", ylabel="Shortest fatigue life (years, logarithmic scale)")
    ax.add_patch(Rectangle((0, fatigue_target), strength_limit, y_upper - fatigue_target,
                           facecolor="tab:green", alpha=0.12, label="Both criteria met"))
    for configuration, marker, color in (("SCR", "o", "tab:blue"), ("SLWR", "^", "tab:orange")):
        points = [r for r in rows if r["configuration"] == configuration]
        if points:
            ax.scatter([r["max_utilisation"] for r in points],
                       [r["shortest_life_years"] for r in points],
                       marker=marker, color=color, s=65, label=configuration, zorder=3)
    ax.axvline(strength_limit, color="tab:red", linestyle="--",
               label=f"Strength criterion: {strength_limit:.1f} ({criteria['strength']['basis_label']})")
    ax.axhline(fatigue_target, color="tab:purple", linestyle=":",
               label=f"Fatigue target: {fatigue_target:.1f} years ({criteria['fatigue']['basis_label']})")
    ax.set_title("Fatigue against strength\nScreening sensitivity")
    ax.grid(True, which="both", alpha=0.2)
    ax.legend(fontsize=8, loc="best")
    _label_points(fig, ax, rows)
    return fig, ax


def _label_points(fig: Figure, ax: Any, rows: list[dict[str, Any]]) -> None:
    # Candidate offsets and text padding are presentation settings in points.
    # Stagger nearby labels without perturbing any plotted result coordinate.
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    occupied = [ax.get_legend().get_window_extent(renderer)]
    for row in rows:
        label = ax.annotate(row["case_id"], (row["max_utilisation"], row["shortest_life_years"]),
                            xytext=(6, 6), textcoords="offset points", fontsize=8)
        candidates = ((dx, dy) for level in range(len(rows) + 1)
                      for dx, dy in ((6, 6 + 14 * level), (6, -14 * (level + 1)),
                                     (-6, 6 + 14 * level), (-6, -14 * (level + 1))))
        for dx, dy in candidates:
            label.set_position((dx, dy))
            label.set_horizontalalignment("left" if dx > 0 else "right")
            box = label.get_window_extent(renderer).expanded(1.05, 1.15)
            if not any(box.overlaps(other) for other in occupied):
                break
        occupied.append(box)


def _csv_text(rows: list[dict[str, Any]], fields: tuple[str, ...]) -> str:
    stream = io.StringIO(newline="")
    writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
    return stream.getvalue()


def _cell(value: Any) -> str:
    return str(value).replace("|", "\\|").replace("\n", " ").replace("\r", " ")


def _tables(data: dict[str, Any]) -> tuple[str, str]:
    results = [
        "# Strength and fatigue results", "",
        f"Basis: {_cell(data['basis_label'])}. Screening sensitivity; not a full fatigue analysis.", "",
        "| Case id | Maximum utilisation (-) | Location | Shortest life (years) | Governing sea state | Meets both criteria |",
        "|---|---:|---|---:|---|---|",
    ]
    for row in data["rows"]:
        results.append(f"| {row['case_id']} | {row['max_utilisation']:.1f} | "
                       f"{_cell(row['max_utilisation_location'])} | {row['shortest_life_years']:.1f} | "
                       f"{row['governing_sea_state']} | {row['meets_both']} |")
    criteria = [
        "# Strength criterion and fatigue target", "",
        "| Criterion | Value | Unit | Basis label | Source |", "|---|---:|---|---|---|",
    ]
    for item in data["criteria"].values():
        criteria.append(f"| {_cell(item['criterion'])} | {item['value']:.1f} | {_cell(item['unit'])} | "
                        f"{_cell(item['basis_label'])} | {_cell(item['source'])} |")
    return "\n".join(results) + "\n", "\n".join(criteria) + "\n"


def generate(results: Path, case_manifest: Path, criteria_file: Path, output: Path) -> dict[str, Any]:
    """Validate the complete exchange, then write figure, tables and token data.

    Existing deliverables are never overwritten. Machine-readable CSV and JSON
    retain the exported precision; display tables use one decimal place.
    """
    results, case_manifest, criteria_file, output = map(Path, (results, case_manifest, criteria_file, output))
    data = _collect(results, case_manifest, criteria_file)
    if any((output / name).exists() for name in ARTIFACTS):
        raise FileExistsError("output contains existing decision artifacts; choose a new directory")
    figure, _ = build_chart(data["rows"], data["criteria"])
    results_md, criteria_md = _tables(data)
    criteria_rows = [{"key": key, **{field: value[field] for field in CRITERIA_FIELDS[1:]}}
                     for key, value in data["criteria"].items()]
    tokens = {
        "FIG-DEC-01": {"kind": "FIGURE", "path": "decision.svg", "data": "results.csv"},
        "TAB-RES-01": {"kind": "TABLE", "path": "results.md", "data": "results.csv"},
        "V-DEC-COUNT-BOTH": {"kind": "VALUE", "value": data["count_both"], "unit": "-"},
        "TAB-DEC-CRITERIA-01": {"kind": "TABLE", "path": "criteria.md", "data": "criteria.csv"},
    }
    output.mkdir(parents=True, exist_ok=True)
    figure.savefig(output / "decision.svg", format="svg", metadata={"Date": None})
    figure.clear()
    for filename, content in (
        ("results.csv", _csv_text(data["rows"], ROW_FIELDS)), ("results.md", results_md),
        ("criteria.csv", _csv_text(criteria_rows, CRITERIA_FIELDS)), ("criteria.md", criteria_md),
        ("tokens.json", json.dumps(tokens, indent=2, allow_nan=False) + "\n"),
    ):
        (output / filename).write_text(content, encoding="utf-8")
    data["artifacts"] = {name: _digest(output / name) for name in ARTIFACTS if name != "manifest.json"}
    (output / "manifest.json").write_text(json.dumps(data, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    return data


def main() -> None:
    """Parse file locations and generate decision artifacts from exports."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--results", type=Path, required=True)
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--criteria", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        data = generate(args.results, args.manifest, args.criteria, args.output)
    except (ValueError, OSError):
        parser.exit(2, "Decision output failed: invalid or incomplete exchange, or unavailable output directory.\n")
    print(f"Cases meeting both criteria: {data['count_both']} of {len(data['rows'])}")


if __name__ == "__main__":
    main()
