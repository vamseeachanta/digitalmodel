# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Join qualified U1 and same-station annual benchmark damage exports.

Run with ``python -m digitalmodel.riser_study.decision_chart --help``.
No solver response, stress or S-N damage is calculated here.
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
from matplotlib.patches import ConnectionPatch

from digitalmodel.riser_study.results_exchange import (
    load_json, validate_campaign_run, validate_result,
)


ROW_FIELDS = (
    "case_id", "configuration", "size", "wall", "max_utilisation",
    "max_utilisation_arc_length_m", "max_utilisation_location", "strength_run_id",
    "annual_benchmark_damage", "damage_arc_length_m", "damage_location", "meets_strength",
)
ARTIFACTS = (
    "FIG-DEC-01.svg", "FIG-DEC-01.csv", "TAB-RES-01.csv", "TAB-RES-01.md",
    "tokens.json", "manifest.json",
)
CASE_ID = re.compile(r"R(08|10|12)-W[12]-(SCR|SLWR)")
RUN_FIELDS = ("id", "phase", "status", "qualified")


def _digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _criteria(path: Path) -> dict[str, Any]:
    criteria = load_json(path)
    if not isinstance(criteria, dict) or set(criteria) != {"strength"}:
        raise ValueError("criteria must specify only the U1 strength screen")
    item = criteria["strength"]
    if not isinstance(item, dict):
        raise ValueError("strength criterion must be an object")
    for field in ("criterion", "unit", "basis_label", "source"):
        if not isinstance(item.get(field), str) or not item[field].strip():
            raise ValueError("strength criterion needs label, unit, basis_label and source")
    if (isinstance(item.get("value"), bool) or item.get("value") != 1
            or item["unit"] != "-"):
        raise ValueError("the U1 strength screening line must be one")
    return criteria


def _expected(path: Path) -> tuple[list[str], list[str], dict[str, Any]]:
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
            or len(states) != 3 or any(not isinstance(state, str) for state in states)
            or set(states) != {"F03", "F04", "F05"}):
        raise ValueError("cases must be unique and fatigue states must be F03, F04 and F05")
    basis = manifest.get("basis_label")
    if not isinstance(basis, str) or not basis.strip():
        raise ValueError("case manifest requires a basis_label")
    return sorted(cases), sorted(states), manifest


def _run_ledger(manifest: dict[str, Any]) -> dict[str, dict[str, Any]]:
    records = manifest.get("runs")
    if not isinstance(records, list) or not records:
        raise ValueError("case manifest requires an explicit run ledger")
    ledger = {}
    for run in records:
        validate_campaign_run(run)
        if run["id"] in ledger:
            raise ValueError("duplicate run id in ledger")
        ledger[run["id"]] = run
    return ledger


def _eligible(result: dict[str, Any], ledger: dict[str, Any], seen: set[str]) -> bool:
    run = result["run"]
    validate_campaign_run(run)
    identifier = run["id"]
    if (identifier not in ledger or identifier in seen
            or any(run[k] != ledger[identifier][k] for k in RUN_FIELDS)):
        raise ValueError("exported run must match exactly one ledger entry")
    seen.add(identifier)
    return run["phase"] == "A" and run["status"] == "completed" and run["qualified"]


def _station_damage(fatigue: list[dict[str, Any]]) -> tuple[float, float, str]:
    stations = []
    for result in fatigue:
        values = {}
        for hot in result["hot_spots"]:
            arc = hot["arc_length_m"]
            if arc in values or "damage_per_year" not in hot:
                raise ValueError("annual damage must be unique and explicit at each station")
            values[arc] = (hot["name"], hot["damage_per_year"])
        if stations and (values.keys() != stations[0].keys()
                         or any(values[a][0] != stations[0][a][0] for a in values)):
            raise ValueError("fatigue exports must share the same station grid and locations")
        stations.append(values)
    sums = {arc: math.fsum(state[arc][1] for state in stations) for arc in stations[0]}
    if not all(math.isfinite(damage) for damage in sums.values()):
        raise ValueError("same-station damage sum must be finite")
    arc = min(sums, key=lambda a: (-sums[a], a))
    return sums[arc], arc, stations[0][arc][0]


def _collect(results: Path, manifest: Path, criteria_file: Path,
             fatigue_tokens_file: Path) -> dict[str, Any]:
    criteria = _criteria(criteria_file)
    cases, states, campaign = _expected(manifest)
    ledger = _run_ledger(campaign)
    fatigue_tokens = load_json(fatigue_tokens_file)
    token = fatigue_tokens.get("V-FAT-RESOLVED", {}) if isinstance(fatigue_tokens, dict) else {}
    resolved = token.get("value") if isinstance(token, dict) else None
    if resolved not in ("resolved", "not resolved"):
        raise ValueError("upstream V-FAT-RESOLVED must state resolved or not resolved")
    strength_files = sorted(set(results.glob("*/strength.json")) | set(results.glob("*/strength-*.json")))
    actual = {p.parent.name for p in strength_files}
    fatigue_cases = {p.parent.name for p in results.glob("*/fatigue-*.json")}
    if actual != set(cases) or fatigue_cases != set(cases):
        raise ValueError("exports must match all completed manifest cases exactly")
    inputs: list[dict[str, str]] = []

    def record(path: Path, role: str) -> None:
        try:
            locator = path.relative_to(manifest.parent).as_posix()
        except ValueError:
            locator = path.name
        inputs.append({"role": role, "path": locator, "sha256": _digest(path)})

    record(manifest, "case_manifest")
    record(criteria_file, "criteria")
    record(fatigue_tokens_file, "fatigue_tokens")
    rows = []
    seen: set[str] = set()
    sea_state_metadata: dict[str, dict[str, Any]] = {}
    for case in cases:
        folder = results / case
        if {p.name for p in folder.glob("fatigue-*.json")} != {f"fatigue-{state}.json" for state in states}:
            raise ValueError("fatigue exports must match all manifest sea states exactly")
        qualified = []
        for path in (p for p in strength_files if p.parent == folder):
            result = load_json(path)
            validate_result(result, "strength")
            if result["case_id"] != case:
                raise ValueError("strength case id conflicts with its directory")
            eligible = _eligible(result, ledger, seen)
            if eligible:
                if result.get("utilisation_basis") != "U1":
                    raise ValueError("qualified strength exports must explicitly identify U1")
                qualified.append(result)
            record(path, "strength")
        if not qualified:
            raise ValueError("each completed case requires qualified Phase A U1")
        strength = min(qualified, key=lambda r: (-r["max_utilisation"]["value"], r["run"]["id"]))
        fatigue = []
        for state in states:
            path = folder / f"fatigue-{state}.json"
            result = load_json(path)
            validate_result(result, "fatigue")
            if result["case_id"] != case or result["sea_state"]["id"] != state:
                raise ValueError("fatigue identifiers conflict with their file location")
            if not _eligible(result, ledger, seen):
                raise ValueError("fatigue exports require completed qualified Phase A runs")
            sea = {key: result["sea_state"][key]
                   for key in ("id", "Hs_m", "Tp_s", "direction_deg", "occurrence")}
            if state in sea_state_metadata and sea_state_metadata[state] != sea:
                raise ValueError("fatigue sea-state parameters differ between cases")
            sea_state_metadata[state] = sea
            fatigue.append(result)
            record(path, "fatigue")
        damage, arc, location = _station_damage(fatigue)
        peak = strength["max_utilisation"]
        rows.append({
            "case_id": case, "configuration": case.rsplit("-", 1)[1],
            "size": case[1:3], "wall": case.split("-")[1],
            "max_utilisation": peak["value"],
            "max_utilisation_arc_length_m": peak["arc_length_m"],
            "max_utilisation_location": peak["location"], "strength_run_id": strength["run"]["id"],
            "annual_benchmark_damage": damage, "damage_arc_length_m": arc,
            "damage_location": location, "meets_strength": peak["value"] <= 1,
        })
    phase_a = [r for r in ledger.values() if r["phase"] == "A"]
    return {
        "basis_label": campaign["basis_label"], "criteria": criteria,
        "fatigue_sea_state_ids": states, "fatigue_resolved": resolved,
        "rows": rows, "count_strength": sum(row["meets_strength"] for row in rows),
        "run_counts": {"attempted": len(phase_a),
                       "completed": sum(r["status"] == "completed" for r in phase_a),
                       "qualified": sum(r["qualified"] for r in phase_a)},
        "runs": list(ledger.values()), "inputs": inputs,
        "phase_b_status": "not yet done",
        "method": "Maximum qualified Phase A U1 versus maximum same-station annual damage sum",
        "limitations": "Screening sensitivity: three sea states, not a full fatigue analysis. "
                       "Air-equivalent benchmark damage, not service life. "
                       "Damage has no qualified service acceptance line. Phase B is not assessed.",
    }


def build_chart(rows: list[dict[str, Any]], criteria: dict[str, Any]) -> tuple[Figure, Any]:
    """Plot the exported damage sum with only the U1 screening line."""
    fig = Figure(figsize=(12, 8), layout="constrained")
    FigureCanvasAgg(fig)
    ax = fig.subplots()
    positive = [r["annual_benchmark_damage"] for r in rows if r["annual_benchmark_damage"] > 0]
    # The empty-positive axis extent is presentation only; zero stays zero and
    # uses the separate strip at the bottom, never a numerical damage floor.
    y_lower, y_upper = (min(positive) / 2, max(positive) * 2) if positive else (0.01, 1)
    ax.set(xlim=(0, max(1, *(r["max_utilisation"] for r in rows)) * 1.15),
           ylim=(y_lower, y_upper), yscale="log", xlabel="Maximum qualified Phase A U1 (-)",
           ylabel="Annual benchmark damage (-, logarithmic scale)")
    for size, color in (("08", "tab:blue"), ("10", "tab:orange"), ("12", "tab:green")):
        for configuration, marker in (("SCR", "o"), ("SLWR", "^")):
            pair = sorted((r for r in rows if r["size"] == size and r["configuration"] == configuration),
                          key=lambda r: r["wall"])
            if len(pair) == 2 and all(r["annual_benchmark_damage"] > 0 for r in pair):
                ax.plot([r["max_utilisation"] for r in pair],
                        [r["annual_benchmark_damage"] for r in pair], color=color, alpha=0.5, zorder=1)
            elif len(pair) == 2:
                # Each endpoint uses its own coordinate system; zero has no
                # coordinate on a logarithmic scale and stays in the strip.
                ax.add_artist(ConnectionPatch(
                    xyA=(pair[0]["max_utilisation"], pair[0]["annual_benchmark_damage"]),
                    xyB=(pair[1]["max_utilisation"], pair[1]["annual_benchmark_damage"]),
                    coordsA=ax.transData if pair[0]["annual_benchmark_damage"] > 0 else ax.get_xaxis_transform(),
                    coordsB=ax.transData if pair[1]["annual_benchmark_damage"] > 0 else ax.get_xaxis_transform(),
                    color=color, alpha=0.5, zorder=1, clip_on=False))
            for is_zero in (True, False):
                points = [r for r in pair if (r["annual_benchmark_damage"] == 0) == is_zero]
                if points:
                    ax.scatter([r["max_utilisation"] for r in points],
                               [r["annual_benchmark_damage"] for r in points],
                               transform=ax.get_xaxis_transform() if is_zero else ax.transData,
                               marker=marker, color=color, s=65, clip_on=not is_zero,
                               label=f"NPS {int(size)} {configuration}" + (" (true zero)" if is_zero else ""),
                               zorder=3)
    ax.axvline(criteria["strength"]["value"], color="tab:red", linestyle="--", label="U1 = 1 screen")
    ax.set_title("Fatigue against strength\nAir-equivalent benchmark damage, not service life")
    ax.grid(True, which="both", alpha=0.2)
    ax.legend(fontsize=8, loc="best")
    _label_points(fig, ax, rows)
    if any(r["annual_benchmark_damage"] == 0 for r in rows):
        ax.text(0.01, 0.02, "True zero: separate axis strip", transform=ax.transAxes, fontsize=8)
    return fig, ax


def _label_points(fig: Figure, ax: Any, rows: list[dict[str, Any]]) -> None:
    # Candidate offsets and text padding are presentation settings in points.
    # Stagger nearby labels without perturbing any plotted result coordinate.
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    occupied = [ax.get_legend().get_window_extent(renderer)]
    for row in rows:
        coordinates = (ax.transData if row["annual_benchmark_damage"] > 0
                       else ax.get_xaxis_transform())
        label = ax.annotate(row["case_id"], (row["max_utilisation"], row["annual_benchmark_damage"]),
                            xycoords=coordinates, xytext=(6, 6), textcoords="offset points", fontsize=8)
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


def _table(data: dict[str, Any]) -> str:
    results = [
        "# Strength and fatigue results", "",
        f"Basis: {_cell(data['basis_label'])}. {_cell(data['limitations'])}", "",
        "| Case id | Maximum qualified Phase A U1 (-) | Strength location | Annual benchmark damage (-) | Damage location | U1 at or below one |",
        "|---|---:|---|---:|---|---|",
    ]
    for row in data["rows"]:
        results.append(f"| {row['case_id']} | {row['max_utilisation']:.1f} | "
                       f"{_cell(row['max_utilisation_location'])} | {row['annual_benchmark_damage']:.1e} | "
                       f"{_cell(row['damage_location'])} | {row['meets_strength']} |")
    return "\n".join(results) + "\n"


def _answer(rows: list[dict[str, Any]], size: str, resolved: str) -> str:
    candidates = [r for r in rows if r["size"] == size and r["meets_strength"]]
    if resolved != "resolved" or not candidates:
        return "ranking unresolved"
    best = min(r["annual_benchmark_damage"] for r in candidates)
    winners = [r for r in candidates if r["annual_benchmark_damage"] == best]
    if len(winners) != 1:
        return "ranking unresolved"
    return f"{winners[0]['configuration']} {winners[0]['wall']}"


def generate(results: Path, case_manifest: Path, criteria_file: Path, output: Path,
             fatigue_tokens_file: Path) -> dict[str, Any]:
    """Validate exports, then write the ten registered decision tokens.

    Existing deliverables are never overwritten. CSV and JSON retain exported
    precision; tables use one decimal place in scientific notation for damage.
    """
    results, case_manifest, criteria_file, output, fatigue_tokens_file = map(
        Path, (results, case_manifest, criteria_file, output, fatigue_tokens_file))
    data = _collect(results, case_manifest, criteria_file, fatigue_tokens_file)
    if output.exists() and any(output.iterdir()):
        raise FileExistsError("output directory must be empty to prevent stale decision artifacts")
    figure, _ = build_chart(data["rows"], data["criteria"])

    def value(item: Any) -> dict[str, Any]:
        return {"kind": "VALUE", "value": item, "unit": "-", "source_file": "manifest.json"}

    tokens = {
        "FIG-DEC-01": {"kind": "FIGURE", "path": "FIG-DEC-01.svg", "data": "FIG-DEC-01.csv",
                       "source_file": "manifest.json"},
        "TAB-RES-01": {"kind": "TABLE", "path": "TAB-RES-01.md", "data": "TAB-RES-01.csv",
                       "source_file": "manifest.json"},
        "V-DEC-COUNT-STR": value(data["count_strength"]),
        **{f"V-ANS-OD{i}": value(_answer(data["rows"], size, data["fatigue_resolved"]))
           for i, size in enumerate(("08", "10", "12"), 1)},
        **{f"V-RUN-{key.upper()}": value(count) for key, count in data["run_counts"].items()},
        "V-PHB-STATUS": value(data["phase_b_status"]),
    }
    output.mkdir(parents=True, exist_ok=True)
    figure.savefig(output / "FIG-DEC-01.svg", format="svg", metadata={"Date": None})
    figure.clear()
    csv_text = _csv_text(data["rows"], ROW_FIELDS)
    for filename, content in (
        ("FIG-DEC-01.csv", csv_text), ("TAB-RES-01.csv", csv_text),
        ("TAB-RES-01.md", _table(data)),
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
    parser.add_argument("--fatigue-tokens", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        data = generate(args.results, args.manifest, args.criteria, args.output, args.fatigue_tokens)
    except (ValueError, OSError):
        parser.exit(2, "Decision output failed: invalid or incomplete exchange, or unavailable output directory.\n")
    print(f"Cases at or below U1 = 1: {data['count_strength']} of {len(data['rows'])}")


if __name__ == "__main__":
    main()
