# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Produce air-equivalent benchmark damage, not service life, from exports.

For each bin N = 10**log_a / (range_MPa * SCF * thickness_factor)**m.
Annual benchmark damage = occurrence * sum(cycles_per_year / N).
Annual cycles precede occurrence weighting. The three sea-state contributions
are summed at the same station before any maximum is selected. The largest
nominal histogram range is plotted, without implying an equivalent range.

The study records FAT 80, slope 3, 2 million cycles at 80 MPa, no endurance
limit and SCF 1.2. No safety factor or additional fatigue mechanism is added.
A zero contribution remains zero; reciprocal life at zero is not established.

Usage: python -m digitalmodel.riser_study.riser_fatigue_runs RESULTS OUTPUT
The input layout is RESULTS/<case_id>/fatigue-<sea_state_id>.json.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import math
import textwrap
from pathlib import Path
from xml.etree import ElementTree as ET

from .results_exchange import validate_result

SCOPE = ("Screening sensitivity over 3 sea states; not a full fatigue analysis; "
         "air-equivalent benchmark damage, not service life")
# Three states are required by the sensitivity brief, not by a fatigue standard.
SEA_STATE_COUNT = 3
# Serialization comparison tolerance; not an engineering acceptance allowance.
RECORD_REL_TOL = 1e-8


def _json(value: object) -> str:
    return json.dumps(value, sort_keys=True, allow_nan=False)


def _write_json(path: Path, value: object) -> None:
    path.write_text(json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8")


def _point_result(point: dict, curve: dict, occurrence: float) -> dict:
    correction = curve.get("thickness_correction")
    factor = 1.0
    try:
        if correction is not None:
            factor = (correction["thickness_mm"] / correction["reference_thickness_mm"]) ** correction["exponent"]
        if not math.isfinite(factor) or factor <= 0:
            raise ValueError("thickness factor exceeds numeric range")
        bins = point.get("range_histogram", [{"stress_range_MPa": point.get("stress_range_MPa"),
                                              "cycles_per_year": point["cycles_per_year"]}])
        damages = []
        for item in bins:
            stress = item["stress_range_MPa"] * point["scf"] * factor
            if not math.isfinite(stress):
                raise ValueError("corrected stress is nonfinite")
            if stress == 0 or item["cycles_per_year"] == 0 or occurrence == 0:
                damages.append(0.0)
            else:
                # Logarithmic evaluation avoids overflow in 10**log_a itself.
                damage = 10 ** (math.log10(occurrence) + math.log10(item["cycles_per_year"])
                                + curve["m"] * math.log10(stress) - curve["log_a"])
                if damage == 0:
                    raise ValueError("damage underflows numeric range")
                damages.append(damage)
        damage = math.fsum(damages)
        peak = max(item["stress_range_MPa"] for item in bins)
        effective = peak * point["scf"] * factor
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("fatigue calculation exceeds numeric range") from exc
    if not all(math.isfinite(value) for value in (factor, damage, effective)):
        raise ValueError("fatigue calculation is nonfinite")
    return {"hot_spot": point.get("name", "arc_point"), "arc_length_m": point["arc_length_m"],
            "stress_range_MPa": peak, "effective_stress_range_MPa": effective,
            "cycles_per_year": point["cycles_per_year"], "scf": point["scf"],
            "thickness_factor": factor, "damage_per_year": damage}


def calculate(data: dict) -> list[dict]:
    """Calculate every hot spot and check the recorded case/state maximum.

    Raises:
        ValueError: If the export is invalid or its recorded maximum disagrees.
    """
    validate_result(data, "fatigue")
    rows = [{"case_id": data["case_id"], "sea_state_id": data["sea_state"]["id"], "scope": SCOPE,
             **_point_result(point, data["sn_curve"], data["sea_state"]["occurrence"])}
            for point in data["hot_spots"]]
    worst = _governing(rows)
    if not math.isclose(worst["damage_per_year"], data["damage_per_year"],
                        rel_tol=RECORD_REL_TOL, abs_tol=0):
        raise ValueError("recorded damage_per_year disagrees with calculated hot-spot maximum")
    return rows


def _governing(rows: list[dict]) -> dict:
    return min(rows, key=lambda row: (-row["damage_per_year"], row["case_id"],
                                     row["sea_state_id"], row["hot_spot"]))


def _load(results: Path) -> list[tuple[Path, dict, bytes]]:
    records = []
    for path in sorted(results.glob("*/fatigue-*.json")):
        raw = path.read_bytes()
        try:
            data = json.loads(raw)
        except (ValueError, UnicodeError) as exc:
            raise ValueError("invalid fatigue JSON export") from exc
        validate_result(data, "fatigue")
        if path.parent.name != data["case_id"] or path.name != f"fatigue-{data['sea_state']['id']}.json":
            raise ValueError("export case or sea state differs from its file location")
        records.append((path, data, raw))
    if not records:
        raise ValueError("no fatigue exports found")
    definitions, states, names = {}, {}, {}
    for _, data, _ in records:
        sea, case = data["sea_state"], data["case_id"]
        state_id = sea["id"]
        if state_id in definitions and definitions[state_id] != sea:
            raise ValueError("sea-state definition differs across cases")
        definitions[state_id] = sea
        states.setdefault(case, set()).add(state_id)
        spot_names = {point["name"] for point in data["hot_spots"]}
        if case in names and names[case] != spot_names:
            raise ValueError("hot-spot coverage differs across sea states")
        names[case] = spot_names
    if len(definitions) != SEA_STATE_COUNT or any(value != set(definitions) for value in states.values()):
        raise ValueError("every case must export the same 3 sea states")
    if sum(sea["occurrence"] for sea in definitions.values()) > 1 + RECORD_REL_TOL:
        raise ValueError("sea-state occurrence fractions sum above one")
    return records


def _input_row(data: dict) -> dict:
    return {"case_id": data["case_id"], "sea_state_id": data["sea_state"]["id"],
            **{key: data["sea_state"][key] for key in ("Hs_m", "Tp_s", "direction_deg", "occurrence")},
            "sn_name": data["sn_curve"]["name"], "sn_source_label": data["sn_curve"]["source_label"],
            "m": data["sn_curve"]["m"], "log_a": data["sn_curve"]["log_a"],
            "thickness_correction": _json(data["sn_curve"].get("thickness_correction")),
            "sn_curve_record": _json(data["sn_curve"]),
            "hot_spot_inputs": _json(data["hot_spots"]),
            "arc_distribution_inputs": _json(data.get("arc_distribution")),
            "run": _json(data["run"]), "damage_rule": "linear Miner sum",
            "cycle_basis": "annual continuous exposure; multiply occurrence once",
            "safety_factor": "none added", "scope": SCOPE}


def _csv_rows(rows: list[dict], fields: list[str]) -> tuple[str, list[dict]]:
    stream = io.StringIO(newline="")
    writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
    payload = stream.getvalue()
    # The manifest and SVG share the actual serialized CSV rows, not rounded copies.
    return payload, list(csv.DictReader(io.StringIO(payload)))


def _table_display(key: str, value: str) -> str:
    calculated = {"stress_range_MPa", "effective_stress_range_MPa", "damage_per_year",
                  "arc_length_m", "thickness_factor", "scf", "cycles_per_year"}
    if key in calculated and value:
        number = float(value)
        return f"{number:.1e}" if number and (abs(number) < 0.1 or abs(number) >= 100000) else f"{number:.1f}"
    return value


def _root(token: str, title: str, rows: list[dict], width: int, height: int):
    root = ET.Element("svg", xmlns="http://www.w3.org/2000/svg", width=str(width),
                      height=str(height), viewBox=f"0 0 {width} {height}", role="img",
                      **{"font-family": "sans-serif", "font-size": "13"})
    ET.SubElement(root, "title").text = f"{token}: {title}. {SCOPE}"
    ET.SubElement(root, "metadata").text = _json(rows)
    _text(root, 20, 30, f"{token}: {title}")
    _text(root, 20, 55, SCOPE)
    return root


def _text(root, x, y, value, **attrs):
    ET.SubElement(root, "text", x=str(x), y=str(y), **attrs).text = str(value)


def heat_color(value: float, low: float, high: float) -> str:
    """Map positive damage to a log colour; retain white for true zero."""
    if value == 0:
        return "#ffffff"
    fraction = (math.log10(value) - low) / (high - low)
    return f"#{round(240 - 210 * fraction):02x}{round(245 - 165 * fraction):02x}{round(255 - 100 * fraction):02x}"


def _range_svg(token: str, rows: list[dict], title: str, limit: float) -> tuple[str, dict]:
    root = _root(token, title, rows, 1100, 900)
    seas = sorted({row["sea_state_id"] for row in rows})
    cases = sorted({row["case_id"] for row in rows})
    positions = {sea: 200 + index * 320 for index, sea in enumerate(seas)}
    palette = ("#165a91", "#ae3c18", "#287943", "#7651a8")
    panels = {}
    for panel, name in enumerate(("touchdown", "hang_off")):
        top, bottom = 120 + panel * 330, 350 + panel * 330
        scale = (bottom - top) / (limit or 1)
        panels[name] = {"top": top, "bottom": bottom, "y_scale": scale}
        _text(root, 100, top - 20, f"{name}: nominal stress range (MPa)")
        ET.SubElement(root, "path", d=f"M 100 {top} V {bottom} H 1000", fill="none", stroke="black")
        for tick in range(5):
            _text(root, 20, bottom - tick * (bottom - top) / 4, f"{limit * tick / 4:.1f}")
        for sea, x in positions.items():
            _text(root, x, bottom + 25, sea)
        for case_index, case in enumerate(cases):
            points = []
            for index, row in enumerate(rows):
                if row["case_id"] != case or row["hot_spot"] != name:
                    continue
                x = positions[row["sea_state_id"]]
                y = bottom - float(row["stress_range_MPa"]) * scale
                points.append((x, y))
                mark = ET.SubElement(root, "circle", cx=str(x), cy=str(y), r="5",
                                     fill=palette[case_index],
                                     **{"data-row": str(index), "data-values": _json(row)})
                ET.SubElement(mark, "title").text = (
                    f"{case} / {row['sea_state_id']} / {name}: "
                    f"{float(row['stress_range_MPa']):.1f} MPa; no range acceptance line")
            ET.SubElement(root, "polyline", points=" ".join(f"{x},{y}" for x, y in points),
                          fill="none", stroke=palette[case_index],
                          **{"data-case": case, "stroke-dasharray": "6 3" if "-W2-" in case else "none"})
    for index, case in enumerate(cases):
        _text(root, 100 + index * 240, 770, case, fill=palette[index])
    _text(root, 100, 810, "Sea state; common range scale across sizes and hot spots")
    return ET.tostring(root, encoding="unicode") + "\n", {"panels": panels, "sea_positions": positions}


def _heat_svg(token: str, rows: list[dict], title: str) -> tuple[str, dict]:
    root = _root(token, title, rows, 1300, 1000)
    positive = [float(row["damage_per_year"]) for row in rows if float(row["damage_per_year"]) > 0]
    low = math.log10(min(positive)) if positive else 0.0
    high = math.log10(max(positive)) if positive else 1.0
    if high == low:
        high = low + 1.0
    cases = sorted({row["case_id"] for row in rows})
    seas = sorted({row["sea_state_id"] for row in rows})
    case_y = {case: 150 + index * 52 for index, case in enumerate(cases)}
    sea_x = {sea: 180 + index * 130 for index, sea in enumerate(seas)}
    for panel, name in enumerate(("touchdown", "hang_off")):
        group = ET.SubElement(root, "g", transform=f"translate({panel * 630} 0)",
                              **{"data-panel": name})
        _text(group, 180, 95, name)
        for sea, x in sea_x.items():
            _text(group, x, 125, sea)
        for case, y in case_y.items():
            _text(group, 15, y + 25, case)
        for index, row in enumerate(rows):
            if row["hot_spot"] != name:
                continue
            value = float(row["damage_per_year"])
            mark = ET.SubElement(group, "rect", x=str(sea_x[row["sea_state_id"]]),
                                 y=str(case_y[row["case_id"]]), width="120", height="45",
                                 fill=heat_color(value, low, high), stroke="#555555",
                                 **{"data-row": str(index), "data-values": _json(row)})
            ET.SubElement(mark, "title").text = (
                f"{row['case_id']} / {row['sea_state_id']} / {name}: "
                f"{value:.1e} annual benchmark damage (-); no qualified service acceptance line")
    _text(root, 20, 835, "Annual benchmark damage (-): log10 colour; white = true zero (separate)")
    for tick in range(5):
        value = 10 ** (low + (high - low) * tick / 4)
        x = 20 + tick * 210
        ET.SubElement(root, "rect", x=str(x), y="860", width="180", height="25",
                      fill=heat_color(value, low, high))
        _text(root, x, 910, f"{value:.1e}")
    return ET.tostring(root, encoding="unicode") + "\n", {
        "log_min": low, "log_max": high, "colour_scale": "log10; zero separate",
        "zero_colour": "#ffffff", "sea_positions": sea_x, "case_positions": case_y}


def _table_svg(token: str, rows: list[dict], title: str) -> tuple[str, dict]:
    lines = [[line for key, value in row.items() for line in textwrap.wrap(
        f"{key}: {_table_display(key, value)}", width=160, break_on_hyphens=False)] for row in rows]
    root = _root(token, title, rows, 1500, 150 + sum(len(block) + 1 for block in lines) * 20)
    y = 100
    for index, (row, block_lines) in enumerate(zip(rows, lines)):
        block = ET.SubElement(root, "g", **{"data-row": str(index), "data-values": _json(row)})
        for line in block_lines:
            _text(block, 20, y, line)
            y += 20
        y += 20
    return ET.tostring(root, encoding="unicode") + "\n", {}


def _artifact(out: Path, token: str, rows: list[dict], title: str, limit: float = 0) -> dict:
    csv_payload, serialized = _csv_rows(rows, list(rows[0]))
    if token.startswith("FIG-FAT-RNG"):
        svg_payload, scale = _range_svg(token, serialized, title, limit)
    elif token == "FIG-FAT-HEAT-01":
        svg_payload, scale = _heat_svg(token, serialized, title)
    else:
        svg_payload, scale = _table_svg(token, serialized, title)
    csv_name, svg_name = f"{token}.csv", f"{token}.svg"
    (out / csv_name).write_text(csv_payload, encoding="utf-8")
    (out / svg_name).write_text(svg_payload, encoding="utf-8")
    return {"csv": csv_name, "svg": svg_name, "rows": serialized,
            "csv_sha256": hashlib.sha256(csv_payload.encode()).hexdigest(),
            "svg_sha256": hashlib.sha256(svg_payload.encode()).hexdigest(),
            "title": title, "scope": SCOPE, **scale}


def _benchmark(data: dict) -> None:
    curve = data["sn_curve"]
    if (curve["name"] != "FAT 80" or curve["m"] != 3
        or curve.get("reference_cycles") != 2_000_000 or curve.get("reference_range_MPa") != 80
        or "endurance_limit_MPa" not in curve or curve["endurance_limit_MPa"] is not None
        or not math.isclose(curve["log_a"], math.log10(2_000_000 * 80**3), rel_tol=0, abs_tol=1e-12)
        or curve.get("thickness_correction") is not None
        or any(point["scf"] != 1.2 for point in data["hot_spots"] + data.get("arc_distribution", []))):
        raise ValueError("benchmark requires recorded FAT 80, slope 3, 2 million cycles at 80 MPa, no endurance limit, SCF 1.2")


def _station_sums(rows: list[dict]) -> list[dict]:
    stations = {}
    for row in rows:
        key = row["case_id"], row["hot_spot"]
        stations.setdefault(key, []).append(row)
    sums = []
    for (case, name), contributions in sorted(stations.items()):
        if len({row["arc_length_m"] for row in contributions}) != 1:
            raise ValueError("same station coordinates required across sea states")
        dominant = _governing(contributions)
        sums.append({"case_id": case, "hot_spot": name,
                     "arc_length_m": dominant["arc_length_m"], "sea_state_id": dominant["sea_state_id"],
                     "damage_per_year": math.fsum(row["damage_per_year"] for row in contributions),
                     "contributions": contributions, "scope": SCOPE})
    return sums


def _resolution(delta: float | None, paired: list[dict], checks: dict | None) -> dict:
    record = {"status": "not resolved", "checks": checks,
              "reason": "G4 mesh-step and half-hour evidence not supplied"}
    if checks is None:
        return record
    validate_result(checks, "g4_checks")
    if checks["comparison_cases"] != [row["case_id"] for row in paired]:
        raise ValueError("G4 comparison cases differ from the governing wall pair")
    if delta is None:
        record["reason"] = "Percentage not established at zero reference damage"
        return record
    deviations = {name: abs(checks[name]["wall_delta_pct"] - delta) for name in ("mesh_step", "half_hour")}
    record["deviations_pct"] = deviations
    resolved = all(deviations[name] <= checks[name]["tolerance_pct"] for name in deviations)
    resolved = resolved and abs(delta) > max(deviations.values())
    record.update(status="resolved" if resolved else "not resolved",
                  reason="Wall change compared with recorded G4 tolerances and measured deviations")
    return record


def produce(results: Path, output: Path, *, g4_checks: dict | None = None) -> dict:
    """Validate all twelve cases, then write the draft 3 benchmark artifacts.

    An empty output directory is required to prevent stale removed outputs.
    Optional G4 evidence records checks on the same wall-percentage comparison.
    No engineering tolerance is invented when evidence is absent.
    """
    results, output = Path(results), Path(output)
    if results.resolve() == output.resolve() or results.resolve() in output.resolve().parents:
        raise ValueError("output must be outside the results tree")
    if output.exists() and any(output.iterdir()):
        raise ValueError("output directory must be empty; retain earlier outputs separately")
    records = _load(results)
    expected = {f"R{size}-{wall}-{config}" for size in ("08", "10", "12")
                for wall in ("W1", "W2") for config in ("SCR", "SLWR")}
    if {data["case_id"] for _, data, _ in records} != expected:
        raise ValueError("all twelve cases are required")
    hot_spots, inputs, sources = [], [], []
    for path, data, raw in records:
        _benchmark(data)
        hot_spots.extend(calculate(data))
        inputs.append(_input_row(data))
        sources.append({"export": path.relative_to(results).as_posix(),
                        "sha256": hashlib.sha256(raw).hexdigest(), "run": data["run"]})
    if any({row["hot_spot"] for row in hot_spots if row["case_id"] == case} != {"touchdown", "hang_off"}
           for case in expected):
        raise ValueError("both touchdown and hang_off hot spots required")
    sums = _station_sums(hot_spots)
    governing = _governing(sums)
    config_spots = {config: _governing([row for row in sums if row["case_id"].endswith(f"-{config}")])["hot_spot"]
                    for config in ("SCR", "SLWR")}
    size, _, config = governing["case_id"].split("-")
    paired = [_governing([row for row in sums if row["case_id"] == f"{size}-{wall}-{config}"])
              for wall in ("W1", "W2")]
    d1, d2 = (row["damage_per_year"] for row in paired)
    delta = 100 * (d2 - d1) / d1 if d1 else None
    if delta is not None and not math.isfinite(delta):
        raise ValueError("wall damage percentage exceeds numeric range")
    resolution = _resolution(delta, paired, g4_checks)
    values = {"V-FAT-GOV-CASE": governing["case_id"], "V-FAT-GOV-SS": governing["sea_state_id"],
              "V-FAT-DMAX": governing["damage_per_year"],
              "V-FAT-SCR-HOTSPOT": config_spots["SCR"], "V-FAT-SLWR-HOTSPOT": config_spots["SLWR"],
              "V-FAT-WT-DELTA": delta, "V-FAT-RESOLVED": resolution["status"]}
    comparison = {"definition": "100 * (W2 damage - W1 damage) / W1 damage; same size and configuration",
                  "reference_case": paired[0]["case_id"], "comparison_case": paired[1]["case_id"],
                  "reference_station": paired[0], "comparison_station": paired[1],
                  "reason": "Percentage not established at zero reference damage" if delta is None else ""}
    tokens = {"scope": SCOPE, "values": values, "units": {key: "%" if key == "V-FAT-WT-DELTA" else "-" for key in values},
              "wall_comparison": comparison, "resolution": resolution}
    output.mkdir(parents=True, exist_ok=True)
    limit = max(row["stress_range_MPa"] for row in hot_spots)
    artifacts = {f"FIG-FAT-RNG-{size}": _artifact(
        output, f"FIG-FAT-RNG-{size}", [row for row in hot_spots if row["case_id"].startswith(f"R{size}-")],
        f"NPS {int(size)} touchdown and hang-off ranges by sea state", limit) for size in ("08", "10", "12")}
    artifacts["FIG-FAT-HEAT-01"] = _artifact(output, "FIG-FAT-HEAT-01", hot_spots, "Annual benchmark damage by case and sea state")
    artifacts["TAB-FAT-01"] = _artifact(output, "TAB-FAT-01", hot_spots, "Stress ranges and annual benchmark damage")
    run_record = {"scope": SCOPE, "inputs": inputs, "g4_checks": g4_checks}
    _write_json(output / "run_record.json", run_record)
    manifest = {"schema_version": "riser-fatigue-screening-v2", "scope": SCOPE,
                "sources": sources, "hot_spots": hot_spots, "artifacts": artifacts,
                "station_sums": sums, "governing": governing, "values": values,
                "wall_comparison": comparison, "resolution": resolution,
                "run_record": "run_record.json",
                "run_record_sha256": hashlib.sha256((output / "run_record.json").read_bytes()).hexdigest(),
                "reciprocal_at_zero_damage": "not established"}
    _write_json(output / "tokens.json", tokens)
    _write_json(output / "manifest.json", manifest)
    return {"hot_spots": hot_spots, "cases": sums, "inputs": inputs, "manifest": manifest}


def main() -> None:
    """Read exported JSON and write SVG, CSV, manifest and value tokens."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--g4-checks", type=Path, help="Recorded mesh-step and half-hour comparison checks")
    args = parser.parse_args()
    try:
        checks = json.loads(args.g4_checks.read_text()) if args.g4_checks else None
        produce(args.results, args.output, g4_checks=checks)
    except ValueError as exc:
        parser.exit(2, f"Invalid fatigue exports: {exc}\n")


if __name__ == "__main__":
    main()
