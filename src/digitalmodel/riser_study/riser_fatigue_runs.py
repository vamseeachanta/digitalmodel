# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build a screening sensitivity over 3 sea states from fatigue exports.

Method basis: the supplied results-exchange contract requires linear Miner
summation and an explicitly recorded S-N curve. For each range bin,
N = 10**log_a / (range_MPa * SCF * thickness_factor)**m;
D/year = occurrence * sum(cycles_per_year / N), and life = 1 / D.
Annual cycles are BEFORE sea-state occurrence; occurrence is applied once.
The three states are compared separately, never normalized or summed into a
full-scatter life. A histogram's plotted range is its largest nominal range
at the hot spot with greatest damage, not an equivalent damage range.

Thickness_factor is 1 unless the curve carries a correction, in which case
it is (thickness_mm/reference_thickness_mm)**exponent, with no implicit
clipping or reference thickness. Exporters must record that convention.
No safety factor, mean stress correction or other fatigue mechanism is added.

Hand calculation (ASSUMED synthetic example): m=3, log_a=12, SCF=1,
range=100 MPa and 1,000,000 annual cycles give N=1,000,000 cycles.
At occurrence=0.2, D=0.2/year and life=5 years. For 100,000 annual cycles
at each of 100 and 200 MPa, D=0.2*(0.1+0.8)=0.18/year.

V-FAT-WT-FACTOR = minimum life of W2 / minimum life of W1, for the
nominal size AND configuration of the globally governing case. Each minimum
is taken over that case's three states and hot spots; the paired states may
differ. W1/W2 are matrix labels, not an inferred physical thickness order.
The manifest records both cases and their governing states. The global
minimum and configuration-specific hot spots use maximum annual damage;
exact ties use case id, sea-state id, then hot-spot name in lexical order.
Zero damage has unbounded life, represented as null, never a finite number.

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

SCOPE = "Screening sensitivity over 3 sea states; not a full fatigue analysis"
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
        life = 1 / damage if damage else None
        peak = max(item["stress_range_MPa"] for item in bins)
        effective = peak * point["scf"] * factor
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("fatigue calculation exceeds numeric range") from exc
    if not all(math.isfinite(value) for value in (factor, damage, effective)) or (
        life is not None and not math.isfinite(life)
    ):
        raise ValueError("fatigue calculation is nonfinite")
    return {"hot_spot": point.get("name", "arc_point"), "arc_length_m": point["arc_length_m"],
            "stress_range_MPa": peak, "effective_stress_range_MPa": effective,
            "cycles_per_year": point["cycles_per_year"], "scf": point["scf"],
            "thickness_factor": factor, "damage_per_year": damage, "life_years": life}


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
    for field in ("damage_per_year", "life_years"):
        expected, recorded = worst[field], data[field]
        if (expected is None) != (recorded is None) or (
            expected is not None and not math.isclose(expected, recorded, rel_tol=RECORD_REL_TOL, abs_tol=0)
        ):
            raise ValueError(f"recorded {field} disagrees with calculated hot-spot maximum")
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
    # Human-readable calculated comparisons use one decimal; CSV stays unrounded.
    if key == "life_years" and not value:
        return "unbounded"
    calculated = {"stress_range_MPa", "effective_stress_range_MPa", "damage_per_year",
                  "life_years", "arc_length_m", "thickness_factor", "scf"}
    if key in calculated and value:
        number = float(value)
        return f"{number:.1e}" if number and (abs(number) < 0.1 or abs(number) >= 100000) else f"{number:.1f}"
    return value


def _svg(token: str, rows: list[dict], title: str, x_field: str | None,
         y_field: str | None, log_y: bool, reason: str) -> tuple[str, dict]:
    # Display geometry only: pixel sizes, padding and tick count are layout choices.
    width = max(1000, len(rows) * 85) if y_field else 1400
    bottom, top = 410, 130
    table_lines = [
        [line for key, value in row.items()
         for line in textwrap.wrap(f"{key}: {_table_display(key, value)}", width=160, break_on_hyphens=False)]
        for row in rows
    ] if not y_field else []
    height = 760 if y_field else max(240, 150 + sum(len(lines) + 1 for lines in table_lines) * 20)
    root = ET.Element("svg", xmlns="http://www.w3.org/2000/svg", width=str(width),
                      height=str(height), viewBox=f"0 0 {width} {height}", role="img",
                      **{"font-family": "sans-serif", "font-size": "13"})
    ET.SubElement(root, "title").text = f"{token}: {title}. {SCOPE}"
    ET.SubElement(root, "metadata").text = _json(rows)

    def text(x, y, value, **attrs):
        ET.SubElement(root, "text", x=str(x), y=str(y), **attrs).text = str(value)

    text(20, 30, f"{token}: {title}")
    text(20, 55, SCOPE)
    if rows and reason:
        text(20, 78, reason)
    if not rows:
        text(20, 100, reason)
    scale = {"plot_bottom": bottom, "y_min": 0.0, "y_scale": 1.0, "log_y": log_y}
    if y_field:
        values = [float(row[y_field]) for row in rows if row[y_field] != ""]
        transformed = [math.log10(value) if log_y else value for value in values]
        low = min(transformed, default=0.0) if log_y else 0.0
        high = max(transformed, default=1.0)
        if high == low:
            high = low + 1.0
        y_scale = (bottom - top) / (high - low)
        scale.update(y_min=low, y_scale=y_scale)
        ET.SubElement(root, "path", d=f"M 100 {top} V {bottom} H {width - 30}",
                      fill="none", stroke="black")
        for tick in range(5):
            v = low + (high - low) * tick / 4
            label = 10**v if log_y else v
            text(5, bottom - tick * (bottom - top) / 4, f"{label:.1f}")
        text(100, 105, title + (" (logarithmic scale)" if log_y else ""))
        text(100, 575, "Arc length from hang-off (m)" if x_field else "Case / sea state")
        x_values = [float(row[x_field]) for row in rows] if x_field else []
        x_min, x_max = (min(x_values), max(x_values)) if x_values else (0, 1)
        # Display-only palette and line styles identify overlapping case traces.
        palette = ("#165a91", "#ae3c18", "#287943", "#7651a8", "#a56a00", "#007f85")
        group_field = "case_id" if x_field else "sea_state_id"
        groups = sorted({row[group_field] for row in rows})
        traces = {key: [] for key in groups}
        for index, row in enumerate(rows):
            attrs = {"data-row": str(index), "data-values": _json(row)}
            x = (100 + (float(row[x_field]) - x_min) / (x_max - x_min or 1) * (width - 150)
                 if x_field else 130 + index * (width - 170) / max(1, len(rows)))
            label = f"{row['case_id']} / {row['sea_state_id']}"
            group = groups.index(row[group_field])
            color = palette[group % len(palette)]
            if row[y_field] == "":
                text(x, top, "unbounded", **attrs)
            else:
                value = float(row[y_field])
                v = math.log10(value) if log_y else value
                y = bottom - (v - low) * y_scale
                mark = ET.SubElement(root, "circle", cx=str(x), cy=str(y),
                                     r="4", fill=color, **attrs)
                ET.SubElement(mark, "title").text = f"{label}: {value:.1f}"
                traces[row[group_field]].append((x, y))
            if not x_field:
                text(x, bottom + 20, label, transform=f"rotate(55 {x} {bottom + 20})")
        if x_field:
            for tick in range(5):
                text(100 + tick * (width - 150) / 4, bottom + 20,
                     f"{x_min + tick * (x_max - x_min) / 4:.1f}")
            for index, (key, points) in enumerate(traces.items()):
                ET.SubElement(root, "polyline", points=" ".join(f"{x},{y}" for x, y in points),
                              fill="none", stroke=palette[index % len(palette)],
                              **{"stroke-dasharray": "6 3" if index >= len(palette) else "none",
                                 "data-case": key})
        for index, group in enumerate(groups):
            x, y = 100 + (index % 6) * 145, 620 + (index // 6) * 24
            ET.SubElement(root, "path", d=f"M {x} {y-4} h 16", stroke=palette[index % len(palette)],
                          **{"stroke-dasharray": "6 3" if x_field and index >= len(palette) else "none"})
            text(x + 20, y, group)
    else:
        # Tables preserve every field in metadata and visible wrapped record blocks.
        y = 100
        for index, (row, lines) in enumerate(zip(rows, table_lines)):
            block = ET.SubElement(root, "g", **{"data-row": str(index), "data-values": _json(row)})
            for line in lines:
                ET.SubElement(block, "text", x="20", y=str(y)).text = line
                y += 20
            y += 20
    return ET.tostring(root, encoding="unicode") + "\n", scale


def _artifact(out: Path, token: str, rows: list[dict], fields: list[str], title: str,
              y_field: str | None = None, x_field: str | None = None,
              log_y: bool = False, reason: str = "") -> dict:
    csv_payload, serialized = _csv_rows(rows, fields)
    svg_payload, scale = _svg(token, serialized, title, x_field, y_field, log_y, reason)
    csv_name, svg_name = f"{token}.csv", f"{token}.svg"
    (out / csv_name).write_text(csv_payload, encoding="utf-8")
    (out / svg_name).write_text(svg_payload, encoding="utf-8")
    return {"csv": csv_name, "svg": svg_name, "rows": serialized,
            "csv_sha256": hashlib.sha256(csv_payload.encode()).hexdigest(),
            "svg_sha256": hashlib.sha256(svg_payload.encode()).hexdigest(),
            "title": title, "scope": SCOPE, "y_field": y_field, "x_field": x_field,
            "status": "available" if rows else "unavailable", "reason": reason, **scale}


def produce(results: Path, output: Path) -> dict:
    """Validate a common three-state set and write token artifacts.

    Each case must retain the same hot spots across states. Both SCR and SLWR
    and the paired walls of the governing size/configuration are required to
    resolve the requested value tokens. Other sizes may be processed as subsets.
    Validation and calculations finish before output files are created.
    """
    results, output = Path(results), Path(output)
    if results.resolve() == output.resolve() or results.resolve() in output.resolve().parents:
        raise ValueError("output must be outside the results tree")
    records = _load(results)
    hot_spots, cases, inputs, sources = [], [], [], []
    for path, data, raw in records:
        rows = calculate(data)
        hot_spots.extend(rows)
        cases.append(_governing(rows))
        inputs.append(_input_row(data))
        sources.append({"export": path.relative_to(results).as_posix(),
                        "sha256": hashlib.sha256(raw).hexdigest(), "run": data["run"]})
    governing = _governing(hot_spots)
    config_spots = {}
    for config in ("SCR", "SLWR"):
        subset = [row for row in hot_spots if row["case_id"].endswith(f"-{config}")]
        if not subset:
            raise ValueError("both SCR and SLWR exports are required")
        config_spots[config] = _governing(subset)["hot_spot"]
    size, _, config = governing["case_id"].split("-")
    paired = []
    for wall in ("W1", "W2"):
        subset = [row for row in cases if row["case_id"] == f"{size}-{wall}-{config}"]
        if not subset:
            raise ValueError("governing size/configuration requires both wall cases")
        paired.append(_governing(subset))
    life1, life2 = paired[0]["life_years"], paired[1]["life_years"]
    factor = life2 / life1 if life1 is not None and life2 is not None else None
    if factor is not None and (not math.isfinite(factor) or factor <= 0):
        raise ValueError("wall life ratio exceeds numeric range")
    values = {"V-FAT-GOV-CASE": governing["case_id"], "V-FAT-GOV-SS": governing["sea_state_id"],
              "V-FAT-LMIN": governing["life_years"],
              "V-FAT-SCR-HOTSPOT": config_spots["SCR"], "V-FAT-SLWR-HOTSPOT": config_spots["SLWR"],
              "V-FAT-WT-FACTOR": factor}
    arcs, missing = [], []
    for _, data, _ in records:
        if data["sea_state"]["id"] != governing["sea_state_id"]:
            continue
        if "arc_distribution" not in data:
            missing.append(data["case_id"])
        for point in data.get("arc_distribution", []):
            arcs.append({"case_id": data["case_id"], "sea_state_id": governing["sea_state_id"], "scope": SCOPE,
                         **_point_result(point, data["sn_curve"], data["sea_state"]["occurrence"])})
    # No sparse hot-spot interpolation is presented as an arc distribution.
    reason = "Arc distribution not exported" + (" for: " + ", ".join(missing) if missing else "")
    tokens = {"scope": SCOPE, "values": values,
              "wall_comparison": {"definition": "W2 minimum life / W1 minimum life; same size and configuration",
                                  "numerator": paired[1]["case_id"], "denominator": paired[0]["case_id"],
                                  "numerator_sea_state": paired[1]["sea_state_id"],
                                  "denominator_sea_state": paired[0]["sea_state_id"],
                                  "reason": "unbounded life in pair" if factor is None else ""}}
    output.mkdir(parents=True, exist_ok=True)
    fields = list(cases[0])
    artifacts = {
        "FIG-FAT-01": _artifact(output, "FIG-FAT-01", cases, fields, "Nominal stress range (MPa)", "stress_range_MPa"),
        "FIG-FAT-02": _artifact(output, "FIG-FAT-02", cases, fields, "Fatigue life (years)", "life_years", log_y=True),
        "FIG-FAT-03": _artifact(output, "FIG-FAT-03", arcs, fields, "Damage per year (-)", "damage_per_year",
                                "arc_length_m", reason=reason if missing else ""),
        "TAB-FAT-01": _artifact(output, "TAB-FAT-01", cases, fields, "Stress range and life per case and sea state"),
        "TAB-FAT-IN-01": _artifact(output, "TAB-FAT-IN-01", inputs, list(inputs[0]), "Recorded fatigue inputs"),
    }
    if arcs and missing:
        artifacts["FIG-FAT-03"]["status"] = "partial"
    manifest = {"schema_version": "riser-fatigue-screening-v1", "scope": SCOPE,
                "sources": sources, "hot_spots": hot_spots, "artifacts": artifacts,
                "governing": governing, "missing_arc_distribution": missing,
                "values": values, "wall_comparison": tokens["wall_comparison"]}
    _write_json(output / "tokens.json", tokens)
    _write_json(output / "manifest.json", manifest)
    return {"hot_spots": hot_spots, "cases": cases, "inputs": inputs, "manifest": manifest}


def main() -> None:
    """Read exported JSON and write SVG, CSV, manifest and value tokens."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("results", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    try:
        produce(args.results, args.output)
    except ValueError as exc:
        parser.exit(2, f"Invalid fatigue exports: {exc}\n")


if __name__ == "__main__":
    main()
