# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build per-size strength artifacts from explicit, validated result exports.

No solver is imported. Legacy normalised stresses are retained as provenance;
U1 uses the supplied API RP 2RD equivalent stresses on both fibres instead.
"""

from __future__ import annotations

import argparse
from collections import defaultdict
import hashlib
import math
from pathlib import Path
import re
from typing import Any

from .plotting import render_artifact, render_table, row, write_json
from .results_exchange import load_result

# Study screening basis supplied with the output contract, not certification.
EXTREME_ALLOWABLE_MPA = 360.0
EXCURSION_SCREEN_DEG = 10.0
SIZES = ("08", "10", "12")
CASES = {f"R{s}-W{w}-{k}" for s in SIZES for w in (1, 2) for k in ("SCR", "SLWR")}


def _key(data: dict[str, Any]) -> tuple[Any, ...]:
    screen, load = data["screening"], data["load_case"]
    return (screen["condition"], screen["dynamic"], load["offset"]["x_m"], load["wave"]["direction_deg"])


def _u1(data: dict[str, Any]) -> list[tuple[str, str, int, float]]:
    screen = data["screening"]
    allowable = ((2/3)*screen["u1"]["smys_MPa"] if screen["condition"] == "functional"
                 else EXTREME_ALLOWABLE_MPA)
    return [(state, fibre, index, stress/allowable)
            for state, fibres in screen["u1"]["stress_MPa"].items() if fibres is not None
            for fibre, values in fibres.items() for index, stress in enumerate(values)]


def _tensions(data: dict[str, Any]) -> dict[str, list[float]]:
    return {"static": data["effective_tension_kN"], **{
        key: values for key, values in data["screening"]["tension_envelope_kN"].items() if values is not None}}


def _excursions(data: dict[str, Any]) -> dict[str, float]:
    screen, angles = data["screening"], data["top_angle_deg"]
    reference = screen["excursion_reference_deg"]
    ratios = {"static": abs(angles["static"]-reference)/screen["excursion_limit_deg"]}
    if screen["dynamic"]:
        ratios["dynamic"] = max(abs(angles[s]-reference) for s in ("dynamic_min", "dynamic_max"))/screen["excursion_limit_deg"]
    return ratios


def _provenance(data: dict[str, Any], source: str, digest: str) -> dict[str, Any]:
    return {"source_file": source, "input_sha256": data["run"]["input_sha256"], "result_sha256": digest}


def _mark(token: str, data: dict[str, Any], source: str, digest: str, series: str,
          x: float, y: float, x_unit: str, y_unit: str, *, panel: str,
          axis: str = "left", panel_y_title: str = "") -> dict[str, Any]:
    datum = row(token, data["case_id"], series, x, y, x_unit, y_unit, source)
    datum.update(panel=panel, axis=axis, panel_y_title=panel_y_title,
                 condition=data["screening"]["condition"], load_case_id=data["load_case"]["id"],
                 span_offset_m=data["load_case"]["offset"]["x_m"],
                 direction_deg=data["load_case"]["wave"]["direction_deg"],
                 **_provenance(data, source, digest))
    return datum


def _line(token: str, rows: list[dict[str, Any]], value: float, series: str, *,
          panel: str, axis: str = "left") -> None:
    selected = [r for r in rows if r["panel"] == panel and r["axis"] == axis]
    for x in (min(r["x"] for r in selected), max(r["x"] for r in selected)):
        datum = dict(selected[0])
        datum.update(case_id="", series=series, x=x, y=value, source_file="study screening basis",
                     input_sha256="", result_sha256="", load_case_id="")
        rows.append(datum)


def _load_batch(results: Path) -> tuple[list[tuple[dict[str, Any], str, str]], list[dict[str, Any]]]:
    paths = sorted(results.rglob("strength.json"))
    if not paths:
        raise ValueError("no strength.json exports found")
    records, sources, seen = [], [], set()
    environments: dict[tuple[Any, ...], Any] = {}
    for path in paths:
        data = load_result(path, "strength")
        if "screening" not in data:
            raise ValueError("per-size outputs require explicit screening inputs")
        case = data["case_id"]
        if case not in {path.parent.name, path.parent.parent.name}:
            raise ValueError("case directory mismatch")
        load, screen = data["load_case"], data["screening"]
        if (load["offset"]["y_m"] != 0 or load["offset"]["x_m"] not in {-90, 0, 90}
                or load["wave"].get("direction_deg") not in {0, 180}):
            raise ValueError("explicit span offset and screened wave direction required")
        if (screen["u1"]["smys_MPa"] != 450 or screen["excursion_limit_deg"] != EXCURSION_SCREEN_DEG):
            raise ValueError("screening material or excursion basis differs from study inputs")
        identity = (case, *_key(data))
        if identity in seen:
            raise ValueError("duplicate case/load condition export")
        seen.add(identity)
        environment = (load["wave"], load["current"])
        key = _key(data)
        if key in environments and environment != environments[key]:
            raise ValueError("paired cases have different environmental inputs")
        environments[key] = environment
        source = "results/" + path.relative_to(results).as_posix()
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        sources.append({"file": source, "sha256": digest, "record": data})
        if screen["qualified"]:
            records.append((data, source, digest))
    records.sort(key=lambda r: (r[0]["case_id"], _key(r[0])))
    if {d["case_id"] for d, _, _ in records} != CASES:
        raise ValueError("all twelve cases require qualified exports")
    coverage = [{_key(d) for d, _, _ in records if d["case_id"] == case} for case in sorted(CASES)]
    if any(keys != coverage[0] for keys in coverage):
        raise ValueError("load coverage differs between cases")
    dynamics = defaultdict(set)
    for data, _, _ in records:
        if data["screening"]["dynamic"]:
            dynamics[data["case_id"]].add(_key(data)[2:])
    if dynamics and (set(dynamics) != CASES or len({frozenset(v) for v in dynamics.values()}) != 1):
        raise ValueError("dynamic coverage differs between cases")
    for case in sorted(CASES):
        references = [d for d, _, _ in records if d["case_id"] == case and _key(d) == ("functional", False, 0, 0)]
        if len(references) != 1 or references[0]["screening"]["static_check"] is None:
            raise ValueError("each case requires one qualified nominal functional analytical check")
        reference = references[0]
        if any(d["screening"]["excursion_reference_deg"] != reference["top_angle_deg"]["static"]
               for d, _, _ in records if d["case_id"] == case):
            raise ValueError("excursion reference differs from nominal functional angle")
    return records, sources


def _percent(after: float, before: float) -> float | str:
    return 100*(after-before)/before if before != 0 else "not established"


def build_strength(results: Path, output: Path, *, reference_pair: str,
                   touchdown_window_m: float) -> dict[str, Any]:
    """Validate the entire batch, then write the per-size strength output set.

    The explicit reference size controls wall comparisons, and its wall controls
    the paired touchdown comparison. No native interface or standard is read.
    The touchdown window is retained for command-line compatibility/provenance;
    U3 is an exported zone stress range, not a newly sampled utilisation window.
    """
    results, output = Path(results), Path(output)
    if (not math.isfinite(touchdown_window_m) or touchdown_window_m <= 0
            or not re.fullmatch(r"R(?:08|10|12)-W[12]", reference_pair)):
        raise ValueError("explicit reference pair and positive touchdown window required")
    if any(output.glob("FIG-STR-0*")):
        raise ValueError("deprecated output files present; select a fresh destination")
    records, sources = _load_batch(results)
    artifacts, unresolved = [], []
    figures: dict[str, list[dict[str, Any]]] = defaultdict(list)
    for data, source, digest in records:
        size = data["case_id"][1:3]
        condition, dynamic, offset, direction = _key(data)
        prefix = f"{condition}:"
        profile_panel = f"Span {offset:.1f} m, direction {direction:.1f} deg"
        # Profiles share axes across both walls and configurations within each load panel.
        for state, values in _tensions(data).items():
            token = f"FIG-STR-TEFF-{size}"
            for arc, value in zip(data["arc_length_m"], values):
                figures[token].append(_mark(token, data, source, digest, prefix+state+(":history" if dynamic else ":equilibrium"),
                    arc, value, "m", "kN", panel=profile_panel))
        token = f"FIG-STR-U1-{size}"
        for state, fibre, index, value in _u1(data):
            figures[token].append(_mark(token, data, source, digest, prefix+state+":"+fibre+(":history" if dynamic else ":equilibrium"),
                data["arc_length_m"][index], value, "m", "-", panel=profile_panel))
        token = f"FIG-STR-HOT-{size}"
        values = {"static": data["hang_off_tension_kN"]["static"]}
        if dynamic:
            values.update(dynamic_min=data["screening"]["hang_off_dynamic_min_kN"],
                          dynamic_max=data["hang_off_tension_kN"]["dynamic_max"])
        for state, value in values.items():
            series = prefix+state+((":history" if dynamic else ":equilibrium") if state == "static" else "")
            figures[token].append(_mark(token, data, source, digest, series, offset,
                value, "m", "kN", panel=f"Direction {direction:.1f} deg"))
        token = f"FIG-STR-ANG-{size}"
        for state in (("static", "dynamic_min", "dynamic_max") if dynamic else ("static",)):
            series = prefix+state+((":history" if dynamic else ":equilibrium") if state == "static" else "")
            figures[token].append(_mark(token, data, source, digest, series, offset,
                data["top_angle_deg"][state], "m", "deg", panel=f"Direction {direction:.1f} deg"))
        for state, value in _excursions(data).items():
            series = prefix+state+":excursion"+((":history" if dynamic else ":equilibrium") if state == "static" else "")
            figures[token].append(_mark(token, data, source, digest, series, offset,
                value, "m", "-", panel=f"Direction {direction:.1f} deg", axis="right"))
        token = f"FIG-STR-TDR-{size}"
        figures[token].append(_mark(token, data, source, digest, prefix+("dynamic" if dynamic else "static"),
            offset, data["touchdown"]["stress_range_MPa"], "m", "MPa", panel=f"Direction {direction:.1f} deg"))
    # Paired ratios use the same wall, offset, direction, condition and analysis type.
    lookup = {(d["case_id"], _key(d)): (d, s, h) for d, s, h in records}
    for data, source, digest in records:
        if not data["case_id"].endswith("SLWR"):
            continue
        scr, scr_source, scr_digest = lookup[(data["case_id"].replace("SLWR", "SCR"), _key(data))]
        denominator = scr["touchdown"]["stress_range_MPa"]
        if denominator == 0:
            unresolved.append({"case_id": data["case_id"], "load_case_id": data["load_case"]["id"],
                               "reason": "zero SCR stress range; ratio not established"})
            continue
        condition, dynamic, offset, direction = _key(data)
        token = f"FIG-STR-TDR-{data['case_id'][1:3]}"
        figures[token].append(_mark(token, data, source+";"+scr_source, digest+";"+scr_digest,
            condition+":"+("dynamic" if dynamic else "static")+":SLWR/SCR", offset,
            data["touchdown"]["stress_range_MPa"]/denominator, "m", "-",
            panel=f"Direction {direction:.1f} deg", axis="right"))
        figures[token][-1]["input_sha256"] += ";"+scr["run"]["input_sha256"]
    summaries: dict[str, dict[str, Any]] = {}
    summary_token = "FIG-STR-SUM-01"
    for index, case in enumerate(sorted(CASES)):
        subset = [r for r in records if r[0]["case_id"] == case]
        candidates = [(value, d, s, h, state, fibre, station)
                      for d, s, h in subset for state, fibre, station, value in _u1(d)]
        maximum = max(candidates, key=lambda c: c[0])
        minimum = min(((v, d, s, h) for d, s, h in subset for values in _tensions(d).values() for v in values), key=lambda c: c[0])
        # Explicit keys keep ties independent of dictionary ordering/comparison.
        tdr = max(((d["touchdown"]["stress_range_MPa"], d, s, h) for d, s, h in subset), key=lambda c: c[0])
        excursion = max(((max(_excursions(d).values()), d, s, h) for d, s, h in subset), key=lambda c: c[0])
        hot = max(((max(v[0] for v in _tensions(d).values()), d, s, h) for d, s, h in subset), key=lambda c: c[0])
        summaries[case] = {"u1": maximum, "teff": minimum, "tdr": tdr, "excursion": excursion, "hot": hot}
        for measure, title, unit in [("u1", "Maximum U1", "-"), ("teff", "Minimum effective tension", "kN"),
                                     ("tdr", "Maximum touchdown stress range", "MPa"),
                                     ("excursion", "Maximum excursion ratio", "-"),
                                     ("hot", "Maximum hang-off effective tension", "kN")]:
            value, d, s, h, *_ = summaries[case][measure]
            figures[summary_token].append(_mark(summary_token, d, s, h, measure, index, value,
                "case index", unit, panel=title, panel_y_title=f"{title} ({unit})"))
    counts = {"records": len(sources), "qualified": len(records),
              "dynamic": sum(d["screening"]["dynamic"] for d, _, _ in records),
              "static": sum(not d["screening"]["dynamic"] for d, _, _ in records), "cases": len(CASES)}
    synthetic = all("synthetic" in d["software"]["name"].casefold() for d, _, _ in records)
    note = ("SYNTHETIC fixture results. " if synthetic else "Exported qualified records. ")
    note += "Regular-wave screening proxy; dynamic points only at recorded offsets."
    for token, rows in sorted(figures.items()):
        measure = token.split("-")[2]
        specs = {"TEFF": ("Effective tension", "Arc length from hang-off (m)", "Effective tension (kN)", 0),
                 "U1": ("U1: API RP 2RD stress / allowable", "Arc length from hang-off (m)", "U1 (-)", 1),
                 "HOT": ("Hang-off effective tension", "Span offset (m)", "Effective tension (kN)", 0),
                 "ANG": ("Top angle and excursion ratio", "Span offset (m)", "Angle from vertical (deg)", None),
                 "TDR": ("Touchdown stress range U3 and SLWR/SCR ratio", "Span offset (m)", "Stress range (MPa)", None),
                 "SUM": ("Strength screening summary", "Case", "", None)}
        title, x_title, y_title, limit = specs[measure]
        for panel in dict.fromkeys(r["panel"] for r in rows):
            if limit is not None:
                _line(token, rows, limit, "screen" if limit else "zero", panel=panel)
            if measure == "ANG":
                _line(token, rows, 1, "excursion screen = 1.0", panel=panel, axis="right")
            if measure == "TDR" and any(r["panel"] == panel and r["axis"] == "right" for r in rows):
                _line(token, rows, 1, "equal stress range ratio = 1.0", panel=panel, axis="right")
            if measure == "SUM" and panel in {"Maximum U1", "Minimum effective tension", "Maximum excursion ratio"}:
                _line(token, rows, 0 if panel == "Minimum effective tension" else 1,
                      "zero" if panel == "Minimum effective tension" else "screen", panel=panel)
        artifacts.append(render_artifact(output, token, rows, title=title,
            x_title=x_title, y_title=y_title, categorical=measure == "SUM",
            note=note+(f" Cases: {counts['cases']}; static: {counts['static']}; dynamic: {counts['dynamic']}; qualified: {counts['qualified']}." if measure == "SUM" else "")))
    tables = _tables(records)
    for token, rows in tables.items():
        artifacts.append(render_table(output, token, rows,
            title="Nominal static analytical check" if token == "TAB-STA-01" else "Exported API RP 1111 combined-loading checks"))
    tokens = _values(records, summaries, reference_pair)
    tokens += [{"id": a["id"], "value": a["file"], "unit": "-",
                "source_file": sorted({r["source_file"] for r in a["rows"] if r["case_id"]})} for a in artifacts]
    manifest = {"artifacts": sorted(artifacts, key=lambda a: a["id"]), "sources": sources,
                "reference_pair": reference_pair, "touchdown_window_m": touchdown_window_m,
                "run_counts": counts, "unresolved_ratios": unresolved,
                "u1_basis": {"functional": "API RP 2RD equivalent stress / ((2/3) SMYS), both fibres",
                             "extreme": "API RP 2RD equivalent stress / 360 MPa, both fibres"}}
    write_json(output / "tokens.json", sorted(tokens, key=lambda t: t["id"]))
    write_json(output / "manifest.json", manifest)
    return manifest


def _tables(records: list[tuple[dict[str, Any], str, str]]) -> dict[str, list[dict[str, Any]]]:
    tables: dict[str, list[dict[str, Any]]] = {"TAB-STA-01": [], "TAB-STR-1111-01": []}
    for data, source, digest in records:
        screen = data["screening"]
        provenance = _provenance(data, source, digest)
        if _key(data) == ("functional", False, 0, 0):
            check = screen["static_check"]
            for measure, key, software, unit in [
                ("top_tension", "top_tension_kN", data["hang_off_tension_kN"]["static"], "kN"),
                ("top_angle", "top_angle_deg", data["top_angle_deg"]["static"], "deg"),
                ("touchdown_position", "touchdown_x_m", check["software_touchdown_x_m"], "m")]:
                analytical = check["analytical"][key]
                tables["TAB-STA-01"].append(dict(case_id=data["case_id"], measure=measure,
                    software=software, analytical=analytical, difference=software-analytical,
                    difference_pct=_percent(software, analytical), unit=unit, basis=check["basis"], **provenance))
        combined = screen["api_rp_1111"]
        checks = combined["checks"] or [{"arc_length_m": "not in scope", "utilisation": "not in scope", "check": "not in scope"}]
        for check in checks:
            tables["TAB-STR-1111-01"].append(dict(case_id=data["case_id"], load_case_id=data["load_case"]["id"],
                span_offset_m=data["load_case"]["offset"]["x_m"], direction_deg=data["load_case"]["wave"]["direction_deg"],
                analysis="dynamic" if screen["dynamic"] else "static", in_scope=combined["in_scope"],
                **check, method=combined["method"], reason=combined["reason"], **provenance))
    return tables


def _values(records: list[tuple[dict[str, Any], str, str]], summaries: dict[str, Any], reference: str) -> list[dict[str, Any]]:
    tokens = []
    all_sources = [s for _, s, _ in records]
    def add(name: str, value: Any, unit: str = "-", sources: Any = None) -> None:
        tokens.append({"id": name, "value": value, "unit": unit, "source_file": all_sources if sources is None else sources})
    governing = max(sorted(summaries), key=lambda case: summaries[case]["u1"][0])
    value, data, source, _, state, fibre, station = summaries[governing]["u1"]
    add("V-STR-GOV-CASE", governing, sources=source)
    add("V-STR-UMAX-GOV", value, sources=source)
    add("V-STR-GOV-LOC", f"{data['arc_length_m'][station]:.1f} m from hang-off, {fibre} fibre, {data['screening']['condition']}:{state}", sources=source)
    for kind in ("SCR", "SLWR"):
        case = max((c for c in sorted(summaries) if c.endswith("-"+kind)), key=lambda c: summaries[c]["u1"][0])
        add("V-STR-UMAX-"+kind, summaries[case]["u1"][0], sources=summaries[case]["u1"][2])
    size = reference.split("-")[0]
    wall = {w: max(summaries[f"{size}-{w}-{k}"]["u1"][0] for k in ("SCR", "SLWR")) for w in ("W1", "W2")}
    add("V-STR-WT-DELTA", _percent(wall["W2"], wall["W1"]), "%")
    add("V-STR-TEFF-MIN", min(s["teff"][0] for s in summaries.values()), "kN")
    add("V-STR-COMP-COUNT", sum(s["teff"][0] < 0 for s in summaries.values()))
    add("V-STR-UANG-MAX", max(s["excursion"][0] for s in summaries.values()))
    ranges = {kind: [d["touchdown"]["stress_range_MPa"] for d, _, _ in records
                    if d["case_id"] == reference+"-"+kind and d["screening"]["dynamic"]] for kind in ("SCR", "SLWR")}
    delta = _percent(max(ranges["SLWR"]), max(ranges["SCR"])) if all(ranges.values()) else "not established"
    add("V-STR-TDR-DELTA-REF", delta, "%")
    offsets = {d["load_case"]["offset"]["x_m"] for d, _, _ in records if d["screening"]["dynamic"]}
    directions = {d["load_case"]["wave"]["direction_deg"] for d, _, _ in records if d["screening"]["dynamic"]}
    if offsets == {-90, 0, 90}:
        basis = "near, nominal and far offsets"
    elif offsets == {0}:
        basis = "nominal offset, " + ("one screened direction" if len(directions) == 1 else "two screened directions")
    else:
        basis = "not established"
    add("V-STR-DYN-BASIS", basis)
    versions = sorted({d["software"]["name"]+" "+d["software"]["version"] for d, _, _ in records})
    add("V-SW-VERSION", "; ".join(versions))
    return tokens


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
