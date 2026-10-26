# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic exchange and hand-count checks; no solver is imported."""

import copy
import csv
import hashlib
import json
import subprocess
import sys
from pathlib import Path

import pytest

from digitalmodel.riser_study.decision_chart import build_chart, generate
from digitalmodel.riser_study import decision_chart
from digitalmodel.riser_study.results_exchange import load_json, validate_result


# ASSUMED synthetic arithmetic, not engineering results. Tuples contain U1 and
# three annual damages at each of two stations. Sum each station before max.
HAND = {
    "R08-W1-SCR": (0.8, [0.01, 0.02, 0.03], [0.03, 0.01, 0.01], 0.06),
    "R08-W1-SLWR": (0.6, [0.01, 0.01, 0.01], [0.02, 0.02, 0.01], 0.05),
    "R08-W2-SCR": (1.1, [0.01, 0.01, 0.01], [0.01, 0.01, 0.02], 0.04),
    "R08-W2-SLWR": (1.0, [0.01, 0.01, 0.01], [0.01, 0.01, 0.01], 0.03),
    "R10-W1-SCR": (0.9, [0.01, 0.02, 0.01], [0.01, 0.01, 0.01], 0.04),
    "R10-W1-SLWR": (1.2, [0.01, 0.01, 0.01], [0.01, 0.01, 0.01], 0.03),
    "R10-W2-SCR": (0.7, [0.02, 0.01, 0.02], [0.01, 0.01, 0.01], 0.05),
    "R10-W2-SLWR": (1.3, [0.02, 0.02, 0.02], [0.01, 0.01, 0.01], 0.06),
    "R12-W1-SCR": (0.5, [0.01, 0.01, 0.01], [0.01, 0.01, 0.01], 0.03),
    "R12-W1-SLWR": (0.8, [0.02, 0.02, 0.02], [0.01, 0.01, 0.01], 0.06),
    "R12-W2-SCR": (1.1, [0.01, 0.01, 0.01], [0.01, 0.01, 0.01], 0.03),
    "R12-W2-SLWR": (0.4, [0.03, 0.03, 0.03], [0.01, 0.01, 0.01], 0.09),
}
FIXTURES = Path(__file__).parent / "fixtures"
D3_IDS = {"FIG-DEC-01", "TAB-RES-01", "V-DEC-COUNT-STR", "V-ANS-OD1",
          "V-ANS-OD2", "V-ANS-OD3", "V-RUN-ATTEMPTED", "V-RUN-COMPLETED",
          "V-RUN-QUALIFIED", "V-PHB-STATUS"}


def write_json(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, allow_nan=False), encoding="utf-8")


@pytest.fixture
def exchange(tmp_path):
    """Record annual damages explicitly; no reciprocal life is used."""
    root = Path(__file__).resolve().parents[2]
    benchmark = load_json(root / "docs/benchmarks/catenary_closed_form/benchmark.json")
    c1 = benchmark["cases"][0]["expected"]
    runs = []

    def run(identifier):
        record = {"id": identifier, "phase": "A", "status": "completed", "qualified": True}
        runs.append(record)
        return {**record, "input_sha256": hashlib.sha256(b"synthetic input").hexdigest(),
                "result_file": "synthetic.json", "date": "2026-10-25"}

    for case, (util, touchdown, hang_off, _) in HAND.items():
        strength = {
            "case_id": case, "software": {"name": "synthetic", "version": "fixture"},
            "load_case": {"id": "synthetic-strength", "wave": {}, "current": {}, "offset": {}},
            "arc_length_m": [0, 100],
            "effective_tension_kN": [c1["top_tension_kN"], 50],
            "bending_moment_kNm": [0, 10], "von_mises_utilisation": [util / 2, util],
            "utilisation_basis": "U1",
            "method": "ASSUMED synthetic U1; no design check",
            "allowable": {"value": 100, "unit": "MPa", "basis_label": "ASSUMED",
                          "source": "synthetic arithmetic example"},
            "max_utilisation": {"value": util, "arc_length_m": 100, "location": "touchdown"},
            "touchdown": {"arc_length_m": 100, "utilisation": util, "stress_range_MPa": 10},
            "top_angle_deg": {"static": 10, "dynamic_min": 8, "dynamic_max": 12},
            "hang_off_tension_kN": {"static": c1["top_tension_kN"], "dynamic_max": 600},
            "run": run(f"{case}-strength"),
        }
        write_json(tmp_path / "results" / case / "strength.json", strength)
        for state, td, ho in zip(("F03", "F04", "F05"), touchdown, hang_off):
            fatigue = {
                "case_id": case,
                "sea_state": {"id": state, "Hs_m": 2, "Tp_s": 8,
                              "direction_deg": 0, "occurrence": 0.2},
                "hot_spots": [
                    {"name": name, "arc_length_m": arc, "stress_range_MPa": 10,
                     "cycles_per_year": 1000, "scf": 1, "damage_per_year": damage}
                    for name, arc, damage in (("touchdown", 100, td), ("hang_off", 0, ho))],
                "sn_curve": {"name": "synthetic", "source": "ASSUMED arithmetic example",
                             "m": 3, "log_a": 12, "thickness_correction": 1},
                "damage_per_year": max(td, ho), "run": run(f"{case}-{state}"),
            }
            write_json(tmp_path / "results" / case / f"fatigue-{state}.json", fatigue)
    manifest = {"matrix_version": 1, "basis_label": "synthetic", "fatigue_sea_state_ids": ["F03", "F04", "F05"],
                "cases": [{"case_id": case} for case in HAND], "runs": runs}
    write_json(tmp_path / "cases.json", manifest)
    write_json(tmp_path / "fatigue-tokens.json",
               {"V-FAT-RESOLVED": {"kind": "VALUE", "value": "resolved", "unit": "-"}})
    return tmp_path


def run_generation(root, criteria=None):
    return generate(root / "results", root / "cases.json",
                    criteria or FIXTURES / "criteria.json", root / "out",
                    root / "fatigue-tokens.json")


def test_every_point_matches_csv_manifest_and_hand_rows(exchange, monkeypatch):
    plotted = []

    def capture(rows, criteria):
        fig, ax = build_chart(rows, criteria)
        plotted.extend(tuple(point) for collection in ax.collections
                       for point in collection.get_offsets())
        return fig, ax

    monkeypatch.setattr(decision_chart, "build_chart", capture)
    manifest = run_generation(exchange)
    rows = list(csv.DictReader((exchange / "out/FIG-DEC-01.csv").open()))
    assert rows == list(csv.DictReader((exchange / "out/TAB-RES-01.csv").open()))
    fig, ax = build_chart(manifest["rows"], manifest["criteria"])
    points = [tuple(point) for collection in ax.collections for point in collection.get_offsets()]
    assert len(points) == len(rows) == len(HAND)
    assert sorted(plotted) == sorted(points)
    for csv_row, row in zip(rows, manifest["rows"]):
        util, _, _, damage = HAND[row["case_id"]]
        assert float(csv_row["max_utilisation"]) == row["max_utilisation"] == util
        assert float(csv_row["annual_benchmark_damage"]) == row["annual_benchmark_damage"]
        assert row["annual_benchmark_damage"] == pytest.approx(damage)
        assert (util, row["annual_benchmark_damage"]) in points
        assert row["meets_strength"] is (util <= 1)
        assert row["max_utilisation_location"] == "touchdown"
    assert sorted(t.get_text() for t in ax.texts) == sorted(HAND)
    fig.canvas.draw()
    boxes = [label.get_window_extent(fig.canvas.get_renderer()) for label in ax.texts]
    assert all(not a.overlaps(b) for i, a in enumerate(boxes) for b in boxes[i + 1:])
    assert ax.get_yscale() == "log"
    assert len(ax.collections) == 6  # One colour per size, one marker per configuration.
    assert len(ax.patches) == 0
    assert len(ax.lines) == 7  # Six wall links, exactly one screening line.
    assert list(ax.lines[-1].get_xdata()) == [1, 1]
    assert all(line.get_linestyle() != ":" for line in ax.lines)
    for size in ("08", "10", "12"):
        for configuration in ("SCR", "SLWR"):
            pair = [r for r in manifest["rows"]
                    if r["size"] == size and r["configuration"] == configuration]
            assert any(list(line.get_xdata()) == [r["max_utilisation"] for r in pair]
                       and list(line.get_ydata()) == [r["annual_benchmark_damage"] for r in pair]
                       for line in ax.lines[:-1])
    assert manifest["count_strength"] == 8  # Hand enumeration, inclusive at U1 = 1.
    tokens = load_json(exchange / "out/tokens.json")
    assert set(tokens) == D3_IDS
    assert tokens["V-DEC-COUNT-STR"]["value"] == 8
    assert [tokens[f"V-ANS-OD{i}"]["value"] for i in (1, 2, 3)] == ["SLWR W2", "SCR W1", "SCR W1"]
    assert [tokens[f"V-RUN-{k}"]["value"] for k in ("ATTEMPTED", "COMPLETED", "QUALIFIED")] == [48, 48, 48]
    assert tokens["V-PHB-STATUS"]["value"] == "not yet done"
    assert {p.name for p in (exchange / "out").iterdir()} == {
        "FIG-DEC-01.svg", "FIG-DEC-01.csv", "TAB-RES-01.md", "TAB-RES-01.csv", "tokens.json", "manifest.json"}
    for token in tokens.values():
        if "path" in token:
            assert (exchange / "out" / token["path"]).is_file()
        assert "source_file" in token
    svg = (exchange / "out/FIG-DEC-01.svg").read_text()
    assert "Annual benchmark damage" in svg
    assert "not service life" in svg
    assert "fatigue target" not in svg.lower()
    for source in manifest["inputs"]:
        path = exchange / source["path"]
        if source["role"] == "criteria":
            path = FIXTURES / "criteria.json"
        assert hashlib.sha256(path.read_bytes()).hexdigest() == source["sha256"]
    for artifact, digest in manifest["artifacts"].items():
        assert hashlib.sha256((exchange / "out" / artifact).read_bytes()).hexdigest() == digest
    fig.clear()


@pytest.mark.parametrize("mutation", ["limit", "fatigue"])
def test_only_u1_screen_is_accepted(exchange, mutation):
    criteria = load_json(FIXTURES / "criteria.json")
    if mutation == "limit":
        criteria["strength"]["value"] = 0.6
    else:
        criteria["fatigue"] = {"value": 20, "unit": "years"}
    write_json(exchange / "invalid.json", criteria)
    with pytest.raises(ValueError):
        run_generation(exchange, exchange / "invalid.json")
    assert not (exchange / "out").exists()


@pytest.mark.parametrize("damage", ["missing-case", "missing-state", "extra-state", "wrong-case",
                                   "wrong-state", "duplicate-case", "duplicate-state", "bad-criteria",
                                   "orphan-case", "inconsistent-state"])
def test_incomplete_or_ambiguous_exchange_is_rejected(exchange, damage):
    case_dir = exchange / "results/R08-W1-SCR"
    if damage == "missing-case":
        (case_dir / "strength.json").rename(case_dir / "strength.absent")
    elif damage == "missing-state":
        (case_dir / "fatigue-F03.json").rename(case_dir / "fatigue.absent")
    elif damage == "extra-state":
        write_json(case_dir / "fatigue-F06.json", load_json(case_dir / "fatigue-F03.json"))
    elif damage == "orphan-case":
        write_json(exchange / "results/R08-W3-SCR/fatigue-F03.json",
                   load_json(case_dir / "fatigue-F03.json"))
    elif damage == "inconsistent-state":
        data = load_json(case_dir / "fatigue-F03.json")
        data["sea_state"]["Hs_m"] = 3
        write_json(case_dir / "fatigue-F03.json", data)
    elif damage in ("wrong-case", "wrong-state"):
        data = load_json(case_dir / "fatigue-F03.json")
        if damage == "wrong-case":
            data["case_id"] = "R08-W2-SCR"
        else:
            data["sea_state"]["id"] = "F04"
        write_json(case_dir / "fatigue-F03.json", data)
    elif damage.startswith("duplicate"):
        data = load_json(exchange / "cases.json")
        key = "cases" if damage == "duplicate-case" else "fatigue_sea_state_ids"
        data[key].append(data[key][0])
        write_json(exchange / "cases.json", data)
    else:
        data = load_json(FIXTURES / "criteria.json")
        data["strength"]["value"] = 0
        write_json(exchange / "invalid.json", data)
    with pytest.raises(ValueError):
        run_generation(exchange, exchange / "invalid.json" if damage == "bad-criteria" else None)
    assert not (exchange / "out").exists()


@pytest.mark.parametrize("field", ["software", "method", "allowable", "load_case", "run"])
def test_strength_requires_run_and_method_metadata(exchange, field):
    data = load_json(exchange / "results/R08-W1-SCR/strength.json")
    del data[field]
    with pytest.raises(ValueError):
        validate_result(data, "strength")


@pytest.mark.parametrize("mutation", ["length", "peak", "peak-position", "negative", "angle", "date", "digest"])
def test_strength_semantic_errors(exchange, mutation):
    data = load_json(exchange / "results/R08-W1-SCR/strength.json")
    if mutation == "length":
        data["effective_tension_kN"].append(1)
    elif mutation == "peak":
        data["max_utilisation"]["value"] = 0.3
    elif mutation == "peak-position":
        data["max_utilisation"]["arc_length_m"] = 0
    elif mutation == "negative":
        data["von_mises_utilisation"][0] = -1
    elif mutation == "angle":
        data["top_angle_deg"]["dynamic_min"] = 20
    elif mutation == "date":
        data["run"]["date"] = "2026-02-30"
    else:
        data["run"]["input_sha256"] = "invalid"
    with pytest.raises(ValueError):
        validate_result(data, "strength")


def test_fatigue_histogram_and_scalar_are_supported(exchange):
    data = load_json(exchange / "results/R08-W1-SCR/fatigue-F03.json")
    validate_result(data, "fatigue")
    hot = data["hot_spots"][0]
    del hot["stress_range_MPa"]
    hot["range_histogram"] = [{"stress_range_MPa": 10, "cycles_per_year": 1000}]
    validate_result(data, "fatigue")
    invalid = copy.deepcopy(data)
    invalid["hot_spots"][0]["range_histogram"][0]["cycles_per_year"] = -1
    with pytest.raises(ValueError):
        validate_result(invalid, "fatigue")


@pytest.mark.parametrize("text", ['{"a":1,"a":2}', '{"value":NaN}', '{"value":Infinity}'])
def test_ambiguous_json_is_rejected(tmp_path, text):
    path = tmp_path / "bad.json"
    path.write_text(text)
    with pytest.raises(ValueError):
        load_json(path)


def test_cli_and_existing_outputs_are_preserved(exchange):
    proc = subprocess.run([sys.executable, "-m", "digitalmodel.riser_study.decision_chart",
                           "--results", str(exchange / "results"),
                           "--manifest", str(exchange / "cases.json"),
                           "--criteria", str(FIXTURES / "criteria.json"),
                           "--output", str(exchange / "out"),
                           "--fatigue-tokens", str(exchange / "fatigue-tokens.json")], capture_output=True, text=True)
    assert proc.returncode == 0, proc.stderr
    before = (exchange / "out/manifest.json").read_bytes()
    with pytest.raises(FileExistsError):
        run_generation(exchange)
    assert (exchange / "out/manifest.json").read_bytes() == before


@pytest.mark.parametrize("date", ["2026-10-25", "2026-10-25T12:00:00Z", "2026-10-25T12:00:00+01:00"])
def test_run_dates_and_timezone_timestamps(exchange, date):
    data = load_json(exchange / "results/R08-W1-SCR/strength.json")
    data["run"]["date"] = date
    validate_result(data, "strength")


@pytest.mark.parametrize("field,value", [("life_years", 0), ("life_years", -1),
                                       ("life_years", float("inf")), ("damage_per_year", float("nan"))])
def test_fatigue_requires_finite_positive_life(exchange, field, value):
    data = load_json(exchange / "results/R08-W1-SCR/fatigue-F03.json")
    data[field] = value
    with pytest.raises(ValueError):
        validate_result(data, "fatigue")



@pytest.mark.parametrize("mutation", ["missing-damage", "station-grid", "duplicate-station",
                                     "missing-u1", "missing-phase", "unqualified", "phase-b",
                                     "ledger-mismatch", "duplicate-run", "missing-ledger",
                                     "unknown-resolution", "negative-station-damage"])
def test_unqualified_or_unlocated_data_is_rejected(exchange, mutation):
    path = exchange / "results/R08-W1-SCR/fatigue-F03.json"
    data = load_json(path)
    if mutation == "missing-damage":
        del data["hot_spots"][0]["damage_per_year"]
    elif mutation == "station-grid":
        data["hot_spots"][0]["arc_length_m"] = 99
    elif mutation == "duplicate-station":
        data["hot_spots"].append(data["hot_spots"][0])
    elif mutation == "negative-station-damage":
        data["hot_spots"][0]["damage_per_year"] = -1
    elif mutation == "unknown-resolution":
        write_json(exchange / "fatigue-tokens.json", {"V-FAT-RESOLVED": {"value": "unknown"}})
    elif mutation in ("duplicate-run", "missing-ledger"):
        manifest = load_json(exchange / "cases.json")
        if mutation == "duplicate-run":
            manifest["runs"].append(manifest["runs"][0])
        else:
            del manifest["runs"]
        write_json(exchange / "cases.json", manifest)
    else:
        path = exchange / "results/R08-W1-SCR/strength.json"
        data = load_json(path)
        if mutation == "missing-u1":
            del data["utilisation_basis"]
        elif mutation == "missing-phase":
            del data["run"]["phase"]
        elif mutation == "ledger-mismatch":
            data["run"]["id"] = "absent"
        else:
            data["run"]["qualified"] = mutation != "unqualified"
            data["run"]["phase"] = "B" if mutation == "phase-b" else "A"
            manifest = load_json(exchange / "cases.json")
            manifest["runs"][0].update({k: data["run"][k] for k in ("phase", "qualified")})
            write_json(exchange / "cases.json", manifest)
    write_json(path, data)
    with pytest.raises(ValueError):
        run_generation(exchange)
    assert not (exchange / "out").exists()


def test_qualified_phase_a_envelope_and_run_counts(exchange):
    path = exchange / "results/R08-W1-SCR/strength.json"
    original = load_json(path)
    manifest = load_json(exchange / "cases.json")
    for identifier, phase, qualified, util in (("higher", "A", True, 0.95),
                                              ("excluded", "A", False, 2),
                                              ("later", "B", True, 3)):
        data = copy.deepcopy(original)
        data["run"].update(id=identifier, phase=phase, qualified=qualified)
        data["von_mises_utilisation"] = [0, util]
        data["max_utilisation"]["value"] = util
        manifest["runs"].append({k: data["run"][k] for k in ("id", "phase", "status", "qualified")})
        write_json(path.with_name(f"strength-{identifier}.json"), data)
    manifest["runs"].append({"id": "unfinished", "phase": "A", "status": "failed", "qualified": False})
    write_json(exchange / "cases.json", manifest)
    data = run_generation(exchange)
    assert data["rows"][0]["max_utilisation"] == 0.95
    tokens = load_json(exchange / "out/tokens.json")
    assert [tokens[f"V-RUN-{k}"]["value"] for k in ("ATTEMPTED", "COMPLETED", "QUALIFIED")] == [51, 50, 49]
    assert tokens["V-PHB-STATUS"]["value"] == "not yet done"


@pytest.mark.parametrize("condition", ["not resolved", "no-strength-pass", "tie"])
def test_answers_require_strength_and_resolved_fatigue(exchange, condition):
    if condition == "not resolved":
        write_json(exchange / "fatigue-tokens.json", {"V-FAT-RESOLVED": {"value": condition}})
    elif condition == "no-strength-pass":
        for case in HAND:
            if case.startswith("R08"):
                path = exchange / "results" / case / "strength.json"
                data = load_json(path)
                data["von_mises_utilisation"] = [0, 1.1]
                data["max_utilisation"]["value"] = 1.1
                write_json(path, data)
    else:
        for state in ("F03", "F04", "F05"):
            path = exchange / "results/R08-W1-SLWR" / f"fatigue-{state}.json"
            data = load_json(path)
            for spot in data["hot_spots"]:
                spot["damage_per_year"] = 0.01
            data["damage_per_year"] = 0.01
            write_json(path, data)
    run_generation(exchange)
    tokens = load_json(exchange / "out/tokens.json")
    assert tokens["V-ANS-OD1"]["value"] == "ranking unresolved"
    if condition == "not resolved":
        assert all(tokens[f"V-ANS-OD{i}"]["value"] == "ranking unresolved" for i in (1, 2, 3))


def test_same_station_sum_precedes_max_and_no_occurrence_reweighting(exchange):
    data = run_generation(exchange)
    first = data["rows"][0]
    assert first["annual_benchmark_damage"] == pytest.approx(0.06)
    assert first["damage_arc_length_m"] == 100
    assert first["damage_location"] == "touchdown"
    assert first["annual_benchmark_damage"] != pytest.approx(0.08)  # Sum of state maxima is wrong.


def test_zero_damage_is_explicit_and_never_floored(exchange):
    for state in ("F03", "F04", "F05"):
        path = exchange / "results/R08-W1-SCR" / f"fatigue-{state}.json"
        data = load_json(path)
        for spot in data["hot_spots"]:
            spot["damage_per_year"] = 0
        data["damage_per_year"] = 0
        write_json(path, data)
    data = run_generation(exchange)
    fig, ax = build_chart(data["rows"], data["criteria"])
    assert data["rows"][0]["annual_benchmark_damage"] == 0
    assert any("true zero" in text.get_text().lower() for text in ax.texts)
    zero = ax.collections[0]
    assert tuple(zero.get_offsets()[0]) == (0.8, 0)
    assert zero.get_offset_transform() != ax.transData
    fig.clear()


def test_coincident_points_keep_coordinates_and_readable_labels():
    rows = [{"case_id": case, "configuration": case.rsplit("-", 1)[1], "size": case[1:3],
             "wall": case.split("-")[1], "max_utilisation": 0.8,
             "annual_benchmark_damage": 0.03} for case in HAND]
    fig, ax = build_chart(rows, load_json(FIXTURES / "criteria.json"))
    fig.canvas.draw()
    boxes = [label.get_window_extent(fig.canvas.get_renderer()) for label in ax.texts]
    assert len(boxes) == len(HAND)
    assert all(not a.overlaps(b) for i, a in enumerate(boxes) for b in boxes[i + 1:])
    assert all(tuple(point) == (0.8, 0.03) for c in ax.collections for point in c.get_offsets())
    fig.clear()


@pytest.mark.parametrize("run", [
    {"id": "a", "phase": "A", "status": "attempted", "qualified": True},
    {"id": "a", "phase": "A", "status": "failed", "qualified": True},
    {"id": "a", "phase": "A", "status": "completed", "qualified": "true"},
    {"id": "a", "phase": "C", "status": "completed", "qualified": True},
])
def test_run_ledger_requires_explicit_consistent_status(exchange, run):
    data = load_json(exchange / "cases.json")
    data["runs"].append(run)
    write_json(exchange / "cases.json", data)
    with pytest.raises(ValueError):
        run_generation(exchange)


def test_plot_colours_encode_size_and_markers_encode_configuration(exchange):
    data = run_generation(exchange)
    fig, ax = build_chart(data["rows"], data["criteria"])
    colours = [tuple(c.get_facecolors()[0]) for c in ax.collections]
    assert colours[0] == colours[1]
    assert colours[2] == colours[3]
    assert colours[4] == colours[5]
    assert len(set(colours)) == 3
    paths = [c.get_paths()[0] for c in ax.collections]
    assert all((paths[0].vertices == paths[i].vertices).all() for i in (2, 4))
    assert all((paths[1].vertices == paths[i].vertices).all() for i in (3, 5))
    assert paths[0].vertices.shape != paths[1].vertices.shape
    fig.clear()


def test_all_zero_damage_still_links_walls_without_log_floor(exchange):
    for path in (exchange / "results").glob("*/fatigue-*.json"):
        data = load_json(path)
        data["damage_per_year"] = 0
        for spot in data["hot_spots"]:
            spot["damage_per_year"] = 0
        write_json(path, data)
    data = run_generation(exchange)
    fig, ax = build_chart(data["rows"], data["criteria"])
    assert len(ax.patches) == 6  # Endpoint-aware links, with no filled region.
    assert all(link.xy1[1] == link.xy2[1] == 0 for link in ax.patches)
    assert len(ax.lines) == 1
    assert all(point[1] == 0 for collection in ax.collections for point in collection.get_offsets())
    assert all(row["annual_benchmark_damage"] == 0 for row in data["rows"])
    fig.clear()


def test_legacy_output_directory_is_rejected_without_stale_tokens(exchange):
    output = exchange / "out"
    output.mkdir()
    (output / "criteria.md").write_text("previous output")
    with pytest.raises(FileExistsError):
        run_generation(exchange)
    assert not (output / "tokens.json").exists()
    assert (output / "criteria.md").read_text() == "previous output"


def test_damage_ties_use_stable_station_order(exchange):
    for state in ("F03", "F04", "F05"):
        path = exchange / "results/R08-W1-SCR" / f"fatigue-{state}.json"
        data = load_json(path)
        for spot in data["hot_spots"]:
            spot["damage_per_year"] = 0.01
        data["damage_per_year"] = 0.01
        write_json(path, data)
    data = run_generation(exchange)
    assert data["rows"][0]["damage_arc_length_m"] == 0
    assert data["rows"][0]["damage_location"] == "hang_off"


@pytest.mark.parametrize("retained", [1, 11])
def test_manifest_and_exports_cannot_omit_competitors(exchange, retained):
    manifest = load_json(exchange / "cases.json")
    manifest["cases"] = manifest["cases"][:retained]
    keep = {row["case_id"] for row in manifest["cases"]}
    for folder in (exchange / "results").iterdir():
        if folder.name not in keep:
            folder.rename(exchange / folder.name)
    write_json(exchange / "cases.json", manifest)
    with pytest.raises(ValueError, match="matrix"):
        run_generation(exchange)
    assert not (exchange / "out").exists()


@pytest.mark.parametrize("offset", ["+00:60", "-00:60", "+01:99", "+24:00", "-24:00"])
def test_rfc3339_rejects_out_of_range_offsets(exchange, offset):
    data = load_json(exchange / "results/R08-W1-SCR/strength.json")
    data["run"]["date"] = "2026-10-25T12:00:00" + offset
    with pytest.raises(ValueError):
        validate_result(data, "strength")


@pytest.mark.parametrize("index", range(3))
def test_peer_export_records_match_shared_contract(index):
    fixture = load_json(FIXTURES / "peer-records.json")[index]
    validate_result(fixture["record"], fixture["kind"])


@pytest.mark.parametrize("correction", [None, {
    "thickness_mm": 40, "reference_thickness_mm": 20,
    "exponent": 1, "source_label": "ASSUMED synthetic correction",
}])
def test_peer_fatigue_thickness_metadata(exchange, correction):
    data = load_json(exchange / "results/R08-W1-SCR/fatigue-F03.json")
    data["sn_curve"]["source_label"] = data["sn_curve"].pop("source")
    data["sn_curve"]["thickness_correction"] = correction
    validate_result(data, "fatigue")


@pytest.mark.parametrize("correction", [0, -1, True, {}, {
    "thickness_mm": 0, "reference_thickness_mm": 20,
    "exponent": 1, "source_label": "ASSUMED",
}])
def test_invalid_thickness_metadata_is_rejected(exchange, correction):
    data = load_json(exchange / "results/R08-W1-SCR/fatigue-F03.json")
    data["sn_curve"]["thickness_correction"] = correction
    with pytest.raises(ValueError):
        validate_result(data, "fatigue")


@pytest.mark.parametrize("version", [None, True, 0, 2, "1"])
def test_unknown_or_missing_matrix_version_is_rejected(exchange, version):
    data = load_json(exchange / "cases.json")
    if version is None:
        del data["matrix_version"]
    else:
        data["matrix_version"] = version
    write_json(exchange / "cases.json", data)
    with pytest.raises(ValueError, match="matrix"):
        run_generation(exchange)
    assert not (exchange / "out").exists()


def test_versioned_contract_is_recorded_in_output(exchange):
    from digitalmodel.riser_study.results_exchange import EXCHANGE_CONTRACT, exchange_schema
    from jsonschema import Draft202012Validator

    schema = exchange_schema()
    Draft202012Validator.check_schema(schema)
    assert schema["$id"] == EXCHANGE_CONTRACT == "urn:digitalmodel:riser-results:v1"
    data = run_generation(exchange)
    assert data["exchange_contract"] == EXCHANGE_CONTRACT
    assert data["matrix_version"] == 1
    assert len(data["rows"]) == 12


def test_peer_metadata_is_accepted_by_decision_reader(exchange):
    for path in (exchange / "results").glob("*/strength.json"):
        data = load_json(path)
        del data["allowable"]["basis_label"]
        write_json(path, data)
    for path in (exchange / "results").glob("*/fatigue-*.json"):
        data = load_json(path)
        data["sn_curve"]["source_label"] = data["sn_curve"].pop("source")
        data["sn_curve"]["thickness_correction"] = None
        write_json(path, data)
    data = run_generation(exchange)
    assert data["rows"][0]["annual_benchmark_damage"] == pytest.approx(0.06)
    tokens = load_json(exchange / "out/tokens.json")
    assert tokens["V-ANS-OD1"]["value"] == "SLWR W2"


@pytest.mark.parametrize("source", ["source", "source_label"])
def test_fatigue_source_must_be_nonblank(exchange, source):
    data = load_json(exchange / "results/R08-W1-SCR/fatigue-F03.json")
    data["sn_curve"].pop("source")
    data["sn_curve"][source] = " "
    with pytest.raises(ValueError):
        validate_result(data, "fatigue")
