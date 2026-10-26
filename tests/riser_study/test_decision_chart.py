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


# ASSUMED synthetic arithmetic examples, not engineering results. Each tuple is
# (maximum utilisation, three sea-state lives in years, shortest life, both).
# With the fixture criteria u <= 1 and L >= 20, hand counting gives six passes:
# R08-W1-SCR, R08-W1-SLWR, R08-W2-SLWR, R10-W2-SCR, R12-W1-SCR, R12-W2-SLWR.
HAND = {
    "R08-W1-SCR": (0.8, [40, 20, 30], 20, True),
    "R08-W1-SLWR": (0.6, [80, 60, 70], 60, True),
    "R08-W2-SCR": (1.1, [50, 40, 60], 40, False),
    "R08-W2-SLWR": (1.0, [30, 20, 40], 20, True),
    "R10-W1-SCR": (0.9, [10, 30, 20], 10, False),
    "R10-W1-SLWR": (1.2, [10, 30, 20], 10, False),
    "R10-W2-SCR": (0.7, [25, 35, 45], 25, True),
    "R10-W2-SLWR": (1.3, [40, 50, 60], 40, False),
    "R12-W1-SCR": (0.5, [100, 80, 90], 80, True),
    "R12-W1-SLWR": (0.8, [15, 25, 35], 15, False),
    "R12-W2-SCR": (1.1, [20, 30, 40], 20, False),
    "R12-W2-SLWR": (0.4, [60, 50, 70], 50, True),
}
FIXTURES = Path(__file__).parent / "fixtures"


def write_json(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, allow_nan=False), encoding="utf-8")


@pytest.fixture
def exchange(tmp_path):
    """Write synthetic values; the C1 hand benchmark supplies static tensions."""
    root = Path(__file__).resolve().parents[2]
    benchmark = load_json(root / "docs/benchmarks/catenary_closed_form/benchmark.json")
    c1 = benchmark["cases"][0]["expected"]
    # All numbers below except the cited C1 tension are ASSUMED synthetic
    # schema examples. No S-N calculation or dynamic response is represented.
    run = {"input_sha256": hashlib.sha256(b"synthetic input").hexdigest(),
           "result_file": "synthetic.json", "date": "2026-10-25"}
    for case, (util, lives, _, _) in HAND.items():
        strength = {
            "case_id": case, "software": {"name": "synthetic", "version": "fixture"},
            "load_case": {"id": "synthetic-strength", "wave": {}, "current": {}, "offset": {}},
            "arc_length_m": [0, 100],
            "effective_tension_kN": [c1["top_tension_kN"], 50],
            "bending_moment_kNm": [0, 10], "von_mises_utilisation": [util / 2, util],
            "method": "ASSUMED synthetic utilisation; no design check",
            "allowable": {"value": 100, "unit": "MPa", "basis_label": "ASSUMED",
                          "source": "synthetic arithmetic example"},
            "max_utilisation": {"value": util, "arc_length_m": 100, "location": "touchdown"},
            "touchdown": {"arc_length_m": 100, "utilisation": util, "stress_range_MPa": 10},
            "top_angle_deg": {"static": 10, "dynamic_min": 8, "dynamic_max": 12},
            "hang_off_tension_kN": {"static": c1["top_tension_kN"], "dynamic_max": 600},
            "run": run,
        }
        write_json(tmp_path / "results" / case / "strength.json", strength)
        for state, life in zip(("S1", "S2", "S3"), lives):
            fatigue = {
                "case_id": case,
                "sea_state": {"id": state, "Hs_m": 2, "Tp_s": 8,
                              "direction_deg": 0, "occurrence": 0.2},
                "hot_spots": [{"name": "touchdown", "arc_length_m": 100,
                               "stress_range_MPa": 10, "cycles_per_year": 1000, "scf": 1}],
                "sn_curve": {"name": "synthetic", "source": "ASSUMED arithmetic example",
                             "m": 3, "log_a": 12, "thickness_correction": 1},
                "damage_per_year": 1 / life, "life_years": life, "run": run,
            }
            write_json(tmp_path / "results" / case / f"fatigue-{state}.json", fatigue)
    manifest = {"basis_label": "synthetic", "fatigue_sea_state_ids": ["S1", "S2", "S3"],
                "cases": [{"case_id": case} for case in HAND]}
    write_json(tmp_path / "cases.json", manifest)
    return tmp_path


def run_generation(root, criteria=None):
    return generate(root / "results", root / "cases.json",
                    criteria or FIXTURES / "criteria.json", root / "out")


def test_every_point_matches_csv_manifest_and_hand_rows(exchange, monkeypatch):
    plotted = []

    def capture(rows, criteria):
        fig, ax = build_chart(rows, criteria)
        plotted.extend(tuple(point) for collection in ax.collections
                       for point in collection.get_offsets())
        return fig, ax

    monkeypatch.setattr(decision_chart, "build_chart", capture)
    manifest = run_generation(exchange)
    rows = list(csv.DictReader((exchange / "out/results.csv").open()))
    fig, ax = build_chart(manifest["rows"], manifest["criteria"])
    points = [tuple(point) for collection in ax.collections
              for point in collection.get_offsets()]
    assert len(points) == len(rows) == len(HAND)
    assert sorted(plotted) == sorted(points)
    for csv_row, row in zip(rows, manifest["rows"]):
        util, _, life, passed = HAND[row["case_id"]]
        assert float(csv_row["max_utilisation"]) == row["max_utilisation"] == util
        assert float(csv_row["shortest_life_years"]) == row["shortest_life_years"] == life
        assert (util, life) in points
        assert row["meets_both"] is passed
        assert row["max_utilisation_location"] == "touchdown"
    assert sorted(t.get_text() for t in ax.texts) == sorted(HAND)
    fig.canvas.draw()
    boxes = [label.get_window_extent(fig.canvas.get_renderer()) for label in ax.texts]
    assert all(not first.overlaps(second) for i, first in enumerate(boxes) for second in boxes[i + 1:])
    assert ax.get_yscale() == "log"
    assert len(ax.collections) == 2  # SCR and SLWR have separate markers.
    assert len(ax.patches) == 1  # Quadrant bounded by the file criteria.
    assert manifest["count_both"] == 6  # Hand enumeration above, not a computed oracle.
    assert manifest["criteria"]["strength"]["basis_label"] == "ASSUMED"
    tokens = load_json(exchange / "out/tokens.json")
    assert tokens["V-DEC-COUNT-BOTH"]["value"] == 6
    assert {"FIG-DEC-01", "TAB-RES-01", "TAB-DEC-CRITERIA-01"} <= tokens.keys()
    for key in ("FIG-DEC-01", "TAB-RES-01", "TAB-DEC-CRITERIA-01"):
        assert (exchange / "out" / tokens[key]["path"]).is_file()
    assert "ASSUMED" in (exchange / "out/criteria.csv").read_text()
    assert "Screening sensitivity" in (exchange / "out/decision.svg").read_text()
    for source in manifest["inputs"]:
        path = exchange / source["path"]
        if source["role"] == "criteria":
            path = FIXTURES / "criteria.json"
        assert hashlib.sha256(path.read_bytes()).hexdigest() == source["sha256"]
    for artifact, digest in manifest["artifacts"].items():
        assert hashlib.sha256((exchange / "out" / artifact).read_bytes()).hexdigest() == digest
    fig.clear()


def test_criteria_lines_and_table_follow_changed_file(exchange):
    criteria = load_json(FIXTURES / "criteria.json")
    # ASSUMED alternative limits: R08-W1-SLWR and R12-W1-SCR pass by hand.
    criteria["strength"]["value"] = 0.6
    criteria["fatigue"]["value"] = 55
    write_json(exchange / "alternative.json", criteria)
    manifest = run_generation(exchange, exchange / "alternative.json")
    fig, ax = build_chart(manifest["rows"], manifest["criteria"])
    assert list(ax.lines[0].get_xdata()) == [0.6, 0.6]
    assert list(ax.lines[1].get_ydata()) == [55, 55]
    assert manifest["count_both"] == 2
    rows = list(csv.DictReader((exchange / "out/criteria.csv").open()))
    assert [float(row["value"]) for row in rows] == [0.6, 55]
    assert all(row["source"] == criteria[row["key"]]["source"] for row in rows)
    fig.clear()


@pytest.mark.parametrize("damage", ["missing-case", "missing-state", "extra-state", "wrong-case",
                                   "wrong-state", "duplicate-case", "duplicate-state", "bad-criteria",
                                   "orphan-case", "inconsistent-state"])
def test_incomplete_or_ambiguous_exchange_is_rejected(exchange, damage):
    case_dir = exchange / "results/R08-W1-SCR"
    if damage == "missing-case":
        (case_dir / "strength.json").rename(case_dir / "strength.absent")
    elif damage == "missing-state":
        (case_dir / "fatigue-S1.json").rename(case_dir / "fatigue.absent")
    elif damage == "extra-state":
        write_json(case_dir / "fatigue-S4.json", load_json(case_dir / "fatigue-S1.json"))
    elif damage == "orphan-case":
        write_json(exchange / "results/R08-W3-SCR/fatigue-S1.json",
                   load_json(case_dir / "fatigue-S1.json"))
    elif damage == "inconsistent-state":
        data = load_json(case_dir / "fatigue-S1.json")
        data["sea_state"]["Hs_m"] = 3  # ASSUMED inconsistent synthetic wave height.
        write_json(case_dir / "fatigue-S1.json", data)
    elif damage in ("wrong-case", "wrong-state"):
        data = load_json(case_dir / "fatigue-S1.json")
        if damage == "wrong-case":
            data["case_id"] = "R08-W2-SCR"
        else:
            data["sea_state"]["id"] = "S2"
        write_json(case_dir / "fatigue-S1.json", data)
    elif damage.startswith("duplicate"):
        data = load_json(exchange / "cases.json")
        key = "cases" if damage == "duplicate-case" else "fatigue_sea_state_ids"
        data[key].append(data[key][0])
        write_json(exchange / "cases.json", data)
    else:
        data = load_json(FIXTURES / "criteria.json")
        data["fatigue"]["value"] = 0
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
    data = load_json(exchange / "results/R08-W1-SCR/fatigue-S1.json")
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
                           "--output", str(exchange / "out")], capture_output=True, text=True)
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
    data = load_json(exchange / "results/R08-W1-SCR/fatigue-S1.json")
    data[field] = value
    with pytest.raises(ValueError):
        validate_result(data, "fatigue")


def test_life_ties_use_stable_sea_state_order(exchange):
    path = exchange / "results/R08-W1-SCR/fatigue-S1.json"
    data = load_json(path)
    data["life_years"] = 20  # ASSUMED tie with S2; S1 sorts first.
    data["damage_per_year"] = 0.05  # Hand reciprocal: 1 / 20.
    write_json(path, data)
    manifest = run_generation(exchange)
    assert manifest["rows"][0]["governing_sea_state"] == "S1"


def test_coincident_points_keep_coordinates_and_readable_labels():
    # ASSUMED identical points exercise presentation only; no result is moved.
    rows = [{"case_id": case, "configuration": case.rsplit("-", 1)[1],
             "max_utilisation": 0.8, "shortest_life_years": 30} for case in HAND]
    fig, ax = build_chart(rows, load_json(FIXTURES / "criteria.json"))
    fig.canvas.draw()
    boxes = [label.get_window_extent(fig.canvas.get_renderer()) for label in ax.texts]
    assert len(boxes) == len(HAND)
    assert all(not a.overlaps(b) for i, a in enumerate(boxes) for b in boxes[i + 1:])
    assert all(tuple(point) == (0.8, 30) for c in ax.collections for point in c.get_offsets())
    fig.clear()
