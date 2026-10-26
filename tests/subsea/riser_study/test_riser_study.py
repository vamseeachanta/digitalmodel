# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic exchange, rendering and static geometry contracts."""

from copy import deepcopy
import csv
import hashlib
import json
import math
from pathlib import Path
import subprocess
import sys
import xml.etree.ElementTree as ET

import pytest

ROOT = Path(__file__).resolve().parents[3]
PACKAGE = ROOT / "src/digitalmodel/subsea/riser_study"
NS = {"s": "http://www.w3.org/2000/svg"}


@pytest.fixture
def strength():
    """ASSUMED synthetic values: uniaxial 60/100 MPa gives utilisation 0.6."""
    return {
        "case_id": "R08-W1-SCR",
        "software": {"name": "synthetic", "version": "fixture"},
        "load_case": {"id": "synthetic-extreme", "wave": {"Hs_m": 0},
                      "current": {"speed_m_s": 0}, "offset": {"x_m": 0, "y_m": 0}},
        "arc_length_m": [0, 50, 100],
        "effective_tension_kN": [120, 90, 70],
        "bending_moment_kNm": [0, 2, 1],
        "von_mises_utilisation": [0.4, 0.6, 0.5],
        "method": "ASSUMED synthetic uniaxial von Mises stress / allowable",
        "allowable": {"value": 100, "unit": "MPa", "source": "ASSUMED synthetic"},
        "max_utilisation": {"value": 0.6, "arc_length_m": 50, "location": "touchdown"},
        "touchdown": {"arc_length_m": 50, "utilisation": 0.6, "stress_range_MPa": 12},
        "top_angle_deg": {"static": 10, "dynamic_min": 8, "dynamic_max": 13},
        "hang_off_tension_kN": {"static": 120, "dynamic_max": 140},
        "run": {"input_sha256": "a" * 64, "result_file": "exports/synthetic.json",
                "date": "2026-10-26"},
    }


@pytest.fixture
def fatigue():
    """ASSUMED synthetic Miner example: 0.01/year gives 100 years."""
    return {
        "case_id": "R08-W1-SCR",
        "sea_state": {"id": "synthetic-1", "Hs_m": 2, "Tp_s": 8,
                      "direction_deg": 0, "occurrence": 0.1},
        "hot_spots": [{"name": "touchdown", "arc_length_m": 50,
                       "stress_range_MPa": 12, "cycles_per_year": 1000, "scf": 1}],
        "sn_curve": {"name": "synthetic", "source": "ASSUMED synthetic",
                     "m": 3, "log_a": 12, "thickness_correction": 1},
        "damage_per_year": 0.01, "life_years": 100,
        "run": {"input_sha256": "b" * 64, "result_file": "exports/synthetic.json",
                "date": "2026-10-26"},
    }


def exchange():
    from digitalmodel.subsea.riser_study import results_exchange
    return results_exchange


def test_schema_accepts_both_contracts(strength, fatigue):
    import jsonschema
    schema = json.loads((PACKAGE / "results_exchange.schema.json").read_text())
    jsonschema.Draft202012Validator.check_schema(schema)
    for record, kind in [(strength, "strength"), (fatigue, "fatigue")]:
        jsonschema.validate(record, schema)
        assert exchange().validate_result(record, kind) == record


@pytest.mark.parametrize("field", ["case_id", "software", "load_case", "arc_length_m",
    "effective_tension_kN", "bending_moment_kNm", "von_mises_utilisation", "method",
    "allowable", "max_utilisation", "touchdown", "top_angle_deg", "hang_off_tension_kN", "run"])
def test_missing_strength_fields_rejected(strength, field):
    del strength[field]
    with pytest.raises(ValueError):
        exchange().validate_result(strength, "strength")


@pytest.mark.parametrize("defect", ["length", "order", "nan", "bool", "negative_utilisation",
    "max_value", "max_arc", "tdp_arc", "angle_range", "date", "hash", "absolute_path",
    "parent_path", "case_id", "allowable", "extra"])
def test_semantic_strength_defects_rejected(strength, defect, tmp_path):
    if defect == "length":
        strength["effective_tension_kN"].pop()
    elif defect == "order":
        strength["arc_length_m"] = [0, 100, 50]
    elif defect == "nan":
        strength["bending_moment_kNm"][0] = float("nan")
    elif defect == "bool":
        strength["effective_tension_kN"][0] = True
    elif defect == "negative_utilisation":
        strength["von_mises_utilisation"][0] = -1
    elif defect == "max_value":
        strength["max_utilisation"]["value"] = 0.9
    elif defect == "max_arc":
        strength["max_utilisation"]["arc_length_m"] = 0
    elif defect == "tdp_arc":
        strength["touchdown"]["arc_length_m"] = 101
    elif defect == "angle_range":
        strength["top_angle_deg"]["dynamic_min"] = 14
    elif defect == "date":
        strength["run"]["date"] = "2026-02-30"
    elif defect == "hash":
        strength["run"]["input_sha256"] = "invalid"
    elif defect == "absolute_path":
        strength["run"]["result_file"] = str(tmp_path / "result.json")
    elif defect == "parent_path":
        strength["run"]["result_file"] = "../result.json"
    elif defect == "case_id":
        strength["case_id"] = "arbitrary"
    elif defect == "allowable":
        strength["allowable"]["value"] = 0
    elif defect == "extra":
        strength["undocumented"] = 1
    with pytest.raises(ValueError):
        exchange().validate_result(strength, "strength")


def test_fatigue_histogram_and_invalid_records(fatigue):
    hot = fatigue["hot_spots"][0]
    hot["range_histogram"] = [{"stress_range_MPa": hot.pop("stress_range_MPa"),
                               "cycles_per_year": hot["cycles_per_year"]}]
    assert exchange().validate_result(fatigue, "fatigue") == fatigue
    for change in [{"occurrence": 1.1}, {"Tp_s": 0}]:
        bad = deepcopy(fatigue)
        bad["sea_state"].update(change)
        with pytest.raises(ValueError):
            exchange().validate_result(bad, "fatigue")
    bad = deepcopy(fatigue)
    bad["hot_spots"][0]["stress_range_MPa"] = 12
    with pytest.raises(ValueError):
        exchange().validate_result(bad, "fatigue")
    bad = deepcopy(fatigue)
    bad["life_years"] = 99
    with pytest.raises(ValueError):
        exchange().validate_result(bad, "fatigue")


def write_cases(tmp_path, strength):
    """Create paired ASSUMED fixtures; the SLWR maximum is hand-set to 0.8."""
    results = tmp_path / "results"
    for kind, values in [("SCR", [0.4, 0.6, 0.5]), ("SLWR", [0.3, 0.8, 0.4])]:
        record = deepcopy(strength)
        record["case_id"] = f"R08-W1-{kind}"
        record["von_mises_utilisation"] = values
        record["max_utilisation"]["value"] = values[1]
        record["touchdown"]["utilisation"] = values[1]
        folder = results / record["case_id"]
        folder.mkdir(parents=True)
        (folder / "strength.json").write_text(json.dumps(record))
    return results


def test_every_svg_value_matches_csv_manifest_and_sources(tmp_path, strength):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = write_cases(tmp_path, strength)
    out = tmp_path / "figures"
    manifest = build_strength(results, out, reference_pair="R08-W1", touchdown_window_m=50)
    expected = {f"FIG-STR-0{i}" for i in range(1, 7)}
    assert {a["id"] for a in manifest["artifacts"]} == expected
    assert json.loads((out / "manifest.json").read_text()) == manifest
    for artifact in manifest["artifacts"]:
        rows = artifact["rows"]
        with (out / artifact["csv_file"]).open(newline="") as stream:
            csv_rows = list(csv.DictReader(stream))
        assert csv_rows == [{k: str(v) for k, v in r.items()} for r in rows]
        svg = ET.parse(out / artifact["file"]).getroot()
        assert json.loads(svg.find("s:metadata", NS).text) == rows
        marks = svg.findall('.//s:circle[@data-row]', NS)
        assert len(marks) == len(rows)
        for mark, row in zip(marks, rows):
            assert float(mark.attrib["data-x"]) == row["x"]
            assert float(mark.attrib["data-y"]) == row["y"]
            # Check actual geometry, not only embedded metadata.
            axes = svg.find('.//s:g[@id="axes"]', NS).attrib
            assert float(mark.attrib["cx"]) == pytest.approx(
                float(axes["data-x-origin"]) + row["x"] * float(axes["data-x-scale"]), abs=1e-6)
            assert float(mark.attrib["cy"]) == pytest.approx(
                float(axes["data-y-origin"]) + row["y"] * float(axes["data-y-scale"]), abs=1e-6)
        assert "stroke-dasharray" in (out / artifact["file"]).read_text()
    tokens = {r["id"]: r for r in json.loads((out / "tokens.json").read_text())}
    assert tokens["V-STR-GOV-CASE"]["value"] == "R08-W1-SLWR"
    assert tokens["V-STR-UMAX-GOV"]["value"] == 0.8
    assert tokens["V-STR-UMAX-SCR"]["value"] == 0.6
    assert tokens["V-STR-UMAX-SLWR"]["value"] == 0.8
    assert tokens["V-STR-TDP-SCR-REF"]["value"] == 0.6
    assert tokens["V-STR-TDP-SLWR-REF"]["value"] == 0.8
    assert tokens["V-SW-VERSION"]["value"] == "synthetic fixture"
    for source in manifest["sources"]:
        path = tmp_path / source["file"]
        assert hashlib.sha256(path.read_bytes()).hexdigest() == source["sha256"]
    curve = next(a for a in manifest["artifacts"] if a["id"] == "FIG-STR-01")
    assert [r["y"] for r in curve["rows"] if r["case_id"].endswith("SCR")] == [120, 90, 70]
    maximum = next(a for a in manifest["artifacts"] if a["id"] == "FIG-STR-03")
    assert {r["y"] for r in maximum["rows"] if r["series"] == "limit"} == {1}
    top = next(a for a in manifest["artifacts"] if a["id"] == "FIG-STR-05")
    assert {r["series"] for r in top["rows"]} == {"static", "dynamic_min", "dynamic_max"}
    tension = next(a for a in manifest["artifacts"] if a["id"] == "FIG-STR-06")
    assert {r["series"] for r in tension["rows"]} == {"static", "dynamic_max"}


def test_bad_batch_writes_nothing_and_requires_explicit_reference(tmp_path, strength):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = write_cases(tmp_path, strength)
    for reference, window in [("R10-W1", 50), ("R08-W1", -1)]:
        with pytest.raises(ValueError):
            build_strength(results, tmp_path / "out", reference_pair=reference, touchdown_window_m=window)
    wrong = results / "R08-W1-SLWR/strength.json"
    record = json.loads(wrong.read_text())
    record["case_id"] = "R08-W1-SCR"
    wrong.write_text(json.dumps(record))
    with pytest.raises(ValueError):
        build_strength(results, tmp_path / "out", reference_pair="R08-W1", touchdown_window_m=50)
    assert not (tmp_path / "out").exists()


def test_hand_checked_utilisation(strength):
    # Uniaxial von Mises: sqrt(sigma^2) = 60 MPa; allowable = 100 MPa.
    assert exchange().validate_result(strength, "strength")["max_utilisation"]["value"] == 60 / 100


@pytest.fixture
def geometry():
    """ASSUMED example from tests/riser_configuration_screen/test_static.py."""
    return {"water_depth_m": 1500, "submerged_weight_n_m": 1000,
            "grounded_length_m": 500, "hangoff_angle_deg": 10,
            "sag_fraction": 0.1, "hog_fraction": 0.4, "buoyancy_factor": 2}


def test_schematic_equal_scale_and_screen_geometry(tmp_path, geometry):
    from digitalmodel.subsea.riser_study.riser_configuration_plot import build_configuration
    from digitalmodel.riser_configuration_screen.static import scr_offsets, lazy_wave_offsets
    manifest = build_configuration(geometry, tmp_path, reference_pair="R08-W1",
                                   source_file="synthetic-geometry.json")
    artifact = manifest["artifacts"][0]
    svg = ET.parse(tmp_path / "SCH-01.svg").getroot()
    axes = svg.find('.//s:g[@id="axes"]', NS).attrib
    assert abs(float(axes["data-x-scale"])) == abs(float(axes["data-y-scale"]))
    for kind, solve in [("SCR", scr_offsets), ("SLWR", lazy_wave_offsets)]:
        actual = [r for r in artifact["rows"] if r["case_id"] == f"R08-W1-{kind}"]
        expected = solve(geometry, [0])[0]["shape"]
        assert [(r["x"], r["y"]) for r in actual] == pytest.approx(
            [(p[1], geometry["water_depth_m"] - p[2]) for p in expected])
    text = " ".join(svg.itertext())
    for label in ["Hang-off", "Sag bend", "Hog bend", "Buoyancy section", "Touchdown", "Seabed", "Water line"]:
        assert label in text
    assert (tmp_path / "SCH-01.csv").exists()


def test_closed_form_fixture_matches_stated_tolerances():
    """C1 plain benchmark; elastic C2 is outside this geometry screen's scope."""
    from digitalmodel.riser_configuration_screen.static import scr_offsets
    benchmark = json.loads((ROOT / "docs/benchmarks/catenary_closed_form/benchmark.json").read_text())
    case = benchmark["cases"][0]
    data = case["inputs"]
    # Independent closed-form solve in terms of suspended arc s:
    # a=(s^2-h^2)/(2h), X=a*asinh(s/a)+(L-s).
    low, high = data["h_m"], data["L_m"]
    for _ in range(100):
        s = (low + high) / 2
        a = (s*s-data["h_m"]**2)/(2*data["h_m"])
        span = a*math.asinh(s/a) + data["L_m"]-s
        if span < data["X_m"]:
            low = s
        else: high = s
    angle = math.degrees(math.atan2(a, s))
    row = scr_offsets({"water_depth_m": data["h_m"], "submerged_weight_n_m": data["w_N_per_m"],
                       "grounded_length_m": data["L_m"]-s, "hangoff_angle_deg": angle}, [0])[0]
    bands = benchmark["calibrates"]["band"]
    assert row["top_tension_n"] / 1000 == pytest.approx(case["expected"]["top_tension_kN"],
        rel=bands["top_tension"]["percent"]/100)
    assert row["top_angle_deg"] == pytest.approx(case["expected"]["top_angle_deg_from_vertical"],
        abs=bands["top_angle"]["degrees"])


def test_cli_end_to_end_without_licensed_import(tmp_path, strength):
    results = write_cases(tmp_path, strength)
    script = """
import runpy, sys

def guard(event, args):
    if event == 'import' and args[0].split('.')[0] in {'OrcFxAPI', 'OrcWaveAPI', 'aqwa'}:
        raise AssertionError('licensed interface import attempted')
sys.addaudithook(guard)
sys.argv = ['strength', *sys.argv[1:]]
runpy.run_module('digitalmodel.subsea.riser_study.riser_strength_runs', run_name='__main__')
"""
    command = [sys.executable, "-c", script, str(results), str(tmp_path / "out"),
               "--reference-pair", "R08-W1", "--touchdown-window-m", "50"]
    result = subprocess.run(command, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    assert (tmp_path / "out/tokens.json").exists()


def test_twelve_cases_and_multiple_software_versions(tmp_path, strength):
    """Cover the complete diameter/wall/configuration matrix with synthetic data."""
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = tmp_path / "results"
    for size in ("08", "10", "12"):
        for wall in ("1", "2"):
            for kind in ("SCR", "SLWR"):
                data = deepcopy(strength)
                data["case_id"] = f"R{size}-W{wall}-{kind}"
                if wall == "2":
                    data["software"]["version"] = "fixture-alternate"
                folder = results / data["case_id"]
                folder.mkdir(parents=True)
                (folder / "strength.json").write_text(json.dumps(data))
    output = tmp_path / "output"
    manifest = build_strength(results, output, reference_pair="R12-W2", touchdown_window_m=1)
    assert len(manifest["sources"]) == 12
    for artifact in manifest["artifacts"]:
        assert len({r["case_id"] for r in artifact["rows"] if r["case_id"]}) == 12
        svg = ET.parse(output / artifact["file"]).getroot()
        assert "SYNTHETIC" in " ".join(svg.itertext())
        # Verify every drawn response polyline against the tabulated marks.
        axes = svg.find('.//s:g[@id="axes"]', NS).attrib
        for line in svg.findall('.//s:polyline[@data-case]', NS):
            rows = [r for r in artifact["rows"] if r["case_id"] == line.attrib["data-case"]
                    and r["series"] == line.attrib["data-series"]]
            assert len(line.attrib["points"].split()) == len(rows)
            for point, datum in zip(line.attrib["points"].split(), rows):
                x, y = map(float, point.split(","))
                assert x == pytest.approx(float(axes["data-x-origin"]) + datum["x"]*float(axes["data-x-scale"]), abs=1e-6)
                assert y == pytest.approx(float(axes["data-y-origin"]) + datum["y"]*float(axes["data-y-scale"]), abs=1e-6)
    tokens = {t["id"]: t for t in json.loads((output / "tokens.json").read_text())}
    assert tokens["V-STR-GOV-CASE"]["value"] == "R08-W1-SCR"
    assert tokens["V-SW-VERSION"]["value"] == "synthetic fixture; synthetic fixture-alternate"
    assert len(tokens["V-SW-VERSION"]["source_file"]) == 12


@pytest.mark.parametrize("field", ["case_id", "sea_state", "hot_spots", "sn_curve",
                                   "damage_per_year", "life_years", "run"])
def test_missing_fatigue_fields_rejected(fatigue, field):
    del fatigue[field]
    with pytest.raises(ValueError):
        exchange().validate_result(fatigue, "fatigue")


def test_zero_damage_and_histogram_total(fatigue):
    fatigue["damage_per_year"] = 0
    fatigue["life_years"] = None
    assert exchange().validate_result(fatigue, "fatigue") == fatigue
    hot = fatigue["hot_spots"][0]
    hot["range_histogram"] = [{"stress_range_MPa": hot.pop("stress_range_MPa"), "cycles_per_year": 10}]
    with pytest.raises(ValueError, match="annual cycles"):
        exchange().validate_result(fatigue, "fatigue")


def test_intermediate_touchdown_is_recorded_without_interpolation(tmp_path, strength):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    strength["touchdown"] = {"arc_length_m": 75, "utilisation": 0.55, "stress_range_MPa": 12}
    # ASSUMED intermediate response, deliberately independent of nearest samples.
    results = write_cases(tmp_path, strength)
    for file in results.glob("*/strength.json"):
        data = json.loads(file.read_text())
        data["touchdown"]["utilisation"] = 0.55
        file.write_text(json.dumps(data))
    manifest = build_strength(results, tmp_path / "out", reference_pair="R08-W1", touchdown_window_m=1)
    artifact = next(a for a in manifest["artifacts"] if a["id"] == "FIG-STR-04")
    assert [(r["x"], r["y"]) for r in artifact["rows"]] == [(75, 0.55), (75, 0.55)]


def test_schema_diagnostics_do_not_echo_values(strength):
    strength["case_id"] = "invalid-sensitive-example"
    with pytest.raises(ValueError) as error:
        exchange().validate_result(strength, "strength")
    assert "invalid-sensitive-example" not in str(error.value)
    assert "case_id" in str(error.value)


def test_geometry_cli_provenance_and_no_native_interface(tmp_path, geometry):
    source = tmp_path / "synthetic-geometry.json"
    source.write_text(json.dumps(geometry))
    script = """
import runpy, sys

def guard(event, args):
    if event == 'import' and args[0].split('.')[0] in {'OrcFxAPI', 'OrcWaveAPI', 'aqwa'}:
        raise AssertionError('licensed interface import attempted')
sys.addaudithook(guard)
sys.argv = ['configuration', *sys.argv[1:]]
runpy.run_module('digitalmodel.subsea.riser_study.riser_configuration_plot', run_name='__main__')
"""
    out = tmp_path / "out"
    result = subprocess.run([sys.executable, "-c", script, str(source), str(out),
                             "--reference-pair", "R08-W1"], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    manifest = json.loads((out / "SCH-01.manifest.json").read_text())
    assert manifest["input_sha256"] == hashlib.sha256(source.read_bytes()).hexdigest()
    artifact = manifest["artifacts"][0]
    with (out / artifact["csv_file"]).open(newline="") as stream:
        assert list(csv.DictReader(stream)) == [{k: str(v) for k, v in r.items()} for r in artifact["rows"]]
    svg = ET.parse(out / artifact["file"]).getroot()
    assert json.loads(svg.find("s:metadata", NS).text) == artifact["rows"]
    assert len(svg.findall('.//s:circle[@data-row]', NS)) == len(artifact["rows"])
    assert svg.find('.//s:polyline[@data-series="buoyancy"]', NS) is not None
