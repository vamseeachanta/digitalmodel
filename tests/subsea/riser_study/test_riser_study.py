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


def screening_record(strength, size="08", wall="1", kind="SCR", *,
                     condition="extreme", dynamic=True, offset=0, direction=0):
    """ASSUMED exports with independent fibre stresses and load histories."""
    data = deepcopy(strength)
    data["case_id"] = f"R{size}-W{wall}-{kind}"
    data["load_case"]["id"] = f"{condition}-{dynamic}-{offset}-{direction}"
    data["load_case"]["wave"]["direction_deg"] = direction
    data["load_case"]["offset"]["x_m"] = offset
    scale = (0.75 if wall == "2" else 1) * (1.1 if kind == "SLWR" else 1)
    fibres = {"inner": [120*scale, 180*scale, 150*scale],
              "outer": [110*scale, 210*scale, 130*scale]}
    extreme = {"inner": [144*scale, 216*scale, 180*scale],
               "outer": [180*scale, 288*scale, 200*scale]}
    minimum = [110, 80, -2] if kind == "SCR" else [100, 70, 60]
    data["top_angle_deg"]["static"] = 10 + offset/90
    data["touchdown"]["stress_range_MPa"] = (
        (12 if kind == "SCR" else 9) * (0.8 if wall == "2" else 1) if dynamic else 0)
    data["screening"] = {
        "condition": condition, "dynamic": dynamic, "qualified": True,
        "u1": {"method": "API RP 2RD", "smys_MPa": 450,
               "stress_MPa": {"static": fibres, "dynamic_max": extreme if dynamic else None}},
        "tension_envelope_kN": {"dynamic_min": minimum if dynamic else None,
                                "dynamic_max": [140, 110, 90] if dynamic else None},
        "hang_off_dynamic_min_kN": minimum[0] if dynamic else None,
        "excursion_reference_deg": 10, "excursion_limit_deg": 10,
        "static_check": ({"analytical": {"top_tension_kN": 100, "top_angle_deg": 9,
                                         "touchdown_x_m": 99},
                          "software_touchdown_x_m": 100, "basis": "ASSUMED synthetic catenary"}
                         if condition == "functional" else None),
        "api_rp_1111": {"in_scope": True, "method": "ASSUMED exported combined-loading check",
                        "reason": "ASSUMED synthetic history", "checks": [
                            {"arc_length_m": 50, "utilisation": 0.4*scale,
                             "check": "combined loading"}]},
    }
    return data


def write_cases(tmp_path, strength, *, dynamic_directions=(0, 180), dynamic_offsets=(0,)):
    """Write twelve cases with reference, mean-offset and nominal dynamic exports."""
    results = tmp_path / "results"
    for size in ("08", "10", "12"):
        for wall in ("1", "2"):
            for kind in ("SCR", "SLWR"):
                conditions = [("functional", False, 0, 0)]
                conditions += [("extreme", False, x, d) for x in (-90, 0, 90) for d in (0, 180)]
                conditions += [("extreme", True, x, d) for x in dynamic_offsets for d in dynamic_directions]
                for condition, dynamic, offset, direction in conditions:
                    data = screening_record(strength, size, wall, kind, condition=condition,
                                            dynamic=dynamic, offset=offset, direction=direction)
                    folder = results / data["case_id"] / data["load_case"]["id"]
                    folder.mkdir(parents=True)
                    (folder / "strength.json").write_text(json.dumps(data))
    return results


FIGURES = {f"FIG-STR-{measure}-{size}" for measure in ("TEFF", "U1", "HOT", "ANG", "TDR")
           for size in ("08", "10", "12")} | {"FIG-STR-SUM-01"}
TABLES = {"TAB-STA-01", "TAB-STR-1111-01"}
VALUES = {"V-STR-" + name for name in ("GOV-CASE", "UMAX-GOV", "GOV-LOC", "UMAX-SCR",
          "UMAX-SLWR", "WT-DELTA", "TEFF-MIN", "COMP-COUNT", "UANG-MAX", "TDR-DELTA-REF",
          "DYN-BASIS")} | {"V-SW-VERSION"}


def test_every_svg_value_matches_csv_manifest_and_sources(tmp_path, strength):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = write_cases(tmp_path, strength)
    out = tmp_path / "figures"
    manifest = build_strength(results, out, reference_pair="R08-W1", touchdown_window_m=50)
    expected = FIGURES | TABLES
    assert {a["id"] for a in manifest["artifacts"]} == expected
    assert json.loads((out / "manifest.json").read_text()) == manifest
    for artifact in manifest["artifacts"]:
        rows = artifact["rows"]
        with (out / artifact["csv_file"]).open(newline="") as stream:
            csv_rows = list(csv.DictReader(stream))
        assert csv_rows == [{k: str(v) for k, v in r.items()} for r in rows]
        if artifact["id"] in TABLES:
            continue
        svg = ET.parse(out / artifact["file"]).getroot()
        assert json.loads(svg.find("s:metadata", NS).text) == rows
        marks = sorted(svg.findall('.//s:circle[@data-row]', NS), key=lambda m: int(m.attrib["data-row"]))
        assert len(marks) == len(rows)
        for mark, row in zip(marks, rows):
            assert float(mark.attrib["data-x"]) == row["x"]
            assert float(mark.attrib["data-y"]) == row["y"]
            # Check actual geometry, not only embedded metadata.
            panel = next(p for p in svg.findall('.//s:g[@data-panel]', NS)
                         if mark in p.findall('.//s:circle[@data-row]', NS))
            axes = next(a for a in panel.findall('.//s:g[@data-axis]', NS)
                        if a.attrib["data-axis"] == row.get("axis", "left")).attrib
            assert float(mark.attrib["cx"]) == pytest.approx(
                float(axes["data-x-origin"]) + row["x"] * float(axes["data-x-scale"]), abs=1e-6)
            assert float(mark.attrib["cy"]) == pytest.approx(
                float(axes["data-y-origin"]) + row["y"] * float(axes["data-y-scale"]), abs=1e-6)
        assert "stroke-dasharray" in (out / artifact["file"]).read_text()
    tokens = {r["id"]: r for r in json.loads((out / "tokens.json").read_text())}
    assert tokens["V-STR-GOV-CASE"]["value"] == "R08-W1-SLWR"
    assert tokens["V-STR-UMAX-GOV"]["value"] == pytest.approx(288*1.1/360)
    assert tokens["V-STR-UMAX-SCR"]["value"] == pytest.approx(288/360)
    assert tokens["V-STR-UMAX-SLWR"]["value"] == pytest.approx(288*1.1/360)
    assert set(tokens) == FIGURES | TABLES | VALUES
    assert {p.name for p in out.iterdir()} == {"tokens.json", "manifest.json"} | {
        t + ext for t in FIGURES for ext in (".svg", ".csv")} | {
        t + ext for t in TABLES for ext in (".md", ".csv")}
    assert tokens["V-STR-WT-DELTA"]["value"] == pytest.approx(-25)
    assert tokens["V-STR-WT-DELTA"]["unit"] == "%"
    assert tokens["V-STR-TDR-DELTA-REF"]["value"] == -25
    assert tokens["V-STR-TDR-DELTA-REF"]["unit"] == "%"
    assert tokens["V-STR-TEFF-MIN"]["value"] == -2
    assert tokens["V-STR-TEFF-MIN"]["unit"] == "kN"
    assert tokens["V-STR-COMP-COUNT"]["value"] == 6
    assert tokens["V-STR-UANG-MAX"]["value"] == pytest.approx(0.3)
    assert tokens["V-STR-DYN-BASIS"]["value"] == "nominal offset, two screened directions"
    assert "outer" in tokens["V-STR-GOV-LOC"]["value"]
    assert tokens["V-SW-VERSION"]["value"] == "synthetic fixture"
    for source in manifest["sources"]:
        actual = results / source["file"].removeprefix("results/")
        assert source["sha256"] == hashlib.sha256(actual.read_bytes()).hexdigest()
    for artifact in manifest["artifacts"]:
        if artifact["id"] in TABLES:
            continue
        assert artifact["rows"]
        for datum in artifact["rows"]:
            if datum["case_id"]:
                assert datum["input_sha256"]
                assert datum["result_sha256"]
    summary = next(a for a in manifest["artifacts"] if a["id"] == "FIG-STR-SUM-01")
    assert len({r["panel"] for r in summary["rows"]}) == 5
    assert manifest["run_counts"] == {"records": 108, "qualified": 108, "dynamic": 24,
                                       "static": 84, "cases": 12}
    curve = next(a for a in manifest["artifacts"] if a["id"] == "FIG-STR-TEFF-08")
    assert [r["y"] for r in curve["rows"] if r["case_id"] == "R08-W1-SCR"
            and r["series"] == "functional:static:equilibrium"] == [120, 90, 70]
    utilisation = next(a for a in manifest["artifacts"] if a["id"] == "FIG-STR-U1-08")
    assert {r["y"] for r in utilisation["rows"] if r["series"] == "screen"} == {1}
    for measure in ("ANG", "HOT"):
        artifact = next(a for a in manifest["artifacts"] if a["id"] == f"FIG-STR-{measure}-08")
        assert {r["series"].split(":")[1] for r in artifact["rows"]
                if r["case_id"] and r["axis"] == "left"} == {"static", "dynamic_min", "dynamic_max"}


def test_bad_batch_writes_nothing_and_requires_explicit_reference(tmp_path, strength):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = write_cases(tmp_path, strength)
    for reference, window in [("R14-W1", 50), ("R08-W1", -1)]:
        with pytest.raises(ValueError):
            build_strength(results, tmp_path / "out", reference_pair=reference, touchdown_window_m=window)
    wrong = next((results / "R08-W1-SLWR").rglob("strength.json"))
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
    """Per-size figures retain both walls and configurations with all offsets."""
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = write_cases(tmp_path, strength)
    for path in results.rglob("strength.json"):
        data = json.loads(path.read_text())
        if "-W2-" in data["case_id"]:
            data["software"]["version"] = "fixture-alternate"
            path.write_text(json.dumps(data))
    output = tmp_path / "output"
    manifest = build_strength(results, output, reference_pair="R12-W2", touchdown_window_m=1)
    for artifact in manifest["artifacts"]:
        cases = {r["case_id"] for r in artifact["rows"] if r["case_id"]}
        assert len(cases) == (4 if artifact["id"][-2:] in {"08", "10", "12"} else 12)
        if artifact["id"] in TABLES:
            continue
        svg = ET.parse(output / artifact["file"]).getroot()
        assert "SYNTHETIC" in " ".join(svg.itertext())
        for panel in svg.findall('.//s:g[@data-panel]', NS):
            for line in panel.findall('.//s:polyline[@data-case]', NS):
                rows = [r for r in artifact["rows"] if r["case_id"] == line.attrib["data-case"]
                        and r["series"] == line.attrib["data-series"]
                        and r["panel"] == panel.attrib["data-panel"]
                        and r["axis"] == line.attrib["data-axis"]]
                axes = next(a for a in panel.findall('.//s:g[@data-axis]', NS)
                            if a.attrib["data-axis"] == line.attrib["data-axis"]).attrib
                assert len(line.attrib["points"].split()) == len(rows)
                for point, datum in zip(line.attrib["points"].split(), rows):
                    x, y = map(float, point.split(","))
                    assert x == pytest.approx(float(axes["data-x-origin"]) + datum["x"]*float(axes["data-x-scale"]), abs=1e-6)
                    assert y == pytest.approx(float(axes["data-y-origin"]) + datum["y"]*float(axes["data-y-scale"]), abs=1e-6)
    tokens = {t["id"]: t for t in json.loads((output / "tokens.json").read_text())}
    assert tokens["V-STR-GOV-CASE"]["value"] == "R08-W1-SLWR"
    assert tokens["V-SW-VERSION"]["value"] == "synthetic fixture; synthetic fixture-alternate"
    assert len(tokens["V-SW-VERSION"]["source_file"]) == 108


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


def test_intermediate_touchdown_is_retained_in_source_provenance(tmp_path, strength):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    strength["touchdown"] = {"arc_length_m": 75, "utilisation": 0.55, "stress_range_MPa": 12}
    results = write_cases(tmp_path, strength)
    manifest = build_strength(results, tmp_path / "out", reference_pair="R08-W1", touchdown_window_m=1)
    assert all(s["record"]["touchdown"]["arc_length_m"] == 75 for s in manifest["sources"])
    assert not any(a["id"] == "FIG-STR-04" for a in manifest["artifacts"])


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


@pytest.mark.parametrize("defect", ["missing_screening", "missing_fibre", "stress_length",
    "negative_stress", "sigma_over_smys", "reversed_envelope", "hangoff_envelope",
    "missing_static_check", "missing_case", "duplicate", "cross_offset", "wrong_method",
    "unqualified_reference", "different_wave", "mixed_dynamic_coverage"])
def test_d3_invalid_batch_is_rejected_before_output(tmp_path, strength, defect):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = write_cases(tmp_path, strength)
    path = results / "R08-W1-SCR" / "functional-False-0-0" / "strength.json"
    data = json.loads(path.read_text())
    screen = data["screening"]
    if defect == "missing_screening":
        del data["screening"]
    elif defect == "missing_fibre":
        del screen["u1"]["stress_MPa"]["static"]["outer"]
    elif defect == "stress_length":
        screen["u1"]["stress_MPa"]["static"]["inner"].pop()
    elif defect == "negative_stress":
        screen["u1"]["stress_MPa"]["static"]["inner"][0] = -1
    elif defect == "sigma_over_smys":
        screen["u1"]["method"] = "sigma/SMYS"
    elif defect == "reversed_envelope":
        path = results / "R08-W1-SCR/extreme-True-0-0/strength.json"
        data = json.loads(path.read_text())
        data["screening"]["tension_envelope_kN"]["dynamic_min"][1] = 200
    elif defect == "hangoff_envelope":
        path = results / "R08-W1-SCR/extreme-True-0-0/strength.json"
        data = json.loads(path.read_text())
        data["screening"]["hang_off_dynamic_min_kN"] = 5
    elif defect == "missing_static_check":
        screen["static_check"] = None
    elif defect == "missing_case":
        data["case_id"] = "R08-W2-SCR"
    elif defect == "duplicate":
        extra = path.parent.parent / "duplicate"
        extra.mkdir()
        (extra / "strength.json").write_text(json.dumps(data))
    elif defect == "cross_offset":
        data["load_case"]["offset"]["y_m"] = 90
    elif defect == "wrong_method":
        screen["api_rp_1111"]["method"] = ""
    elif defect == "unqualified_reference":
        screen["qualified"] = False
    elif defect == "different_wave":
        data["load_case"]["wave"]["Hs_m"] = 1
    elif defect == "mixed_dynamic_coverage":
        path = results / "R08-W1-SCR/extreme-True-0-180/strength.json"
        data = json.loads(path.read_text())
        data["screening"]["qualified"] = False
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError):
        build_strength(results, tmp_path / "out", reference_pair="R08-W1", touchdown_window_m=50)
    assert not (tmp_path / "out").exists()


def test_d3_profile_values_ratios_and_static_table_are_hand_checked(tmp_path, strength):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = write_cases(tmp_path, strength)
    manifest = build_strength(results, tmp_path / "out", reference_pair="R08-W1", touchdown_window_m=50)
    artifacts = {a["id"]: a for a in manifest["artifacts"]}
    u1 = [r for r in artifacts["FIG-STR-U1-08"]["rows"] if r["case_id"] == "R08-W1-SCR"]
    assert any(r["y"] == 210/300 and "functional:static:outer" in r["series"] for r in u1)
    assert any(r["y"] == 288/360 and "extreme:dynamic_max:outer" in r["series"] for r in u1)
    teff = artifacts["FIG-STR-TEFF-08"]["rows"]
    assert {r["y"] for r in teff if "dynamic_min" in r["series"]} == {-2, 60, 70, 80, 100, 110}
    for measure in ("HOT", "ANG", "TDR"):
        rows = artifacts[f"FIG-STR-{measure}-08"]["rows"]
        assert {r["x"] for r in rows if r["case_id"]} == {-90, 0, 90}
    tdr = artifacts["FIG-STR-TDR-08"]["rows"]
    assert all(r["y"] == pytest.approx(0.75) for r in tdr if r["axis"] == "right" and r["case_id"])
    assert {r["direction_deg"] for r in tdr if r["axis"] == "right" and r["case_id"]} == {0, 180}
    hot = artifacts["FIG-STR-HOT-08"]["rows"]
    assert {r["y"] for r in hot if "dynamic_min" in r["series"]} == {100, 110}
    sta = artifacts["TAB-STA-01"]["rows"]
    first = next(r for r in sta if r["case_id"] == "R08-W1-SCR" and r["measure"] == "top_tension")
    assert first["software"] == 120 and first["analytical"] == 100
    assert first["difference"] == 20 and first["difference_pct"] == 20
    assert all(r["utilisation"] >= 0 for r in artifacts["TAB-STR-1111-01"]["rows"])


def test_d3_static_only_zero_range_and_out_of_scope_are_explicit(tmp_path, strength):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = tmp_path / "results"
    for size in ("08", "10", "12"):
        for wall in ("1", "2"):
            for kind in ("SCR", "SLWR"):
                data = screening_record(strength, size, wall, kind, condition="functional", dynamic=False)
                data["screening"]["api_rp_1111"] = {"in_scope": False, "method": "not applicable",
                    "reason": "ASSUMED out-of-scope fixture", "checks": []}
                folder = results / data["case_id"]
                folder.mkdir(parents=True)
                (folder / "strength.json").write_text(json.dumps(data))
    out = tmp_path / "out"
    manifest = build_strength(results, out, reference_pair="R08-W1", touchdown_window_m=50)
    tokens = {t["id"]: t for t in json.loads((out / "tokens.json").read_text())}
    assert tokens["V-STR-DYN-BASIS"]["value"] == "not established"
    assert tokens["V-STR-TDR-DELTA-REF"]["value"] == "not established"
    assert manifest["unresolved_ratios"]
    table = next(a for a in manifest["artifacts"] if a["id"] == "TAB-STR-1111-01")
    assert all(r["utilisation"] == "not in scope" for r in table["rows"])


@pytest.mark.parametrize("directions,offsets,expected", [
    ((0,), (0,), "nominal offset, one screened direction"),
    ((0, 180), (0,), "nominal offset, two screened directions"),
    ((0, 180), (-90, 0, 90), "near, nominal and far offsets"),
])
def test_dynamic_basis_tracks_actual_offsets_and_directions(tmp_path, strength, directions, offsets, expected):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    results = write_cases(tmp_path, strength, dynamic_directions=directions, dynamic_offsets=offsets)
    out = tmp_path / "out"
    manifest = build_strength(results, out, reference_pair="R10-W1", touchdown_window_m=50)
    tokens = {t["id"]: t for t in json.loads((out / "tokens.json").read_text())}
    assert tokens["V-STR-DYN-BASIS"]["value"] == expected
    for measure in ("TEFF", "HOT", "ANG", "TDR"):
        artifact = next(a for a in manifest["artifacts"] if a["id"] == f"FIG-STR-{measure}-10")
        dynamic = [r for r in artifact["rows"] if r["case_id"] and "dynamic" in r["series"]]
        assert {r["span_offset_m"] for r in dynamic} == set(offsets)
        assert {r["direction_deg"] for r in dynamic} == set(directions)
    # Every span series progresses in offset; mean and history-reference statics
    # must not create a line that doubles back to nominal from the far offset.
    hot = next(a for a in manifest["artifacts"] if a["id"] == "FIG-STR-HOT-10")
    svg = ET.parse(out / hot["file"]).getroot()
    for line in svg.findall('.//s:polyline[@data-case]', NS):
        xs = [float(p.split(",")[0]) for p in line.attrib["points"].split()]
        assert xs == sorted(xs)


def test_legacy_output_destination_is_preserved_and_rejected(tmp_path, strength):
    from digitalmodel.subsea.riser_study.riser_strength_runs import build_strength
    out = tmp_path / "out"
    out.mkdir()
    legacy = out / "FIG-STR-02.svg"
    legacy.write_text("existing artifact")
    with pytest.raises(ValueError, match="deprecated"):
        build_strength(tmp_path / "results", out, reference_pair="R08-W1", touchdown_window_m=50)
    assert legacy.read_text() == "existing artifact"
    assert not (out / "tokens.json").exists()
