# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic fatigue contracts; no solver or standard data is required.

Hand basis: ASSUMED m=3, log10(a)=12, range=100 MPa, n=1,000,000/y,
SCF=1 and occurrence=0.2. N=10^12/100^3=1,000,000 cycles;
D=0.2*n/N=0.2/y. Halving range reduces damage by a factor of eight. Histogram basis: 100,000 cycles at each of 100 and 200 MPa:
D=0.2*(100,000/1,000,000 + 100,000/125,000)=0.18/y.
All geometry, dates and other values below are ASSUMED synthetic inputs.
"""

import copy
import csv
import importlib
import json
import math
import os
import subprocess
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

import pytest


def api():
    return importlib.import_module("digitalmodel.riser_study.riser_fatigue_runs")


def schema():
    return importlib.import_module("digitalmodel.riser_study.results_exchange")


def record(case="R08-W1-SCR", sea="S1", stress=100.0, distribution=True):
    point = {"arc_length_m": 1000.0, "stress_range_MPa": stress,
             "cycles_per_year": 1_000_000.0, "scf": 1.0}
    data = {
        "case_id": case,
        "sea_state": {"id": sea, "Hs_m": 2.0, "Tp_s": 8.0,
                      "direction_deg": 0.0, "occurrence": 0.2},
        "hot_spots": [{"name": "touchdown", **point},
                      {"name": "hang_off", **point, "arc_length_m": 0.0,
                       "stress_range_MPa": stress / 2}],
        "sn_curve": {"name": "ASSUMED synthetic power law", "source_label": "ASSUMED",
                     "m": 3.0, "log_a": 12.0, "thickness_correction": None},
        "damage_per_year": 0.2 * (stress / 100) ** 3,
        "run": {"input_sha256": "a" * 64, "result_file": "synthetic-results.json",
                "date": "2026-10-25", "data_basis": "synthetic"},
    }
    if distribution:
        data["arc_distribution"] = [{**point, "arc_length_m": 0.0,
                                     "stress_range_MPa": stress / 2}, point]
    return data


def campaign(root, distribution=True):
    """Write twelve synthetic cases and three common sea states per brief."""
    for size in ("08", "10", "12"):
        for wall in ("W1", "W2"):
            for config in ("SCR", "SLWR"):
                case = f"R{size}-{wall}-{config}"
                for sea, stress in (("S1", 100.0), ("S2", 50.0), ("S3", 25.0)):
                    stress /= (2 if wall == "W2" else 1) * (2 if config == "SLWR" else 1)
                    folder = root / case
                    folder.mkdir(parents=True, exist_ok=True)
                    (folder / f"fatigue-{sea}.json").write_text(
                        json.dumps(benchmark_record(case, sea, stress, distribution)), encoding="utf-8")
    return root


def test_hand_miner_damage():
    rows = api().calculate(record())
    assert rows[0]["damage_per_year"] == pytest.approx(0.2)
    assert rows[1]["damage_per_year"] == pytest.approx(0.025)
    assert all("life_years" not in row for row in rows)


def test_histogram_scf_and_recorded_thickness():
    data = record(distribution=False)
    spot = data["hot_spots"][0]
    del spot["stress_range_MPa"]
    spot["range_histogram"] = [{"stress_range_MPa": 100.0, "cycles_per_year": 100_000},
                               {"stress_range_MPa": 200.0, "cycles_per_year": 100_000}]
    spot["cycles_per_year"] = 200_000
    data["damage_per_year"] = 0.18
    assert api().calculate(data)[0]["damage_per_year"] == pytest.approx(0.18)
    spot["scf"] = 2.0
    data["damage_per_year"] = 1.44
    assert api().calculate(data)[0]["damage_per_year"] == pytest.approx(1.44)
    # ASSUMED correction (40/20)^1=2; m=3 increases damage by 8.
    data["sn_curve"]["thickness_correction"] = {
        "thickness_mm": 40.0, "reference_thickness_mm": 20.0,
        "exponent": 1.0, "source_label": "ASSUMED synthetic correction"}
    data["damage_per_year"] = 11.52
    assert api().calculate(data)[0]["damage_per_year"] == pytest.approx(11.52)


@pytest.mark.parametrize("field,value", [
    ("case_id", "bad"), ("life_years", -1), ("damage_per_year", float("nan")),
    ("damage_per_year", True), ("run", {}), ("hot_spots", []),
])
def test_schema_rejects_invalid_records(field, value):
    data = record()
    data[field] = value
    with pytest.raises(ValueError):
        schema().validate_result(data, "fatigue")


@pytest.mark.parametrize("mutation", [
    lambda d: d["sea_state"].update(occurrence=1.1),
    lambda d: d["sn_curve"].update(m=0),
    lambda d: d["sn_curve"].update(source_label=""),
    lambda d: d["run"].update(input_sha256="invalid"),
    lambda d: d["run"].update(date="2026-99-99"),
    lambda d: d["hot_spots"][0].update(scf=0),
    lambda d: d["hot_spots"][0].update(name="unknown"),
    lambda d: d["hot_spots"].append(copy.deepcopy(d["hot_spots"][0])),
    lambda d: d["arc_distribution"].reverse(),
    lambda d: d["hot_spots"][0].update(range_histogram=[]),
])
def test_schema_nested_validation(mutation):
    data = record()
    mutation(data)
    with pytest.raises(ValueError):
        schema().validate_result(data, "fatigue")


def test_strength_exchange_schema():
    data = {
        "case_id": "R08-W1-SCR", "software": {"name": "synthetic", "version": "ASSUMED"},
        "load_case": {"id": "synthetic", "wave": {}, "current": {}, "offset": {}},
        "arc_length_m": [0.0, 1000.0], "effective_tension_kN": [100.0, 50.0],
        "bending_moment_kNm": [0.0, 10.0], "von_mises_utilisation": [0.1, 0.2],
        "utilisation_method": "ASSUMED synthetic ratio", "utilisation_allowable": 1.0,
        "max_utilisation": {"value": 0.2, "arc_length_m": 1000.0, "location": "touchdown"},
        "touchdown": {"arc_length_m": 1000.0, "utilisation": 0.2, "stress_range_MPa": 100.0},
        "top_angle_deg": {"static": 10.0, "dynamic_min": 9.0, "dynamic_max": 11.0},
        "hang_off_tension_kN": {"static": 100.0, "dynamic_max": 110.0},
        "run": record()["run"],
    }
    schema().validate_result(data, "strength")
    data["effective_tension_kN"].pop()
    with pytest.raises(ValueError, match="length"):
        schema().validate_result(data, "strength")


def test_recorded_damage_cannot_silently_disagree():
    data = record()
    data["damage_per_year"] = 2.0
    with pytest.raises(ValueError, match="recorded"):
        api().calculate(data)


def test_histogram_cycle_total_must_match():
    data = record()
    spot = data["hot_spots"][0]
    del spot["stress_range_MPa"]
    spot["range_histogram"] = [{"stress_range_MPa": 100.0, "cycles_per_year": 1.0}]
    with pytest.raises(ValueError, match="cycles"):
        api().calculate(data)


def test_zero_damage_has_no_reciprocal_output():
    data = record(stress=0)
    rows = api().calculate(data)
    assert all(row["damage_per_year"] == 0 and "life_years" not in row for row in rows)


@pytest.mark.parametrize("fault", ["missing", "filename", "case", "sea", "wall"])
def test_incomplete_or_inconsistent_campaign_is_rejected(tmp_path, fault):
    root = campaign(tmp_path / "results")
    path = root / "R08-W1-SCR/fatigue-S1.json"
    if fault == "missing":
        path.rename(path.with_suffix(".excluded"))
    else:
        data = json.loads(path.read_text())
        if fault == "filename":
            path.rename(path.with_name("fatigue-other.json"))
        elif fault == "case":
            data["case_id"] = "R10-W1-SCR"
        elif fault == "sea":
            data["sea_state"]["Hs_m"] = 99.0
        else:
            for sea in ("S1", "S2", "S3"):
                p = root / "R08-W2-SCR" / f"fatigue-{sea}.json"
                p.rename(p.with_suffix(".excluded"))
        if fault in ("case", "sea"):
            path.write_text(json.dumps(data))
    with pytest.raises(ValueError):
        api().produce(root, tmp_path / "out")
    assert not (tmp_path / "out/tokens.json").exists()


def test_cli_reads_exports_only(tmp_path):
    root = campaign(tmp_path / "results")
    proc = subprocess.run([sys.executable, "-m", "digitalmodel.riser_study.riser_fatigue_runs",
                           str(root), str(tmp_path / "out")], capture_output=True, text=True,
                          env={**os.environ, "PYTHONPATH": str(Path(__file__).resolve().parents[3] / "src")})
    assert proc.returncode == 0, proc.stderr
    assert (tmp_path / "out/tokens.json").exists()


def test_shared_schema_is_valid_and_rejects_wrong_kind():
    from jsonschema import Draft202012Validator

    document = schema().exchange_schema()
    Draft202012Validator.check_schema(document)
    Draft202012Validator(document).validate(record())
    with pytest.raises(ValueError):
        schema().validate_result(record(), "strength")


def test_closed_form_benchmark_arc_locations_are_preserved():
    """Use repository closed-form geometry only; fatigue remains synthetic."""
    reference = Path(__file__).resolve().parents[3] / (
        "docs/domains/orcaflex/library/campaigns/generic-scr-slwr/static-reference.json")
    benchmark = json.loads(reference.read_text())
    data = record(distribution=False)
    # Geometry comes from the retained closed-form SCR benchmark, not a solver.
    length = benchmark["models"]["scr"]["oo"]["suspended_length_m"]
    data["hot_spots"][0]["arc_length_m"] = length
    rows = api().calculate(data)
    assert rows[0]["arc_length_m"] == length


def test_same_case_hot_spot_set_must_match_across_states(tmp_path):
    root = campaign(tmp_path / "results")
    path = root / "R08-W1-SCR/fatigue-S3.json"
    data = json.loads(path.read_text())
    data["hot_spots"].pop()
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match="hot-spot coverage"):
        api().produce(root, tmp_path / "out")


def test_occurrence_cannot_exceed_full_annual_exposure(tmp_path):
    root = campaign(tmp_path / "results")
    for path in root.glob("*/fatigue-*.json"):
        data = json.loads(path.read_text())
        data["sea_state"]["occurrence"] = 0.5
        path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match="sum above one"):
        api().produce(root, tmp_path / "out")


def test_numeric_overflow_is_rejected():
    data = record()
    data["sn_curve"]["log_a"] = -1000.0
    with pytest.raises(ValueError, match="numeric range"):
        api().calculate(data)


def test_zero_occurrence_and_zero_cycles_have_no_damage():
    data = record()
    data["sea_state"]["occurrence"] = 0.0
    data["damage_per_year"] = 0.0
    assert api().calculate(data)[0]["damage_per_year"] == 0.0
    data = record()
    for point in data["hot_spots"] + data["arc_distribution"]:
        point["cycles_per_year"] = 0.0
    data["damage_per_year"] = 0.0
    assert api().calculate(data)[0]["damage_per_year"] == 0.0


def test_absent_thickness_record_adds_no_correction():
    data = record()
    del data["sn_curve"]["thickness_correction"]
    assert api().calculate(data)[0]["thickness_factor"] == 1.0


@pytest.mark.parametrize("windows_newlines", [False, True])
def test_input_digests_and_artifact_digests(tmp_path, monkeypatch, windows_newlines):
    import hashlib

    if windows_newlines:
        # Exercise CRLF text-stream translation even on a non-Windows host.
        original_open = Path.open

        def windows_open(path, mode="r", buffering=-1, encoding=None, errors=None, newline=None):
            if "w" in mode and "b" not in mode and newline is None:
                newline = "\r\n"
            return original_open(path, mode, buffering, encoding, errors, newline)

        monkeypatch.setattr(Path, "open", windows_open)
    root = campaign(tmp_path / "results")
    out = tmp_path / "out"
    api().produce(root, out)
    manifest = json.loads((out / "manifest.json").read_text())
    assert manifest["run_record_sha256"] == hashlib.sha256((out / "run_record.json").read_bytes()).hexdigest()
    assert all(b"\r" not in path.read_bytes() for path in out.iterdir())
    for source in manifest["sources"]:
        assert source["sha256"] == hashlib.sha256((root / source["export"]).read_bytes()).hexdigest()
    for artifact in manifest["artifacts"].values():
        for kind in ("csv", "svg"):
            assert artifact[f"{kind}_sha256"] == hashlib.sha256((out / artifact[kind]).read_bytes()).hexdigest()
        assert all("3 sea states" in row["scope"] for row in artifact["rows"])


def test_governing_hot_spot_is_by_damage_not_peak_range():
    data = record(distribution=False)
    data["hot_spots"][0]["cycles_per_year"] = 1.0
    # Hang-off: 50 MPa and 1,000,000 cycles => D=0.025/y.
    data["damage_per_year"] = 0.025
    rows = api().calculate(data)
    assert max(rows, key=lambda row: row["damage_per_year"])["hot_spot"] == "hang_off"


def test_output_must_not_contaminate_result_discovery(tmp_path):
    root = campaign(tmp_path / "results")
    with pytest.raises(ValueError, match="outside"):
        api().produce(root, root / "output")


FIGURES = {"FIG-FAT-RNG-08", "FIG-FAT-RNG-10", "FIG-FAT-RNG-12", "FIG-FAT-HEAT-01"}
VALUES = {"V-FAT-DMAX", "V-FAT-GOV-CASE", "V-FAT-GOV-SS", "V-FAT-SCR-HOTSPOT",
          "V-FAT-SLWR-HOTSPOT", "V-FAT-WT-DELTA", "V-FAT-RESOLVED"}


def benchmark_record(case="R08-W1-SCR", sea="S1", stress=100.0, distribution=True):
    data = record(case, sea, stress, distribution)
    data["sn_curve"].update(name="FAT 80", m=3.0, log_a=math.log10(2_000_000 * 80**3),
                            reference_cycles=2_000_000, reference_range_MPa=80.0,
                            endurance_limit_MPa=None)
    for point in data["hot_spots"] + data.get("arc_distribution", []):
        point["scf"] = 1.2
    data["damage_per_year"] = 0.2 * 1_000_000 / 2_000_000 * (stress * 1.2 / 80)**3
    return data


def test_d3_tokens_and_no_removed_outputs(tmp_path):
    out = tmp_path / "out"
    result = api().produce(campaign(tmp_path / "results"), out)
    tokens = json.loads((out / "tokens.json").read_text())
    assert set(tokens["values"]) == VALUES
    assert set(result["manifest"]["artifacts"]) == FIGURES | {"TAB-FAT-01"}
    assert {p.name for p in out.iterdir()} == {"tokens.json", "manifest.json", "run_record.json"} | {
        f"{token}.{suffix}" for token in FIGURES | {"TAB-FAT-01"} for suffix in ("svg", "csv")}
    expected = sum(benchmark_record(sea=sea, stress=stress)["damage_per_year"]
                   for sea, stress in (("S1", 100), ("S2", 50), ("S3", 25)))
    assert tokens["values"]["V-FAT-DMAX"] == pytest.approx(expected)
    assert tokens["values"]["V-FAT-WT-DELTA"] == pytest.approx(-87.5)
    assert tokens["values"]["V-FAT-RESOLVED"] == "not resolved"
    assert tokens["units"]["V-FAT-WT-DELTA"] == "%"
    assert len(result["hot_spots"]) == 72
    for path in out.iterdir():
        text = path.read_text()
        assert "life_years" not in text
        assert "unbounded" not in text
        assert "air-equivalent benchmark damage, not service life" in text


def test_every_svg_value_equals_csv_and_manifest(tmp_path):
    out = tmp_path / "out"
    api().produce(campaign(tmp_path / "results"), out)
    manifest = json.loads((out / "manifest.json").read_text())
    for token, artifact in manifest["artifacts"].items():
        with (out / artifact["csv"]).open() as stream:
            rows = list(csv.DictReader(stream))
        assert rows == artifact["rows"]
        svg = ET.parse(out / artifact["svg"]).getroot()
        assert json.loads(svg.find("{http://www.w3.org/2000/svg}metadata").text) == rows
        marks = [node for node in svg.iter() if "data-row" in node.attrib]
        assert len(marks) == len(rows)
        for mark in marks:
            row = rows[int(mark.attrib["data-row"])]
            assert json.loads(mark.attrib["data-values"]) == row
            if token.startswith("FIG-FAT-RNG"):
                panel = artifact["panels"][row["hot_spot"]]
                assert float(mark.attrib["cy"]) == pytest.approx(
                    panel["bottom"] - float(row["stress_range_MPa"]) * panel["y_scale"])
                assert float(mark.attrib["cx"]) == artifact["sea_positions"][row["sea_state_id"]]
            elif token == "FIG-FAT-HEAT-01":
                value = float(row["damage_per_year"])
                assert mark.attrib["fill"] == api().heat_color(value, artifact["log_min"], artifact["log_max"])
                assert float(mark.attrib["x"]) == artifact["sea_positions"][row["sea_state_id"]]
                assert float(mark.attrib["y"]) == artifact["case_positions"][row["case_id"]]
    assert len(manifest["sources"]) == 36


def test_range_panels_and_heatmap_cover_matrix(tmp_path):
    out = tmp_path / "out"
    manifest = api().produce(campaign(tmp_path / "results"), out)["manifest"]
    for size in ("08", "10", "12"):
        artifact = manifest["artifacts"][f"FIG-FAT-RNG-{size}"]
        assert len(artifact["rows"]) == 24
        assert set(artifact["panels"]) == {"touchdown", "hang_off"}
        assert {row["case_id"] for row in artifact["rows"]} == {
            f"R{size}-{wall}-{config}" for wall in ("W1", "W2") for config in ("SCR", "SLWR")}
    heat = manifest["artifacts"]["FIG-FAT-HEAT-01"]
    assert len(heat["rows"]) == 72
    assert len(heat["case_positions"]) == 12
    assert len(heat["sea_positions"]) == 3
    assert heat["colour_scale"] == "log10; zero separate"


def test_same_station_sum_precedes_governing_selection(tmp_path):
    root = campaign(tmp_path / "results")
    # Different stations govern different states: summing state maxima is wrong.
    for sea, touchdown, hangoff in (("S1", 100, 10), ("S2", 10, 110), ("S3", 10, 10)):
        path = root / "R08-W1-SCR" / f"fatigue-{sea}.json"
        data = benchmark_record(sea=sea, stress=touchdown)
        data["hot_spots"][1]["stress_range_MPa"] = hangoff
        data["damage_per_year"] = 0.2 * 1_000_000 / 2_000_000 * (max(touchdown, hangoff)*1.2/80)**3
        path.write_text(json.dumps(data))
    manifest = api().produce(root, tmp_path / "out")["manifest"]
    expected = sum(0.2 * 1_000_000 / 2_000_000 * (s*1.2/80)**3 for s in (10, 110, 10))
    assert manifest["values"]["V-FAT-DMAX"] == pytest.approx(expected)
    assert manifest["governing"]["hot_spot"] == "hang_off"
    assert manifest["values"]["V-FAT-GOV-SS"] == "S2"


def test_station_coordinates_cannot_change_across_states(tmp_path):
    root = campaign(tmp_path / "results")
    path = root / "R08-W1-SCR/fatigue-S2.json"
    data = json.loads(path.read_text())
    data["hot_spots"][0]["arc_length_m"] += 1
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match="station"):
        api().produce(root, tmp_path / "out")


def test_recorded_fat80_inputs_and_hourly_miner_sum(tmp_path):
    out = tmp_path / "out"
    api().produce(campaign(tmp_path / "results"), out)
    run = json.loads((out / "run_record.json").read_text())
    assert len(run["inputs"]) == 36
    for row in run["inputs"]:
        curve = json.loads(row["sn_curve_record"])
        assert curve["name"] == "FAT 80"
        assert curve["m"] == 3
        assert curve["reference_cycles"] == 2_000_000
        assert curve["reference_range_MPa"] == 80
        assert curve["endurance_limit_MPa"] is None
        assert curve["log_a"] == pytest.approx(math.log10(2_000_000 * 80**3))
        assert all(point["scf"] == 1.2 for point in json.loads(row["hot_spot_inputs"]))
    data = benchmark_record()
    hourly_damage = (1_000_000 / 8760) / 2_000_000 * (100 * 1.2 / 80)**3
    assert api().calculate(data)[0]["damage_per_year"] == pytest.approx(hourly_damage * 8760 * 0.2)


@pytest.mark.parametrize("field,value", [("m", 4), ("reference_cycles", 1_000_000),
    ("reference_range_MPa", 100), ("endurance_limit_MPa", 10), ("log_a", 12)])
def test_campaign_rejects_wrong_benchmark_curve(tmp_path, field, value):
    root = campaign(tmp_path / "results")
    path = root / "R08-W1-SCR/fatigue-S1.json"
    data = json.loads(path.read_text())
    data["sn_curve"][field] = value
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match="benchmark"):
        api().produce(root, tmp_path / "out")


def test_zero_campaign_has_separate_zero_colour_and_undefined_percentage(tmp_path):
    root = campaign(tmp_path / "results")
    for path in root.glob("*/fatigue-*.json"):
        data = json.loads(path.read_text())
        path.write_text(json.dumps(benchmark_record(data["case_id"], data["sea_state"]["id"], 0)))
    out = tmp_path / "out"
    manifest = api().produce(root, out)["manifest"]
    assert manifest["values"]["V-FAT-DMAX"] == 0
    assert manifest["values"]["V-FAT-WT-DELTA"] is None
    assert manifest["values"]["V-FAT-RESOLVED"] == "not resolved"
    assert manifest["reciprocal_at_zero_damage"] == "not established"
    heat = ET.parse(out / "FIG-FAT-HEAT-01.svg").getroot()
    assert all(mark.attrib["fill"] == "#ffffff" for mark in heat.iter() if "data-row" in mark.attrib)


def test_incomplete_size_matrix_is_rejected(tmp_path):
    root = campaign(tmp_path / "results")
    for path in (root / "R12-W2-SLWR").glob("*.json"):
        path.rename(path.with_suffix(".excluded"))
    with pytest.raises(ValueError, match="twelve"):
        api().produce(root, tmp_path / "out")


def test_stale_output_directory_is_rejected_without_deleting_data(tmp_path):
    out = tmp_path / "out"
    out.mkdir()
    stale = out / "FIG-FAT-02.svg"
    stale.write_text("retained legacy data")
    with pytest.raises(ValueError, match="empty"):
        api().produce(campaign(tmp_path / "results"), out)
    assert stale.read_text() == "retained legacy data"


def g4_evidence():
    return {"comparison_cases": ["R08-W1-SCR", "R08-W2-SCR"],
            "mesh_step": {"wall_delta_pct": -87.4, "tolerance_pct": 1.0, "source_sha256": "b" * 64},
            "half_hour": {"wall_delta_pct": -87.3, "tolerance_pct": 1.0, "source_sha256": "c" * 64}}


def test_resolution_requires_both_g4_checks_for_the_same_wall_pair(tmp_path):
    root = campaign(tmp_path / "results")
    evidence = g4_evidence()
    result = api().produce(root, tmp_path / "resolved", g4_checks=evidence)
    assert result["manifest"]["values"]["V-FAT-RESOLVED"] == "resolved"
    assert result["manifest"]["resolution"]["checks"] == evidence
    evidence["half_hour"]["wall_delta_pct"] = -80.0
    result = api().produce(root, tmp_path / "failed-check", g4_checks=evidence)
    assert result["manifest"]["values"]["V-FAT-RESOLVED"] == "not resolved"
    evidence = g4_evidence()
    evidence["comparison_cases"] = ["R10-W1-SCR", "R10-W2-SCR"]
    with pytest.raises(ValueError, match="comparison"):
        api().produce(root, tmp_path / "wrong-pair", g4_checks=evidence)
    evidence = g4_evidence()
    del evidence["half_hour"]
    with pytest.raises(ValueError):
        api().produce(root, tmp_path / "missing-check", g4_checks=evidence)


def test_wall_change_must_exceed_measured_uncertainty(tmp_path):
    root = campaign(tmp_path / "results")
    evidence = g4_evidence()
    for check in ("mesh_step", "half_hour"):
        evidence[check].update(wall_delta_pct=0, tolerance_pct=100)
    result = api().produce(root, tmp_path / "out", g4_checks=evidence)
    assert result["manifest"]["values"]["V-FAT-RESOLVED"] == "not resolved"


def test_recorded_benchmark_scf_cannot_change(tmp_path):
    root = campaign(tmp_path / "results")
    path = root / "R08-W1-SCR/fatigue-S1.json"
    data = json.loads(path.read_text())
    data["hot_spots"][0]["scf"] = 1.0
    path.write_text(json.dumps(data))
    with pytest.raises(ValueError, match="benchmark"):
        api().produce(root, tmp_path / "out")



def test_heatmap_mixed_zero_is_not_a_logarithmic_floor(tmp_path):
    root = campaign(tmp_path / "results")
    path = root / "R08-W1-SCR/fatigue-S1.json"
    path.write_text(json.dumps(benchmark_record(stress=0)))
    out = tmp_path / "out"
    manifest = api().produce(root, out)["manifest"]
    heat = manifest["artifacts"]["FIG-FAT-HEAT-01"]
    positive = [float(row["damage_per_year"]) for row in heat["rows"] if float(row["damage_per_year"]) > 0]
    assert heat["log_min"] == pytest.approx(math.log10(min(positive)))
    svg = ET.parse(out / heat["svg"]).getroot()
    for mark in svg.iter():
        if "data-row" in mark.attrib:
            row = heat["rows"][int(mark.attrib["data-row"])]
            assert (mark.attrib["fill"] == "#ffffff") == (float(row["damage_per_year"]) == 0)


def test_each_wall_uses_its_own_governing_station_sum(tmp_path):
    root = campaign(tmp_path / "results")
    path = root / "R08-W2-SCR/fatigue-S2.json"
    path.write_text(json.dumps(benchmark_record("R08-W2-SCR", "S2", 80)))
    manifest = api().produce(root, tmp_path / "out")["manifest"]
    d1 = sum(0.2 * 1_000_000 / 2_000_000 * (s * 1.2 / 80)**3 for s in (100, 50, 25))
    d2 = sum(0.2 * 1_000_000 / 2_000_000 * (s * 1.2 / 80)**3 for s in (50, 80, 12.5))
    assert manifest["values"]["V-FAT-WT-DELTA"] == pytest.approx(100 * (d2 - d1) / d1)
    pair = manifest["wall_comparison"]
    assert pair["comparison_station"]["sea_state_id"] == "S2"
    assert pair["reference_station"]["sea_state_id"] == "S1"


def test_cli_accepts_shared_g4_evidence(tmp_path):
    root = campaign(tmp_path / "results")
    checks = tmp_path / "checks.json"
    checks.write_text(json.dumps(g4_evidence()))
    proc = subprocess.run([sys.executable, "-m", "digitalmodel.riser_study.riser_fatigue_runs",
                           str(root), str(tmp_path / "out"), "--g4-checks", str(checks)],
                          capture_output=True, text=True,
                          env={**os.environ, "PYTHONPATH": str(Path(__file__).resolve().parents[3] / "src")})
    assert proc.returncode == 0, proc.stderr
    tokens = json.loads((tmp_path / "out/tokens.json").read_text())
    assert tokens["values"]["V-FAT-RESOLVED"] == "resolved"


@pytest.mark.parametrize("recorded_damage", [0.0, 200_000.0])
def test_positive_stress_product_underflow_is_rejected(recorded_damage):
    """Exponent arithmetic gives 0.2 * 1e6 * 10**(-400 + 400) = 200000/y."""
    data = record(stress=1e-200, distribution=False)
    data["sn_curve"].update(m=1.0, log_a=-400.0)
    for point in data["hot_spots"]:
        point["scf"] = 1e-200
    data["damage_per_year"] = recorded_damage
    with pytest.raises(ValueError, match="corrected stress underflows"):
        api().calculate(data)


@pytest.mark.parametrize("factor,expected", [(1.0, 0.2), (2.0, 1.6)])
def test_common_fatigue_exchange_fixture(factor, expected):
    data = json.loads((Path(__file__).parent / "fixtures/fatigue-exchange.json").read_text())
    data["sn_curve"]["thickness_correction"] = factor
    data["damage_per_year"] = expected
    data["life_years"] = 1 / expected
    schema().validate_result(data, "fatigue")
    rows = api().calculate(data)
    assert rows[0]["damage_per_year"] == pytest.approx(expected)
    assert rows[0]["thickness_factor"] == factor
    assert data["sn_curve"]["source"] == "ASSUMED"
    assert all("life_years" not in row for row in rows)


def test_common_strength_basis_fields():
    data = {
        "case_id": "R08-W1-SCR", "software": {"name": "synthetic", "version": "ASSUMED"},
        "load_case": {"id": "synthetic", "wave": {"Hs_m": 0.0},
                      "current": {"speed_m_s": 0.0}, "offset": {"x_m": 0.0, "y_m": 0.0}},
        "arc_length_m": [0.0, 1000.0], "effective_tension_kN": [100.0, 50.0],
        "bending_moment_kNm": [0.0, 10.0], "von_mises_utilisation": [0.1, 0.2],
        "method": "ASSUMED synthetic ratio",
        "allowable": {"value": 100.0, "unit": "MPa", "source": "ASSUMED"},
        "max_utilisation": {"value": 0.2, "arc_length_m": 1000.0, "location": "touchdown"},
        "touchdown": {"arc_length_m": 1000.0, "utilisation": 0.2, "stress_range_MPa": 100.0},
        "top_angle_deg": {"static": 10.0, "dynamic_min": 9.0, "dynamic_max": 11.0},
        "hang_off_tension_kN": {"static": 100.0, "dynamic_max": 110.0},
        "run": record()["run"],
    }
    schema().validate_result(data, "strength")
    data["allowable"]["basis_label"] = "ASSUMED synthetic allowable"
    schema().validate_result(data, "strength")
    data["utilisation_method"] = data["method"]
    data["utilisation_allowable"] = 1.0
    with pytest.raises(ValueError):
        schema().validate_result(data, "strength")


def test_source_representations_cannot_be_mixed():
    data = record()
    data["sn_curve"]["source"] = "ASSUMED other source"
    with pytest.raises(ValueError):
        schema().validate_result(data, "fatigue")


@pytest.mark.parametrize("factor", [0.0, -1.0, True])
def test_numeric_thickness_factor_must_be_positive(factor):
    data = record()
    data["sn_curve"]["thickness_correction"] = factor
    with pytest.raises(ValueError):
        schema().validate_result(data, "fatigue")


def test_zero_common_exchange_and_legacy_life_consistency():
    data = json.loads((Path(__file__).parent / "fixtures/fatigue-exchange.json").read_text())
    for point in data["hot_spots"]:
        point["stress_range_MPa"] = 0.0
    data.update(damage_per_year=0.0, life_years=None)
    assert all(row["damage_per_year"] == 0 for row in api().calculate(data))
    data["life_years"] = 1.0
    with pytest.raises(ValueError):
        schema().validate_result(data, "fatigue")
    data = record()
    data["life_years"] = 10.0
    with pytest.raises(ValueError, match="reciprocal"):
        schema().validate_result(data, "fatigue")


def test_common_exchange_campaign_preserves_source_and_factor(tmp_path):
    root = campaign(tmp_path / "results")
    for path in root.glob("*/fatigue-*.json"):
        data = json.loads(path.read_text())
        data["sn_curve"]["source"] = data["sn_curve"].pop("source_label")
        data["sn_curve"]["thickness_correction"] = 1.0
        path.write_text(json.dumps(data))
    result = api().produce(root, tmp_path / "out")
    assert len(result["inputs"]) == 36
    assert all(row["sn_source_label"] == "ASSUMED" for row in result["inputs"])
    assert all(json.loads(row["sn_curve_record"])["thickness_correction"] == 1.0
               for row in result["inputs"])
    assert all(row["thickness_factor"] == 1.0 for row in result["hot_spots"])
    assert result["manifest"]["values"]["V-FAT-WT-DELTA"] == pytest.approx(-87.5)
