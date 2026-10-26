# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic fatigue contracts; no solver or standard data is required.

Hand basis: ASSUMED m=3, log10(a)=12, range=100 MPa, n=1,000,000/y,
SCF=1 and occurrence=0.2. N=10^12/100^3=1,000,000 cycles;
D=0.2*n/N=0.2/y and L=1/D=5 y. Halving range gives eight times
that life. Histogram basis: 100,000 cycles at each of 100 and 200 MPa:
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
        "life_years": 5.0 * (100 / stress) ** 3 if stress else None,
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
                        json.dumps(record(case, sea, stress, distribution)), encoding="utf-8")
    return root


def test_hand_miner_and_reciprocal_life():
    rows = api().calculate(record())
    assert rows[0]["damage_per_year"] == pytest.approx(0.2)
    assert rows[0]["life_years"] == pytest.approx(5.0)
    assert rows[1]["damage_per_year"] == pytest.approx(0.025)
    for row in rows:
        assert row["life_years"] == pytest.approx(1 / row["damage_per_year"])


def test_histogram_scf_and_recorded_thickness():
    data = record(distribution=False)
    spot = data["hot_spots"][0]
    del spot["stress_range_MPa"]
    spot["range_histogram"] = [{"stress_range_MPa": 100.0, "cycles_per_year": 100_000},
                               {"stress_range_MPa": 200.0, "cycles_per_year": 100_000}]
    spot["cycles_per_year"] = 200_000
    data["damage_per_year"], data["life_years"] = 0.18, 1 / 0.18
    assert api().calculate(data)[0]["damage_per_year"] == pytest.approx(0.18)
    spot["scf"] = 2.0
    data["damage_per_year"], data["life_years"] = 1.44, 1 / 1.44
    assert api().calculate(data)[0]["damage_per_year"] == pytest.approx(1.44)
    # ASSUMED correction (40/20)^1=2; m=3 increases damage by 8.
    data["sn_curve"]["thickness_correction"] = {
        "thickness_mm": 40.0, "reference_thickness_mm": 20.0,
        "exponent": 1.0, "source_label": "ASSUMED synthetic correction"}
    data["damage_per_year"], data["life_years"] = 11.52, 1 / 11.52
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


def test_zero_damage_is_explicit_unbounded_life():
    data = record(stress=0)
    rows = api().calculate(data)
    assert all(row["damage_per_year"] == 0 and row["life_years"] is None for row in rows)


def test_three_state_campaign_and_tokens(tmp_path):
    result = api().produce(campaign(tmp_path / "results"), tmp_path / "out")
    tokens = json.loads((tmp_path / "out/tokens.json").read_text())
    assert tokens["values"] == {
        "V-FAT-GOV-CASE": "R08-W1-SCR", "V-FAT-GOV-SS": "S1", "V-FAT-LMIN": pytest.approx(5.0),
        "V-FAT-SCR-HOTSPOT": "touchdown", "V-FAT-SLWR-HOTSPOT": "touchdown",
        "V-FAT-WT-FACTOR": pytest.approx(8.0)}
    assert len(result["hot_spots"]) == 72
    assert len(result["inputs"]) == 36
    assert "3 sea states" in tokens["scope"]
    assert tokens["wall_comparison"]["numerator"] == "R08-W2-SCR"
    assert tokens["wall_comparison"]["denominator"] == "R08-W1-SCR"


def test_every_svg_value_equals_csv_and_manifest(tmp_path):
    out = tmp_path / "out"
    api().produce(campaign(tmp_path / "results"), out)
    manifest = json.loads((out / "manifest.json").read_text())
    assert set(manifest["artifacts"]) == {
        "FIG-FAT-01", "FIG-FAT-02", "FIG-FAT-03", "TAB-FAT-01", "TAB-FAT-IN-01"}
    for token, artifact in manifest["artifacts"].items():
        with (out / artifact["csv"]).open() as stream:
            csv_rows = list(csv.DictReader(stream))
        assert csv_rows == artifact["rows"]
        svg = ET.parse(out / artifact["svg"]).getroot()
        metadata = svg.find("{http://www.w3.org/2000/svg}metadata")
        assert json.loads(metadata.text) == csv_rows
        plotted = [el for el in svg.iter() if "data-row" in el.attrib]
        assert len(plotted) == len(csv_rows)
        for mark in plotted:
            row = csv_rows[int(mark.attrib["data-row"])]
            assert json.loads(mark.attrib["data-values"]) == row
            if token.startswith("FIG"):
                value = float(row[artifact["y_field"]])
                transformed = math.log10(value) if artifact["log_y"] else value
                expected = artifact["plot_bottom"] - (
                    transformed - artifact["y_min"]) * artifact["y_scale"]
                assert float(mark.attrib["cy"]) == pytest.approx(expected)
    assert len(manifest["sources"]) == 36


def test_missing_arc_distribution_emits_explicit_unavailable_artifact(tmp_path):
    out = tmp_path / "out"
    api().produce(campaign(tmp_path / "results", distribution=False), out)
    artifact = json.loads((out / "manifest.json").read_text())["artifacts"]["FIG-FAT-03"]
    assert artifact["status"] == "unavailable"
    assert artifact["rows"] == []
    assert "not exported" in (out / "FIG-FAT-03.svg").read_text()


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
    assert rows[0]["life_years"] == pytest.approx(5.0)


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


def test_partial_arc_coverage_is_visible(tmp_path):
    root = campaign(tmp_path / "results")
    path = root / "R08-W1-SCR/fatigue-S1.json"
    data = json.loads(path.read_text())
    del data["arc_distribution"]
    path.write_text(json.dumps(data))
    out = tmp_path / "out"
    api().produce(root, out)
    artifact = json.loads((out / "manifest.json").read_text())["artifacts"]["FIG-FAT-03"]
    assert artifact["status"] == "partial"
    assert "R08-W1-SCR" in artifact["reason"]
    svg = ET.parse(out / "FIG-FAT-03.svg").getroot()
    assert any("not exported" in (node.text or "") for node in svg if node.tag.endswith("text"))


def test_input_table_wraps_all_recorded_fields_inside_viewbox(tmp_path):
    out = tmp_path / "out"
    api().produce(campaign(tmp_path / "results"), out)
    svg = ET.parse(out / "TAB-FAT-IN-01.svg").getroot()
    blocks = [node for node in svg.iter() if "data-row" in node.attrib]
    assert len(blocks) == 36
    for block in blocks:
        assert len(list(block)) > 1
        for text in block:
            assert float(text.attrib["y"]) < float(svg.attrib["height"])
            assert len(text.text) <= 160


def test_arc_plot_identifies_case_traces(tmp_path):
    out = tmp_path / "out"
    api().produce(campaign(tmp_path / "results"), out)
    svg = ET.parse(out / "FIG-FAT-03.svg").getroot()
    traces = [node for node in svg.iter() if "data-case" in node.attrib]
    assert {node.attrib["data-case"] for node in traces} == {
        f"R{size}-{wall}-{config}" for size in ("08", "10", "12")
        for wall in ("W1", "W2") for config in ("SCR", "SLWR")}


def test_wall_factor_uses_each_wall_own_governing_state(tmp_path):
    root = campaign(tmp_path / "results")
    path = root / "R08-W2-SCR/fatigue-S2.json"
    path.write_text(json.dumps(record("R08-W2-SCR", "S2", 80.0)))
    out = tmp_path / "out"
    api().produce(root, out)
    tokens = json.loads((out / "tokens.json").read_text())
    # ASSUMED synthetic power law: L_W2/L_W1 = (100/80)^3 = 1.953125.
    assert tokens["values"]["V-FAT-WT-FACTOR"] == pytest.approx(1.953125)
    assert tokens["wall_comparison"]["numerator_sea_state"] == "S2"


def test_recorded_life_must_be_reciprocal_and_overflow_is_rejected():
    data = record()
    data["life_years"] = 6.0
    with pytest.raises(ValueError, match="recorded"):
        api().calculate(data)
    data = record()
    data["sn_curve"]["log_a"] = -1000.0
    with pytest.raises(ValueError, match="numeric range"):
        api().calculate(data)


def test_zero_occurrence_and_zero_cycles_have_no_damage():
    data = record()
    data["sea_state"]["occurrence"] = 0.0
    data["damage_per_year"], data["life_years"] = 0.0, None
    assert api().calculate(data)[0]["damage_per_year"] == 0.0
    data = record()
    for point in data["hot_spots"] + data["arc_distribution"]:
        point["cycles_per_year"] = 0.0
    data["damage_per_year"], data["life_years"] = 0.0, None
    assert api().calculate(data)[0]["life_years"] is None


def test_absent_thickness_record_adds_no_correction():
    data = record()
    del data["sn_curve"]["thickness_correction"]
    assert api().calculate(data)[0]["thickness_factor"] == 1.0


def test_all_zero_campaign_uses_null_life_without_logarithm(tmp_path):
    root = campaign(tmp_path / "results")
    for path in root.glob("*/fatigue-*.json"):
        data = json.loads(path.read_text())
        path.write_text(json.dumps(record(data["case_id"], data["sea_state"]["id"], stress=0)))
    out = tmp_path / "out"
    api().produce(root, out)
    tokens = json.loads((out / "tokens.json").read_text())
    assert tokens["values"]["V-FAT-LMIN"] is None
    assert tokens["values"]["V-FAT-WT-FACTOR"] is None
    assert "unbounded" in (out / "FIG-FAT-02.svg").read_text()
    assert "unbounded" in (out / "TAB-FAT-01.svg").read_text()


def test_input_digests_and_artifact_digests(tmp_path):
    import hashlib

    root = campaign(tmp_path / "results")
    out = tmp_path / "out"
    api().produce(root, out)
    manifest = json.loads((out / "manifest.json").read_text())
    for source in manifest["sources"]:
        assert source["sha256"] == hashlib.sha256((root / source["export"]).read_bytes()).hexdigest()
    for artifact in manifest["artifacts"].values():
        for kind in ("csv", "svg"):
            assert artifact[f"{kind}_sha256"] == hashlib.sha256((out / artifact[kind]).read_bytes()).hexdigest()
        assert all("3 sea states" in row["scope"] for row in artifact["rows"])


def test_governing_hot_spot_is_by_damage_not_peak_range():
    data = record(distribution=False)
    data["hot_spots"][0]["cycles_per_year"] = 1.0
    # Hang-off: 50 MPa and 1,000,000 cycles => D=0.025/y, L=40 y.
    data["damage_per_year"], data["life_years"] = 0.025, 40.0
    rows = api().calculate(data)
    assert max(rows, key=lambda row: row["damage_per_year"])["hot_spot"] == "hang_off"


def test_output_must_not_contaminate_result_discovery(tmp_path):
    root = campaign(tmp_path / "results")
    with pytest.raises(ValueError, match="outside"):
        api().produce(root, root / "output")
