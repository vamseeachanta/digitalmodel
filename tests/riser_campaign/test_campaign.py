# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Solver-free campaign contracts; all fixture quantities are ASSUMED."""

import copy
import hashlib
import importlib
import json
import math
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[2]
CAMPAIGN = ROOT / "examples/riser-campaign"


def api():
    """Load the campaign adapter without a licensed solver interface."""
    return importlib.import_module("scripts.riser_campaign.generate_inputs")


def manifest():
    """Load the proposed matrix with the production validator."""
    return api().load_manifest(CAMPAIGN / "manifest.json")


def test_matrix_has_full_factorial_coverage():
    data = manifest()
    cases = data["cases"]
    assert len(cases) == 48
    expected = {
        (floater, size, depth, configuration)
        for floater in ("FPSO", "semi-submersible", "spar", "TLP")
        for size in ("small", "large")
        for depth in (500, 1000, 1500)
        for configuration in ("SCR", "lazy-wave")
    }
    assert {(r["floater_type"], r["size_class"], r["water_depth_m"],
             r["configuration"]) for r in cases} == expected
    assert len({r["rao_set_id"] for r in cases}) == 8


@pytest.mark.parametrize("mutation", [
    lambda d: d["cases"][0].update(case_id="../escape"),
    lambda d: d["cases"][0].update(unknown=1),
    lambda d: d["cases"][0].update(water_depth_m=float("nan")),
    lambda d: d["cases"][0].update(water_depth_m=True),
    lambda d: d["cases"][0].update(rao_set_id="missing"),
    lambda d: d["cases"][0]["pipe"].update(wall_thickness_m=1),
    lambda d: d["cases"][0]["pipe"].pop("basis"),
    lambda d: d["cases"].append(copy.deepcopy(d["cases"][0])),
    lambda d: d["cases"][0]["current_profile"]["points"][-1].update(depth_fraction=0.5),
    lambda d: d["cases"][1]["buoyancy"].update(buoyancy_factor=0.5),
    lambda d: d["cases"][1]["buoyancy"].update(sag_fraction=0.9),
    lambda d: d["rao_sets"][0]["rows"][0].update(period_s=0),
    lambda d: d["rao_sets"][0]["rows"][0]["amplitudes"].pop(),
    lambda d: d["rao_sets"][0]["rows"].reverse(),
    lambda d: d["cases"][0]["principal_particulars"].update(source_id="missing"),
    lambda d: d["cases"][0]["offset_cases"][0].update(x_m=1),
    lambda d: d["cases"][0]["sea_states"][0].update(period_s=99),
    lambda d: d["cases"][0]["sea_states"][0].update(direction_deg=90),
])
def test_invalid_manifests_fail_before_output(mutation):
    data = json.loads((CAMPAIGN / "manifest.json").read_text())
    mutation(data)
    with pytest.raises(ValueError):
        api().validate_manifest(data)


def test_loader_rejects_duplicate_json_keys(tmp_path):
    path = tmp_path / "duplicate.json"
    path.write_text('{"version": 1, "version": 2}')
    with pytest.raises(ValueError, match="duplicate"):
        api().load_manifest(path)


@pytest.mark.parametrize("case_index", range(48))
def test_geometry_preserves_installed_length_and_buoyancy(case_index):
    data = manifest()
    case = data["cases"][case_index]
    rows = api().geometry(case, data)
    baseline = rows[0]
    length = baseline["suspended_length_m"] + baseline["grounded_length_m"]
    assert length > case["water_depth_m"]
    for row in rows:
        assert abs(row["vertical_closure_error_m"]) < 1e-6
        assert abs(row["horizontal_closure_error_m"]) < 1e-6
        assert row["suspended_length_m"] + row["grounded_length_m"] == pytest.approx(length, abs=1e-6)
        assert row["grounded_length_m"] > 0
        assert 0 < row["top_angle_deg"] < 45
        height = case["water_depth_m"] - case["pipe"]["outer_diameter_m"]/2
        assert row["shape"][0][1:] == pytest.approx([0, height], abs=1e-6)
        assert row["shape"][-1][1:] == pytest.approx([row["horizontal_span_m"], 0], abs=1e-6)
        arcs = [p[0] for p in row["shape"]]
        assert arcs == sorted(arcs)
        assert arcs[-1] == pytest.approx(row["suspended_length_m"], abs=1e-6)
        if case["configuration"] == "lazy-wave":
            assert row["buoyed_start_m"] == pytest.approx(baseline["buoyed_start_m"], abs=1e-6)
            assert row["buoyed_length_m"] == pytest.approx(baseline["buoyed_length_m"], abs=1e-6)
            assert 0 < row["buoyed_start_m"] < row["buoyed_start_m"] + row["buoyed_length_m"] < row["suspended_length_m"]
            assert row["hog_clearance_m"] > 0


@pytest.mark.parametrize("configuration", ["SCR", "lazy-wave"])
def test_yaml_mapping_preserves_mass_buoyancy_raos_and_excursions(configuration):
    data = manifest()
    case = next(c for c in data["cases"] if c["configuration"] == configuration)
    rows = api().geometry(case, data)
    model = api().model_data(case, data, rows, 2, 1)
    assert model["General"]["UnitsSystem"] == "SI"
    line = model["Lines"][0]
    assert line["EndAX"] == 0
    assert model["Vessels"][0]["InitialPosition"][0] == -case["offset_cases"][2]["x_m"]
    assert line["EndAZ"] == 0
    assert line["EndBZ"] == -case["water_depth_m"] + case["pipe"]["outer_diameter_m"]/2
    assert line["EndBX"] == pytest.approx(rows[0]["horizontal_span_m"] + rows[0]["grounded_length_m"])
    sections = line["LineType, Length, TargetSegmentLength"]
    assert sum(s[1] for s in sections) == pytest.approx(rows[0]["suspended_length_m"] + rows[0]["grounded_length_m"])
    pipe = case["pipe"]
    area = math.pi * (pipe["outer_diameter_m"]**2 - (pipe["outer_diameter_m"] - 2*pipe["wall_thickness_m"])**2) / 4
    assert model["LineTypes"][0]["MassPerUnitLength"] == pytest.approx(area*pipe["steel_density_kg_m3"]/1000)
    assert line["ContentsDensity"] == pytest.approx(pipe["contents_density_kg_m3"]/1000)
    assert model["Vessels"][0]["SuperimposedMotion"] == "RAOs"
    vessel_type = model["VesselTypes"][0]
    assert vessel_type["RAOResponseUnits"] == "degrees"
    table = vessel_type["Draughts"][0]["DisplacementRAOs"]["RAOs"][0]
    assert len(next(v for k, v in table.items() if k.startswith("RAOPeriodOrFrequency,"))) >= 3
    wave = model["Environment"]["WaveTrains"][0]
    assert wave["WaveType"] == "Airy"
    assert wave["WavePeriod"] == case["sea_states"][1]["period_s"]
    assert wave["WaveHeight"] == case["sea_states"][1]["wave_height_m"]
    assert model["Environment"]["RefCurrentSpeed"] == case["current_profile"]["surface_speed_m_s"]
    assert "CurrentDepth, CurrentFactor, CurrentRotation" in model["Environment"]
    assert len(sections) == (3 if configuration == "lazy-wave" else 1)
    if configuration == "lazy-wave":
        rho = data["settings"]["seawater_density_kg_m3"]
        g = data["settings"]["gravity_m_s2"]
        density = pipe["contents_density_kg_m3"]/1000
        weights = []
        for kind in model["LineTypes"]:
            displaced = math.pi * kind["OD"]**2 / 4
            contents = math.pi * kind["ID"]**2 / 4 * density
            weights.append((kind["MassPerUnitLength"] + contents - displaced*rho/1000)*1000*g)
        assert weights[1] == pytest.approx(weights[0]*(1-case["buoyancy"]["buoyancy_factor"]), rel=1e-10)
        assert sections[0][1] == pytest.approx(rows[0]["buoyed_start_m"])
        assert sections[1][1] == pytest.approx(rows[0]["buoyed_length_m"])


def test_generation_is_deterministic_and_resumable(tmp_path):
    first, second = tmp_path / "first", tmp_path / "second"
    result = api().generate(CAMPAIGN / "manifest.json", first)
    repeated = api().generate(CAMPAIGN / "manifest.json", second)
    assert result == repeated
    assert len(result["cases"]) == 48
    assert sum(len(row["inputs"]) for row in result["cases"]) == 288
    for row in result["cases"]:
        for item in row["inputs"] + [row["geometry"]]:
            path = first / item["path"]
            assert hashlib.sha256(path.read_bytes()).hexdigest() == item["sha256"]
            assert path.read_bytes() == (second / item["path"]).read_bytes()
    paths = list(first.rglob("*.*"))
    before = {p: p.stat().st_mtime_ns for p in paths}
    assert api().generate(CAMPAIGN / "manifest.json", first) == result
    assert {p: p.stat().st_mtime_ns for p in paths} == before
    damaged = first / result["cases"][0]["inputs"][0]["path"]
    damaged.write_text("preserve unexpected existing data\n")
    with pytest.raises(ValueError, match="digest|existing|differ"):
        api().generate(CAMPAIGN / "manifest.json", first)
    assert damaged.read_text() == "preserve unexpected existing data\n"


def test_generation_rejects_symlink_output(tmp_path):
    destination = tmp_path / "destination"
    destination.mkdir()
    link = tmp_path / "link"
    link.symlink_to(destination, target_is_directory=True)
    with pytest.raises(ValueError, match="symlink"):
        api().generate(CAMPAIGN / "manifest.json", link)
    assert not list(destination.iterdir())


def test_cli_prints_every_case_without_importing_solver_bindings(tmp_path):
    # A fresh interpreter proves imports, including lazy dependencies, stay offline.
    script = """
import sys
def deny(event, args):
    if event == 'import' and args[0].split('.')[0] in ('OrcFxAPI', 'OrcaWave', 'aqwa'):
        raise RuntimeError('licensed solver binding attempted')
sys.addaudithook(deny)
from scripts.riser_campaign.generate_inputs import main
main()
"""
    env = {**os.environ, "PYTHONPATH": os.pathsep.join([str(ROOT / "src"), str(ROOT)])}
    run = subprocess.run([sys.executable, "-c", script, "--manifest", str(CAMPAIGN / "manifest.json"),
                          "--output-dir", str(tmp_path / "cli")],
                         cwd=ROOT, env=env, capture_output=True, text=True, check=True)
    assert "48 cases; 288 solver inputs; 0 licensed runs" in run.stdout
    for case in manifest()["cases"]:
        assert case["case_id"] in run.stdout


def test_committed_worked_examples_match_generated_inputs(tmp_path):
    index = api().generate(CAMPAIGN / "manifest.json", tmp_path / "examples")
    examples = json.loads((CAMPAIGN / "examples/index.json").read_text())
    assert {r["configuration"] for r in examples["files"]} == {"SCR", "lazy-wave"}
    for entry in examples["files"]:
        path = CAMPAIGN / "examples" / entry["path"]
        assert path.read_bytes() == (tmp_path / "examples" / entry["generated_path"]).read_bytes()
        assert hashlib.sha256(path.read_bytes()).hexdigest() == entry["sha256"]
    assert examples["manifest_sha256"] == index["manifest_sha256"]


def test_ranges_and_configuration_labels_are_unambiguous():
    data = manifest()
    data["parameter_ranges"][1] = copy.deepcopy(data["parameter_ranges"][0])
    with pytest.raises(ValueError, match="range"):
        api().validate_manifest(data)


def test_generation_keeps_manifest_and_schema_snapshots(tmp_path):
    result = api().generate(CAMPAIGN / "manifest.json", tmp_path / "snapshots")
    for name in ("manifest", "schema", "basis"):
        entry = result[name]
        payload = (tmp_path / "snapshots" / entry["path"]).read_bytes()
        assert hashlib.sha256(payload).hexdigest() == result[f"{name}_sha256"]


def test_yaml_has_no_aliases_and_serialization_preserves_native_references(tmp_path):
    import yaml
    from digitalmodel.solvers.orcaflex.yaml_utils import orcaflex_load

    result = api().generate(CAMPAIGN / "manifest.json", tmp_path / "roundtrip")
    for row in result["cases"]:
        for item in row["inputs"]:
            path = tmp_path / "roundtrip" / item["path"]
            assert not any(isinstance(event, yaml.AliasEvent) for event in yaml.parse(path.read_text()))
            loaded, _ = orcaflex_load(path)
            assert loaded["Lines"][0]["EndAConnection"] == loaded["Vessels"][0]["Name"]
            assert loaded["Vessels"][0]["VesselType"] == loaded["VesselTypes"][0]["Name"]


def test_generation_reads_one_frozen_manifest_snapshot(tmp_path, monkeypatch):
    targets = {CAMPAIGN / name for name in ("manifest.json", "manifest.schema.json", "basis.md")}
    original = Path.read_bytes
    reads = []

    def read_once(path):
        if path in targets:
            assert path not in reads, "each source must be captured once for generation and hashing"
            reads.append(path)
        return original(path)

    monkeypatch.setattr(Path, "read_bytes", read_once)
    result = api().generate(CAMPAIGN / "manifest.json", tmp_path / "frozen")
    assert set(reads) == targets
    for name, filename in (("manifest", "manifest.json"), ("schema", "manifest.schema.json"), ("basis", "basis.md")):
        assert result[f"{name}_sha256"] == hashlib.sha256(original(CAMPAIGN / filename)).hexdigest()
