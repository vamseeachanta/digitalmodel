# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Frozen matrix and independently calculated native-field contracts."""

import copy
import csv
import hashlib
import importlib
import io
import json
import math
from pathlib import Path

import pytest

CAMPAIGN = Path(__file__).resolve().parents[2] / "examples/riser-campaign"
# Frozen UTF-8, tab-separated scope supplied with the engineering input decision.
SCOPE_SHA256 = "f95a98735eb3882a354857fc56975a82d1b67095d6a57cb8cf565229e9ddf2c1"


def api():
    return importlib.import_module("scripts.riser_campaign.frozen_scope")


def tranche():
    return json.loads((CAMPAIGN / "manifest.json").read_text(encoding="utf-8"))["frozen_tranche"]


def test_frozen_bytes_and_cartesian_ids():
    payload = (CAMPAIGN / "matrix-i0339-2.tsv").read_bytes()
    assert hashlib.sha256(payload).hexdigest() == SCOPE_SHA256
    rows = list(csv.DictReader(io.StringIO(payload.decode("utf-8")), delimiter="\t"))
    assert len(rows) == 12
    assert {r["case_id"] for r in rows} == {
        f"R{nps:02}-W{wall}-{kind}" for nps in (8, 10, 12)
        for wall in (1, 2) for kind in ("SCR", "SLWR")}


def test_every_scope_field_matches_manifest():
    t = tranche()
    rows = list(csv.DictReader((CAMPAIGN / "matrix-i0339-2.tsv").open(encoding="utf-8"), delimiter="\t"))
    assert t["rows"] == rows
    assert t["scope_sha256"] == SCOPE_SHA256
    assert t["criteria_version"] == "I0339-2-v1"
    api().validate_tranche(t)


@pytest.mark.parametrize("mutation", [
    lambda t: t["rows"].pop(),
    lambda t: t["rows"].append(copy.deepcopy(t["rows"][0])),
    lambda t: t["rows"][0].update(outside_diameter_mm="203.200"),
    lambda t: t["rows"][0].update(checked_wall_mm="12.5500"),
    lambda t: t["rows"][0].update(fatigue_seas="F03;F04"),
    lambda t: t.update(scope_sha256="0" * 64),
    lambda t: t.update(criteria_version="unversioned"),
])
def test_frozen_scope_mutations_fail(mutation):
    t = tranche()
    mutation(t)
    with pytest.raises(ValueError):
        api().validate_tranche(t)


def test_exact_primary_expansion_and_dependencies():
    loads = api().expand_loads(tranche())
    assert len(loads) == 144  # 12 Q0 + 72 S + 24 nominal D + 36 F.
    assert len({r["id"] for r in loads}) == 144
    assert sum(r["kind"] == "statics" for r in loads) == 84
    assert sum(r["kind"] == "dynamics" for r in loads) == 24
    fatigue = [r for r in loads if r["kind"] == "fatigue"]
    assert {(r["case_id"], r["sea_state_id"]) for r in fatigue} == {
        (f"R{nps:02}-W{wall}-{kind}", sea) for nps in (8, 10, 12)
        for wall in (1, 2) for kind in ("SCR", "SLWR") for sea in ("F03", "F04", "F05")}
    for row in fatigue:
        assert (row["span_offset_m"], row["current_speed_m_s"], row["wave_toward_deg"]) == (0, 0, 180)
        assert row["stationary_seconds"] == 3600
        assert row["seed"] == {"F03": 104729, "F04": 130363, "F05": 155921}[row["sea_state_id"]]
        assert row["equilibrium_id"] == row["case_id"] + "--Q0"
    regular = [r for r in loads if r["kind"] == "dynamics"]
    assert {(r["wave_toward_deg"], r["current_toward_deg"]) for r in regular} == {(0, 15), (180, 195)}
    assert {r["span_offset_m"] for r in regular} == {0}
    static = [r for r in loads if r["group"] == "S"]
    assert {r["span_offset_m"] for r in static} == {-90, 0, 90}
    api().validate_loads(tranche(), loads)


@pytest.mark.parametrize("duplicate", [False, True])
def test_expansion_rejects_missing_or_duplicated_rows(duplicate):
    loads = api().expand_loads(tranche())
    if duplicate:
        loads[-1] = copy.deepcopy(loads[0])
    else:
        loads.pop()
    with pytest.raises(ValueError, match="load"):
        api().validate_loads(tranche(), loads)


@pytest.mark.parametrize("index", range(12))
def test_dimensions_wall_and_module_conservation(index):
    row = tranche()["rows"][index]
    p = api().section_properties(row)
    od = {8: 0.2191, 10: 0.2731, 12: 0.3239}[int(row["nps_in"])]
    nominal = float(row["wall_thickness_mm"]) / 1000
    checked = (0.875 * float(row["wall_thickness_mm"]) - 3) / 1000
    assert p["outer_diameter_m"] == pytest.approx(od, abs=1e-12)
    assert p["checked_wall_m"] == pytest.approx(checked)
    assert p["checked_wall_m"] == pytest.approx(float(row["checked_wall_mm"]) / 1000, abs=1e-12)
    bore = od - 2 * nominal
    area = math.pi / 4 * (od**2 - bore**2)
    inertia = math.pi / 64 * (od**4 - bore**4)
    coated = od + 0.08  # Two 40 mm radial coatings, supplied property basis.
    coating_area = math.pi / 4 * (coated**2 - od**2)
    dry_mass = area * 7850 + coating_area * 900
    weight = (dry_mass + math.pi / 4 * bore**2 * 800 - math.pi / 4 * coated**2 * 1025) * 9.80665
    assert p["dry_mass_kg_m"] == pytest.approx(dry_mass)
    assert p["EA_N"] == pytest.approx(207e9 * area)
    assert p["EI_Nm2"] == pytest.approx(207e9 * inertia)
    assert p["submerged_weight_N_m"] == pytest.approx(weight)
    if row["configuration"] == "SLWR":
        module_volume_per_m = math.pi / 4 * (p["module_od_m"]**2 - coated**2) / 2.5
        assert (1025 - 450) * 9.80665 * module_volume_per_m == pytest.approx(2 * weight)
        assert p["buoyed_weight_N_m"] == pytest.approx(-weight)
        matrix_od = float(row["buoyancy_layout"].split("OD=")[1].split("m;")[0])
        assert p["module_od_m"] == pytest.approx(matrix_od, abs=0.0000005)


def test_pressure_uses_depth_in_metres_and_gauge_kpa():
    assert api().pressures(0) == {"internal_kPa_g": 20000, "external_kPa_g": 0}
    p = api().pressures(1500)
    # Hand arithmetic: 800 * 9.80665 * 1500 / 1000 + 20000.
    assert p["internal_kPa_g"] == pytest.approx(31767.98)
    assert p["external_kPa_g"] == pytest.approx(15077.724375)
    assert p["internal_kPa_g"] - p["external_kPa_g"] == pytest.approx(16690.255625)


@pytest.mark.parametrize("index", range(12))
def test_geometry_reserve_fixed_arcs_and_depth_transform(index):
    scope = tranche()["rows"][index]
    g = api().prepare_geometry(scope)
    assert g["vertical_separation_m"] == 1490
    assert g["contact_centreline_correction_m"] == 0
    assert g["anchor_m"][2] == -1500
    assert g["hangoff_m"] == [0, 0, -10]
    assert min(r["grounded_length_m"] for r in g["offsets"]) >= 20
    assert min(r["adverse_friction_min_tension_N"] for r in g["offsets"]) >= 1000
    first = g["offsets"][0]
    assert first["offset_m"] == 0
    for r in g["offsets"]:
        assert r["suspended_length_m"] + r["grounded_length_m"] == pytest.approx(g["installed_length_m"], abs=1e-6)
        assert r["shape_global"][0][1:] == pytest.approx([-r["offset_m"], -10], abs=1e-6)
        assert r["shape_global"][-1][2] == pytest.approx(-1500, abs=1e-6)
        assert r["horizontal_span_m"] + r["grounded_length_m"] - r["offset_m"] == pytest.approx(g["anchor_m"][0], abs=1e-6)
        if scope["configuration"] == "SLWR":
            assert r["buoyed_start_m"] == pytest.approx(g["buoyancy_start_m"], abs=1e-6)
            assert r["buoyed_length_m"] == pytest.approx(g["buoyancy_length_m"], abs=1e-6)
    if scope["configuration"] == "SLWR":
        assert g["nominal_sag_ASB_m"] == pytest.approx(50)
        assert first["hog_clearance_m"] == pytest.approx(700)


def test_unsupported_heading_fails_closed():
    row = api().expand_loads(tranche())[12]
    row["wave_toward_deg"] = 90
    with pytest.raises(ValueError, match="heading"):
        api().native_model(tranche()["rows"][0], row, api().prepare_geometry(tranche()["rows"][0]))


def test_generation_records_counts_snapshots_and_blocked_dynamics(tmp_path):
    from scripts.riser_campaign.generate_inputs import generate
    result = generate(CAMPAIGN / "manifest.json", tmp_path / "generated")
    frozen = result["frozen_tranche"]
    assert frozen["criteria_version"] == "I0339-2-v1"
    assert frozen["scope_sha256"] == SCOPE_SHA256
    assert frozen["written_counts"] == {"statics": 84, "dynamics": 24, "fatigue": 36}
    assert frozen["phase_a_required"] == {"statics": 88, "dynamics_including_fatigue": 70}
    assert frozen["pending_governing_qa"] == {"statics": 4, "dynamics_including_fatigue": 10}
    assert len(frozen["cases"]) == 12
    for case in frozen["cases"]:
        assert len(case["inputs"]) == 12
        for item in case["inputs"]:
            payload = (tmp_path / "generated" / item["path"]).read_bytes()
            assert hashlib.sha256(payload).hexdigest() == item["sha256"]
            assert item["qualification"] == "NOT_RUN"
            if item["kind"] != "statics":
                assert "RAO_CLEARANCE_REQUIRED" in item["release_blockers"]
    for entry in frozen["snapshots"] + frozen["code_sources"] + [frozen["load_ledger"]]:
        payload = (tmp_path / "generated" / entry["path"]).read_bytes()
        assert hashlib.sha256(payload).hexdigest() == entry["sha256"]


@pytest.mark.parametrize("kind", ["statics", "dynamics", "fatigue"])
def test_native_fields_and_fatigue_seas(kind):
    scope = tranche()["rows"][5]
    load = next(r for r in api().expand_loads(tranche()) if r["case_id"] == scope["case_id"] and r["kind"] == kind)
    g = api().prepare_geometry(scope)
    model = api().native_model(scope, load, g)
    line = model["Lines"][0]
    assert line["EndAZ"] == -10
    assert line["EndBZ"] == -1500
    assert model["Vessels"][0]["InitialPosition"][0] == -load["span_offset_m"]
    assert model["Vessels"][0]["SuperimposedMotion"] == "None"
    assert line["ContentsPressureRefZ"] == 0
    assert line["ContentsPressure"] == 20000
    assert line["ContentsDensity"] == 0.8
    assert model["Environment"]["SeabedNormalStiffness"] == 100
    for section in model["LineTypes"]:
        assert section["SeabedAxialFrictionCoefficient"] == 0.5
        assert section["SeabedLateralFrictionCoefficient"] == 0.5
        assert section["StressOD"] == 0.2731
        assert section["StressID"] == pytest.approx(0.2731 - 2 * 0.0194)
    assert sum(r[1] for r in line["LineType, Length, NumberOfSegments"]) == pytest.approx(g["installed_length_m"])
    wave = model["Environment"]["WaveTrains"][0]
    if kind == "fatigue":
        assert wave["WaveType"] == "JONSWAP"
        assert wave["WaveHs"] == 2.112264
        assert wave["WaveTp"] == 6.59
        assert wave["WaveGamma"] == 1
        assert wave["WaveSeed"] == 104729
        assert wave["WaveDirection"] == 180
        assert model["General"]["StageDuration"] == [150, 3600]
        assert model["Environment"]["RefCurrentSpeed"] == 0
    elif kind == "dynamics":
        assert wave["WaveType"] == "Dean stream"
        assert wave["WaveHeight"] == pytest.approx(26.60026176)
        assert model["General"]["StageDuration"] == [150, 600]


@pytest.mark.parametrize("gamma", [0.99, 7.01])
def test_gamma_outside_supported_physical_bounds_still_fails(gamma):
    from digitalmodel.solvers.orcaflex.modular_generator.schema.environment import WaveTrain
    with pytest.raises(ValueError):
        WaveTrain(type="jonswap", height=2, period=7, gamma=gamma)


def test_frozen_sources_are_captured_once(tmp_path, monkeypatch):
    from scripts.riser_campaign.generate_inputs import generate
    original = Path.read_bytes
    targets = {CAMPAIGN / name for name in ("matrix-i0339-2.tsv", "criteria-i0339-2.json")}
    reads = []

    def once(path):
        if path in targets:
            assert path not in reads
            reads.append(path)
        return original(path)

    monkeypatch.setattr(Path, "read_bytes", once)
    generate(CAMPAIGN / "manifest.json", tmp_path / "frozen-source-snapshot")
    assert set(reads) == targets


def test_all_native_loads_have_exact_offset_pressure_current_and_seas(tmp_path):
    import yaml
    from scripts.riser_campaign.generate_inputs import generate
    result = generate(CAMPAIGN / "manifest.json", tmp_path / "all-loads")
    for case in result["frozen_tranche"]["cases"]:
        anchors, lengths, arcs = [], [], []
        for item in case["inputs"]:
            model = yaml.safe_load((tmp_path / "all-loads" / item["path"]).read_text(encoding="utf-8"))
            line, env = model["Lines"][0], model["Environment"]
            anchors.append((line["EndBX"], line["EndBY"], line["EndBZ"]))
            sections = line["LineType, Length, NumberOfSegments"]
            assert all(type(count) is int and count > 0 and length > 0
                       for _, length, count in sections)
            sizes = [length / count for _, length, count in sections for _ in range(count)]
            assert all(max(a / b, b / a) <= 2.0 for a, b in zip(sizes, sizes[1:]))
            lengths.append(sum(r[1] for r in sections))
            arcs.append([r[1] for r in sections])
            assert model["Vessels"][0]["InitialPosition"][0] == -item["span_offset_m"]
            assert line["ContentsPressureRefZ"] == 0
            assert line["ContentsPressure"] == 20000
            assert env["RefCurrentDirection"] == item["current_toward_deg"]
            profile = env["CurrentDepth, CurrentFactor, CurrentRotation"]
            assert [r[0] for r in profile] == [0, 3.9624, 17.0688, 36.8808, 43.8912, 52.1208, 1500]
            if item["group"] == "S" or item["group"] == "D":
                expected = [1.898904, 1.700784, 1.399032, 0.999744, 0.701040, 0.100584, 0.100584]
                assert [r[1]*env["RefCurrentSpeed"] for r in profile] == pytest.approx(expected)
            wave = env["WaveTrains"][0]
            if item["kind"] == "fatigue":
                hs, tp = {"F03": (2.112264, 6.59), "F04": (2.71272, 7.43), "F05": (3.53568, 8.19)}[item["sea_state_id"]]
                assert (wave["WaveHs"], wave["WaveTp"], wave["WaveGamma"]) == (hs, tp, 1)
                assert "WaveTz" not in wave
                assert wave["WaveSeed"] == item["seed"]
                assert model["General"]["StageDuration"] == [150, 3600]
                assert env["RefCurrentSpeed"] == 0
        assert len(set(anchors)) == 1
        assert lengths == pytest.approx([lengths[0]] * 12)
        assert all(a == arcs[0] for a in arcs)


@pytest.mark.parametrize("index", [0, 1])
def test_mesh_has_coarse_spans_and_fine_material_hotspots(index):
    scope = tranche()["rows"][index]
    g = api().prepare_geometry(scope)
    load = next(r for r in api().expand_loads(tranche()) if r["case_id"] == scope["case_id"])
    model = api().native_model(scope, load, g)
    sections = model["Lines"][0]["LineType, Length, NumberOfSegments"]
    assert max(length / count for _, length, count in sections) <= 3
    assert max(length / count for _, length, count in sections) > 2.5
    hotspots = [0, *[r["suspended_length_m"] for r in g["offsets"]]]
    if scope["configuration"] == "SLWR":
        hotspots += [g["buoyancy_start_m"], g["buoyancy_end_m"]]
        shape = g["offsets"][0]["shape"]
        turning = [b for a, b, c in zip(shape, shape[1:], shape[2:]) if (b[2]-a[2])*(c[2]-b[2]) < 0]
        assert [p[2] for p in turning] == pytest.approx([50, 700], abs=1e-6)
        hotspots += [p[0] for p in turning]
    for station in hotspots:
        start = 0
        covering = []
        for _, length, count in sections:
            if start - 1e-6 <= station <= start + length + 1e-6:
                covering.append(length / count)
            start += length
        assert covering and max(covering) <= 0.5


@pytest.mark.parametrize("duplicate", [False, True])
def test_incomplete_frozen_manifest_writes_nothing(tmp_path, duplicate):
    from scripts.riser_campaign.generate_inputs import generate
    manifest = json.loads((CAMPAIGN / "manifest.json").read_text(encoding="utf-8"))
    rows = manifest["frozen_tranche"]["rows"]
    if duplicate:
        rows[-1] = copy.deepcopy(rows[0])
    else:
        rows.pop()
    source = tmp_path / "invalid-manifest.json"
    source.write_text(json.dumps(manifest), encoding="utf-8", newline="\n")
    output = tmp_path / "not-created"
    with pytest.raises(ValueError):
        generate(source, output)
    assert not output.exists()


def test_declared_smaller_original_matrix_is_validated():
    from scripts.riser_campaign.validate_manifest import validate_manifest
    manifest = json.loads((CAMPAIGN / "manifest.json").read_text(encoding="utf-8"))
    manifest["matrix_axes"] = {"floater_type": ["semi-submersible"], "size_class": ["small", "large"],
                               "water_depth_m": [500, 1000, 1500], "configuration": ["SCR", "lazy-wave"]}
    manifest["cases"] = [c for c in manifest["cases"] if c["floater_type"] == "semi-submersible"]
    assert len(manifest["cases"]) == 12
    validate_manifest(manifest)
    manifest["cases"].pop()
    with pytest.raises(ValueError, match="matrix"):
        validate_manifest(manifest)
