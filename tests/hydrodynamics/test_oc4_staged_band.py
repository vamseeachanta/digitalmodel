# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline staged diffraction tests; synthetic inputs cannot qualify native data."""

import importlib.util
from pathlib import Path

import numpy as np
import pytest

EXAMPLE = Path(__file__).resolve().parents[2] / "examples/hydrodynamics/oc4-semisub"


def load(name):
    spec = importlib.util.spec_from_file_location(name, EXAMPLE / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize("depth,rule", [(1500, 6), (0.7, 6), (12, 8)])
def test_valid_band_inverts_exact_depth_dispersion(depth, rule):
    checker = load("mesh_band_v3")
    span = 1.72
    upper = checker.valid_band(span, depth=depth, panels_per_wave=rule)
    k = 2 * np.pi / (span * rule)
    assert upper == pytest.approx(np.sqrt(9.80665 * k * np.tanh(k * depth)), rel=1e-14)
    limit = load("check_mesh").panel_span_limit(upper, depth, panels_per_wave=rule)
    assert limit >= span
    panel = np.array([[[0, 0, -1], [span, 0, -1], [span, 0, -1], [0, 0, -1]]])
    assert checker.df3_declared(panel, upper, depth=depth, panels_per_wave=rule)["pass"]
    assert not checker.df3_declared(panel, upper * 1.0001, depth=depth, panels_per_wave=rule)["pass"]
    assert not checker.df3_declared(panel, 3.5, depth=1500)["pass"]


@pytest.mark.parametrize("span,depth,rule", [(0, 1500, 6), (1, 0, 6), (1, 1, 0),
                                           (float("nan"), 1, 6), (1, float("inf"), 6)])
def test_valid_band_rejects_invalid_inputs(span, depth, rule):
    with pytest.raises(ValueError):
        load("mesh_band_v3").valid_band(span, depth=depth, panels_per_wave=rule)


def test_staged_builder_preserves_mass_headings_peaks_and_checked_band(tmp_path):
    import math
    import yaml
    builder = load("build_datafile_v3")
    upper = load("mesh_band_v3").valid_band(1.72, depth=1500)
    specification = builder.resolved_specification(upper)
    builder.build_datafiles(tmp_path, specification)
    strength = yaml.safe_load((tmp_path / "stage-s-model-v3.yml").read_text())
    fatigue = yaml.safe_load((tmp_path / "stage-f-model-v3.yml").read_text())
    old = load("build_datafile_v2").datafile_model("l1-hull-v3.gdf")
    assert strength["Bodies"][0]["BodyAddInteriorSurfacePanels"] == "No"
    assert fatigue["Bodies"][0]["BodyAddInteriorSurfacePanels"] == "Yes"
    for deck in (strength, fatigue):
        assert deck["WaterDepth"] == 1500
        assert deck["WaveHeading"] == [-180, -135, -90, -45, 0, 45, 90, 135, 180]
        for key in ("BodyMass", "BodyCentreOfMass", "BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz"):
            assert deck["Bodies"][0][key] == old["Bodies"][0][key]
    s, f = strength["PeriodOrFrequency"], fatigue["PeriodOrFrequency"]
    assert s == sorted({round(i / 100, 2) for i in range(30, 56)} | {0.418879})
    assert len(s) == 27
    assert f == sorted({round(i / 100, 2) for i in range(5, 245)} |
                       {upper, *(2 * math.pi / tp for tp in (6.59, 7.43, 8.19))})
    assert len(f) == 244 and max(f) == upper
    assert max(s) < specification["irregular_frequency_estimate_rad_s"]
    low = builder.resolved_specification(1.5)
    assert not low["stages"]["F"]["interior_lid"]
    with pytest.raises(ValueError):
        builder.resolved_specification(0.5)


def cube_packet_source(tmp_path):
    v = np.array([[0, 0, -1], [1, 0, -1], [1, 1, -1], [0, 1, -1],
                  [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0]], float)
    hull = v[[[0, 3, 2, 1], [0, 1, 5, 4], [1, 2, 6, 5], [2, 3, 7, 6], [3, 0, 4, 7]]]
    source = tmp_path / "source.gdf"
    load("generate_mesh").write_gdf(source, hull, 9.80665)
    return source


def platform_packet_source(tmp_path, edge=1):
    import importlib.util
    path = Path(__file__).with_name("test_oc4_extended_band.py")
    spec = importlib.util.spec_from_file_location("mesh_fixtures", path)
    fixture = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(fixture)
    hull, _ = fixture.platform_hull_and_lid(edge)
    source = tmp_path / "source.gdf"
    load("generate_mesh").write_gdf(source, hull, 9.80665)
    return source


def test_staged_packet_binds_mesh_lid_both_decks_and_resumes(tmp_path):
    import hashlib
    source = platform_packet_source(tmp_path)
    directory = tmp_path / "packet"
    prepare = load("prepare_staged_v3")
    manifest = prepare.prepare_packet(directory, source, l2_source=source)
    receipt = load("input_checks_v3").df0(directory)
    assert receipt["pass"] and set(receipt["stages"]) == {"S", "F"}
    assert receipt["stages"]["S"]["lid_required"] is False
    assert receipt["stages"]["F"]["lid_required"] is True
    assert receipt["lid"]["pass"]
    assert manifest["meshes"]["L1"]["historical_3_50_df3"]["pass"] is False
    assert manifest["meshes"]["L1"]["declared_df3"]["pass"]
    assert manifest["meshes"]["L2"]["valid_band_rad_s"] > 0
    before = {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in directory.iterdir()}
    assert prepare.prepare_packet(directory, source, l2_source=source) == manifest
    assert before == {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in directory.iterdir()}


@pytest.mark.parametrize("damage", ["depth", "frequency", "mass", "heading", "lid_off", "mesh_bytes",
                                    "manifest_band", "lid_bytes", "gdf_gravity", "recipe_hash"])
def test_staged_df0_rejects_changed_inputs(tmp_path, damage):
    import json
    import yaml
    source = platform_packet_source(tmp_path)
    directory = tmp_path / "packet"
    load("prepare_staged_v3").prepare_packet(directory, source)
    model = directory / "stage-f-model-v3.yml"
    deck = yaml.safe_load(model.read_text())
    if damage == "depth":
        deck["WaterDepth"] = 200
    elif damage == "frequency":
        deck["PeriodOrFrequency"][-1] += 0.01
    elif damage == "mass":
        deck["Bodies"][0]["BodyMass"] += 1
    elif damage == "heading":
        deck["WaveHeading"] = [0]
    elif damage == "lid_off":
        deck["Bodies"][0]["BodyAddInteriorSurfacePanels"] = "No"
    elif damage in ("mesh_bytes", "lid_bytes"):
        name = "l1-hull-v3.gdf" if damage == "mesh_bytes" else "l1-lid-v3.gdf"
        path = directory / name
        path.write_text(path.read_text() + "\n")
    elif damage == "gdf_gravity":
        path = directory / "l1-hull-v3.gdf"
        path.write_text(path.read_text().replace("9.80665", "9.7"))
    else:
        path = directory / "manifest-v3.json"
        manifest = json.loads(path.read_text())
        if damage == "manifest_band":
            manifest["meshes"]["L1"]["valid_band_rad_s"] += 0.1
        else:
            manifest["source_sha256"]["input-specification-v3.json"] = "0" * 64
        path.write_text(json.dumps(manifest))
    model.write_text(yaml.safe_dump(deck))
    with pytest.raises(ValueError):
        load("input_checks_v3").df0(directory)


def test_native_estimates_scale_supplied_benchmark_without_claiming_capacity():
    estimator = load("native_estimate_v3")
    base = estimator.estimate(14064, 32)
    assert base["time_h"] == pytest.approx(3043.96 / 3600)
    assert base["memory_gb"] == pytest.approx(35.608)
    double = estimator.estimate(14064, 4, lid_panels=14064)
    assert double["time_h"] == pytest.approx((3043.96 / 32) * 8 * 4 / 3600)
    assert double["memory_gb"] == pytest.approx(35.608 * 4)
    assert double["status"] == "ESTIMATED_NOT_MEASURED"
    assert double["capacity_established"] is False
    s = estimator.estimate(14064, 27)
    f = estimator.estimate(14064, 244)
    assert s["time_h"] == pytest.approx(0.713428125)
    assert f["time_h"] == pytest.approx(6.447276388888889)
    for panels, frequencies in ((0, 4), (14064, 0), (float("nan"), 4), (14064, 4.5)):
        with pytest.raises(ValueError):
            estimator.estimate(panels, frequencies)


def test_packet_estimates_include_both_lid_scenarios_and_four_l2_probes(tmp_path):
    import math
    import yaml
    source = platform_packet_source(tmp_path)
    directory = tmp_path / "packet"
    manifest = load("prepare_staged_v3").prepare_packet(directory, source, l2_source=source)
    assert set(manifest["estimates"]) == {"S_without_lid", "S_with_lid", "F_without_lid", "F_with_lid", "L2_probes"}
    assert manifest["estimates"]["S_without_lid"]["frequencies"] == 27
    assert manifest["estimates"]["L2_probes"]["frequencies"] == 4
    assert manifest["estimates"]["F_with_lid"]["reference_lid_panels"] == manifest["lid"]["panels"]
    probe = yaml.safe_load((directory / "l2-probes-model-v3.yml").read_text())
    assert probe["PeriodOrFrequency"] == sorted([0.418879, *(2 * math.pi / tp for tp in (6.59, 7.43, 8.19))])
    assert probe["Bodies"][0]["BodyMeshFileName"] == "l2-hull-v3.gdf"
    assert probe["Bodies"][0]["BodyAddInteriorSurfacePanels"] == "No"
    receipt = load("input_checks_v3").df0(directory)
    assert receipt["l2_probe_df3"]["pass"]


def test_lower_band_packet_has_no_lid_and_refuses_source_change_on_resume(tmp_path):
    import json
    source = platform_packet_source(tmp_path, edge=2)
    directory = tmp_path / "packet"
    prepare = load("prepare_staged_v3")
    prepare.prepare_packet(directory, source)
    result = load("input_checks_v3").df0(directory)
    assert result["lid"] is None
    assert not result["stages"]["F"]["lid_required"]
    manifest = json.loads((directory / "manifest-v3.json").read_text())
    assert manifest["estimates"]["F_with_lid"] is None
    source.write_text(source.read_text() + "\n")
    with pytest.raises(ValueError, match="source digest"):
        prepare.prepare_packet(directory, source)


def test_l2_probe_and_estimate_tampering_fail_df0_even_with_updated_file_hash(tmp_path):
    import json
    import yaml
    source = platform_packet_source(tmp_path)
    directory = tmp_path / "packet"
    prepare = load("prepare_staged_v3")
    prepare.prepare_packet(directory, source, l2_source=source)
    manifest_path = directory / "manifest-v3.json"
    manifest = json.loads(manifest_path.read_text())
    probe_path = directory / "l2-probes-model-v3.yml"
    original = probe_path.read_text()
    probe = yaml.safe_load(original)
    probe["Bodies"][0]["BodyAddInteriorSurfacePanels"] = "Yes"
    probe_path.write_text(yaml.safe_dump(probe))
    manifest["files"][probe_path.name] = prepare.digest(probe_path)
    manifest_path.write_text(json.dumps(manifest))
    with pytest.raises(ValueError, match="L2 probe datafile"):
        load("input_checks_v3").df0(directory)
    probe_path.write_text(original)
    manifest["files"][probe_path.name] = prepare.digest(probe_path)
    manifest["estimates"]["L2_probes"]["memory_gb"] = 0
    manifest_path.write_text(json.dumps(manifest))
    with pytest.raises(ValueError, match="Native estimate"):
        load("input_checks_v3").df0(directory)


def test_missing_bottom_scaled_cube_is_rejected_before_packet_generation(tmp_path):
    source = cube_packet_source(tmp_path)
    check = load("check_mesh")
    damaged = check.read_gdf(source)[1:] * 2
    assert check.measure(damaged)["bad_edges"] == 4
    load("generate_mesh").write_gdf(source, damaged, 9.80665)
    with pytest.raises(ValueError, match="Hull acceptance"):
        load("prepare_staged_v3").prepare_packet(tmp_path / "packet", source)
    assert not (tmp_path / "packet").exists()


@pytest.mark.parametrize("damage", ["bottom", "aspect", "hydrostatics"])
def test_consistently_rehashed_geometry_cannot_pass_staged_df0(tmp_path, damage):
    import json
    source = platform_packet_source(tmp_path)
    packet = tmp_path / "packet"
    prepare = load("prepare_staged_v3")
    prepare.prepare_packet(packet, source)
    hull = packet / "l1-hull-v3.gdf"
    panels = load("check_mesh").read_gdf(hull)
    if damage == "bottom":
        panels = panels[1:]
    elif damage == "aspect":
        panels[:, :, 0] *= 10
        panels[:, :, 1] /= 10
    else:
        panels[:, :, 2] *= 0.5
    load("generate_mesh").write_gdf(hull, panels, 9.80665)
    manifest_path = packet / "manifest-v3.json"
    manifest = json.loads(manifest_path.read_text())
    manifest["meshes"]["L1"] = prepare.mesh_record(hull)
    manifest["files"][hull.name] = prepare.digest(hull)
    manifest_path.write_text(json.dumps(manifest))
    with pytest.raises(ValueError, match="Hull acceptance"):
        load("input_checks_v3").df0(packet)
