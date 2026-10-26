# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline gates for the supplied extended-band study; no licensed imports."""

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


def test_df3_uses_high_band_and_exact_depth():
    checker = load("check_mesh")
    panel = np.array([[[0, 0, -1], [1, 0, -1], [0, 1, -1], [0, 1, -1]]])
    assert checker.df3(panel, [0.05, 0.35], depth=1500)["pass"]
    high = checker.df3(panel, [0.05, 3.5], depth=1500)
    assert not high["pass"] and high["oversized_panels"] == 1
    assert high["limit_m"] == pytest.approx(0.8383265196279277, rel=1e-12)
    assert checker.panel_span_limit(3.5, 1500) == pytest.approx(
        2 * np.pi * 9.80665 / 3.5**2 / 6
    )


def cube_and_lid():
    v = np.array([[0, 0, -1], [1, 0, -1], [1, 1, -1], [0, 1, -1],
                  [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0]], float)
    return (v[[[0, 3, 2, 1], [0, 1, 5, 4], [1, 2, 6, 5],
               [2, 3, 7, 6], [3, 0, 4, 7]]], v[[[4, 5, 6, 7]]])


@pytest.mark.parametrize("damage", ["none", "outside", "reverse", "duplicate", "hole", "above", "hull_cap"])
def test_lid_requires_interior_coverage_and_no_overlap(damage):
    hull, lid = cube_and_lid()
    if damage == "outside":
        lid[:, :, 0] += 2
    elif damage == "reverse":
        lid = lid[:, ::-1]
    elif damage == "duplicate":
        lid = np.concatenate([lid, lid])
    elif damage == "hole":
        lid = lid[:, [0, 1, 2, 2]]
    elif damage == "above":
        lid[:, :, 2] += 0.1
    elif damage == "hull_cap":
        hull = np.concatenate([hull, lid])
    if damage == "none":
        assert load("check_mesh").validate_lid(hull, lid)["area_m2"] == 1
    else:
        with pytest.raises(ValueError):
            load("check_mesh").validate_lid(hull, lid)


def test_post_mesh_refinement_checks_serialized_span(tmp_path):
    generator = load("generate_mesh")
    panel = np.array([[[0, 0, -1], [2, 0, -1], [0, 2, -1], [0, 2, -1]]], float)
    refined = generator.enforce_span(panel, 0.8383265196279277)
    assert len(refined) > len(panel)
    assert load("check_mesh").df3(refined, [3.5], depth=1500)["pass"]
    assert np.sum(np.linalg.norm(np.cross(refined[:, 1] - refined[:, 0],
                                         refined[:, 2] - refined[:, 0]), axis=1)) / 2 == pytest.approx(2)
    out = tmp_path / "hull.gdf"
    generator.write_gdf(out, refined, 9.80665)
    assert load("check_mesh").df3(load("check_mesh").read_gdf(out), [3.5], depth=1500)["pass"]


@pytest.mark.parametrize("omega,depth", [(0, 1500), (3.5, 0), (float("nan"), 1500), (-1, 1500)])
def test_dispersion_rejects_invalid_inputs(omega, depth):
    with pytest.raises(ValueError):
        load("check_mesh").panel_span_limit(omega, depth)


def test_versioned_builder_exact_frequency_mass_depth_and_lid(tmp_path):
    builder = load("build_datafile_v2")
    path = tmp_path / "model.yml"
    builder.build_datafile(path, "coarse-hull-v2.gdf")
    import yaml
    deck = yaml.safe_load(path.read_text())
    assert deck["WaterDepth"] == 1500
    assert deck["Bodies"][0]["BodyAddInteriorSurfacePanels"] == "Yes"
    frequencies = deck["PeriodOrFrequency"]
    assert len(frequencies) == 350
    assert frequencies[:3] == [0.05, 0.06, 0.07] and frequencies[-1] == 3.5
    assert frequencies == sorted(set(frequencies))
    for peak in (0.41887902047863906, 0.7671776931843207,
                 0.8456507815854086, 0.9534423834870389):
        assert peak in frequencies
    assert 1.983 not in frequencies  # probe is a separate one-frequency case
    body = deck["Bodies"][0]
    assert body["BodyMass"] == 13473
    assert body["BodyCentreOfMass"] == [0, 0, -13.46]
    assert body["BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz"] == [
        [6827000, 0, 0], [0, 6827000, 0], [0, 0, 12260000]]


def small_packet(tmp_path):
    import hashlib
    import json
    builder, generator = load("build_datafile_v2"), load("generate_mesh")
    hull, lid = cube_and_lid()
    hull, lid = hull * 0.25, lid * 0.25
    artifacts = []
    for kind, panels in (("hull", hull), ("lid", lid)):
        path = tmp_path / f"coarse-{kind}-v2.gdf"
        generator.write_gdf(path, panels, 9.80665)
        artifacts.append(dict(file=path.name, kind=kind, panels=len(panels),
                              sha256=hashlib.sha256(path.read_bytes()).hexdigest()))
    manifest = dict(schema_version=2, level="coarse", inputs=dict(depth_m=1500, gravity_m_s2=9.80665,
                    omega_max_rad_s=3.5, panels_per_wavelength=6,
                    max_span_m=0.8383265196279277,
                    source_sha256={name: hashlib.sha256((EXAMPLE/name).read_bytes()).hexdigest()
                                   for name in ("generate_mesh.py", "check_mesh.py")}), artifacts=artifacts)
    (tmp_path/"coarse-manifest-v2.json").write_text(json.dumps(manifest))
    builder.build_datafile(tmp_path/"coarse-model-v2.yml", "coarse-hull-v2.gdf")
    return tmp_path


@pytest.mark.parametrize("damage", ["none", "depth", "lid", "frequency", "mass", "heading", "inertia", "hash", "gdf_gravity", "manifest", "history"])
def test_df0_rejects_input_and_hash_changes(tmp_path, damage):
    import json
    import yaml
    packet = small_packet(tmp_path)
    path = packet/"coarse-model-v2.yml"
    deck = yaml.safe_load(path.read_text())
    if damage == "depth":
        deck["WaterDepth"] = 200
    elif damage == "lid":
        deck["Bodies"][0]["BodyAddInteriorSurfacePanels"] = "No"
    elif damage == "frequency":
        deck["PeriodOrFrequency"][0] = 0.051
    elif damage == "mass":
        deck["Bodies"][0]["BodyMass"] += 1
    elif damage == "heading":
        deck["WaveHeading"] = [0]
    elif damage == "inertia":
        deck["Bodies"][0]["BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz"][0][0] += 1
    elif damage == "hash":
        with (packet/"coarse-hull-v2.gdf").open("a") as out:
            out.write("\n")
    elif damage == "gdf_gravity":
        gdf = packet/"coarse-hull-v2.gdf"
        gdf.write_text(gdf.read_text().replace("9.80665", "9.7"))
        import hashlib
        mf = packet/"coarse-manifest-v2.json"
        m = json.loads(mf.read_text())
        m["artifacts"][0]["sha256"] = hashlib.sha256(gdf.read_bytes()).hexdigest()
        mf.write_text(json.dumps(m))
    elif damage == "manifest":
        mf = packet/"coarse-manifest-v2.json"
        m = json.loads(mf.read_text())
        m["inputs"]["depth_m"] = 200
        mf.write_text(json.dumps(m))
    path.write_text(yaml.safe_dump(deck))
    expected = json.loads((EXAMPLE/"expected-v2.json").read_text())
    if damage == "history":
        expected["historical_failures"]["DF5.2"] = "PASS"
    check = load("input_checks_v2")
    if damage == "none":
        result = check.df0(packet, "coarse", expected=expected)
        assert result["pass"]
        assert result["historical_failures"] == {"DF5.2": "FAIL", "DF5.3": "FAIL"}
    else:
        with pytest.raises(ValueError):
            check.df0(packet, "coarse", expected=expected)


@pytest.fixture(scope="module")
def generated_packet(tmp_path_factory):
    import os
    directory = Path(os.environ["OC4_PACKET_DIR"]) if "OC4_PACKET_DIR" in os.environ else tmp_path_factory.mktemp("extended-packet")
    generator = load("generate_mesh")
    generator.generate_pair(directory)
    for level in ("coarse", "fine"):
        load("build_datafile_v2").build_datafile(directory/f"{level}-model-v2.yml", f"{level}-hull-v2.gdf")
    return directory


def test_generated_pair_has_checked_hulls_lids_and_resumable_manifests(generated_packet):
    import hashlib
    import json
    results = []
    before = {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in generated_packet.glob("*.gdf")}
    for level in ("coarse", "fine"):
        manifest = json.loads((generated_packet/f"{level}-manifest-v2.json").read_text())
        result = load("input_checks_v2").df0(generated_packet, level)
        assert result["pass"] and result["lid"]["pass"]
        assert result["frequency_count"] == 350
        assert all(a["df3"]["max_span_m"] <= 0.8383265196279277 for a in manifest["artifacts"])
        results.append(manifest)
    assert results[1]["artifacts"][0]["panels"] > results[0]["artifacts"][0]["panels"]
    assert results[1]["artifacts"][0]["df3"]["max_span_m"] < results[0]["artifacts"][0]["df3"]["max_span_m"]
    load("generate_mesh").generate_pair(generated_packet)
    assert before == {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in generated_packet.glob("*.gdf")}


def test_pair_rejects_tampered_retained_case(tmp_path):
    import json
    (tmp_path/"coarse-manifest-v2.json").write_text(json.dumps({"inputs": {}}))
    with pytest.raises(ValueError, match="inputs differ"):
        load("generate_mesh").generate_pair(tmp_path)
