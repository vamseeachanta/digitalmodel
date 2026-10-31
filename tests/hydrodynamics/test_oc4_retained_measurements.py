# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Retained-input receipts expose missing lid dependencies without losing decks."""

import hashlib
import importlib.util
import json
from pathlib import Path

import numpy as np
import pytest
import yaml

EXAMPLE = Path(__file__).resolve().parents[2] / "examples/hydrodynamics/oc4-semisub"


def load(name, root=EXAMPLE):
    spec = importlib.util.spec_from_file_location(name, root / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def inputs(tmp_path, scale=2):
    source = tmp_path / "inputs"
    source.mkdir()
    fixture = load("test_oc4_staged_band", Path(__file__).parent)
    synthetic = fixture.platform_packet_source(tmp_path, edge=scale)
    panels = load("check_mesh").read_gdf(synthetic)
    for level in ("L1", "L2"):
        load("generate_mesh").write_gdf(source / f"oc4_{level}.gdf", panels, 9.80665)
        (source / f"oc4_{level}.yml").write_text("UnitsSystem: SI\n")
    (source / "oc4_L1_df4.yml").write_text("UnitsSystem: SI\n")
    basis = json.loads((EXAMPLE / "expected-v2.json").read_text())["mass_properties"]
    (source / "mass-properties.json").write_text(json.dumps(basis))
    sums = [f"{hashlib.sha256(p.read_bytes()).hexdigest()}  {p.name}" for p in source.iterdir()]
    (source / "SHA256SUMS.txt").write_text("\n".join(sums) + "\n")
    return source


def test_measurement_receipt_binds_inputs_decks_mass_and_benchmark(tmp_path):
    source = inputs(tmp_path)
    destination = tmp_path / "measured"
    measured = load("measure_retained_v3").measure_inputs(source, destination)
    assert len(measured["input_sha256"]) == 7
    assert measured["mass_basis_matches"]
    assert not measured["benchmark"]["panel_count_matches_L1"]
    assert measured["stages"]["S"]["frequency_count"] == 27
    assert measured["stages"]["F"]["upper_rad_s"] == measured["meshes"]["L1"]["valid_band_rad_s"]
    assert measured["lid"]["status"] == "NOT_REQUIRED"
    assert measured["l2_probe_df3"]["pass"]
    for stage in ("s", "f"):
        deck = yaml.safe_load((destination / f"stage-{stage}-model-v3.yml").read_text())
        assert len(deck["PeriodOrFrequency"]) == measured["stages"][stage.upper()]["frequency_count"]
        assert np.isfinite(deck["PeriodOrFrequency"]).all()
    assert not list(destination.glob("*hull*.gdf"))
    assert load("measure_retained_v3").measure_inputs(source, destination) == measured
    (destination / "stage-s-model-v3.yml").write_text("changed\n")
    with pytest.raises(ValueError, match="Output digest"):
        load("measure_retained_v3").measure_inputs(source, destination)


def test_input_digest_mismatch_is_rejected_before_generation(tmp_path):
    source = inputs(tmp_path)
    (source / "oc4_L1.gdf").write_text("changed\n")
    with pytest.raises(ValueError, match="Input digest"):
        load("measure_retained_v3").measure_inputs(source, tmp_path / "measured")
    assert not (tmp_path / "measured").exists()


def test_missing_lid_tool_is_explicit_open_not_a_pass(tmp_path, monkeypatch):
    source = inputs(tmp_path, scale=1)
    measured = load("measure_retained_v3")
    prepare = measured.module("prepare_staged_v3")

    def unavailable(*args, **kwargs):
        raise ModuleNotFoundError("No module named 'gmsh'", name="gmsh")

    monkeypatch.setattr(prepare, "generate_lid", unavailable)
    original = measured.module
    monkeypatch.setattr(measured, "module", lambda name: prepare if name == "prepare_staged_v3" else original(name))
    result = measured.measure_inputs(source, tmp_path / "measured")
    assert result["lid"]["status"] == "OPEN_TOOL_UNAVAILABLE"
    assert result["estimates"]["F_with_lid"] is None
    assert result["stages"]["F"]["interior_lid"]
    assert result["packet_df0"] == "OPEN_REFERENCE_LID_UNAVAILABLE"


def test_committed_measurements_and_decks_match_bound_generator_sources():
    directory = EXAMPLE / "retained-v3"
    report = json.loads((directory / "retained-measurements-v3.json").read_text())
    prepare = load("prepare_staged_v3")
    assert report["source_sha256"] == {
        **prepare.source_hashes(), "measure_retained_v3.py": prepare.digest(EXAMPLE / "measure_retained_v3.py")}
    for name, digest in report["generated_sha256"].items():
        assert prepare.digest(directory / name) == digest
    assert report["meshes"]["L1"]["panels"] == 14064
    assert report["meshes"]["L2"]["panels"] == 35776
    assert report["meshes"]["L1"]["declared_df3"]["pass"]
    assert report["meshes"]["L2"]["declared_df3"]["pass"]
    assert report["l2_probe_count"] == 4
    assert report["stages"]["F"]["frequency_count"] == 244
    assert report["stages"]["S"]["frequency_count"] == 27
    assert report["stages"]["F"]["interior_lid"]
    assert report["benchmark"]["panel_count_matches_L1"]
    assert report["mass_basis_matches"]
    assert all(report["hull_acceptance"][level]["pass"] for level in ("L1", "L2"))
    assert report["lid"]["status"] == "REFERENCE_GENERATED_NATIVE_READBACK_OPEN"
    assert report["lid"]["check"]["gmsh_version"] == "4.15.1"
    assert report["lid"]["check"]["pass"]
    assert report["lid"]["mesh"]["declared_df3"]["pass"]
    assert report["lid"]["mesh"]["max_span_m"] <= report["meshes"]["L1"]["max_span_m"]
    for stage in ("S", "F"):
        assert report["estimates"][f"{stage}_with_lid"]["reference_lid_panels"] == report["lid"]["mesh"]["panels"]
    for stage in ("S", "F"):
        deck = yaml.safe_load((directory / f"stage-{stage.lower()}-model-v3.yml").read_text())
        specification = json.loads((directory / "resolved-specification-v3.json").read_text())
        assert deck == load("build_datafile_v3").stage_model(stage, specification)
