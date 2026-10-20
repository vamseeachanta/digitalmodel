"""Synthetic protocol tests; values below are not engineering acceptance data."""

import copy
import csv
import importlib.util
import json
from pathlib import Path

import pytest
import yaml


SCRIPT = (
    Path(__file__).resolve().parents[2] / "scripts/benchmark/low_frequency_campaign.py"
)
SPEC = importlib.util.spec_from_file_location("low_frequency_campaign", SCRIPT)
campaign = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(campaign)


@pytest.fixture
def inputs(tmp_path):
    # Deliberately synthetic payloads: preparation is not mesh qualification.
    base = {
        "version": "1.0",
        "analysis_type": "diffraction",
        "vessel": {
            "name": "generic",
            "geometry": {"mesh_file": "coarse.gdf", "mesh_format": "gdf"},
            "inertia": {"mass": 1.0, "centre_of_gravity": [0, 0, 0]},
        },
        "environment": {"water_depth": 1.0},
        "frequencies": {"input_type": "frequency", "values": [0.2, 0.5]},
        "wave_headings": {"values": [0.0, 90.0]},
        "solver_options": {"remove_irregular_frequencies": True, "precision": "double"},
    }
    base_path = tmp_path / "base.yml"
    base_path.write_text(yaml.safe_dump(base), encoding="utf-8")
    for level in campaign.LEVELS:
        (tmp_path / f"{level}.gdf").write_text(level, encoding="utf-8")
    request = {
        "source": "synthetic test specification, not engineering input",
        "meshes": {level: f"{level}.gdf" for level in campaign.LEVELS},
        "frequencies_rad_s": [0.1, 0.2, 0.35, 0.5],
        "headings_deg": [0.0, 90.0, 180.0],
        "criteria": {
            key: {"value": value, "source": "synthetic test criterion"}
            for key, value in [
                ("mesh_absolute_m_per_m", 0.1),
                ("solver_absolute_m_per_m", 0.1),
                ("asymptotic_max_rad_s", 0.1),
            ]
        },
    }
    request_path = tmp_path / "request.json"
    request_path.write_text(json.dumps(request), encoding="utf-8")
    return base_path, request_path, tmp_path / "prepared", base, request


def prepare(inputs):
    return campaign.prepare(*inputs[:3])


def export(inputs, manifest, mutate=None):
    rows = []
    for case in manifest["cases"]:
        for frequency in manifest["frequencies_rad_s"]:
            for heading in manifest["headings_deg"]:
                for dof in campaign.DOFS:
                    rows.append(
                        dict(
                            run_id=case["run_id"],
                            frequency_rad_s=frequency,
                            heading_deg=heading,
                            dof=dof,
                            amplitude=1.0 if dof == "heave" else 0.0,
                            unit="m/m",
                        )
                    )
    if mutate:
        mutate(rows)
    path = inputs[2].parent / "results.csv"
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=campaign.COLUMNS)
        writer.writeheader()
        writer.writerows(rows)
    return path


def test_preparation_preserves_physical_inputs_and_sources(inputs):
    original = inputs[0].read_bytes()
    manifest = prepare(inputs)
    assert len(manifest["cases"]) == 12  # two solvers x three meshes x two treatments
    assert inputs[0].read_bytes() == original
    for case in manifest["cases"]:
        spec = yaml.safe_load((inputs[2] / case["spec"]).read_text(encoding="utf-8"))
        assert spec["vessel"]["inertia"] == inputs[3]["vessel"]["inertia"]
        assert spec["environment"] == inputs[3]["environment"]
        assert spec["solver_options"]["precision"] == "double"
        assert "remove_irregular_frequencies" not in spec["solver_options"]
        assert spec["solver_options"]["irregular_frequency_method"] == case["treatment"]
        assert spec["frequencies"]["values"] == inputs[4]["frequencies_rad_s"]
    assert campaign.verify_manifest(inputs[2] / "manifest.json") == manifest


@pytest.mark.parametrize(
    "field,value",
    [
        ("frequencies_rad_s", [0.2, 0.5]),
        ("frequencies_rad_s", [0.1, 0.35, 0.5]),
        ("frequencies_rad_s", [0.1, 0.2, float("nan"), 0.5]),
        ("frequencies_rad_s", [0.1, 0.2, 0.2, 0.5]),
        ("frequencies_rad_s", [0, 0.2, 0.5]),
        ("headings_deg", [0, 180]),
        ("headings_deg", [0, 90, 360]),
        ("source", ""),
    ],
)
def test_bad_grid_rejected_before_output(inputs, field, value):
    inputs[4][field] = value
    inputs[1].write_text(json.dumps(inputs[4]), encoding="utf-8")
    with pytest.raises(ValueError):
        prepare(inputs)
    assert not inputs[2].exists()


@pytest.mark.parametrize("value", [None, -1, True, float("inf")])
def test_missing_or_invalid_criterion_is_not_defaulted(inputs, value):
    inputs[4]["criteria"]["mesh_absolute_m_per_m"]["value"] = value
    inputs[1].write_text(json.dumps(inputs[4]), encoding="utf-8")
    with pytest.raises(ValueError):
        prepare(inputs)


@pytest.mark.parametrize("mesh", ["../outside.gdf", "/outside.gdf", "missing.gdf"])
def test_invalid_mesh_path(inputs, mesh):
    inputs[4]["meshes"]["fine"] = mesh
    inputs[1].write_text(json.dumps(inputs[4]), encoding="utf-8")
    with pytest.raises(ValueError):
        prepare(inputs)


def test_auxiliary_mesh_rejected(inputs):
    inputs[3]["vessel"]["control_surface"] = {"mesh_file": "fine.gdf"}
    inputs[0].write_text(yaml.safe_dump(inputs[3]), encoding="utf-8")
    with pytest.raises(ValueError, match="auxiliary"):
        prepare(inputs)


def test_existing_output_not_overwritten(inputs):
    prepare(inputs)
    before = (inputs[2] / "manifest.json").read_bytes()
    with pytest.raises(FileExistsError):
        prepare(inputs)
    assert (inputs[2] / "manifest.json").read_bytes() == before


def test_changed_mesh_detected(inputs):
    prepare(inputs)
    (inputs[0].parent / "fine.gdf").write_text("changed", encoding="utf-8")
    with pytest.raises(ValueError, match="digest"):
        campaign.verify_manifest(inputs[2] / "manifest.json")


def test_complete_numerical_screen_never_qualifies(inputs):
    manifest = prepare(inputs)
    path = export(inputs, manifest)
    before = path.read_bytes()
    report = campaign.assess(inputs[2] / "manifest.json", path)
    assert report["numerical_screen"] == "PASS"
    assert report["qualification"] == "NOT_EVALUATED"
    assert report["native_execution"] == "NOT_EVALUATED"
    assert report["row_count"] == 288  # explicit Cartesian product of synthetic inputs
    assert path.read_bytes() == before
    assert {check["kind"] for check in report["checks"]} == {
        "heave_asymptote",
        "mesh_pair",
        "solver_pair",
        "treatment_delta",
    }


@pytest.mark.parametrize(
    "mutate",
    [
        lambda rows: rows.pop(),
        lambda rows: rows.append(copy.copy(rows[0])),
        lambda rows: rows[0].update(amplitude="nan"),
        lambda rows: rows[0].update(amplitude="1e1000"),
        lambda rows: rows[0].update(amplitude="1e-1000"),
        lambda rows: rows[0].update(amplitude=-1),
        lambda rows: rows[0].update(unit="deg/m"),
        lambda rows: rows[0].update(dof="roll"),
        lambda rows: rows[0].update(run_id="unknown"),
        lambda rows: rows[0].update(frequency_rad_s=0.11),
    ],
)
def test_invalid_export_cannot_pass(inputs, mutate):
    manifest = prepare(inputs)
    with pytest.raises(ValueError):
        campaign.assess(inputs[2] / "manifest.json", export(inputs, manifest, mutate))


def test_large_artifact_is_retained_and_fails(inputs):
    manifest = prepare(inputs)
    path = export(inputs, manifest, lambda rows: rows[0].update(amplitude=9))
    report = campaign.assess(inputs[2] / "manifest.json", path)
    assert report["numerical_screen"] == "FAIL"
    assert any(check["difference"] == 9 for check in report["checks"])
    assert "9" in path.read_text(encoding="utf-8")


def test_heave_band_boundary_is_inclusive(inputs):
    manifest = prepare(inputs)

    def boundary(rows):
        for row in rows:
            if row["dof"] == "heave":
                row["amplitude"] = 1.02

    report = campaign.assess(
        inputs[2] / "manifest.json", export(inputs, manifest, boundary)
    )
    assert report["numerical_screen"] == "PASS"
