# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic protocol tests; values below are not engineering acceptance data."""

import copy
import csv
import importlib.util
import json
import math
import sys
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
    # Reconstructed wet geometry; approval references are synthetic test data.
    base = {
        "version": "1.0",
        "analysis_type": "diffraction",
        "vessel": {
            "name": "generic",
            "geometry": {
                "mesh_file": "coarse.gdf",
                "mesh_format": "gdf",
                "symmetry": "none",
            },
            "inertia": {"mass": 119052930, "centre_of_gravity": [0, 0, 0]},
        },
        "environment": {"water_depth": 1000.0, "water_density": 1025},
        "frequencies": {"input_type": "frequency", "values": [0.2, 0.5]},
        "wave_headings": {"values": [0.0, 90.0]},
        "solver_options": {"remove_irregular_frequencies": True, "precision": "double"},
    }
    base_path = tmp_path / "base.yml"
    base_path.write_text(yaml.safe_dump(base), encoding="utf-8")
    for level in campaign.LEVELS:
        (tmp_path / f"{level}.gdf").write_bytes(campaign.mesh_geometry.gdf_bytes(level))
    (tmp_path / "full-body-meshes.json").write_bytes(
        (
            SCRIPT.parents[2] / "examples/hydrodynamics/full-body-meshes.json"
        ).read_bytes()
    )
    request = {
        "source": "synthetic test specification, not engineering input",
        "approvals": {
            key: "APPROVED: synthetic reference" for key in campaign.APPROVALS
        },
        "meshes": {level: f"{level}.gdf" for level in campaign.LEVELS},
        "mesh_manifest": "full-body-meshes.json",
        "frequencies_rad_s": [0.1, 0.2, 0.35, 0.5],
        "headings_deg": [0.0, 90.0, 180.0],
        "criteria": {
            key: {
                "value": value,
                "source": "synthetic test criterion",
                "status": "approved",
                "approval": "APPROVED: synthetic reference",
            }
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


@pytest.mark.parametrize("level", campaign.LEVELS)
def test_mesh_digest_required_before_any_case_is_written(inputs, level):
    path = inputs[0].parent / f"{level}.gdf"
    # A title change leaves geometry and DF2 intact, isolating the digest guard.
    path.write_bytes(path.read_bytes().replace(b"Reconstructed", b"Altered", 1))
    with pytest.raises(ValueError, match="mesh digest mismatch"):
        prepare(inputs)
    assert not inputs[2].exists()


@pytest.mark.parametrize(
    "fault", ["missing", "outside", "invalid_json", "missing_level", "bad_digest"]
)
def test_mesh_manifest_required_before_any_case_is_written(inputs, fault):
    path = inputs[0].parent / "full-body-meshes.json"
    if fault == "missing":
        inputs[4].pop("mesh_manifest")
    elif fault == "outside":
        inputs[4]["mesh_manifest"] = "../full-body-meshes.json"
    elif fault == "invalid_json":
        path.write_text("not JSON")
    else:
        manifest = json.loads(path.read_text())
        if fault == "missing_level":
            manifest["meshes"].pop("fine")
        else:
            manifest["meshes"]["fine"]["sha256"] = "invalid"
        path.write_text(json.dumps(manifest))
    inputs[1].write_text(json.dumps(inputs[4]))
    with pytest.raises(ValueError):
        prepare(inputs)
    assert not inputs[2].exists()


def test_mesh_manifest_is_bound_to_preparation(inputs):
    manifest = prepare(inputs)
    assert "mesh_manifest" in manifest["inputs"]
    path = inputs[0].parent / "full-body-meshes.json"
    path.write_bytes(path.read_bytes() + b"\n")
    with pytest.raises(ValueError, match="input digest mismatch"):
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
    assert any(check.get("difference") == 9 for check in report["checks"])
    with path.open(encoding="utf-8", newline="") as stream:
        retained = list(csv.DictReader(stream))
    mutated = [
        row
        for row in retained
        if row["run_id"] == manifest["cases"][0]["run_id"]
        and row["frequency_rad_s"] == "0.1"
        and row["heading_deg"] == "0.0"
        and row["dof"] == "sway"
    ]
    assert len(mutated) == 1
    assert campaign.number(mutated[0]["amplitude"]) == 9


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


def test_campaign_is_collected_by_diffraction_ci_domain():
    detector_path = SCRIPT.parents[1] / "ci/detect_touched_domains.py"
    spec = importlib.util.spec_from_file_location(
        "campaign_domain_detector", detector_path
    )
    detector = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = detector
    spec.loader.exec_module(detector)
    domains = detector.parse_domains(SCRIPT.parents[2] / "tests/DOMAINS.md")
    test_path = "tests/hydrodynamics/test_low_frequency_campaign.py"
    diffraction = next(d for d in domains if d.name == "hydrodynamics-diffraction")
    assert test_path in diffraction.roots
    assert "hydrodynamics-diffraction" in {
        d.name for d in detector.touched_domains([test_path], domains)
    }


def test_assessment_validates_each_criterion_once(inputs, monkeypatch):
    manifest = prepare(inputs)
    path = export(inputs, manifest)
    # Preparation already validates source-bound criteria; isolate assessment.
    monkeypatch.setattr(campaign, "verify_manifest", lambda _: manifest)
    validated = []
    original = campaign.cited

    def track(record):
        validated.append(record)
        return original(record)

    monkeypatch.setattr(campaign, "cited", track)
    report = campaign.assess(inputs[2] / "manifest.json", path)
    assert report["numerical_screen"] == "PASS"
    assert len(validated) == len(manifest["criteria"])
    assert all(
        sum(item is record for item in validated) == 1
        for record in manifest["criteria"].values()
    )


@pytest.mark.parametrize(
    "field",
    ["criteria", "geometry", "heading_convention", "weight_report", "frequency_list"],
)
@pytest.mark.parametrize(
    "value", ["MISSING", "PROPOSED: needs DF QM approval", None, "approved"]
)
def test_missing_approval_blocks_all_cases(inputs, field, value):
    inputs[4]["approvals"][field] = value
    inputs[1].write_text(json.dumps(inputs[4]))
    with pytest.raises(ValueError, match="approval|MISSING|PROPOSED"):
        prepare(inputs)
    assert not inputs[2].exists()


@pytest.mark.parametrize(
    "flags,declared",
    [
        ((0, 1), "none"),
        ((1, 0), "none"),
        ((0, 0), "xz"),
        ((0, 0), "yz"),
        ((0, 1), "yz"),
    ],
)
def test_symmetry_mismatch_blocks_all_cases(inputs, flags, declared):
    path = inputs[0].parent / "fine.gdf"
    path.write_bytes(
        campaign.mesh_geometry.encode_gdf(
            campaign.mesh_geometry.panels_for_level("fine"), flags
        )
    )
    inputs[3]["vessel"]["geometry"]["symmetry"] = declared
    inputs[0].write_text(yaml.safe_dump(inputs[3]))
    with pytest.raises(ValueError, match="symmetry"):
        prepare(inputs)
    assert not inputs[2].exists()


def test_half_mesh_with_cleared_flags_cannot_issue(inputs):
    import numpy as np

    panels = campaign.mesh_geometry.panels_for_level("coarse")
    panels = panels[np.min(panels[:, :, 1], axis=1) >= -1e-9]
    # Cleared flags cannot hide the open centreline from the DF2 geometry check.
    # The half-body FAIL regression is covered independently with native flags.
    path = inputs[0].parent / "coarse.gdf"
    path.write_bytes(campaign.mesh_geometry.encode_gdf(panels))
    with pytest.raises(ValueError, match="DF2"):
        prepare(inputs)
    assert not inputs[2].exists()


def test_mass_mismatch_blocks_all_cases(inputs):
    inputs[3]["vessel"]["inertia"]["mass"] /= 2
    inputs[0].write_text(yaml.safe_dump(inputs[3]))
    with pytest.raises(ValueError, match="DF2"):
        prepare(inputs)
    assert not inputs[2].exists()


def test_equal_volume_asymmetry_blocks_all_cases(inputs):
    geometry = campaign.mesh_geometry
    panels = geometry.panels_for_level("fine")
    panels[:, :, 1] += 0.004 * geometry.BEAM * panels[:, :, 0] / geometry.LENGTH
    (inputs[0].parent / "fine.gdf").write_bytes(geometry.encode_gdf(panels))
    with pytest.raises(ValueError, match="DF2 fine: FAIL"):
        prepare(inputs)
    assert not inputs[2].exists()


def test_coarse_replaces_original_half_mesh(inputs):
    # The old base mesh is retained as provenance, never forced into the ladder.
    inputs[3]["vessel"]["geometry"]["mesh_file"] = "old.gdf"
    (inputs[0].parent / "old.gdf").write_text("original unqualified mesh")
    inputs[0].write_text(yaml.safe_dump(inputs[3]))
    manifest = prepare(inputs)
    assert len(manifest["cases"]) == 12
    assert set(manifest["df2"]) == set(campaign.LEVELS)
    assert all(r["decision"] == "PASS" for r in manifest["df2"].values())


def test_proposed_template_cannot_issue(inputs):
    template = SCRIPT.parents[2] / "examples/hydrodynamics/low-frequency-campaign.json"
    inputs[1].write_bytes(template.read_bytes())
    with pytest.raises(ValueError, match="MISSING|PROPOSED|approval|proposed"):
        prepare(inputs)
    assert not inputs[2].exists()


def test_proposed_criterion_cannot_be_approved_by_source_string(inputs):
    record = inputs[4]["criteria"]["mesh_absolute_m_per_m"]
    record["status"] = "proposed"
    inputs[1].write_text(json.dumps(inputs[4]))
    with pytest.raises(ValueError, match="approved|PROPOSED"):
        prepare(inputs)
    assert not inputs[2].exists()


@pytest.mark.parametrize("mode", ["free_floating", "unknown"])
def test_nonexplicit_mass_cannot_issue(inputs, mode):
    inputs[3]["vessel"]["inertia"]["mode"] = mode
    inputs[0].write_text(yaml.safe_dump(inputs[3]))
    with pytest.raises(ValueError, match="explicit mass"):
        prepare(inputs)
    assert not inputs[2].exists()


def test_asymptotic_limit_above_candidate_bound_rejected(inputs):
    inputs[4]["criteria"]["asymptotic_max_rad_s"]["value"] = 0.35
    inputs[1].write_text(json.dumps(inputs[4]))
    with pytest.raises(ValueError, match="asymptotic limit"):
        prepare(inputs)


@pytest.mark.parametrize(
    "draft_scale,area_scale,density_scale,gravity,limit,allowed",
    [
        (4, 1, 1, 9.80665, 0.15, False),
        (0.25, 1, 1, 9.80665, 0.35, True),
        (1, 1, 0.5, 9.80665, 0.23, True),
        (1, 1, 2, 9.80665, 0.24, False),
        (1, 1, 1, 4, 0.2, False),
        (1, 1, 1, 20, 0.3, True),
        (1, 0.5, 1, 9.80665, 0.23, True),
        (1, 2, 1, 9.80665, 0.23, True),
    ],
)
def test_asymptotic_ceiling_uses_df2_loading_and_environment(
    inputs, draft_scale, area_scale, density_scale, gravity, limit, allowed
):
    geometry = campaign.mesh_geometry
    manifest_path = inputs[0].parent / "full-body-meshes.json"
    mesh_manifest = json.loads(manifest_path.read_text())
    for level in campaign.LEVELS:
        panels = geometry.panels_for_level(level)
        panels[:, :, 0] *= area_scale
        panels[:, :, 2] *= draft_scale
        data = geometry.encode_gdf(panels)
        (inputs[0].parent / f"{level}.gdf").write_bytes(data)
        mesh_manifest["meshes"][level]["sha256"] = campaign.digest(data)
    manifest_path.write_text(json.dumps(mesh_manifest))
    inputs[3]["vessel"]["inertia"]["mass"] *= draft_scale * area_scale * density_scale
    inputs[3]["environment"].update(water_density=1025 * density_scale, gravity=gravity)
    inputs[0].write_text(yaml.safe_dump(inputs[3]))
    inputs[4]["criteria"]["asymptotic_max_rad_s"]["value"] = limit
    inputs[1].write_text(json.dumps(inputs[4]))
    expected_ceiling = math.sqrt(gravity / (geometry.DRAFT * draft_scale)) / 4
    assert (limit < expected_ceiling) == allowed
    if allowed:
        manifest = prepare(inputs)
        assert manifest["asymptotic_ceiling_rad_s"] == pytest.approx(expected_ceiling)
    else:
        with pytest.raises(ValueError, match="asymptotic limit"):
            prepare(inputs)
        assert not inputs[2].exists()


@pytest.mark.parametrize("gravity", [None, -1, 0, True, float("nan"), float("inf")])
def test_invalid_gravity_blocks_derived_ceiling_before_output(inputs, gravity):
    inputs[3]["environment"]["gravity"] = gravity
    inputs[0].write_text(yaml.safe_dump(inputs[3]))
    with pytest.raises(ValueError):
        prepare(inputs)
    assert not inputs[2].exists()


def test_mesh_generator_is_collected_by_diffraction_ci_domain():
    text = (SCRIPT.parents[2] / "tests/DOMAINS.md").read_text()
    row = next(
        line
        for line in text.splitlines()
        if line.startswith("| hydrodynamics-diffraction ")
    )
    assert "`tests/hydrodynamics/test_full_body_mesh.py`" in row
