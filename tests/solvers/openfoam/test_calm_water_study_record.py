# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Validate retained native starts without treating them as settled results."""

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[3]


def load_builder():
    spec = importlib.util.spec_from_file_location(
        "study_builder", REPO / "scripts/cfd/build_calm_water_study.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_coefficient_history_preserves_force_sign_and_normalisation():
    builder = load_builder()
    text = "# Time total_x total_y total_z pressure_x pressure_y pressure_z viscous_x viscous_y viscous_z\n1 -10 0 0 -6 0 0 -4 0 0\n"
    assert builder.coefficient_history(text, density=2, area=5, speed=2) == (
        "iteration,ct,cp,cv\n1,0.5,0.3,0.2\n"
    )


def test_malformed_native_history_is_rejected():
    builder = load_builder()
    with pytest.raises(ValueError):
        builder.coefficient_history("1 nan 0 0 -6 0 0 -4 0 0\n", 2, 5, 2)


def test_criterion_digest_and_order_are_checked():
    builder = load_builder()
    data = b"{}\n"
    record = {
        "criterion_sha256": hashlib.sha256(data).hexdigest(),
        "criterion_recorded_at": "2026-10-14T00:00:00+00:00",
        "timed_start_started_at": "2026-10-14T00:00:01+00:00",
    }
    builder.verify_criterion(record, data)
    with pytest.raises(ValueError):
        builder.verify_criterion(record, b"changed")
    record["criterion_recorded_at"] = "2026-10-14T00:00:02+00:00"
    with pytest.raises(ValueError):
        builder.verify_criterion(record, data)


def test_short_starts_cannot_be_reported_as_qualified_results():
    builder = load_builder()
    manifest = {"runs": [{"completed_iterations": 100}], "qualified_results": 0}
    page = builder.render_report(manifest)
    assert "not established" in page
    assert "not available" in page
    assert "Revision history" in page
    assert "Qualified resistance results: 0" in page
    assert "<script" not in page


def test_header_whitespace_normalisation_preserves_numerical_bytes():
    builder = load_builder()
    numerical = "1 -10 0 0 -6 0 0 -4 0 0\n"
    assert builder.normalise_force_header("# Force   \n" + numerical) == (
        "# Force\n" + numerical
    )


def test_half_domain_reduction_doubles_force_and_preserves_pressure_thrust():
    builder = load_builder()
    text = "1 -10 0 0 2 0 0 -12 0 0\n"
    assert builder.coefficient_history(text, 2, 5, 2, half_domain=True) == (
        "iteration,ct,cp,cv\n1,1,-0.2,1.2\n"
    )


def test_normalisation_uses_executed_velocity_not_unrounded_request(tmp_path):
    builder = load_builder()
    field = tmp_path / "U"
    field.write_text("Umean 2.19663;\nmUmean -2.19663;\n")
    assert builder.executed_velocity(field) == 2.19663


def test_velocity_readback_rejects_wrong_flow_direction(tmp_path):
    builder = load_builder()
    field = tmp_path / "U"
    field.write_text("Umean 2.19663;\nmUmean 2.19663;\n")
    with pytest.raises(ValueError):
        builder.executed_velocity(field)


def test_full_run_requires_native_end_and_requested_last_iteration():
    builder = load_builder()
    with pytest.raises(ValueError, match="incomplete"):
        builder.verify_full_run("Time = 24999\nEnd\n", 25000, 0)
    with pytest.raises(ValueError, match="incomplete"):
        builder.verify_full_run("Time = 25000\n", 25000, 0)
    with pytest.raises(ValueError, match="incomplete"):
        builder.verify_full_run("Time = 25000\nEnd\n", 25000, 1)
    assert builder.verify_full_run("Time = 24999\nTime = 25000\nEnd\n", 25000, 0) == 25000


def test_complete_force_history_requires_every_iteration():
    builder = load_builder()
    with pytest.raises(ValueError, match="missing"):
        builder.verify_force_iterations("iteration,ct,cp,cv\n1,1,1,0\n3,1,1,0\n", 3)
    builder.verify_force_iterations("iteration,ct,cp,cv\n1,1,1,0\n2,1,1,0\n", 2)


def test_completed_run_statistics_use_iteration_window():
    builder = load_builder()
    text = "iteration,ct,cp,cv\n1,1,0.5,0.5\n2,2,1,1\n3,4,2,2\n"
    result = builder.full_run_statistics(text, 3, 2)
    assert result["samples"] == 2
    assert result["mean_ct"] == 3
    assert result["first_iteration"] == 2
    assert result["last_iteration"] == 3
    with pytest.raises(ValueError):
        builder.full_run_statistics(text, 4, 2)


def test_restart_histories_preserve_all_rows_and_reject_overlap(tmp_path):
    builder = load_builder()
    for name, row in (("0", "1 -10 0 0 -6 0 0 -4 0 0\n"), ("1", "2 -12 0 0 -7 0 0 -5 0 0\n")):
        folder = tmp_path / name
        folder.mkdir()
        (folder / "force.dat").write_text("# Force\n" + row)
    text, digests = builder.join_force_histories(tmp_path)
    coefficients = builder.coefficient_history(text, 2, 5, 2)
    builder.verify_force_iterations(coefficients, 2)
    assert len(digests) == 2
    assert "total_x_N" in text and "iteration" in text
    (tmp_path / "1/force.dat").write_text("1 -12 0 0 -7 0 0 -5 0 0\n")
    text, _ = builder.join_force_histories(tmp_path)
    with pytest.raises(ValueError):
        builder.coefficient_history(text, 2, 5, 2)


def test_full_collector_checks_native_digest_and_retained_history(tmp_path):
    builder = load_builder()
    case = tmp_path / "case"
    case.mkdir()
    native = "Time = 1\nTime = 2\nEnd\n"
    (case / "log.interFoam.full").write_text(native)
    forces = case / "postProcessing/forces/0"
    forces.mkdir(parents=True)
    (forces / "force.dat").write_text("1 -10 0 0 -6 0 0 -4 0 0\n2 -12 0 0 -7 0 0 -5 0 0\n")
    record = {"mesh": "coarse", "status": "complete", "return_code": 0, "native_log_sha256": hashlib.sha256(native.encode()).hexdigest()}
    normalisation = {"density_kg_per_m3": 2, "area_m2": 5, "speed_m_per_s": 2}
    files = {}
    result = builder.collect_full_run(case, record, normalisation, {"full_iterations": 2, "averaging_iterations": 2}, tmp_path / "out", files)
    assert result["statistics"]["samples"] == 2
    assert result["statistics"]["mean_resistance_n"] == 22
    assert len(files) == 2
    for name, item in files.items():
        assert hashlib.sha256((tmp_path / "out" / name).read_bytes()).hexdigest() == item["sha256"]
    record["native_log_sha256"] = "changed"
    with pytest.raises(ValueError, match="digest"):
        builder.collect_full_run(case, record, normalisation, {"full_iterations": 2}, tmp_path / "out", {})


def test_report_distinguishes_failed_continuation_from_complete_resistance():
    builder = load_builder()
    page = builder.render_report({"runs": [], "qualified_results": 0, "full_runs": [{"mesh": "coarse", "status": "failed", "last_iteration": 123, "measured_wall_seconds": 12.3}]})
    assert "failed" in page
    assert "123" in page
    assert "12.3" in page
    assert "Qualified resistance results: 0" in page
    assert "Native continuation" in page


def test_run_log_retains_measured_cost_and_iteration_scope():
    builder = load_builder()
    timed = {"mesh": "coarse", "cells": 123, "ranks": 8, "completed_iterations": 100, "measured_wall_seconds": 1.2}
    full = {"mesh": "coarse", "cells": 123, "ranks": 8, "last_iteration": 25000, "measured_wall_seconds": 3.4, "status": "complete"}
    rows = [json.loads(line) for line in builder.render_run_log({"runs": [timed], "full_runs": [full]}).splitlines()]
    assert rows[0] == dict(timed, kind="timed_start")
    assert rows[1] == dict(full, kind="full_continuation")


def test_full_continuation_uses_its_native_start_for_criterion_order():
    builder = load_builder()
    criteria = b"{}\n"
    record = {"criterion_sha256": hashlib.sha256(criteria).hexdigest(), "criterion_recorded_at": "2026-10-14T00:00:00+00:00", "started_at": "2026-10-14T00:00:01+00:00"}
    builder.verify_criterion(record, criteria)
    record["started_at"] = "2026-10-13T23:59:59+00:00"
    with pytest.raises(ValueError):
        builder.verify_criterion(record, criteria)


def test_formal_report_structure_and_internal_links_follow_reporting_basis():
    from html.parser import HTMLParser

    class Links(HTMLParser):
        def __init__(self):
            super().__init__()
            self.ids = set()
            self.targets = []

        def handle_starttag(self, tag, attrs):
            for key, value in attrs:
                if key == "id":
                    self.ids.add(value)
                if key in ("href", "xlink:href") and value.startswith("#"):
                    self.targets.append(value[1:])

    page = load_builder().render_report({"runs": [], "full_runs": [], "qualified_results": 0, "criteria": {"comparison": {"relative_tolerance_percent": 3.0}, "grid_uncertainty": {"medium_mesh_target_percent": 1.0}}})
    headings = ["1 Executive Summary", "2 Introduction", "3 Design Data", "4 Analysis Methodology", "5 Detailed Results", "6 Key Findings", "7 Limitations", "8 Recommendations", "9 References", "10 Acronyms and Symbols", "Appendix A"]
    positions = [page.index(value) for value in headings]
    assert positions == sorted(positions)
    assert "Contents" in page and "Assumption ledger" in page
    assert "Target hull cell length" in page
    assert "3.0 %" in page and "1.0 %" in page
    assert "9.1 Published references" in page and "9.2 Internal references" in page
    assert "Validation is planned" in page
    numbered_body = page.split('<h2 id="references">')[0]
    assert all(value not in numbered_body for value in ("src/", "scripts/", "data/", "SHA-256"))
    import re
    without_links = re.sub(r"<a\b[^>]*>.*?</a>", "", numbered_body, flags=re.S)
    assert not re.search(r"\[(?:I|E)\d+\]", without_links)
    links = Links()
    links.feed(page)
    assert links.targets and all(target in links.ids for target in links.targets)


def test_physical_basis_is_read_from_executed_geometry_and_inputs(tmp_path):
    from types import SimpleNamespace

    for folder in ("constant/triSurface", "0.orig"):
        (tmp_path / folder).mkdir(parents=True)
    (tmp_path / "constant/triSurface/kcs.stl").write_text("vertex 0 -0.5 -0.3\nvertex 7 0 0\nvertex 3 -0.4 0.2\n")
    (tmp_path / "constant/g").write_text("value (0 0 -9.81);\n")
    (tmp_path / "constant/transportProperties").write_text("water { nu 1.1e-6; rho 998.8; }\nair { nu 1e-5; rho 1; }\nsigma 0;\n")
    (tmp_path / "0.orig/U").write_text("Umean 2.19663;\nmUmean -2.19663;\n")
    (tmp_path / "log.checkMesh").write_text("Overall domain bounding box (-30 -20 -19) (20 0 5)\n")
    config = SimpleNamespace(lpp=7.2786, beam=1.019, draft=0.3418, wetted_surface=9.4379, density=998.8)
    builder = load_builder()
    basis = builder.read_model_basis(tmp_path, config, {"froude_number": 0.26})
    assert basis["geometry_bounds_m"]["x"] == [0.0, 7.0]
    assert basis["domain_bounds_m"]["y"] == [-20.0, 0.0]
    assert basis["case_gravity_m_per_s2"] == 9.81
    assert basis["speed_m_per_s"] == 2.19663
    assert basis["kinematic_viscosity_m2_per_s"] == 1.1e-6
    assert basis["air_kinematic_viscosity_m2_per_s"] == 1e-5
    assert basis["air_density_kg_per_m3"] == 1
    assert basis["surface_tension_n_per_m"] == 0
    (tmp_path / "constant/triSurface/kcs.stl").write_text("vertex nan 0 0\n")
    with pytest.raises(ValueError):
        builder.read_model_basis(tmp_path, config, {"froude_number": 0.26})

    (tmp_path / "constant/triSurface/kcs.stl").write_text("vertex 0 -0.5 -0.3\nvertex 7 0 0\nvertex 3 -0.4 0.2\n")
    (tmp_path / "log.checkMesh").write_text("Overall domain bounding box (nan -20 -19) (20 0 5)\n")
    with pytest.raises(ValueError):
        builder.read_model_basis(tmp_path, config, {"froude_number": 0.26})


def test_history_figure_is_self_contained_and_deterministic():
    builder = load_builder()
    data = "iteration,ct,cp,cv\n" + "".join(f"{index},0.004,0.002,0.002\n" for index in range(1, 11))
    first = builder.render_history_plot({"coarse": data}, window=4)
    assert first == builder.render_history_plot({"coarse": data}, window=4)
    assert first.startswith("<svg")
    assert "<script" not in first and "<image" not in first
    assert "<metadata" not in first


def test_mesh_difference_uses_stated_coarse_reference():
    builder = load_builder()
    runs = [{"mesh": "coarse", "statistics": {"mean_ct": 0.004}}, {"mesh": "medium", "statistics": {"mean_ct": 0.0042}}]
    text = builder.mesh_difference_text(runs)
    assert "5.0%" in text and "coarse" in text
    assert "uncertainty" in text


def test_face_resolution_record_preserves_source_and_checks_its_limit(tmp_path):
    builder = load_builder()
    log = tmp_path / "log.face-resolution"
    data = "FACE RESOLUTION VERDICT: PASS -- patch 'hull': 6100 faces, max 0.004258 m2 = 2.9x the target cell area (0.03829 m)^2, limit 8x; mean face 0.001542 m2\n"
    log.write_text(data)
    result = builder.read_face_check(log)
    assert result["faces"] == 6100
    assert result["target_cell_length_m"] == 0.03829
    assert result["limit_1"] == 8
    assert result["source_sha256"] == hashlib.sha256(data.encode()).hexdigest()
    log.write_text(data.replace("PASS", "FAIL"))
    with pytest.raises(ValueError):
        builder.read_face_check(log)
    log.write_text(data.replace("max 0.004258", "max 0.04"))
    with pytest.raises(ValueError):
        builder.read_face_check(log)


def test_full_record_mesh_name_cannot_overwrite_an_output_neighbor(tmp_path):
    builder = load_builder()
    case = tmp_path / "case"
    case.mkdir()
    native = "Time = 1\nTime = 2\nEnd\n"
    (case / "log.interFoam.full").write_text(native)
    forces = case / "postProcessing/forces/0"
    forces.mkdir(parents=True)
    (forces / "force.dat").write_text("1 -10 0 0 -6 0 0 -4 0 0\n2 -12 0 0 -7 0 0 -5 0 0\n")
    output = tmp_path / "out"
    sentinel = output / "data/cfd/escaped-full-force.dat"
    sentinel.parent.mkdir(parents=True)
    original = b"preserved caller file\n"
    sentinel.write_bytes(original)
    before = hashlib.sha256(original).hexdigest()
    record = {"mesh": "../../escaped", "status": "complete", "return_code": 0, "native_log_sha256": hashlib.sha256(native.encode()).hexdigest()}
    try:
        with pytest.raises(ValueError):
            builder.collect_full_run(case, record, {"density_kg_per_m3": 2, "area_m2": 5, "speed_m_per_s": 2}, {"full_iterations": 2, "averaging_iterations": 2}, output, {})
    finally:
        tampered = hashlib.sha256(sentinel.read_bytes()).hexdigest()
        sentinel.write_bytes(original)
        restored = hashlib.sha256(sentinel.read_bytes()).hexdigest()
        print(json.dumps({"before": before, "tampered": tampered, "restored": restored}))
        assert restored == before
    assert tampered == before


def load_verifier():
    spec = importlib.util.spec_from_file_location(
        "study_verifier", REPO / "scripts/cfd/verify_calm_water_study.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_committed_study_evidence_and_report_are_reproducible():
    verifier = load_verifier()
    manifest = verifier.verify_retained(REPO)
    assert verifier.render_retained(REPO, manifest) == (
        REPO / "docs/domains/openfoam/calm-water-resistance-study.html"
    ).read_text()
    page = verifier.render_retained(REPO, manifest)
    assert "indicative" in page.lower()
    assert "No numerical experimental comparison is emitted" in page


@pytest.mark.parametrize("field,value", [
    ("qualified_results", 1), ("readiness_status", "validated"),
])
def test_retained_evidence_rejects_qualification_claims(field, value):
    verifier = load_verifier()
    manifest = json.loads((REPO / "data/cfd/calm_water_study/manifest.json").read_text())
    manifest[field] = value
    with pytest.raises(ValueError):
        verifier.verify_retained(REPO, manifest)


@pytest.mark.parametrize("mutation", ["digest", "cost", "statistics", "path"])
def test_retained_evidence_rejects_inconsistent_records(mutation):
    verifier = load_verifier()
    manifest = json.loads((REPO / "data/cfd/calm_water_study/manifest.json").read_text())
    if mutation == "digest":
        next(iter(manifest["files"].values()))["sha256"] = "0" * 64
    elif mutation == "cost":
        manifest["runs"][0]["projected_full_seconds"] *= 2
    elif mutation == "statistics":
        manifest["full_runs"][0]["statistics"]["mean_ct"] *= 2
    else:
        manifest["files"]["../outside"] = next(iter(manifest["files"].values()))
    with pytest.raises(ValueError):
        verifier.verify_retained(REPO, manifest)
