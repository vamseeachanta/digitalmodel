# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Regress static results, evidence ordering, removals and deterministic reporting."""
import importlib.util
import json
import math
import sys
from pathlib import Path

import pytest

RECORD = Path(__file__).resolve().parents[2] / "docs/domains/mooring/screening"


def load(name):
    spec = importlib.util.spec_from_file_location(name, RECORD / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def run():
    return load("run_screening")


@pytest.fixture(scope="module")
def builder(run):
    return load("build_report")


@pytest.fixture
def inputs():
    return [json.loads((RECORD / name).read_text(encoding="utf-8"))
            for name in ("basis.json", "criteria.json", "revisions.json")]


def test_fixed_case_matches_existing_force_based_test(run):
    """Use the independently stated expected geometry from the module's own test."""
    result = run.solve_static(10000.0, 50.0, 100.0)
    # TestSimplifiedCatenarySolverForceBased.test_force_based_basic / expected_from_force.
    parameter = (10000.0 - 50.0 * 100.0) / 50.0
    x_over_a = math.acosh(1.0 + 100.0 / parameter)
    assert result["horizontal_tension_n"] == pytest.approx(5000.0, abs=0.01)
    assert result["suspended_length_m"] == pytest.approx(parameter * math.sinh(x_over_a), abs=0.0001)
    assert result["suspended_horizontal_span_m"] == pytest.approx(parameter * x_over_a, abs=0.01)
    assert result["fairlead_tension_n"] == pytest.approx(10000.0, abs=0.0001)


@pytest.mark.parametrize("force,weight,depth", [(float("nan"), 50, 100), (10000, float("inf"), 100),
                                                    (10000, 50, 0), (5000, 50, 100)])
def test_invalid_static_input_is_rejected(run, force, weight, depth):
    with pytest.raises(ValueError):
        run.solve_static(force, weight, depth)


def test_source_tampering_is_rejected_without_modifying_caller(run, tmp_path):
    source = tmp_path / "source.json"
    source.write_text("original\n")
    pin = run.digest(source)
    basis = {"source_files": {source.name: pin}}
    run.verify_sources(basis, root=tmp_path)
    source.write_text("tampered\n")
    tampered = run.digest(source)
    with pytest.raises(ValueError, match="Source digest mismatch"):
        run.verify_sources(basis, root=tmp_path)
    assert run.digest(source) == tampered
    source.write_text("original\n")
    assert run.digest(source) == pin
    run.write_json(tmp_path / "preservation.json", {"before": pin, "tampered": tampered, "restored": run.digest(source)})


@pytest.mark.parametrize("fault", ["digest", "order", "timezone"])
def test_criteria_evidence_fails_closed(run, tmp_path, fault):
    path = tmp_path / "criteria.json"
    path.write_text('{}\n')
    receipt = {"criteria_sha256": run.digest(path), "criteria_recorded_at_utc": "2026-10-14T00:00:00+00:00",
               "first_run_started_at_utc": "2026-10-14T00:00:01+00:00"}
    run.validate_evidence(path, receipt)
    if fault == "digest":
        path.write_text('{"changed":true}\n')
    elif fault == "order":
        receipt["first_run_started_at_utc"] = receipt["criteria_recorded_at_utc"]
    else:
        receipt["criteria_recorded_at_utc"] = "2026-10-14T00:00:00"
    with pytest.raises(ValueError):
        run.validate_evidence(path, receipt)


def test_all_matrix_entries_remain_unqualified(run, inputs):
    basis, criteria, _ = inputs
    result = run.compute_record(basis, criteria)
    assert result["counts"] == {"matrix_entries": 120, "unique_static_solves": 5,
                                "entries_with_static_line": 60, "entries_not_reproduced": 60,
                                "qualified_environmental_cases": 0}
    assert len({r["id"] for r in result["solutions"]}) == 5
    for solution in result["solutions"]:
        assert all(solution["checks"].values())
        assert solution["required_standard_factor"] is None
        assert solution["standards_disposition"] == "not established"
    for entry in result["matrix"]:
        assert (entry["static_solution"] is not None) == (entry["concept"] in basis["static_concepts"])


def test_revision_history_follows_summary_and_matches_latest(run, builder, inputs):
    basis, criteria, revisions = inputs
    result = run.compute_record(basis, criteria)
    page = builder.report_page(builder.sections(result, basis, revisions), result["revision"])
    assert page.index('id="summary"') < page.index('id="revision-history"') < page.index('id="computed-values"')
    assert revisions["history"][-1]["revision"] == result["revision"]
    assert revisions["history"][-1]["change"] in page
    with pytest.raises(ValueError, match="Newest revision"):
        builder.revision_section(revisions, "unrecorded")


def test_page_binds_every_result_and_rejects_changed_value(run, builder, inputs):
    basis, criteria, revisions = inputs
    result = run.compute_record(basis, criteria)
    page = builder.report_page(builder.sections(result, basis, revisions), result["revision"])
    assert builder.validate_page(page, result, basis, revisions) == 39
    changed = page.replace('data-format="integer">5</span>', 'data-format="integer">6</span>', 1)
    with pytest.raises(ValueError):
        builder.validate_page(changed, result, basis, revisions)


def test_prepared_page_removes_original_uncomputed_content(run, builder, inputs):
    basis, criteria, revisions = inputs
    result = run.compute_record(basis, criteria)
    source = '<html><head><title>Old</title><include src="partial.html"></include></head><body>~80 kN/m, 90%, PASS</body></html>'
    prepared = builder.website_page(source, builder.sections(result, basis, revisions), result["revision"])
    assert builder.validate_page(prepared, result, basis, revisions) == 39
    for original in ("~80", "90%", '<include src='):
        assert original not in prepared
    assert 'id="not-reproduced"' in prepared
    assert basis["run_record_url"] in prepared


def test_page_rejects_external_resources(run, builder, inputs):
    basis, criteria, revisions = inputs
    result = run.compute_record(basis, criteria)
    page = builder.report_page(builder.sections(result, basis, revisions), result["revision"])
    with pytest.raises(ValueError, match="self-contained"):
        builder.validate_page(page.replace('</head>', '<script src="external.js"></script></head>'), result, basis, revisions)


def test_record_rerun_and_two_builds_are_identical(run, builder, tmp_path, inputs):
    basis, criteria, _ = inputs
    a, b = tmp_path / "a", tmp_path / "b"
    a.mkdir()
    b.mkdir()
    run.write_json(a / "results.json", run.compute_record(basis, criteria))
    run.write_json(b / "results.json", run.compute_record(basis, criteria))
    builder.build(a)
    builder.build(b)
    for name in ("results.json", "report.html", "README.md", "comparison.md", "manifest.json"):
        assert (a / name).read_bytes() == (b / name).read_bytes()
    manifest = json.loads((a / "manifest.json").read_text())
    for name, expected in manifest["files"].items():
        assert run.digest(a / name) == expected["sha256"]
        assert (a / name).stat().st_size == expected["bytes"]
    for name, expected in manifest["record_inputs"].items():
        assert run.digest(RECORD / name) == expected


def test_committed_native_output_and_manifest_readback(run, inputs):
    basis, criteria, _ = inputs
    computed = run.compute_record(basis, criteria)
    assert computed == json.loads((RECORD / "results.json").read_text())
    manifest = json.loads((RECORD / "manifest.json").read_text())
    for name, expected in manifest["files"].items():
        assert run.digest(RECORD / name) == expected["sha256"]
    receipt = json.loads((RECORD / "execution.json").read_text())
    run.validate_evidence(RECORD / "criteria.json", receipt)


def test_page_rejects_uncomputed_claim_outside_result_block(run, builder, inputs):
    basis, criteria, revisions = inputs
    result = run.compute_record(basis, criteria)
    page = builder.report_page(builder.sections(result, basis, revisions), result["revision"])
    with pytest.raises(ValueError, match="Unbound numerical claim"):
        builder.validate_page(page.replace('</body>', '<p>Peak environmental tension: 999 kN</p></body>'), result, basis, revisions)


def test_imported_solver_origin_is_bound_to_the_record(run, inputs, tmp_path, monkeypatch):
    basis, criteria, _ = inputs
    imported = sys.modules[run.SimplifiedCatenarySolver.__module__]
    monkeypatch.setattr(imported, "__file__", str(tmp_path / "other.py"))
    with pytest.raises(ValueError, match="Imported module digest mismatch"):
        run.compute_record(basis, criteria)


def test_website_output_has_no_trailing_whitespace(run, builder, inputs):
    basis, criteria, revisions = inputs
    result = run.compute_record(basis, criteria)
    source = '<html>\n<head>  \n<title>Old</title>\n</head>\n<body>old values</body></html>'
    prepared = builder.website_page(source, builder.sections(result, basis, revisions), result['revision'])
    assert all(line == line.rstrip(' \t') for line in prepared.splitlines())
