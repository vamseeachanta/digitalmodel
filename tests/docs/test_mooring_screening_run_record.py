# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Regress static results, evidence ordering, removals and deterministic reporting."""
import copy
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


def assert_record_matches(actual, expected):
    """Compare native libm results within eight ULPs; everything else is exact."""
    actual = copy.deepcopy(actual)
    assert len(actual["solutions"]) == len(expected["solutions"])
    for solution, reference in zip(actual["solutions"], expected["solutions"]):
        for values, targets, fields in (
            (solution["native"], reference["native"], (
                "suspended_length_m", "suspended_horizontal_span_m",
                "vertical_tension_n", "fairlead_tension_n")),
            (solution, reference, ("grounded_length_m", "anchor_radius_m",
                                   "provided_static_mbl_to_tension", "resultant_error_n")),
        ):
            for field in fields:
                value, target = values[field], targets[field]
                assert type(value) is type(target) is float
                assert math.isfinite(value) and math.isfinite(target)
                if field == "resultant_error_n":
                    assert value >= 0
                scale = reference["native"]["fairlead_tension_n"] if field == "resultant_error_n" else target
                assert abs(value - target) <= 8 * math.ulp(scale), field
                values[field] = target
    assert json.dumps(actual, sort_keys=True, allow_nan=False) == json.dumps(
        expected, sort_keys=True, allow_nan=False)


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
    assert_record_matches(computed, json.loads((RECORD / "results.json").read_text()))
    manifest = json.loads((RECORD / "manifest.json").read_text())
    assert manifest["source_files"] == basis["source_files"]
    assert manifest["counts"] == computed["counts"]
    for name, expected in manifest["files"].items():
        assert run.digest(RECORD / name) == expected["sha256"]
        assert (RECORD / name).stat().st_size == expected["bytes"]
    for name, expected in manifest["record_inputs"].items():
        assert run.digest(RECORD / name) == expected
    receipt = json.loads((RECORD / "execution.json").read_text())
    run.validate_evidence(RECORD / "criteria.json", receipt)


@pytest.mark.parametrize("direction", [-math.inf, math.inf])
def test_record_ignores_last_bit_catenary_roundoff(run, inputs, monkeypatch, direction):
    """Native libm roundoff must not drift the deterministic static record."""
    basis, criteria, _ = inputs
    baseline = run.compute_record(basis, criteria)
    original = run.SimplifiedCatenarySolver.solve_from_force

    def perturbed(self, *args, **kwargs):
        result = original(self, *args, **kwargs)
        for field in ("arc_length", "horizontal_distance", "weight_suspended"):
            for _ in range(4):
                setattr(result, field, math.nextafter(getattr(result, field), direction))
        return result

    monkeypatch.setattr(run.SimplifiedCatenarySolver, "solve_from_force", perturbed)
    perturbed_record = run.compute_record(basis, criteria)
    assert perturbed_record != baseline
    assert_record_matches(perturbed_record, baseline)


def test_resultant_criterion_is_enforced_at_full_precision(run, inputs, monkeypatch):
    basis, criteria, _ = inputs
    # A value just beyond the limit must still fail the full-precision check.
    criteria = {**criteria, "resultant_tolerance_n": 0.0001}
    original = run.solve_static

    def outside_criterion(force, weight, depth):
        result = original(force, weight, depth)
        result["fairlead_tension_n"] = force + 0.0001000001
        return result

    monkeypatch.setattr(run, "solve_static", outside_criterion)
    with pytest.raises(ValueError, match="Static geometry criterion failed"):
        run.compute_record(basis, criteria)


def test_linux_ci_native_values_match_record(run, inputs, monkeypatch, tmp_path):
    """Replay native results from Linux CPython 3.11.18, run 43111878898."""
    # Values are transcribed from the tests-contracts full assertion diff:
    # https://github.com/vamseeachanta/digitalmodel/actions/runs/43111878898/job/130833128162
    # (span m, length m, vertical force N, fairlead resultant N), by depth m.
    observed = {
        200: (263.39157938496334, 346.4101615137755, 22684.614119495072, 26193.93613670653),
        500: (658.4789484624084, 866.0254037844387, 56711.535298737675, 65484.84034176632),
        1000: (1316.9578969248169, 1732.0508075688774, 113423.07059747535, 130969.68068353264),
        1500: (1975.4368453872253, 2598.076211353316, 170134.605896213, 196454.52102529895),
        2000: (2633.9157938496337, 3464.101615137755, 226846.1411949507, 261939.36136706528),
    }
    basis, criteria, _ = inputs
    original = run.solve_static

    def linux_native(force, weight, depth):
        result = original(force, weight, depth)
        span, length, vertical, resultant = observed[depth]
        result.update(suspended_horizontal_span_m=span, suspended_length_m=length,
                      vertical_tension_n=vertical, fairlead_tension_n=resultant)
        return result

    monkeypatch.setattr(run, "solve_static", linux_native)
    computed = run.compute_record(basis, criteria)
    assert_record_matches(computed, json.loads((RECORD / "results.json").read_text()))


def test_record_artifacts_use_utf8_lf(run, builder, inputs, tmp_path):
    basis, criteria, _ = inputs
    run.write_json(tmp_path / "results.json", run.compute_record(basis, criteria))
    builder.build(tmp_path)
    for path in tmp_path.iterdir():
        assert b"\r" not in path.read_bytes(), path.name
        path.read_bytes().decode("utf-8")


def test_native_json_writer_preserves_committed_serialization(run, tmp_path):
    """Keep the byte golden for formatting independently of native libm values."""
    original = (RECORD / "results.json").read_bytes()
    destination = tmp_path / "results.json"
    run.write_json(destination, json.loads(original))
    assert destination.read_bytes() == original


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


def test_interpreter_patch_version_is_metadata_only(run, builder, inputs, tmp_path, monkeypatch):
    """Reproduce computed bytes across patch versions while retaining run metadata."""
    import platform

    basis, criteria, _ = inputs
    recorded = run.compute_record(basis, criteria)
    assert_record_matches(recorded, json.loads((RECORD / "results.json").read_text()))
    run.write_json(tmp_path / "baseline.json", recorded)
    manifests = []
    for version in ("3.11.17", "3.11.15"):
        monkeypatch.setattr(platform, "python_version", lambda version=version: version)
        computed = run.compute_record(basis, criteria)
        assert computed == recorded
        assert "python_version" not in computed
        destination = tmp_path / version
        destination.mkdir()
        run.write_json(destination / "results.json", computed)
        assert (destination / "results.json").read_bytes() == (tmp_path / "baseline.json").read_bytes()
        manifest = builder.build(destination)
        assert manifest["python_version"] == version
        manifests.append(manifest)
    assert manifests[0] != manifests[1]
    computed_manifests = [{key: value for key, value in manifest.items() if key != "python_version"}
                          for manifest in manifests]
    assert computed_manifests[0] == computed_manifests[1]


@pytest.mark.parametrize("fault", ["native", "residual", "input", "metadata", "extra", "nan", "type",
                                   "input_type", "bool_type", "zero_sign", "horizontal"])
def test_record_comparison_rejects_drift(fault):
    expected = json.loads((RECORD / "results.json").read_bytes())
    actual = copy.deepcopy(expected)
    solution = actual["solutions"][0]
    if fault == "native":
        for _ in range(9):
            solution["native"]["suspended_length_m"] = math.nextafter(
                solution["native"]["suspended_length_m"], math.inf)
    elif fault == "residual":
        solution["resultant_error_n"] += 9 * math.ulp(solution["native"]["fairlead_tension_n"])
    elif fault == "input":
        solution["input"]["weight_n_m"] = math.nextafter(solution["input"]["weight_n_m"], math.inf)
    elif fault == "metadata":
        actual["revision"] = "changed"
    elif fault == "extra":
        solution["native"]["extra"] = True
    elif fault == "nan":
        solution["native"]["suspended_length_m"] = math.nan
    elif fault == "input_type":
        solution["water_depth_m"] = float(solution["water_depth_m"])
    elif fault == "bool_type":
        solution["checks"]["positive_horizontal_tension"] = 1
    elif fault == "zero_sign":
        actual["counts"]["qualified_environmental_cases"] = -0.0
    elif fault == "horizontal":
        solution["native"]["horizontal_tension_n"] = math.nextafter(
            solution["native"]["horizontal_tension_n"], math.inf)
    else:
        solution["native"]["suspended_length_m"] = str(solution["native"]["suspended_length_m"])
    with pytest.raises(AssertionError):
        assert_record_matches(actual, expected)


def test_retained_record_rebuild_matches_committed_manifest(run, builder, tmp_path):
    """Rebuild from historical native bytes; only runtime metadata may differ."""
    (tmp_path / "results.json").write_bytes((RECORD / "results.json").read_bytes())
    fresh = builder.build(tmp_path)
    committed = json.loads((RECORD / "manifest.json").read_bytes())
    for name in fresh["files"]:
        assert (tmp_path / name).read_bytes() == (RECORD / name).read_bytes(), name
    fresh["python_version"] = committed["python_version"]
    assert json.dumps(fresh, sort_keys=True) == json.dumps(committed, sort_keys=True)


@pytest.mark.parametrize("section,field", [
    ("native", "suspended_length_m"), ("native", "suspended_horizontal_span_m"),
    ("native", "vertical_tension_n"), ("native", "fairlead_tension_n"),
    (None, "grounded_length_m"), (None, "anchor_radius_m"),
    (None, "provided_static_mbl_to_tension"), (None, "resultant_error_n"),
])
def test_record_comparison_roundoff_boundary(section, field):
    expected = json.loads((RECORD / "results.json").read_bytes())
    actual = copy.deepcopy(expected)
    solution = actual["solutions"][0]
    values = solution[section] if section else solution
    target = values[field]
    scale = solution["native"]["fairlead_tension_n"] if field == "resultant_error_n" else target
    values[field] = target + 8 * math.ulp(scale)
    assert_record_matches(actual, expected)
    values[field] = target + 9 * math.ulp(scale)
    with pytest.raises(AssertionError):
        assert_record_matches(actual, expected)


def test_website_export_includes_its_linked_run_record(run, builder, inputs, tmp_path, monkeypatch):
    """The exported run-record link must resolve within the exported bundle."""
    basis, criteria, _ = inputs
    record = tmp_path / "record"
    record.mkdir()
    for original in RECORD.iterdir():
        if original.is_file():
            (record / original.name).write_bytes(original.read_bytes())
    source = tmp_path / "source.html"
    source.write_text("<html><head><title>Old</title></head><body>Old</body></html>")
    basis["website_source_sha256"] = run.digest(source)
    run.write_json(record / "basis.json", basis)
    monkeypatch.setattr(builder, "RECORD", record)
    output, website = tmp_path / "output", tmp_path / "website"
    output.mkdir()
    run.write_json(output / "results.json", run.compute_record(basis, criteria))
    builder.build(output, source, website)
    page = website / "content/demos/mooring.html"
    relative = "mooring-screening/README.md"
    assert f'href="{relative}"' in page.read_text()
    bundle = page.parent / "mooring-screening"
    assert (bundle / "README.md").read_bytes() == (output / "README.md").read_bytes()
    assert (bundle / "manifest.json").read_bytes() == (output / "manifest.json").read_bytes()
    native = json.loads((bundle / "manifest.json").read_text())
    for name, expected in native["files"].items():
        assert run.digest(bundle / name) == expected["sha256"]
    for name, expected in native["record_inputs"].items():
        assert run.digest(bundle / name) == expected
    exported = json.loads((website / "manifest.json").read_text())
    for path in bundle.iterdir():
        relative = path.relative_to(website).as_posix()
        assert exported["files"][relative]["sha256"] == run.digest(path)
