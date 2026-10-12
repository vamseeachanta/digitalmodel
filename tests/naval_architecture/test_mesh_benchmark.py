"""Benchmark evidence preserves analytic comparators and physical areas."""
import importlib.util
from pathlib import Path
import pytest


def load_script():
    path = Path(__file__).resolve().parents[2] / "scripts/validation/benchmark_mesh_hydrostatics.py"
    spec = importlib.util.spec_from_file_location("mesh_benchmark", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_benchmark_matches_analytic_wigley_and_excludes_caps():
    module = load_script()
    record = module.run_case(nx=40, nz=20, draft=6.0)
    assert record["volume_m3"] == pytest.approx(4 * 100 * 10 * 6 / 9, rel=0.005)
    assert record["wetted_area_m2"] == pytest.approx(record["reference_area_m2"], rel=0.005)
    assert record["generated_physical_area_m2"]["min"] > 0
    assert record["generated_physical_area_m2"]["count"] == 4 * 40 * 20
    assert record["station_count"] == 10
    assert record["section_evaluation_count"] == 11


def valid_record(module):
    result = {"quantities": {"V": {"value": 10.0}}}
    return {"criteria": dict(module.CRITERIA), "numpy": "test-version",
            "source_faces": 1_000_000, "volume_relative_error": 5e-5,
            "area_relative_error": 1e-4, "elapsed_seconds_validate_hydrostatics": 50.0,
            "peak_rss_bytes": 1_000_000_000, "implementation": "legacy",
            "implementation_sha256": "baseline-sha", "benchmark_script_sha256": "script-sha",
            "source_sha256": {"hull_fixtures.py": "fixture-sha"},
            "scipy": "test-scipy", "result": result, "result_sha256": module.canonical_digest(result)}


def test_qualification_uses_live_criteria_bounds(monkeypatch):
    module = load_script()
    record = valid_record(module)
    assert module.is_qualified(record)
    monkeypatch.setitem(module.CRITERIA, "volume_relative_error_max", 0.0)
    assert not module.is_qualified(record)


def test_resource_absence_does_not_prevent_diagnostics(monkeypatch):
    import sys
    monkeypatch.setitem(sys.modules, "resource", None)
    module = load_script()
    record = module.run_case(nx=40, nz=20)
    assert record["peak_rss_bytes"] is None
    assert not record["qualified"]
    assert record["volume_m3"] > 0


def test_comparison_rejects_dependency_drift_and_regression():
    module = load_script()
    baseline = valid_record(module)
    current = dict(baseline, implementation="current", implementation_sha256="current-sha")
    assert module.compare_records(baseline, current)["qualified"]
    current["numpy"] = "different-version"
    assert not module.compare_records(baseline, current)["qualified"]
    current = dict(baseline, implementation="current", implementation_sha256="current-sha",
                   elapsed_seconds_validate_hydrostatics=56.0)
    assert not module.compare_records(baseline, current)["qualified"]


def test_partial_draft_exercises_mixed_faces_and_records_sections():
    module = load_script()
    record = module.run_case(nx=40, nz=20)
    draft = record["draft_m"]
    area = 10 * (draft**2 / 6 - draft**3 / (3 * 6**2))
    assert record["reference_midship_area_m2"] == pytest.approx(area)
    assert record["midship_area_m2"] == pytest.approx(area, rel=0.005)
    assert record["reference_volume_m3"] == pytest.approx(2 * 100 / 3 * area)
    assert all(abs((station + 50) / 2.5 - round((station + 50) / 2.5)) > 0.001
               for station in record["grid_stations_m"])
    assert record["generated_physical_area_m2"]["min"] < record["source_area_m2"]["min"]
    assert "C_M" in record["result"]["quantities"]
    assert "C_P" in record["result"]["quantities"]
    assert len(record["result_sha256"]) == 64


@pytest.mark.parametrize("field", ["scipy", "benchmark_script_sha256", "result_sha256"])
def test_comparison_binds_dependencies_script_and_all_outputs(field):
    module = load_script()
    baseline = valid_record(module)
    revised = dict(baseline, implementation="current", implementation_sha256="current-sha")
    revised[field] = "changed"
    assert not module.compare_records(baseline, revised)["qualified"]


def test_comparison_refuses_self_comparison_and_fixture_drift():
    module = load_script()
    baseline = valid_record(module)
    assert not module.compare_records(baseline, baseline)["qualified"]
    revised = dict(baseline, implementation="current", implementation_sha256="current-sha",
                   source_sha256={"hull_fixtures.py": "changed-fixture"})
    assert not module.compare_records(baseline, revised)["qualified"]


def test_comparison_refuses_tampered_outputs_with_stale_digest():
    module = load_script()
    baseline = valid_record(module)
    revised = dict(baseline, implementation="current", implementation_sha256="current-sha",
                   result={"quantities": {"V": {"value": 20.0}}})
    assert not module.compare_records(baseline, revised)["qualified"]
