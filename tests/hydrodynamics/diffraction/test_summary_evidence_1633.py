"""Summary evidence regressions for the Claude review of issue 1633."""
import importlib.util
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import MultiSolverComparator


def load_module(tmp_path, monkeypatch):
    path = Path(__file__).resolve().parents[3] / "scripts/benchmark/validate_owd_vs_spec.py"
    spec = importlib.util.spec_from_file_location("summary_evidence_test", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    monkeypatch.setattr(module, "L00_DIR", tmp_path)
    monkeypatch.setattr(module, "CASES", {"2.7": {"description": "Synthetic case", "vessel_name": "Synthetic"}})
    (tmp_path / "validation_config.yaml").write_text(
        yaml.safe_dump({"cases": {"2.7": {"status": "pass", "notes": "r=1.0"}}}), encoding="utf-8")
    return module


def write_report(tmp_path, report):
    directory = tmp_path / "2.7/benchmark"
    directory.mkdir(parents=True)
    (directory / "benchmark_report.json").write_text(json.dumps(report), encoding="utf-8")


def report_fixture():
    dofs = ("surge", "sway", "heave", "roll", "pitch", "yaw")
    return {"comparison_status": "DECIDED", "overall_consensus": "FULL",
            "consensus_by_dof": {d.upper(): {"mean_pairwise_correlation": 1.0} for d in dofs},
            "pairwise_results": {"a_vs_b": {"comparison_status": "DECIDED",
                "hydrostatic_comparison": None, "added_mass_correlations": {"1,1": 1.0},
                "damping_correlations": {"1,1": 1.0},
                "rao_comparisons": {d: {"max_magnitude_diff": 0.0, "n_points": 12,
                    "magnitude_quality": "COMPARED", "magnitude_correlation": 1.0,
                    "phase_correlation": None, "max_phase_diff": None} for d in dofs}}}}


def test_missing_report_never_manufactures_comparison(tmp_path, monkeypatch):
    module = load_module(tmp_path, monkeypatch)
    results = module._build_results_from_config()
    assert results["2.7"]["status"] != "completed"
    assert not results["2.7"]["dof_summary_by_body"]
    html = module._generate_master_html(results, tmp_path).read_text(encoding="utf-8")
    assert "ALL PASS" not in html


def test_decided_report_with_metadata_renders(tmp_path, monkeypatch):
    module = load_module(tmp_path, monkeypatch)
    write_report(tmp_path, report_fixture())
    results = module._build_results_from_config()
    html = module._generate_master_html(results, tmp_path).read_text(encoding="utf-8")
    assert "ALL PASS" in html


@pytest.mark.parametrize("status", ["owd_only", "comparison_failed", "refused"])
def test_noncompleted_status_never_passes(tmp_path, monkeypatch, status):
    module = load_module(tmp_path, monkeypatch)
    result = {"2.7": {"description": "Synthetic", "status": status}}
    html = module._generate_master_html(result, tmp_path).read_text(encoding="utf-8")
    assert "ALL PASS" not in html


@pytest.mark.parametrize("sample_count", [0, None])
def test_absent_samples_never_pass(tmp_path, monkeypatch, sample_count):
    module = load_module(tmp_path, monkeypatch)
    report = report_fixture()
    report.pop("comparison_status")  # Legacy data cannot establish an authoritative verdict.
    for entry in report["pairwise_results"]["a_vs_b"]["rao_comparisons"].values():
        entry["n_points"] = sample_count
    write_report(tmp_path, report)
    results = module._build_results_from_config()
    html = module._generate_master_html(results, tmp_path).read_text(encoding="utf-8")
    assert "ALL PASS" not in html


def test_empty_results_never_pass(tmp_path, monkeypatch):
    module = load_module(tmp_path, monkeypatch)
    html = module._generate_master_html({}, tmp_path).read_text(encoding="utf-8")
    assert "ALL PASS" not in html


def test_real_serialized_dof_refusal_is_recognized(tmp_path, monkeypatch, two_identical_results):
    module = load_module(tmp_path, monkeypatch)
    report_path = tmp_path / "serialized.json"
    MultiSolverComparator(two_identical_results).export_report_json(report_path)
    report = json.loads(report_path.read_text(encoding="utf-8"))
    # A direct DOF record must retain its own refusal independently of aggregation.
    entry = next(iter(next(iter(report["pairwise_results"].values()))["rao_comparisons"].values()))
    entry["refusal_reason"] = "INSUFFICIENT_DATA"
    assert module._report_has_refusal(entry)


@pytest.mark.parametrize("field", ["mean_pairwise_correlation", "max_magnitude_diff"])
def test_null_metrics_render_without_passing(tmp_path, monkeypatch, field):
    module = load_module(tmp_path, monkeypatch)
    report = report_fixture()
    if field == "mean_pairwise_correlation":
        report["consensus_by_dof"]["HEAVE"][field] = None
    else:
        report["pairwise_results"]["a_vs_b"]["rao_comparisons"]["heave"][field] = None
    write_report(tmp_path, report)
    results = module._build_results_from_config()
    html = module._generate_master_html(results, tmp_path).read_text(encoding="utf-8")
    assert "ALL PASS" not in html
    assert "Unavailable" in html
