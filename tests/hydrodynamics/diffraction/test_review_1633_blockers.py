"""Regression contracts for review blockers on digitalmodel issue 1633."""
import ast
import importlib.util
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.hydrodynamics.diffraction.benchmark_runner import BenchmarkConfig, BenchmarkRunner
from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import ComparisonPolicy, MultiSolverComparator

ROOT = Path(__file__).resolve().parents[3]
CALLERS = [
    "run_3way_benchmark.py", "run_spar_benchmark.py",
    "run_validation_benchmark.py", "rerun_benchmarks_with_fidp.py",
    "regenerate_ship_benchmark.py", "regenerate_barge_benchmark.py",
]


@pytest.mark.parametrize("caller", CALLERS)
def test_benchmark_caller_policy_constructs_without_legacy_failure(caller, two_identical_results):
    tree = ast.parse((ROOT / "scripts/benchmark" / caller).read_text(encoding="utf-8"))
    calls = [node for node in ast.walk(tree) if isinstance(node, ast.Call)
             and isinstance(node.func, ast.Name) and node.func.id == "BenchmarkConfig"]
    assert calls, caller
    fields = {"tolerance", "solver_relative_uncertainty", "response_absolute_resolution",
              "minimum_explained_variance", "comparison_justification"}
    for call in calls:
        kwargs = {kw.arg: ast.literal_eval(kw.value) for kw in call.keywords if kw.arg in fields}
        runner = object.__new__(BenchmarkRunner)
        runner.config = BenchmarkConfig(**kwargs)
        # No measured uncertainty budget is supplied by these archived scripts.
        policy = runner._build_comparison_policy()
        assert policy is None
        report = MultiSolverComparator(two_identical_results, policy=policy).generate_report()
        assert report.comparison_status == "REFUSED"


@pytest.mark.parametrize("field", ["solver_relative_uncertainty", "response_absolute_resolution"])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_policy_rejects_nonfinite_uncertainty(field, value):
    kwargs = dict(solver_relative_uncertainty=0.01, response_absolute_resolution=0.001,
                  minimum_explained_variance=0.98, justification="Synthetic regression fixture")
    kwargs[field] = value
    with pytest.raises(ValueError):
        ComparisonPolicy(**kwargs)


def load_summary_module():
    spec = importlib.util.spec_from_file_location(
        "review_summary_regression", ROOT / "scripts/benchmark/validate_owd_vs_spec.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write_summary_fixture(tmp_path, report):
    (tmp_path / "validation_config.yaml").write_text(
        yaml.safe_dump({"cases": {"2.7": {"status": "pass"}}}), encoding="utf-8")
    output = tmp_path / "2.7/benchmark"
    output.mkdir(parents=True)
    (output / "benchmark_report.json").write_text(json.dumps(report), encoding="utf-8")


@pytest.mark.parametrize("scope", ["report", "pair", "dof", "consensus", "second_pair"])
def test_summary_refusal_overrides_handwritten_pass(tmp_path, monkeypatch, scope):
    module = load_summary_module()
    monkeypatch.setattr(module, "L00_DIR", tmp_path)
    monkeypatch.setattr(module, "CASES", {"2.7": {"description": "Synthetic case", "vessel_name": "Synthetic"}})
    pair = {"rao_comparisons": {"heave": {"max_magnitude_diff": 0.0}}}
    report = {"pairwise_results": {"a_vs_b": pair}, "consensus_by_dof": {}}
    targets = {"report": report, "pair": pair, "dof": pair["rao_comparisons"]["heave"]}
    if scope == "consensus":
        report["consensus_by_dof"]["HEAVE"] = {"comparison_status": "REFUSED"}
    elif scope == "second_pair":
        report["pairwise_results"]["b_vs_c"] = {"comparison_status": "REFUSED"}
    else:
        targets[scope]["comparison_status"] = "REFUSED"
    write_summary_fixture(tmp_path, report)
    results = module._build_results_from_config()
    assert results["2.7"]["status"] == "refused"
    html = module._generate_master_html(results, tmp_path).read_text(encoding="utf-8")
    assert "NEEDS INVESTIGATION" in html
    assert "ALL PASS" not in html
