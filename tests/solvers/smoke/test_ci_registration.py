"""Keep this new test directory in the explicit solver-smoke CI invocation."""

import importlib.util
from pathlib import Path
import shlex
import sys

import yaml


ROOT = Path(__file__).resolve().parents[3]


def test_smoke_regressions_are_in_metadata_and_executed_arguments():
    config = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())
    gate = config["gates"]["tests-solver-smoke"]
    assert "tests/solvers/smoke/" in gate["test_roots"]
    assert "tests/solvers/smoke/" in shlex.split(gate["command"])
    assert "`tests/solvers/smoke/`" in (ROOT / "tests/DOMAINS.md").read_text()


def test_touched_source_and_new_test_paths_select_smoke(monkeypatch):
    path = ROOT / "scripts/ci/detect_touched_domains.py"
    spec = importlib.util.spec_from_file_location("smoke_domain_detector", path)
    detector = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, spec.name, detector)
    spec.loader.exec_module(detector)
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    for changed in ["src/digitalmodel/solvers/smoke/probes.py",
                    "tests/solvers/smoke/test_orcaflex_probe_contract.py"]:
        selected = detector.touched_domains([changed], domains)
        assert "solver-smoke" in {domain.name for domain in selected}
