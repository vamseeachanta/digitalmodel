# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Keep staged-byte scanner regressions reachable through domain CI."""

import importlib.util
import json
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

from tests.scripts._pytest_selection import pytest_selects_target

ROOT = Path(__file__).resolve().parents[2]
DETECTOR = ROOT / "scripts/ci/detect_touched_domains.py"
REGRESSIONS = "tests/scripts/test_no_conflict_markers.py"
SCANNERS = (
    "scripts/enforcement/check-no-conflict-markers.sh",
    "scripts/enforcement/check_no_conflict_markers.py",
)


def load_detector():
    spec = importlib.util.spec_from_file_location("conflict_marker_detector", DETECTOR)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def assert_regressions_executed(domains, pytestconfig):
    """Check both ownership and the pytest operands actually used by CI."""
    workflows = next(domain for domain in domains if domain.name == "workflows")
    gates = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())["gates"]
    gate = gates["tests-workflows"]
    test_path = Path(REGRESSIONS)
    assert any(test_path.is_relative_to(root) for root in workflows.roots)
    assert any(test_path.is_relative_to(root) for root in gate["test_roots"])
    assert pytest_selects_target(pytestconfig, gate["command"], REGRESSIONS, [REGRESSIONS])
    assert (ROOT / test_path).is_file()


@pytest.mark.parametrize(
    "changed_paths",
    [[SCANNERS[0]], [SCANNERS[1]], [REGRESSIONS], [*SCANNERS, REGRESSIONS]],
    ids=["shell-only", "python-only", "test-only", "complete-change"],
)
def test_scanner_changes_select_executed_regressions(changed_paths, pytestconfig):
    detector = load_detector()
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    selected = detector.touched_domains(changed_paths, domains)
    assert {domain.name for domain in selected} == {"workflows"}
    assert_regressions_executed(selected, pytestconfig)


def test_full_sweep_includes_executed_regressions(pytestconfig):
    result = subprocess.run(
        [sys.executable, str(DETECTOR), "--mode", "full"],
        cwd=ROOT,
        check=True,
        text=True,
        capture_output=True,
    )
    matrix = json.loads(result.stdout)["include"]
    assert "workflows" in {entry["domain"] for entry in matrix}
    detector = load_detector()
    assert_regressions_executed(detector.parse_domains(ROOT / "tests/DOMAINS.md"), pytestconfig)

    # The touched workflow and the sweep must consume the same gate command.
    for filename, job in (
        ("quality-gates-by-domain.yml", "domain-tests"),
        ("full-matrix-sweep.yml", "sweep-tests"),
    ):
        workflow = yaml.safe_load((ROOT / ".github/workflows" / filename).read_text())
        steps = workflow["jobs"][job]["steps"]
        read_gate = next(step for step in steps if step.get("id") == "gate")
        run_gate = next(step for step in steps if step.get("id") == "domain-gate")
        assert read_gate["env"]["GATE_NAME"] == "tests-${{ matrix.domain }}"
        assert ".claude/quality-gates.yaml" in read_gate["run"]
        assert 'gates[os.environ["GATE_NAME"]]["command"]' in read_gate["run"]
        assert "${{ steps.gate.outputs.command }}" in run_gate["run"]
