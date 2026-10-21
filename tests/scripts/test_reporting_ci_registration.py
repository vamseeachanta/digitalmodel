# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pin reporting collection to the command consumed by hosted domain tests.

The expected root and domain come from issue 2450 and tests/DOMAINS.md.
No engineering constants or numerical acceptance limits are introduced.
"""

import importlib.util
import shlex
import sys
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
REPORTING_ROOT = "tests/reporting/"
DOMAIN = "infrastructure-other"


def test_reporting_gate_is_enabled_and_collects_the_folder():
    gates = yaml.safe_load(
        (ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8")
    )["gates"]
    gate = gates[f"tests-{DOMAIN}"]
    assert gate["enabled"] is True
    assert gate["domain"] == DOMAIN
    assert REPORTING_ROOT in gate["test_roots"]
    command = shlex.split(gate["command"])
    pytest_args = command[command.index("pytest") + 1:]
    assert REPORTING_ROOT in pytest_args
    assert gate["failure_action"] == "block"


@pytest.mark.parametrize("changed_path", [
    "tests/reporting/test_calc_report.py",
    "tests/reporting/test_engine_golden.py",
    "tests/reporting/golden/standard_report_plain.html",
    "src/digitalmodel/reporting/calc_report.py",
    "src/digitalmodel/reporting/assets/calc_report_format.html",
])
def test_reporting_changes_select_the_collecting_domain(changed_path, monkeypatch):
    spec = importlib.util.spec_from_file_location(
        "reporting_ci_domain_detector", ROOT / "scripts/ci/detect_touched_domains.py"
    )
    detector = importlib.util.module_from_spec(spec)
    monkeypatch.setitem(sys.modules, spec.name, detector)
    spec.loader.exec_module(detector)
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    owners = [domain.name for domain in domains if REPORTING_ROOT in domain.roots]
    assert owners == [DOMAIN]
    selected = detector.touched_domains([changed_path], domains)
    assert DOMAIN in {domain.name for domain in selected}


def test_registration_guard_runs_in_the_independent_hosted_harness():
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text(
            encoding="utf-8"
        )
    )
    harness = workflow["jobs"]["ci-harness-tests"]
    assert "if" not in harness
    step = next(step for step in harness["steps"] if step["name"] == "Run harness tests")
    assert "if" not in step
    assert "tests/scripts/test_reporting_ci_registration.py" in shlex.split(step["run"])
