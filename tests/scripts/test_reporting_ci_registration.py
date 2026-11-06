# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pin reporting collection to the command consumed by hosted domain tests.

The expected root and domain come from issue 2450 and tests/DOMAINS.md.
No engineering constants or numerical acceptance limits are introduced.
"""

import importlib.util
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

from tests.scripts._pytest_selection import pytest_selects_target

ROOT = Path(__file__).resolve().parents[2]
REPORTING_ROOT = "tests/reporting/"
DOMAIN = "infrastructure-other"
# Collection acceptance names are specified in issue 2450.
REQUIRED_MODULES = {
    "tests/reporting/test_calc_report.py",
    "tests/reporting/test_engine_golden.py",
}


def assert_reporting_gate_coverage(gate, pytestconfig):
    assert gate["enabled"] is True
    assert gate["domain"] == DOMAIN
    assert REPORTING_ROOT in gate["test_roots"]
    assert pytest_selects_target(
        pytestconfig, gate["command"], REPORTING_ROOT,
        (path.relative_to(ROOT).as_posix() for path in (ROOT / REPORTING_ROOT).rglob("test_*.py")),
        require_explicit_target=True,
    )
    assert gate["failure_action"] == "block"


def test_reporting_gate_is_enabled_and_collects_the_folder(pytestconfig):
    gates = yaml.safe_load(
        (ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8")
    )["gates"]
    gate = gates[f"tests-{DOMAIN}"]
    assert_reporting_gate_coverage(gate, pytestconfig)


@pytest.mark.parametrize("field,value", [
    ("enabled", False), ("failure_action", "warn"),
    ("failure_action", "report"), ("failure_action", None),
])
def test_reporting_guard_rejects_nonblocking_metadata(field, value, pytestconfig):
    gate = {
        "enabled": True, "domain": DOMAIN, "failure_action": "block",
        "test_roots": [REPORTING_ROOT],
        "command": f"python -m pytest {REPORTING_ROOT}",
    }
    gate[field] = value
    with pytest.raises(AssertionError):
        assert_reporting_gate_coverage(gate, pytestconfig)


@pytest.mark.parametrize("args", [
    "tests/unrelated/ --ignore tests/reporting/",
    "tests/unrelated/ --rootdir tests/reporting/",
    "tests/unrelated/ --confcutdir tests/reporting/",
    "tests/reporting/ --ignore=tests/reporting/test_calc_report.py",
    "tests/reporting/ --deselect=tests/reporting/test_engine_golden.py::test_required",
])
def test_reporting_guard_rejects_incomplete_commands(args, pytestconfig):
    gate = {
        "enabled": True, "domain": DOMAIN, "failure_action": "block",
        "test_roots": [REPORTING_ROOT], "command": f"python -m pytest {args}",
    }
    with pytest.raises(AssertionError):
        assert_reporting_gate_coverage(gate, pytestconfig)


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


def test_registration_guard_runs_in_the_independent_hosted_harness(pytestconfig):
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text(
            encoding="utf-8"
        )
    )
    harness = workflow["jobs"]["ci-harness-tests"]
    assert "if" not in harness
    step = next(step for step in harness["steps"] if step["name"] == "Run harness tests")
    assert "if" not in step
    path = "tests/scripts/test_reporting_ci_registration.py"
    assert pytest_selects_target(
        pytestconfig, step["run"], path, [path], require_explicit_target=True,
    )


@pytest.mark.parametrize("ignored_module", [None, *sorted(REQUIRED_MODULES)])
def test_reporting_folder_really_collects_required_modules(tmp_path, ignored_module):
    """Check real collection, including successful but incomplete negative controls.

    Issue 2450 requires both named modules. The expected set is read directly
    from that requirement, independently of gate metadata. Ignoring either
    module must remove exactly that module from the required collected set,
    even though pytest still exits successfully for the remaining folder.
    """
    command = [
        sys.executable,
        "-m",
        "pytest",
        REPORTING_ROOT,
        "--collect-only",
        "--verbosity=-1",
        "-p",
        "no:cacheprovider",
        f"--basetemp={tmp_path / 'collection'}",
    ]
    if ignored_module is not None:
        command.append(f"--ignore={ignored_module}")
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
    assert result.returncode == 0, result.stdout + result.stderr
    collected_modules = {
        line.split("::", 1)[0]
        for line in result.stdout.splitlines()
        if line.startswith(REPORTING_ROOT) and "::" in line
    }
    missing = REQUIRED_MODULES - collected_modules
    assert missing == ({ignored_module} if ignored_module is not None else set())
