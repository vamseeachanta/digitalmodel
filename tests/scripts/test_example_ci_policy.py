# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep the explicit, non-gating example policy reviewable (issue #1992)."""

import configparser
import fnmatch
import importlib.util
from pathlib import Path
import shlex
import subprocess
import sys

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/ci/detect_touched_domains.py"


@pytest.fixture(scope="module")
def detector():
    spec = importlib.util.spec_from_file_location("example_ci_detector", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize(
    "path",
    [
        "examples/demos/gtm/tests/test_demo_01_cli.py",
        "examples/demos/gtm/tests/test_prospect_adapter.py",
        "examples/demos/gtm/demo_02_wall_thickness_multicode.py",
        "examples/domains/workflows/test_all_workflows.py",
        "examples/prototypes/orcaflex-agent/generate_test.py",
        "examples/future/tests/test_new_example.py",
    ],
)
def test_example_changes_have_an_explicit_non_gating_policy(detector, path):
    """Issue #1992 option 2: examples select the empty domain set by policy."""
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    assert detector.is_no_domain_path(path)
    assert not detector.is_full_matrix_trigger(path)
    assert detector.touched_domains([path], domains) == []


def test_every_tracked_example_test_is_classified(detector):
    """Inventory comes from git and pytest.ini, rather than a frozen file count."""
    config = configparser.ConfigParser()
    config.read(ROOT / "pytest.ini")
    patterns = config["pytest"]["python_files"].split()
    paths = subprocess.check_output(
        ["git", "ls-files", "-z", "--", "examples/"], cwd=ROOT
    ).decode().split("\0")
    candidates = [
        path for path in paths
        if any(fnmatch.fnmatchcase(Path(path).name, pattern) for pattern in patterns)
    ]
    assert candidates, "The example-test census unexpectedly found no candidates"
    assert all(detector.is_no_domain_path(path) for path in candidates)


def test_example_policy_does_not_hide_another_changed_domain(detector):
    """Hand calculation: empty set union workflows equals workflows."""
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    selected = detector.touched_domains(
        ["examples/future/tests/test_example.py", "tests/scripts/test_example_ci_policy.py"],
        domains,
    )
    assert {domain.name for domain in selected} == {"workflows"}
    assert not detector.is_no_domain_path("tests/scripts/test_example_ci_policy.py")


def test_example_policy_does_not_hide_full_matrix_changes(detector):
    """Hand calculation: empty set union every domain equals every domain."""
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    assert detector.touched_domains(
        ["examples/future/tests/test_example.py", "pytest.ini"], domains
    ) == domains


def test_examples_remain_collectable():
    """Non-gating is a dispatch policy, not a collection exclusion."""
    spec = importlib.util.spec_from_file_location("example_root_conftest", ROOT / "conftest.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    assert "examples" not in module.collect_ignore
    config = configparser.ConfigParser()
    config.read(ROOT / "pytest.ini")
    assert "examples" not in config["pytest"]["norecursedirs"].split()


def test_example_policy_document_changes_dispatch_its_guard(detector):
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    selected = detector.touched_domains(
        ["docs/development/example-test-policy.md"], domains
    )
    assert {domain.name for domain in selected} == {"workflows"}


def test_example_policy_guard_is_executed_by_its_domain():
    """Ownership must correspond to a path in the actual domain gate command."""
    gates = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text())["gates"]
    gate = gates["tests-workflows"]
    assert gate["enabled"]
    assert "tests/scripts/" in gate["test_roots"]
    assert "tests/scripts/" in shlex.split(gate["command"])
