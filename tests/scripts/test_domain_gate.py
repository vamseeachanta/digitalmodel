# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Exercise the commands and required aggregate used by both CI workflows."""

import importlib.util
import os
import shlex
import sys
import subprocess
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("domain_gate", ROOT / "scripts/ci/domain_gate.py")
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)


def test_holds_are_file_specific_and_shrink_only():
    policy = gate.read_policy(ROOT)
    ceiling = gate.initial_hold_files(ROOT)
    assert set(policy["held"]) <= ceiling
    for path, entry in policy["held"].items():
        assert Path(path).suffix == ".py"
        assert entry["issue"] == "#2083"
        assert entry["reason"]
        assert entry["gate"] in gate.INITIAL_HELD_ROOTS
        assert f"--ignore={path}" in gate.command_for(entry["gate"], ROOT)
        assert f"--ignore={path}" not in gate.command_for(entry["gate"], ROOT, sweep=True)


def test_hold_registry_cannot_expand_in_a_later_pull_request():
    gate.validate_shrink_only({"held": {"a": {}}}, {"held": {"a": {}, "b": {}}})
    with pytest.raises(ValueError, match="grow"):
        gate.validate_shrink_only({"held": {"a": {}, "b": {}}}, {"held": {"a": {}}})
    with pytest.raises(ValueError, match="change ownership"):
        gate.validate_shrink_only({"held": {"a": {"gate": "b"}}}, {"held": {"a": {"gate": "a"}}})


@pytest.mark.parametrize("name,path", [
    ("tests-infrastructure-core", "tests/benchmarks/test_cp_benchmarks.py"),
    ("tests-marine-engineering", "tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py"),
])
def test_cathodic_protection_has_one_owner(name, path):
    for sweep in (False, True):
        assert f"--ignore={path}" in gate.command_for(name, ROOT, sweep=sweep)
        assert f"--ignore={path}" not in gate.command_for("tests-cathodic-protection", ROOT, sweep=sweep)


@pytest.mark.parametrize("filename,job,sweep", [
    ("quality-gates-by-domain.yml", "domain-tests", False),
    ("full-matrix-sweep.yml", "sweep-tests", True),
])
@pytest.mark.parametrize("name", [
    "tests-infrastructure-core", "tests-structural", "tests-misc",
    "tests-marine-engineering", "tests-cathodic-protection",
])
def test_workflow_reader_uses_effective_commands(filename, job, sweep, name, tmp_path):
    workflow = yaml.safe_load((ROOT / ".github/workflows" / filename).read_text())
    step = next(step for step in workflow["jobs"][job]["steps"] if step.get("id") == "gate")
    assert "scripts/ci/domain_gate.py" in step["run"]
    assert ("--sweep" in step["run"]) is sweep
    assert "--junitxml=reports/junit-" in str(workflow["jobs"][job]["steps"])
    assert "-o junit_family=xunit2" in str(workflow["jobs"][job]["steps"])
    # Replace only dependency bootstrapping; execute the real workflow reader.
    script = step["run"].replace(
        "uv run --no-sources --with pyyaml==6.0.2 python", shlex.quote(sys.executable)
    )
    output = tmp_path / "output"
    env = {**os.environ, "GATE_NAME": name, "GITHUB_OUTPUT": str(output)}
    result = subprocess.run(["bash", "-euo", "pipefail", "-c", script],
                            cwd=ROOT, env=env, capture_output=True, text=True)
    assert result.returncode == 0, result.stdout + result.stderr
    assert output.read_text() == "command<<EOF\n" + gate.command_for(name, ROOT, sweep=sweep) + "\nEOF\n"


@pytest.mark.parametrize("detect,harness,domain,has_domains,expected", [
    ("success", "success", "skipped", "false", 0),
    ("success", "failure", "skipped", "false", 1),
    ("failure", "success", "skipped", "false", 1),
    ("success", "success", "success", "true", 0),
    ("success", "success", "failure", "true", 1),
    ("success", "success", "skipped", "true", 1),
])
def test_required_aggregate_checks_docs_and_code(detect, harness, domain, has_domains, expected, tmp_path):
    workflow = yaml.safe_load((ROOT / ".github/workflows/quality-gates-by-domain.yml").read_text())
    aggregate = workflow["jobs"]["domain-tests-required"]
    assert aggregate["if"] == "always()"
    assert aggregate["needs"] == ["detect-domains", "ci-harness-tests", "domain-tests"]
    harness_job = workflow["jobs"]["ci-harness-tests"]
    assert "if" not in harness_job
    assert not harness_job.get("continue-on-error", False)
    script = next(step["run"] for step in aggregate["steps"] if step["name"] == "Enforce required domain result")
    env = {**os.environ, "DETECT_RESULT": detect, "HARNESS_RESULT": harness,
           "DOMAIN_RESULT": domain, "HAS_DOMAINS": has_domains, "GITHUB_EVENT_NAME": "push"}
    result = subprocess.run(["bash", "-c", script], env=env, cwd=tmp_path, capture_output=True, text=True)
    assert result.returncode == expected, result.stdout + result.stderr


def test_unrecorded_collection_exclusion_is_visible(tmp_path):
    directory = tmp_path / "tests" / "a"
    directory.mkdir(parents=True)
    (directory / "test_hidden.py").write_text("")
    (tmp_path / "pytest.ini").write_text("[pytest]\nnorecursedirs = tests/a\n")
    (tmp_path / "conftest.py").write_text("collect_ignore = []\n")
    assert gate.collection_exclusions(tmp_path) == {"tests/a/test_hidden.py"}


def test_nested_collection_ignore_is_visible(tmp_path):
    directory = tmp_path / "tests" / "a"
    directory.mkdir(parents=True)
    (directory / "test_hidden.py").write_text("")
    (directory / "conftest.py").write_text("collect_ignore = ['test_hidden.py']\n")
    (tmp_path / "pytest.ini").write_text("[pytest]\nnorecursedirs =\n")
    (tmp_path / "conftest.py").write_text("collect_ignore = []\n")
    assert gate.collection_exclusions(tmp_path) == {"tests/a/test_hidden.py"}


@pytest.mark.parametrize("control", [
    "collect_ignore = []\ncollect_ignore.append('test_hidden.py')\n",
    "collect_ignore = []\ncollect_ignore += ['test_hidden.py']\n",
    "def pytest_ignore_collect(collection_path, config):\n    return True\n",
])
def test_dynamic_collection_exclusions_fail_closed(tmp_path, control):
    directory = tmp_path / "tests"
    directory.mkdir()
    (directory / "test_hidden.py").write_text("")
    (directory / "conftest.py").write_text(control)
    (tmp_path / "pytest.ini").write_text("[pytest]\nnorecursedirs =\n")
    (tmp_path / "conftest.py").write_text("collect_ignore = []\n")
    with pytest.raises(ValueError, match="Unsupported"):
        gate.collection_exclusions(tmp_path)
