# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Effective commands preserve the two existing workflow recipes as data."""

import importlib.util
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

from tests.scripts._pytest_selection import pytest_selects_target

ROOT = Path(__file__).resolve().parents[2]
READER = ROOT / "scripts/ci/domain_gate_command.py"
TEST_FILE = "tests/scripts/test_domain_gate_command.py"
WORKFLOWS = ("quality-gates-by-domain", "full-matrix-sweep")


@pytest.fixture
def reader():
    spec = importlib.util.spec_from_file_location("domain_gate_command", READER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def legacy_command(gates, gate, workflow):
    """Frozen recipes from the two workflow readers at the base revision."""
    command = gates[gate]["command"]
    if gate == "tests-subsea":
        if workflow == "quality-gates-by-domain":
            command += " tests/riser_fatigue/ tests/riser_bom/"
        else:
            command += " tests/riser_bom/"
    return command


@pytest.mark.parametrize("workflow", WORKFLOWS)
@pytest.mark.parametrize("gate", ["tests-example", "tests-subsea"])
def test_synthetic_commands_preserve_bytes(reader, workflow, gate):
    command = 'python -m pytest "tests/a b/"  -k \'literal; text\' '
    gates = {gate: {"command": command}}
    assert reader.effective_command({"gates": gates}, gate, workflow) == legacy_command(
        gates, gate, workflow
    )


@pytest.mark.parametrize("workflow", WORKFLOWS)
def test_every_configured_gate_matches_legacy(reader, workflow):
    gate_file = ROOT / ".claude/quality-gates.yaml"
    document = reader.load_gates(gate_file)
    # A new entry must participate without changing a hardcoded gate count.
    document["gates"]["temporary-new-gate"] = {"command": "pytest tests/new/"}
    visited = set()
    for gate in document["gates"]:
        if "command" in document["gates"][gate]:
            assert reader.effective_command(document, gate, workflow) == legacy_command(
                document["gates"], gate, workflow
            )
        else:
            # Aggregate gates have no executable command; never invent one.
            with pytest.raises(ValueError):
                reader.effective_command(document, gate, workflow)
        visited.add(gate)
    assert "temporary-new-gate" in visited
    document["gates"]["temporary-new-gate"]["command"] = None
    with pytest.raises(ValueError):
        for gate in document["gates"]:
            reader.effective_command(document, gate, workflow)


@pytest.mark.parametrize("document,gate,workflow", [
    (None, "test", WORKFLOWS[0]),
    ({}, "test", WORKFLOWS[0]),
    ({"gates": []}, "test", WORKFLOWS[0]),
    ({"gates": {"test": []}}, "test", WORKFLOWS[0]),
    ({"gates": {"test": {}}}, "test", WORKFLOWS[0]),
    ({"gates": {"test": {"command": "pytest"}}}, "missing", WORKFLOWS[0]),
    ({"gates": {"test": {"command": "pytest"}}}, "test", "unknown"),
])
def test_invalid_structure_and_selection_fail(reader, document, gate, workflow):
    with pytest.raises(ValueError):
        reader.effective_command(document, gate, workflow)


@pytest.mark.parametrize("command", [None, 0, [], {}, "", " \t", *[
    "pytest" + delimiter + "echo sentinel"
    for delimiter in "\n\r\v\f\x1c\x1d\x1e\x85\u2028\u2029\0"
]])
def test_invalid_commands_fail(reader, command):
    with pytest.raises(ValueError):
        reader.effective_command({"gates": {"test": {"command": command}}},
                                 "test", WORKFLOWS[0])


@pytest.mark.parametrize("text", [
    "gates: {}\ngates: {}\n",
    "gates:\n  test: {command: pytest, command: echo}\n",
    "gates:\n  test: {command: pytest}\n  test: {command: echo}\n",
    "gates: {[a]: {command: pytest}}",
    "gates: [",
    "gates: !!python/object:builtins.object {}",
])
def test_invalid_yaml_fails(reader, tmp_path, text):
    path = tmp_path / "gates.yaml"
    path.write_text(text, encoding="utf-8")
    with pytest.raises(yaml.YAMLError):
        reader.load_gates(path)


def invoke(path, gate="tests-subsea", workflow=WORKFLOWS[0]):
    return subprocess.run(
        [sys.executable, str(READER), "--gate-file", str(path), "--gate", gate,
         "--workflow", workflow], capture_output=True, text=True,
    )


@pytest.mark.parametrize("workflow,suffix", [
    (WORKFLOWS[0], " tests/riser_fatigue/ tests/riser_bom/"),
    (WORKFLOWS[1], " tests/riser_bom/"),
])
def test_cli_returns_shell_text_without_evaluation(tmp_path, workflow, suffix):
    sentinel = tmp_path / "sentinel"
    command = f"pytest; touch '{sentinel}' $(touch '{sentinel}') `touch '{sentinel}'"
    gate_file = tmp_path / "gates.yaml"
    gate_file.write_text(yaml.safe_dump({"gates": {"tests-subsea": {"command": command}}}))
    result = invoke(gate_file, workflow=workflow)
    assert result.returncode == 0, result.stderr
    assert result.stdout == command + suffix + "\n"
    assert result.stderr == ""
    assert not sentinel.exists()


@pytest.mark.parametrize("kind", [
    "missing-file", "invalid-encoding", "invalid-yaml", "invalid-command",
    "missing-gate", "unknown-workflow",
])
def test_cli_fails_without_command_or_traceback(tmp_path, kind):
    path = tmp_path / "gates.yaml"
    if kind == "invalid-encoding":
        path.write_bytes(b"\xff")
    elif kind != "missing-file":
        path.write_text("gates: [" if kind == "invalid-yaml" else
                        "gates: {tests-subsea: {command: pytest}}", encoding="utf-8")
    if kind == "invalid-command":
        path.write_text("gates: {tests-subsea: {command: null}}", encoding="utf-8")
    result = invoke(path, gate="absent" if kind == "missing-gate" else "tests-subsea",
                    workflow="unknown" if kind == "unknown-workflow" else WORKFLOWS[0])
    assert result.returncode != 0
    assert result.stdout == ""
    assert result.stderr == "domain gate command: invalid configuration or selection\n"


def workflow_steps(workflow):
    document = yaml.safe_load((ROOT / f".github/workflows/{workflow}.yml").read_text())
    job = "domain-tests" if workflow == WORKFLOWS[0] else "sweep-tests"
    return document, document["jobs"][job]["steps"]


@pytest.mark.parametrize("workflow", WORKFLOWS)
def test_workflow_reader_and_consumer_contract(workflow):
    _, steps = workflow_steps(workflow)
    step = next(step for step in steps if step.get("id") == "gate")
    assert step["env"]["GATE_NAME"] == "tests-${{ matrix.domain }}"
    assert step["run"] == (
        "command=$(uv run --no-sources --with pyyaml==6.0.2 python "
        "scripts/ci/domain_gate_command.py "
        '--gate-file .claude/quality-gates.yaml --gate "$GATE_NAME" '
        f"--workflow {workflow})\n"
        '{\n  echo "command<<EOF"\n  echo "$command"\n  echo "EOF"\n'
        '} >> "$GITHUB_OUTPUT"\n'
    )
    consumer = next(step for step in steps if step.get("id") == "domain-gate")
    assert consumer["run"].count("${{ steps.gate.outputs.command }}") == 2
    assert 'exit "$code"' in consumer["run"]
    assert "--junitxml=reports/junit-${{ matrix.domain }}.xml" in consumer["run"]
    assert "scripts/ci/run_shard.py" in consumer["run"]


def assert_enrolled(document, pytestconfig):
    harness = document["jobs"]["ci-harness-tests"]
    assert "if" not in harness
    step = next(step for step in harness["steps"] if step.get("name") == "Run harness tests")
    assert "if" not in step
    assert pytest_selects_target(pytestconfig, step["run"], TEST_FILE, [TEST_FILE],
                                 require_explicit_target=True)


def test_guard_is_enrolled_unconditionally(pytestconfig):
    document, _ = workflow_steps(WORKFLOWS[0])
    assert_enrolled(document, pytestconfig)
    step = next(step for step in document["jobs"]["ci-harness-tests"]["steps"]
                if step.get("name") == "Run harness tests")
    step["run"] = step["run"].replace(TEST_FILE, "")
    with pytest.raises(AssertionError):
        assert_enrolled(document, pytestconfig)


@pytest.mark.parametrize("args", [[], ["--unknown", "untrusted-text"], ["--gate-file"]])
def test_cli_argument_errors_have_bounded_diagnostics(args):
    result = subprocess.run([sys.executable, str(READER), *args],
                            capture_output=True, text=True)
    assert result.returncode != 0
    assert result.stdout == ""
    assert result.stderr == "domain gate command: invalid configuration or selection\n"
