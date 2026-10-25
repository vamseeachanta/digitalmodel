# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Executable evidence boundary: unknown skips and failures remain visible."""
import importlib.util
import json
import os
import subprocess
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest
from _pytest.reports import CollectReport

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "public_evidence", ROOT / "scripts/riser_database/check_public_tests.py"
)
HELPER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(HELPER)


@pytest.mark.parametrize("module,reasons", HELPER.SKIP_REASONS.items())
def test_documented_skip_reasons(module, reasons):
    """Each exact documented reason is accepted only for its emitting module."""
    for reason in reasons:
        node = next(iter(HELPER.SKIP_TESTS[module]))
        assert HELPER.documented_skip(f"tests/riser_database/{module}::{node}", reason)
        assert not HELPER.documented_skip("tests/riser_database/other.py::test", reason)
        assert not HELPER.documented_skip(f"{module}::{node}", reason + "; hidden failure")
        assert not HELPER.documented_skip(f"{module}::undocumented_test", reason)


def test_unknown_skip_and_teardown_failure_are_not_success():
    evidence = HELPER.Evidence()
    evidence.pytest_runtest_logreport(SimpleNamespace(
        nodeid="test.py::test", failed=False, skipped=True,
        longrepr=("test.py", 1, "Skipped: unexpected"), when="setup",
    ))
    evidence.pytest_runtest_logreport(SimpleNamespace(
        nodeid="test.py::other", failed=False, skipped=False, when="call",
    ))
    evidence.pytest_runtest_logreport(SimpleNamespace(
        nodeid="test.py::other", failed=True, skipped=False, when="teardown",
    ))
    evidence.pytest_collectreport(SimpleNamespace(nodeid="bad.py", failed=True))
    result = evidence.result()
    assert result["passed"] == 0
    assert result["failed"] == 1
    assert result["collection_errors"] == 1
    assert result["unexpected_skips"] == {"test.py::test": "unexpected"}


def test_pack_documents_skip_contract():
    pack = (ROOT / "src/digitalmodel/riser_database/docs/agent-pack.md").read_text(encoding="utf-8")
    assert "check_public_tests.py" in pack
    for module in HELPER.SKIP_REASONS:
        assert module in pack
    assert "81 passed, 0 failed, 12 skipped" in pack
    assert "NOT_RECORDED" in pack


@pytest.mark.parametrize("reason", [
    "undocumented module exclusion",
    HELPER.SKIP_REASONS["test_citations.py"][0],
], ids=["unknown-reason", "documented-runtime-reason"])
def test_collection_skips_are_counted_and_never_inherit_runtime_exemptions(reason):
    node = "tests/riser_database/test_citations.py"
    evidence = HELPER.Evidence()
    evidence.pytest_collectreport(CollectReport(
        nodeid=node, outcome="skipped", result=[],
        longrepr=(node, 1, f"Skipped: {reason}"),
    ))
    result = evidence.result()
    assert result["skipped"] == 1
    assert result["skip_modules"] == {"test_citations.py": 1}
    assert result["skips"] == {node: reason}
    assert result["unexpected_skips"] == {node: reason}


@pytest.mark.parametrize("skip_module", [False, True])
def test_public_command_audits_collection_skips_with_a_passing_neighbour(tmp_path, skip_module):
    tests = tmp_path / "tests/riser_database"
    tests.mkdir(parents=True)
    (tests / "test_positive.py").write_text(
        "def test_positive():\n    assert 2 + 2 == 4\n", encoding="utf-8", newline="\n",
    )
    if skip_module:
        (tests / "test_collection_skip.py").write_text(
            'import pytest\npytest.skip("undocumented module exclusion", allow_module_level=True)\n',
            encoding="utf-8", newline="\n",
        )
    output = tmp_path / "evidence.json"
    env = {key: value for key, value in os.environ.items() if not key.startswith("GIT_")}
    command = subprocess.run([
        sys.executable, Path(HELPER.__file__).as_posix(),
        "--output", output.as_posix(),
        f"--basetemp={(tmp_path / 'pytest-tmp').as_posix()}",
    ], cwd=tmp_path, env=env, capture_output=True, encoding="utf-8", check=False)
    assert command.returncode == int(skip_module)
    result = json.loads(output.read_text(encoding="utf-8"))
    assert result["passed"] == 1
    assert result["failed"] == result["collection_errors"] == 0
    assert result["skipped"] == int(skip_module)
    expected = {"tests/riser_database/test_collection_skip.py": "undocumented module exclusion"}
    assert result["unexpected_skips"] == (expected if skip_module else {})
