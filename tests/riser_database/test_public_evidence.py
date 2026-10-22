# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Executable evidence boundary: unknown skips and failures remain visible."""
import importlib.util
from pathlib import Path
from types import SimpleNamespace

import pytest

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
    pack = (ROOT / "src/digitalmodel/riser_database/docs/agent-pack.md").read_text()
    assert "check_public_tests.py" in pack
    for module in HELPER.SKIP_REASONS:
        assert module in pack
    assert "81 passed, 0 failed, 12 skipped" in pack
    assert "NOT_RECORDED" in pack
