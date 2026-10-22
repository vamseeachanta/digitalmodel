# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""The fixture report derives counts from receipts and detects stale output."""

import importlib.util
import json
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "fixture_report", ROOT / "scripts/reporting/build_diffraction_fixture_report.py",
)
REPORT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(REPORT)


def test_counts_come_from_individual_cases(tmp_path):
    """Hand count: one pass, failure, error, skip and expected failure each.

    Suite attributes deliberately disagree. Each literal testcase below defines
    one outcome, so no implementation-derived expected values are used.
    """
    receipt = tmp_path / "pytest.xml"
    receipt.write_text('''<testsuites><testsuite tests="999" skipped="999">
    <testcase classname="example" name="passes"/>
    <testcase classname="example" name="fails"><failure/></testcase>
    <testcase classname="example" name="errors"><error/></testcase>
    <testcase classname="example" name="skips"><skipped message="optional package absent"/></testcase>
    <testcase classname="example" name="expected"><skipped type="pytest.xfail"/></testcase>
    </testsuite></testsuites>''')
    result = REPORT.receipt(receipt)
    assert result["counts"] == {
        "passed": 1, "failed": 1, "errors": 1, "skipped": 1, "xfailed": 1,
    }
    assert result["not_evaluated"] == [{
        "test": "example::skips", "reason": "optional package absent",
    }]


def test_committed_report_rebuild_and_staleness(tmp_path):
    """Regeneration must reproduce the retained bytes and reject an altered count."""
    directory = REPORT.DESTINATION
    evidence = json.loads((directory / "evidence.json").read_text())
    REPORT.write_report(evidence, directory, check=True)
    REPORT.write_report(evidence, tmp_path)
    for name in ("index.html", "changes.json", "evidence.json"):
        assert (tmp_path / name).read_bytes() == (directory / name).read_bytes()
    evidence["after"]["counts"]["skipped"] += 1
    with pytest.raises(ValueError, match="Stale generated fixture report"):
        REPORT.write_report(evidence, tmp_path, check=True)


def test_generated_report_escapes_reasons():
    """A pytest reason cannot add HTML markup to the report."""
    evidence = json.loads((REPORT.DESTINATION / "evidence.json").read_text())
    evidence["after"]["not_evaluated"] = [{"test": "<test>", "reason": "<script>"}]
    output = REPORT.render(evidence)
    assert "&lt;script&gt;" in output
    assert "<script>" not in output


def test_location_redaction_is_generated():
    """Synthetic absolute locations are assembled without retaining a profile."""
    location = "/" + "home" + "/" + "example/data.yml"
    assert REPORT.sanitize("absent: " + location) == "absent: [path removed]"
