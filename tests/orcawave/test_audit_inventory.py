# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Acceptance inventory must not promote absent or failing test evidence."""

import importlib.util
import os
from pathlib import Path
import subprocess
import sys

import pytest


CASES = {
    1: (
        "tests/orcawave/test_audit_p1.py::test_01_volume_waterline[True-False]",
        "tests/orcawave/test_audit_p1.py::test_01_volume_waterline[False-False]",
        "tests/orcawave/test_audit_p1.py::test_01_volume_waterline[False-True]",
        "tests/orcawave/test_audit_p1.py::test_01_inward_flagged",
    ),
    4: ("tests/orcawave/test_audit_p1.py::test_04_rayleigh_maxima",),
    18: (
        "tests/orcawave/test_audit_p2.py::test_18_heuristic_explicit[8]",
        "tests/orcawave/test_audit_p2.py::test_18_heuristic_explicit[16]",
    ),
}


def passing_log(nodes):
    return "\n".join(f"PASSED {node}" for node in nodes)


@pytest.fixture
def inventory():
    path = (
        Path(__file__).resolve().parents[2]
        / "scripts/validation/audit_orcawave_sections.py"
    )
    spec = importlib.util.spec_from_file_location("audit_orcawave_sections", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_empty_log_cannot_establish_a_fixed_section(inventory):
    assert all(row["status"] == "OPEN" for row in inventory.audit("").values())


@pytest.mark.parametrize("outcome", ["FAILED", "SKIPPED", "XFAIL", "XPASS", "ERROR"])
def test_nonpassing_parameter_cannot_be_hidden_by_a_passing_parameter(
    inventory, outcome
):
    log = f"PASSED {CASES[18][0]}\n" f"{outcome} {CASES[18][1]}\n"
    assert inventory.audit(log)[18]["status"] == "OPEN"


@pytest.mark.parametrize("section", [1, 4, 18])
def test_complete_qualified_cases_establish_a_fixed_section(inventory, section):
    assert inventory.audit(passing_log(CASES[section]))[section]["status"] == "FIXED"


@pytest.mark.parametrize("section,missing", [(1, 2), (18, 1)])
def test_missing_acceptance_parameter_keeps_section_open(inventory, section, missing):
    remaining = CASES[section][:missing] + CASES[section][missing + 1 :]
    assert inventory.audit(passing_log(remaining))[section]["status"] == "OPEN"


@pytest.mark.parametrize("section", [1, 4, 18])
def test_same_name_in_another_file_cannot_establish_evidence(inventory, section):
    nodes = [f"tests/unrelated.py::{node.split('::', 1)[1]}" for node in CASES[section]]
    assert inventory.audit(passing_log(nodes))[section]["status"] == "OPEN"


def test_unknown_parameter_cannot_replace_a_required_case(inventory):
    nodes = (CASES[18][0], CASES[18][1].replace("[16]", "[999]"))
    assert inventory.audit(passing_log(nodes))[18]["status"] == "OPEN"


@pytest.mark.parametrize("outcome", ["FAILED", "SKIPPED", "XFAIL", "XPASS", "ERROR"])
@pytest.mark.parametrize("reason", ["", " - reported reason"])
def test_conflicting_outcome_keeps_complete_evidence_open(inventory, outcome, reason):
    log = passing_log(CASES[18]) + f"\n{outcome} {CASES[18][1]}{reason}"
    assert inventory.audit(log)[18]["status"] == "OPEN"


def test_expected_case_inventory_matches_actual_collection(inventory):
    root = Path(__file__).resolve().parents[2]
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    collected = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "--collect-only",
            "-qq",
            "-p",
            "no:cacheprovider",
            "tests/orcawave/test_audit_p1.py",
            "tests/orcawave/test_audit_p2.py",
            "tests/orcawave/test_pack_gates.py",
        ],
        cwd=root,
        env=env,
        capture_output=True,
        encoding="utf-8",
        check=True,
    )
    actual = {
        line for line in collected.stdout.splitlines() if line.startswith("tests/")
    }
    expected = {node for nodes in inventory.NODES.values() for node in nodes}
    assert expected <= actual, "Expected acceptance cases differ from pytest collection"
    required_tests = {node.split("[", 1)[0] for node in expected}
    assert expected == {
        node for node in actual if node.split("[", 1)[0] in required_tests
    }


def test_every_required_case_is_needed_for_fixed_status(inventory):
    for section, nodes in inventory.NODES.items():
        if section in (3, 13, 17):
            continue  # Qualification limits keep these sections OPEN independently.
        assert inventory.audit(passing_log(nodes))[section]["status"] == "FIXED"
        for missing in nodes:
            log = passing_log(node for node in nodes if node != missing)
            assert inventory.audit(log)[section]["status"] == "OPEN", missing


def test_arithmetic_cannot_qualify_a_licensed_clause(inventory):
    log = passing_log(inventory.NODES[3])
    result = inventory.audit(log)
    assert result[3]["status"] == "OPEN"
    assert "NOT_EVALUATED" in result[3]["evidence"]
