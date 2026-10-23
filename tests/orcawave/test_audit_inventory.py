# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Acceptance inventory must not promote absent or failing test evidence."""

import importlib.util
from pathlib import Path

import pytest


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


@pytest.mark.parametrize("outcome", ["FAILED", "SKIPPED", "XFAIL", "XPASS"])
def test_nonpassing_parameter_cannot_be_hidden_by_a_passing_parameter(
    inventory, outcome
):
    log = (
        "PASSED tests/orcawave/test_audit_p1.py::test_04_rayleigh_maxima[one]\n"
        f"{outcome} tests/orcawave/test_audit_p1.py::test_04_rayleigh_maxima[two]\n"
    )
    assert inventory.audit(log)[4]["status"] == "OPEN"


def test_arithmetic_cannot_qualify_a_licensed_clause(inventory):
    log = "\n".join(
        f"PASSED tests/orcawave/test_audit_p1.py::{name}" for name in inventory.NODES[3]
    )
    result = inventory.audit(log)
    assert result[3]["status"] == "OPEN"
    assert "NOT_EVALUATED" in result[3]["evidence"]
