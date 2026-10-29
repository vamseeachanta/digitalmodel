# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Reference-gap regressions; synthetic inputs do not qualify an installation.

The worked example is an independent hand enumeration of the existing workflow
in slay_probe.evaluate_static_acceptance (the static-QA table in #2560): wire
10 m lies in [5, 15] m; reaction 1 kN is positive; zero bending and last-support
force satisfy S1; identical lift-off/departure readings have zero differences;
zero grounded tension is nonnegative. Hence six PASS, no FAIL, three pending.
Changing grounded tension to -1 kN gives five PASS, one FAIL, three pending.
These are synthetic arithmetic inputs, assumed, based on previous experience;
none of the workflow bands is asserted to come from a published standard.
"""

import json
from pathlib import Path

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.slay_probe import (
    PENDING_TABLE_B_ROWS,
    evaluate_static_acceptance,
    table_b_reference_basis,
)


def readings():
    """Supply the synthetic inputs enumerated in the module hand calculation."""
    return dict(
        wire_length=10, end_a_reaction=1, last_support_force=0,
        grounded_tension=[0], overbend_strain=0, lift_off_arc=0,
        departure_angle=0, reference=dict(lift_off_arc=0, departure_angle=0),
    )


def test_worked_example_reports_gaps_without_full_acceptance():
    result = evaluate_static_acceptance(**readings())
    assert result["counts"] == {"passed": 6, "failed": 0, "not_evaluated": 3}
    assert result["fully_accepted"] is False
    assert result["reference_basis"] == table_b_reference_basis()
    assert set(result["reference_basis"]["rows"]) == set(PENDING_TABLE_B_ROWS)
    for row in result["reference_basis"]["rows"].values():
        assert row["acceptance_source"] is None
        assert row["numeric_tolerance"] is None
        assert row["required_evidence"]
    json.dumps(result, allow_nan=False)


def test_published_cable_equations_are_explicitly_excluded_as_acceptance():
    basis = table_b_reference_basis()
    source = basis["reviewed_sources"]["elastic_catenary"]
    assert source["edition"] == "NREL/TP-500-41958, November 2007"
    assert source["locator"] == "Section 2.5, equations (2-38), (2-43a,b)"
    assert source["role"] == "background method only"
    assert "neglects bending stiffness" in source["limitation"]
    for row in basis["rows"].values():
        assert row["status"] == "NOT_EVALUATED"
        assert row["acceptance_source"] is None


@pytest.mark.parametrize("value", [None, 0, -1, float("nan"), float("inf")])
def test_readings_and_citation_shaped_inputs_cannot_enable_pending_rows(value):
    data = readings()
    data.update(touchdown_x=value, sag_bend_strain=value)
    # A caller-provided title or tolerance is not a reviewed published criterion.
    data["reference"].update(
        touchdown_x=value, sag_bend_strain=value, stiff_excess_m=value,
        acceptance_source={"title": "unreviewed citation", "edition": "unreviewed"},
        numeric_tolerance=0,
    )
    result = evaluate_static_acceptance(**data)
    assert result["counts"] == {"passed": 6, "failed": 0, "not_evaluated": 3}
    assert result["fully_accepted"] is False
    for name in PENDING_TABLE_B_ROWS:
        assert result["rows"][name]["status"] == "NOT_EVALUATED"
        assert result["reference_basis"]["rows"][name]["acceptance_source"] is None
    json.dumps(result, allow_nan=False)


def test_gaps_do_not_hide_a_failed_evaluated_reading():
    data = readings()
    data["grounded_tension"] = [-1]  # Independent sign check in module docstring.
    result = evaluate_static_acceptance(**data)
    assert result["status"] == "rejected"
    assert result["counts"] == {"passed": 5, "failed": 1, "not_evaluated": 3}
    assert result["reference_basis"] == table_b_reference_basis()


def test_mutating_one_report_cannot_qualify_a_later_report():
    first = evaluate_static_acceptance(**readings())
    first["reference_basis"]["rows"]["touchdown_x"]["status"] = "PASS"
    first["reference_basis"]["rows"]["touchdown_x"]["required_evidence"].clear()
    first["reference_basis"]["reviewed_sources"].clear()
    later = evaluate_static_acceptance(**readings())
    assert later["reference_basis"]["rows"]["touchdown_x"]["status"] == "NOT_EVALUATED"
    assert later["reference_basis"]["rows"]["touchdown_x"]["required_evidence"]
    assert later["reference_basis"]["reviewed_sources"]


def test_documented_python_example_matches_the_hand_counts():
    document = (
        Path(__file__).resolve().parents[3]
        / "docs/domains/pipeline-installation/slay-campaign/table-b-references.md"
    ).read_text(encoding="utf-8")
    example = document.split("```python\n", 1)[1].split("```", 1)[0]
    namespace = {}
    exec(compile(example, "table-b-references.md", "exec"), namespace)
    assert namespace["result"]["counts"] == {"passed": 6, "failed": 0, "not_evaluated": 3}
    assert namespace["rejected"]["counts"] == {"passed": 5, "failed": 1, "not_evaluated": 3}
