# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand checks for a synthetic supported-load digest.

Source: Engineering Statics, online edition consulted 2026, section 5.3,
equation 5.3.1 (sum of forces equals zero). All case values and tolerances
are synthetic test inputs, not material properties or design requirements.
"""

from dataclasses import replace
import importlib.util
from pathlib import Path

import pytest

from digitalmodel.ansys.screening_validation import (
    ScreeningCriteria,
    assess_supported_digest,
)


def criteria(**changes):
    """Explicit synthetic inputs; no default engineering acceptance limit."""
    base = ScreeningCriteria(
        allowable_mpa=100.0,
        yield_strength_mpa=200.0,
        expected_applied_fx_n=300.0,
        expected_applied_fy_n=400.0,
        force_tolerance_n=1.0,
        stress_tolerance_mpa=0.0,
        unity_tolerance=0.0,
        source="Synthetic hand example in this test; tolerances are test inputs",
    )
    return replace(base, **changes)


def digest(**changes):
    values = {
        "max_seqv_mpa": 60.0,
        "allowable_mpa": 100.0,
        "uc": 0.6,
        "applied_fx_n": 300.0,
        "applied_fy_n": 400.0,
        "reaction_fx_n": -300.0,
        "reaction_fy_n": -400.0,
        "force_residual_n": 0.0,
    }
    values.update(changes)
    return ",".join(f"{key},{value}" for key, value in values.items())


def test_balanced_case_passes_content_checks_without_native_qualification():
    """Hand: UC=60/100=0.6; F=(300-300,400-400)=(0,0) N.

    Peak 60 MPa is below both explicit limits 100 and 200 MPa. The recorded
    applied force equals the independently supplied (300,400) N case.
    """
    result = assess_supported_digest(digest(), criteria())
    assert result.content_status == "PASS"
    assert all(check.status == "PASS" for check in result.checks.values())
    assert result.native_qualification == "NOT_EVALUATED"
    assert result.criteria_source == criteria().source


@pytest.mark.parametrize(
    "changes,failed",
    [
        ({"reaction_fx_n": 300.0}, "force_balance"),
        ({"reaction_fx_n": -297.0, "reaction_fy_n": -396.0}, "force_balance"),
        ({"force_residual_n": -1.0}, "residual_record"),
        ({"force_residual_n": 5.0}, "residual_record"),
        (
            {
                "applied_fx_n": 0.0,
                "applied_fy_n": 0.0,
                "reaction_fx_n": 0.0,
                "reaction_fy_n": 0.0,
            },
            "applied_load",
        ),
        ({"applied_fx_n": 600.0, "reaction_fx_n": -600.0}, "applied_load"),
        ({"allowable_mpa": 1000.0, "uc": 0.06}, "stress"),
        ({"max_seqv_mpa": -60.0}, "stress"),
        ({"max_seqv_mpa": 100.0, "uc": 1.0}, "stress"),
        ({"uc": 0.5}, "unity"),
        ({"uc": -0.6}, "unity"),
    ],
)
def test_bad_content_never_passes(changes, failed):
    """Hand residuals: wrong sign gives 600 N; offsets (3,4) give 5 N.

    Both exceed the explicit 1 N test criterion. A wrong applied force still
    fails when balanced by an equally wrong reaction. Altered allowables and
    UC values are checked against the independent 100 MPa input and 60/100.
    """
    result = assess_supported_digest(digest(**changes), criteria())
    assert result.content_status == "FAIL"
    assert result.checks[failed].status == "FAIL"
    assert result.native_qualification == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "offset,expected", [(0.5, "PASS"), (1.0, "PASS"), (1.5, "FAIL")]
)
def test_force_tolerance_boundary(offset, expected):
    """Hand: residual=(offset,0), so its norm is offset; criterion is <=1 N."""
    result = assess_supported_digest(
        digest(reaction_fx_n=-300.0 + offset, force_residual_n=offset), criteria()
    )
    assert result.checks["force_balance"].status == expected


def test_missing_evidence_is_not_evaluated():
    """No force components means that signed equilibrium cannot be assessed."""
    result = assess_supported_digest(
        "max_seqv_mpa,60,allowable_mpa,100,uc,0.6", criteria()
    )
    assert result.checks["stress"].status == "PASS"
    assert result.checks["force_balance"].status == "NOT_EVALUATED"
    assert result.content_status == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "field,affected",
    [
        ("max_seqv_mpa", {"stress", "unity"}),
        ("allowable_mpa", {"stress"}),
        ("uc", {"unity"}),
        ("applied_fx_n", {"applied_load", "force_balance", "residual_record"}),
        ("applied_fy_n", {"applied_load", "force_balance", "residual_record"}),
        ("reaction_fx_n", {"force_balance", "residual_record"}),
        ("reaction_fy_n", {"force_balance", "residual_record"}),
        ("force_residual_n", {"residual_record"}),
    ],
)
def test_each_missing_field_marks_only_dependent_checks(field, affected):
    """A missing datum cannot acquire a zero or invalidate unrelated evidence."""
    tokens = digest().split(",")
    text = ",".join(
        f"{key},{value}"
        for key, value in zip(tokens[::2], tokens[1::2])
        if key != field
    )
    result = assess_supported_digest(text, criteria())
    assert result.content_status == "NOT_EVALUATED"
    for name, check in result.checks.items():
        assert check.status == ("NOT_EVALUATED" if name in affected else "PASS")


def test_bad_stress_with_missing_forces_still_fails():
    """A known failure takes precedence over missing evidence elsewhere."""
    result = assess_supported_digest(
        "max_seqv_mpa,120,allowable_mpa,100,uc,1.2", criteria()
    )
    assert result.content_status == "FAIL"


@pytest.mark.parametrize("raw", ["", "uc,nan", "uc,1,UC,2", "uc,,peak,1"])
def test_assessment_uses_strict_production_parser(raw):
    with pytest.raises(ValueError):
        assess_supported_digest(raw, criteria())


@pytest.mark.parametrize(
    "changes",
    [
        {"allowable_mpa": 0.0},
        {"yield_strength_mpa": -1.0},
        {"force_tolerance_n": -1.0},
        {"stress_tolerance_mpa": float("nan")},
        {"unity_tolerance": float("inf")},
        {"expected_applied_fx_n": True},
        {"expected_applied_fx_n": 0.0, "expected_applied_fy_n": 0.0},
        {"source": " "},
    ],
)
def test_invalid_criteria_are_rejected(changes):
    with pytest.raises(ValueError):
        criteria(**changes)


def test_worked_example_reproduces_the_hand_answer():
    """Example inputs use the same hand answer: UC=0.6, residual=0 N."""
    path = (
        Path(__file__).resolve().parents[2]
        / "examples/ansys/digest-assessment/build.py"
    )
    spec = importlib.util.spec_from_file_location("digest_example", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    result = module.worked_example()
    assert result.content_status == "PASS"
    assert result.native_qualification == "NOT_EVALUATED"
    assert set(result.checks) == {
        "stress",
        "unity",
        "applied_load",
        "force_balance",
        "residual_record",
    }
