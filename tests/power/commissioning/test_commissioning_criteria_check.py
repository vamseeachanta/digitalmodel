"""Regression checks for machine-checkable commissioning resistance criteria."""

from dataclasses import replace

import pytest

from digitalmodel.power.commissioning import (
    CommissioningPhase,
    CommissioningSequenceGenerator,
    TestResult as CommissioningResult,
    TestResultsValidator as ResultsValidator,
)


def _result(step_id, actual_value, status="pass"):
    return CommissioningResult(
        step_id=step_id,
        status=status,
        actual_value=actual_value,
        notes="",
        timestamp="2026-01-01T00:00:00",
    )


@pytest.fixture
def fat_steps():
    return CommissioningSequenceGenerator(
        {"subsystems": ["switchgear"]}
    ).generate_sequence(CommissioningPhase.FAT)


@pytest.fixture
def sat_steps():
    return CommissioningSequenceGenerator({}).generate_sequence(CommissioningPhase.SAT)


@pytest.mark.parametrize(
    ("phase", "step_id"),
    [(CommissioningPhase.FAT, "FAT-001"), (CommissioningPhase.SAT, "SAT-002")],
)
@pytest.mark.parametrize("reading", ["", "   ", "\t\n"])
def test_blank_not_run_reading_preserves_incomplete_count(phase, step_id, reading):
    steps = CommissioningSequenceGenerator({}).generate_sequence(phase)
    validator = ResultsValidator(steps)
    results = [
        _result(step.step_id, reading, "not_run")
        if step.step_id == step_id
        else _result(step.step_id, "OK")
        for step in steps
    ]

    assert validator.validate(results) == {
        "passed": 4,
        "failed": 0,
        "not_run": 1,
        "errors": [],
    }
    assert validator.generate_punch_list(results) == []


def test_low_fat_reading_overrides_six_submitted_passes(fat_steps):
    validator = ResultsValidator(fat_steps)
    results = [_result(step.step_id, "0.5 MOhm") for step in fat_steps]

    assert validator.validate(results) == {
        "passed": 5,
        "failed": 1,
        "not_run": 0,
        "errors": [],
    }
    punch = validator.generate_punch_list(results)
    assert len(punch) == 1
    assert punch[0].step_id == "FAT-001"
    assert punch[0].severity == "critical"
    assert "0.5 MOhm" in punch[0].description
    assert "R >= 100 MOhm" in punch[0].description


@pytest.mark.parametrize(
    ("reading", "passed", "failed"),
    [
        ("100 MOhm", 1, 0),
        ("250 MOhm", 1, 0),
        ("99.9 MOhm", 0, 1),
        ("100000000 Ohm", 1, 0),
        ("99999999 Ohm", 0, 1),
        ("99999999.9999999999999999999999999999 Ohm", 0, 1),
    ],
)
def test_fat_limit_is_inclusive_with_exact_unit_conversion(
    fat_steps, reading, passed, failed
):
    validator = ResultsValidator(fat_steps)
    results = [_result("FAT-001", reading)]

    assert validator.validate(results) == {
        "passed": passed,
        "failed": failed,
        "not_run": 5,
        "errors": [],
    }
    punch = validator.generate_punch_list(results)
    assert len(punch) == failed
    if failed:
        assert reading in punch[0].description
        assert "R >= 100 MOhm" in punch[0].description


@pytest.mark.parametrize(
    ("reading", "passed", "failed"),
    [
        ("1.5 Ohm", 0, 1),
        ("0.8 Ohm", 1, 0),
        ("1 Ohm", 1, 0),
        ("0.000001 MOhm", 1, 0),
        ("0.0000015 MOhm", 0, 1),
    ],
)
def test_sat_grounding_limit_uses_both_resistance_units(
    sat_steps, reading, passed, failed
):
    validator = ResultsValidator(sat_steps)
    results = [_result("SAT-002", reading)]

    assert validator.validate(results) == {
        "passed": passed,
        "failed": failed,
        "not_run": 4,
        "errors": [],
    }
    punch = validator.generate_punch_list(results)
    assert len(punch) == failed
    if failed:
        assert punch[0].step_id == "SAT-002"
        assert punch[0].severity == "critical"
        assert reading in punch[0].description
        assert "R <= 1 Ohm" in punch[0].description


@pytest.mark.parametrize(
    "reading", ["abc", "OK", "", "0.5 kOhm", "NaN MOhm", "inf MOhm", "100 MOhm extra"]
)
def test_unparseable_checkable_reading_reports_step_and_value(fat_steps, reading):
    summary = ResultsValidator(fat_steps).validate([_result("FAT-001", reading)])

    assert summary["passed"] == 0
    assert len(summary["errors"]) == 1
    assert "FAT-001" in summary["errors"][0]
    assert repr(reading) in summary["errors"][0]


@pytest.mark.parametrize("step_id", ["SAT-001", "SAT-003"])
@pytest.mark.parametrize("status", ["pass", "fail", "not_run", "other"])
def test_per_length_and_text_criteria_keep_status_behavior(sat_steps, step_id, status):
    validator = ResultsValidator(sat_steps)
    results = [_result(step_id, "abc", status)]

    assert validator.validate(results) == {
        "passed": int(status == "pass"),
        "failed": int(status == "fail"),
        "not_run": 4 + int(status == "not_run"),
        "errors": [],
    }
    assert len(validator.generate_punch_list(results)) == int(status == "fail")


@pytest.mark.parametrize(
    "criterion",
    ["R > 100 MOhm", "R >= 100 kOhm", "R >= 100 MOhm extra", "R >= 100 MOhm\n"],
)
def test_criterion_must_match_the_whole_supported_form(fat_steps, criterion):
    step = replace(fat_steps[0], acceptance_criteria=criterion)
    validator = ResultsValidator([step])
    results = [_result("FAT-001", "abc")]

    assert validator.validate(results) == {
        "passed": 1,
        "failed": 0,
        "not_run": 0,
        "errors": [],
    }
    assert validator.generate_punch_list(results) == []


@pytest.mark.parametrize("status", ["fail", "not_run", "other"])
def test_reading_that_misses_criterion_fails_regardless_of_status(fat_steps, status):
    validator = ResultsValidator(fat_steps)
    results = [_result("FAT-001", "0.5 MOhm", status)]

    assert validator.validate(results) == {
        "passed": 0,
        "failed": 1,
        "not_run": 5,
        "errors": [],
    }
    punch = validator.generate_punch_list(results)
    assert len(punch) == 1
    assert "0.5 MOhm" in punch[0].description
    assert "R >= 100 MOhm" in punch[0].description


def test_explicit_failure_stays_failed_when_reading_meets_criterion(fat_steps):
    validator = ResultsValidator(fat_steps)
    results = [_result("FAT-001", "500 MOhm", "fail")]

    assert validator.validate(results) == {
        "passed": 0,
        "failed": 1,
        "not_run": 5,
        "errors": [],
    }
    punch = validator.generate_punch_list(results)
    assert len(punch) == 1
    assert punch[0].step_id == "FAT-001"
    assert punch[0].severity == "critical"


def test_explicit_failure_stays_failed_when_reading_is_unparseable(fat_steps):
    validator = ResultsValidator(fat_steps)
    results = [_result("FAT-001", "abc", "fail")]

    summary = validator.validate(results)

    assert summary["passed"] == 0
    assert summary["failed"] == 1
    assert len(summary["errors"]) == 1
    assert len(validator.generate_punch_list(results)) == 1
