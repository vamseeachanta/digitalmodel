# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic acceptance inputs; no native CFD result is represented here."""

import dataclasses

import pytest

from digitalmodel.solvers.openfoam.near_wall_acceptance import (
    NearWallCriteria,
    WallMeasurement,
    assess_near_wall,
)


# Deliberate test inputs, not recommended engineering limits.
CRITERIA = NearWallCriteria(50, 0.2, 0.9, 0.05, "synthetic test basis")
MEASUREMENT = WallMeasurement(40, 60, 9, 10, "hull", "synthetic measurement")


def test_exact_limits_pass_and_report_each_case():
    report = assess_near_wall({"small": MEASUREMENT, "large": MEASUREMENT}, CRITERIA)
    assert report["passed"]
    assert report["coverage_spread"] == 0
    assert report["cases"]["small"]["coverage"] == pytest.approx(0.9)


@pytest.mark.parametrize("criteria,measurements", [(None, {}), (CRITERIA, {}), (CRITERIA, {"a": None})])
def test_missing_evidence_is_not_evaluated(criteria, measurements):
    report = assess_near_wall(measurements, criteria)
    assert not report["passed"]
    assert report["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("field,value", [
    ("y_plus_min", -1), ("y_plus_min", float("nan")),
    ("y_plus_max", float("inf")), ("y_plus_min", 61),
    ("covered_area_m2", 11), ("covered_area_m2", -1),
    ("total_area_m2", 0), ("patch", ""), ("source", ""),
])
def test_invalid_measurements_are_rejected(field, value):
    with pytest.raises(ValueError):
        assess_near_wall({"a": dataclasses.replace(MEASUREMENT, **{field: value})}, CRITERIA)


@pytest.mark.parametrize("field,value", [
    ("target_y_plus", 0), ("relative_tolerance", -1),
    ("minimum_coverage", 1.1), ("maximum_coverage_spread", float("nan")),
    ("source", ""),
])
def test_invalid_criteria_are_rejected(field, value):
    with pytest.raises(ValueError):
        assess_near_wall({"a": MEASUREMENT}, dataclasses.replace(CRITERIA, **{field: value}))


def test_scale_dependent_y_plus_fails_even_with_identical_coverage():
    large = dataclasses.replace(MEASUREMENT, y_plus_max=600)
    report = assess_near_wall({"small": MEASUREMENT, "large": large}, CRITERIA)
    assert report["status"] == "FAIL"
    assert not report["cases"]["large"]["y_plus_passed"]


def test_coverage_mismatch_fails_even_when_both_cases_meet_minimum():
    large = dataclasses.replace(MEASUREMENT, covered_area_m2=10)
    report = assess_near_wall({"small": MEASUREMENT, "large": large}, CRITERIA)
    assert not report["passed"]
    assert report["coverage_spread"] == pytest.approx(0.1)


def test_exact_decimal_coverage_spread_limit_passes():
    a = dataclasses.replace(MEASUREMENT, covered_area_m2=100, total_area_m2=100)
    b = dataclasses.replace(MEASUREMENT, covered_area_m2=95, total_area_m2=100)
    assert assess_near_wall({"a": a, "b": b}, CRITERIA)["passed"]


def test_different_patch_populations_cannot_be_compared():
    with pytest.raises(ValueError, match="patch"):
        assess_near_wall({"a": MEASUREMENT, "b": dataclasses.replace(MEASUREMENT, patch="other")}, CRITERIA)


def test_gci_consumer_rejects_missing_evidence_before_reading_results():
    from pathlib import Path

    from digitalmodel.solvers.openfoam.grid_convergence import gci_report

    with pytest.raises(ValueError, match="near-wall"):
        gci_report([Path("small"), Path("medium"), Path("large")])


def test_benchmark_aggregate_requires_wall_evidence(monkeypatch):
    from types import SimpleNamespace

    from digitalmodel.solvers.openfoam.validation import ship_resistance as module

    referent = module.load_referent()
    force = SimpleNamespace(total=1, pressure=1, viscous=1, samples=200,
                            first_iteration=1, last_iteration=200)
    monkeypatch.setattr(module, "parse_hull_force", lambda *args, **kwargs: force)
    monkeypatch.setattr(module, "coefficients_from_force", lambda *args: {
        "ct": referent.ct, "cp": referent.cr, "cv": referent.cf, "scatter": 0,
    })
    assert not module.evaluate_ship_resistance_run("unused")["all_passed"]
    passing = module.evaluate_ship_resistance_run(
        "unused", near_wall_criteria=CRITERIA,
        near_wall_measurements={"primary": MEASUREMENT},
    )
    assert passing["all_passed"]
    failing = module.evaluate_ship_resistance_run(
        "unused", near_wall_criteria=CRITERIA,
        near_wall_measurements={"primary": dataclasses.replace(MEASUREMENT, y_plus_max=600)},
    )
    assert not failing["all_passed"]
    missing_companion = module.evaluate_ship_resistance_run(
        "unused", companion_force_dat="also_unused", near_wall_criteria=CRITERIA,
        near_wall_measurements={"primary": MEASUREMENT},
    )
    assert not missing_companion["all_passed"]


def test_double_body_admissibility_requires_wall_evidence(monkeypatch):
    from digitalmodel.solvers.openfoam.validation import double_body_form_factor as module

    from .validation.test_double_body_form_factor import _config

    config = _config()
    cv = config.cf_ittc57 * 1.25  # Synthetic ratio, matching existing form-factor fixture.
    monkeypatch.setattr(module, "_reduce", lambda *args: {
        "cv_double_body": cv, "cv_pressure": cv * 0.2, "cv_friction": cv * 0.8,
        "series": [cv] * 200, "force_total_N": 1, "samples": 200,
        "first_iteration": 1, "last_iteration": 200,
    })
    report = module.evaluate_double_body_run("unused", config)
    assert not report["admissibility"]["admissible"]
    assert report["near_wall"]["status"] == "NOT_EVALUATED"
    report = module.evaluate_double_body_run(
        "unused", config, near_wall_criteria=CRITERIA,
        near_wall_measurements={"primary": MEASUREMENT},
    )
    assert report["admissibility"]["admissible"]
    report = module.evaluate_double_body_run(
        "unused", config, near_wall_criteria=CRITERIA,
        near_wall_measurements={"primary": dataclasses.replace(MEASUREMENT, covered_area_m2=0)},
    )
    assert not report["admissibility"]["admissible"]


def test_referent_free_admissibility_reports_missing_wall_evidence(monkeypatch):
    from digitalmodel.solvers.openfoam.validation import referent_free_resistance as module

    from .validation.test_referent_free_resistance import _config

    monkeypatch.setattr(module, "_reduce", lambda *args: {
        "ct": 0.0033, "cp": 0.0007, "cv": 0.0026, "ct_series": [0.0033] * 200,
        "force_total_N": 1, "samples": 200, "first_iteration": 1, "last_iteration": 200,
    })
    report = module.evaluate_referent_free_run("unused", _config())
    assert not report["admissibility"]["admissible"]
    assert report["near_wall"]["status"] == "NOT_EVALUATED"
    assert any("near-wall" in reason for reason in report["admissibility"]["reasons"])
    passing_wall = module.evaluate_referent_free_run(
        "unused", _config(), near_wall_criteria=CRITERIA,
        near_wall_measurements={"primary": MEASUREMENT},
    )
    assert passing_wall["near_wall"]["passed"]
    assert not any("near-wall" in reason for reason in passing_wall["admissibility"]["reasons"])
    failing_wall = module.evaluate_referent_free_run(
        "unused", _config(), near_wall_criteria=CRITERIA,
        near_wall_measurements={"primary": dataclasses.replace(MEASUREMENT, covered_area_m2=0)},
    )
    assert failing_wall["near_wall"]["status"] == "FAIL"
    assert not failing_wall["admissibility"]["admissible"]
