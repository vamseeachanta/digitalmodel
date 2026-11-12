"""Equilibrium evidence through the public validation helpers, without a solve.

Physical basis: OpenStax, University Physics Volume 1 (2016), sections
12.1 (force and moment equilibrium) and 14.4 (displaced-fluid mass).
All numerical inputs and tolerances here are synthetic arithmetic examples,
not engineering recommendations. Each expected verdict is calculated below;
the acceptance policies are those documented in equilibrium_checks.py.
"""

from copy import deepcopy
import json
from unittest.mock import patch

import pytest

from digitalmodel.hydrodynamics.diffraction.output_validator import validate_results
from digitalmodel.hydrodynamics.diffraction.validation_runner import run_validation


@pytest.fixture
def equilibrium_evidence():
    """Hand calculation of inclusive boundaries, using the sources above.

    CoG-CoB: sqrt(3**2 + 4**2) = 5 m; vertical offset is irrelevant.
    Mass: displaced mass = 1000 kg/m3 * 2 m3 = 2000 kg;
    abs(2200 - 2000) = 200 kg = 0.1 * 2000 kg.
    Forces: abs(111 - 100) = 11 N = 1 + 0.1 * abs(100);
    abs(-23 - (-20)) = 3 N = 1 + 0.1 * abs(-20);
    abs(1 - 0) = 1 N = 1 + 0.1 * abs(0).
    All residuals equal their synthetic limits, so all checks must pass.
    """
    return {
        "cog_cob": {
            "centre_of_gravity": [3, 4, 10],
            "centre_of_buoyancy": [0, 0, -10],
            "tolerance_m": 5,
        },
        "mass_displacement": {
            "mass_kg": 2200,
            "displacement_m3": 2,
            "water_density_kg_m3": 1000,
            "relative_tolerance": 0.1,
        },
        "force_balance": {
            "input_forces_n": [100, -20, 0],
            "output_forces_n": [111, -23, 1],
            "relative_tolerance": 0.1,
            "absolute_tolerance_n": 1,
        },
    }


def test_convenience_helper_exports_evidence(
    dense_diffraction_results, equilibrium_evidence, tmp_path
):
    """The boundary calculations in equilibrium_evidence give PASS."""
    path = tmp_path / "validation.json"
    report = validate_results(
        dense_diffraction_results, path, equilibrium_inputs=equilibrium_evidence
    )
    assert report["overall_status"] == "PASS"
    assert report["equilibrium_status"] == "PASS"
    assert report["equilibrium_not_evaluated"] == {}
    assert json.loads(path.read_text()) == report


def test_shared_helper_passes_evidence_without_mutating_it(
    dense_diffraction_results, equilibrium_evidence, tmp_path
):
    """The same boundary arithmetic must survive both helper layers."""
    original = deepcopy(equilibrium_evidence)
    outcome = run_validation(
        dense_diffraction_results, tmp_path, "boundary",
        equilibrium_inputs=equilibrium_evidence,
    )
    assert outcome.verdict == "PASS"
    assert outcome.issues == []
    assert outcome.report["equilibrium_status"] == "PASS"
    assert outcome.report["equilibrium_checks"] == {
        name: [] for name in equilibrium_evidence
    }
    assert json.loads(outcome.report_path.read_text()) == outcome.report
    assert equilibrium_evidence == original


@pytest.mark.parametrize("rule, key, value", [
    # Hand calculation: sqrt(6**2 + 8**2) = 10 m > 5 m.
    ("cog_cob", "centre_of_gravity", [6, 8, 10]),
    # Hand calculation: abs(2201 - 2000) = 201 kg > 200 kg.
    ("mass_displacement", "mass_kg", 2201),
    # Hand calculation: abs(112 - 100) = 12 N > 11 N.
    ("force_balance", "output_forces_n", [112, -23, 1]),
])
def test_failed_rule_reaches_run_verdict_and_saved_report(
    dense_diffraction_results, equilibrium_evidence, tmp_path, rule, key, value
):
    """Each independently calculated excess must cause FAIL, not WARNING."""
    equilibrium_evidence[rule][key] = value
    outcome = run_validation(
        dense_diffraction_results, tmp_path, "failed",
        equilibrium_inputs=equilibrium_evidence,
    )
    assert outcome.verdict == "FAIL"
    assert outcome.report["equilibrium_status"] == "FAIL"
    assert outcome.report["equilibrium_checks"][rule]
    assert any(issue.startswith(f"equilibrium_checks.{rule}:")
               for issue in outcome.issues)
    assert json.loads(outcome.report_path.read_text()) == outcome.report


@pytest.mark.parametrize("evidence", [None, {}])
def test_omitted_or_empty_evidence_remains_not_evaluated(
    dense_diffraction_results, evidence
):
    """Absent evidence cannot establish equilibrium despite other PASS checks."""
    outcome = run_validation(
        dense_diffraction_results, None, "absent", equilibrium_inputs=evidence
    )
    assert outcome.verdict == "PASS"
    assert outcome.report["equilibrium_status"] == "NOT_EVALUATED"
    assert set(outcome.report["equilibrium_not_evaluated"]) == {
        "cog_cob", "mass_displacement", "force_balance"
    }
    assert outcome.issues == []


def test_partial_evidence_evaluates_only_the_supplied_rule(
    dense_diffraction_results, equilibrium_evidence
):
    """The mass boundary passes; the other rules have no evidence."""
    outcome = run_validation(
        dense_diffraction_results, None, "partial",
        equilibrium_inputs={"mass_displacement": equilibrium_evidence["mass_displacement"]},
    )
    assert outcome.verdict == "PASS"
    assert outcome.report["equilibrium_status"] == "NOT_EVALUATED"
    assert outcome.report["equilibrium_checks"]["mass_displacement"] == []
    assert set(outcome.report["equilibrium_not_evaluated"]) == {
        "cog_cob", "force_balance"
    }


def test_missing_tolerance_is_a_failed_check(dense_diffraction_results, equilibrium_evidence):
    """No numerical acceptance limit may be silently supplied by a helper."""
    del equilibrium_evidence["mass_displacement"]["relative_tolerance"]
    outcome = run_validation(
        dense_diffraction_results, None, "invalid",
        equilibrium_inputs=equilibrium_evidence,
    )
    assert outcome.verdict == "FAIL"
    assert outcome.report["equilibrium_checks"]["mass_displacement"]


@pytest.mark.parametrize("enabled, has_results", [(False, True), (True, False)])
def test_skipped_validation_does_not_consume_evidence(
    dense_diffraction_results, equilibrium_evidence, tmp_path, enabled, has_results
):
    """Disabled validation and absent results retain their SKIPPED contract."""
    with patch(
        "digitalmodel.hydrodynamics.diffraction.validation_runner.validate_results"
    ) as validate:
        outcome = run_validation(
            dense_diffraction_results if has_results else None,
            tmp_path, "skipped", enabled=enabled,
            equilibrium_inputs=equilibrium_evidence,
        )
    validate.assert_not_called()
    assert outcome.verdict == "SKIPPED"
    assert outcome.report is None
    assert list(tmp_path.iterdir()) == []
