"""Integration of explicit equilibrium evidence into the validation report."""
import pytest

from digitalmodel.hydrodynamics.diffraction.output_validator import OutputValidator


@pytest.fixture
def evidence():
    return {
        "cog_cob": dict(centre_of_gravity=[0, 0, 5], centre_of_buoyancy=[0, 0, -5], tolerance_m=0),
        "mass_displacement": dict(mass_kg=1000, displacement_m3=1, water_density_kg_m3=1000, relative_tolerance=0),
        "force_balance": dict(input_forces_n=[100, 0, 0], output_forces_n=[100, 0, 0], relative_tolerance=0, absolute_tolerance_n=0),
    }


def test_equilibrium_pass(dense_diffraction_results, evidence):
    validator = OutputValidator(dense_diffraction_results, equilibrium_inputs=evidence)
    report = validator.run_all_validations()
    assert report["equilibrium_checks"] == {key: [] for key in evidence}
    assert report["overall_status"] == "PASS"
    assert report["equilibrium_status"] == "PASS"
    assert report["equilibrium_not_evaluated"] == {}


@pytest.mark.parametrize("rule,key,value", [
    ("cog_cob", "centre_of_gravity", [1, 0, 5]),
    ("mass_displacement", "mass_kg", 1100),
    ("force_balance", "output_forces_n", [99, 0, 0]),
])
def test_each_failed_rule_fails_report(mock_diffraction_results, evidence, rule, key, value):
    evidence[rule][key] = value
    report = OutputValidator(mock_diffraction_results, equilibrium_inputs=evidence).run_all_validations()
    assert report["overall_status"] == "FAIL"
    assert report["equilibrium_status"] == "FAIL"
    assert report["equilibrium_not_evaluated"] == {}
    assert report["equilibrium_checks"][rule]


@pytest.mark.parametrize("fixture_name, expected_status", [
    ("dense_diffraction_results", "PASS"),
    ("mock_diffraction_results", "WARNING"),
    ("fail_diffraction_results", "FAIL"),
])
def test_missing_evidence_preserves_overall_status(request, fixture_name, expected_status, evidence):
    from digitalmodel.hydrodynamics.diffraction.validation_runner import flatten_issues

    report = OutputValidator(request.getfixturevalue(fixture_name)).run_all_validations()
    assert report["overall_status"] == expected_status
    assert report["equilibrium_status"] == "NOT_EVALUATED"
    assert report["equilibrium_checks"] == {key: [] for key in evidence}
    assert report["equilibrium_not_evaluated"] == {
        key: "Not evaluated: explicit evidence and tolerance required"
        for key in evidence
    }
    assert not any("Not evaluated" in issue for issue in flatten_issues(report))


def test_invalid_evidence_fails_report(mock_diffraction_results, evidence):
    evidence["mass_displacement"]["mass_kg"] = float("nan")
    report = OutputValidator(mock_diffraction_results, equilibrium_inputs=evidence).run_all_validations()
    assert report["overall_status"] == "FAIL"
    assert report["equilibrium_status"] == "FAIL"


@pytest.mark.parametrize("failed", [False, True])
def test_partial_evidence_preserves_failures(dense_diffraction_results, evidence, failed):
    inputs = {"force_balance": evidence["force_balance"]}
    if failed:
        inputs["force_balance"]["output_forces_n"] = [99, 0, 0]
    report = OutputValidator(dense_diffraction_results, equilibrium_inputs=inputs).run_all_validations()
    assert report["overall_status"] == ("FAIL" if failed else "PASS")
    assert report["equilibrium_status"] == ("FAIL" if failed else "NOT_EVALUATED")
    assert set(report["equilibrium_not_evaluated"]) == {"cog_cob", "mass_displacement"}
    assert bool(report["equilibrium_checks"]["force_balance"]) is failed


def test_unknown_rule_is_a_failure(dense_diffraction_results):
    report = OutputValidator(
        dense_diffraction_results, equilibrium_inputs={"unknown": {}}
    ).run_all_validations()
    assert report["overall_status"] == "FAIL"
    assert report["equilibrium_status"] == "FAIL"
    assert report["equilibrium_checks"]["input_errors"]


def test_missing_evidence_survives_report_export(dense_diffraction_results, tmp_path):
    import json

    validator = OutputValidator(dense_diffraction_results)
    report = validator.run_all_validations()
    path = tmp_path / "validation.json"
    validator.export_report(path)
    saved = json.loads(path.read_text(encoding="utf-8"))
    assert saved == report
    assert saved["overall_status"] == "PASS"
    assert saved["equilibrium_status"] == "NOT_EVALUATED"
    assert len(saved["equilibrium_not_evaluated"]) == 3
