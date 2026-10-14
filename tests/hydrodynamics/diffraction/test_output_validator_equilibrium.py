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


def test_equilibrium_pass(mock_diffraction_results, evidence):
    validator = OutputValidator(mock_diffraction_results, equilibrium_inputs=evidence)
    assert validator.run_all_validations()["equilibrium_checks"] == {key: [] for key in evidence}


@pytest.mark.parametrize("rule,key,value", [
    ("cog_cob", "centre_of_gravity", [1, 0, 5]),
    ("mass_displacement", "mass_kg", 1100),
    ("force_balance", "output_forces_n", [99, 0, 0]),
])
def test_each_failed_rule_fails_report(mock_diffraction_results, evidence, rule, key, value):
    evidence[rule][key] = value
    report = OutputValidator(mock_diffraction_results, equilibrium_inputs=evidence).run_all_validations()
    assert report["overall_status"] == "FAIL"
    assert report["equilibrium_checks"][rule]


def test_missing_evidence_is_not_a_pass(mock_diffraction_results):
    report = OutputValidator(mock_diffraction_results).run_all_validations()
    assert all("Not evaluated" in issues[0] for issues in report["equilibrium_checks"].values())
    assert report["overall_status"] != "PASS"


def test_invalid_evidence_fails_report(mock_diffraction_results, evidence):
    evidence["mass_displacement"]["mass_kg"] = float("nan")
    report = OutputValidator(mock_diffraction_results, equilibrium_inputs=evidence).run_all_validations()
    assert report["overall_status"] == "FAIL"
