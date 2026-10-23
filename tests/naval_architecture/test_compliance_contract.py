# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Completeness and serialization tests; inputs are synthetic arithmetic cases."""

import json

import pytest

from digitalmodel.naval_architecture import compliance


def test_explicit_empty_selection_is_not_evaluated():
    report = compliance.run_compliance_checks({}, check_ids=[])
    assert report["status"] == "NOT_EVALUATED"
    assert report["checks"] == []
    assert report["overall_pass"] is False
    assert report["missing_inputs"] == ["check_ids"]


@pytest.mark.parametrize("missing", ["depth_m", "actual_z_cm3"])
def test_missing_geometry_has_structured_evidence(missing):
    data = {"draft_m": 7.0, "depth_m": 10.0, "lwl_m": 150.0,
            "beam_m": 20.0, "cb": 0.65, "actual_z_cm3": 5000.0}
    del data[missing]
    check_id = "draft_depth_ratio" if missing == "depth_m" else "section_modulus"
    report = compliance.run_compliance_checks(data, check_ids=[check_id])
    row = report["checks"][0]
    assert row["status"] == report["status"] == "NOT_EVALUATED"
    assert row["missing_inputs"] == report["missing_inputs"] == [missing]
    assert row["pass"] is False
    assert "clause_reference" in row


@pytest.mark.parametrize("draft,expected,margin", [(7.0, "PASS", 0.15),
                                                   (9.0, "FAIL", -0.05)])
def test_complete_selected_screen_with_hand_margin(draft, expected, margin):
    """Independent T/D arithmetic, using an explicitly supplied test bound.

    Synthetic maximum ratio 0.85 is a test input, not a regulatory value.
    T=7,D=10 gives ratio 0.7, margin 0.85-0.7=0.15; T=9 gives
    ratio 0.9, margin 0.85-0.9=-0.05. Comparisons use unrounded ratios.
    """
    row = compliance.check_draft_depth_ratio(draft, 10.0, max_ratio=0.85)
    assert row["status"] == expected
    assert 0.85 - row["value"] == pytest.approx(margin)
    report = compliance.run_compliance_checks(
        {"draft_m": draft, "depth_m": 10.0}, check_ids=["draft_depth_ratio"])
    assert report["status"] == expected
    assert report["missing_inputs"] == []


def test_serialized_contract_preserves_status_inputs_and_clause():
    report = compliance.run_compliance_checks({"angles_deg": [], "gz_m": []},
                                               check_ids=["imo_intact"])
    restored = json.loads(json.dumps(report, allow_nan=False))
    row = restored["checks"][0]
    assert row["status"] == "NOT_EVALUATED"
    assert row["missing_inputs"] == ["angles_deg", "gz_m", "gm_m"]
    assert row["clause_reference"] == "MSC.267(85), 2008, Part A 2.2"
    assert row["pass"] is False


def test_unqualified_clause_is_explicit_not_invented():
    row = compliance.check_draft_depth_ratio(7.0, 10.0)
    assert row["clause_reference"] is None
    assert row["reference_status"] == "UNVERIFIED"


@pytest.mark.parametrize("data,status", [
    ({"draft_m": 7.0, "depth_m": 10.0}, "PASS"),
    ({"draft_m": 9.0, "depth_m": 10.0}, "FAIL"),
    ({"draft_m": 7.0}, "NOT_EVALUATED"),
])
def test_serialized_status_never_disagrees_with_pass(data, status):
    """T/D cases and margins are derived in test_complete_selected_screen_with_hand_margin."""
    report = compliance.run_compliance_checks(data, check_ids=["draft_depth_ratio"])
    restored = json.loads(json.dumps(report, allow_nan=False))
    row = restored["checks"][0]
    assert row["status"] == restored["status"] == status
    assert row["pass"] is (status == "PASS")
    assert restored["overall_pass"] is (status == "PASS")
    assert row["check_id"] == "draft_depth_ratio"


def test_every_default_row_has_structured_contract():
    report = json.loads(json.dumps(compliance.run_compliance_checks({}), allow_nan=False))
    for row in report["checks"]:
        assert row["status"] == "NOT_EVALUATED"
        assert row["pass"] is False
        assert row["missing_inputs"]
        assert "clause_reference" in row
        assert row["reference_status"] in ("RECORDED", "UNVERIFIED")


def test_typed_report_and_legacy_serialization_agree():
    data = {"draft_m": 7.0}
    typed = compliance.evaluate_compliance_checks(data, check_ids=["draft_depth_ratio"])
    assert isinstance(typed, compliance.ComplianceResult)
    assert typed.status == "NOT_EVALUATED"
    assert typed.checks[0].missing_inputs == ("depth_m",)
    assert typed.to_dict() == compliance.run_compliance_checks(data, check_ids=["draft_depth_ratio"])


def test_empty_typed_report_is_not_evaluated():
    result = compliance.ComplianceResult(())
    assert result.status == "NOT_EVALUATED"
    assert result.to_dict()["overall_pass"] is False


@pytest.mark.parametrize("status", ["UNKNOWN", "pass", None])
def test_typed_result_rejects_invalid_status(status):
    with pytest.raises(ValueError, match="status"):
        compliance.CheckResult("test", "test", status)


def test_typed_result_cannot_pass_with_missing_inputs():
    with pytest.raises(ValueError, match="missing inputs"):
        compliance.CheckResult("test", "test", "PASS", missing_inputs=("depth_m",))


def test_unknown_or_duplicate_check_selection_is_refused():
    with pytest.raises(ValueError, match="Unknown"):
        compliance.run_compliance_checks({}, check_ids=["unknown"])
    with pytest.raises(ValueError, match="Duplicate"):
        compliance.run_compliance_checks({}, check_ids=["imo_intact", "imo_intact"])


@pytest.mark.parametrize("ratio,damping,expected", [
    (0.5, 0.0, 4 / 3), (0.5, 0.05, (200 / 113) ** 0.5), (1.0, 0.05, 10.0),
])
def test_requested_oscillator_reference_points(ratio, damping, expected):
    """Mechanics Map (2021), section 15.5, magnification-factor equation.

    https://eng.libretexts.org/@go/page/55503
    At r=0.5,zeta=0: H=1/(1-1/4)=4/3.
    At r=0.5,zeta=0.05: denominator squared=(3/4)^2+(1/20)^2
    =9/16+1/400=113/200, so H=sqrt(200/113).
    At r=1,zeta=0.05: H=1/(2*0.05)=10.
    Relative tolerance 1e-14 is a numerical test criterion.
    """
    from digitalmodel.naval_architecture.seakeeping import simple_heave_rao

    assert simple_heave_rao(ratio, 1.0, damping) == pytest.approx(expected, rel=1e-14)
