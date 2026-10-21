"""Missing-data contract from issue #2597; all dimensions are synthetic.

Status assertions test data completeness, not regulatory acceptance limits.
No invented hydrostatics or section modulus may turn absence into success.
"""

import pytest

from digitalmodel.naval_architecture.compliance import (
    check_draft_depth_ratio,
    check_propeller_immersion,
    check_section_modulus,
    run_compliance_checks,
)


def test_empty_report_is_not_evaluated():
    report = run_compliance_checks({})
    assert report["status"] == "NOT_EVALUATED"
    assert report["overall_pass"] is False
    assert report["passed"] == report["failed"] == 0
    assert report["not_evaluated"] == report["total"]
    assert all(c["status"] == "NOT_EVALUATED" for c in report["checks"])


@pytest.mark.parametrize("depth", [None, float("nan"), float("inf"), 0, -1])
def test_missing_or_invalid_depth_never_passes(depth):
    result = check_draft_depth_ratio(7.0, depth)
    assert result["status"] == "NOT_EVALUATED"
    assert result["pass"] is False
    assert result["reason"]


def test_missing_section_modulus_never_passes():
    result = check_section_modulus(150.0, 20.0, 7.0, 0.65, None)
    assert result["status"] == "NOT_EVALUATED"
    assert result["pass"] is False
    assert "actual_z_cm3" in result["reason"]


def test_missing_propeller_diameter_is_not_invented():
    result = check_propeller_immersion(7.0)
    assert result["status"] == "NOT_EVALUATED"
    assert result["pass"] is False


@pytest.mark.parametrize(
    ("data", "name", "missing"),
    [
        ({"draft_m": 7.0}, "Draft/depth ratio", "depth_m"),
        ({"lwl_m": 150.0, "beam_m": 20.0, "draft_m": 7.0, "cb": 0.65},
         "Hull girder section modulus", "actual_z_cm3"),
        ({"angles_deg": [0, 30, 40], "gz_m": [0, 1, 1]},
         "IMO Intact Stability (6 criteria)", "gm_m"),
        ({"angles_deg": [], "gz_m": [], "gm_m": 1.0},
         "IMO Intact Stability (6 criteria)", "angles_deg"),
        ({"lwl_m": None, "freeboard_m": 5.0}, "Minimum freeboard (Type B)", "lwl_m"),
    ],
)
def test_partial_inputs_explain_unevaluated_checks(data, name, missing):
    report = run_compliance_checks(data)
    result = next(c for c in report["checks"] if c["name"] == name)
    assert result["status"] == "NOT_EVALUATED"
    assert result["pass"] is False
    assert missing in result["reason"]
    assert report["overall_pass"] is False


def test_failure_takes_precedence_over_missing_data():
    """Legacy freeboard screen: 0 m is less than any positive requirement.

    This is an ordering check, not qualification of the legacy coefficient.
    """
    report = run_compliance_checks({"lwl_m": 100.0, "freeboard_m": 0.0})
    assert report["status"] == "FAIL"
    assert report["failed"] == 1
    assert report["not_evaluated"] > 0
    assert report["overall_pass"] is False


def test_evaluated_checks_and_counts_are_preserved():
    """Hand calculation: synthetic T/D=7/10=0.7 (dimensionless).

    The ratio is arithmetic; the legacy acceptance bound is not qualified here.
    Complete counts are a partition: total=passed+failed+not_evaluated.
    """
    report = run_compliance_checks({"draft_m": 7.0, "depth_m": 10.0})
    result = next(c for c in report["checks"] if c["name"] == "Draft/depth ratio")
    assert result["value"] == pytest.approx(0.7)
    assert result["status"] == "PASS"
    assert report["passed"] == 1
    assert report["status"] == "NOT_EVALUATED"
    assert report["total"] == sum(report[k] for k in ("passed", "failed", "not_evaluated"))


def complete_inputs():
    """Synthetic regression data; no measured or regulated values are inferred."""
    return {
        "angles_deg": [0, 10, 20, 30, 40, 50, 60, 70],
        "gz_m": [0.0, 0.30, 0.65, 1.00, 1.30, 1.35, 1.10, 0.70],
        "gm_m": 1.20, "freeboard_m": 5.0, "lwl_m": 150.0,
        "beam_m": 20.0, "draft_m": 7.0, "depth_m": 10.0,
        "displacement_tonnes": 1000.0, "cb": 0.65,
        "actual_z_cm3": 500000.0, "prop_diameter_m": 4.0, "trim_m": 0.0,
    }


def test_complete_legacy_screen_remains_evaluable():
    """Independent arithmetic for unchanged legacy compliance.py expressions.

    T/D=7/10=0.7; B/T=20/7; L/B=150/20=7.5;
    D/L=1000/(0.01*150)^3=8000/27; freeboard requirement=0.025*150=3.75;
    Z requirement=0.01*150^2*20*(0.65+0.7)=6075;
    tip immersion=7-4=3 and requirement=0.1*4=0.4;
    trim=0 compared with 0.015*150=2.25.
    Trapezoidal GZ areas to 30 and 40 degrees are 14.5*pi/180 and
    26*pi/180 m rad; their difference is 11.5*pi/180. GZ at 30 is 1 m,
    maximum is at 50 degrees and GM is 1.2 m. The IMO criteria are delegated
    unchanged to damage_stability.py (MSC.267(85), 2008, Part A 2.2).
    This preserves legacy evaluation, not qualification of approximate limits.
    """
    report = run_compliance_checks(complete_inputs())
    assert report["status"] == "PASS"
    assert report["overall_pass"] is True
    assert report["passed"] == report["total"]
    assert report["failed"] == report["not_evaluated"] == 0


@pytest.mark.parametrize("key", tuple(complete_inputs()))
@pytest.mark.parametrize("replacement", [None, float("nan"), float("inf")])
def test_incomplete_full_input_cannot_pass(key, replacement):
    data = complete_inputs()
    data[key] = replacement
    report = run_compliance_checks(data)
    assert report["status"] == "NOT_EVALUATED"
    assert report["overall_pass"] is False
    assert report["not_evaluated"] > 0


@pytest.mark.parametrize("angles,gz", [([0, 30], [0]), ([0, 30, 30], [0, 1, 1]),
                                      ([0, 40, 30], [0, 1, 1]), ([0], [0])])
def test_malformed_stability_curve_is_not_evaluated(angles, gz):
    data = complete_inputs()
    data.update(angles_deg=angles, gz_m=gz)
    result = run_compliance_checks(data)["checks"][0]
    assert result["status"] == "NOT_EVALUATED"
    assert result["pass"] is False
