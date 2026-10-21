"""Missing-data contract from issue #2597; all dimensions are synthetic.

Status assertions test data completeness, not regulatory acceptance limits.
No invented hydrostatics or section modulus may turn absence into success.
"""

import numpy as np
import pytest

from digitalmodel.naval_architecture.compliance import (
    check_beam_draft_ratio,
    check_displacement_lwl_ratio,
    check_draft_depth_ratio,
    check_imo_intact,
    check_length_beam_ratio,
    check_max_trim,
    check_min_freeboard,
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
         "IMO Intact Stability (Part A 2.2, 6 criteria)", "gm_m"),
        ({"angles_deg": [], "gz_m": [], "gm_m": 1.0},
         "IMO Intact Stability (Part A 2.2, 6 criteria)", "angles_deg"),
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


@pytest.mark.parametrize("cb", [-0.7, -0.1, 0.0, 1.01, None, float("nan")])
def test_invalid_block_coefficient_is_not_evaluated(cb):
    """A malformed coefficient cannot lower the required strength to zero."""
    data = complete_inputs()
    data.update(cb=cb, actual_z_cm3=5000.0)
    direct = check_section_modulus(150.0, 20.0, 7.0, cb, 5000.0)
    report = run_compliance_checks(data)
    row = next(c for c in report["checks"] if c["code"] == "ABS-SVR")
    for result in (direct, row):
        assert result["status"] == "NOT_EVALUATED"
        assert result["pass"] is False
        assert "cb" in result["reason"]
    assert report["overall_pass"] is False


@pytest.mark.parametrize("cb", [0.01, 1.0])
def test_valid_block_coefficient_boundaries_remain_evaluable(cb):
    result = check_section_modulus(150.0, 20.0, 7.0, cb, 5000.0)
    assert result["status"] in ("PASS", "FAIL")


@pytest.mark.parametrize("angles,gz", [
    ([0, 10, 20, 30, 35], [0, 0.3, 0.65, 1.0, 1.15]),
    ([10, 20, 30, 40, 50], [0.3, 0.65, 1.0, 1.3, 1.35]),
    ([-10, 0, 30, 40], [0, 0, 1.0, 1.3]),
])
def test_stability_curve_requires_full_integration_interval(angles, gz):
    data = complete_inputs()
    data.update(angles_deg=angles, gz_m=gz)
    direct = check_imo_intact(angles, gz, data["gm_m"])
    report = run_compliance_checks(data)
    for row in (direct, report["checks"][0]):
        assert row["status"] == "NOT_EVALUATED"
        assert row["pass"] is False
        assert "0" in row["reason"] and "40" in row["reason"]
        assert "details" not in row
    assert report["overall_pass"] is False


@pytest.mark.parametrize("angles", [[0, 10, 30, 40], [0, 10, 30, 45]])
def test_complete_curve_can_end_at_or_beyond_integration_limit(angles):
    result = check_imo_intact(angles, [0, 0.3, 1.0, 1.3], 1.2)
    assert result["status"] == "PASS"
    assert result["pass"] is True
    assert len(result["details"]) == 6


@pytest.mark.parametrize("scalar_type", [np.float32, np.float64, np.int64])
def test_numpy_real_scalars_remain_evaluable(scalar_type):
    """An exactly represented geometry ratio must not become missing data."""
    report = run_compliance_checks({
        "draft_m": scalar_type(7), "depth_m": scalar_type(10),
    })
    result = next(c for c in report["checks"] if c["name"] == "Draft/depth ratio")
    assert result["status"] == "PASS"
    assert result["pass"] is True
    assert result["value"] == pytest.approx(0.7)


def direct_check_cases():
    """Exercise public check functions with independent supplied arguments."""
    data = complete_inputs()
    return [
        (check_imo_intact, {k: data[k] for k in ("angles_deg", "gz_m", "gm_m")}),
        (check_min_freeboard, {"freeboard_m": 5.0, "lwl_m": 150.0}),
        (check_beam_draft_ratio, {"beam_m": 20.0, "draft_m": 7.0,
                                 "min_ratio": 1.8, "max_ratio": 5.0}),
        (check_length_beam_ratio, {"lwl_m": 150.0, "beam_m": 20.0,
                                  "min_ratio": 4.0, "max_ratio": 11.0}),
        (check_draft_depth_ratio, {"draft_m": 7.0, "depth_m": 10.0,
                                  "max_ratio": 0.85}),
        (check_section_modulus, {k: data[k] for k in (
            "lwl_m", "beam_m", "draft_m", "cb", "actual_z_cm3")}),
        (check_displacement_lwl_ratio, {"displacement_tonnes": 1000.0,
                                       "lwl_m": 150.0}),
        (check_propeller_immersion, {"draft_m": 7.0, "prop_diameter_m": 4.0}),
        (check_max_trim, {"trim_m": 0.0, "lwl_m": 150.0,
                          "max_trim_fraction": 0.015}),
    ]


@pytest.mark.parametrize("function,arguments", direct_check_cases())
def test_direct_checks_include_evaluated_pass_status(function, arguments):
    result = function(**arguments)
    assert result["status"] == "PASS"
    assert result["pass"] is True


@pytest.mark.parametrize("function,arguments,key", [
    (function, arguments, key)
    for function, arguments in direct_check_cases() for key in arguments
])
@pytest.mark.parametrize("invalid", [None, float("nan"), float("inf"), True, "bad"])
def test_direct_checks_validate_each_argument(function, arguments, key, invalid):
    arguments = dict(arguments, **{key: invalid})
    result = function(**arguments)
    assert result["status"] == "NOT_EVALUATED"
    assert result["pass"] is False
    assert key in result["reason"]


@pytest.mark.parametrize("function,arguments,key", [
    (function, arguments, key)
    for function, arguments in direct_check_cases() for key in arguments
    if key in {"draft_m", "depth_m", "lwl_m", "beam_m", "prop_diameter_m",
               "displacement_tonnes", "max_ratio", "min_ratio", "max_trim_fraction"}
])
@pytest.mark.parametrize("invalid", [0.0, -7.0])
def test_direct_checks_reject_nonpositive_dimensions_and_bounds(
    function, arguments, key, invalid,
):
    result = function(**dict(arguments, **{key: invalid}))
    assert result["status"] == "NOT_EVALUATED"
    assert result["pass"] is False
    assert key in result["reason"]


@pytest.mark.parametrize("function,arguments", [
    (check_imo_intact, dict(direct_check_cases()[0][1], gm_m=0.0)),
    (check_min_freeboard, {"freeboard_m": 0.0, "lwl_m": 150.0}),
    (check_beam_draft_ratio, {"beam_m": 1.0, "draft_m": 7.0}),
    (check_length_beam_ratio, {"lwl_m": 10.0, "beam_m": 20.0}),
    (check_draft_depth_ratio, {"draft_m": 9.0, "depth_m": 10.0}),
    (check_section_modulus, {"lwl_m": 150.0, "beam_m": 20.0,
                             "draft_m": 7.0, "cb": 0.65, "actual_z_cm3": 0.0}),
    (check_displacement_lwl_ratio, {"displacement_tonnes": 1.0, "lwl_m": 150.0}),
    (check_propeller_immersion, {"draft_m": 7.0, "prop_diameter_m": 10.0}),
    (check_max_trim, {"trim_m": -3.0, "lwl_m": 150.0}),
])
def test_direct_checks_include_evaluated_fail_status(function, arguments):
    result = function(**arguments)
    assert result["status"] == "FAIL"
    assert result["pass"] is False


@pytest.mark.parametrize("function,arguments", direct_check_cases()[2:4])
def test_inverted_ratio_bounds_are_not_evaluated(function, arguments):
    result = function(**dict(arguments, min_ratio=12.0, max_ratio=1.0))
    assert result["status"] == "NOT_EVALUATED"
    assert "min_ratio" in result["reason"] and "max_ratio" in result["reason"]


@pytest.mark.parametrize("angles,gz", [
    ([], []), ([0], [0]), ([0, 30, 40], [0, 1]),
    ([0, 30, 30, 40], [0, 1, 1, 1]), ([0, 40, 30], [0, 1, 1]),
    ([0, float("nan"), 40], [0, 1, 1]), ([0, 30, 40], [0, True, 1]),
])
def test_direct_stability_check_rejects_malformed_curve(angles, gz):
    result = check_imo_intact(angles, gz, 1.2)
    assert result["status"] == "NOT_EVALUATED"
    assert result["pass"] is False


def test_stability_row_and_aggregate_identify_limited_scope():
    report = run_compliance_checks(complete_inputs())
    assert "Part A 2.2" in report["checks"][0]["name"]
    assert "Part A 2.2 only" in report["name"]
    assert report["status"] == "PASS"
