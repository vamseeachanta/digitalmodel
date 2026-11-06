# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Condition-specific offsets and preservation of physical criteria."""

import itertools

import pytest

from digitalmodel.cathodic_protection.reference_electrodes import (
    ReferenceElectrode, convert_potential, electrode_reference,
)
from digitalmodel.cathodic_protection.cp_survey import CISSurveyPoint, analyze_cis_survey
from digitalmodel.cathodic_protection.cp_reporting import (
    ComplianceStatus, compliance_check_potential,
)


@pytest.mark.parametrize("target,expected", [
    ("SHE", -0.534), ("CSE", -0.850), ("Ag/AgCl/seawater", -0.784),
    ("Ag/AgCl/saturated KCl", -0.731), ("SCE", -0.775),
])
def test_hand_calculated_offsets(target, expected):
    # E_target = -0.850 + 0.316 - E_ref,target(SHE), all at 25 deg C.
    assert convert_potential(-0.850, "CSE", target) == pytest.approx(expected)
    assert str(electrode_reference(target).citation.url).startswith("https://")


@pytest.mark.parametrize("source,target", list(itertools.product(list(ReferenceElectrode), repeat=2)))
def test_all_round_trips(source, target):
    # Zinc has no universal stable field offset; a calibration is supplied explicitly.
    options = {"zinc_potential_she_V": -0.800}
    converted = convert_potential(-0.9, source, target, **options)
    assert convert_potential(converted, target, source, **options) == pytest.approx(-0.9)


def test_zinc_requires_calibration_and_temperature_is_explicit():
    with pytest.raises(ValueError, match="calibrat"):
        convert_potential(-0.85, "CSE", "zinc")
    assert convert_potential(-0.85, "CSE", "zinc", zinc_potential_she_V=-0.8) == pytest.approx(0.266)
    with pytest.raises(ValueError, match="25"):
        convert_potential(-0.85, "CSE", "SCE", temperature_c=35)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_readings(value):
    with pytest.raises(ValueError):
        convert_potential(value, "CSE", "SHE")


def test_ambiguous_electrode_rejected():
    with pytest.raises(ValueError):
        convert_potential(-0.85, "CSE", "Ag/AgCl")


def test_survey_and_report_convert_criteria_not_just_labels():
    points = [CISSurveyPoint(distance_m=0, on_potential_V=-0.9, off_potential_V=-0.834)]
    result = analyze_cis_survey(points, reference_electrode="Ag/AgCl/seawater")
    assert result.protected_points == 1
    assert result.reference_electrode == "Ag/AgCl/seawater"
    assert result.protection_criterion_V == pytest.approx(-0.784)
    report = compliance_check_potential(-0.834, reference_electrode="Ag/AgCl/seawater")
    assert report.status == ComplianceStatus.COMPLIANT
    assert report.criterion_value == pytest.approx(-0.784)
    assert report.unit == "V vs Ag/AgCl/seawater"
    # Explicit native criteria retain their native scale, preserving existing API usage.
    native = compliance_check_potential(
        -0.79, criterion_V=-0.80, reference_electrode="Ag/AgCl/seawater",
    )
    assert native.criterion_value == -0.80


def test_empty_survey_still_reports_converted_criterion():
    result = analyze_cis_survey([], reference_electrode="SCE")
    assert result.total_points == 0
    assert result.protection_criterion_V == pytest.approx(-0.775)


def test_monitoring_uses_condition_specific_offsets():
    from digitalmodel.cathodic_protection.cp_monitoring import (
        POTENTIAL_CONVERSIONS, REFERENCE_ELECTRODE_SPECS,
        ReferenceElectrodeType, set_alarm_thresholds,
    )
    assert POTENTIAL_CONVERSIONS["cu_cuso4"]["ag_agcl"] == pytest.approx(66)
    assert REFERENCE_ELECTRODE_SPECS[ReferenceElectrodeType.AG_AGCL]["potential_vs_she_mV"] == 250
    assert REFERENCE_ELECTRODE_SPECS[ReferenceElectrodeType.ZN]["potential_vs_she_mV"] is None
    assert set_alarm_thresholds(ReferenceElectrodeType.SHE).underprotection_V == pytest.approx(-0.55)
    with pytest.raises(ValueError, match="calibrat"):
        set_alarm_thresholds(ReferenceElectrodeType.ZN)


def test_other_potential_checks_preserve_measurement_basis():
    from digitalmodel.cathodic_protection.api_rp_1632 import check_protection_potential
    from digitalmodel.cathodic_protection.pipeline_cp import check_potential_criteria

    buried_tank = check_protection_potential(-0.80, reference_electrode="SCE")
    assert buried_tank["pass"]
    assert buried_tank["criterion"] == pytest.approx(-0.775)
    pipeline = check_potential_criteria(-0.80, reference_electrode="SCE")
    assert pipeline.is_protected
    assert pipeline.criterion_V == pytest.approx(-0.775)
    assert pipeline.reference_electrode == "SCE"


def test_offshore_native_criterion_is_not_replaced_by_converted_onshore_default():
    from digitalmodel.cathodic_protection.iso_15589_2 import check_protection_potential

    # Native offshore -0.800 V seawater is -0.866 V CSE, not -0.850.
    result = check_protection_potential(-0.86, reference_electrode="CSE")
    assert not result["pass"]
    assert result["criterion_min"] == pytest.approx(-0.866)
    assert result["criterion_max"] == pytest.approx(-1.166)


def test_invalid_survey_and_report_values():
    with pytest.raises(ValueError):
        analyze_cis_survey([], protection_criterion_V=-1.3, overprotection_limit_V=-1.2)
    for value in (float("nan"), float("inf")):
        with pytest.raises(ValueError):
            compliance_check_potential(value)
        with pytest.raises(ValueError):
            analyze_cis_survey([CISSurveyPoint(distance_m=0, on_potential_V=value)])


@pytest.mark.parametrize("temperature_c", [5.0, 35.0, float("nan"), float("inf")])
@pytest.mark.parametrize("native_criteria", [False, True])
def test_survey_rejects_unsupported_conversion_temperature(temperature_c, native_criteria):
    criteria = (
        {"protection_criterion_V": -0.8, "overprotection_limit_V": -1.1}
        if native_criteria else {}
    )
    with pytest.raises(ValueError, match="25 deg C"):
        analyze_cis_survey(
            [], reference_electrode="SCE", temperature_c=temperature_c, **criteria,
        )


@pytest.mark.parametrize("temperature_c", [5.0, 35.0, float("nan"), float("inf")])
def test_report_rejects_unsupported_default_conversion_temperature(temperature_c):
    with pytest.raises(ValueError, match="25 deg C"):
        compliance_check_potential(-0.9, reference_electrode="SCE", temperature_c=temperature_c)


def test_workflows_accept_explicit_conversion_temperature():
    points = [CISSurveyPoint(distance_m=0, on_potential_V=-0.9, off_potential_V=-0.8)]
    survey = analyze_cis_survey(points, reference_electrode="SCE", temperature_c=25.0)
    report = compliance_check_potential(-0.8, reference_electrode="SCE", temperature_c=25.0)
    assert survey.protected_points == 1
    assert report.status == ComplianceStatus.COMPLIANT
    assert survey.protection_criterion_V == pytest.approx(report.criterion_value)


def test_report_explicit_native_criterion_does_not_use_tabulated_conversion():
    report = compliance_check_potential(
        -0.8, criterion_V=-0.79, reference_electrode="calibrated reference", temperature_c=35.0,
    )
    assert report.status == ComplianceStatus.COMPLIANT
    assert report.criterion_value == -0.79
    assert report.unit == "V vs calibrated reference"
