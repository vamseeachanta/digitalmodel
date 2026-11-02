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
    ("Ag/AgCl/saturated KCl", -0.731),
])
def test_hand_calculated_offsets(target, expected):
    # E_target = -0.850 + 0.316 - E_ref,target(SHE), all at 25 deg C.
    assert convert_potential(-0.850, "CSE", target) == pytest.approx(expected)
    assert str(electrode_reference(target).citation.url).startswith("https://")


@pytest.mark.parametrize("source,target", list(itertools.product(list(ReferenceElectrode), repeat=2)))
def test_all_round_trips(source, target):
    # Synthetic calibrations exercise both directions without a tabulated SCE value.
    options = {"zinc_potential_she_V": -0.800, "sce_potential_she_V": 0.200}
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
    assert result.protection_criterion_V == pytest.approx(-0.800)
    report = compliance_check_potential(-0.834, reference_electrode="Ag/AgCl/seawater")
    assert report.status == ComplianceStatus.COMPLIANT
    assert report.criterion_value == pytest.approx(-0.800)
    assert report.unit == "V vs Ag/AgCl/seawater"
    # Explicit native criteria retain their native scale, preserving existing API usage.
    native = compliance_check_potential(
        -0.79, criterion_V=-0.80, reference_electrode="Ag/AgCl/seawater",
    )
    assert native.criterion_value == -0.80


def test_empty_survey_still_reports_converted_criterion():
    result = analyze_cis_survey([], reference_electrode="SCE", sce_potential_she_V=0.200)
    assert result.total_points == 0
    assert result.protection_criterion_V == pytest.approx(-0.734)


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

    buried_tank = check_protection_potential(-0.80, reference_electrode="SCE", sce_potential_she_V=0.200)
    assert buried_tank["pass"]
    assert buried_tank["criterion"] == pytest.approx(-0.734)
    pipeline = check_potential_criteria(-0.80, reference_electrode="SCE", sce_potential_she_V=0.200)
    assert pipeline.is_protected
    assert pipeline.criterion_V == pytest.approx(-0.734)
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



@pytest.mark.parametrize("reading,protected", [(-0.800, 1), (-0.790, 0)])
def test_seawater_survey_uses_native_default(reading, protected):
    points = [CISSurveyPoint(distance_m=0, on_potential_V=reading)]
    result = analyze_cis_survey(points, reference_electrode="Ag/AgCl/seawater")
    assert result.protection_criterion_V == -0.800
    assert result.protected_points == protected
    assert result.underprotected_points == 1 - protected
    # A caller can still choose a different explicit criterion in this electrode.
    explicit = analyze_cis_survey(
        points, reference_electrode="Ag/AgCl/seawater", protection_criterion_V=-0.780,
    )
    assert explicit.protected_points == 1
    assert explicit.protection_criterion_V == -0.780


@pytest.mark.parametrize("source,target", [("CSE", "SCE"), ("SCE", "CSE"), ("SCE", "SCE")])
def test_sce_requires_its_own_calibration(source, target):
    assert electrode_reference("SCE").potential_she_V is None
    with pytest.raises(ValueError, match="SCE.*calibrat"):
        convert_potential(-0.850, source, target, zinc_potential_she_V=-0.800)


@pytest.mark.parametrize("calibration", [0.200, 0.300])
def test_sce_conversion_uses_supplied_calibration(calibration):
    converted = convert_potential(-0.850, "CSE", "SCE", sce_potential_she_V=calibration)
    assert converted == pytest.approx(-0.850 + 0.316 - calibration)
    assert convert_potential(converted, "SCE", "CSE", sce_potential_she_V=calibration) == pytest.approx(-0.850)


@pytest.mark.parametrize("calibration", [float("nan"), float("inf"), -float("inf")])
def test_sce_rejects_nonfinite_calibration(calibration):
    with pytest.raises(ValueError, match="SCE.*finite"):
        convert_potential(-0.850, "CSE", "SCE", sce_potential_she_V=calibration)


def test_sce_calibration_does_not_supply_zinc_offset():
    with pytest.raises(ValueError, match="zinc.*calibrat"):
        convert_potential(-0.850, "SCE", "zinc", sce_potential_she_V=0.200)


def test_offshore_check_accepts_sce_calibration():
    from digitalmodel.cathodic_protection.iso_15589_2 import check_protection_potential

    result = check_protection_potential(-0.850, reference_electrode="SCE", sce_potential_she_V=0.200)
    assert result["pass"]
    assert result["criterion_min"] == pytest.approx(-0.750)
    assert result["criterion_max"] == pytest.approx(-1.050)
