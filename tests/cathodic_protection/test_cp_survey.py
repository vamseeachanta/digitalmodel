"""Tests for CP survey data analysis."""

import math

import pytest

from digitalmodel.cathodic_protection.cp_survey import (
    CISSurveyPoint,
    analyze_cis_survey,
    attenuation_analysis,
    classify_dcvg_indication,
)


def test_cis_survey_all_protected():
    """All survey points meeting -0.850 V CSE criterion."""
    points = [
        CISSurveyPoint(distance_m=i * 10.0, on_potential_V=-0.90, off_potential_V=-0.88)
        for i in range(10)
    ]
    result = analyze_cis_survey(points)

    assert result.total_points == 10
    assert result.protected_points == 10
    assert result.underprotected_points == 0
    assert result.protection_percentage == 100.0
    assert len(result.deficiency_locations) == 0


def test_cis_survey_mixed_potentials():
    """Mix of protected and underprotected points."""
    points = [
        CISSurveyPoint(distance_m=0.0, on_potential_V=-0.95, off_potential_V=-0.90),
        CISSurveyPoint(distance_m=100.0, on_potential_V=-0.88, off_potential_V=-0.85),
        CISSurveyPoint(distance_m=200.0, on_potential_V=-0.80, off_potential_V=-0.75),
        CISSurveyPoint(distance_m=300.0, on_potential_V=-0.70, off_potential_V=-0.65),
    ]
    result = analyze_cis_survey(points)

    assert result.total_points == 4
    assert result.underprotected_points == 2  # -0.75 and -0.65 > -0.85
    assert result.protected_points == 2
    assert len(result.deficiency_locations) == 2
    assert 200.0 in result.deficiency_locations


def test_cis_survey_overprotection():
    """Detect overprotection below -1.200 V CSE."""
    points = [
        CISSurveyPoint(distance_m=0.0, on_potential_V=-1.30, off_potential_V=-1.25),
        CISSurveyPoint(distance_m=100.0, on_potential_V=-0.90, off_potential_V=-0.88),
    ]
    result = analyze_cis_survey(points)

    assert result.overprotected_points == 1
    assert result.protected_points == 1


def test_dcvg_severity_minor():
    """Low IR drop (<15%) classified as minor."""
    result = classify_dcvg_indication(
        voltage_gradient_mV=10.0,
        pipe_to_soil_shift_mV=100.0,
    )
    assert result.ir_drop_percentage == pytest.approx(10.0, abs=0.1)
    assert result.severity == "minor"


def test_dcvg_severity_severe():
    """IR drop 35-60% classified as severe."""
    result = classify_dcvg_indication(
        voltage_gradient_mV=45.0,
        pipe_to_soil_shift_mV=100.0,
    )
    assert result.ir_drop_percentage == pytest.approx(45.0, abs=0.1)
    assert result.severity == "severe"


def test_dcvg_severity_very_severe():
    """IR drop >60% classified as very severe."""
    result = classify_dcvg_indication(
        voltage_gradient_mV=75.0,
        pipe_to_soil_shift_mV=100.0,
    )
    assert result.severity == "very_severe"


def test_attenuation_analysis_12inch_pipe():
    """Attenuation analysis for a 12-inch coated pipeline."""
    result = attenuation_analysis(
        pipe_od_m=0.3048,
        pipe_wt_m=0.0127,
        pipe_resistivity_ohm_m=1.7e-7,
        coating_resistance_ohm_m2=10000.0,
    )
    assert result.attenuation_constant_per_km > 0
    assert result.characteristic_length_km > 0
    assert result.pipe_resistance_ohm_per_km > 0
    assert result.leakage_conductance_S_per_km > 0
    # Characteristic length should be order of 10-100 km for well-coated pipe
    assert result.characteristic_length_km > 1.0
