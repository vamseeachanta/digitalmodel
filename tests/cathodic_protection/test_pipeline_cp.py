"""Tests for pipeline cathodic protection design."""

import math

import pytest

from digitalmodel.cathodic_protection.pipeline_cp import (
    PipelineEnvironment,
    PipelineCPInput,
    anode_spacing,
    holiday_detection_voltage,
    pipeline_current_demand,
)


def test_buried_soil_pipeline_current_demand():
    """12-inch pipeline, 10 km, buried in soil with 3% coating breakdown."""
    params = PipelineCPInput(
        outer_diameter_m=0.3048,  # 12 inch
        wall_thickness_m=0.0127,  # 0.5 inch
        length_m=10000.0,
        environment=PipelineEnvironment.BURIED_SOIL,
        coating_breakdown_factor=0.03,
        soil_resistivity_ohm_m=50.0,
    )
    result = pipeline_current_demand(params)

    # Surface area = pi * 0.3048 * 10000 = ~9576 m²
    expected_area = math.pi * 0.3048 * 10000.0
    assert result.total_surface_area_m2 == pytest.approx(expected_area, rel=0.01)

    # Effective bare area = 9576 * 0.03 = ~287 m²
    assert result.effective_bare_area_m2 == pytest.approx(expected_area * 0.03, rel=0.01)

    # Current demand = 287 * 10 / 1000 = ~2.87 A (buried soil = 10 mA/m²)
    assert result.current_demand_A == pytest.approx(2.87, rel=0.05)
    assert result.current_density_mA_m2 == 10.0


def test_seawater_pipeline_higher_current():
    """Submerged seawater pipeline needs much higher current density."""
    soil_params = PipelineCPInput(
        outer_diameter_m=0.3048,
        wall_thickness_m=0.0127,
        length_m=1000.0,
        environment=PipelineEnvironment.BURIED_SOIL,
        coating_breakdown_factor=0.03,
    )
    sea_params = PipelineCPInput(
        outer_diameter_m=0.3048,
        wall_thickness_m=0.0127,
        length_m=1000.0,
        environment=PipelineEnvironment.SUBMERGED_SEAWATER,
        coating_breakdown_factor=0.03,
    )
    soil_result = pipeline_current_demand(soil_params)
    sea_result = pipeline_current_demand(sea_params)

    # Seawater (100 mA/m²) should be 10x soil (10 mA/m²)
    assert sea_result.current_demand_A > soil_result.current_demand_A * 5


def test_anode_spacing_calculation():
    """Anode spacing for a 10 km pipeline with 2.87 A demand and 0.5 A per anode."""
    result = anode_spacing(
        pipeline_length_m=10000.0,
        current_demand_A=2.87,
        anode_current_output_A=0.5,
    )
    # Need ~6 anodes (2.87 / 0.5 = 5.74, ceil = 6)
    assert result.number_of_anodes >= 6
    assert result.anode_spacing_m > 0
    assert result.current_per_anode_A > 0
    # Spacing = 10000 / 6 ≈ 1667 m
    assert result.anode_spacing_m <= 2000.0


def test_anode_spacing_clamp_to_minimum():
    """Very high current demand should respect minimum spacing."""
    result = anode_spacing(
        pipeline_length_m=10000.0,
        current_demand_A=100.0,
        anode_current_output_A=0.5,
        min_spacing_m=50.0,
    )
    assert result.anode_spacing_m >= 50.0


def test_holiday_detection_voltage_fbe():
    """FBE coating at 0.4 mm: thin-film formula gives ~316 V."""
    voltage = holiday_detection_voltage(
        wall_thickness_mm=0.4,
        coating_type="FBE",
    )
    # V = 5 * sqrt(400) = 5 * 20 = 100 V
    assert voltage == pytest.approx(100.0, rel=0.01)


def test_holiday_detection_voltage_pe():
    """PE coating at 3 mm: thick-film formula."""
    voltage = holiday_detection_voltage(
        wall_thickness_mm=3.0,
        coating_type="PE",
    )
    # V = 3.7 * sqrt(3000) ≈ 3.7 * 54.77 = ~203 V
    assert voltage == pytest.approx(202.6, rel=0.02)
