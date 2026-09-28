"""Tests for pipeline cathodic protection design."""

import math

import pytest

from digitalmodel.cathodic_protection import dnv_rp_f106
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


def test_holiday_detection_voltage_fbe_order_of_magnitude():
    """0.4 mm FBE per NACE SP0490: V = 525 * sqrt(15.75 mil) = 2083 V (~2 kV).

    The old 5 * sqrt(t_um) body gave 100 V, twenty times too low (#2209).
    """
    voltage = holiday_detection_voltage(
        coating_thickness_mm=0.4,
        coating_type="FBE",
    )
    t_mil = 0.4 / 0.0254
    assert voltage == pytest.approx(525.0 * math.sqrt(t_mil), rel=1e-6)
    assert voltage == pytest.approx(2083.0, rel=0.01)
    assert voltage > 1000.0


@pytest.mark.parametrize(
    "coating_type,thickness_mm,f106_type",
    [
        ("PE", 3.0, dnv_rp_f106.CoatingType.THREE_LAYER_PE),
        ("3LPE", 2.0, dnv_rp_f106.CoatingType.THREE_LAYER_PE),
        ("PP", 3.0, dnv_rp_f106.CoatingType.THREE_LAYER_PP),
        ("ASPHALT", 5.0, dnv_rp_f106.CoatingType.ASPHALT_ENAMEL),
        ("coal_tar", 6.0, dnv_rp_f106.CoatingType.COAL_TAR_ENAMEL),
    ],
)
def test_holiday_detection_voltage_agrees_with_f106(
    coating_type, thickness_mm, f106_type
):
    """Non-FBE families delegate to dnv_rp_f106.holiday_detection_voltage."""
    voltage = holiday_detection_voltage(thickness_mm, coating_type)
    expected = dnv_rp_f106.holiday_detection_voltage(f106_type, thickness_mm)
    assert voltage == pytest.approx(expected, rel=1e-9)


def test_holiday_detection_voltage_pe_hand_value():
    """3LPE at 2.0 mm: F106 CDS No.2 rule 10 kV/mm -> 20 kV (cap 25 kV)."""
    assert holiday_detection_voltage(2.0, "PE") == pytest.approx(20000.0)
    assert holiday_detection_voltage(3.0, "PE") == pytest.approx(25000.0)


def test_holiday_detection_voltage_deprecated_wall_thickness_keyword():
    """wall_thickness_mm still works but warns; result equals the new keyword."""
    with pytest.warns(DeprecationWarning, match="coating_thickness_mm"):
        old = holiday_detection_voltage(wall_thickness_mm=0.4, coating_type="FBE")
    assert old == pytest.approx(
        holiday_detection_voltage(coating_thickness_mm=0.4, coating_type="FBE")
    )


def test_holiday_detection_voltage_rejects_bad_inputs():
    with pytest.raises(ValueError, match="required"):
        holiday_detection_voltage(coating_type="FBE")
    with pytest.raises(ValueError, match="positive"):
        holiday_detection_voltage(0.0, "FBE")
    with pytest.warns(DeprecationWarning):
        with pytest.raises(ValueError, match="not both"):
            holiday_detection_voltage(0.4, "FBE", wall_thickness_mm=0.4)
    # F106 has no tabulated rule for polychloroprene (project-specific).
    with pytest.raises(ValueError, match="project-specific"):
        holiday_detection_voltage(1.0, "neoprene")
