"""Tests for impressed current cathodic protection (ICCP) system design."""

import pytest

from digitalmodel.cathodic_protection.iccp_design import (
    AnodeBedType,
    AnodeMaterial,
    RectifierSizingInput,
    anode_bed_design,
    cable_sizing,
    rectifier_sizing,
)


def test_rectifier_sizing_pipeline():
    """Typical pipeline ICCP: 5 A current, 2 ohm ground bed."""
    input_params = RectifierSizingInput(
        total_current_A=5.0,
        ground_bed_resistance_ohm=2.0,
        structure_coating_resistance_ohm=1.0,
        cable_resistance_ohm=0.5,
        back_emf_V=2.0,
        safety_factor=1.25,
    )
    result = rectifier_sizing(input_params)

    # V = (5 * (2+1+0.5) + 2) * 1.25 = (17.5 + 2) * 1.25 = 24.375 V
    assert result.dc_voltage_V == pytest.approx(24.375, rel=0.01)
    assert result.dc_current_A == 5.0
    assert result.power_W > 0
    # Should recommend at least 24V rating
    assert result.recommended_rating_V >= 24
    assert result.recommended_rating_A >= 5


def test_rectifier_sizing_higher_current():
    """Large structure with 50 A demand."""
    input_params = RectifierSizingInput(
        total_current_A=50.0,
        ground_bed_resistance_ohm=0.5,
        structure_coating_resistance_ohm=0.3,
        cable_resistance_ohm=0.2,
        back_emf_V=2.0,
    )
    result = rectifier_sizing(input_params)

    assert result.dc_current_A == 50.0
    assert result.recommended_rating_A >= 50
    assert result.power_W > 1000  # should be >1 kW for 50 A
    assert result.exceeds_standard_range is False
    assert result.units_required == 1


def test_rectifier_sizing_flags_exceeding_standard_range():
    """300 A into a 5 ohm bed: V = (300*(5+1+0) + 2) * 1.25 = 2252.5 V.

    Largest standard unit is 120 V / 200 A, so the voltage needs
    ceil(2252.5 / 120) = 19 units (current alone would need 2). The old
    code silently reported 120 V / 200 A (issue #2209).
    """
    input_params = RectifierSizingInput(
        total_current_A=300.0,
        ground_bed_resistance_ohm=5.0,
        structure_coating_resistance_ohm=1.0,
        cable_resistance_ohm=0.0,
        back_emf_V=2.0,
        safety_factor=1.25,
    )
    result = rectifier_sizing(input_params)

    assert result.dc_voltage_V == pytest.approx(2252.5, rel=1e-6)
    assert result.exceeds_standard_range is True
    assert result.units_required >= 19
    assert result.units_required == 19
    # Per-unit ratings stay at the largest standard size.
    assert result.recommended_rating_V == 120.0
    assert result.recommended_rating_A == 200.0


def test_anode_bed_deep_well():
    """Deep well anode bed: 10 A, 50 ohm-m soil, HSCI anodes."""
    result = anode_bed_design(
        total_current_A=10.0,
        soil_resistivity_ohm_m=50.0,
        design_life_years=25.0,
        bed_type=AnodeBedType.DEEP_WELL,
        anode_material=AnodeMaterial.HIGH_SILICON_CAST_IRON,
    )
    assert result.number_of_anodes >= 1
    assert result.bed_resistance_ohm > 0
    # Life figure is quarantined (issue #2209): None unless experimental=True.
    assert result.estimated_life_years is None
    assert result.bed_type == "deep_well"


def test_anode_bed_life_experimental_smoke():
    """With experimental=True the (quarantined) life figure is returned."""
    result = anode_bed_design(
        total_current_A=10.0,
        soil_resistivity_ohm_m=50.0,
        design_life_years=25.0,
        bed_type=AnodeBedType.DEEP_WELL,
        anode_material=AnodeMaterial.HIGH_SILICON_CAST_IRON,
        experimental=True,
    )
    assert result.estimated_life_years is not None
    assert result.estimated_life_years >= 25.0
    # Geometry / resistance are identical with or without the flag.
    plain = anode_bed_design(
        total_current_A=10.0,
        soil_resistivity_ohm_m=50.0,
        design_life_years=25.0,
        bed_type=AnodeBedType.DEEP_WELL,
        anode_material=AnodeMaterial.HIGH_SILICON_CAST_IRON,
    )
    assert plain.number_of_anodes == result.number_of_anodes
    assert plain.bed_resistance_ohm == result.bed_resistance_ohm


def test_anode_bed_mmo_fewer_anodes():
    """MMO anodes have higher current density → fewer anodes needed."""
    hsci = anode_bed_design(
        total_current_A=10.0,
        soil_resistivity_ohm_m=50.0,
        anode_material=AnodeMaterial.HIGH_SILICON_CAST_IRON,
    )
    mmo = anode_bed_design(
        total_current_A=10.0,
        soil_resistivity_ohm_m=50.0,
        anode_material=AnodeMaterial.MIXED_METAL_OXIDE,
    )
    # MMO can handle 100 A/m² vs HSCI at 10 A/m²
    assert mmo.number_of_anodes <= hsci.number_of_anodes


def test_cable_sizing_standard():
    """Cable sizing for 10 A, 500 m one-way, max 2 V drop."""
    result = cable_sizing(
        current_A=10.0,
        cable_length_m=500.0,
        max_voltage_drop_V=2.0,
    )
    assert result["min_area_mm2"] > 0
    assert result["selected_area_mm2"] >= result["min_area_mm2"]
    assert result["voltage_drop_V"] <= 2.0
    # For 10 A, 500 m: min area = 0.0175 * 10 * 1000 / 2 = 87.5 mm²
    assert result["selected_area_mm2"] >= 87.5


def test_cable_sizing_temperature_correction():
    """Higher temperature increases resistivity → larger cable."""
    cool = cable_sizing(current_A=10.0, cable_length_m=100.0, temperature_c=20.0)
    hot = cable_sizing(current_A=10.0, cable_length_m=100.0, temperature_c=60.0)
    assert hot["min_area_mm2"] > cool["min_area_mm2"]
