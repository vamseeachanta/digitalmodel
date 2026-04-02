"""Tests for corrosion rate prediction models."""

import pytest

from digitalmodel.cathodic_protection.corrosion_rate import (
    CO2CorrosionInput,
    GalvanicCorrosionInput,
    de_waard_milliams_co2,
    galvanic_corrosion,
    norsok_m506_co2,
    pitting_rate_estimate,
)


def test_de_waard_milliams_moderate_conditions():
    """CO2 corrosion at 60°C, 2 bar CO2, pH 4.5."""
    input_params = CO2CorrosionInput(
        temperature_c=60.0,
        co2_partial_pressure_bar=2.0,
        ph=4.5,
    )
    result = de_waard_milliams_co2(input_params)

    assert result.model_used == "de_Waard_Milliams_1975"
    assert result.corrosion_rate_mm_yr > 0
    # At 60°C, 2 bar CO2: typically 2-8 mm/yr range
    assert 0.5 < result.corrosion_rate_mm_yr < 20.0


def test_de_waard_milliams_higher_temp_higher_rate():
    """Higher temperature should give higher corrosion rate."""
    low_t = CO2CorrosionInput(temperature_c=30.0, co2_partial_pressure_bar=1.0)
    high_t = CO2CorrosionInput(temperature_c=80.0, co2_partial_pressure_bar=1.0)

    result_low = de_waard_milliams_co2(low_t)
    result_high = de_waard_milliams_co2(high_t)

    assert result_high.corrosion_rate_mm_yr > result_low.corrosion_rate_mm_yr


def test_de_waard_milliams_higher_ph_lower_rate():
    """Higher pH should reduce corrosion rate."""
    low_ph = CO2CorrosionInput(
        temperature_c=60.0, co2_partial_pressure_bar=2.0, ph=4.0
    )
    high_ph = CO2CorrosionInput(
        temperature_c=60.0, co2_partial_pressure_bar=2.0, ph=6.0
    )

    result_low = de_waard_milliams_co2(low_ph)
    result_high = de_waard_milliams_co2(high_ph)

    assert result_high.corrosion_rate_mm_yr < result_low.corrosion_rate_mm_yr


def test_norsok_m506_moderate_conditions():
    """NORSOK M-506 at 60°C, 2 bar CO2."""
    result = norsok_m506_co2(
        temperature_c=60.0,
        co2_partial_pressure_bar=2.0,
        ph=4.5,
    )
    assert result.model_used == "NORSOK_M506_simplified"
    assert result.corrosion_rate_mm_yr > 0


def test_galvanic_corrosion_carbon_steel_stainless():
    """Carbon steel coupled to stainless steel in seawater."""
    input_params = GalvanicCorrosionInput(
        anode_material="carbon_steel",
        cathode_material="stainless_steel_316",
        anode_area_m2=1.0,
        cathode_area_m2=1.0,
        electrolyte_resistivity_ohm_m=0.25,
    )
    result = galvanic_corrosion(input_params)

    assert result.corrosion_rate_mm_yr > 0
    assert result.area_ratio == pytest.approx(1.0)
    # Potential difference: |-0.65 - (-0.05)| = 0.60 V — should be significant
    assert result.galvanic_current_density_mA_m2 > 100


def test_galvanic_corrosion_area_ratio_effect():
    """Large cathode / small anode increases corrosion rate."""
    small_cathode = GalvanicCorrosionInput(
        anode_material="carbon_steel",
        cathode_material="stainless_steel_304",
        anode_area_m2=1.0,
        cathode_area_m2=0.1,
    )
    large_cathode = GalvanicCorrosionInput(
        anode_material="carbon_steel",
        cathode_material="stainless_steel_304",
        anode_area_m2=1.0,
        cathode_area_m2=10.0,
    )

    result_small = galvanic_corrosion(small_cathode)
    result_large = galvanic_corrosion(large_cathode)

    assert result_large.corrosion_rate_mm_yr > result_small.corrosion_rate_mm_yr


def test_pitting_rate_estimate():
    """Pitting rate should be 3x general corrosion (default factor)."""
    general_rate = 1.0  # mm/yr
    pit_rate = pitting_rate_estimate(general_rate)
    assert pit_rate == pytest.approx(3.0, rel=0.01)


def test_pitting_rate_p90():
    """P90 pitting factor ~5x general rate."""
    general_rate = 1.0
    pit_rate = pitting_rate_estimate(general_rate, confidence_level="p90")
    assert pit_rate == pytest.approx(5.0, rel=0.01)
