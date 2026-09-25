"""Tests for corrosion rate prediction models."""

import pytest

from digitalmodel.cathodic_protection._experimental import ExperimentalModelError
from digitalmodel.cathodic_protection.corrosion_rate import (
    CORROSION_RATE_FACTOR,
    CO2CorrosionInput,
    GalvanicCorrosionInput,
    de_waard_milliams_co2,
    faraday_rate_factor,
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


# ---- Faraday rate factors (issue #2209) ----


def test_faraday_rate_factor_iron_hand_derived():
    """Fe: rate = M/(z F) * 1e-3 A/m2 * s/yr / rho * 1e3 mm/m.

    M = 55.85 g/mol, z = 2, F = 96485.33 C/mol, rho = 7870 kg/m3,
    1 yr = 3.15576e7 s:
        55.85 / (2 * 96485.33) = 2.8942e-4 g/C
        * 1e-3 A/m2            = 2.8942e-7 g/(s m2)
        * 3.15576e7 s/yr       = 9.1334 g/(m2 yr)
        / 7.87e6 g/m3          = 1.1605e-6 m/yr
        * 1e3 mm/m             = 0.0011605 mm/yr per mA/m2
    """
    molar_mass = 55.85  # g/mol
    valence = 2
    faraday = 96485.33212  # C/mol
    density_g_m3 = 7870.0 * 1e3
    seconds_per_year = 3.15576e7
    expected = (
        molar_mass / (valence * faraday) * 1e-3 * seconds_per_year / density_g_m3 * 1e3
    )
    assert expected == pytest.approx(0.001161, abs=1e-6)
    assert faraday_rate_factor(55.85, 2, 7870.0) == pytest.approx(expected, abs=1e-5)
    assert CORROSION_RATE_FACTOR["carbon_steel"] == pytest.approx(expected, abs=1e-5)


@pytest.mark.parametrize(
    "material,expected",
    [
        ("carbon_steel", 0.0011605),  # Fe, M=55.85, z=2, rho=7870
        ("cast_iron", 0.0011605),  # treated as Fe in this module
        ("aluminum_alloy", 0.0010894),  # Al, M=26.98, z=3, rho=2700
        ("copper", 0.0011599),  # Cu, M=63.55, z=2, rho=8960
        ("zinc", 0.0014975),  # Zn, M=65.38, z=2, rho=7140
    ],
)
def test_corrosion_rate_factor_values(material, expected):
    """Hand-derived Faraday factors; the old table was 10x too high."""
    assert CORROSION_RATE_FACTOR[material] == pytest.approx(expected, abs=1e-5)
    assert CORROSION_RATE_FACTOR[material] < 0.002


# ---- galvanic_corrosion: quarantined (issue #2209) ----


def test_galvanic_corrosion_is_quarantined():
    """Without experimental=True the ohmic-only model must refuse to run."""
    input_params = GalvanicCorrosionInput(
        anode_material="carbon_steel",
        cathode_material="stainless_steel_316",
        anode_area_m2=1.0,
        cathode_area_m2=1.0,
    )
    with pytest.raises(ExperimentalModelError, match="BS PD 6484"):
        galvanic_corrosion(input_params)


def test_galvanic_corrosion_experimental_smoke():
    """Carbon steel coupled to stainless steel in seawater (experimental)."""
    input_params = GalvanicCorrosionInput(
        anode_material="carbon_steel",
        cathode_material="stainless_steel_316",
        anode_area_m2=1.0,
        cathode_area_m2=1.0,
        electrolyte_resistivity_ohm_m=0.25,
    )
    result = galvanic_corrosion(input_params, experimental=True)

    assert result.corrosion_rate_mm_yr > 0
    assert result.area_ratio == pytest.approx(1.0)
    # Potential difference: |-0.65 - (-0.05)| = 0.60 V -> i = 0.6/(0.25*0.1) A/m2
    assert result.galvanic_current_density_mA_m2 == pytest.approx(24000.0, rel=1e-3)
    # rate = i * Faraday factor (Fe) = 24000 * 0.0011605
    assert result.corrosion_rate_mm_yr == pytest.approx(
        24000.0 * CORROSION_RATE_FACTOR["carbon_steel"], rel=1e-3
    )


def test_galvanic_corrosion_experimental_area_ratio_effect():
    """Large cathode / small anode increases corrosion rate (experimental)."""
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

    result_small = galvanic_corrosion(small_cathode, experimental=True)
    result_large = galvanic_corrosion(large_cathode, experimental=True)

    assert result_large.corrosion_rate_mm_yr > result_small.corrosion_rate_mm_yr


@pytest.mark.parametrize("field", ["anode_material", "cathode_material"])
def test_galvanic_corrosion_unknown_material_raises(field):
    """Unknown materials must not silently default to carbon steel / 304."""
    kwargs = dict(
        anode_material="carbon_steel",
        cathode_material="stainless_steel_316",
        anode_area_m2=1.0,
        cathode_area_m2=1.0,
    )
    kwargs[field] = "unobtainium"
    with pytest.raises(ValueError, match="unobtainium"):
        galvanic_corrosion(GalvanicCorrosionInput(**kwargs), experimental=True)


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
