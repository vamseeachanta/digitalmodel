"""Tests for corrosion rate prediction models."""

import math

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
    oxygen_limiting_current_density,
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


# ---- galvanic_corrosion: mixed-potential model (issue #2247) ----
#
# Kinetics: Dunn & Cragnolino (1997), CNWRA 97-010, eqs. 2-2, 2-3, 2-26.


def _galv_input(**overrides):
    params = dict(
        anode_corrosion_potential_V=-0.65,
        anode_corrosion_current_density_A_m2=1e-2,
        anode_tafel_slope_V=0.06,
        cathode_corrosion_potential_V=-0.05,
        cathode_corrosion_current_density_A_m2=1e-3,
        cathode_tafel_slope_V=0.12,
        cathode_limiting_current_density_A_m2=math.inf,
        anode_area_m2=1.0,
        cathode_area_m2=1.0,
        anode_material="carbon_steel",
    )
    params.update(overrides)
    return GalvanicCorrosionInput(**params)


def test_galvanic_corrosion_is_quarantined():
    with pytest.raises(ExperimentalModelError, match="BS PD 6484"):
        galvanic_corrosion(_galv_input())


def test_galvanic_symmetric_couple_exact_hand_value():
    """beta_a = beta_c, i_a0*A_a = i_c0*A_c, no i_L, R_s = 0.

    With x = 10**((E - E_a0)/beta) and K = 10**((E_c0 - E_a0)/beta) the balance
    i0*(x - 1) = i0*(K/x - 1) gives x = sqrt(K): E is the midpoint of the free
    potentials. K = 10**(0.6/0.1) = 1e6, x = 1000:
    E = (-0.65 - 0.05)/2 = -0.35 V; I = 0.01*(1000 - 1) = 9.99 A;
    total anodic density = 0.01 + 9.99 = 10.0 A/m2 = 1e4 mA/m2.
    """
    res = galvanic_corrosion(
        _galv_input(
            anode_tafel_slope_V=0.1,
            cathode_tafel_slope_V=0.1,
            cathode_corrosion_current_density_A_m2=1e-2,
        ),
        experimental=True,
    )
    assert res.converged
    assert res.couple_potential_V == pytest.approx(-0.35, abs=1e-9)
    assert res.galvanic_current_A == pytest.approx(9.99, rel=1e-9)
    assert res.anodic_current_density_A_m2 == pytest.approx(10.0, rel=1e-9)
    assert res.corrosion_rate_mm_yr == pytest.approx(
        1e4 * faraday_rate_factor(55.85, 2, 7870.0), rel=1e-9
    )
    assert res.galvanic_corrosion_rate_mm_yr == pytest.approx(
        9.99e3 * faraday_rate_factor(55.85, 2, 7870.0), rel=1e-9
    )


def test_galvanic_tafel_intersection_hand_value():
    """Far from both free potentials the Evans lines intersect at

    E (1/beta_a + 1/beta_c) = log10(A_c i_c0/(A_a i_a0)) + E_a0/beta_a + E_c0/beta_c
    E * 25 = -1 - 10.8333 - 0.41667 -> E = -0.490 V;
    i = 0.01 * 10**(0.16/0.06) = 4.642 A/m2 (the "-1" self-corrosion terms
    shift this by < 0.2 %).
    """
    res = galvanic_corrosion(_galv_input(), experimental=True)
    assert res.couple_potential_V == pytest.approx(-0.490, abs=1e-3)
    assert res.galvanic_current_density_A_m2 == pytest.approx(4.642, rel=5e-3)
    assert abs(res.residual_V) < 1e-9


@pytest.mark.parametrize("ratio", [0.01, 0.1, 1.0, 10.0, 100.0, 1000.0])
@pytest.mark.parametrize("r_s", [0.0, 5.0])
@pytest.mark.parametrize("i_lim", [0.1, math.inf])
def test_galvanic_couple_between_free_potentials(ratio, r_s, i_lim):
    res = galvanic_corrosion(
        _galv_input(
            cathode_area_m2=ratio,
            solution_resistance_ohm=r_s,
            cathode_limiting_current_density_A_m2=i_lim,
        ),
        experimental=True,
    )
    assert res.converged
    assert -0.65 < res.anode_potential_V <= res.cathode_potential_V + 1e-9
    assert res.cathode_potential_V < -0.05
    assert res.galvanic_current_A > 0


def test_galvanic_current_increases_with_area_ratio():
    densities = [
        galvanic_corrosion(
            _galv_input(cathode_area_m2=r, cathode_limiting_current_density_A_m2=0.1),
            experimental=True,
        ).galvanic_current_density_A_m2
        for r in (0.1, 1.0, 10.0, 100.0)
    ]
    assert all(b > a for a, b in zip(densities, densities[1:]))


def test_galvanic_diffusion_limited_plateau():
    """With a fast anode and small i_L the cathode sits on its diffusion plateau:

    I/A_a -> (A_c/A_a) * (i_L - i_O2(E_c0)), i_O2(E_c0) = i_c0/(1 + i_c0/i_L).
    """
    i_lim, i_c0 = 0.1, 1e-3
    plateau = i_lim - i_c0 / (1.0 + i_c0 / i_lim)
    for ratio in (1.0, 10.0, 100.0):
        res = galvanic_corrosion(
            _galv_input(cathode_area_m2=ratio, cathode_limiting_current_density_A_m2=i_lim),
            experimental=True,
        )
        assert res.galvanic_current_density_A_m2 < ratio * plateau
        assert res.galvanic_current_density_A_m2 > 0.95 * ratio * plateau
        assert res.diffusion_fraction > 0.95
    # On the plateau the current barely depends on the cathodic Tafel slope.
    a = galvanic_corrosion(
        _galv_input(cathode_limiting_current_density_A_m2=1e-3, cathode_tafel_slope_V=0.10),
        experimental=True,
    )
    b = galvanic_corrosion(
        _galv_input(cathode_limiting_current_density_A_m2=1e-3, cathode_tafel_slope_V=0.20),
        experimental=True,
    )
    assert a.galvanic_current_A == pytest.approx(b.galvanic_current_A, rel=0.02)


def test_galvanic_solution_resistance_reduces_current():
    base = galvanic_corrosion(_galv_input(), experimental=True)
    ohmic = galvanic_corrosion(_galv_input(solution_resistance_ohm=10.0), experimental=True)
    assert ohmic.galvanic_current_A < base.galvanic_current_A
    drop = ohmic.cathode_potential_V - ohmic.anode_potential_V
    assert drop == pytest.approx(ohmic.galvanic_current_A * 10.0, rel=1e-9)


def test_galvanic_explicit_faraday_data_matches_material_key():
    by_key = galvanic_corrosion(_galv_input(), experimental=True)
    explicit = galvanic_corrosion(
        _galv_input(
            anode_material=None,
            anode_molar_mass_g_mol=55.85,
            anode_valence=2,
            anode_density_kg_m3=7870.0,
        ),
        experimental=True,
    )
    assert explicit.corrosion_rate_mm_yr == pytest.approx(by_key.corrosion_rate_mm_yr, rel=1e-12)


def test_galvanic_unknown_material_raises():
    with pytest.raises(ValueError, match="unknown anode_material"):
        _galv_input(anode_material="unobtainium")


def test_galvanic_missing_faraday_data_raises():
    with pytest.raises(ValueError, match="anode_material or all of"):
        _galv_input(anode_material=None, anode_molar_mass_g_mol=55.85)


def test_galvanic_swapped_potentials_raise():
    with pytest.raises(ValueError, match="more positive"):
        _galv_input(cathode_corrosion_potential_V=-0.70)


def test_galvanic_has_no_uncited_default_table():
    """The uncited galvanic-series table was removed (#2247): inputs are explicit."""
    import digitalmodel.cathodic_protection.corrosion_rate as cr

    assert not hasattr(cr, "GALVANIC_POTENTIAL")


def test_oxygen_limiting_current_density_hand_value():
    """i_L = 4 F D C / delta = 4 * 96485.33212 * 2e-9 * 0.25 / 5e-4 = 0.385941 A/m2."""
    assert oxygen_limiting_current_density(2e-9, 0.25, 5e-4) == pytest.approx(
        0.38594133, rel=1e-7
    )
    with pytest.raises(ValueError):
        oxygen_limiting_current_density(0.0, 0.25, 5e-4)


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
