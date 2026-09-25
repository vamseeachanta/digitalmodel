"""Tests for marine structure cathodic protection design.

Expected values are hand-derived from DNV-RP-B401 Table 10-1 (initial,
final), Table 10-2 (mean) and Sec. 6.3 (buried, 0.020 A/m²), issue #2207.
Temperate column: 0-30 m initial 0.200 / mean 0.100 / final 0.130 A/m²;
>30-100 m initial 0.170 / mean 0.080 / final 0.110 A/m².
"""

import math

import pytest

from digitalmodel.cathodic_protection.b401_tables import (
    Climate,
    DepthBand,
    DesignPhase,
)
from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.marine_structure_cp import (
    ClimateRegion,
    ExposureZone,
    StructuralZone,
    anode_distribution,
    marine_structure_current_demand,
    retrofit_assessment,
    standoff_anode_design_loop,
    zone_current_density,
)

EDITION = "2010"  # edition whose tables the wiki holds; avoids the None warning


def test_jacket_platform_cp_design():
    """Typical North Sea jacket: submerged + buried zones, temperate climate."""
    zones = [
        StructuralZone(
            zone_name="submerged_legs",
            exposure_zone=ExposureZone.SUBMERGED,
            surface_area_m2=2000.0,
            coating_breakdown_factor=0.05,
        ),
        StructuralZone(
            zone_name="buried_piles",
            exposure_zone=ExposureZone.BURIED_MUDLINE,
            surface_area_m2=500.0,
            coating_breakdown_factor=1.0,  # bare steel at mudline
        ),
    ]
    result = marine_structure_current_demand(
        zones=zones,
        climate_region=ClimateRegion.TEMPERATE,
        design_life_years=25.0,
        anode_net_mass_kg=200.0,
        edition=EDITION,
    )

    # Submerged temperate 0-30 m: initial=0.200, mean=0.100, final=0.130 A/m²
    # Submerged legs: 2000 * 0.05 * 0.100 = 10 A mean
    # Buried piles (Sec. 6.3): 500 * 1.0 * 0.020 = 10 A mean
    assert result.total_mean_current_A == pytest.approx(20.0, abs=1e-6)
    # Initial: 2000 * 0.05 * 0.200 + 500 * 0.020 = 20 + 10 = 30 A
    assert result.total_initial_current_A == pytest.approx(30.0, abs=1e-6)
    # Final: 2000 * 0.05 * 0.130 + 500 * 0.020 = 13 + 10 = 23 A
    assert result.total_final_current_A == pytest.approx(23.0, abs=1e-6)
    # Mass: 20 * 25 * 8760 / (2000 * 0.90) = 2433.33 kg -> 13 anodes of 200 kg
    assert result.total_anode_mass_kg == pytest.approx(2433.33, abs=0.01)
    assert result.number_of_anodes == 13
    assert len(result.zone_details) == 2
    assert result.citations == [
        "dnv-rp-b401 2011 Table 10-1",
        "dnv-rp-b401 2011 Table 10-2",
        "dnv-rp-b401 2011 Sec. 6.3 (buried surfaces)",
    ]
    assert result.edition == "2010"
    assert result.standard == "DNV-RP-B401 (October 2010)"


def test_review_appendix_a2_jacket_hand_calc():
    """Review Appendix A2: temperate, 0-30 m, 8000 m² bare, 25 yr, Al, u=0.90.

    I_mean = 8000 * 0.100 = 800 A; M = 800 * 25 * 8760 / (0.90 * 2000)
    = 97 333 kg; N = ceil(97 333 / 200) = 487.
    I_initial = 8000 * 0.200 = 1600 A; I_final = 8000 * 0.130 = 1040 A.
    """
    zones = [
        StructuralZone(
            zone_name="jacket",
            exposure_zone=ExposureZone.SUBMERGED,
            surface_area_m2=8000.0,
            depth_m=20.0,
        ),
    ]
    result = marine_structure_current_demand(
        zones=zones,
        climate_region=ClimateRegion.TEMPERATE,
        design_life_years=25.0,
        anode_net_mass_kg=200.0,
        anode_capacity_Ah_kg=2000.0,
        utilization_factor=0.90,
        edition=EDITION,
    )
    assert result.total_mean_current_A == pytest.approx(800.0, abs=1e-6)
    assert result.total_initial_current_A == pytest.approx(1600.0, abs=1e-6)
    assert result.total_final_current_A == pytest.approx(1040.0, abs=1e-6)
    assert result.total_anode_mass_kg == pytest.approx(97333.33, abs=0.01)
    # Without anode geometry the count is mass-only and says so.
    assert result.number_of_anodes == 487
    assert result.number_of_anodes == math.ceil(97333.33 / 200.0)
    assert result.number_of_anodes_mass == 487
    assert result.governing_case == "mass"
    assert result.current_output_checked is False
    assert result.number_of_anodes_initial is None


def test_review_appendix_a2_full_b401_design_loop():
    """Review Appendix A2 with the B401 Sec. 7 loop (#2211): initial governs.

    Mass: N_mass = ceil(97 333 / 200) = 487.
    Initial (fresh anode, L = 2.0 m, rho = 0.30 ohm-m, 2750 kg/m3):
    r_eq = sqrt(200 / (pi * 2 * 2750)) = 0.1076 m;
    R_a = 0.30 / (2 pi 2.0) * (ln(8 / 0.1076) - 1) = 0.0790 ohm;
    I_a = 0.25 / 0.0790 = 3.16 A; N_initial = ceil(1600 / 3.16) = 506.
    Final (anode consumed to u = 0.90: remaining mass 20 kg, same length):
    r_f = sqrt(20 / (pi * 2 * 2750)) = 0.03402 m (L >= 4 r_f, long form);
    R_f = 0.30 / (2 pi 2.0) * (ln(8 / 0.03402) - 1) = 0.10648 ohm;
    I_f = 0.25 / 0.10648 = 2.348 A; N_final = ceil(1040 / 2.348) = 443.
    N = max(487, 506, 443) = 506, governing case "initial".
    """
    zones = [
        StructuralZone(
            zone_name="jacket",
            exposure_zone=ExposureZone.SUBMERGED,
            surface_area_m2=8000.0,
            depth_m=20.0,
        ),
    ]
    result = marine_structure_current_demand(
        zones=zones,
        climate_region=ClimateRegion.TEMPERATE,
        design_life_years=25.0,
        anode_net_mass_kg=200.0,
        anode_capacity_Ah_kg=2000.0,
        utilization_factor=0.90,
        edition=EDITION,
        anode_length_m=2.0,
        seawater_resistivity_ohm_m=0.30,
    )
    assert result.current_output_checked is True
    assert result.number_of_anodes_mass == 487
    assert result.anode_resistance_initial_ohm == pytest.approx(0.0790, abs=0.0001)
    assert result.anode_current_output_initial_A == pytest.approx(3.16, abs=0.01)
    assert result.number_of_anodes_initial == 506
    assert result.number_of_anodes_initial == math.ceil(1600.0 / (0.25 / 0.07899))

    # Final case computed from the depleted geometry in the test itself.
    r_final = kernel.equivalent_radius_from_mass((1.0 - 0.90) * 200.0, 2.0, 2750.0)
    assert r_final == pytest.approx(0.03402, abs=1e-5)
    assert 2.0 >= 4.0 * r_final
    R_final = kernel.long_slender_standoff(0.30, 2.0, r_final)
    assert R_final == pytest.approx(0.10648, abs=1e-5)
    n_final = math.ceil(1040.0 / (0.25 / R_final))
    assert n_final == 443
    assert result.anode_resistance_final_ohm == pytest.approx(R_final, rel=1e-9)
    assert result.number_of_anodes_final == n_final

    assert result.number_of_anodes == max(487, 506, n_final) == 506
    assert result.governing_case == "initial"
    assert "dnv-rp-b401 2011 Table 10-6 with Sec. 5 (structure-to-electrolyte potential criteria)" in result.citations

    # The standalone loop reproduces the same per-case counts.
    loop = standoff_anode_design_loop(
        total_mass_kg=97333.333,
        initial_current_A=1600.0,
        final_current_A=1040.0,
        anode_net_mass_kg=200.0,
        anode_length_m=2.0,
        utilization_factor=0.90,
        seawater_resistivity_ohm_m=0.30,
    )
    assert (loop.number_of_anodes_mass, loop.number_of_anodes_initial, loop.number_of_anodes_final) == (487, 506, 443)
    assert loop.governing_case == "initial"


def test_final_case_governs_when_final_demand_dominates():
    """A structure whose final demand exceeds the depleted output governs on 'final'.

    Same anodes as A2 (I_f = 2.348 A depleted) with initial 1000 A,
    mean 500 A over 5 yr (M = 12 167 kg, N_mass = 61), final 1040 A:
    N_initial = ceil(1000 / 3.165) = 316, N_final = ceil(1040 / 2.348) = 443.
    """
    loop = standoff_anode_design_loop(
        total_mass_kg=12166.7,
        initial_current_A=1000.0,
        final_current_A=1040.0,
        anode_net_mass_kg=200.0,
        anode_length_m=2.0,
        utilization_factor=0.90,
        seawater_resistivity_ohm_m=0.30,
    )
    assert loop.number_of_anodes_mass == 61
    assert loop.number_of_anodes_initial == 316
    assert loop.number_of_anodes_final == 443
    assert loop.number_of_anodes == 443
    assert loop.governing_case == "final"


def test_zero_breakdown_is_perfect_coating_and_none_is_bare():
    """f_c = 0.0 gives zero demand; None means bare steel (f_c = 1.0)."""
    perfect = StructuralZone(
        zone_name="z",
        exposure_zone=ExposureZone.SUBMERGED,
        surface_area_m2=1000.0,
        coating_breakdown_factor=0.0,
    )
    bare = StructuralZone(
        zone_name="z",
        exposure_zone=ExposureZone.SUBMERGED,
        surface_area_m2=1000.0,
    )
    assert bare.coating_breakdown_factor is None
    assert bare.effective_breakdown_factor == 1.0

    zero = marine_structure_current_demand(
        [perfect], ClimateRegion.TEMPERATE, edition=EDITION
    )
    full = marine_structure_current_demand([bare], ClimateRegion.TEMPERATE, edition=EDITION)
    assert zero.total_initial_current_A == 0.0
    assert zero.total_mean_current_A == 0.0
    assert zero.total_final_current_A == 0.0
    # Bare temperate 0-30 m mean: 1000 * 1.0 * 0.100 = 100 A
    assert full.total_mean_current_A == pytest.approx(100.0, abs=1e-6)


def test_depth_band_selects_table_row():
    """Submerged at 50 m uses the >30-100 m row: temperate mean 0.080 A/m²."""
    zone = StructuralZone(
        zone_name="legs",
        exposure_zone=ExposureZone.SUBMERGED,
        surface_area_m2=1000.0,
        depth_m=50.0,
    )
    result = marine_structure_current_demand(
        [zone], ClimateRegion.TEMPERATE, edition=EDITION
    )
    # 1000 * 1.0 * 0.080 = 80 A mean; 0.170 -> 170 A initial; 0.110 -> 110 A final
    assert result.total_mean_current_A == pytest.approx(80.0, abs=1e-6)
    assert result.total_initial_current_A == pytest.approx(170.0, abs=1e-6)
    assert result.total_final_current_A == pytest.approx(110.0, abs=1e-6)


def test_surface_temperature_overrides_climate_region():
    """25 °C surface water is tropical (> 20 °C): 0-30 m mean 0.070 A/m²."""
    zone = StructuralZone(
        zone_name="legs", exposure_zone=ExposureZone.SUBMERGED, surface_area_m2=1000.0
    )
    result = marine_structure_current_demand(
        [zone], ClimateRegion.ARCTIC, edition=EDITION, surface_temperature_c=25.0
    )
    assert result.total_mean_current_A == pytest.approx(70.0, abs=1e-6)
    assert result.zone_details[0]["climate"] == "tropical"


def test_tidal_uses_shallowest_band_regardless_of_depth():
    """B401 has no tidal row; the 0-30 m seawater row is used at any depth."""
    tidal = zone_current_density(
        ExposureZone.TIDAL, Climate.TEMPERATE, 250.0, DesignPhase.MEAN, EDITION
    )
    shallow = zone_current_density(
        ExposureZone.SUBMERGED, Climate.TEMPERATE, 10.0, DesignPhase.MEAN, EDITION
    )
    assert tidal is not None and shallow is not None
    assert tidal.value == shallow.value == 0.100
    assert "0-30" in tidal.citation.note


def test_buried_and_splash_densities():
    """Buried: Sec. 6.3 0.020 A/m² all phases; splash/atmospheric: none."""
    for phase in DesignPhase:
        buried = zone_current_density(
            ExposureZone.BURIED_MUDLINE, Climate.ARCTIC, 0.0, phase, EDITION
        )
        assert buried is not None
        assert buried.value == 0.020
        assert buried.citation.section.startswith("Sec. 6.3")
        assert zone_current_density(
            ExposureZone.SPLASH, Climate.ARCTIC, 0.0, phase, EDITION
        ) is None
        assert zone_current_density(
            ExposureZone.ATMOSPHERIC, Climate.ARCTIC, 0.0, phase, EDITION
        ) is None


def test_zone_details_carry_provenance():
    """Each zone row records its densities, depth band inputs and citations."""
    zone = StructuralZone(
        zone_name="splash", exposure_zone=ExposureZone.SPLASH, surface_area_m2=100.0
    )
    result = marine_structure_current_demand(
        [zone], ClimateRegion.TEMPERATE, edition=EDITION
    )
    row = result.zone_details[0]
    assert row["mean_current_density_A_m2"] == 0.0
    assert row["citations"] == []
    assert result.citations == []
    assert result.total_mean_current_A == 0.0


def test_tropical_lower_current_than_arctic():
    """Tropical climate should have lower current density than arctic."""
    zone = [
        StructuralZone(
            zone_name="submerged",
            exposure_zone=ExposureZone.SUBMERGED,
            surface_area_m2=1000.0,
            coating_breakdown_factor=1.0,
        ),
    ]
    tropical = marine_structure_current_demand(
        zones=zone, climate_region=ClimateRegion.TROPICAL, edition=EDITION
    )
    arctic = marine_structure_current_demand(
        zones=zone, climate_region=ClimateRegion.ARCTIC, edition=EDITION
    )
    # Table 10-2, 0-30 m: tropical 0.070 vs arctic 0.120 A/m²
    assert tropical.total_mean_current_A == pytest.approx(70.0, abs=1e-6)
    assert arctic.total_mean_current_A == pytest.approx(120.0, abs=1e-6)
    assert tropical.total_mean_current_A < arctic.total_mean_current_A


def test_anode_distribution_proportional():
    """Anodes should be distributed proportional to final current demand."""
    zones = [
        StructuralZone(
            zone_name="high_demand",
            exposure_zone=ExposureZone.SUBMERGED,
            surface_area_m2=3000.0,
            coating_breakdown_factor=0.10,
        ),
        StructuralZone(
            zone_name="low_demand",
            exposure_zone=ExposureZone.BURIED_MUDLINE,
            surface_area_m2=500.0,
            coating_breakdown_factor=0.10,
        ),
    ]
    dist = anode_distribution(
        zones=zones,
        total_anodes=100,
        climate_region=ClimateRegion.TEMPERATE,
        edition=EDITION,
    )
    # Final densities: submerged temperate 0-30 m 0.130 vs buried 0.020 A/m²
    # Demands: 3000*0.1*0.130 = 39 vs 500*0.1*0.020 = 1 -> 97.5 % / 2.5 %
    assert dist["high_demand"] == 98
    assert dist["low_demand"] == 2
    assert sum(dist.values()) == 100


def test_anode_distribution_ignores_perfectly_coated_zone():
    """A zone with f_c = 0.0 attracts no anodes."""
    zones = [
        StructuralZone(
            zone_name="coated",
            exposure_zone=ExposureZone.SUBMERGED,
            surface_area_m2=3000.0,
            coating_breakdown_factor=0.0,
        ),
        StructuralZone(
            zone_name="bare",
            exposure_zone=ExposureZone.SUBMERGED,
            surface_area_m2=500.0,
        ),
    ]
    dist = anode_distribution(zones, 10, ClimateRegion.TEMPERATE, edition=EDITION)
    assert dist == {"coated": 0, "bare": 10}


def test_retrofit_assessment_adequate():
    """A system with 50% remaining mass at midlife should be adequate."""
    result = retrofit_assessment(
        original_anode_mass_kg=10000.0,
        elapsed_years=12.0,
        design_life_years=25.0,
        mean_current_A=20.0,
        measured_potential_V=-0.900,  # well protected
    )
    # Faraday: 20 A * 12 yr * 8760 / 2000 = 1051 kg consumed; usable
    # 9000 kg -> 7949 kg left -> 90.7 yr remaining (#2211)
    assert result.remaining_anode_life_years == pytest.approx(90.74, abs=0.05)
    assert not result.is_retrofit_needed or result.additional_anodes_needed == 0


def test_retrofit_assessment_depleted():
    """Nearly depleted system with poor potential should flag retrofit."""
    result = retrofit_assessment(
        original_anode_mass_kg=2000.0,
        elapsed_years=20.0,
        design_life_years=25.0,
        mean_current_A=20.0,
        measured_potential_V=-0.750,  # below protection threshold
    )
    assert result.is_retrofit_needed
    assert result.additional_anodes_needed > 0
    assert "URGENT" in result.recommendation


def test_depth_band_helper_is_used_for_submerged():
    """Sanity: DepthBand rows drive the lookup (300 m is still >100-300)."""
    at_300 = zone_current_density(
        ExposureZone.SUBMERGED, Climate.TEMPERATE, 300.0, DesignPhase.MEAN, EDITION
    )
    assert at_300 is not None
    assert DepthBand.M100_300.value in at_300.citation.note
    assert at_300.value == 0.090
