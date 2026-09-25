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
from digitalmodel.cathodic_protection.marine_structure_cp import (
    ClimateRegion,
    ExposureZone,
    StructuralZone,
    anode_distribution,
    marine_structure_current_demand,
    retrofit_assessment,
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
    assert result.number_of_anodes == 487
    assert result.number_of_anodes == math.ceil(97333.33 / 200.0)


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
    # 20 A * 12 yr * 8760 / (2000 * 0.9) = 1168 kg consumed
    assert result.remaining_anode_life_years > 0
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
