"""Tests for marine structure cathodic protection design."""

import pytest

from digitalmodel.cathodic_protection.marine_structure_cp import (
    ClimateRegion,
    ExposureZone,
    StructuralZone,
    anode_distribution,
    marine_structure_current_demand,
    retrofit_assessment,
)


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
    )

    # Submerged temperate: initial=200, mean=100, final=120 mA/m²
    # Submerged legs: 2000*0.05*100/1000 = 10 A mean
    # Buried piles: 500*1.0*20/1000 = 10 A mean
    assert result.total_mean_current_A == pytest.approx(20.0, rel=0.05)
    assert result.total_anode_mass_kg > 0
    assert result.number_of_anodes > 0
    assert len(result.zone_details) == 2


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
        zones=zone, climate_region=ClimateRegion.TROPICAL
    )
    arctic = marine_structure_current_demand(
        zones=zone, climate_region=ClimateRegion.ARCTIC
    )
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
    )
    # Submerged has much higher current density (120 vs 20 mA/m²)
    assert dist["high_demand"] > dist["low_demand"]
    assert sum(dist.values()) == 100


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
