"""Tests for marine CP assessment module.

TDD: Tests written BEFORE implementation.
Covers seawater current density lookup, zone-based demand calculation,
calcareous deposit effects, and multi-zone CP design.
"""

import pytest

from digitalmodel.cathodic_protection.marine_cp import (
    MarineCPInput,
    MarineCPResult,
    Zone,
    ZoneType,
    get_seawater_current_density,
    calculate_zone_demand,
    design_marine_cp,
)


# ────────────────────────────────────────────────
# Test get_seawater_current_density
# ────────────────────────────────────────────────

class TestGetSeawaterCurrentDensity:
    """Tests for seawater current density lookup by temperature and depth."""

    def test_tropical_shallow_bare(self):
        """Tropical shallow water (25°C, 10 m) → ~70 mA/m² mean (DNV Table 10-1)."""
        density = get_seawater_current_density(
            temperature_c=25.0,
            depth_m=10.0,
            calcareous=False,
        )
        # Tropical shallow submerged bare steel: ~70-150 mA/m²
        assert 50.0 <= density <= 200.0

    def test_cold_deep_water(self):
        """Arctic cold deep water (4°C, 300 m) → higher current density."""
        density = get_seawater_current_density(
            temperature_c=4.0,
            depth_m=300.0,
            calcareous=False,
        )
        # Cold deep water has higher current density requirement
        assert density > 100.0

    def test_calcareous_deposit_reduces_demand(self):
        """Calcareous deposits should reduce current density."""
        bare = get_seawater_current_density(
            temperature_c=15.0,
            depth_m=50.0,
            calcareous=False,
        )
        calc = get_seawater_current_density(
            temperature_c=15.0,
            depth_m=50.0,
            calcareous=True,
        )
        assert calc < bare

    def test_warmer_water_lower_density(self):
        """Warmer water typically has lower mean current density than cold."""
        warm = get_seawater_current_density(
            temperature_c=28.0,
            depth_m=20.0,
            calcareous=False,
        )
        cold = get_seawater_current_density(
            temperature_c=5.0,
            depth_m=20.0,
            calcareous=False,
        )
        assert warm < cold

    def test_deeper_water_higher_density(self):
        """Deeper water → higher hydrostatic pressure → slightly higher density."""
        shallow = get_seawater_current_density(
            temperature_c=15.0,
            depth_m=10.0,
            calcareous=False,
        )
        deep = get_seawater_current_density(
            temperature_c=15.0,
            depth_m=500.0,
            calcareous=False,
        )
        assert deep >= shallow


# ────────────────────────────────────────────────
# Test calculate_zone_demand
# ────────────────────────────────────────────────

class TestCalculateZoneDemand:
    """Tests for zone-based current demand calculation."""

    def test_submerged_zone(self):
        """Submerged zone with bare steel → current demand in Amps."""
        zone = Zone(
            name="submerged_legs",
            zone_type=ZoneType.SUBMERGED,
            surface_area_m2=1000.0,
            coating_breakdown_factor=1.0,
        )
        demand_A = calculate_zone_demand(
            zone=zone,
            temperature_c=15.0,
            depth_m=50.0,
        )
        # ~100 mA/m² * 1000 m² * 1.0 / 1000 = ~100 A
        assert demand_A > 0
        assert demand_A == pytest.approx(100.0, rel=0.3)

    def test_splash_zone_zero_demand(self):
        """Splash zone is not CP-protected → zero demand."""
        zone = Zone(
            name="splash",
            zone_type=ZoneType.SPLASH,
            surface_area_m2=500.0,
            coating_breakdown_factor=1.0,
        )
        demand_A = calculate_zone_demand(
            zone=zone,
            temperature_c=15.0,
            depth_m=0.0,
        )
        assert demand_A == pytest.approx(0.0, abs=0.01)

    def test_mudline_zone(self):
        """Mudline/buried zone has lower current density than submerged."""
        sub_zone = Zone(
            name="submerged",
            zone_type=ZoneType.SUBMERGED,
            surface_area_m2=1000.0,
            coating_breakdown_factor=1.0,
        )
        mud_zone = Zone(
            name="mudline",
            zone_type=ZoneType.MUDLINE,
            surface_area_m2=1000.0,
            coating_breakdown_factor=1.0,
        )
        sub_demand = calculate_zone_demand(sub_zone, temperature_c=15.0, depth_m=50.0)
        mud_demand = calculate_zone_demand(mud_zone, temperature_c=15.0, depth_m=50.0)
        assert mud_demand < sub_demand

    def test_coated_zone_lower_demand(self):
        """Coated zone (5% breakdown) has much lower demand than bare."""
        bare = Zone(
            name="bare",
            zone_type=ZoneType.SUBMERGED,
            surface_area_m2=1000.0,
            coating_breakdown_factor=1.0,
        )
        coated = Zone(
            name="coated",
            zone_type=ZoneType.SUBMERGED,
            surface_area_m2=1000.0,
            coating_breakdown_factor=0.05,
        )
        bare_demand = calculate_zone_demand(bare, temperature_c=15.0, depth_m=50.0)
        coated_demand = calculate_zone_demand(coated, temperature_c=15.0, depth_m=50.0)
        assert coated_demand < bare_demand * 0.10


# ────────────────────────────────────────────────
# Test design_marine_cp (end-to-end)
# ────────────────────────────────────────────────

class TestDesignMarineCP:
    """End-to-end marine CP design for multi-zone structures."""

    def test_north_sea_jacket(self):
        """Typical North Sea jacket with splash, submerged, and mudline zones."""
        inp = MarineCPInput(
            structure_name="North Sea Jacket A",
            zones=[
                Zone(
                    name="splash_zone",
                    zone_type=ZoneType.SPLASH,
                    surface_area_m2=200.0,
                    coating_breakdown_factor=1.0,
                ),
                Zone(
                    name="submerged_legs",
                    zone_type=ZoneType.SUBMERGED,
                    surface_area_m2=3000.0,
                    coating_breakdown_factor=0.05,
                ),
                Zone(
                    name="buried_piles",
                    zone_type=ZoneType.MUDLINE,
                    surface_area_m2=800.0,
                    coating_breakdown_factor=1.0,
                ),
            ],
            water_temperature_c=8.0,
            water_depth_m=100.0,
            design_life_years=25.0,
            anode_net_mass_kg=250.0,
            anode_capacity_Ah_kg=2000.0,
            utilization_factor=0.90,
        )
        result = design_marine_cp(inp)

        assert isinstance(result, MarineCPResult)
        assert result.total_current_demand_A > 0
        assert result.total_anode_mass_kg > 0
        assert result.number_of_anodes >= 1
        assert len(result.zone_demands) == 3
        # Splash zone should contribute zero
        splash = [z for z in result.zone_demands if z["zone_name"] == "splash_zone"]
        assert splash[0]["current_demand_A"] == pytest.approx(0.0, abs=0.01)

    def test_tropical_monopile(self):
        """Tropical monopile wind turbine foundation."""
        inp = MarineCPInput(
            structure_name="Tropical Monopile",
            zones=[
                Zone(
                    name="submerged",
                    zone_type=ZoneType.SUBMERGED,
                    surface_area_m2=800.0,
                    coating_breakdown_factor=0.03,
                ),
                Zone(
                    name="mudline",
                    zone_type=ZoneType.MUDLINE,
                    surface_area_m2=200.0,
                    coating_breakdown_factor=0.5,
                ),
            ],
            water_temperature_c=28.0,
            water_depth_m=25.0,
            design_life_years=30.0,
            anode_net_mass_kg=200.0,
        )
        result = design_marine_cp(inp)
        assert result.total_current_demand_A > 0
        assert result.number_of_anodes >= 1
