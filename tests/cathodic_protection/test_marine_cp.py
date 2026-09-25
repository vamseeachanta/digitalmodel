"""Tests for marine CP assessment module.

Covers seawater current density lookup, zone-based demand calculation and
multi-zone CP design. Expected values are hand-derived from DNV-RP-B401
Table 10-1 / 10-2 step values (issue #2207); the former continuous
temperature/depth model and calcareous reduction factor are gone.

Climate by surface temperature: > 20 °C tropical, 12-20 sub-tropical,
7-<12 temperate, < 7 arctic. Depth rows: 0-30, >30-100, >100-300, >300 m.
"""

import pytest

from digitalmodel.cathodic_protection.b401_tables import DesignPhase
from digitalmodel.cathodic_protection.marine_cp import (
    MarineCPInput,
    MarineCPResult,
    Zone,
    ZoneType,
    calculate_zone_demand,
    design_marine_cp,
    get_seawater_current_density,
    seawater_current_density,
)

EDITION = "2010"  # edition whose tables the wiki holds; avoids the None warning


# ────────────────────────────────────────────────
# Test get_seawater_current_density
# ────────────────────────────────────────────────

class TestGetSeawaterCurrentDensity:
    """Tests for seawater current density lookup by temperature and depth."""

    def test_tropical_shallow_bare(self):
        """Tropical (25 °C), 0-30 m: Table 10-2 mean 0.070 A/m² = 70 mA/m²."""
        density = get_seawater_current_density(
            temperature_c=25.0,
            depth_m=10.0,
            calcareous=False,
            edition=EDITION,
        )
        assert density == pytest.approx(70.0, abs=1e-9)

    def test_cold_deep_water(self):
        """Arctic (4 °C), >100-300 m: Table 10-2 mean 0.110 A/m² = 110 mA/m²."""
        density = get_seawater_current_density(
            temperature_c=4.0,
            depth_m=300.0,
            calcareous=False,
            edition=EDITION,
        )
        assert density == pytest.approx(110.0, abs=1e-9)

    def test_calcareous_flag_has_no_effect(self):
        """B401 densities already include calcareous deposits: flag is inert."""
        bare = get_seawater_current_density(
            temperature_c=15.0, depth_m=50.0, calcareous=False, edition=EDITION
        )
        calc = get_seawater_current_density(
            temperature_c=15.0, depth_m=50.0, calcareous=True, edition=EDITION
        )
        # Sub-tropical (15 °C), >30-100 m: 0.070 A/m² = 70 mA/m²
        assert bare == pytest.approx(70.0, abs=1e-9)
        assert calc == bare

    def test_warmer_water_lower_density(self):
        """Tropical 0-30 m mean 0.070 vs arctic 0-30 m mean 0.120 A/m²."""
        warm = get_seawater_current_density(28.0, 20.0, edition=EDITION)
        cold = get_seawater_current_density(5.0, 20.0, edition=EDITION)
        assert warm == pytest.approx(70.0, abs=1e-9)
        assert cold == pytest.approx(120.0, abs=1e-9)

    def test_deeper_water_higher_density(self):
        """Sub-tropical: 0-30 m mean 0.080 vs >300 m mean 0.100 A/m²."""
        shallow = get_seawater_current_density(15.0, 10.0, edition=EDITION)
        deep = get_seawater_current_density(15.0, 500.0, edition=EDITION)
        assert shallow == pytest.approx(80.0, abs=1e-9)
        assert deep == pytest.approx(100.0, abs=1e-9)

    def test_phase_argument_selects_table(self):
        """Temperate (9 °C) 0-30 m: initial 0.200, mean 0.100, final 0.130."""
        initial = get_seawater_current_density(
            9.0, 10.0, phase=DesignPhase.INITIAL, edition=EDITION
        )
        mean = get_seawater_current_density(9.0, 10.0, edition=EDITION)
        final = get_seawater_current_density(
            9.0, 10.0, phase=DesignPhase.FINAL, edition=EDITION
        )
        assert (initial, mean, final) == pytest.approx((200.0, 100.0, 130.0))

    def test_cited_lookup(self):
        """The cited variant returns A/m² with the Table 10-2 citation."""
        cited = seawater_current_density(9.0, 10.0, edition=EDITION)
        assert cited.value == 0.100
        assert cited.units == "A/m2"
        assert cited.citation.section == "Table 10-2"


# ────────────────────────────────────────────────
# Test calculate_zone_demand
# ────────────────────────────────────────────────

class TestCalculateZoneDemand:
    """Tests for zone-based current demand calculation."""

    def test_submerged_zone(self):
        """Sub-tropical (15 °C), >30-100 m, 1000 m² bare: 1000 * 0.070 = 70 A."""
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
            edition=EDITION,
        )
        assert demand_A == pytest.approx(70.0, abs=1e-9)

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
            edition=EDITION,
        )
        assert demand_A == 0.0

    def test_tidal_zone_uses_shallow_row(self):
        """Tidal at 100 m water depth still uses the 0-30 m row (no tidal row)."""
        zone = Zone(name="tidal", zone_type=ZoneType.TIDAL, surface_area_m2=100.0)
        demand_A = calculate_zone_demand(zone, 9.0, 100.0, edition=EDITION)
        # Temperate 0-30 m mean 0.100: 100 * 0.100 = 10 A
        assert demand_A == pytest.approx(10.0, abs=1e-9)

    def test_mudline_zone(self):
        """Mudline (Sec. 6.3, 0.020 A/m²) is lower than submerged (0.070)."""
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
        sub_demand = calculate_zone_demand(sub_zone, 15.0, 50.0, edition=EDITION)
        mud_demand = calculate_zone_demand(mud_zone, 15.0, 50.0, edition=EDITION)
        # 1000 * 0.020 = 20 A
        assert mud_demand == pytest.approx(20.0, abs=1e-9)
        assert mud_demand < sub_demand

    def test_coated_zone_lower_demand(self):
        """Coated zone (5% breakdown) has 5 % of the bare demand."""
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
        bare_demand = calculate_zone_demand(bare, 15.0, 50.0, edition=EDITION)
        coated_demand = calculate_zone_demand(coated, 15.0, 50.0, edition=EDITION)
        assert coated_demand == pytest.approx(0.05 * bare_demand, abs=1e-9)


# ────────────────────────────────────────────────
# Test design_marine_cp (end-to-end)
# ────────────────────────────────────────────────

class TestDesignMarineCP:
    """End-to-end marine CP design for multi-zone structures."""

    def test_north_sea_jacket(self):
        """Typical North Sea jacket with splash, submerged, and mudline zones.

        8 °C is temperate; 100 m is the >30-100 m row: mean 0.080 A/m².
        Submerged: 3000 * 0.05 * 0.080 = 12 A; mudline: 800 * 0.020 = 16 A.
        Total 28 A; M = 28 * 25 * 8760 / (2000 * 0.90) = 3406.67 kg;
        N = ceil(3406.67 / 250) = 14.
        """
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
        result = design_marine_cp(inp, edition=EDITION)

        assert isinstance(result, MarineCPResult)
        assert result.total_current_demand_A == pytest.approx(28.0, abs=1e-6)
        assert result.total_anode_mass_kg == pytest.approx(3406.67, abs=0.01)
        assert result.number_of_anodes == 14
        assert len(result.zone_demands) == 3
        # Splash zone should contribute zero
        splash = [z for z in result.zone_demands if z["zone_name"] == "splash_zone"]
        assert splash[0]["current_demand_A"] == 0.0
        assert splash[0]["citation"] is None
        assert result.citations == [
            "dnv-rp-b401 2011 Table 10-2",
            "dnv-rp-b401 2011 Sec. 6.3 (buried surfaces)",
        ]
        assert result.edition == "2010"

    def test_tropical_monopile(self):
        """Tropical monopile: 28 °C, 25 m -> 0-30 m row mean 0.070 A/m².

        Submerged 800 * 0.03 * 0.070 = 1.68 A; mudline 200 * 0.5 * 0.020 = 2 A.
        """
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
        result = design_marine_cp(inp, edition=EDITION)
        assert result.total_current_demand_A == pytest.approx(3.68, abs=1e-6)
        # M = 3.68 * 30 * 8760 / (2000 * 0.90) = 537.28 kg -> 3 anodes
        assert result.total_anode_mass_kg == pytest.approx(537.28, abs=0.01)
        assert result.number_of_anodes == 3
