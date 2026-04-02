"""Tests for anode sizing calculator per DNV-RP-B401.

TDD: These tests were written BEFORE the implementation.
"""

import math

import pytest

from digitalmodel.cathodic_protection.anode_sizing import (
    AnodeType,
    AnodeSizingInput,
    AnodeSizingResult,
    calculate_current_demand,
    calculate_anode_mass,
    calculate_anode_resistance,
    design_cp_system,
)


# ────────────────────────────────────────────────
# Test calculate_current_demand
# ────────────────────────────────────────────────

class TestCalculateCurrentDemand:
    """Tests for current demand calculation per DNV-RP-B401 §7.4.1."""

    def test_basic_current_demand(self):
        """100 m² surface, 100 mA/m² density, 5% breakdown → 0.5 A."""
        result = calculate_current_demand(
            surface_area_m2=100.0,
            coating_breakdown_factor=0.05,
            current_density_mA_m2=100.0,
        )
        # I = 100 * 0.05 * 100 / 1000 = 0.5 A
        assert result == pytest.approx(0.5, rel=1e-6)

    def test_bare_steel_full_demand(self):
        """Bare steel (breakdown=1.0) → full current density applies."""
        result = calculate_current_demand(
            surface_area_m2=500.0,
            coating_breakdown_factor=1.0,
            current_density_mA_m2=150.0,
        )
        # I = 500 * 1.0 * 150 / 1000 = 75 A
        assert result == pytest.approx(75.0, rel=1e-6)

    def test_zero_breakdown_zero_demand(self):
        """Perfect coating (breakdown=0.0) → zero current demand."""
        result = calculate_current_demand(
            surface_area_m2=1000.0,
            coating_breakdown_factor=0.0,
            current_density_mA_m2=200.0,
        )
        assert result == pytest.approx(0.0, abs=1e-10)

    def test_large_structure(self):
        """Large offshore jacket: 5000 m², 3% breakdown, 100 mA/m²."""
        result = calculate_current_demand(
            surface_area_m2=5000.0,
            coating_breakdown_factor=0.03,
            current_density_mA_m2=100.0,
        )
        # I = 5000 * 0.03 * 100 / 1000 = 15 A
        assert result == pytest.approx(15.0, rel=1e-6)


# ────────────────────────────────────────────────
# Test calculate_anode_mass
# ────────────────────────────────────────────────

class TestCalculateAnodeMass:
    """Tests for anode mass requirement per DNV-RP-B401 §7.7.1."""

    def test_basic_mass_requirement(self):
        """10 A mean demand, 25-year life, Al-Zn-In anode, 0.90 utilization."""
        result = calculate_anode_mass(
            current_demand_A=10.0,
            design_life_years=25.0,
            utilization_factor=0.90,
            anode_capacity_Ah_kg=2000.0,
        )
        # M = (10 * 25 * 8760) / (2000 * 0.90) = 2190000 / 1800 = 1216.67 kg
        assert result == pytest.approx(1216.67, rel=0.01)

    def test_zinc_anode_heavier(self):
        """Zinc anodes (780 A-h/kg) require more mass than Al-Zn-In."""
        alzn_mass = calculate_anode_mass(
            current_demand_A=5.0,
            design_life_years=20.0,
            anode_capacity_Ah_kg=2000.0,
        )
        zn_mass = calculate_anode_mass(
            current_demand_A=5.0,
            design_life_years=20.0,
            anode_capacity_Ah_kg=780.0,
        )
        assert zn_mass > alzn_mass * 2.0

    def test_bracelet_utilization(self):
        """Bracelet anodes with 0.80 utilization need more mass than stand-off."""
        bracelet = calculate_anode_mass(
            current_demand_A=10.0,
            design_life_years=25.0,
            utilization_factor=0.80,
        )
        standoff = calculate_anode_mass(
            current_demand_A=10.0,
            design_life_years=25.0,
            utilization_factor=0.90,
        )
        assert bracelet > standoff


# ────────────────────────────────────────────────
# Test calculate_anode_resistance
# ────────────────────────────────────────────────

class TestCalculateAnodeResistance:
    """Tests for anode resistance formulas (McCoy, Dwight)."""

    def test_mccoy_standoff(self):
        """McCoy formula for slender stand-off anode in seawater."""
        result = calculate_anode_resistance(
            anode_type=AnodeType.STAND_OFF,
            length_m=1.0,
            radius_m=0.08,
            resistivity_ohm_m=0.30,
        )
        # R = (0.30 / (2*pi*1.0)) * (ln(4*1.0/0.08) - 1)
        # = 0.04775 * (ln(50) - 1) = 0.04775 * (3.912 - 1) = 0.139 ohm
        assert result > 0.0
        assert result == pytest.approx(0.139, rel=0.05)

    def test_dwight_bracelet(self):
        """Dwight formula for bracelet anode on pipeline."""
        result = calculate_anode_resistance(
            anode_type=AnodeType.BRACELET,
            length_m=0.3,
            radius_m=0.20,
            resistivity_ohm_m=0.30,
        )
        # Bracelet uses Dwight formula with different geometry
        assert result > 0.0

    def test_flush_mount_higher_resistance(self):
        """Flush-mount anodes have higher resistance (half-space radiation)."""
        flush = calculate_anode_resistance(
            anode_type=AnodeType.FLUSH_MOUNT,
            length_m=1.0,
            radius_m=0.08,
            resistivity_ohm_m=0.30,
        )
        standoff = calculate_anode_resistance(
            anode_type=AnodeType.STAND_OFF,
            length_m=1.0,
            radius_m=0.08,
            resistivity_ohm_m=0.30,
        )
        # Flush mount radiates into half-space → ~2x resistance
        assert flush > standoff

    def test_higher_resistivity_higher_resistance(self):
        """Higher electrolyte resistivity → higher anode resistance."""
        low_rho = calculate_anode_resistance(
            anode_type=AnodeType.STAND_OFF,
            length_m=1.0,
            radius_m=0.08,
            resistivity_ohm_m=0.15,
        )
        high_rho = calculate_anode_resistance(
            anode_type=AnodeType.STAND_OFF,
            length_m=1.0,
            radius_m=0.08,
            resistivity_ohm_m=0.50,
        )
        assert high_rho > low_rho


# ────────────────────────────────────────────────
# Test design_cp_system (end-to-end)
# ────────────────────────────────────────────────

class TestDesignCPSystem:
    """Tests for full CP system design workflow."""

    def test_basic_design(self):
        """Standard offshore jacket CP design with stand-off anodes."""
        inp = AnodeSizingInput(
            surface_area_m2=3000.0,
            coating_breakdown_factor=0.05,
            current_density_mA_m2=100.0,
            design_life_years=25.0,
            anode_type=AnodeType.STAND_OFF,
            anode_length_m=1.0,
            anode_radius_m=0.08,
            anode_net_mass_kg=200.0,
            resistivity_ohm_m=0.30,
            anode_capacity_Ah_kg=2000.0,
            utilization_factor=0.90,
        )
        result = design_cp_system(inp)

        assert isinstance(result, AnodeSizingResult)
        assert result.current_demand_A > 0
        assert result.total_anode_mass_kg > 0
        assert result.number_of_anodes >= 1
        assert result.anode_resistance_ohm > 0
        assert result.anode_current_output_A > 0

    def test_bracelet_pipeline_design(self):
        """Pipeline with bracelet anodes."""
        inp = AnodeSizingInput(
            surface_area_m2=9577.0,  # ~10 km, 12-inch pipeline
            coating_breakdown_factor=0.03,
            current_density_mA_m2=100.0,
            design_life_years=30.0,
            anode_type=AnodeType.BRACELET,
            anode_length_m=0.30,
            anode_radius_m=0.20,
            anode_net_mass_kg=150.0,
            resistivity_ohm_m=0.25,
            anode_capacity_Ah_kg=2000.0,
            utilization_factor=0.80,
        )
        result = design_cp_system(inp)

        # Pipeline: I = 9577 * 0.03 * 100 / 1000 = 28.73 A
        assert result.current_demand_A == pytest.approx(28.73, rel=0.01)
        assert result.number_of_anodes > 0

    def test_flush_mount_hull_design(self):
        """Ship hull with flush-mount anodes."""
        inp = AnodeSizingInput(
            surface_area_m2=5000.0,
            coating_breakdown_factor=0.02,
            current_density_mA_m2=150.0,
            design_life_years=5.0,  # drydock interval
            anode_type=AnodeType.FLUSH_MOUNT,
            anode_length_m=0.80,
            anode_radius_m=0.06,
            anode_net_mass_kg=50.0,
            resistivity_ohm_m=0.25,
            anode_capacity_Ah_kg=2000.0,
            utilization_factor=0.85,
        )
        result = design_cp_system(inp)

        # Current demand = 5000 * 0.02 * 150 / 1000 = 15 A
        assert result.current_demand_A == pytest.approx(15.0, rel=1e-6)
        assert result.number_of_anodes >= 1

    def test_driving_voltage_in_result(self):
        """Result should include driving voltage calculation."""
        inp = AnodeSizingInput(
            surface_area_m2=1000.0,
            coating_breakdown_factor=0.05,
            current_density_mA_m2=100.0,
            design_life_years=25.0,
            anode_type=AnodeType.STAND_OFF,
            anode_length_m=1.0,
            anode_radius_m=0.08,
            anode_net_mass_kg=200.0,
            resistivity_ohm_m=0.30,
        )
        result = design_cp_system(inp)

        # Driving voltage = |E_cathode - E_anode| = |(-0.800) - (-1.050)| = 0.25 V
        assert result.driving_voltage_V == pytest.approx(0.25, rel=0.01)
