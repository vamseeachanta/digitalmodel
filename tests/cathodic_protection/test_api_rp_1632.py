"""Tests for API RP 1632 cathodic protection calculations.

Benchmark values are hand-calculated from API RP 1632 equations.
Reference anode: Zinc, soil rho=10 ohm-m, L=1.5 m, d=0.05 m.
"""

import math

import pytest

from digitalmodel.cathodic_protection.api_rp_1632 import (
    ANODE_CAPACITY,
    ANODE_OC_POTENTIAL,
    PROTECTION_POTENTIAL_CSE,
    anode_driving_voltage,
    anode_life_years,
    anode_resistance_vertical_rod,
    check_protection_potential,
    current_demand,
    current_per_anode,
    number_of_anodes,
)

# Reference parameters for benchmark calculations
RHO = 10.0  # ohm-m
L = 1.5  # m
D = 0.05  # m


class TestAnodeDrivingVoltage:
    def test_zinc_driving_voltage(self):
        """Zinc: |-1.10 - (-0.850)| = 0.250 V."""
        result = anode_driving_voltage("zinc")
        assert result == pytest.approx(0.250, abs=1e-3)

    def test_magnesium_h1_driving_voltage(self):
        """Magnesium H-1: |-1.55 - (-0.850)| = 0.700 V."""
        result = anode_driving_voltage("magnesium_h1")
        assert result == pytest.approx(0.700, abs=1e-3)

    def test_unknown_anode_type_raises(self):
        with pytest.raises(KeyError):
            anode_driving_voltage("copper")


class TestAnodeResistanceVerticalRod:
    def test_dwight_equation_benchmark(self):
        """R_a = (10 / (2*pi*1.5)) * (ln(8*1.5/0.05) - 1).

        = 1.0610 * (ln(240) - 1) = 1.0610 * 4.4806 = 4.754 ohm.
        """
        result = anode_resistance_vertical_rod(RHO, L, D)
        expected = (RHO / (2 * math.pi * L)) * (math.log(8 * L / D) - 1)
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(4.754, abs=0.01)

    def test_higher_resistivity(self):
        """Doubling resistivity doubles resistance."""
        r1 = anode_resistance_vertical_rod(10.0, L, D)
        r2 = anode_resistance_vertical_rod(20.0, L, D)
        assert r2 == pytest.approx(2.0 * r1, rel=1e-6)


class TestCurrentDemand:
    def test_bare_steel_10m2(self):
        """10 m² bare pipe at 21.5 mA/m² -> 0.215 A."""
        result = current_demand(10.0, 21.5)
        assert result == pytest.approx(0.215, abs=1e-4)

    def test_coated_steel(self):
        """10 m² coated pipe at 0.3 mA/m² -> 0.003 A."""
        result = current_demand(10.0, 0.3)
        assert result == pytest.approx(0.003, abs=1e-6)


class TestCurrentPerAnode:
    def test_zinc_anode_benchmark(self):
        """I = 0.250 / (2 * 4.754) = 0.02629 A."""
        result = current_per_anode("zinc", RHO, L, D)
        assert result == pytest.approx(0.02629, abs=0.001)

    def test_magnesium_higher_output(self):
        """Magnesium has higher driving voltage so higher current."""
        i_zn = current_per_anode("zinc", RHO, L, D)
        i_mg = current_per_anode("magnesium_h1", RHO, L, D)
        assert i_mg > i_zn


class TestNumberOfAnodes:
    def test_benchmark_case(self):
        """10 m² bare at 21.5 mA/m², zinc anodes -> ceil(0.215/0.02629) = 9."""
        result = number_of_anodes(10.0, 21.5, "zinc", RHO, L, D)
        assert result == 9

    def test_small_area_needs_at_least_one(self):
        """Even a tiny area needs at least 1 anode."""
        result = number_of_anodes(0.01, 21.5, "zinc", RHO, L, D)
        assert result >= 1


class TestAnodeLifeYears:
    def test_zinc_1kg_benchmark(self):
        """Life = (1 * 780) / (0.02629 * 8760) = 3.39 years."""
        i_a = current_per_anode("zinc", RHO, L, D)
        result = anode_life_years(1.0, "zinc", i_a)
        assert result == pytest.approx(3.39, abs=0.05)

    def test_heavier_anode_lasts_longer(self):
        """Doubling mass doubles life."""
        i_a = current_per_anode("zinc", RHO, L, D)
        life1 = anode_life_years(1.0, "zinc", i_a)
        life2 = anode_life_years(2.0, "zinc", i_a)
        assert life2 == pytest.approx(2.0 * life1, rel=1e-6)


class TestCheckProtectionPotential:
    def test_adequately_protected(self):
        """Potential of -0.900 V is more negative than -0.850 -> pass."""
        result = check_protection_potential(-0.900)
        assert result["pass"] is True
        assert result["potential"] == -0.900
        assert result["criterion"] == PROTECTION_POTENTIAL_CSE

    def test_exactly_at_criterion(self):
        """-0.850 V exactly meets the criterion."""
        result = check_protection_potential(-0.850)
        assert result["pass"] is True

    def test_under_protected(self):
        """-0.800 V is less negative than -0.850 -> fail."""
        result = check_protection_potential(-0.800)
        assert result["pass"] is False
