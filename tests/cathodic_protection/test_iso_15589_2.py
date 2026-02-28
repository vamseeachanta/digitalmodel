"""Tests for ISO 15589-2 offshore pipeline cathodic protection calculations.

Benchmark values are hand-calculated from ISO 15589-2 equations.
"""

import math

import pytest

from digitalmodel.cathodic_protection.iso_15589_2 import (
    ANODE_CAPACITY_ALZNI,
    ANODE_OC_POTENTIAL_ALZNI,
    ANODE_UTILIZATION_FACTOR,
    IC_COLD,
    IC_WARM,
    PROTECTION_POTENTIAL_AGAGCL,
    PROTECTION_POTENTIAL_MAX,
    anode_mass_requirement,
    anode_output_current,
    anode_resistance,
    check_protection_potential,
    coating_breakdown_factor,
    initial_current_density,
    pipeline_current_demand,
)


class TestInitialCurrentDensity:
    def test_warm_water(self):
        """T=20 degC (>15): ic = 150.0 mA/m²."""
        assert initial_current_density(20.0) == pytest.approx(150.0)

    def test_cold_water(self):
        """T=0 degC (<7): ic = 200.0 mA/m²."""
        assert initial_current_density(0.0) == pytest.approx(200.0)

    def test_interpolated_11c(self):
        """T=11 degC: 150 + 50*(15-11)/(15-7) = 150 + 25 = 175.0."""
        assert initial_current_density(11.0) == pytest.approx(175.0)

    def test_at_boundary_15c(self):
        """T=15 degC exactly -> IC_WARM = 150.0."""
        assert initial_current_density(15.0) == pytest.approx(150.0)

    def test_at_boundary_7c(self):
        """T=7 degC exactly -> IC_COLD = 200.0."""
        assert initial_current_density(7.0) == pytest.approx(200.0)


class TestCoatingBreakdownFactor:
    def test_at_time_zero(self):
        """fc(0) = fc_i = 0.10."""
        result = coating_breakdown_factor(0.10, 0.30, 0.0, 25.0)
        assert result == pytest.approx(0.10)

    def test_at_design_life(self):
        """fc(T_design) = fc_f = 0.30."""
        result = coating_breakdown_factor(0.10, 0.30, 25.0, 25.0)
        assert result == pytest.approx(0.30)

    def test_midlife(self):
        """fc(15/25) = 0.10 + 0.20*(15/25) = 0.22."""
        result = coating_breakdown_factor(0.10, 0.30, 15.0, 25.0)
        assert result == pytest.approx(0.22)


class TestPipelineCurrentDemand:
    def test_benchmark_case(self):
        """D=0.3239m, L=1000m, fc=0.15, ic=150 mA/m².

        I = pi * 0.3239 * 1000 * 0.15 * 0.150 = 22.88 A.
        """
        result = pipeline_current_demand(0.3239, 1000.0, 0.15, 150.0)
        expected = math.pi * 0.3239 * 1000.0 * 0.15 * 0.150
        assert result == pytest.approx(expected, rel=1e-4)
        assert result == pytest.approx(22.88, abs=0.1)

    def test_zero_breakdown(self):
        """fc=0 means no current demand (perfect coating)."""
        result = pipeline_current_demand(0.3239, 1000.0, 0.0, 150.0)
        assert result == pytest.approx(0.0, abs=1e-10)


class TestAnodeResistance:
    def test_benchmark_case(self):
        """rho=0.25, L_a=0.25, r_a=0.015.

        R_a = (0.25/(2*pi*0.25)) * (ln(2*0.25/0.015) - 0.5)
             = 0.1592 * (3.5066 - 0.5) = 0.1592 * 3.0066 = 0.4787 ohm.
        """
        result = anode_resistance(0.25, 0.25, 0.015)
        expected = (0.25 / (2 * math.pi * 0.25)) * (
            math.log(2 * 0.25 / 0.015) - 0.5
        )
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(0.4787, abs=0.005)


class TestAnodeOutputCurrent:
    def test_benchmark_case(self):
        """I = |(-1.050) - (-0.800)| / 0.4787 = 0.250 / 0.4787 = 0.5223 A."""
        r_a = anode_resistance(0.25, 0.25, 0.015)
        result = anode_output_current(r_a)
        assert result == pytest.approx(0.5223, abs=0.01)

    def test_custom_potentials(self):
        """Verify with explicit potential values."""
        result = anode_output_current(1.0, E_anode_V=-1.10, E_struct_V=-0.80)
        assert result == pytest.approx(0.30, abs=1e-6)


class TestAnodeMassRequirement:
    def test_benchmark_case(self):
        """I=1.0 A, T=25 yr: M = (1.0*25*8760)/(2000*0.90) = 121.67 kg."""
        result = anode_mass_requirement(1.0, 25.0)
        assert result == pytest.approx(121.67, abs=0.1)

    def test_double_current_doubles_mass(self):
        """Doubling current demand doubles required mass."""
        m1 = anode_mass_requirement(1.0, 25.0)
        m2 = anode_mass_requirement(2.0, 25.0)
        assert m2 == pytest.approx(2.0 * m1, rel=1e-6)


class TestCheckProtectionPotential:
    def test_adequately_protected(self):
        """-0.900 V is between -0.800 and -1.100 -> pass."""
        result = check_protection_potential(-0.900)
        assert result["pass"] is True
        assert result["potential"] == -0.900

    def test_under_protected(self):
        """-0.750 V is less negative than -0.800 -> fail."""
        result = check_protection_potential(-0.750)
        assert result["pass"] is False

    def test_over_protected_embrittlement_risk(self):
        """-1.150 V is more negative than -1.100 -> fail (H2 embrittlement)."""
        result = check_protection_potential(-1.150)
        assert result["pass"] is False

    def test_at_min_boundary(self):
        """-0.800 V exactly at the minimum -> pass."""
        result = check_protection_potential(-0.800)
        assert result["pass"] is True

    def test_at_max_boundary(self):
        """-1.100 V exactly at the maximum -> pass."""
        result = check_protection_potential(-1.100)
        assert result["pass"] is True
