"""
ABOUTME: Test suite for sacrificial anode design calculations per DNV-RP-B401 (WRK-356)
ABOUTME: Covers net/gross anode mass, anode count, resistance (flush & bracelet), driving
ABOUTME: voltage, and current output — all using standalone I_c-based interface.

TDD: these tests were written BEFORE the implementation in
     cp_sacrificial_anode_b401.py.
"""

import math

import pytest

from digitalmodel.infrastructure.common.cp_sacrificial_anode_b401 import (
    anode_count,
    anode_current_output,
    anode_resistance_bracelet,
    anode_resistance_flush,
    driving_voltage,
    gross_anode_mass,
    net_anode_mass,
)


# ---------------------------------------------------------------------------
# net_anode_mass
# ---------------------------------------------------------------------------

class TestNetAnodeMass:
    """DNV-RP-B401: m_net = I_c * T * 8760 / (u * epsilon)."""

    def test_net_mass_aluminium_typical(self):
        """Known-answer check for aluminium anode, 25yr, u=0.8, epsilon=780 Ah/kg."""
        I_c = 10.0       # A
        T = 25.0         # years
        u = 0.8
        epsilon = 780.0  # Ah/kg — zinc value used for easy hand-calc
        expected = (10.0 * 25.0 * 8760.0) / (0.8 * 780.0)
        result = net_anode_mass(I_c=I_c, T=T, u=u, epsilon=epsilon)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_net_mass_aluminium_2000_capacity(self):
        """Aluminium anode (epsilon=2000 Ah/kg), 25yr design life."""
        I_c = 5.0
        T = 25.0
        u = 0.8
        epsilon = 2000.0
        expected = (5.0 * 25.0 * 8760.0) / (0.8 * 2000.0)
        result = net_anode_mass(I_c=I_c, T=T, u=u, epsilon=epsilon)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_net_mass_scales_linearly_with_current(self):
        """Doubling I_c should double net mass."""
        base = net_anode_mass(I_c=5.0, T=25.0, u=0.8, epsilon=780.0)
        double = net_anode_mass(I_c=10.0, T=25.0, u=0.8, epsilon=780.0)
        assert double == pytest.approx(2.0 * base, rel=1e-9)

    def test_net_mass_scales_linearly_with_design_life(self):
        """Doubling T should double net mass."""
        base = net_anode_mass(I_c=10.0, T=10.0, u=0.8, epsilon=780.0)
        double = net_anode_mass(I_c=10.0, T=20.0, u=0.8, epsilon=780.0)
        assert double == pytest.approx(2.0 * base, rel=1e-9)

    def test_net_mass_positive_for_positive_inputs(self):
        """Net mass must always be positive for valid inputs."""
        result = net_anode_mass(I_c=1.0, T=1.0, u=0.8, epsilon=780.0)
        assert result > 0.0

    def test_net_mass_zero_current_raises(self):
        """I_c = 0 is non-physical; raise ValueError."""
        with pytest.raises(ValueError, match="I_c"):
            net_anode_mass(I_c=0.0, T=25.0, u=0.8, epsilon=780.0)

    def test_net_mass_negative_current_raises(self):
        """Negative current is non-physical; raise ValueError."""
        with pytest.raises(ValueError, match="I_c"):
            net_anode_mass(I_c=-5.0, T=25.0, u=0.8, epsilon=780.0)

    def test_net_mass_zero_design_life_raises(self):
        """T = 0 yields infinite anode need; raise ValueError."""
        with pytest.raises(ValueError, match="T"):
            net_anode_mass(I_c=10.0, T=0.0, u=0.8, epsilon=780.0)

    def test_net_mass_invalid_utilization_zero_raises(self):
        """u = 0 causes division by zero; raise ValueError."""
        with pytest.raises(ValueError, match="u"):
            net_anode_mass(I_c=10.0, T=25.0, u=0.0, epsilon=780.0)

    def test_net_mass_utilization_above_one_raises(self):
        """u > 1.0 is non-physical; raise ValueError."""
        with pytest.raises(ValueError, match="u"):
            net_anode_mass(I_c=10.0, T=25.0, u=1.1, epsilon=780.0)

    def test_net_mass_zero_capacity_raises(self):
        """epsilon = 0 causes division by zero; raise ValueError."""
        with pytest.raises(ValueError, match="epsilon"):
            net_anode_mass(I_c=10.0, T=25.0, u=0.8, epsilon=0.0)


# ---------------------------------------------------------------------------
# gross_anode_mass
# ---------------------------------------------------------------------------

class TestGrossAnodeMass:
    """m_gross = m_net / u."""

    def test_gross_mass_equals_net_over_u(self):
        """gross = net / u by definition."""
        m_net_val = 1000.0
        u = 0.8
        result = gross_anode_mass(m_net=m_net_val, u=u)
        assert result == pytest.approx(m_net_val / u, rel=1e-9)

    def test_gross_exceeds_net(self):
        """Gross mass must always exceed net mass (u < 1)."""
        m_net_val = 500.0
        u = 0.75
        assert gross_anode_mass(m_net=m_net_val, u=u) > m_net_val

    def test_gross_mass_u_equals_one_returns_net(self):
        """When u=1.0, gross mass equals net mass."""
        m_net_val = 300.0
        result = gross_anode_mass(m_net=m_net_val, u=1.0)
        assert result == pytest.approx(m_net_val, rel=1e-9)

    def test_gross_mass_zero_net_raises(self):
        """m_net = 0 is non-physical; raise ValueError."""
        with pytest.raises(ValueError, match="m_net"):
            gross_anode_mass(m_net=0.0, u=0.8)

    def test_gross_mass_zero_u_raises(self):
        """u = 0 causes division by zero; raise ValueError."""
        with pytest.raises(ValueError, match="u"):
            gross_anode_mass(m_net=100.0, u=0.0)


# ---------------------------------------------------------------------------
# anode_count
# ---------------------------------------------------------------------------

class TestAnodeCount:
    """N = ceil(m_gross / m_anode)."""

    def test_anode_count_exact_division(self):
        """When m_gross is exact multiple of m_anode, count is exact."""
        result = anode_count(m_gross=1000.0, m_anode=200.0)
        assert result == 5

    def test_anode_count_rounds_up(self):
        """Fractional anode count rounds up (ceiling division)."""
        result = anode_count(m_gross=201.0, m_anode=200.0)
        assert result == 2

    def test_anode_count_minimum_one(self):
        """Even tiny mass requirement yields at least 1 anode."""
        result = anode_count(m_gross=0.001, m_anode=200.0)
        assert result == 1

    def test_anode_count_large_platform(self):
        """Large platform with many anodes."""
        result = anode_count(m_gross=10000.0, m_anode=200.0)
        assert result == 50

    def test_anode_count_returns_integer(self):
        """Return type must be int."""
        result = anode_count(m_gross=600.0, m_anode=200.0)
        assert isinstance(result, int)

    def test_anode_count_zero_gross_raises(self):
        """m_gross = 0 is non-physical; raise ValueError."""
        with pytest.raises(ValueError, match="m_gross"):
            anode_count(m_gross=0.0, m_anode=200.0)

    def test_anode_count_zero_anode_mass_raises(self):
        """m_anode = 0 causes division by zero; raise ValueError."""
        with pytest.raises(ValueError, match="m_anode"):
            anode_count(m_gross=1000.0, m_anode=0.0)


# ---------------------------------------------------------------------------
# anode_resistance_flush (slender flush-mounted / stand-off)
# ---------------------------------------------------------------------------

class TestAnodeResistanceFlush:
    """DNV-RP-B401 Dwight formula: R_a = (rho/(2*pi*L)) * (ln(2L/r) - 1).

    Note: the task spec quotes ln(2L/r) - 1 for the flush/stand-off formula.
    The B401-2021 implementation uses ln(4L/r) - 1.  This new function
    implements the simpler ln(2L/r) - 1 variant explicitly requested.
    """

    def test_flush_resistance_known_values(self):
        """Hand-calculated check: rho=0.3, L=1.0, r=0.05."""
        rho = 0.3
        L = 1.0
        r = 0.05
        expected = (rho / (2.0 * math.pi * L)) * (math.log(2.0 * L / r) - 1.0)
        result = anode_resistance_flush(rho=rho, L=L, r=r)
        assert result == pytest.approx(expected, rel=1e-9)

    def test_flush_resistance_positive(self):
        """Resistance must always be positive for valid geometry."""
        result = anode_resistance_flush(rho=0.3, L=2.0, r=0.05)
        assert result > 0.0

    def test_flush_resistance_increases_with_resistivity(self):
        """Higher seawater resistivity increases anode resistance."""
        r_low = anode_resistance_flush(rho=0.25, L=1.0, r=0.05)
        r_high = anode_resistance_flush(rho=0.35, L=1.0, r=0.05)
        assert r_high > r_low

    def test_flush_resistance_decreases_with_length(self):
        """Longer anode has lower resistance (more current path area)."""
        r_short = anode_resistance_flush(rho=0.3, L=1.0, r=0.05)
        r_long = anode_resistance_flush(rho=0.3, L=2.0, r=0.05)
        assert r_long < r_short

    def test_flush_resistance_zero_rho_raises(self):
        """rho = 0 is non-physical; raise ValueError."""
        with pytest.raises(ValueError, match="rho"):
            anode_resistance_flush(rho=0.0, L=1.0, r=0.05)

    def test_flush_resistance_zero_length_raises(self):
        """L = 0 causes division by zero; raise ValueError."""
        with pytest.raises(ValueError, match="L"):
            anode_resistance_flush(rho=0.3, L=0.0, r=0.05)

    def test_flush_resistance_zero_radius_raises(self):
        """r = 0 causes ln(0) = -inf; raise ValueError."""
        with pytest.raises(ValueError, match="r"):
            anode_resistance_flush(rho=0.3, L=1.0, r=0.0)

    def test_flush_resistance_stubby_geometry_raises(self):
        """2L/r <= e geometry is invalid for Dwight formula; raise ValueError."""
        # 2L/r = 2*0.05/0.10 = 1.0 < e — invalid
        with pytest.raises(ValueError, match="geometry"):
            anode_resistance_flush(rho=0.3, L=0.05, r=0.10)


# ---------------------------------------------------------------------------
# anode_resistance_bracelet
# ---------------------------------------------------------------------------

class TestAnodeResistanceBracelet:
    """Modified bracelet formula: R_a = 0.315 * rho / sqrt(A)."""

    def test_bracelet_resistance_known_values(self):
        """Hand-calculated check: rho=0.3, A=0.5 m2."""
        rho = 0.3
        A = 0.5
        expected = 0.315 * rho / math.sqrt(A)
        result = anode_resistance_bracelet(rho=rho, A=A)
        assert result == pytest.approx(expected, rel=1e-9)

    def test_bracelet_resistance_positive(self):
        """Resistance must always be positive."""
        result = anode_resistance_bracelet(rho=0.3, A=0.2)
        assert result > 0.0

    def test_bracelet_resistance_increases_with_resistivity(self):
        """Higher resistivity increases resistance."""
        r_low = anode_resistance_bracelet(rho=0.25, A=0.3)
        r_high = anode_resistance_bracelet(rho=0.35, A=0.3)
        assert r_high > r_low

    def test_bracelet_resistance_decreases_with_area(self):
        """Larger exposed area reduces resistance."""
        r_small = anode_resistance_bracelet(rho=0.3, A=0.1)
        r_large = anode_resistance_bracelet(rho=0.3, A=0.5)
        assert r_large < r_small

    def test_bracelet_resistance_zero_rho_raises(self):
        """rho = 0 is non-physical; raise ValueError."""
        with pytest.raises(ValueError, match="rho"):
            anode_resistance_bracelet(rho=0.0, A=0.3)

    def test_bracelet_resistance_zero_area_raises(self):
        """A = 0 causes division by zero; raise ValueError."""
        with pytest.raises(ValueError, match="A"):
            anode_resistance_bracelet(rho=0.3, A=0.0)

    def test_bracelet_resistance_negative_area_raises(self):
        """Negative exposed area is non-physical; raise ValueError."""
        with pytest.raises(ValueError, match="A"):
            anode_resistance_bracelet(rho=0.3, A=-0.1)


# ---------------------------------------------------------------------------
# driving_voltage
# ---------------------------------------------------------------------------

class TestDrivingVoltage:
    """E_drive = E_c - E_a (protection potential minus anode open-circuit potential)."""

    def test_driving_voltage_standard_aluminium(self):
        """Standard seawater: E_c=-0.80V, E_a=-1.05V → drive=0.25V."""
        result = driving_voltage(E_c=-0.80, E_a=-1.05)
        assert result == pytest.approx(0.25, rel=1e-9)

    def test_driving_voltage_standard_zinc(self):
        """Zinc anode: E_c=-0.80V, E_a=-1.00V → drive=0.20V."""
        result = driving_voltage(E_c=-0.80, E_a=-1.00)
        assert result == pytest.approx(0.20, rel=1e-9)

    def test_driving_voltage_positive_when_anode_more_negative(self):
        """Anode must be more negative than structure for protection to work."""
        result = driving_voltage(E_c=-0.80, E_a=-1.05)
        assert result > 0.0

    def test_driving_voltage_zero_when_equal_raises(self):
        """E_c == E_a yields zero driving force — no protection; raise ValueError."""
        with pytest.raises(ValueError, match="driving voltage"):
            driving_voltage(E_c=-0.80, E_a=-0.80)

    def test_driving_voltage_negative_raises(self):
        """E_a less negative than E_c reverses current — invalid; raise ValueError."""
        with pytest.raises(ValueError, match="driving voltage"):
            driving_voltage(E_c=-0.80, E_a=-0.50)


# ---------------------------------------------------------------------------
# anode_current_output
# ---------------------------------------------------------------------------

class TestAnodeCurrentOutput:
    """I_a = (E_c - E_a) / R_a (Ohm's law for galvanic anode circuit)."""

    def test_current_output_ohms_law(self):
        """Basic Ohm's law check: 0.25 V / 0.05 Ω = 5 A."""
        result = anode_current_output(E_c=-0.80, E_a=-1.05, R_a=0.05)
        assert result == pytest.approx(0.25 / 0.05, rel=1e-9)

    def test_current_output_positive_for_valid_inputs(self):
        """Current output must always be positive when anode is more negative."""
        result = anode_current_output(E_c=-0.80, E_a=-1.05, R_a=0.1)
        assert result > 0.0

    def test_current_output_doubles_with_halved_resistance(self):
        """Halving R_a doubles I_a for fixed driving voltage."""
        I_high_R = anode_current_output(E_c=-0.80, E_a=-1.05, R_a=0.10)
        I_low_R = anode_current_output(E_c=-0.80, E_a=-1.05, R_a=0.05)
        assert I_low_R == pytest.approx(2.0 * I_high_R, rel=1e-9)

    def test_current_output_zero_resistance_raises(self):
        """R_a = 0 causes division by zero; raise ValueError."""
        with pytest.raises(ValueError, match="R_a"):
            anode_current_output(E_c=-0.80, E_a=-1.05, R_a=0.0)

    def test_current_output_negative_resistance_raises(self):
        """Negative resistance is non-physical; raise ValueError."""
        with pytest.raises(ValueError, match="R_a"):
            anode_current_output(E_c=-0.80, E_a=-1.05, R_a=-0.1)

    def test_current_output_invalid_potentials_raises(self):
        """E_a >= E_c raises ValueError via driving_voltage check."""
        with pytest.raises(ValueError, match="driving voltage"):
            anode_current_output(E_c=-0.80, E_a=-0.50, R_a=0.05)


# ---------------------------------------------------------------------------
# Integration — end-to-end DNV-RP-B401 sacrificial anode design workflow
# ---------------------------------------------------------------------------

class TestSacrificialAnodeWorkflowIntegration:
    """
    End-to-end integration: current demand → mass → count → resistance → output.

    Reference scenario (hand-calculated):
      I_c = 15 A,  T = 25 yr,  u = 0.8,  epsilon = 780 Ah/kg (zinc)
      m_net = 15 * 25 * 8760 / (0.8 * 780) = 5244.230... kg
      m_gross = m_net / 0.8 = 6555.288... kg
      N = ceil(6555.288 / 300) = ceil(21.851) = 22 anodes

      Bracelet anode: rho=0.30, A=0.25 m2
      R_a = 0.315 * 0.30 / sqrt(0.25) = 0.315 * 0.30 / 0.5 = 0.189 Ω

      E_c=-0.80V, E_a=-1.00V (zinc OCP)
      driving = 0.20 V
      I_a = 0.20 / 0.189 = 1.058... A/anode
    """

    # Reference parameters
    I_c = 15.0       # A
    T = 25.0         # years
    u = 0.8
    epsilon = 780.0  # Ah/kg (zinc)
    m_anode = 300.0  # kg per anode

    rho = 0.30       # ohm·m seawater resistivity
    A_bracelet = 0.25  # m2 exposed area

    E_c = -0.80      # V vs SCE
    E_a = -1.00      # V vs SCE (zinc)

    def test_integration_net_mass(self):
        m_net = net_anode_mass(I_c=self.I_c, T=self.T, u=self.u, epsilon=self.epsilon)
        expected = self.I_c * self.T * 8760.0 / (self.u * self.epsilon)
        assert m_net == pytest.approx(expected, rel=1e-6)

    def test_integration_gross_mass(self):
        m_net = net_anode_mass(I_c=self.I_c, T=self.T, u=self.u, epsilon=self.epsilon)
        m_gross = gross_anode_mass(m_net=m_net, u=self.u)
        assert m_gross == pytest.approx(m_net / self.u, rel=1e-6)

    def test_integration_anode_count(self):
        m_net = net_anode_mass(I_c=self.I_c, T=self.T, u=self.u, epsilon=self.epsilon)
        m_gross = gross_anode_mass(m_net=m_net, u=self.u)
        N = anode_count(m_gross=m_gross, m_anode=self.m_anode)
        # m_gross / 300 = 21.85... → ceil = 22
        assert N == 22

    def test_integration_bracelet_resistance(self):
        R_a = anode_resistance_bracelet(rho=self.rho, A=self.A_bracelet)
        expected = 0.315 * self.rho / math.sqrt(self.A_bracelet)
        assert R_a == pytest.approx(expected, rel=1e-9)

    def test_integration_current_output(self):
        R_a = anode_resistance_bracelet(rho=self.rho, A=self.A_bracelet)
        I_a = anode_current_output(E_c=self.E_c, E_a=self.E_a, R_a=R_a)
        dV = driving_voltage(E_c=self.E_c, E_a=self.E_a)
        assert I_a == pytest.approx(dV / R_a, rel=1e-9)

    def test_integration_total_output_sufficient(self):
        """Total anode current output must exceed or meet protective current demand."""
        m_net = net_anode_mass(I_c=self.I_c, T=self.T, u=self.u, epsilon=self.epsilon)
        m_gross = gross_anode_mass(m_net=m_net, u=self.u)
        N = anode_count(m_gross=m_gross, m_anode=self.m_anode)
        R_a = anode_resistance_bracelet(rho=self.rho, A=self.A_bracelet)
        I_a = anode_current_output(E_c=self.E_c, E_a=self.E_a, R_a=R_a)
        total_output = N * I_a
        # Adequate if total current output meets protective current
        assert total_output >= self.I_c
