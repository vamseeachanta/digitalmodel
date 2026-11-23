"""Fluids library integration tests for piping design workflows.

Reference: vamseeachanta/workspace-hub#1450
Library: https://github.com/CalebBell/fluids (v1.3.0, MIT)
"""

import math

import pytest

fluids = pytest.importorskip("fluids")

from fluids.core import Reynolds, dP_from_K
from fluids.fittings import (
    K_gate_valve_Crane,
    K_globe_valve_Crane,
    bend_rounded,
)
from fluids.friction import friction_factor, one_phase_dP
from fluids.safety_valve import API520_A_g, API520_round_size


# ---------------------------------------------------------------------------
# Friction factor tests
# ---------------------------------------------------------------------------

class TestFrictionFactor:
    """Verify Darcy friction factor against known results."""

    def test_turbulent_colebrook(self):
        """Clamond exact solution for Re=1e5, eD=1e-4 matches reference."""
        fd = friction_factor(Re=1e5, eD=1e-4)
        assert fd == pytest.approx(0.01851, rel=1e-3)

    def test_laminar_regime(self):
        """Laminar friction factor = 64/Re for Re < 2040."""
        fd = friction_factor(Re=1000)
        assert fd == pytest.approx(64 / 1000, rel=1e-6)

    def test_smooth_pipe(self):
        """Smooth pipe (eD=0) returns reasonable turbulent value."""
        fd = friction_factor(Re=1e5, eD=0)
        assert 0.01 < fd < 0.03

    def test_high_roughness_increases_friction(self):
        """Higher roughness produces higher friction factor."""
        fd_smooth = friction_factor(Re=1e5, eD=1e-5)
        fd_rough = friction_factor(Re=1e5, eD=1e-2)
        assert fd_rough > fd_smooth


# ---------------------------------------------------------------------------
# Pressure drop tests
# ---------------------------------------------------------------------------

class TestPressureDrop:
    """Pipe pressure drop for representative offshore scenario."""

    # 8-inch seawater injection line
    D = 0.2032           # 8 inch in meters
    rho = 1025.0         # seawater density kg/m3
    mu = 0.001           # dynamic viscosity Pa.s
    V = 2.0              # flow velocity m/s
    roughness = 4.57e-5  # commercial steel m
    L = 100.0            # pipe length m

    @property
    def mass_flow(self):
        A = math.pi * self.D**2 / 4
        return self.rho * self.V * A

    def test_reynolds_number(self):
        """Reynolds number for 8-inch seawater line at 2 m/s."""
        Re = Reynolds(V=self.V, D=self.D, rho=self.rho, mu=self.mu)
        assert Re == pytest.approx(416560, rel=1e-2)
        assert Re > 4000  # turbulent

    def test_pressure_drop_reasonable(self):
        """100m pipe dP should be in 10-50 kPa range for this scenario."""
        dP = one_phase_dP(
            m=self.mass_flow, rho=self.rho, mu=self.mu,
            D=self.D, roughness=self.roughness, L=self.L,
        )
        assert 10_000 < dP < 50_000  # Pa

    def test_pressure_drop_scales_with_length(self):
        """Doubling pipe length roughly doubles pressure drop."""
        dP_100 = one_phase_dP(
            m=self.mass_flow, rho=self.rho, mu=self.mu,
            D=self.D, roughness=self.roughness, L=100,
        )
        dP_200 = one_phase_dP(
            m=self.mass_flow, rho=self.rho, mu=self.mu,
            D=self.D, roughness=self.roughness, L=200,
        )
        assert dP_200 == pytest.approx(2 * dP_100, rel=0.01)


# ---------------------------------------------------------------------------
# Fitting K-factor tests
# ---------------------------------------------------------------------------

class TestFittingKFactors:
    """K-factor lookup for common piping fittings (Crane TP-410)."""

    D = 0.2032  # 8 inch

    def test_gate_valve_low_loss(self):
        """Gate valve K should be small (low resistance when fully open)."""
        K = K_gate_valve_Crane(D1=self.D, D2=self.D, angle=0.0)
        assert 0.05 < K < 0.5

    def test_globe_valve_high_loss(self):
        """Globe valve K is significantly higher than gate valve."""
        K_gate = K_gate_valve_Crane(D1=self.D, D2=self.D, angle=0.0)
        K_globe = K_globe_valve_Crane(D1=self.D, D2=self.D)
        assert K_globe > 5 * K_gate

    def test_90_degree_bend(self):
        """90-degree rounded bend K-factor is physically reasonable."""
        Re = Reynolds(V=2.0, D=self.D, rho=1025, mu=0.001)
        K = bend_rounded(Di=self.D, angle=90, rc=0.3048, Re=Re)
        assert 0.05 < K < 1.0

    def test_dp_from_k_factor(self):
        """Pressure drop from K-factor matches Bernoulli-based calc."""
        K = 4.77  # approximate globe valve
        dP = dP_from_K(K=K, rho=1025, V=2.0)
        # dP = K * 0.5 * rho * V^2
        expected = K * 0.5 * 1025 * 2.0**2
        assert dP == pytest.approx(expected, rel=1e-3)


# ---------------------------------------------------------------------------
# Relief valve sizing tests
# ---------------------------------------------------------------------------

class TestReliefValveSizing:
    """API 520 relief valve sizing for gas service."""

    def test_api520_gas_area(self):
        """Required orifice area is positive and physically reasonable."""
        A = API520_A_g(
            m=1.0, T=350, Z=0.95, MW=28.97, k=1.4,
            P1=1e6, P2=101325,
        )
        assert A > 0
        assert 1e-4 < A < 1e-2  # m2, reasonable for 1 kg/s gas

    def test_api520_round_size_larger(self):
        """Rounded standard orifice is >= required area."""
        A_required = API520_A_g(
            m=1.0, T=350, Z=0.95, MW=28.97, k=1.4,
            P1=1e6, P2=101325,
        )
        A_standard = API520_round_size(A_required)
        assert A_standard >= A_required
