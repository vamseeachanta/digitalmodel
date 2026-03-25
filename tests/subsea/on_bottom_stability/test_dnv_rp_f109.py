"""Tests for DNV-RP-F109 on-bottom stability module.

All expected values are independently calculated from the published equations
with documented assumptions and tolerances.
"""
from __future__ import annotations

import math

import pytest

from digitalmodel.subsea.on_bottom_stability.dnv_rp_f109 import (
    C_D_SMOOTH,
    C_L_SMOOTH,
    C_M_SMOOTH,
    GAMMA_SC_NORMAL,
    StabilityResult,
    absolute_stability_check,
    generalized_stability_check,
    hydrodynamic_force_per_meter,
    lift_force_per_meter,
    submerged_weight_per_meter,
)


# ---------------------------------------------------------------------------
# Hydrodynamic force (DNV-RP-F109 S3.2.1, Eq 3.1)
# ---------------------------------------------------------------------------
class TestHydrodynamicForce:
    """Verify hydrodynamic force per DNV-RP-F109 S3.2.1, Eq 3.1.

    F_H = 0.5 * rho * C_D * D * |U| * U  +  rho * C_M * (pi/4) * D^2 * a
    """

    def test_zero_velocity_and_acceleration_gives_zero(self):
        """No flow and no acceleration means no hydrodynamic force."""
        result = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=0.0, a_m_s2=0.0,
        )
        assert result == pytest.approx(0.0, abs=1e-6)

    def test_pure_drag_force(self):
        """Steady current (a=0) -- pure drag.

        F_drag = 0.5 * 1025 * 0.9 * 0.5 * 1.0 * 1.0 = 230.625 N/m
        """
        result = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=1.0, a_m_s2=0.0,
        )
        expected = 0.5 * 1025.0 * C_D_SMOOTH * 0.5 * 1.0 * 1.0
        assert expected == pytest.approx(230.625, abs=0.001)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_pure_inertia_force(self):
        """No velocity (U=0) -- pure inertia.

        F_inertia = 1025 * 3.29 * (pi/4) * 0.5^2 * 1.0 = 662.18 N/m (approx)
        """
        result = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=0.0, a_m_s2=1.0,
        )
        expected = 1025.0 * C_M_SMOOTH * (math.pi / 4) * 0.5**2 * 1.0
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(662.18, abs=0.5)

    def test_negative_velocity_reverses_drag(self):
        """Negative velocity reverses drag direction via |U|*U term."""
        result_pos = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=1.0, a_m_s2=0.0,
        )
        result_neg = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=-1.0, a_m_s2=0.0,
        )
        assert result_neg == pytest.approx(-result_pos, rel=1e-6)

    def test_combined_drag_and_inertia(self):
        """Combined drag + inertia gives sum of both terms."""
        drag = 0.5 * 1025.0 * C_D_SMOOTH * 0.5 * 1.0 * 1.0
        inertia = 1025.0 * C_M_SMOOTH * (math.pi / 4) * 0.5**2 * 0.5
        expected = drag + inertia
        result = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=1.0, a_m_s2=0.5,
        )
        assert result == pytest.approx(expected, rel=1e-6)


# ---------------------------------------------------------------------------
# Lift force (DNV-RP-F109 S3.2.1, Eq 3.2)
# ---------------------------------------------------------------------------
class TestLiftForce:
    """Verify lift force per DNV-RP-F109 S3.2.1, Eq 3.2.

    F_L = 0.5 * rho * C_L * D * U^2
    """

    def test_zero_velocity_gives_zero_lift(self):
        """No flow means no lift."""
        result = lift_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=0.0,
        )
        assert result == pytest.approx(0.0, abs=1e-6)

    def test_known_lift_force(self):
        """F_L = 0.5 * 1025 * 0.9 * 0.5 * 1.0^2 = 230.625 N/m."""
        result = lift_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=1.0,
        )
        expected = 0.5 * 1025.0 * C_L_SMOOTH * 0.5 * 1.0**2
        assert expected == pytest.approx(230.625, abs=0.001)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_lift_always_positive(self):
        """Lift is always positive regardless of velocity sign (U^2 term)."""
        result = lift_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=-2.0,
        )
        assert result > 0.0


# ---------------------------------------------------------------------------
# Submerged weight (DNV-RP-F109 S2.2)
# ---------------------------------------------------------------------------
class TestSubmergedWeight:
    """Verify submerged weight per DNV-RP-F109 S2.2.

    W_s = (W_steel + W_coat + W_contents - W_displaced) * g
    """

    def test_positive_submerged_weight_for_steel_pipe(self):
        """A typical steel pipe filled with seawater has positive submerged weight."""
        result = submerged_weight_per_meter(
            D_outer_m=0.3239,       # 12.75 inch OD
            t_wall_m=0.0127,        # 0.5 inch WT
            rho_steel_kg_m3=7850.0,
            rho_coat_kg_m3=950.0,
            t_coat_m=0.003,
            rho_contents_kg_m3=1025.0,  # seawater filled
            rho_w_kg_m3=1025.0,
        )
        assert result > 0.0

    def test_empty_thin_wall_pipe_can_be_buoyant(self):
        """An empty large-diameter thin-wall pipe may be buoyant (negative Ws)."""
        result = submerged_weight_per_meter(
            D_outer_m=1.0,
            t_wall_m=0.005,
            rho_steel_kg_m3=7850.0,
            rho_coat_kg_m3=0.0,
            t_coat_m=0.0,
            rho_contents_kg_m3=0.0,  # empty (gas or air)
            rho_w_kg_m3=1025.0,
        )
        # Large empty pipe displaces more water than its steel weight
        assert result < 0.0

    def test_weight_increases_with_heavier_contents(self):
        """Heavier contents increase submerged weight."""
        w_empty = submerged_weight_per_meter(
            D_outer_m=0.3239, t_wall_m=0.0127,
            rho_steel_kg_m3=7850.0, rho_coat_kg_m3=0.0, t_coat_m=0.0,
            rho_contents_kg_m3=0.0, rho_w_kg_m3=1025.0,
        )
        w_water = submerged_weight_per_meter(
            D_outer_m=0.3239, t_wall_m=0.0127,
            rho_steel_kg_m3=7850.0, rho_coat_kg_m3=0.0, t_coat_m=0.0,
            rho_contents_kg_m3=1025.0, rho_w_kg_m3=1025.0,
        )
        assert w_water > w_empty


# ---------------------------------------------------------------------------
# Absolute stability check (DNV-RP-F109 S4.3.1, Eq 4.1)
# ---------------------------------------------------------------------------
class TestAbsoluteStability:
    """Verify absolute lateral stability per DNV-RP-F109 S4.3.1, Eq 4.1.

    gamma_SC * (F_H + mu * F_L) / (mu * W_s) <= 1.0
    """

    def test_stable_pipe(self):
        """Heavy pipe with low loading is stable (utilisation < 1)."""
        result = absolute_stability_check(
            W_s_N_m=1000.0,
            F_H_N_m=100.0,
            F_L_N_m=50.0,
            mu_soil=0.6,
        )
        assert isinstance(result, StabilityResult)
        # gamma_SC * (100 + 0.6*50) / (0.6*1000) = 1.1 * 130 / 600 = 0.2383
        expected_util = GAMMA_SC_NORMAL * (100.0 + 0.6 * 50.0) / (0.6 * 1000.0)
        assert result.utilisation == pytest.approx(expected_util, rel=1e-6)
        assert result.is_stable is True

    def test_unstable_pipe(self):
        """Light pipe with high loading is unstable (utilisation > 1)."""
        result = absolute_stability_check(
            W_s_N_m=100.0,
            F_H_N_m=500.0,
            F_L_N_m=200.0,
            mu_soil=0.6,
        )
        # gamma_SC * (500 + 0.6*200) / (0.6*100) = 1.1 * 620 / 60 = 11.37
        assert result.utilisation > 1.0
        assert result.is_stable is False

    def test_zero_submerged_weight_gives_infinite_utilisation(self):
        """Zero submerged weight means infinite utilisation (always unstable)."""
        result = absolute_stability_check(
            W_s_N_m=0.0,
            F_H_N_m=100.0,
            F_L_N_m=50.0,
            mu_soil=0.6,
        )
        assert result.utilisation == float("inf")
        assert result.is_stable is False

    def test_negative_submerged_weight_is_unstable(self):
        """Negative submerged weight (buoyant pipe) is always unstable."""
        result = absolute_stability_check(
            W_s_N_m=-100.0,
            F_H_N_m=100.0,
            F_L_N_m=50.0,
            mu_soil=0.6,
        )
        assert result.is_stable is False

    def test_custom_gamma_sc(self):
        """Custom safety class factor changes utilisation."""
        result_normal = absolute_stability_check(
            W_s_N_m=1000.0, F_H_N_m=100.0, F_L_N_m=50.0, mu_soil=0.6,
        )
        result_high = absolute_stability_check(
            W_s_N_m=1000.0, F_H_N_m=100.0, F_L_N_m=50.0, mu_soil=0.6,
            gamma_SC=1.32,
        )
        assert result_high.utilisation > result_normal.utilisation


# ---------------------------------------------------------------------------
# Generalized stability check (DNV-RP-F109 S4.3.2, Eq 4.5)
# ---------------------------------------------------------------------------
class TestGeneralizedStability:
    """Verify generalized lateral stability per DNV-RP-F109 S4.3.2, Eq 4.5.

    gamma_SC * F_H / (mu * (W_s - F_L) + F_R) <= 1.0
    """

    def test_stable_with_soil_resistance(self):
        """Pipe stable when soil resistance augments friction."""
        result = generalized_stability_check(
            W_s_N_m=1000.0,
            F_H_N_m=200.0,
            F_L_N_m=100.0,
            mu_soil=0.6,
            F_R_N_m=300.0,
        )
        # gamma_SC * 200 / (0.6*(1000-100) + 300) = 1.1*200 / (540+300) = 220/840 = 0.2619
        expected_util = GAMMA_SC_NORMAL * 200.0 / (0.6 * (1000.0 - 100.0) + 300.0)
        assert result.utilisation == pytest.approx(expected_util, rel=1e-6)
        assert result.is_stable is True

    def test_unstable_without_soil_resistance(self):
        """Without soil resistance, same pipe may be unstable."""
        result = generalized_stability_check(
            W_s_N_m=500.0,
            F_H_N_m=500.0,
            F_L_N_m=200.0,
            mu_soil=0.6,
            F_R_N_m=0.0,
        )
        # gamma_SC * 500 / (0.6*(500-200) + 0) = 1.1*500 / 180 = 3.056
        assert result.utilisation > 1.0
        assert result.is_stable is False

    def test_zero_capacity_gives_infinite_utilisation(self):
        """When capacity denominator is zero or negative, utilisation is infinite."""
        result = generalized_stability_check(
            W_s_N_m=100.0,
            F_H_N_m=100.0,
            F_L_N_m=100.0,    # F_L == W_s
            mu_soil=0.6,
            F_R_N_m=0.0,      # No passive resistance
        )
        # denominator = 0.6*(100-100) + 0 = 0 -> infinite
        assert result.utilisation == float("inf")
        assert result.is_stable is False

    def test_lift_exceeding_weight_is_unstable(self):
        """Lift force exceeding submerged weight makes pipe unstable."""
        result = generalized_stability_check(
            W_s_N_m=100.0,
            F_H_N_m=50.0,
            F_L_N_m=200.0,    # F_L > W_s
            mu_soil=0.6,
            F_R_N_m=0.0,
        )
        assert result.is_stable is False
