# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Tests for DNV-RP-F109 on-bottom stability module.

Expected values check the implemented expressions with documented assumptions
and tolerances; synthetic cases do not establish standard qualification.
"""
from __future__ import annotations

import math

import pytest

from digitalmodel.subsea.on_bottom_stability.dnv_rp_f109 import (
    StabilityResult,
    absolute_stability_check,
    generalized_stability_check,
    hydrodynamic_force_per_meter,
    lift_force_per_meter,
    submerged_weight_per_meter,
)


# ---------------------------------------------------------------------------
# Hydrodynamic force (generic instantaneous Morison model)
# ---------------------------------------------------------------------------
class TestHydrodynamicForce:
    """Verify hydrodynamic force per generic instantaneous Morison model.

    F_H = 0.5 * rho * C_D * D * |U| * U  +  rho * C_M * (pi/4) * D^2 * a
    """

    def test_zero_velocity_and_acceleration_gives_zero(self):
        """No flow and no acceleration means no hydrodynamic force."""
        result = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=0.0, a_m_s2=0.0, C_D=0.9, C_M=3.29,
        )
        assert result == pytest.approx(0.0, abs=1e-6)

    def test_pure_drag_force(self):
        """Steady current (a=0) -- pure drag.

        F_drag = 0.5 * 1025 * 0.9 * 0.5 * 1.0 * 1.0 = 230.625 N/m
        """
        result = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=1.0, a_m_s2=0.0, C_D=0.9, C_M=3.29,
        )
        expected = 0.5 * 1025.0 * 0.9 * 0.5 * 1.0 * 1.0
        assert expected == pytest.approx(230.625, abs=0.001)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_pure_inertia_force(self):
        """No velocity (U=0) -- pure inertia.

        F_inertia = 1025 * 3.29 * (pi/4) * 0.5^2 * 1.0 = 662.18 N/m (approx)
        """
        result = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=0.0, a_m_s2=1.0, C_D=0.9, C_M=3.29,
        )
        expected = 1025.0 * 3.29 * (math.pi / 4) * 0.5**2 * 1.0
        assert result == pytest.approx(expected, rel=1e-6)
        assert result == pytest.approx(662.18, abs=0.5)

    def test_negative_velocity_reverses_drag(self):
        """Negative velocity reverses drag direction via |U|*U term."""
        result_pos = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=1.0, a_m_s2=0.0, C_D=0.9, C_M=3.29,
        )
        result_neg = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=-1.0, a_m_s2=0.0, C_D=0.9, C_M=3.29,
        )
        assert result_neg == pytest.approx(-result_pos, rel=1e-6)

    def test_combined_drag_and_inertia(self):
        """Combined drag + inertia gives sum of both terms."""
        drag = 0.5 * 1025.0 * 0.9 * 0.5 * 1.0 * 1.0
        inertia = 1025.0 * 3.29 * (math.pi / 4) * 0.5**2 * 0.5
        expected = drag + inertia
        result = hydrodynamic_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=1.0, a_m_s2=0.5, C_D=0.9, C_M=3.29,
        )
        assert result == pytest.approx(expected, rel=1e-6)


# ---------------------------------------------------------------------------
# Lift force (generic instantaneous lift model)
# ---------------------------------------------------------------------------
class TestLiftForce:
    """Verify lift force per generic instantaneous lift model.

    F_L = 0.5 * rho * C_L * D * U^2
    """

    def test_zero_velocity_gives_zero_lift(self):
        """No flow means no lift."""
        result = lift_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=0.0, C_L=0.9,
        )
        assert result == pytest.approx(0.0, abs=1e-6)

    def test_known_lift_force(self):
        """F_L = 0.5 * 1025 * 0.9 * 0.5 * 1.0^2 = 230.625 N/m."""
        result = lift_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=1.0, C_L=0.9,
        )
        expected = 0.5 * 1025.0 * 0.9 * 0.5 * 1.0**2
        assert expected == pytest.approx(230.625, abs=0.001)
        assert result == pytest.approx(expected, rel=1e-6)

    def test_lift_always_positive(self):
        """Lift is always positive regardless of velocity sign (U^2 term)."""
        result = lift_force_per_meter(
            rho_w_kg_m3=1025.0, D_outer_m=0.5, U_m_s=-2.0, C_L=0.9,
        )
        assert result > 0.0


# ---------------------------------------------------------------------------
# Submerged weight (DNV-RP-F109 clauses 1.5 and 2.3)
# ---------------------------------------------------------------------------
class TestSubmergedWeight:
    """Verify submerged weight per DNV-RP-F109 clauses 1.5 and 2.3.

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
            rho_contents_kg_m3=0.0,  # synthetic empty bore
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
# Absolute stability check (DNV-RP-F109 clause 3.6.2, Eqs. (3.38)-(3.39))
# ---------------------------------------------------------------------------
class TestAbsoluteStability:
    """Verify absolute lateral stability per DNV-RP-F109 clause 3.6.2, Eqs. (3.38)-(3.39).

    gamma_SC * (F_H + mu * F_L) / (mu * W_s) <= 1.0
    """

    def test_stable_pipe(self):
        """Heavy pipe with low loading is stable (utilisation < 1)."""
        result = absolute_stability_check(
            W_s_N_m=1000.0,
            F_H_N_m=100.0,
            F_L_N_m=50.0,
            mu_soil=0.6, gamma_SC=1.1,
        )
        assert isinstance(result, StabilityResult)
        # gamma_SC * (100 + 0.6*50) / (0.6*1000) = 1.1 * 130 / 600 = 0.2383
        expected_util = 1.1 * (100.0 + 0.6 * 50.0) / (0.6 * 1000.0)
        assert result.utilisation == pytest.approx(expected_util, rel=1e-6)
        assert result.is_stable is True

    def test_unstable_pipe(self):
        """Light pipe with high loading is unstable (utilisation > 1)."""
        result = absolute_stability_check(
            W_s_N_m=100.0,
            F_H_N_m=500.0,
            F_L_N_m=200.0,
            mu_soil=0.6, gamma_SC=1.1,
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
            mu_soil=0.6, gamma_SC=1.1,
        )
        assert result.utilisation == float("inf")
        assert result.is_stable is False

    def test_negative_submerged_weight_is_unstable(self):
        """Negative submerged weight (buoyant pipe) is always unstable."""
        result = absolute_stability_check(
            W_s_N_m=-100.0,
            F_H_N_m=100.0,
            F_L_N_m=50.0,
            mu_soil=0.6, gamma_SC=1.1,
        )
        assert result.is_stable is False

    def test_explicit_factor_increases_demand(self):
        """Explicit synthetic factors change demand without table-row selection."""
        result_lower = absolute_stability_check(
            W_s_N_m=1000.0, F_H_N_m=100.0, F_L_N_m=50.0, mu_soil=0.6, gamma_SC=1.1,
        )
        result_higher = absolute_stability_check(
            W_s_N_m=1000.0, F_H_N_m=100.0, F_L_N_m=50.0, mu_soil=0.6,
            gamma_SC=1.32,
        )
        assert result_higher.utilisation > result_lower.utilisation


@pytest.mark.parametrize("horizontal", [500.0])
@pytest.mark.parametrize("method", ["absolute", "generalized"])
def test_static_entry_points_agree_on_peak_demand(horizontal, method):
    """Both static entry points enforce the same nonnegative peak criterion."""
    kwargs = dict(
        W_s_N_m=100.0, F_H_N_m=horizontal, F_L_N_m=20.0,
        mu_soil=0.5, gamma_SC=1.0,
    )
    if method == "absolute":
        result = absolute_stability_check(**kwargs)
        expected = (500.0 + 0.5 * 20.0) / (0.5 * 100.0)
    else:
        result = generalized_stability_check(**kwargs, F_R_N_m=0.0)
        expected = (500.0 + 0.5 * 20.0) / (0.5 * 100.0)
    assert result.utilisation == pytest.approx(expected)
    assert result.is_stable is False


@pytest.mark.parametrize("horizontal", [500.0])
def test_simplified_required_weight_matches_peak_load_check(horizontal):
    """The wrapper's weight demand must agree with its canonical check."""
    from digitalmodel.subsea.on_bottom_stability import lateral_stability_check

    result = lateral_stability_check(100.0, horizontal, 20.0, 0.5, safety_factor=1.0)
    assert result.required_weight_n_per_m == pytest.approx(1020.0)
    assert not result.is_stable


def test_synthetic_unfactored_worked_example():
    """Check the synthetic case in docs/subsea/on-bottom-stability-consistency.md.

    No retrievable source supports the former page attribution. These inputs
    and coefficients define an arithmetic regression, not a sourced design case.
    """
    horizontal = hydrodynamic_force_per_meter(
        1025.0, 0.5, 1.5, 0.6, C_D=0.7, C_M=3.29,
    )
    lift = lift_force_per_meter(1025.0, 0.5, 1.5, C_L=0.9)
    friction = 0.6 * (2200.0 - lift)
    # Hand arithmetic for the synthetic model, rounded only at the final step:
    # FH = 0.5*1025*0.7*0.5*1.5^2 + 1025*3.29*pi/4*0.5^2*0.6 = 800.9 N/m.
    # FL = 0.5*1025*0.9*0.5*1.5^2 = 518.9 N/m.
    # Friction = 0.6*(2200 - 0.5*1025*0.9*0.5*1.5^2) = 1008.7 N/m.
    # Required weight = FH/0.6 + FL = 1853.7 N/m (unrounded loads).
    assert horizontal == pytest.approx(800.9, abs=0.05)
    assert lift == pytest.approx(518.9, abs=0.05)
    assert friction == pytest.approx(1008.7, abs=0.05)
    assert friction / horizontal == pytest.approx(1.26, abs=0.005)
    required = horizontal / 0.6 + lift
    assert required == pytest.approx(1853.7, abs=0.05)
    result = absolute_stability_check(2200.0, horizontal, lift, 0.6, gamma_SC=1.0)
    assert result.is_stable
    assert result.utilisation == pytest.approx(required / 2200.0)
    assert absolute_stability_check(
        required, horizontal, lift, 0.6, gamma_SC=1.0,
    ).utilisation == pytest.approx(1.0)


class TestAbsoluteStabilityMargin:
    """Check weight demand independently of the force capacity/drive ratio."""

    @pytest.mark.parametrize("horizontal", [100.0])
    @pytest.mark.parametrize("supplied", [0.0, -10.0, 200.0, 275.0, 1000.0])
    def test_weight_margin_and_boundary(self, horizontal, supplied):
        from digitalmodel.subsea.on_bottom_stability import absolute_stability_margin

        result = absolute_stability_margin(supplied, horizontal, 50.0, 0.5, gamma_SC=1.1)
        # Existing absolute equation: 1.1 * (100 / 0.5 + 50) = 275 N/m.
        assert result.required_weight_N_m == pytest.approx(275.0)
        assert result.supplied_weight_N_m == supplied
        assert result.weight_margin_ratio == pytest.approx(supplied / 275.0)
        assert result.weight_margin_N_m == pytest.approx(supplied - 275.0)
        assert result.governing_check == "absolute_lateral"
        assert result.is_stable == (supplied >= 275.0)

    def test_synthetic_weight_margin_differs_from_friction_margin(self):
        from digitalmodel.subsea.on_bottom_stability import absolute_stability_margin

        horizontal = hydrodynamic_force_per_meter(
            1025.0, 0.5, 1.5, 0.6, C_D=0.7, C_M=3.29,
        )
        lift = lift_force_per_meter(1025.0, 0.5, 1.5, C_L=0.9)
        result = absolute_stability_margin(
            2200.0, horizontal, lift, 0.6, gamma_SC=1.0,
        )
        # Same synthetic case as above, with unrounded loads throughout:
        # required = FH/0.6 + FL = 1853.7 N/m;
        # ratio = 2200/required = 1.2; reserve = 2200 - required = 346.3 N/m.
        assert result.required_weight_N_m == pytest.approx(1853.7, abs=0.05)
        assert result.weight_margin_ratio == pytest.approx(1.187, abs=0.0005)
        assert result.weight_margin_N_m == pytest.approx(346.3, abs=0.05)

    def test_lift_only_governs(self):
        from digitalmodel.subsea.on_bottom_stability import absolute_stability_margin

        result = absolute_stability_margin(100.0, 0.0, 100.0, 0.6, gamma_SC=1.2)
        assert result.required_weight_N_m == pytest.approx(120.0)
        assert result.governing_check == "uplift"
        assert not result.is_stable

    @pytest.mark.parametrize("supplied", [-10.0, 0.0, 10.0])
    def test_zero_load_has_no_weight_ratio(self, supplied):
        from digitalmodel.subsea.on_bottom_stability import absolute_stability_margin

        result = absolute_stability_margin(supplied, 0.0, 0.0, 0.6, gamma_SC=1.1)
        assert result.required_weight_N_m == 0.0
        assert result.weight_margin_ratio is None
        assert result.weight_margin_N_m == supplied
        assert result.governing_check == "no_load"
        # Preserve the check's conservative treatment of nonpositive weight.
        assert result.is_stable == (supplied > 0.0)

    @pytest.mark.parametrize("field,value", [
        ("W_s_N_m", math.nan), ("W_s_N_m", math.inf),
        ("F_H_N_m", math.inf), ("F_L_N_m", math.nan),
        ("F_L_N_m", -1.0), ("mu_soil", 0.0), ("mu_soil", -0.5),
        ("mu_soil", math.nan), ("gamma_SC", 0.0),
        ("gamma_SC", -1.0), ("gamma_SC", math.inf),
    ])
    def test_invalid_margin_inputs(self, field, value):
        from digitalmodel.subsea.on_bottom_stability import absolute_stability_margin

        kwargs = dict(W_s_N_m=100.0, F_H_N_m=10.0, F_L_N_m=5.0,
                      mu_soil=0.6, gamma_SC=1.0)
        kwargs[field] = value
        with pytest.raises(ValueError, match=field):
            absolute_stability_margin(**kwargs)


# ---------------------------------------------------------------------------
# Generalized stability check (static compatibility alias, clause 3.6.2)
# ---------------------------------------------------------------------------
class TestGeneralizedStability:
    """Verify the static compatibility alias under clause 3.6.2.

    Both clause 3.6.2 criteria are enforced; no displacement model is claimed.
    """

    def test_stable_with_soil_resistance(self):
        """Pipe stable when soil resistance augments friction."""
        result = generalized_stability_check(
            W_s_N_m=1000.0,
            F_H_N_m=200.0,
            F_L_N_m=100.0,
            mu_soil=0.6, gamma_SC=1.1,
            F_R_N_m=300.0, passive_resistance_basis="initial_penetration",
        )
        # Eq. (3.38): 1.1*(200 + 0.6*100)/(0.6*1000 + 300).
        expected_util = 1.1 * (200.0 + 0.6 * 100.0) / (0.6 * 1000.0 + 300.0)
        assert result.utilisation == pytest.approx(expected_util, rel=1e-6)
        assert result.is_stable is True

    def test_unstable_without_soil_resistance(self):
        """Without soil resistance, same pipe may be unstable."""
        result = generalized_stability_check(
            W_s_N_m=500.0,
            F_H_N_m=500.0,
            F_L_N_m=200.0,
            mu_soil=0.6, gamma_SC=1.1,
            F_R_N_m=0.0,
        )
        # Both lateral and uplift checks use the explicit synthetic factor.
        assert result.utilisation > 1.0
        assert result.is_stable is False

    def test_lift_at_weight_is_unstable_with_factor(self):
        """Factored lift plus horizontal demand exceeds the available weight."""
        result = generalized_stability_check(
            W_s_N_m=100.0,
            F_H_N_m=100.0,
            F_L_N_m=100.0,    # F_L == W_s
            mu_soil=0.6, gamma_SC=1.1,
            F_R_N_m=0.0,      # No passive resistance
        )
        # Eq. (3.38): 1.1*(100 + 0.6*100)/(0.6*100).
        assert result.utilisation == pytest.approx(176 / 60)
        assert result.is_stable is False

    def test_lift_exceeding_weight_is_unstable(self):
        """Lift force exceeding submerged weight makes pipe unstable."""
        result = generalized_stability_check(
            W_s_N_m=100.0,
            F_H_N_m=50.0,
            F_L_N_m=200.0,    # F_L > W_s
            mu_soil=0.6, gamma_SC=1.1,
            F_R_N_m=0.0,
        )
        assert result.is_stable is False
