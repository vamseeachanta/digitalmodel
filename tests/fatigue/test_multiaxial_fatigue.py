"""Tests for multiaxial_fatigue — multiaxial fatigue criteria."""

import math
import pytest

from digitalmodel.fatigue.multiaxial_fatigue import (
    StressState,
    von_mises_equivalent,
    principal_stress_range,
    findley_critical_plane,
    shear_stress_correction,
    multiaxial_damage_interaction,
)


class TestVonMisesEquivalent:
    """Test von Mises equivalent stress range."""

    def test_uniaxial_returns_same(self):
        """Pure uniaxial σx → Δσ_eq = Δσx."""
        state = StressState(sigma_x=100.0)
        result = von_mises_equivalent(state)
        assert abs(result.equivalent_stress_range - 100.0) < 0.01

    def test_pure_shear(self):
        """Pure shear τxy=100 → Δσ_eq = √3 × 100 ≈ 173.2."""
        state = StressState(tau_xy=100.0)
        result = von_mises_equivalent(state)
        expected = math.sqrt(3.0) * 100.0
        assert abs(result.equivalent_stress_range - expected) < 0.1

    def test_biaxial_equal(self):
        """Equal biaxial σx = σy = 100 → Δσ_eq = 100."""
        state = StressState(sigma_x=100.0, sigma_y=100.0)
        result = von_mises_equivalent(state)
        assert abs(result.equivalent_stress_range - 100.0) < 0.1

    def test_combined_normal_shear(self):
        """σx=100, τxy=50 → Δσ_eq = √(100² + 3×50²) ≈ 132.3."""
        state = StressState(sigma_x=100.0, tau_xy=50.0)
        result = von_mises_equivalent(state)
        expected = math.sqrt(100.0**2 + 3.0 * 50.0**2)
        assert abs(result.equivalent_stress_range - expected) < 0.1


class TestPrincipalStressRange:
    """Test maximum principal stress range method."""

    def test_uniaxial_principal_is_nominal(self):
        """Pure σx → principal = σx."""
        state = StressState(sigma_x=150.0)
        result = principal_stress_range(state)
        assert abs(result.equivalent_stress_range - 150.0) < 0.01

    def test_pure_shear_principal(self):
        """Pure shear τ=100 → σ₁ = 100, σ₂ = -100."""
        state = StressState(tau_xy=100.0)
        result = principal_stress_range(state)
        assert abs(result.principal_1 - 100.0) < 0.01
        assert abs(result.principal_2 - (-100.0)) < 0.01

    def test_principal_angle_45_for_shear(self):
        """Principal direction at 45° for pure shear."""
        state = StressState(tau_xy=100.0)
        result = principal_stress_range(state)
        assert abs(result.critical_plane_angle - 45.0) < 0.01


class TestFindleyCriticalPlane:
    """Test Findley critical plane criterion."""

    def test_uniaxial_findley(self):
        """For uniaxial stress, Findley should return a result."""
        state = StressState(sigma_x=100.0)
        result = findley_critical_plane(state, k_findley=0.3)
        assert result.equivalent_stress_range > 0
        assert result.critical_plane_angle is not None

    def test_higher_shear_increases_damage(self):
        """Adding shear should increase the equivalent stress."""
        state1 = StressState(sigma_x=100.0)
        state2 = StressState(sigma_x=100.0, tau_xy=50.0)
        r1 = findley_critical_plane(state1)
        r2 = findley_critical_plane(state2)
        assert r2.equivalent_stress_range > r1.equivalent_stress_range

    def test_findley_method_string(self):
        state = StressState(sigma_x=100.0, tau_xy=50.0)
        result = findley_critical_plane(state, k_findley=0.25)
        assert "Findley" in result.method
        assert "0.25" in result.method


class TestShearStressCorrection:
    """Test IIW shear stress correction."""

    def test_pure_normal(self):
        """Pure normal → equiv = normal stress."""
        eq = shear_stress_correction(100.0, 0.0)
        assert abs(eq - 100.0) < 0.01

    def test_combined(self):
        """σ=100, τ=50 → √(100² + 3×50²) ≈ 132.3."""
        eq = shear_stress_correction(100.0, 50.0)
        expected = math.sqrt(100.0**2 + 3.0 * 50.0**2)
        assert abs(eq - expected) < 0.1

    def test_damage_interaction_below_one_passes(self):
        """D_σ=0.3, D_τ=0.2 → 0.3² + 0.2² = 0.13 ≤ 1 → pass."""
        d = multiaxial_damage_interaction(0.3, 0.2)
        assert d < 1.0
        assert abs(d - 0.13) < 0.001
