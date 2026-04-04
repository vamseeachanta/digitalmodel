"""
Tests for shallow_water.py — DNV shallow water factors, PIANC 121
bank effects, depth classification, and validation utilities.

References: DNV-RP-C205 Table 7-1, PIANC Report 121.
"""

from __future__ import annotations

from types import SimpleNamespace

import numpy as np
import pytest

from digitalmodel.hydrodynamics.parametric_hull_analysis.shallow_water import (
    dnv_shallow_water_factor,
    pianc_bank_clearance_width,
    pianc_bank_suction_force,
    validate_shallow_water_results,
)
from digitalmodel.hydrodynamics.parametric_hull_analysis.models import (
    BankEffectResult,
    BankSlopeType,
    DepthClassification,
    classify_depth,
)


# ===================================================================
# dnv_shallow_water_factor tests
# ===================================================================


class TestDnvShallowWaterFactor:
    """Tests for DNV-RP-C205 Table 7-1 correction factors."""

    def test_deep_water_returns_one(self):
        """h/T > 5 should return factor = 1.0."""
        assert dnv_shallow_water_factor(10.0, "heave") == 1.0
        assert dnv_shallow_water_factor(5.01, "surge") == 1.0
        assert dnv_shallow_water_factor(100.0, "sway") == 1.0

    def test_boundary_at_five(self):
        """h/T exactly 5.0 should NOT return 1.0 (it is <= 5)."""
        factor = dnv_shallow_water_factor(5.0, "heave")
        assert factor > 1.0

    def test_factor_greater_than_one_shallow(self):
        """All shallow water factors should be > 1.0."""
        for dof in ["heave", "surge", "sway", "roll", "pitch"]:
            factor = dnv_shallow_water_factor(2.0, dof)
            assert factor > 1.0, f"{dof} at h/T=2.0 should be > 1.0"

    def test_factor_increases_with_decreasing_depth(self):
        """Factor should increase as h/T decreases (shallower water)."""
        f_3 = dnv_shallow_water_factor(3.0, "heave")
        f_2 = dnv_shallow_water_factor(2.0, "heave")
        f_1_5 = dnv_shallow_water_factor(1.5, "heave")
        assert f_1_5 > f_2 > f_3 > 1.0

    def test_heave_strongest_amplification(self):
        """Heave should have the strongest shallow water amplification."""
        h_over_T = 2.0
        f_heave = dnv_shallow_water_factor(h_over_T, "heave")
        f_surge = dnv_shallow_water_factor(h_over_T, "surge")
        f_sway = dnv_shallow_water_factor(h_over_T, "sway")
        assert f_heave > f_surge, "Heave should amplify more than surge"
        assert f_heave > f_sway, "Heave should amplify more than sway"

    def test_very_shallow_clamped(self):
        """h/T < 1.05 is clamped to 1.05 — should not raise."""
        factor = dnv_shallow_water_factor(0.5, "heave")
        assert factor > 1.0
        assert np.isfinite(factor)

    def test_exactly_at_clamp(self):
        """h/T = 1.05 should give a finite factor."""
        factor = dnv_shallow_water_factor(1.05, "heave")
        assert np.isfinite(factor)
        assert factor > 1.0

    def test_unknown_dof_falls_back_to_heave(self):
        """Unknown DOF string falls back to heave coefficients."""
        f_unknown = dnv_shallow_water_factor(2.0, "yaw_imaginary")
        f_heave = dnv_shallow_water_factor(2.0, "heave")
        assert f_unknown == f_heave

    def test_all_dofs_produce_valid_factor(self):
        """All known DOFs produce valid float > 1.0 at h/T=2."""
        for dof in ["heave", "surge", "sway", "roll", "pitch"]:
            f = dnv_shallow_water_factor(2.0, dof)
            assert isinstance(f, float)
            assert f > 1.0

    def test_factor_formula_heave(self):
        """Verify the formula: 1 + a/(h_over_T - 1)^b for heave."""
        h_over_T = 2.0
        a, b = 0.40, 0.80  # heave coefficients from source
        expected = 1.0 + a / (h_over_T - 1.0) ** b
        actual = dnv_shallow_water_factor(h_over_T, "heave")
        assert pytest.approx(actual, rel=1e-10) == expected

    def test_factor_formula_surge(self):
        """Verify the formula for surge coefficients."""
        h_over_T = 3.0
        a, b = 0.15, 0.60  # surge coefficients
        expected = 1.0 + a / (h_over_T - 1.0) ** b
        actual = dnv_shallow_water_factor(h_over_T, "surge")
        assert pytest.approx(actual, rel=1e-10) == expected


# ===================================================================
# classify_depth tests
# ===================================================================


class TestClassifyDepth:
    """Tests for the depth classification utility."""

    def test_deep(self):
        assert classify_depth(40.0, 10.0) == DepthClassification.DEEP

    def test_medium(self):
        assert classify_depth(25.0, 10.0) == DepthClassification.MEDIUM

    def test_shallow(self):
        assert classify_depth(18.0, 10.0) == DepthClassification.SHALLOW

    def test_very_shallow(self):
        assert classify_depth(14.0, 10.0) == DepthClassification.VERY_SHALLOW

    def test_infinite_depth(self):
        assert classify_depth(float("inf"), 10.0) == DepthClassification.DEEP


# ===================================================================
# validate_shallow_water_results tests
# ===================================================================


class TestValidateShallowWaterResults:
    """Tests for BEM vs analytical comparison utility."""

    @staticmethod
    def _mock_bem_result(added_mass_value: float, n_omega: int = 10):
        """Create a mock BEM result with constant 6x6 added mass."""
        A = np.full((n_omega, 6, 6), added_mass_value)
        return SimpleNamespace(added_mass=A)

    def test_ratio_one_for_identical_results(self):
        """Same deep and shallow BEM → ratio = 1.0."""
        bem = self._mock_bem_result(500.0)
        result = validate_shallow_water_results(bem, bem, draft=10.0, water_depth=20.0)
        assert pytest.approx(result["capytaine_ratio"], rel=1e-10) == 1.0

    def test_ratio_increases_for_amplified_shallow(self):
        """Shallow added mass > deep → ratio > 1.0."""
        bem_deep = self._mock_bem_result(500.0)
        bem_shallow = self._mock_bem_result(750.0)
        result = validate_shallow_water_results(
            bem_deep, bem_shallow, draft=10.0, water_depth=20.0
        )
        assert result["capytaine_ratio"] == pytest.approx(1.5, rel=1e-10)

    def test_dnv_factor_included(self):
        """Result includes the analytical DNV factor."""
        bem = self._mock_bem_result(500.0)
        result = validate_shallow_water_results(bem, bem, draft=10.0, water_depth=20.0)
        assert "dnv_factor" in result
        assert result["dnv_factor"] > 1.0  # h/T = 2.0 → shallow

    def test_depth_class_included(self):
        """Result includes depth classification string."""
        bem = self._mock_bem_result(500.0)
        result = validate_shallow_water_results(bem, bem, draft=10.0, water_depth=20.0)
        assert result["depth_class"] == "shallow"  # h/T = 2.0

    def test_none_added_mass_returns_nan(self):
        """None added mass produces NaN ratio."""
        bem_none = SimpleNamespace(added_mass=None)
        bem_ok = self._mock_bem_result(500.0)
        result = validate_shallow_water_results(
            bem_none, bem_ok, draft=10.0, water_depth=20.0
        )
        assert np.isnan(result["capytaine_ratio"])

    def test_relative_error_computed(self):
        """Relative error is |cap_ratio - dnv_factor| / dnv_factor."""
        bem_deep = self._mock_bem_result(500.0)
        bem_shallow = self._mock_bem_result(600.0)
        result = validate_shallow_water_results(
            bem_deep, bem_shallow, draft=10.0, water_depth=20.0
        )
        expected_err = abs(result["capytaine_ratio"] - result["dnv_factor"]) / result["dnv_factor"]
        assert pytest.approx(result["relative_error"], rel=1e-10) == expected_err


# ===================================================================
# pianc_bank_suction_force tests
# ===================================================================


class TestPiancBankSuctionForce:
    """Tests for PIANC 121 bank suction force model."""

    def test_returns_bank_effect_result(self):
        """Should return a BankEffectResult dataclass."""
        result = pianc_bank_suction_force(
            speed_ms=3.0,
            water_depth=15.0,
            midship_area=200.0,
            bank_clearance=20.0,
            slope=BankSlopeType.MODERATE,
            beam=30.0,
        )
        assert isinstance(result, BankEffectResult)

    def test_zero_speed_zero_force(self):
        """Zero speed → zero lateral force and yaw moment."""
        result = pianc_bank_suction_force(
            speed_ms=0.0,
            water_depth=15.0,
            midship_area=200.0,
            bank_clearance=20.0,
            slope=BankSlopeType.MODERATE,
            beam=30.0,
        )
        assert result.lateral_force_N == pytest.approx(0.0)
        assert result.yaw_moment_Nm == pytest.approx(0.0)

    def test_force_increases_with_speed_squared(self):
        """Force proportional to V^2."""
        r1 = pianc_bank_suction_force(
            speed_ms=2.0, water_depth=15.0, midship_area=200.0,
            bank_clearance=20.0, slope=BankSlopeType.MODERATE, beam=30.0,
        )
        r2 = pianc_bank_suction_force(
            speed_ms=4.0, water_depth=15.0, midship_area=200.0,
            bank_clearance=20.0, slope=BankSlopeType.MODERATE, beam=30.0,
        )
        assert pytest.approx(r2.lateral_force_N / r1.lateral_force_N, rel=1e-10) == 4.0

    def test_force_decreases_with_bank_clearance(self):
        """Larger bank clearance → smaller suction force (exponential decay)."""
        r_close = pianc_bank_suction_force(
            speed_ms=3.0, water_depth=15.0, midship_area=200.0,
            bank_clearance=10.0, slope=BankSlopeType.MODERATE, beam=30.0,
        )
        r_far = pianc_bank_suction_force(
            speed_ms=3.0, water_depth=15.0, midship_area=200.0,
            bank_clearance=50.0, slope=BankSlopeType.MODERATE, beam=30.0,
        )
        assert r_close.lateral_force_N > r_far.lateral_force_N

    def test_steep_bank_stronger_than_gentle(self):
        """Steep bank produces more suction than gentle bank."""
        r_steep = pianc_bank_suction_force(
            speed_ms=3.0, water_depth=15.0, midship_area=200.0,
            bank_clearance=20.0, slope=BankSlopeType.STEEP, beam=30.0,
        )
        r_gentle = pianc_bank_suction_force(
            speed_ms=3.0, water_depth=15.0, midship_area=200.0,
            bank_clearance=20.0, slope=BankSlopeType.GENTLE, beam=30.0,
        )
        assert r_steep.lateral_force_N > r_gentle.lateral_force_N

    def test_yaw_moment_is_quarter_length(self):
        """Yaw moment = F_y * L / 4."""
        L = 200.0
        result = pianc_bank_suction_force(
            speed_ms=3.0, water_depth=15.0, midship_area=200.0,
            bank_clearance=20.0, slope=BankSlopeType.MODERATE,
            beam=30.0, vessel_length=L,
        )
        expected_moment = result.lateral_force_N * L / 4.0
        assert pytest.approx(result.yaw_moment_Nm, rel=1e-10) == expected_moment

    def test_force_formula_explicit(self):
        """Verify F_y = rho * V^2 * (Am/h) * a * exp(-b * y/B)."""
        V = 3.0
        h = 15.0
        Am = 200.0
        y_bank = 20.0
        B = 30.0
        rho = 1025.0
        a, b = 0.40, 1.5  # MODERATE coefficients
        f_bank = a * np.exp(-b * y_bank / B)
        expected = rho * V**2 * (Am / h) * f_bank
        result = pianc_bank_suction_force(
            speed_ms=V, water_depth=h, midship_area=Am,
            bank_clearance=y_bank, slope=BankSlopeType.MODERATE,
            beam=B, rho=rho,
        )
        assert pytest.approx(result.lateral_force_N, rel=1e-10) == expected


# ===================================================================
# pianc_bank_clearance_width tests
# ===================================================================


class TestPiancBankClearanceWidth:
    """Tests for PIANC 121 minimum bank clearance width."""

    def test_slow_gentle_zero(self):
        """Slow speed + gentle bank → 0 clearance width."""
        # Need h/T > 3 for deep to avoid amplification
        w = pianc_bank_clearance_width(
            beam=30.0, draft=5.0, water_depth=50.0,
            speed_knots=5.0, slope=BankSlopeType.GENTLE,
        )
        assert w == pytest.approx(0.0)

    def test_fast_steep_largest(self):
        """Fast speed + steep bank should give largest clearance (deep water)."""
        w = pianc_bank_clearance_width(
            beam=30.0, draft=5.0, water_depth=50.0,
            speed_knots=15.0, slope=BankSlopeType.STEEP,
        )
        assert w == pytest.approx(1.3 * 30.0)

    def test_very_shallow_amplifies_clearance(self):
        """Very shallow water multiplies clearance by 1.5."""
        # h/T = 14/10 = 1.4 → very_shallow
        w_vs = pianc_bank_clearance_width(
            beam=30.0, draft=10.0, water_depth=14.0,
            speed_knots=10.0, slope=BankSlopeType.MODERATE,
        )
        # h/T = 50/10 = 5.0 → deep
        w_deep = pianc_bank_clearance_width(
            beam=30.0, draft=10.0, water_depth=50.0,
            speed_knots=10.0, slope=BankSlopeType.MODERATE,
        )
        assert w_vs == pytest.approx(w_deep * 1.5)

    def test_shallow_amplifies_clearance(self):
        """Shallow water multiplies clearance by 1.2."""
        # h/T = 18/10 = 1.8 → shallow
        w_shallow = pianc_bank_clearance_width(
            beam=30.0, draft=10.0, water_depth=18.0,
            speed_knots=10.0, slope=BankSlopeType.MODERATE,
        )
        # deep
        w_deep = pianc_bank_clearance_width(
            beam=30.0, draft=10.0, water_depth=50.0,
            speed_knots=10.0, slope=BankSlopeType.MODERATE,
        )
        assert w_shallow == pytest.approx(w_deep * 1.2)

    def test_speed_categories(self):
        """Clearance increases with speed category: slow < moderate < fast."""
        kwargs = dict(beam=30.0, draft=5.0, water_depth=50.0, slope=BankSlopeType.MODERATE)
        w_slow = pianc_bank_clearance_width(speed_knots=5.0, **kwargs)
        w_mod = pianc_bank_clearance_width(speed_knots=10.0, **kwargs)
        w_fast = pianc_bank_clearance_width(speed_knots=15.0, **kwargs)
        assert w_slow < w_mod < w_fast
