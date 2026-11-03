"""Tests for fatigue damage accumulation."""

import numpy as np
import pandas as pd
import pytest
from pylife.materiallaws.woehlercurve import WoehlerCurve

from digitalmodel.fatigue.sn_curves import get_sn_curve
from digitalmodel.fatigue.damage import (
    miner_damage,
    design_life_check,
    thickness_correction,
)


@pytest.fixture
def f_curve():
    return get_sn_curve("F")


@pytest.fixture
def simple_histogram():
    return pd.DataFrame({
        "stress_range": [120.0, 100.0, 80.0, 60.0, 40.0],
        "cycles":       [1e3,   5e3,   2e4,  8e4,  5e5],
    })


class TestMinerDamage:
    @pytest.mark.parametrize("explicit_endurance", [False, True])
    def test_mixed_finite_and_perfect_endurance_bins(self, explicit_endurance):
        # Synthetic Basquin curve: N(200) = 1e6 * (200/100)^-3 = 125000;
        # Miner sum = 1/125000 + 10000/infinity = 8e-6. pyLife's
        # WoehlerCurve contract defines absent k_2 as perfect endurance.
        parameters = {"SD": 100.0, "ND": 1e6, "k_1": 3.0}
        if explicit_endurance:
            parameters["k_2"] = np.inf
        curve = WoehlerCurve(pd.Series(parameters))
        histogram = pd.DataFrame({"stress_range": [200.0, 50.0], "cycles": [1, 10000]})
        result = miner_damage(histogram, curve)
        assert result["allowable_cycles"].tolist() == [125000.0, np.inf]
        assert result["damage"].tolist() == pytest.approx([8e-6, 0.0])
        assert result.attrs["total_damage"] == pytest.approx(8e-6, rel=1e-12)

    @pytest.mark.parametrize("k_2", [5.0, np.inf])
    def test_infinity_above_endurance_limit_is_rejected(self, monkeypatch, k_2):
        curve = WoehlerCurve(pd.Series({"SD": 100.0, "ND": 1e6, "k_1": 3.0, "k_2": k_2}))
        monkeypatch.setattr(curve, "cycles", lambda stress: np.full(len(stress), np.inf))
        with pytest.raises(ValueError, match="allowable"):
            miner_damage(pd.DataFrame({"stress_range": [200.0], "cycles": [1]}), curve)

    @pytest.mark.parametrize("stress, cycles", [([0.0], [10.0]), ([100.0], [0.0])])
    def test_unloaded_histogram_has_zero_damage(self, f_curve, stress, cycles):
        result = miner_damage(pd.DataFrame({"stress_range": stress, "cycles": cycles}), f_curve)
        assert result.attrs["total_damage"] == 0.0
        assert result["damage"].tolist() == [0.0]

    def test_returns_dataframe_with_damage_columns(self, simple_histogram, f_curve):
        result = miner_damage(simple_histogram, f_curve)
        assert "allowable_cycles" in result.columns
        assert "damage" in result.columns

    def test_total_damage_attribute(self, simple_histogram, f_curve):
        result = miner_damage(simple_histogram, f_curve)
        total = result.attrs["total_damage"]
        assert total > 0
        assert abs(total - result["damage"].sum()) < 1e-10

    def test_damage_sum_reasonable(self, simple_histogram, f_curve):
        """For a moderate histogram the damage should be < 1 (safe)."""
        result = miner_damage(simple_histogram, f_curve)
        assert 0 < result.attrs["total_damage"] < 1.0

    def test_high_stress_high_damage(self, f_curve):
        """A single very high stress bin should produce large damage."""
        hist = pd.DataFrame({
            "stress_range": [200.0],
            "cycles": [1e6],
        })
        result = miner_damage(hist, f_curve)
        assert result.attrs["total_damage"] > 1.0

    def test_does_not_modify_input(self, simple_histogram, f_curve):
        original_cols = list(simple_histogram.columns)
        miner_damage(simple_histogram, f_curve)
        assert list(simple_histogram.columns) == original_cols


class TestDesignLifeCheck:
    def test_safe_design(self, simple_histogram, f_curve):
        result = design_life_check(simple_histogram, f_curve, target_years=25)
        # Moderate loading should pass for 25 years
        assert result["pass_fail"] in ("PASS", "FAIL")
        assert result["life_factor"] > 0
        assert result["years_to_failure"] > 0

    def test_annual_factor_scales_damage(self, simple_histogram, f_curve):
        r1 = design_life_check(simple_histogram, f_curve, annual_cycles_factor=1.0)
        r2 = design_life_check(simple_histogram, f_curve, annual_cycles_factor=2.0)
        assert abs(r2["damage"] - 2 * r1["damage"]) < 1e-10

    def test_pass_fail_logic(self, f_curve):
        """Very light loading should always pass."""
        hist = pd.DataFrame({
            "stress_range": [30.0],
            "cycles": [100.0],
        })
        result = design_life_check(hist, f_curve, target_years=25)
        assert result["pass_fail"] == "PASS"


class TestThicknessCorrection:
    def test_no_correction_below_ref(self):
        """Thickness <= t_ref should return unchanged stress."""
        s = np.array([100.0, 80.0])
        corrected = thickness_correction(s, t_actual=20.0, t_ref=25.0)
        np.testing.assert_array_equal(corrected, s)

    def test_correction_above_ref(self):
        """Thickness > t_ref should increase stress."""
        s = np.array([100.0])
        corrected = thickness_correction(s, t_actual=50.0, t_ref=25.0)
        expected = 100.0 * (50.0 / 25.0) ** 0.25
        np.testing.assert_allclose(corrected, [expected], rtol=1e-10)

    def test_correction_formula(self):
        """Verify exact formula: S * (t_actual / t_ref)^k."""
        s = np.array([80.0])
        t_act, t_ref, k = 40.0, 25.0, 0.20
        corrected = thickness_correction(s, t_actual=t_act, t_ref=t_ref, k=k)
        expected = 80.0 * (40.0 / 25.0) ** 0.20
        np.testing.assert_allclose(corrected, [expected], rtol=1e-10)

    def test_at_ref_thickness(self):
        """Exactly at reference thickness: no correction."""
        s = np.array([100.0])
        corrected = thickness_correction(s, t_actual=25.0, t_ref=25.0)
        np.testing.assert_array_equal(corrected, s)
