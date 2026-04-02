# ABOUTME: Tests for FatiguePostprocessor — S-N curves, Miner's damage, stress ranges
# ABOUTME: Verifies fatigue calculations and APDL extraction commands

"""Tests for fatigue_postprocessor — FatiguePostprocessor methods."""

import math

import pytest
import pandas as pd

from digitalmodel.ansys.fatigue_postprocessor import (
    FatigueConfig,
    FatigueLoadCase,
    FatiguePostprocessor,
    FatigueResult,
    SNcurve,
)


def _fp() -> FatiguePostprocessor:
    return FatiguePostprocessor()


# ---------------------------------------------------------------------------
# S-N curve creation
# ---------------------------------------------------------------------------

class TestSNcurve:
    def test_from_dnv_d_curve(self):
        sn = SNcurve.from_dnv("D")
        assert sn.name == "D"
        assert sn.m1 == 3.0

    def test_from_dnv_f_curve(self):
        sn = SNcurve.from_dnv("F")
        assert sn.name == "F"
        assert sn.log_a1 == pytest.approx(11.855)

    def test_from_dnv_case_insensitive(self):
        sn = SNcurve.from_dnv("b1")
        assert sn.name == "B1"

    def test_unknown_curve_raises(self):
        with pytest.raises(KeyError):
            SNcurve.from_dnv("INVALID")


# ---------------------------------------------------------------------------
# Cycles to failure
# ---------------------------------------------------------------------------

class TestCyclesToFailure:
    def test_d_curve_at_100mpa(self):
        """DNV D curve at 100 MPa: log N = 12.164 - 3*log(100) = 12.164 - 6 = 6.164
        N = 10^6.164 ≈ 1.46e6"""
        sn = SNcurve.from_dnv("D")
        N = _fp().calculate_cycles_to_failure(100.0, sn)
        assert 1e6 < N < 2e6

    def test_zero_stress_returns_inf(self):
        sn = SNcurve.from_dnv("D")
        N = _fp().calculate_cycles_to_failure(0.0, sn)
        assert N == float("inf")

    def test_higher_stress_fewer_cycles(self):
        sn = SNcurve.from_dnv("D")
        N_low = _fp().calculate_cycles_to_failure(50.0, sn)
        N_high = _fp().calculate_cycles_to_failure(150.0, sn)
        assert N_high < N_low

    def test_negative_stress_returns_inf(self):
        sn = SNcurve.from_dnv("D")
        N = _fp().calculate_cycles_to_failure(-10.0, sn)
        assert N == float("inf")


# ---------------------------------------------------------------------------
# Thickness correction
# ---------------------------------------------------------------------------

class TestThicknessCorrection:
    def test_no_correction_below_reference(self):
        """Thickness <= 25mm should have no correction."""
        S = _fp().calculate_thickness_correction(100.0, 20.0, 25.0, 0.2)
        assert S == pytest.approx(100.0)

    def test_correction_increases_stress(self):
        """Thicker plates get higher effective stress range."""
        S = _fp().calculate_thickness_correction(100.0, 50.0, 25.0, 0.2)
        assert S > 100.0

    def test_known_correction(self):
        """50mm plate: factor = (50/25)^0.2 = 2^0.2 ≈ 1.149"""
        S = _fp().calculate_thickness_correction(100.0, 50.0, 25.0, 0.2)
        expected = 100.0 * (50.0 / 25.0) ** 0.2
        assert S == pytest.approx(expected)


# ---------------------------------------------------------------------------
# Fatigue damage (Miner's rule)
# ---------------------------------------------------------------------------

class TestFatigueDamage:
    def test_single_load_case_damage(self):
        config = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=1.0,
            thickness_correction=False,
        )
        result = _fp().calculate_fatigue_damage(config)
        assert result.total_damage > 0
        assert result.total_damage < 1.0  # 1e5 cycles at 100 MPa on D curve

    def test_damage_accumulates_across_cases(self):
        lc1 = FatigueLoadCase(load_case_id="LC1", stress_range_mpa=100.0, num_cycles=1e5)
        lc2 = FatigueLoadCase(load_case_id="LC2", stress_range_mpa=80.0, num_cycles=2e5)
        config = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[lc1, lc2],
            design_fatigue_factor=1.0,
            thickness_correction=False,
        )
        result = _fp().calculate_fatigue_damage(config)
        assert len(result.damage_per_case) == 2
        total = sum(result.damage_per_case.values())
        assert result.total_damage == pytest.approx(total)

    def test_dff_affects_utilization(self):
        config = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=3.0,
            thickness_correction=False,
        )
        result = _fp().calculate_fatigue_damage(config)
        # With DFF=3, allowable damage = 1/3 = 0.333
        assert result.allowable_damage == pytest.approx(1.0 / 3.0)
        # Utilization = damage / allowable
        assert result.utilization > result.total_damage

    def test_scf_increases_damage(self):
        config_no_scf = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=1.0,
            thickness_correction=False,
            scf=1.0,
        )
        config_with_scf = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=1.0,
            thickness_correction=False,
            scf=1.5,
        )
        d1 = _fp().calculate_fatigue_damage(config_no_scf).total_damage
        d2 = _fp().calculate_fatigue_damage(config_with_scf).total_damage
        assert d2 > d1

    def test_fatigue_life_years_computed(self):
        config = FatigueConfig(
            sn_curve=SNcurve.from_dnv("D"),
            load_cases=[FatigueLoadCase(stress_range_mpa=100.0, num_cycles=1e5)],
            design_fatigue_factor=1.0,
            thickness_correction=False,
        )
        result = _fp().calculate_fatigue_damage(config)
        assert result.fatigue_life_years > 0
        assert result.fatigue_life_years < float("inf")


# ---------------------------------------------------------------------------
# APDL stress range extraction
# ---------------------------------------------------------------------------

class TestStressRangeExtraction:
    def test_generates_post1(self):
        text = _fp().generate_stress_range_extraction()
        assert "/POST1" in text

    def test_generates_set_commands(self):
        text = _fp().generate_stress_range_extraction(
            load_step_pairs=[(2, 1)]
        )
        assert "SET,2" in text
        assert "SET,1" in text

    def test_generates_sadd_for_range(self):
        text = _fp().generate_stress_range_extraction(
            load_step_pairs=[(3, 1)]
        )
        assert "SADD," in text


# ---------------------------------------------------------------------------
# S-N curve APDL
# ---------------------------------------------------------------------------

class TestSNCurveAPDL:
    def test_generates_log_a_parameter(self):
        sn = SNcurve.from_dnv("D")
        text = _fp().generate_sn_curve_apdl(sn)
        assert "*SET,LOG_A1," in text

    def test_generates_slope_parameter(self):
        sn = SNcurve.from_dnv("D")
        text = _fp().generate_sn_curve_apdl(sn)
        assert "*SET,M1," in text

    def test_includes_curve_name(self):
        sn = SNcurve.from_dnv("F")
        text = _fp().generate_sn_curve_apdl(sn)
        assert "F" in text


# ---------------------------------------------------------------------------
# Damage summary DataFrame
# ---------------------------------------------------------------------------

class TestDamageSummaryDF:
    def test_returns_dataframe(self):
        results = [FatigueResult(location_id="LOC1", total_damage=0.5)]
        df = _fp().create_damage_summary_df(results)
        assert isinstance(df, pd.DataFrame)
        assert len(df) == 1

    def test_status_column(self):
        results = [
            FatigueResult(location_id="LOC1", utilization=0.5),
            FatigueResult(location_id="LOC2", utilization=1.5),
        ]
        df = _fp().create_damage_summary_df(results)
        statuses = df["status"].tolist()
        assert statuses[0] == "PASS"
        assert statuses[1] == "FAIL"

    def test_empty_results(self):
        df = _fp().create_damage_summary_df([])
        assert len(df) == 0
