# ABOUTME: TDD tests for production test quality scoring
# ABOUTME: Covers stabilization, duration, drawdown, separator, gas lift criteria

"""Tests for production_engineering.test_quality_scorer module."""

import pytest

from digitalmodel.production_engineering.test_quality_scorer import (
    ProductionTestQualityScorer,
    ProductionTestRecord,
    QualityFlag,
    WellType,
)


# ---------------------------------------------------------------------------
# Fixtures / helpers
# ---------------------------------------------------------------------------

def make_flowing_test(**overrides) -> ProductionTestRecord:
    """Good-quality flowing well test (should score Green)."""
    defaults = dict(
        well_id="WELL-001",
        test_date="2026-01-15",
        well_type=WellType.FLOWING,
        duration_hours=8.0,
        oil_rate_bopd=500.0,
        gas_rate_mscfd=250.0,
        water_rate_bwpd=100.0,
        flowing_wellhead_pressure_psi=800.0,
        separator_pressure_psi=200.0,
        static_wellhead_pressure_psi=1200.0,
        rate_start_bopd=495.0,
        rate_end_bopd=505.0,
    )
    defaults.update(overrides)
    return ProductionTestRecord(**defaults)


def make_gas_lift_test(**overrides) -> ProductionTestRecord:
    """Good-quality gas lift well test (should score Green)."""
    defaults = dict(
        well_id="WELL-GL-001",
        test_date="2026-01-16",
        well_type=WellType.GAS_LIFT,
        duration_hours=10.0,
        oil_rate_bopd=800.0,
        gas_rate_mscfd=600.0,
        water_rate_bwpd=400.0,
        flowing_wellhead_pressure_psi=500.0,
        separator_pressure_psi=100.0,
        static_wellhead_pressure_psi=900.0,
        rate_start_bopd=795.0,
        rate_end_bopd=805.0,
        gas_lift_rate_start_mscfd=1000.0,
        gas_lift_rate_end_mscfd=1005.0,
    )
    defaults.update(overrides)
    return ProductionTestRecord(**defaults)


scorer = ProductionTestQualityScorer()


# ---------------------------------------------------------------------------
# ProductionTestRecord properties
# ---------------------------------------------------------------------------

class TestProductionTestRecordProperties:

    def test_liquid_rate_sums_oil_and_water(self):
        test = make_flowing_test(oil_rate_bopd=500, water_rate_bwpd=100)
        assert test.liquid_rate_blpd == 600.0

    def test_watercut_fraction(self):
        test = make_flowing_test(oil_rate_bopd=400, water_rate_bwpd=600)
        assert test.watercut == pytest.approx(0.6)

    def test_watercut_zero_when_no_liquid(self):
        test = make_flowing_test(oil_rate_bopd=0, water_rate_bwpd=0)
        assert test.watercut == 0.0

    def test_gor_calculation(self):
        test = make_flowing_test(oil_rate_bopd=500, gas_rate_mscfd=250)
        # 250,000 scf/d / 500 bbl/d = 500 scf/bbl
        assert test.gor_scf_per_bbl == pytest.approx(500.0)

    def test_gor_none_when_zero_oil(self):
        test = make_flowing_test(oil_rate_bopd=0, gas_rate_mscfd=100)
        assert test.gor_scf_per_bbl is None


# ---------------------------------------------------------------------------
# Duration check
# ---------------------------------------------------------------------------

class TestDurationCheck:

    def test_flowing_well_meets_minimum_duration(self):
        test = make_flowing_test(duration_hours=6.0)  # min = 4h
        result = scorer.score(test)
        assert QualityFlag.TEST_DURATION_SHORT not in result.flags

    def test_flowing_well_below_minimum_duration_flagged(self):
        test = make_flowing_test(duration_hours=3.0)  # min = 4h
        result = scorer.score(test)
        assert QualityFlag.TEST_DURATION_SHORT in result.flags

    def test_gas_lift_minimum_duration_is_longer(self):
        test = make_gas_lift_test(duration_hours=6.0)  # min = 8h
        result = scorer.score(test)
        assert QualityFlag.TEST_DURATION_SHORT in result.flags

    def test_esp_minimum_duration(self):
        test = make_flowing_test(
            well_type=WellType.ESP, duration_hours=5.0
        )  # min = 6h
        result = scorer.score(test)
        assert QualityFlag.TEST_DURATION_SHORT in result.flags

    def test_rod_pump_minimum_duration_12h(self):
        test = make_flowing_test(
            well_type=WellType.ROD_PUMP, duration_hours=10.0
        )  # min = 12h
        result = scorer.score(test)
        assert QualityFlag.TEST_DURATION_SHORT in result.flags


# ---------------------------------------------------------------------------
# Stabilization check
# ---------------------------------------------------------------------------

class TestStabilizationCheck:

    def test_stable_rate_no_flag(self):
        test = make_flowing_test(rate_start_bopd=490, rate_end_bopd=510)
        result = scorer.score(test)
        assert QualityFlag.STABILIZATION_INSUFFICIENT not in result.flags
        assert QualityFlag.RATE_UNSTABLE not in result.flags

    def test_unstable_rate_flagged(self):
        test = make_flowing_test(rate_start_bopd=300, rate_end_bopd=700)
        result = scorer.score(test)
        assert (
            QualityFlag.STABILIZATION_INSUFFICIENT in result.flags
            or QualityFlag.RATE_UNSTABLE in result.flags
        )

    def test_no_rate_endpoints_partial_credit(self):
        test = make_flowing_test(rate_start_bopd=None, rate_end_bopd=None)
        result_no_endpoints = scorer.score(test)
        test_stable = make_flowing_test(rate_start_bopd=495, rate_end_bopd=505)
        result_stable = scorer.score(test_stable)
        assert result_stable.score > result_no_endpoints.score


# ---------------------------------------------------------------------------
# Pressure drawdown check
# ---------------------------------------------------------------------------

class TestPressureDrawdownCheck:

    def test_adequate_drawdown_no_flag(self):
        test = make_flowing_test(
            static_wellhead_pressure_psi=1200, flowing_wellhead_pressure_psi=800
        )
        result = scorer.score(test)
        assert QualityFlag.PRESSURE_DRAWDOWN_LOW not in result.flags

    def test_low_drawdown_flagged(self):
        test = make_flowing_test(
            static_wellhead_pressure_psi=1000, flowing_wellhead_pressure_psi=980
        )
        result = scorer.score(test)
        assert QualityFlag.PRESSURE_DRAWDOWN_LOW in result.flags

    def test_missing_static_pressure_flagged(self):
        test = make_flowing_test(static_wellhead_pressure_psi=None)
        result = scorer.score(test)
        assert QualityFlag.NO_STATIC_PRESSURE in result.flags


# ---------------------------------------------------------------------------
# Separator back-pressure check
# ---------------------------------------------------------------------------

class TestSeparatorCheck:

    def test_low_separator_pressure_no_flag(self):
        test = make_flowing_test(
            flowing_wellhead_pressure_psi=800, separator_pressure_psi=200
        )
        result = scorer.score(test)
        assert QualityFlag.SEPARATOR_BACK_PRESSURE_HIGH not in result.flags

    def test_high_separator_pressure_flagged(self):
        test = make_flowing_test(
            flowing_wellhead_pressure_psi=500, separator_pressure_psi=450
        )
        result = scorer.score(test)
        assert QualityFlag.SEPARATOR_BACK_PRESSURE_HIGH in result.flags


# ---------------------------------------------------------------------------
# Gas lift check
# ---------------------------------------------------------------------------

class TestGasLiftCheck:

    def test_stable_gas_lift_no_flag(self):
        test = make_gas_lift_test(
            gas_lift_rate_start_mscfd=1000, gas_lift_rate_end_mscfd=1005
        )
        result = scorer.score(test)
        assert QualityFlag.GAS_LIFT_UNSTABLE not in result.flags

    def test_unstable_gas_lift_flagged(self):
        test = make_gas_lift_test(
            gas_lift_rate_start_mscfd=800, gas_lift_rate_end_mscfd=1400
        )
        result = scorer.score(test)
        assert QualityFlag.GAS_LIFT_UNSTABLE in result.flags

    def test_no_gas_lift_data_flagged(self):
        test = make_gas_lift_test(
            gas_lift_rate_start_mscfd=None, gas_lift_rate_end_mscfd=None
        )
        result = scorer.score(test)
        assert QualityFlag.GAS_LIFT_UNSTABLE in result.flags

    def test_flowing_well_not_penalised_for_gas_lift(self):
        test_flowing = make_flowing_test()
        result = scorer.score(test_flowing)
        assert QualityFlag.GAS_LIFT_UNSTABLE not in result.flags


# ---------------------------------------------------------------------------
# Confidence classification
# ---------------------------------------------------------------------------

class TestConfidenceClassification:

    def test_excellent_test_is_green(self):
        test = make_flowing_test()
        result = scorer.score(test)
        assert result.confidence == "Green"
        assert result.score >= 80

    def test_poor_test_is_red(self):
        # Short duration, high separator pressure, low drawdown, unstable rate
        test = make_flowing_test(
            duration_hours=1.0,
            rate_start_bopd=200,
            rate_end_bopd=800,
            static_wellhead_pressure_psi=1000,
            flowing_wellhead_pressure_psi=995,
            separator_pressure_psi=980,
        )
        result = scorer.score(test)
        assert result.confidence == "Red"
        assert result.score < 50

    def test_amber_range(self):
        # Pass duration and separator, fail stabilization and drawdown
        test = make_flowing_test(
            duration_hours=6.0,
            rate_start_bopd=300,
            rate_end_bopd=700,
            static_wellhead_pressure_psi=1000,
            flowing_wellhead_pressure_psi=995,
            separator_pressure_psi=100,
        )
        result = scorer.score(test)
        assert 50 <= result.score < 80 or result.confidence in {"Amber", "Red"}

    def test_score_bounded_0_to_100(self):
        test = make_flowing_test()
        result = scorer.score(test)
        assert 0 <= result.score <= 100
