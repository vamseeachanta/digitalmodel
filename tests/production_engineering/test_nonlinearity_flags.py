# ABOUTME: TDD tests for nonlinearity flag detection in production tests
# ABOUTME: Covers transient flow, slug flow, gas lift instability, choke criticality

"""Tests for production_engineering.nonlinearity_flags module."""

import pytest

from digitalmodel.production_engineering.nonlinearity_flags import (
    NonlinearityDetector,
    NonlinearityFlag,
)
from digitalmodel.production_engineering.test_quality_scorer import (
    ProductionTestRecord,
    WellType,
)


def make_flowing_test(**overrides) -> ProductionTestRecord:
    defaults = dict(
        well_id="WELL-001",
        test_date="2026-01-15",
        well_type=WellType.FLOWING,
        duration_hours=8.0,
        oil_rate_bopd=500.0,
        gas_rate_mscfd=500.0,
        water_rate_bwpd=100.0,
        flowing_wellhead_pressure_psi=800.0,
        separator_pressure_psi=200.0,
        static_wellhead_pressure_psi=1200.0,
        rate_start_bopd=495.0,
        rate_end_bopd=505.0,
    )
    defaults.update(overrides)
    return ProductionTestRecord(**defaults)


detector = NonlinearityDetector()


class TestTransientFlowDetection:

    def test_adequate_duration_no_transient_flag(self):
        # 8h duration, min for flowing = 4h, threshold = 2x = 8h → borderline
        test = make_flowing_test(duration_hours=10.0)
        flags = detector.detect(test)
        assert NonlinearityFlag.TRANSIENT_FLOW not in flags

    def test_short_duration_flags_transient(self):
        # 2h < 2x4h = 8h threshold
        test = make_flowing_test(duration_hours=2.0)
        flags = detector.detect(test)
        assert NonlinearityFlag.TRANSIENT_FLOW in flags

    def test_gas_lift_transient_threshold_higher(self):
        # min_gas_lift = 8h, threshold = 2x8h = 16h
        test = make_flowing_test(well_type=WellType.GAS_LIFT, duration_hours=10.0)
        flags = detector.detect(test)
        assert NonlinearityFlag.TRANSIENT_FLOW in flags


class TestSlugFlowDetection:

    def test_low_gor_high_watercut_slugflow_flagged(self):
        # Low GOR (300 scf/bbl) + medium watercut (0.4) → slug flow likely
        test = make_flowing_test(
            oil_rate_bopd=500,
            gas_rate_mscfd=150,   # 150k scf / 500 bbl = 300 scf/bbl
            water_rate_bwpd=333,
        )
        flags = detector.detect(test)
        assert NonlinearityFlag.SLUG_FLOW_LIKELY in flags

    def test_high_gor_no_slug_flag(self):
        # High GOR (2000 scf/bbl) → annular/mist flow, not slug
        test = make_flowing_test(
            oil_rate_bopd=500,
            gas_rate_mscfd=1000,   # 1M scf / 500 bbl = 2000 scf/bbl
            water_rate_bwpd=100,
        )
        flags = detector.detect(test)
        assert NonlinearityFlag.SLUG_FLOW_LIKELY not in flags

    def test_very_low_watercut_no_slug_flag(self):
        # Low watercut even with low GOR → single-phase-like liquid behavior
        test = make_flowing_test(
            oil_rate_bopd=500,
            gas_rate_mscfd=150,
            water_rate_bwpd=10,  # watercut = 10/510 ≈ 2% < 30% threshold
        )
        flags = detector.detect(test)
        assert NonlinearityFlag.SLUG_FLOW_LIKELY not in flags


class TestGasLiftInstabilityDetection:

    def test_stable_gas_lift_no_flag(self):
        test = make_flowing_test(
            well_type=WellType.GAS_LIFT,
            gas_lift_rate_start_mscfd=1000.0,
            gas_lift_rate_end_mscfd=1005.0,
        )
        flags = detector.detect(test)
        assert NonlinearityFlag.GAS_LIFT_INSTABILITY not in flags

    def test_highly_variable_gas_lift_flagged(self):
        # 40% variation → instability
        test = make_flowing_test(
            well_type=WellType.GAS_LIFT,
            gas_lift_rate_start_mscfd=700.0,
            gas_lift_rate_end_mscfd=1300.0,
        )
        flags = detector.detect(test)
        assert NonlinearityFlag.GAS_LIFT_INSTABILITY in flags

    def test_non_gas_lift_well_no_instability_flag(self):
        test = make_flowing_test(well_type=WellType.FLOWING)
        flags = detector.detect(test)
        assert NonlinearityFlag.GAS_LIFT_INSTABILITY not in flags


class TestChokeCriticalityDetection:

    def test_subcritical_choke_no_flag(self):
        # pressure ratio = 200/800 = 0.25 < 0.528 critical
        test = make_flowing_test(
            separator_pressure_psi=200,
            flowing_wellhead_pressure_psi=800,
        )
        flags = detector.detect(test)
        assert NonlinearityFlag.CHOKE_NEAR_CRITICAL not in flags

    def test_near_critical_choke_flagged(self):
        # pressure ratio = 700/800 = 0.875 > 0.528 critical
        test = make_flowing_test(
            separator_pressure_psi=700,
            flowing_wellhead_pressure_psi=800,
        )
        flags = detector.detect(test)
        assert NonlinearityFlag.CHOKE_NEAR_CRITICAL in flags


class TestHighWatercutDetection:

    def test_low_watercut_no_flag(self):
        test = make_flowing_test(oil_rate_bopd=800, water_rate_bwpd=200)
        # watercut = 200/1000 = 0.20
        flags = detector.detect(test)
        assert NonlinearityFlag.HIGH_WATERCUT not in flags

    def test_high_watercut_flagged(self):
        test = make_flowing_test(oil_rate_bopd=100, water_rate_bwpd=900)
        # watercut = 900/1000 = 0.90 ≥ 0.80 threshold
        flags = detector.detect(test)
        assert NonlinearityFlag.HIGH_WATERCUT in flags


class TestMultipleFlagsSimultaneous:

    def test_clean_test_returns_empty_flag_list(self):
        test = make_flowing_test(
            duration_hours=12.0,
            oil_rate_bopd=500,
            gas_rate_mscfd=1000,  # high GOR → no slug
            water_rate_bwpd=50,   # low watercut
            flowing_wellhead_pressure_psi=800,
            separator_pressure_psi=150,
        )
        flags = detector.detect(test)
        assert flags == []

    def test_returns_list_type(self):
        test = make_flowing_test()
        flags = detector.detect(test)
        assert isinstance(flags, list)
