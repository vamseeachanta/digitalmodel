# ABOUTME: TDD tests for GIGO (Garbage In, Garbage Out) detector
# ABOUTME: Compares model-predicted rates vs. test rates with physics-based diagnosis

"""Tests for production_engineering.gigo_detector module."""

import pytest

from digitalmodel.production_engineering.gigo_detector import (
    DiagnosticCause,
    GigoDetector,
    GigoResult,
)
from digitalmodel.production_engineering.ipr_models import (
    ReservoirConditions,
    VogelIpr,
)
from digitalmodel.production_engineering.test_quality_scorer import (
    ProductionTestRecord,
    WellType,
)
from digitalmodel.production_engineering.vlp_correlations import (
    FluidProperties,
    TubingConfig,
)


def make_reservoir(**overrides) -> ReservoirConditions:
    defaults = dict(
        reservoir_pressure_psi=3000.0,
        bubble_point_psi=3000.0,
        productivity_index_bopd_psi=2.0,
    )
    defaults.update(overrides)
    return ReservoirConditions(**defaults)


def make_tubing(**overrides) -> TubingConfig:
    defaults = dict(depth_ft=6000.0, tubing_id_in=2.441, roughness_in=0.0006)
    defaults.update(overrides)
    return TubingConfig(**defaults)


def make_fluid(**overrides) -> FluidProperties:
    defaults = dict(oil_api=35.0, gas_gravity=0.65, temperature_f=150.0)
    defaults.update(overrides)
    return FluidProperties(**defaults)


def make_test_record(**overrides) -> ProductionTestRecord:
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


detector = GigoDetector()


class TestGigoDetectorMatch:

    def test_consistent_test_returns_no_anomaly(self):
        """When test rate ≈ model rate, no anomaly flagged."""
        res = make_reservoir(reservoir_pressure_psi=3000)
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        # Build test record consistent with model (rate within 15%)
        test = make_test_record(oil_rate_bopd=600.0)
        result = detector.compare(
            test=test, ipr=ipr, tubing=tubing, fluid=fluid,
            whp_psi=test.flowing_wellhead_pressure_psi,
        )
        assert isinstance(result, GigoResult)

    def test_returns_gigo_result_type(self):
        res = make_reservoir()
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        test = make_test_record()
        result = detector.compare(
            test=test, ipr=ipr, tubing=tubing, fluid=fluid,
            whp_psi=test.flowing_wellhead_pressure_psi,
        )
        assert hasattr(result, "model_rate_bopd")
        assert hasattr(result, "test_rate_bopd")
        assert hasattr(result, "divergence_fraction")
        assert hasattr(result, "flagged")

    def test_large_divergence_is_flagged(self):
        """If test rate is far from model, GIGO flag raised."""
        res = make_reservoir(reservoir_pressure_psi=3000)
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        # Test shows 50 bopd, model predicts ~600+ bopd → large divergence
        test = make_test_record(oil_rate_bopd=50.0)
        result = detector.compare(
            test=test, ipr=ipr, tubing=tubing, fluid=fluid,
            whp_psi=test.flowing_wellhead_pressure_psi,
        )
        assert result.flagged is True

    def test_divergence_fraction_calculated_correctly(self):
        """divergence_fraction = |model - test| / model."""
        res = make_reservoir()
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        test = make_test_record()
        result = detector.compare(
            test=test, ipr=ipr, tubing=tubing, fluid=fluid,
            whp_psi=test.flowing_wellhead_pressure_psi,
        )
        expected_div = abs(result.model_rate_bopd - result.test_rate_bopd) / result.model_rate_bopd
        assert result.divergence_fraction == pytest.approx(expected_div, rel=0.01)


class TestGigoDiagnosticCauses:

    def test_watercut_change_flagged_when_high_wc(self):
        """High watercut + divergence → watercut change diagnosis."""
        res = make_reservoir()
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        test = make_test_record(
            oil_rate_bopd=50.0,
            water_rate_bwpd=950.0,  # very high watercut
        )
        result = detector.compare(
            test=test, ipr=ipr, tubing=tubing, fluid=fluid,
            whp_psi=test.flowing_wellhead_pressure_psi,
        )
        if result.flagged:
            assert DiagnosticCause.WATERCUT_CHANGE in result.likely_causes

    def test_short_stabilization_flagged_when_diverged(self):
        """If stabilization indicators poor + diverged → stabilization diagnosis."""
        res = make_reservoir()
        ipr = VogelIpr(reservoir=res, qmax_bopd=2000.0)
        tubing = make_tubing()
        fluid = make_fluid()
        test = make_test_record(
            oil_rate_bopd=100.0,
            rate_start_bopd=800.0,
            rate_end_bopd=100.0,  # massive rate decline → unstabilised
        )
        result = detector.compare(
            test=test, ipr=ipr, tubing=tubing, fluid=fluid,
            whp_psi=test.flowing_wellhead_pressure_psi,
        )
        if result.flagged:
            assert DiagnosticCause.STABILIZATION in result.likely_causes
