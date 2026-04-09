# ABOUTME: Tests for the field development economics facade over worldenergydata backends.
# ABOUTME: Issue #1858 — Economics facade construction, adapter delegation, error handling.
"""
Tests for digitalmodel.field_development.economics

Covers:
- Facade construction from minimal input
- Adapter selection / delegation to mocked backend calls
- Handling of unsupported fiscal regime / missing fields
- End-to-end evaluation with all adapters
"""

from __future__ import annotations

import sys
from pathlib import Path
from unittest.mock import MagicMock, patch
import math

import pytest

# Make worldenergydata importable for integration bridge tests
_wed_src = Path(__file__).resolve().parents[3] / "worldenergydata" / "src"
if _wed_src.exists() and str(_wed_src) not in sys.path:
    sys.path.insert(0, str(_wed_src))

from digitalmodel.field_development.economics import (
    EconomicsInput,
    EconomicsResult,
    FiscalRegime,
    CostEstimates,
    EvaluationMetrics,
    evaluate_economics,
    build_economics_schedule,
    carbon_sensitivity,
    _resolve_capex_adapter,
    _resolve_abex_adapter,
    _resolve_financial_adapter,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def minimal_input() -> EconomicsInput:
    """Minimal valid EconomicsInput for a GoM TLP field."""
    return EconomicsInput(
        field_name="Test Field Alpha",
        water_depth_m=1200.0,
        host_type="TLP",
        production_capacity_bopd=80_000.0,
        oil_price_usd_per_bbl=70.0,
        discount_rate=0.10,
        fiscal_regime=FiscalRegime.US,
        field_life_years=25,
    )


@pytest.fixture
def full_input() -> EconomicsInput:
    """Fully populated EconomicsInput with all optional fields."""
    return EconomicsInput(
        field_name="Whale Deepwater",
        water_depth_m=2100.0,
        host_type="Spar",
        production_capacity_bopd=100_000.0,
        oil_price_usd_per_bbl=75.0,
        discount_rate=0.10,
        fiscal_regime=FiscalRegime.US,
        field_life_years=30,
        reservoir_size_mmbbl=400.0,
        opex_usd_per_bbl=25.0,
        capex_usd_mm=8_500.0,
        abex_usd_mm=500.0,
        carbon_cost_usd_per_tonne=50.0,
        region="gom",
    )


# ---------------------------------------------------------------------------
# EconomicsInput validation
# ---------------------------------------------------------------------------


class TestEconomicsInput:
    """Test EconomicsInput dataclass construction and validation."""

    def test_minimal_construction(self, minimal_input: EconomicsInput) -> None:
        assert minimal_input.field_name == "Test Field Alpha"
        assert minimal_input.water_depth_m == 1200.0
        assert minimal_input.host_type == "TLP"
        assert minimal_input.fiscal_regime is FiscalRegime.US

    def test_defaults_applied(self, minimal_input: EconomicsInput) -> None:
        assert minimal_input.reservoir_size_mmbbl is None
        assert minimal_input.opex_usd_per_bbl is None
        assert minimal_input.capex_usd_mm is None
        assert minimal_input.abex_usd_mm is None
        assert minimal_input.carbon_cost_usd_per_tonne is None
        assert minimal_input.region is None

    def test_full_construction(self, full_input: EconomicsInput) -> None:
        assert full_input.opex_usd_per_bbl == 25.0
        assert full_input.capex_usd_mm == 8_500.0
        assert full_input.abex_usd_mm == 500.0
        assert full_input.carbon_cost_usd_per_tonne == 50.0
        assert full_input.region == "gom"

    def test_negative_water_depth_raises(self) -> None:
        with pytest.raises(ValueError, match="water_depth_m"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=-100.0,
                host_type="TLP",
                production_capacity_bopd=80_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=25,
            )

    def test_zero_production_raises(self) -> None:
        with pytest.raises(ValueError, match="production_capacity_bopd"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=1000.0,
                host_type="TLP",
                production_capacity_bopd=0.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=25,
            )

    def test_negative_discount_rate_raises(self) -> None:
        with pytest.raises(ValueError, match="discount_rate"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=1000.0,
                host_type="TLP",
                production_capacity_bopd=80_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=-0.5,
                fiscal_regime=FiscalRegime.US,
                field_life_years=25,
            )

    def test_zero_oil_price_raises(self) -> None:
        with pytest.raises(ValueError, match="oil_price_usd_per_bbl"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=1000.0,
                host_type="TLP",
                production_capacity_bopd=80_000.0,
                oil_price_usd_per_bbl=0.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=25,
            )

    def test_zero_field_life_raises(self) -> None:
        with pytest.raises(ValueError, match="field_life_years"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=1000.0,
                host_type="TLP",
                production_capacity_bopd=80_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=0,
            )

    def test_invalid_host_type_raises(self) -> None:
        with pytest.raises(ValueError, match="host_type"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=1000.0,
                host_type="Catamaran",
                production_capacity_bopd=80_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=25,
            )


# ---------------------------------------------------------------------------
# FiscalRegime enum
# ---------------------------------------------------------------------------


class TestFiscalRegime:
    """Test FiscalRegime enum completeness."""

    def test_all_five_regimes(self) -> None:
        assert len(FiscalRegime) == 5
        expected = {"US", "Norway", "UK", "Brazil", "Nigeria"}
        actual = {r.value for r in FiscalRegime}
        assert actual == expected

    def test_unsupported_regime_string(self) -> None:
        with pytest.raises(ValueError):
            FiscalRegime("Australia")


# ---------------------------------------------------------------------------
# CostEstimates and EvaluationMetrics
# ---------------------------------------------------------------------------


class TestResultDataclasses:
    """Test output dataclasses."""

    def test_cost_estimates_construction(self) -> None:
        costs = CostEstimates(
            capex_usd_mm=5_000.0,
            opex_usd_mm_per_yr=200.0,
            abex_usd_mm=300.0,
            capex_source="worldenergydata.cost.CostPredictor",
            abex_source="worldenergydata.decommissioning",
        )
        assert costs.capex_usd_mm == 5_000.0
        assert costs.opex_usd_mm_per_yr == 200.0
        assert costs.abex_usd_mm == 300.0

    def test_evaluation_metrics_construction(self) -> None:
        metrics = EvaluationMetrics(
            npv_usd_mm=1_500.0,
            irr=0.15,
            mirr=0.12,
            payback_years=7.5,
            discount_rate=0.10,
        )
        assert metrics.npv_usd_mm == 1_500.0
        assert metrics.irr == 0.15
        assert metrics.mirr == 0.12
        assert metrics.payback_years == 7.5


# ---------------------------------------------------------------------------
# Adapter resolution
# ---------------------------------------------------------------------------


class TestAdapterResolution:
    """Test adapter selection logic."""

    def test_capex_adapter_returns_callable(self) -> None:
        adapter = _resolve_capex_adapter()
        assert callable(adapter)

    def test_abex_adapter_returns_callable(self) -> None:
        adapter = _resolve_abex_adapter()
        assert callable(adapter)

    def test_financial_adapter_returns_callable(self) -> None:
        adapter = _resolve_financial_adapter()
        assert callable(adapter)


# ---------------------------------------------------------------------------
# evaluate_economics — mocked backends
# ---------------------------------------------------------------------------


class TestEvaluateEconomicsMocked:
    """Test the main evaluate_economics() with mocked worldenergydata backends."""

    def test_basic_evaluation_returns_result(self, minimal_input: EconomicsInput) -> None:
        """evaluate_economics returns an EconomicsResult with valid metrics."""
        result = evaluate_economics(minimal_input)
        assert isinstance(result, EconomicsResult)
        assert isinstance(result.costs, CostEstimates)
        assert isinstance(result.metrics, EvaluationMetrics)

    def test_result_has_field_name(self, minimal_input: EconomicsInput) -> None:
        result = evaluate_economics(minimal_input)
        assert result.field_name == "Test Field Alpha"

    def test_result_has_fiscal_regime(self, minimal_input: EconomicsInput) -> None:
        result = evaluate_economics(minimal_input)
        assert result.fiscal_regime is FiscalRegime.US

    def test_capex_is_positive(self, minimal_input: EconomicsInput) -> None:
        result = evaluate_economics(minimal_input)
        assert result.costs.capex_usd_mm > 0

    def test_opex_is_positive(self, minimal_input: EconomicsInput) -> None:
        result = evaluate_economics(minimal_input)
        assert result.costs.opex_usd_mm_per_yr > 0

    def test_abex_is_positive(self, minimal_input: EconomicsInput) -> None:
        result = evaluate_economics(minimal_input)
        assert result.costs.abex_usd_mm > 0

    def test_npv_is_finite(self, minimal_input: EconomicsInput) -> None:
        result = evaluate_economics(minimal_input)
        assert math.isfinite(result.metrics.npv_usd_mm)

    def test_irr_is_finite(self, minimal_input: EconomicsInput) -> None:
        result = evaluate_economics(minimal_input)
        assert result.metrics.irr is None or math.isfinite(result.metrics.irr)

    def test_mirr_is_finite(self, minimal_input: EconomicsInput) -> None:
        result = evaluate_economics(minimal_input)
        assert result.metrics.mirr is None or math.isfinite(result.metrics.mirr)

    def test_payback_years_positive(self, minimal_input: EconomicsInput) -> None:
        result = evaluate_economics(minimal_input)
        assert result.metrics.payback_years is None or result.metrics.payback_years > 0

    def test_explicit_capex_override_used(self, full_input: EconomicsInput) -> None:
        """When capex_usd_mm is provided, it should be used directly."""
        result = evaluate_economics(full_input)
        assert result.costs.capex_usd_mm == 8_500.0
        assert "user-provided" in result.costs.capex_source.lower()

    def test_explicit_abex_override_used(self, full_input: EconomicsInput) -> None:
        """When abex_usd_mm is provided, it should be used directly."""
        result = evaluate_economics(full_input)
        assert result.costs.abex_usd_mm == 500.0
        assert "user-provided" in result.costs.abex_source.lower()


# ---------------------------------------------------------------------------
# Fiscal regime passthrough
# ---------------------------------------------------------------------------


class TestFiscalRegimePassthrough:
    """Test that fiscal_regime is recorded in the result."""

    @pytest.mark.parametrize("regime", list(FiscalRegime))
    def test_each_regime_accepted(self, regime: FiscalRegime) -> None:
        inp = EconomicsInput(
            field_name=f"Test {regime.value}",
            water_depth_m=1000.0,
            host_type="Semi",
            production_capacity_bopd=50_000.0,
            reservoir_size_mmbbl=150.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=regime,
            field_life_years=20,
        )
        result = evaluate_economics(inp)
        assert result.fiscal_regime is regime


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------


class TestEdgeCases:
    """Edge-case handling for the economics facade."""

    def test_very_small_field(self) -> None:
        """A marginal field should still produce valid output."""
        inp = EconomicsInput(
            field_name="Marginal",
            water_depth_m=300.0,
            host_type="Subsea_Tieback",
            production_capacity_bopd=5_000.0,
            reservoir_size_mmbbl=10.0,
            oil_price_usd_per_bbl=60.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=10,
        )
        result = evaluate_economics(inp)
        assert isinstance(result, EconomicsResult)
        assert result.costs.capex_usd_mm > 0

    def test_very_deep_field(self) -> None:
        """Ultra-deepwater (3000 m) should not crash."""
        inp = EconomicsInput(
            field_name="Ultra Deep",
            water_depth_m=3000.0,
            host_type="FPSO",
            production_capacity_bopd=120_000.0,
            reservoir_size_mmbbl=500.0,
            oil_price_usd_per_bbl=80.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.Brazil,
            field_life_years=30,
        )
        result = evaluate_economics(inp)
        assert isinstance(result, EconomicsResult)


# ---------------------------------------------------------------------------
# CostPredictor adapter wiring (mocked)
# ---------------------------------------------------------------------------


class TestCostPredictorWiring:
    """Test that _resolve_capex_adapter tries CostPredictor before GoM fallback."""

    def test_capex_adapter_falls_back_when_cost_import_fails(
        self, minimal_input: EconomicsInput
    ) -> None:
        """When worldenergydata.cost is unavailable, GoM benchmark is used."""
        with patch.dict("sys.modules", {"worldenergydata.cost": None}):
            adapter = _resolve_capex_adapter()
            capex_mm, source = adapter(minimal_input)
            assert capex_mm > 0
            assert "gom benchmark" in source.lower() or "digitalmodel" in source.lower()

    def test_capex_adapter_uses_predictor_when_available(
        self, minimal_input: EconomicsInput
    ) -> None:
        """When CostPredictor is available and fitted, it is used as primary."""
        mock_result = MagicMock()
        mock_result.cost_usd_mm = 6_500.0

        mock_predictor_cls = MagicMock()
        mock_predictor_instance = MagicMock()
        mock_predictor_instance._n_train = 71
        mock_predictor_instance.predict.return_value = mock_result
        mock_predictor_cls.return_value = mock_predictor_instance

        mock_dataset = [MagicMock()] * 10  # >= 5 records

        with patch(
            "digitalmodel.field_development.economics.CostPredictor",
            mock_predictor_cls,
            create=True,
        ), patch(
            "digitalmodel.field_development.economics.load_public_dataset",
            return_value=mock_dataset,
            create=True,
        ):
            # Since these are lazy imports inside the adapter closure, we need
            # to patch at the module level where they get imported.
            import digitalmodel.field_development.economics as econ_mod

            # Capture original
            orig_resolve = econ_mod._resolve_capex_adapter

            # The adapter uses inline imports, so we mock at sys.modules level
            mock_cost_mod = MagicMock()
            mock_cost_mod.CostPredictor = mock_predictor_cls
            mock_cost_mod.load_public_dataset = MagicMock(return_value=mock_dataset)
            mock_cost_mod.CostDataPoint = MagicMock(return_value=MagicMock())

            mock_schema_mod = MagicMock()

            with patch.dict("sys.modules", {
                "worldenergydata.cost": mock_cost_mod,
                "worldenergydata.cost.data_collection.calibration_schema": mock_schema_mod,
            }):
                adapter = _resolve_capex_adapter()
                capex_mm, source = adapter(minimal_input)
                assert capex_mm == 6_500.0
                assert "costpredictor" in source.lower()

    def test_capex_override_bypasses_adapter(self, full_input: EconomicsInput) -> None:
        """User-provided capex_usd_mm should bypass all adapters."""
        result = evaluate_economics(full_input)
        assert result.costs.capex_usd_mm == 8_500.0
        assert "user-provided" in result.costs.capex_source.lower()


# ---------------------------------------------------------------------------
# build_economics_schedule — CashFlowSchedule bridge
# ---------------------------------------------------------------------------


class TestBuildEconomicsSchedule:
    """Test the CashFlowSchedule bridge function."""

    def test_returns_cashflow_schedule(self, minimal_input: EconomicsInput) -> None:
        """build_economics_schedule returns a CashFlowSchedule."""
        from worldenergydata.economics.dcf import CashFlowSchedule

        schedule = build_economics_schedule(minimal_input)
        assert isinstance(schedule, CashFlowSchedule)

    def test_schedule_length_matches_field_life(
        self, minimal_input: EconomicsInput
    ) -> None:
        """Schedule has field_life_years + 1 entries (year 0..n)."""
        schedule = build_economics_schedule(minimal_input)
        expected = minimal_input.field_life_years + 1
        assert len(schedule.years) == expected
        assert len(schedule.capex) == expected
        assert len(schedule.revenue) == expected
        assert len(schedule.opex) == expected
        assert len(schedule.carbon_cost) == expected

    def test_schedule_has_emissions(self, minimal_input: EconomicsInput) -> None:
        """Schedule includes emission_tco2_per_period for carbon sensitivity."""
        schedule = build_economics_schedule(minimal_input)
        assert schedule.emission_tco2_per_period is not None
        assert len(schedule.emission_tco2_per_period) == len(schedule.years)

    def test_schedule_capex_year0_positive(
        self, minimal_input: EconomicsInput
    ) -> None:
        """CAPEX should be positive (outflow) in year 0."""
        schedule = build_economics_schedule(minimal_input)
        assert schedule.capex[0] > 0

    def test_schedule_revenue_year0_zero(
        self, minimal_input: EconomicsInput
    ) -> None:
        """No revenue in year 0 (development phase)."""
        schedule = build_economics_schedule(minimal_input)
        assert schedule.revenue[0] == 0.0

    def test_schedule_with_cost_overrides(self) -> None:
        """Explicit capex/opex/abex overrides are respected."""
        inp = EconomicsInput(
            field_name="Override Test",
            water_depth_m=1000.0,
            host_type="TLP",
            production_capacity_bopd=50_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=10,
        )
        schedule = build_economics_schedule(
            inp,
            capex_usd_mm=5_000.0,
            opex_usd_mm_per_yr=200.0,
            abex_usd_mm=300.0,
        )
        # Total CAPEX should be 5000 MM (spread) + 300 MM (ABEX in final year)
        total_capex_usd = sum(schedule.capex)
        expected_total = (5_000.0 + 300.0) * 1_000_000.0
        assert abs(total_capex_usd - expected_total) < 1.0

    def test_schedule_usable_with_dcf_calculate_npv(
        self, minimal_input: EconomicsInput
    ) -> None:
        """Schedule can be passed directly to worldenergydata.economics.dcf.calculate_npv."""
        from worldenergydata.economics.dcf import calculate_npv, NPVResult

        schedule = build_economics_schedule(minimal_input)
        result = calculate_npv(schedule, minimal_input.discount_rate)
        assert isinstance(result, NPVResult)
        assert math.isfinite(result.npv)


# ---------------------------------------------------------------------------
# carbon_sensitivity — carbon price sweep
# ---------------------------------------------------------------------------


class TestCarbonSensitivity:
    """Test the carbon sensitivity pass-through."""

    def test_returns_sensitivity_result(self, minimal_input: EconomicsInput) -> None:
        """carbon_sensitivity returns a CarbonSensitivityResult."""
        from worldenergydata.economics.carbon import CarbonSensitivityResult

        result = carbon_sensitivity(minimal_input)
        assert isinstance(result, CarbonSensitivityResult)

    def test_default_carbon_prices(self, minimal_input: EconomicsInput) -> None:
        """Default sweep covers 7 price points."""
        result = carbon_sensitivity(minimal_input)
        assert len(result.carbon_prices) == 7

    def test_custom_carbon_prices(self, minimal_input: EconomicsInput) -> None:
        """Custom carbon prices are passed through."""
        prices = [0, 50, 100]
        result = carbon_sensitivity(minimal_input, carbon_prices=prices)
        assert len(result.carbon_prices) == 3

    def test_npv_decreases_with_carbon_price(
        self, minimal_input: EconomicsInput
    ) -> None:
        """Higher carbon prices should reduce NPV (monotonically decreasing)."""
        result = carbon_sensitivity(minimal_input)
        npvs = result.npv_values
        # Each NPV should be <= the previous one
        for i in range(1, len(npvs)):
            assert npvs[i] <= npvs[i - 1] + 1e-6  # small tolerance

    def test_base_npv_is_zero_carbon(self, minimal_input: EconomicsInput) -> None:
        """base_npv should equal NPV at carbon_price=0."""
        result = carbon_sensitivity(minimal_input)
        assert abs(result.base_npv - result.npv_values[0]) < 1e-6
