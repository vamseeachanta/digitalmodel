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
    DeclineType,
    EconomicsInput,
    EconomicsResult,
    FiscalRegime,
    CostEstimates,
    EvaluationMetrics,
    evaluate_economics,
    build_economics_schedule,
    carbon_sensitivity,
    _production_factors,
    _resolve_capex_adapter,
    _resolve_abex_adapter,
    _resolve_financial_adapter,
    _apply_fiscal_regime,
    _resolve_fiscal_calculator,
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


# ---------------------------------------------------------------------------
# DeclineType enum (#2054)
# ---------------------------------------------------------------------------


class TestDeclineType:
    """Test DeclineType enum members and usage."""

    def test_four_members(self) -> None:
        assert len(DeclineType) == 4
        expected = {"exponential", "hyperbolic", "harmonic", "linear"}
        actual = {d.value for d in DeclineType}
        assert actual == expected

    def test_string_enum(self) -> None:
        assert isinstance(DeclineType.LINEAR, str)
        assert DeclineType.LINEAR == "linear"

    def test_invalid_value_raises(self) -> None:
        with pytest.raises(ValueError):
            DeclineType("parabolic")


# ---------------------------------------------------------------------------
# EconomicsInput decline defaults and validation (#2054)
# ---------------------------------------------------------------------------


class TestDeclineInputDefaults:
    """Test that new decline fields default correctly and validate."""

    def test_defaults_are_linear(self, minimal_input: EconomicsInput) -> None:
        """Default decline_type is LINEAR for backward compatibility."""
        assert minimal_input.decline_type is DeclineType.LINEAR
        assert minimal_input.decline_rate == 0.0
        assert minimal_input.b_factor == 0.5

    def test_exponential_requires_positive_rate(self) -> None:
        with pytest.raises(ValueError, match="decline_rate"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=1000.0,
                host_type="TLP",
                production_capacity_bopd=80_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=25,
                decline_type=DeclineType.EXPONENTIAL,
                decline_rate=0.0,
            )

    def test_harmonic_requires_positive_rate(self) -> None:
        with pytest.raises(ValueError, match="decline_rate"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=1000.0,
                host_type="TLP",
                production_capacity_bopd=80_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=25,
                decline_type=DeclineType.HARMONIC,
                decline_rate=-0.1,
            )

    def test_hyperbolic_b_factor_out_of_range(self) -> None:
        # b_factor must be in (0, 1]; b=1.0 is valid (harmonic limit),
        # but b>1 (e.g. 1.5) is outside the Arps hyperbolic definition.
        with pytest.raises(ValueError, match="b_factor"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=1000.0,
                host_type="TLP",
                production_capacity_bopd=80_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=25,
                decline_type=DeclineType.HYPERBOLIC,
                decline_rate=0.15,
                b_factor=1.5,  # b > 1 is outside the valid range
            )

    def test_hyperbolic_b_factor_one_accepted(self) -> None:
        """b_factor=1.0 is the harmonic limit and must be accepted."""
        inp = EconomicsInput(
            field_name="HarmonicLimit",
            water_depth_m=1000.0,
            host_type="TLP",
            production_capacity_bopd=80_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=25,
            decline_type=DeclineType.HYPERBOLIC,
            decline_rate=0.15,
            b_factor=1.0,
        )
        assert inp.b_factor == 1.0  # no ValueError raised

    def test_hyperbolic_b_factor_zero_rejected(self) -> None:
        with pytest.raises(ValueError, match="b_factor"):
            EconomicsInput(
                field_name="Bad",
                water_depth_m=1000.0,
                host_type="TLP",
                production_capacity_bopd=80_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=25,
                decline_type=DeclineType.HYPERBOLIC,
                decline_rate=0.15,
                b_factor=0.0,  # must be > 0
            )

    def test_linear_allows_zero_rate(self, minimal_input: EconomicsInput) -> None:
        """LINEAR decline does not require a positive decline_rate."""
        assert minimal_input.decline_rate == 0.0  # no error raised


# ---------------------------------------------------------------------------
# _production_factors shape tests (#2054)
# ---------------------------------------------------------------------------


class TestProductionFactors:
    """Test decline factor shapes from _production_factors."""

    def _make_input(self, decline_type, decline_rate=0.15, b_factor=0.5):
        return EconomicsInput(
            field_name="Factor Test",
            water_depth_m=1000.0,
            host_type="TLP",
            production_capacity_bopd=50_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=20,
            decline_type=decline_type,
            decline_rate=decline_rate,
            b_factor=b_factor,
        )

    def test_linear_shape(self) -> None:
        """LINEAR: factors decline from 1.0 toward 0.2 in post-plateau."""
        inp = self._make_input(DeclineType.LINEAR)
        factors = _production_factors(inp, 20)
        assert factors[0] == 0.0  # year 0 = development
        plateau_end = max(1, int(20 * 0.6))  # 12
        assert factors[plateau_end] == 1.0
        assert factors[-1] == pytest.approx(0.2, abs=0.01)

    def test_exponential_monotonically_decreasing(self) -> None:
        """EXPONENTIAL: post-plateau factors strictly decrease."""
        inp = self._make_input(DeclineType.EXPONENTIAL)
        factors = _production_factors(inp, 20)
        plateau_end = max(1, int(20 * 0.6))
        post_plateau = factors[plateau_end + 1:]
        for i in range(1, len(post_plateau)):
            assert post_plateau[i] < post_plateau[i - 1]

    def test_exponential_first_factor(self) -> None:
        """EXPONENTIAL: first post-plateau factor = exp(-D*1)."""
        inp = self._make_input(DeclineType.EXPONENTIAL, decline_rate=0.10)
        factors = _production_factors(inp, 20)
        plateau_end = max(1, int(20 * 0.6))
        expected = math.exp(-0.10 * 1)
        assert factors[plateau_end + 1] == pytest.approx(expected, rel=1e-6)

    def test_hyperbolic_monotonically_decreasing(self) -> None:
        """HYPERBOLIC: post-plateau factors strictly decrease."""
        inp = self._make_input(DeclineType.HYPERBOLIC, b_factor=0.5)
        factors = _production_factors(inp, 20)
        plateau_end = max(1, int(20 * 0.6))
        post_plateau = factors[plateau_end + 1:]
        for i in range(1, len(post_plateau)):
            assert post_plateau[i] < post_plateau[i - 1]

    def test_harmonic_formula(self) -> None:
        """HARMONIC: factor = 1/(1+D*t)."""
        inp = self._make_input(DeclineType.HARMONIC, decline_rate=0.20)
        factors = _production_factors(inp, 20)
        plateau_end = max(1, int(20 * 0.6))
        for t in range(1, 20 - plateau_end + 1):
            yr = plateau_end + t
            expected = 1.0 / (1.0 + 0.20 * t)
            assert factors[yr] == pytest.approx(expected, rel=1e-6)

    def test_all_plateau_factors_are_one(self) -> None:
        """All decline types have factors=1.0 during plateau period."""
        for dt in DeclineType:
            rate = 0.0 if dt == DeclineType.LINEAR else 0.15
            inp = self._make_input(dt, decline_rate=rate)
            factors = _production_factors(inp, 20)
            plateau_end = max(1, int(20 * 0.6))
            for yr in range(1, plateau_end + 1):
                assert factors[yr] == 1.0, f"Failed for {dt.value} at year {yr}"

    def test_year_zero_always_zero(self) -> None:
        """Year 0 (development) always has factor 0.0."""
        for dt in DeclineType:
            rate = 0.0 if dt == DeclineType.LINEAR else 0.15
            inp = self._make_input(dt, decline_rate=rate)
            factors = _production_factors(inp, 20)
            assert factors[0] == 0.0


# ---------------------------------------------------------------------------
# Backward compatibility (#2054)
# ---------------------------------------------------------------------------


class TestDeclineBackwardCompatibility:
    """Verify that adding decline fields does not break existing behaviour."""

    def test_minimal_input_still_works(self, minimal_input: EconomicsInput) -> None:
        """EconomicsInput without decline args evaluates as before."""
        result = evaluate_economics(minimal_input)
        assert isinstance(result, EconomicsResult)
        assert math.isfinite(result.metrics.npv_usd_mm)

    def test_schedule_unchanged_for_linear_default(
        self, minimal_input: EconomicsInput
    ) -> None:
        """build_economics_schedule with defaults matches legacy linear decline."""
        schedule = build_economics_schedule(minimal_input)
        n = minimal_input.field_life_years
        assert len(schedule.years) == n + 1
        # Revenue in year 1 should equal plateau revenue
        assert schedule.revenue[1] > 0

    def test_existing_fixtures_unchanged(self, full_input: EconomicsInput) -> None:
        """full_input fixture still evaluates correctly with new fields at defaults."""
        result = evaluate_economics(full_input)
        assert result.costs.capex_usd_mm == 8_500.0
        assert math.isfinite(result.metrics.npv_usd_mm)


# ---------------------------------------------------------------------------
# build_economics_schedule decline integration (#2054) — RED test
# ---------------------------------------------------------------------------


class TestScheduleDeclineIntegration:
    """Verify build_economics_schedule uses decline params, not hardcoded linear."""

    def test_exponential_schedule_differs_from_linear(self) -> None:
        """Exponential decline must produce different revenue than linear default.

        This is the KEY test: build_economics_schedule currently ignores
        inp.decline_type and hardcodes linear decline.  With exponential
        decline (D=0.15), late-life revenue should differ from the legacy
        linear profile.
        """
        base = dict(
            field_name="Schedule Decline",
            water_depth_m=1000.0,
            host_type="TLP",
            production_capacity_bopd=50_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=20,
            capex_usd_mm=3_000.0,
            opex_usd_per_bbl=20.0,
            abex_usd_mm=200.0,
        )
        linear_inp = EconomicsInput(**base)  # default LINEAR
        exp_inp = EconomicsInput(
            **base,
            decline_type=DeclineType.EXPONENTIAL,
            decline_rate=0.15,
        )

        linear_sched = build_economics_schedule(linear_inp)
        exp_sched = build_economics_schedule(exp_inp)

        # Last producing year (before ABEX) should have different revenue
        last_yr = 19  # year before final year
        assert linear_sched.revenue[last_yr] != pytest.approx(
            exp_sched.revenue[last_yr], rel=0.01
        ), (
            "Exponential schedule revenue should differ from linear — "
            "build_economics_schedule must use decline_type from input"
        )

    def test_schedule_factors_match_production_factors(self) -> None:
        """Revenue ratios in schedule should match _production_factors output."""
        inp = EconomicsInput(
            field_name="Factor Match",
            water_depth_m=1000.0,
            host_type="TLP",
            production_capacity_bopd=50_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=20,
            capex_usd_mm=3_000.0,
            opex_usd_per_bbl=20.0,
            abex_usd_mm=200.0,
            decline_type=DeclineType.EXPONENTIAL,
            decline_rate=0.15,
        )
        factors = _production_factors(inp, 20)
        schedule = build_economics_schedule(inp)

        # Revenue at plateau (year 1) should be 100%; year 15 should match factor
        plateau_revenue = schedule.revenue[1]
        for yr in [5, 10, 15, 19]:
            if plateau_revenue > 0:
                ratio = schedule.revenue[yr] / plateau_revenue
                assert ratio == pytest.approx(factors[yr], rel=0.01), (
                    f"Year {yr}: schedule ratio {ratio:.4f} != factor {factors[yr]:.4f}"
                )


# ---------------------------------------------------------------------------
# EUR-based decline parameterization (#2054)
# ---------------------------------------------------------------------------


class TestEURDeclineParameterization:
    """Test that reservoir_size_mmbbl auto-derives decline rate from EUR."""

    def test_eur_auto_derives_decline_rate(self) -> None:
        """When reservoir_size given without decline_rate, rate is auto-derived."""
        inp = EconomicsInput(
            field_name="EUR Test",
            water_depth_m=1000.0,
            host_type="TLP",
            production_capacity_bopd=30_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=20,
            reservoir_size_mmbbl=1000.0,  # triggers EUR derivation
        )
        assert inp.decline_type is DeclineType.EXPONENTIAL
        assert inp.decline_rate > 0

    def test_eur_constrains_cumulative_production(self) -> None:
        """Derived decline rate constrains total production to approx EUR."""
        reservoir = 1000.0  # MMbbl OOIP
        recovery_factor = 0.15
        eur = reservoir * recovery_factor  # 150 MMbbl

        inp = EconomicsInput(
            field_name="EUR Cum Test",
            water_depth_m=1000.0,
            host_type="TLP",
            production_capacity_bopd=30_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=20,
            reservoir_size_mmbbl=reservoir,
        )

        factors = _production_factors(inp, 20)
        annual_prod = 30_000.0 * 365.0 / 1e6  # MMbbl/yr
        cumulative = sum(f * annual_prod for f in factors)

        assert cumulative == pytest.approx(eur, rel=0.05), (
            f"Cumulative {cumulative:.1f} MMbbl should ≈ EUR {eur:.1f} MMbbl"
        )

    def test_explicit_decline_rate_not_overridden(self) -> None:
        """When both reservoir_size and decline_rate given, use explicit rate."""
        inp = EconomicsInput(
            field_name="Explicit Rate",
            water_depth_m=1000.0,
            host_type="TLP",
            production_capacity_bopd=30_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=20,
            reservoir_size_mmbbl=1000.0,
            decline_type=DeclineType.EXPONENTIAL,
            decline_rate=0.20,
        )
        assert inp.decline_rate == 0.20  # not overridden by EUR derivation


# ---------------------------------------------------------------------------
# Fiscal regime tax adjustment (#2050)
# ---------------------------------------------------------------------------


class TestFiscalRegimeTaxAdjustment:
    """Test fiscal regime tax adjustment applied to economics cashflows."""

    @pytest.fixture
    def tax_test_input(self) -> EconomicsInput:
        """Input with explicit cost overrides to isolate fiscal effects."""
        return EconomicsInput(
            field_name="Fiscal Test Field",
            water_depth_m=1200.0,
            host_type="TLP",
            production_capacity_bopd=80_000.0,
            oil_price_usd_per_bbl=75.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=20,
            capex_usd_mm=5_000.0,
            opex_usd_per_bbl=20.0,
            abex_usd_mm=350.0,
        )

    def test_passthrough_when_fiscal_module_unavailable(
        self, tax_test_input: EconomicsInput
    ) -> None:
        """When worldenergydata fiscal module cannot be imported, cashflows
        pass through unchanged (backward-compatible pre-tax behavior)."""
        import numpy as np
        from digitalmodel.field_development.economics import _build_annual_cashflows

        cashflows = _build_annual_cashflows(
            tax_test_input, 5_000.0, 200.0, 350.0,
        )
        original = cashflows.copy()

        # Force ImportError by patching away the fiscal modules
        with patch.dict("sys.modules", {
            "worldenergydata.eia_us.analysis.us_fiscal": None,
            "worldenergydata.eia_us.analysis": None,
            "worldenergydata.eia_us": None,
        }):
            adjusted = _apply_fiscal_regime(
                cashflows, tax_test_input, 5_000.0, 200.0, 350.0,
            )
        np.testing.assert_array_equal(adjusted, original)

    def test_fiscal_adjustment_reduces_npv_for_uk(self) -> None:
        """UK fiscal regime (75% headline) should produce post-tax NPV < pre-tax NPV."""
        inp = EconomicsInput(
            field_name="UK Fiscal Test",
            water_depth_m=150.0,
            host_type="Subsea_Tieback",
            production_capacity_bopd=40_000.0,
            oil_price_usd_per_bbl=80.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.UK,
            field_life_years=15,
            capex_usd_mm=3_000.0,
            opex_usd_per_bbl=18.0,
            abex_usd_mm=200.0,
        )
        result = evaluate_economics(inp)
        # Post-tax NPV should be strictly less than pre-tax NPV
        assert result.metrics.pre_tax_npv_usd_mm is not None
        assert result.metrics.npv_usd_mm < result.metrics.pre_tax_npv_usd_mm

    def test_fiscal_adjustment_reduces_npv_for_us(
        self, tax_test_input: EconomicsInput
    ) -> None:
        """US federal royalty should reduce NPV vs pre-tax baseline."""
        result = evaluate_economics(tax_test_input)
        assert result.metrics.pre_tax_npv_usd_mm is not None
        assert result.metrics.npv_usd_mm < result.metrics.pre_tax_npv_usd_mm

    def test_pre_tax_metrics_populated(
        self, tax_test_input: EconomicsInput
    ) -> None:
        """Pre-tax NPV and IRR fields are populated when fiscal is applied."""
        result = evaluate_economics(tax_test_input)
        assert result.metrics.pre_tax_npv_usd_mm is not None
        assert math.isfinite(result.metrics.pre_tax_npv_usd_mm)
        # pre_tax_irr may be None if numpy_financial unavailable, but if set
        # it must be finite
        if result.metrics.pre_tax_irr is not None:
            assert math.isfinite(result.metrics.pre_tax_irr)

    def test_result_remains_finite_and_structured(
        self, tax_test_input: EconomicsInput
    ) -> None:
        """All result fields remain finite and correctly typed after fiscal adj."""
        result = evaluate_economics(tax_test_input)
        assert isinstance(result, EconomicsResult)
        assert isinstance(result.metrics, EvaluationMetrics)
        assert math.isfinite(result.metrics.npv_usd_mm)
        assert result.metrics.irr is None or math.isfinite(result.metrics.irr)
        assert result.metrics.mirr is None or math.isfinite(result.metrics.mirr)
        assert (
            result.metrics.payback_years is None
            or result.metrics.payback_years > 0
        )

    def test_backward_compat_existing_fixtures(
        self, minimal_input: EconomicsInput, full_input: EconomicsInput
    ) -> None:
        """Existing fixtures still produce valid EconomicsResult objects."""
        for inp in [minimal_input, full_input]:
            result = evaluate_economics(inp)
            assert isinstance(result, EconomicsResult)
            assert math.isfinite(result.metrics.npv_usd_mm)
            assert result.costs.capex_usd_mm > 0

    @pytest.mark.parametrize("regime", [
        FiscalRegime.US,
        FiscalRegime.UK,
        FiscalRegime.Brazil,
        FiscalRegime.Nigeria,
    ])
    def test_each_regime_applies_tax(self, regime: FiscalRegime) -> None:
        """Each regime with available fiscal module produces adjusted cashflows."""
        import numpy as np
        from digitalmodel.field_development.economics import _build_annual_cashflows

        inp = EconomicsInput(
            field_name=f"Fiscal {regime.value}",
            water_depth_m=1200.0,
            host_type="TLP",
            production_capacity_bopd=60_000.0,
            oil_price_usd_per_bbl=75.0,
            discount_rate=0.10,
            fiscal_regime=regime,
            field_life_years=15,
            capex_usd_mm=4_000.0,
            opex_usd_per_bbl=20.0,
            abex_usd_mm=280.0,
        )
        opex_mm_yr = 20.0 * 60_000.0 * 365.0 / 1e6
        cashflows = _build_annual_cashflows(inp, 4_000.0, opex_mm_yr, 280.0)
        original = cashflows.copy()
        adjusted = _apply_fiscal_regime(cashflows, inp, 4_000.0, opex_mm_yr, 280.0)
        # Tax adjustment should reduce at least one producing year's cashflow
        assert np.any(adjusted < original), (
            f"Fiscal regime {regime.value} should reduce at least one year's cashflow"
        )

    def test_resolve_fiscal_calculator_returns_callable(self) -> None:
        """_resolve_fiscal_calculator returns a callable for supported regimes."""
        inp = EconomicsInput(
            field_name="Resolve Test",
            water_depth_m=1000.0,
            host_type="TLP",
            production_capacity_bopd=50_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.UK,
            field_life_years=15,
        )
        calc = _resolve_fiscal_calculator(inp)
        # UK module should be available
        assert calc is not None
        assert callable(calc)
