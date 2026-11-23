# ABOUTME: Tests for field development workflow orchestrator.
# ABOUTME: Issue #1858/#1848 — end-to-end FDP screening pipeline tests.
"""
Tests for digitalmodel.field_development.workflow

Covers:
- FieldDevelopmentSpec construction and validation
- evaluate_field_development end-to-end pipeline
- compare_concepts side-by-side evaluation
- Concept→economics data flow correctness
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.field_development.workflow import (
    FieldDevelopmentSpec,
    FieldDevelopmentResult,
    ConceptComparison,
    evaluate_field_development,
    compare_concepts,
)
from digitalmodel.field_development.concept_selection import (
    ConceptSelectionResult,
    HostType,
)
from digitalmodel.field_development.economics import (
    EconomicsResult,
    FiscalRegime,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def gom_deepwater_spec() -> FieldDevelopmentSpec:
    """GoM deepwater field — typical Spar/TLP candidate."""
    return FieldDevelopmentSpec(
        field_name="Test Deepwater Alpha",
        water_depth_m=1500.0,
        reservoir_size_mmbbl=300.0,
        production_capacity_bopd=80_000.0,
        oil_price_usd_per_bbl=70.0,
        discount_rate=0.10,
        fiscal_regime=FiscalRegime.US,
        field_life_years=25,
    )


@pytest.fixture
def tieback_spec() -> FieldDevelopmentSpec:
    """Small, shallow field near infrastructure — tieback candidate."""
    return FieldDevelopmentSpec(
        field_name="Test Tieback Beta",
        water_depth_m=400.0,
        reservoir_size_mmbbl=30.0,
        production_capacity_bopd=15_000.0,
        oil_price_usd_per_bbl=65.0,
        discount_rate=0.10,
        fiscal_regime=FiscalRegime.US,
        field_life_years=12,
        distance_to_infra_km=10.0,
    )


@pytest.fixture
def norway_spec() -> FieldDevelopmentSpec:
    """North Sea field under Norwegian fiscal regime."""
    return FieldDevelopmentSpec(
        field_name="Test Norway Gamma",
        water_depth_m=350.0,
        reservoir_size_mmbbl=200.0,
        production_capacity_bopd=60_000.0,
        oil_price_usd_per_bbl=75.0,
        discount_rate=0.08,
        fiscal_regime=FiscalRegime.Norway,
        field_life_years=20,
        fluid_type="oil",
    )


# ---------------------------------------------------------------------------
# FieldDevelopmentSpec validation
# ---------------------------------------------------------------------------


class TestFieldDevelopmentSpec:
    """Test FieldDevelopmentSpec construction and validation."""

    def test_minimal_construction(self, gom_deepwater_spec: FieldDevelopmentSpec) -> None:
        assert gom_deepwater_spec.field_name == "Test Deepwater Alpha"
        assert gom_deepwater_spec.water_depth_m == 1500.0
        assert gom_deepwater_spec.fiscal_regime is FiscalRegime.US

    def test_defaults_applied(self, gom_deepwater_spec: FieldDevelopmentSpec) -> None:
        assert gom_deepwater_spec.distance_to_infra_km is None
        assert gom_deepwater_spec.fluid_type == "oil"
        assert gom_deepwater_spec.opex_usd_per_bbl is None
        assert gom_deepwater_spec.capex_usd_mm is None

    def test_negative_water_depth_raises(self) -> None:
        with pytest.raises(ValueError, match="water_depth_m"):
            FieldDevelopmentSpec(
                field_name="Bad",
                water_depth_m=-100.0,
                reservoir_size_mmbbl=100.0,
                production_capacity_bopd=50_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=20,
            )

    def test_zero_reservoir_raises(self) -> None:
        with pytest.raises(ValueError, match="reservoir_size_mmbbl"):
            FieldDevelopmentSpec(
                field_name="Bad",
                water_depth_m=1000.0,
                reservoir_size_mmbbl=0.0,
                production_capacity_bopd=50_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=20,
            )

    def test_invalid_fluid_type_raises(self) -> None:
        with pytest.raises(ValueError, match="fluid_type"):
            FieldDevelopmentSpec(
                field_name="Bad",
                water_depth_m=1000.0,
                reservoir_size_mmbbl=100.0,
                production_capacity_bopd=50_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=20,
                fluid_type="hydrogen",
            )

    def test_zero_production_raises(self) -> None:
        with pytest.raises(ValueError, match="production_capacity_bopd"):
            FieldDevelopmentSpec(
                field_name="Bad",
                water_depth_m=1000.0,
                reservoir_size_mmbbl=100.0,
                production_capacity_bopd=0.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=20,
            )

    def test_zero_oil_price_raises(self) -> None:
        with pytest.raises(ValueError, match="oil_price_usd_per_bbl"):
            FieldDevelopmentSpec(
                field_name="Bad",
                water_depth_m=1000.0,
                reservoir_size_mmbbl=100.0,
                production_capacity_bopd=50_000.0,
                oil_price_usd_per_bbl=0.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=20,
            )

    def test_invalid_discount_rate_raises(self) -> None:
        with pytest.raises(ValueError, match="discount_rate"):
            FieldDevelopmentSpec(
                field_name="Bad",
                water_depth_m=1000.0,
                reservoir_size_mmbbl=100.0,
                production_capacity_bopd=50_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=1.5,
                fiscal_regime=FiscalRegime.US,
                field_life_years=20,
            )

    def test_zero_field_life_raises(self) -> None:
        with pytest.raises(ValueError, match="field_life_years"):
            FieldDevelopmentSpec(
                field_name="Bad",
                water_depth_m=1000.0,
                reservoir_size_mmbbl=100.0,
                production_capacity_bopd=50_000.0,
                oil_price_usd_per_bbl=70.0,
                discount_rate=0.10,
                fiscal_regime=FiscalRegime.US,
                field_life_years=0,
            )


# ---------------------------------------------------------------------------
# evaluate_field_development — end-to-end pipeline
# ---------------------------------------------------------------------------


class TestEvaluateFieldDevelopment:
    """Test the end-to-end screening pipeline."""

    def test_returns_field_development_result(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        result = evaluate_field_development(gom_deepwater_spec)
        assert isinstance(result, FieldDevelopmentResult)

    def test_concept_selection_populated(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        result = evaluate_field_development(gom_deepwater_spec)
        assert isinstance(result.concept, ConceptSelectionResult)
        assert result.concept.selected_host is not None
        assert len(result.concept.ranked_options) > 0

    def test_economics_populated(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        result = evaluate_field_development(gom_deepwater_spec)
        assert isinstance(result.economics, EconomicsResult)
        assert result.economics.field_name == "Test Deepwater Alpha"
        assert result.economics.fiscal_regime is FiscalRegime.US

    def test_economics_host_matches_selected_concept(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        """The economics evaluation uses the concept-selected host type."""
        result = evaluate_field_development(gom_deepwater_spec)
        selected_host = result.concept.selected_host.value
        # The economics basis string includes the host type
        assert selected_host in result.economics.basis

    def test_npv_is_finite(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        result = evaluate_field_development(gom_deepwater_spec)
        assert math.isfinite(result.economics.metrics.npv_usd_mm)

    def test_costs_are_positive(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        result = evaluate_field_development(gom_deepwater_spec)
        assert result.economics.costs.capex_usd_mm > 0
        assert result.economics.costs.opex_usd_mm_per_yr > 0
        assert result.economics.costs.abex_usd_mm > 0

    def test_tieback_scenario(self, tieback_spec: FieldDevelopmentSpec) -> None:
        """A small shallow field near infrastructure should produce valid results."""
        result = evaluate_field_development(tieback_spec)
        assert isinstance(result, FieldDevelopmentResult)
        assert math.isfinite(result.economics.metrics.npv_usd_mm)

    def test_non_us_fiscal_regime(self, norway_spec: FieldDevelopmentSpec) -> None:
        """Norwegian fiscal regime flows through to economics."""
        result = evaluate_field_development(norway_spec)
        assert result.economics.fiscal_regime is FiscalRegime.Norway

    def test_carbon_cost_flows_through(self) -> None:
        """carbon_cost_usd_per_tonne on spec reaches economics evaluation."""
        spec = FieldDevelopmentSpec(
            field_name="Carbon Test",
            water_depth_m=1000.0,
            reservoir_size_mmbbl=200.0,
            production_capacity_bopd=50_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.UK,
            field_life_years=20,
            carbon_cost_usd_per_tonne=100.0,
        )
        result = evaluate_field_development(spec)
        # With carbon cost, NPV should be lower than without
        spec_no_carbon = FieldDevelopmentSpec(
            field_name="Carbon Test",
            water_depth_m=1000.0,
            reservoir_size_mmbbl=200.0,
            production_capacity_bopd=50_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.UK,
            field_life_years=20,
        )
        result_no_carbon = evaluate_field_development(spec_no_carbon)
        assert result.economics.metrics.npv_usd_mm < result_no_carbon.economics.metrics.npv_usd_mm


# ---------------------------------------------------------------------------
# compare_concepts — side-by-side evaluation
# ---------------------------------------------------------------------------


class TestCompareConceptsMode:
    """Test the compare-concepts side-by-side evaluation."""

    def test_returns_concept_comparison(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        comparison = compare_concepts(gom_deepwater_spec)
        assert isinstance(comparison, ConceptComparison)

    def test_multiple_results_returned(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        comparison = compare_concepts(gom_deepwater_spec, top_n=3)
        # At least 1 viable concept should be evaluated
        assert len(comparison.results) >= 1

    def test_best_property_returns_highest_npv(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        comparison = compare_concepts(gom_deepwater_spec, top_n=3)
        best = comparison.best
        for r in comparison.results:
            assert best.economics.metrics.npv_usd_mm >= r.economics.metrics.npv_usd_mm

    def test_each_result_is_valid(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        comparison = compare_concepts(gom_deepwater_spec, top_n=2)
        for r in comparison.results:
            assert isinstance(r, FieldDevelopmentResult)
            assert isinstance(r.economics, EconomicsResult)
            assert math.isfinite(r.economics.metrics.npv_usd_mm)

    def test_top_n_limits_results(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        comparison = compare_concepts(gom_deepwater_spec, top_n=1)
        assert len(comparison.results) == 1

    def test_zero_viable_concepts_raises(self) -> None:
        """Water depth below all hard limits should raise ValueError."""
        spec = FieldDevelopmentSpec(
            field_name="Too Shallow",
            water_depth_m=50.0,  # below all host depth limits (min 100 m)
            reservoir_size_mmbbl=100.0,
            production_capacity_bopd=50_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=20,
        )
        with pytest.raises(ValueError, match="No viable host concepts"):
            compare_concepts(spec)

    def test_top_n_zero_raises(self) -> None:
        """top_n=0 should raise ValueError."""
        spec = FieldDevelopmentSpec(
            field_name="Bad N",
            water_depth_m=1000.0,
            reservoir_size_mmbbl=200.0,
            production_capacity_bopd=50_000.0,
            oil_price_usd_per_bbl=70.0,
            discount_rate=0.10,
            fiscal_regime=FiscalRegime.US,
            field_life_years=20,
        )
        with pytest.raises(ValueError, match="top_n"):
            compare_concepts(spec, top_n=0)

    def test_distinct_host_types_evaluated(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        """Each result in compare_concepts should evaluate a distinct host type."""
        comparison = compare_concepts(gom_deepwater_spec, top_n=3)
        hosts = [r.evaluated_host for r in comparison.results]
        assert len(set(hosts)) == len(hosts)

    def test_evaluated_host_set_on_result(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        """evaluated_host should be populated on each result."""
        comparison = compare_concepts(gom_deepwater_spec, top_n=2)
        for r in comparison.results:
            assert r.evaluated_host != ""
            assert r.evaluated_host in r.economics.basis


# ---------------------------------------------------------------------------
# evaluate_field_development — evaluated_host
# ---------------------------------------------------------------------------


class TestEvaluatedHostField:
    """Test evaluated_host is set correctly on FieldDevelopmentResult."""

    def test_evaluate_field_development_sets_evaluated_host(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        result = evaluate_field_development(gom_deepwater_spec)
        assert result.evaluated_host == result.concept.selected_host.value

    def test_evaluated_host_matches_economics_basis(
        self, gom_deepwater_spec: FieldDevelopmentSpec
    ) -> None:
        result = evaluate_field_development(gom_deepwater_spec)
        assert result.evaluated_host in result.economics.basis


# ---------------------------------------------------------------------------
# Package-level imports
# ---------------------------------------------------------------------------


class TestPackageLevelImports:
    """Verify workflow symbols are accessible from the package."""

    def test_import_from_package(self) -> None:
        from digitalmodel.field_development import (
            FieldDevelopmentSpec,
            FieldDevelopmentResult,
            ConceptComparison,
            evaluate_field_development,
            compare_concepts,
        )
        assert callable(evaluate_field_development)
        assert callable(compare_concepts)
