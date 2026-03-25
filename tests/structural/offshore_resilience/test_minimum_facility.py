"""
Tests for MinimumFacilityPlatform and ModularCostComparison.

Hand-calc verification covers:
- BSEE platform classification mapping
- Cost delta and savings calculations
- Water depth range validation
- Expansion capability flag
"""

import pytest

from digitalmodel.structural.offshore_resilience.minimum_facility import (
    MinimumFacilityPlatform,
    ModularCostComparison,
    BseePlatformClass,
    InstallationMethod,
    classify_bsee_platform,
    lifecycle_value_index,
)


# ---------------------------------------------------------------------------
# MinimumFacilityPlatform — construction and properties
# ---------------------------------------------------------------------------

class TestMinimumFacilityPlatform:
    def test_wellhead_platform_basic_fields(self):
        """Wellhead platform stores all fields correctly."""
        p = MinimumFacilityPlatform(
            platform_type="wellhead",
            topsides_weight_tonnes=250.0,
            jacket_weight_tonnes=450.0,
            water_depth_range_ft=(50, 300),
            installation_method=InstallationMethod.CRANE_BARGE,
            max_crane_capacity_tonnes=600.0,
            well_slots=4,
            design_life_years=20,
            expansion_capability=True,
        )
        assert p.platform_type == "wellhead"
        assert p.topsides_weight_tonnes == 250.0
        assert p.well_slots == 4
        assert p.expansion_capability is True

    def test_total_weight_tonnes(self):
        """Total structure weight = topsides + jacket."""
        p = MinimumFacilityPlatform(
            platform_type="riser",
            topsides_weight_tonnes=180.0,
            jacket_weight_tonnes=320.0,
            water_depth_range_ft=(100, 400),
            installation_method=InstallationMethod.CRANE_BARGE,
            max_crane_capacity_tonnes=550.0,
            well_slots=0,
            design_life_years=25,
            expansion_capability=False,
        )
        assert p.total_weight_tonnes == pytest.approx(500.0)

    def test_crane_installable_true_when_weight_within_capacity(self):
        """crane_installable returns True when total weight <= crane capacity."""
        p = MinimumFacilityPlatform(
            platform_type="process_minimum",
            topsides_weight_tonnes=200.0,
            jacket_weight_tonnes=350.0,
            water_depth_range_ft=(50, 250),
            installation_method=InstallationMethod.CRANE_BARGE,
            max_crane_capacity_tonnes=600.0,
            well_slots=2,
            design_life_years=15,
            expansion_capability=True,
        )
        assert p.crane_installable is True

    def test_crane_installable_false_when_weight_exceeds_capacity(self):
        """crane_installable returns False when total weight > crane capacity."""
        p = MinimumFacilityPlatform(
            platform_type="process_minimum",
            topsides_weight_tonnes=400.0,
            jacket_weight_tonnes=800.0,
            water_depth_range_ft=(100, 500),
            installation_method=InstallationMethod.HEAVY_LIFT,
            max_crane_capacity_tonnes=600.0,
            well_slots=6,
            design_life_years=25,
            expansion_capability=False,
        )
        assert p.crane_installable is False

    def test_water_depth_midpoint(self):
        """Midpoint of water depth range is correctly computed."""
        p = MinimumFacilityPlatform(
            platform_type="wellhead",
            topsides_weight_tonnes=100.0,
            jacket_weight_tonnes=200.0,
            water_depth_range_ft=(60, 300),
            installation_method=InstallationMethod.CRANE_BARGE,
            max_crane_capacity_tonnes=400.0,
            well_slots=2,
            design_life_years=20,
            expansion_capability=False,
        )
        assert p.water_depth_midpoint_ft == pytest.approx(180.0)

    def test_invalid_platform_type_raises(self):
        """Unsupported platform_type raises ValueError."""
        with pytest.raises(ValueError, match="platform_type"):
            MinimumFacilityPlatform(
                platform_type="submarine",
                topsides_weight_tonnes=100.0,
                jacket_weight_tonnes=200.0,
                water_depth_range_ft=(0, 100),
                installation_method=InstallationMethod.CRANE_BARGE,
                max_crane_capacity_tonnes=400.0,
                well_slots=1,
                design_life_years=10,
                expansion_capability=False,
            )

    def test_invalid_depth_range_raises(self):
        """water_depth_range_ft where min >= max raises ValueError."""
        with pytest.raises(ValueError, match="water_depth_range_ft"):
            MinimumFacilityPlatform(
                platform_type="wellhead",
                topsides_weight_tonnes=100.0,
                jacket_weight_tonnes=200.0,
                water_depth_range_ft=(300, 100),
                installation_method=InstallationMethod.CRANE_BARGE,
                max_crane_capacity_tonnes=400.0,
                well_slots=2,
                design_life_years=20,
                expansion_capability=False,
            )


# ---------------------------------------------------------------------------
# BSEE platform classification mapping
# ---------------------------------------------------------------------------

class TestClassifyBseePlatform:
    def test_small_fixed_shallow(self):
        """Shallow water + low topsides maps to SMALL_FIXED."""
        result = classify_bsee_platform(
            water_depth_ft=100, topsides_weight_tonnes=200
        )
        assert result == BseePlatformClass.SMALL_FIXED

    def test_large_fixed_deep(self):
        """Deep water + heavy topsides maps to LARGE_FIXED."""
        result = classify_bsee_platform(
            water_depth_ft=900, topsides_weight_tonnes=5000
        )
        assert result == BseePlatformClass.LARGE_FIXED

    def test_floating_very_deep(self):
        """Very deep water maps to FLOATING regardless of weight."""
        result = classify_bsee_platform(
            water_depth_ft=2500, topsides_weight_tonnes=3000
        )
        assert result == BseePlatformClass.FLOATING

    def test_marginal_field_boundary(self):
        """Intermediate weight at medium depth maps to SMALL_FIXED (marginal)."""
        result = classify_bsee_platform(
            water_depth_ft=350, topsides_weight_tonnes=600
        )
        assert result == BseePlatformClass.SMALL_FIXED


# ---------------------------------------------------------------------------
# ModularCostComparison
# ---------------------------------------------------------------------------

class TestModularCostComparison:
    def test_net_savings_positive_for_minimum_facility(self):
        """Net savings = conventional - minimum_facility + heavy_lift savings."""
        cc = ModularCostComparison(
            conventional_capex_usd=45_000_000.0,
            minimum_facility_capex_usd=28_000_000.0,
            heavy_lift_vessel_savings_usd=5_000_000.0,
            installation_weather_window_days=14,
        )
        assert cc.net_savings_usd == pytest.approx(22_000_000.0)

    def test_savings_fraction(self):
        """savings_fraction = net_savings / conventional_capex."""
        cc = ModularCostComparison(
            conventional_capex_usd=40_000_000.0,
            minimum_facility_capex_usd=28_000_000.0,
            heavy_lift_vessel_savings_usd=4_000_000.0,
            installation_weather_window_days=10,
        )
        # net = 40M - 28M + 4M = 16M; fraction = 16/40 = 0.40
        assert cc.savings_fraction == pytest.approx(0.40)

    def test_zero_conventional_capex_raises(self):
        """Zero conventional capex raises ZeroDivisionError guard."""
        with pytest.raises(ValueError, match="conventional_capex_usd"):
            ModularCostComparison(
                conventional_capex_usd=0.0,
                minimum_facility_capex_usd=10_000_000.0,
                heavy_lift_vessel_savings_usd=1_000_000.0,
                installation_weather_window_days=7,
            )


# ---------------------------------------------------------------------------
# Lifecycle value index
# ---------------------------------------------------------------------------

class TestLifecycleValueIndex:
    def test_best_case_score_is_high(self):
        """Easy install + flexible ops + easy decom + expandable => index > 0.75."""
        score = lifecycle_value_index(
            installation_difficulty=1,
            operational_flexibility=5,
            decommissioning_complexity=1,
            expansion_headroom=5,
        )
        assert score > 0.75

    def test_worst_case_score_is_low(self):
        """Hard install + rigid ops + hard decom + no expansion => index < 0.30."""
        score = lifecycle_value_index(
            installation_difficulty=5,
            operational_flexibility=1,
            decommissioning_complexity=5,
            expansion_headroom=1,
        )
        assert score < 0.30

    def test_score_bounded_between_zero_and_one(self):
        """Lifecycle value index is always in [0, 1]."""
        for diff, flex, decom, exp in [
            (1, 1, 1, 1),
            (5, 5, 5, 5),
            (3, 3, 3, 3),
        ]:
            score = lifecycle_value_index(diff, flex, decom, exp)
            assert 0.0 <= score <= 1.0

    def test_invalid_scale_raises(self):
        """Values outside 1-5 scale raise ValueError."""
        with pytest.raises(ValueError):
            lifecycle_value_index(
                installation_difficulty=0,
                operational_flexibility=3,
                decommissioning_complexity=3,
                expansion_headroom=3,
            )
