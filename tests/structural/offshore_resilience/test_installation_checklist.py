"""
Tests for installation-centric engineering checklist.

Verifies that checklist items are categorized correctly for both
fixed/floating structures and offshore wind foundations, and that
parameter range validation works as expected.
"""

import pytest

from digitalmodel.structural.offshore_resilience.installation_checklist import (
    InstallationChecklist,
    ChecklistItem,
    ChecklistStatus,
    HammerType,
    FoundationType,
    build_structure_checklist,
    build_wind_foundation_checklist,
)


# ---------------------------------------------------------------------------
# ChecklistItem
# ---------------------------------------------------------------------------

class TestChecklistItem:
    def test_item_defaults_to_pending(self):
        item = ChecklistItem(
            category="vessel_spread",
            parameter="crane_capacity_tonnes",
            description="Maximum crane hook load available",
            min_value=200.0,
            max_value=5000.0,
            unit="t",
        )
        assert item.status == ChecklistStatus.PENDING
        assert item.actual_value is None

    def test_item_passes_when_value_in_range(self):
        item = ChecklistItem(
            category="weather_window",
            parameter="max_hs_m",
            description="Maximum significant wave height for lift operations",
            min_value=0.0,
            max_value=2.5,
            unit="m",
        )
        item.set_value(1.8)
        assert item.status == ChecklistStatus.PASS

    def test_item_fails_when_value_exceeds_max(self):
        item = ChecklistItem(
            category="weather_window",
            parameter="max_hs_m",
            description="Max Hs for operations",
            min_value=0.0,
            max_value=2.5,
            unit="m",
        )
        item.set_value(3.0)
        assert item.status == ChecklistStatus.FAIL

    def test_item_fails_when_value_below_min(self):
        item = ChecklistItem(
            category="vessel_spread",
            parameter="crane_capacity_tonnes",
            description="Crane capacity",
            min_value=200.0,
            max_value=5000.0,
            unit="t",
        )
        item.set_value(150.0)
        assert item.status == ChecklistStatus.FAIL


# ---------------------------------------------------------------------------
# Fixed/Floating structure checklist
# ---------------------------------------------------------------------------

class TestBuildStructureChecklist:
    def test_checklist_contains_vessel_spread_items(self):
        cl = build_structure_checklist(structure_type="fixed")
        categories = [item.category for item in cl.items]
        assert "vessel_spread" in categories

    def test_checklist_contains_weather_window_items(self):
        cl = build_structure_checklist(structure_type="fixed")
        categories = [item.category for item in cl.items]
        assert "weather_window" in categories

    def test_checklist_contains_pile_hammer_items(self):
        cl = build_structure_checklist(structure_type="fixed")
        categories = [item.category for item in cl.items]
        assert "pile_hammer" in categories

    def test_floating_checklist_contains_towout_stability(self):
        cl = build_structure_checklist(structure_type="floating")
        categories = [item.category for item in cl.items]
        assert "towout_stability" in categories

    def test_fixed_checklist_no_towout_stability(self):
        cl = build_structure_checklist(structure_type="fixed")
        categories = [item.category for item in cl.items]
        assert "towout_stability" not in categories

    def test_invalid_structure_type_raises(self):
        with pytest.raises(ValueError, match="structure_type"):
            build_structure_checklist(structure_type="submarine")

    def test_checklist_completion_ratio_zero_before_input(self):
        cl = build_structure_checklist(structure_type="fixed")
        assert cl.completion_ratio == pytest.approx(0.0)

    def test_checklist_all_pass_after_setting_valid_values(self):
        cl = build_structure_checklist(structure_type="fixed")
        for item in cl.items:
            midpoint = (item.min_value + item.max_value) / 2.0
            item.set_value(midpoint)
        assert cl.completion_ratio == pytest.approx(1.0)
        assert cl.pass_count == len(cl.items)


# ---------------------------------------------------------------------------
# Offshore wind foundation checklist
# ---------------------------------------------------------------------------

class TestBuildWindFoundationChecklist:
    def test_monopile_checklist_has_pile_run_analysis(self):
        cl = build_wind_foundation_checklist(
            foundation_type=FoundationType.MONOPILE
        )
        params = [item.parameter for item in cl.items]
        assert any("pile_run" in p for p in params)

    def test_monopile_checklist_has_hammer_selection(self):
        cl = build_wind_foundation_checklist(
            foundation_type=FoundationType.MONOPILE
        )
        params = [item.parameter for item in cl.items]
        assert any("hammer" in p for p in params)

    def test_jacket_wind_checklist_has_fatigue_accumulation(self):
        cl = build_wind_foundation_checklist(
            foundation_type=FoundationType.JACKET
        )
        params = [item.parameter for item in cl.items]
        assert any("fatigue" in p for p in params)

    def test_hammer_type_enum_values(self):
        """HammerType enum has expected offshore installation types."""
        assert HammerType.HYDRAULIC_IMPACT in HammerType
        assert HammerType.VIBRATORY in HammerType
        assert HammerType.DRILLED_GROUTED in HammerType


# ---------------------------------------------------------------------------
# InstallationChecklist summary
# ---------------------------------------------------------------------------

class TestInstallationChecklistSummary:
    def test_summary_counts_pass_fail_pending(self):
        cl = build_structure_checklist(structure_type="fixed")
        # Set half the items to valid values
        half = len(cl.items) // 2
        for item in cl.items[:half]:
            midpoint = (item.min_value + item.max_value) / 2.0
            item.set_value(midpoint)
        summary = cl.summary()
        assert summary["pass"] == half
        assert summary["pending"] == len(cl.items) - half
        assert summary["total"] == len(cl.items)

    def test_summary_is_complete_returns_false_with_pending(self):
        cl = build_structure_checklist(structure_type="fixed")
        assert cl.is_complete is False

    def test_summary_is_complete_returns_true_all_pass(self):
        cl = build_structure_checklist(structure_type="fixed")
        for item in cl.items:
            item.set_value((item.min_value + item.max_value) / 2.0)
        assert cl.is_complete is True
