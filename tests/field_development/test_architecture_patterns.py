# ABOUTME: Tests for subsea architecture pattern analysis functions.
# ABOUTME: Issue #2058 — TDD tests written before implementation.
"""Tests for digitalmodel.field_development.architecture_patterns (#2058)."""

import pytest

from digitalmodel.field_development.benchmarks import (
    SubseaProject,
    load_projects,
)
from digitalmodel.field_development.architecture_patterns import (
    layout_distribution,
    tieback_stats_segmented,
    equipment_stats_by_concept,
    flowline_trends_by_depth,
)


# ---------------------------------------------------------------------------
# Fixture: reuse FIXTURE_RECORDS from test_benchmarks (with new fields)
# ---------------------------------------------------------------------------

FIXTURE_RECORDS = [
    {
        "name": "Alpha",
        "operator": "Shell",
        "water_depth_m": 200,
        "concept_type": "Fixed Platform",
        "tieback_distance_km": None,
        "num_wells": 6,
        "num_trees": 0,
        "num_manifolds": 0,
        "fluid_type": "oil",
        "region": "GoM",
    },
    {
        "name": "Bravo",
        "operator": "BP",
        "water_depth_m": 500,
        "concept_type": "TLP",
        "tieback_distance_km": None,
        "num_wells": 12,
        "num_trees": 8,
        "num_manifolds": 2,
        "fluid_type": "oil",
        "region": "GoM",
        "flowline_diameter_in": 8.0,
        "flowline_material": "Carbon Steel",
        "layout_type": "direct_tieback",
    },
    {
        "name": "Charlie",
        "operator": "Shell",
        "water_depth_m": 900,
        "concept_type": "TLP",
        "tieback_distance_km": None,
        "num_wells": 16,
        "num_trees": 12,
        "num_manifolds": 3,
        "fluid_type": "oil",
        "region": "GoM",
    },
    {
        "name": "Delta",
        "operator": "TotalEnergies",
        "water_depth_m": 1200,
        "concept_type": "Semi",
        "tieback_distance_km": 8.5,
        "num_wells": 20,
        "num_trees": 16,
        "num_manifolds": 4,
        "fluid_type": "oil",
        "region": "West Africa",
        "flowline_diameter_in": 10.75,
        "flowline_material": "Duplex",
        "layout_type": "daisy_chain",
    },
    {
        "name": "Echo",
        "operator": "Equinor",
        "water_depth_m": 1800,
        "concept_type": "Spar",
        "tieback_distance_km": 12.0,
        "num_wells": 10,
        "num_trees": 10,
        "num_manifolds": 2,
        "fluid_type": "gas",
        "region": "GoM",
    },
    {
        "name": "Foxtrot",
        "operator": "Shell",
        "water_depth_m": 2400,
        "concept_type": "FPSO",
        "tieback_distance_km": 25.0,
        "num_wells": 24,
        "num_trees": 20,
        "num_manifolds": 5,
        "fluid_type": "oil",
        "region": "Brazil",
        "flowline_diameter_in": 12.0,
        "flowline_material": "Flexible",
        "layout_type": "star",
    },
    {
        "name": "Golf",
        "operator": "BP",
        "water_depth_m": 350,
        "concept_type": "Subsea Tieback",
        "tieback_distance_km": 6.0,
        "num_wells": 4,
        "num_trees": 4,
        "num_manifolds": 1,
        "fluid_type": "gas",
        "region": "North Sea",
        "flowline_diameter_in": 6.0,
        "flowline_material": "Carbon Steel",
        "layout_type": "direct_tieback",
    },
    {
        "name": "Hotel",
        "operator": "LLOG",
        "water_depth_m": 1600,
        "concept_type": "Subsea Tieback",
        "tieback_distance_km": 18.0,
        "num_wells": 3,
        "num_trees": 3,
        "num_manifolds": 1,
        "fluid_type": "oil",
        "region": "GoM",
    },
]


@pytest.fixture
def projects():
    return load_projects(FIXTURE_RECORDS)


# ---------------------------------------------------------------------------
# layout_distribution
# ---------------------------------------------------------------------------

class TestLayoutDistribution:
    """Tests for layout_distribution (#2058)."""

    def test_returns_dict_keyed_by_concept(self, projects):
        result = layout_distribution(projects)
        assert isinstance(result, dict)
        # Should have entries for concepts that have layout_type set
        assert any(key in result for key in ("TLP", "Semi", "FPSO", "Subsea Tieback"))

    def test_counts_per_concept(self, projects):
        result = layout_distribution(projects)
        # Bravo (TLP, direct_tieback) and Golf (Subsea Tieback, direct_tieback)
        # both have direct_tieback but under different concepts
        if "TLP" in result:
            assert result["TLP"]["direct_tieback"] >= 1
        if "Subsea Tieback" in result:
            assert result["Subsea Tieback"]["direct_tieback"] >= 1

    def test_skips_none_layout(self, projects):
        result = layout_distribution(projects)
        # Alpha, Charlie, Echo, Hotel have no layout_type
        # Their concepts should not appear unless another project provides a layout
        for concept_layouts in result.values():
            assert None not in concept_layouts

    def test_empty_input(self):
        result = layout_distribution([])
        assert result == {}


# ---------------------------------------------------------------------------
# tieback_stats_segmented
# ---------------------------------------------------------------------------

class TestTiebackStatsSegmented:
    """Tests for tieback_stats_segmented (#2058)."""

    def test_returns_nested_dict(self, projects):
        result = tieback_stats_segmented(projects)
        assert isinstance(result, dict)
        # Should have "by_depth_band" and "by_fluid_type" keys
        assert "by_depth_band" in result
        assert "by_fluid_type" in result

    def test_by_depth_band(self, projects):
        result = tieback_stats_segmented(projects)
        by_depth = result["by_depth_band"]
        # Delta (1200m, 8.5km) → 800-1500m band
        # Golf (350m, 6km) → 300-800m band
        assert isinstance(by_depth, dict)
        # At least one band should have stats
        non_empty = {k: v for k, v in by_depth.items() if v.get("count", 0) > 0}
        assert len(non_empty) >= 1

    def test_by_fluid_type(self, projects):
        result = tieback_stats_segmented(projects)
        by_fluid = result["by_fluid_type"]
        # Golf (gas, 6km), Echo (gas, 12km) → gas stats
        # Delta (oil, 8.5km), Foxtrot (oil, 25km), Hotel (oil, 18km) → oil stats
        assert isinstance(by_fluid, dict)
        if "oil" in by_fluid:
            assert by_fluid["oil"]["count"] >= 1
        if "gas" in by_fluid:
            assert by_fluid["gas"]["count"] >= 1

    def test_skips_missing_tieback(self, projects):
        result = tieback_stats_segmented(projects)
        by_depth = result["by_depth_band"]
        # All stats should only count projects with tieback_distance_km set
        total_counted = sum(
            v.get("count", 0) for v in by_depth.values()
        )
        projects_with_tieback = sum(
            1 for p in projects if p.tieback_distance_km is not None
        )
        assert total_counted == projects_with_tieback

    def test_empty_input(self):
        result = tieback_stats_segmented([])
        assert result["by_depth_band"] == {}
        assert result["by_fluid_type"] == {}


# ---------------------------------------------------------------------------
# equipment_stats_by_concept
# ---------------------------------------------------------------------------

class TestEquipmentStatsByConcept:
    """Tests for equipment_stats_by_concept (#2058)."""

    def test_returns_dict_keyed_by_concept(self, projects):
        result = equipment_stats_by_concept(projects)
        assert isinstance(result, dict)
        # Should have entries for concepts present in data
        assert "TLP" in result or "Semi" in result

    def test_trees_and_manifolds_per_concept(self, projects):
        result = equipment_stats_by_concept(projects)
        # Each concept should have trees_per_project and manifolds_per_project stats
        for concept, stats in result.items():
            assert "trees_per_project" in stats
            assert "manifolds_per_project" in stats

    def test_skips_zero_equipment(self, projects):
        result = equipment_stats_by_concept(projects)
        # Alpha (Fixed Platform) has 0 trees and 0 manifolds
        # It should still appear (0 is a valid count, not missing)
        if "Fixed Platform" in result:
            assert result["Fixed Platform"]["trees_per_project"]["count"] >= 1

    def test_empty_input(self):
        result = equipment_stats_by_concept([])
        assert result == {}


# ---------------------------------------------------------------------------
# flowline_trends_by_depth
# ---------------------------------------------------------------------------

class TestFlowlineTrendsByDepth:
    """Tests for flowline_trends_by_depth (#2058)."""

    def test_diameter_stats_per_band(self, projects):
        result = flowline_trends_by_depth(projects)
        assert isinstance(result, dict)
        # At least one depth band should have diameter stats
        has_diameter = any(
            "diameter" in band_data
            for band_data in result.values()
            if isinstance(band_data, dict)
        )
        assert has_diameter

    def test_material_distribution(self, projects):
        result = flowline_trends_by_depth(projects)
        # At least one depth band should have material distribution
        has_material = any(
            "materials" in band_data
            for band_data in result.values()
            if isinstance(band_data, dict)
        )
        assert has_material

    def test_skips_none_flowline(self, projects):
        result = flowline_trends_by_depth(projects)
        # Total projects counted in diameter stats should match those with
        # flowline_diameter_in set (4: Bravo, Delta, Foxtrot, Golf)
        total_diam_count = sum(
            band_data.get("diameter", {}).get("count", 0)
            for band_data in result.values()
            if isinstance(band_data, dict)
        )
        projects_with_diam = sum(
            1 for p in projects if p.flowline_diameter_in is not None
        )
        assert total_diam_count == projects_with_diam

    def test_empty_input(self):
        result = flowline_trends_by_depth([])
        assert result == {}
