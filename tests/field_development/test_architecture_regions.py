# ABOUTME: Synthetic coverage for regional tieback distance statistics.
"""Regional segmentation and independent depth/fluid statistics for #1779."""

from copy import deepcopy

import pytest

from digitalmodel.field_development.architecture_patterns import tieback_stats_segmented
from digitalmodel.field_development.benchmarks import SubseaProject


@pytest.fixture
def regional_projects():
    """Five synthetic tiebacks with distances in km and depths in metres."""
    return [
        SubseaProject(
            name=f"synthetic_{index}",
            region=region,
            tieback_distance_km=distance,
            water_depth_m=depth,
            fluid_type=fluid,
        )
        for index, (region, distance, depth, fluid) in enumerate(
            [
                ("GoM", 12.0, 1800, "gas"),
                ("GoM", 18.0, 1600, "oil"),
                ("West Africa", 8.5, 1200, "oil"),
                ("Brazil", 25.0, 2400, "oil"),
                ("North Sea", 6.0, 350, "gas"),
            ]
        )
    ]


def test_by_region(regional_projects):
    original = deepcopy(regional_projects)
    result = tieback_stats_segmented(regional_projects)
    assert result["by_region"] == {
        "GoM": {"count": 2, "min": 12.0, "max": 18.0, "mean": 15.0},
        "West Africa": {"count": 1, "min": 8.5, "max": 8.5, "mean": 8.5},
        "Brazil": {"count": 1, "min": 25.0, "max": 25.0, "mean": 25.0},
        "North Sea": {"count": 1, "min": 6.0, "max": 6.0, "mean": 6.0},
    }
    assert sum(stats["count"] for stats in result["by_region"].values()) == 5
    assert regional_projects == original


def test_existing_partitions_are_preserved(regional_projects):
    result = tieback_stats_segmented(regional_projects)
    assert result["by_depth_band"] == {
        "1500m+": {"count": 3, "min": 12.0, "max": 25.0, "mean": 55.0 / 3},
        "800-1500m": {"count": 1, "min": 8.5, "max": 8.5, "mean": 8.5},
        "300-800m": {"count": 1, "min": 6.0, "max": 6.0, "mean": 6.0},
    }
    assert result["by_fluid_type"] == {
        "gas": {"count": 2, "min": 6.0, "max": 12.0, "mean": 9.0},
        "oil": {"count": 3, "min": 8.5, "max": 25.0, "mean": 51.5 / 3},
    }


@pytest.mark.parametrize("missing_distance", [False, True])
def test_empty_or_missing_distance_has_no_groups(missing_distance):
    projects = (
        [SubseaProject(name="synthetic_missing", region="GoM")]
        if missing_distance
        else []
    )
    assert tieback_stats_segmented(projects) == {
        "by_depth_band": {},
        "by_fluid_type": {},
        "by_region": {},
    }


def test_missing_region_preserves_other_groups():
    project = SubseaProject(
        name="synthetic_unlocated",
        tieback_distance_km=5.0,
        water_depth_m=500,
        fluid_type="gas",
    )
    assert tieback_stats_segmented([project]) == {
        "by_depth_band": {
            "300-800m": {"count": 1, "min": 5.0, "max": 5.0, "mean": 5.0},
        },
        "by_fluid_type": {
            "gas": {"count": 1, "min": 5.0, "max": 5.0, "mean": 5.0},
        },
        "by_region": {},
    }


@pytest.mark.parametrize("depth,fluid", [(None, "gas"), (500, None), (None, None)])
def test_region_is_independent_of_depth_and_fluid(depth, fluid):
    project = SubseaProject(
        name="synthetic_sparse",
        region="GoM",
        tieback_distance_km=5.0,
        water_depth_m=depth,
        fluid_type=fluid,
    )
    stats = {"count": 1, "min": 5.0, "max": 5.0, "mean": 5.0}
    assert tieback_stats_segmented([project]) == {
        "by_depth_band": {} if depth is None else {"300-800m": stats},
        "by_fluid_type": {} if fluid is None else {"gas": stats},
        "by_region": {"GoM": stats},
    }


def test_zero_distance_is_an_observation(regional_projects):
    regional_projects.append(
        SubseaProject(name="synthetic_zero", region="GoM", tieback_distance_km=0.0)
    )
    assert tieback_stats_segmented(regional_projects)["by_region"]["GoM"] == {
        "count": 3,
        "min": 0.0,
        "max": 18.0,
        "mean": 10.0,
    }


def test_region_labels_are_used_verbatim():
    labels = ["GoM", "gom", " GoM ", ""]
    projects = [
        SubseaProject(name="synthetic_label", region=label, tieback_distance_km=5.0)
        for label in labels
    ]
    assert tieback_stats_segmented(projects)["by_region"] == {
        label: {"count": 1, "min": 5.0, "max": 5.0, "mean": 5.0}
        for label in labels
    }
