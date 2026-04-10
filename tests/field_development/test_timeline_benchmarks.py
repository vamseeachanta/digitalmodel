# ABOUTME: Tests for project timeline benchmark analysis — durations and schedule distributions.
# ABOUTME: Issue #2060 — TDD tests for timeline.py (Option B implementation).
"""Tests for digitalmodel.field_development.timeline (#2060)."""

import pytest

from digitalmodel.field_development.benchmarks import SubseaProject, load_projects
from digitalmodel.field_development.timeline import (
    timeline_duration_stats,
    duration_stats_by_concept_type,
    schedule_distributions,
)


# ---------------------------------------------------------------------------
# Synthetic project fixtures
# ---------------------------------------------------------------------------

def _make_project(
    name: str,
    concept_type: str | None = None,
    year_concept: int | None = None,
    year_feed: int | None = None,
    year_fid: int | None = None,
    year_first_oil: int | None = None,
) -> SubseaProject:
    return SubseaProject(
        name=name,
        concept_type=concept_type,
        year_concept=year_concept,
        year_feed=year_feed,
        year_fid=year_fid,
        year_first_oil=year_first_oil,
    )


# Five projects with complete timeline data across two concept types
PROJECTS_WITH_TIMELINE = [
    # FPSO — concept_to_feed=2, feed_to_fid=2, fid_to_first_oil=4, total=8
    _make_project("Alpha", "FPSO", 2000, 2002, 2004, 2008),
    # FPSO — concept_to_feed=3, feed_to_fid=2, fid_to_first_oil=5, total=10
    _make_project("Bravo", "FPSO", 2005, 2008, 2010, 2015),
    # FPSO — concept_to_feed=2, feed_to_fid=3, fid_to_first_oil=3, total=8
    _make_project("Charlie", "FPSO", 2010, 2012, 2015, 2018),
    # Spar — concept_to_feed=3, feed_to_fid=2, fid_to_first_oil=4, total=9
    _make_project("Delta", "Spar", 2002, 2005, 2007, 2011),
    # Spar — concept_to_feed=2, feed_to_fid=3, fid_to_first_oil=5, total=10
    _make_project("Echo", "Spar", 2008, 2010, 2013, 2018),
]


# ---------------------------------------------------------------------------
# Class 1: SubseaProject timeline fields
# ---------------------------------------------------------------------------

class TestSubseaProjectTimelineFields:
    """SubseaProject exposes the 4 optional timeline fields."""

    def test_timeline_fields_default_to_none(self):
        p = SubseaProject(name="Test")
        assert p.year_concept is None
        assert p.year_feed is None
        assert p.year_fid is None
        assert p.year_first_oil is None

    def test_timeline_fields_accept_int_values(self):
        p = SubseaProject(
            name="Test",
            year_concept=2000,
            year_feed=2003,
            year_fid=2005,
            year_first_oil=2009,
        )
        assert p.year_concept == 2000
        assert p.year_feed == 2003
        assert p.year_fid == 2005
        assert p.year_first_oil == 2009

    def test_existing_fields_unaffected(self):
        """Ensure existing SubseaProject fields still work after timeline addition."""
        p = SubseaProject(
            name="Perdido",
            operator="Shell",
            water_depth_m=2438.0,
            concept_type="Spar",
            num_wells=22,
        )
        assert p.operator == "Shell"
        assert p.water_depth_m == 2438.0
        assert p.num_wells == 22
        assert p.year_concept is None  # not set


# ---------------------------------------------------------------------------
# Class 2: load_projects timeline parsing
# ---------------------------------------------------------------------------

class TestLoadProjectsTimelineParsing:
    """load_projects() correctly parses the 4 timeline year fields."""

    def test_parses_int_year_fields(self):
        records = [{"name": "Alpha", "year_concept": 2000, "year_fid": 2005}]
        projects = load_projects(records)
        assert projects[0].year_concept == 2000
        assert projects[0].year_fid == 2005

    def test_parses_string_year_fields(self):
        records = [{"name": "Beta", "year_first_oil": "2014", "year_feed": "2008"}]
        projects = load_projects(records)
        assert projects[0].year_first_oil == 2014
        assert projects[0].year_feed == 2008

    def test_parses_float_string_year_fields(self):
        """Year values from CSV may be '2004.0' — must coerce to int."""
        records = [{"name": "Gamma", "year_fid": "2004.0"}]
        projects = load_projects(records)
        assert projects[0].year_fid == 2004

    def test_missing_year_fields_are_none(self):
        records = [{"name": "Delta"}]
        projects = load_projects(records)
        assert projects[0].year_concept is None
        assert projects[0].year_feed is None
        assert projects[0].year_fid is None
        assert projects[0].year_first_oil is None

    def test_unparseable_year_field_is_none(self):
        records = [{"name": "Epsilon", "year_fid": "N/A"}]
        projects = load_projects(records)
        assert projects[0].year_fid is None

    def test_all_four_fields_in_single_record(self):
        records = [{
            "name": "Zeta",
            "year_concept": 2001,
            "year_feed": 2003,
            "year_fid": 2005,
            "year_first_oil": 2009,
        }]
        projects = load_projects(records)
        p = projects[0]
        assert p.year_concept == 2001
        assert p.year_feed == 2003
        assert p.year_fid == 2005
        assert p.year_first_oil == 2009


# ---------------------------------------------------------------------------
# Class 3: inter-phase duration math
# ---------------------------------------------------------------------------

class TestInterPhaseDurationMath:
    """timeline_duration_stats() computes correct inter-phase durations."""

    def test_returns_all_four_phase_keys(self):
        stats = timeline_duration_stats(PROJECTS_WITH_TIMELINE)
        assert set(stats.keys()) == {
            "concept_to_feed",
            "feed_to_fid",
            "fid_to_first_oil",
            "concept_to_first_oil",
        }

    def test_concept_to_feed_count(self):
        stats = timeline_duration_stats(PROJECTS_WITH_TIMELINE)
        assert stats["concept_to_feed"]["count"] == 5

    def test_concept_to_feed_mean(self):
        # durations: 2, 3, 2, 3, 2 → mean = 12/5 = 2.4
        stats = timeline_duration_stats(PROJECTS_WITH_TIMELINE)
        assert stats["concept_to_feed"]["mean"] == pytest.approx(2.4)

    def test_fid_to_first_oil_mean(self):
        # durations: 4, 5, 3, 4, 5 → mean = 21/5 = 4.2
        stats = timeline_duration_stats(PROJECTS_WITH_TIMELINE)
        assert stats["fid_to_first_oil"]["mean"] == pytest.approx(4.2)

    def test_retrograde_timeline_excluded(self):
        """Projects where to_year < from_year must be silently skipped."""
        bad = [_make_project("Bad", "FPSO", year_feed=2010, year_fid=2008)]
        stats = timeline_duration_stats(bad)
        assert stats["feed_to_fid"]["count"] == 0

    def test_partial_timeline_excluded_from_pair(self):
        """A project with only year_concept (no year_feed) is excluded from concept_to_feed."""
        partial = [_make_project("Partial", "FPSO", year_concept=2005)]
        stats = timeline_duration_stats(partial)
        assert stats["concept_to_feed"]["count"] == 0

    def test_empty_project_list(self):
        stats = timeline_duration_stats([])
        for pair in ("concept_to_feed", "feed_to_fid", "fid_to_first_oil", "concept_to_first_oil"):
            assert stats[pair]["count"] == 0
            assert stats[pair]["mean"] == 0.0

    def test_stats_keys_present(self):
        stats = timeline_duration_stats(PROJECTS_WITH_TIMELINE)
        for pair_stats in stats.values():
            assert {"count", "mean", "p10", "p50", "p90"} == set(pair_stats.keys())


# ---------------------------------------------------------------------------
# Class 4: grouping by concept type
# ---------------------------------------------------------------------------

class TestGroupingByConceptType:
    """duration_stats_by_concept_type() groups projects correctly."""

    def test_returns_both_concept_types(self):
        result = duration_stats_by_concept_type(PROJECTS_WITH_TIMELINE)
        assert set(result.keys()) == {"FPSO", "Spar"}

    def test_fpso_concept_to_feed_count(self):
        result = duration_stats_by_concept_type(PROJECTS_WITH_TIMELINE)
        assert result["FPSO"]["concept_to_feed"]["count"] == 3

    def test_spar_concept_to_feed_count(self):
        result = duration_stats_by_concept_type(PROJECTS_WITH_TIMELINE)
        assert result["Spar"]["concept_to_feed"]["count"] == 2

    def test_fpso_fid_to_first_oil_mean(self):
        # FPSO fid_to_first_oil: 4, 5, 3 → mean = 4.0
        result = duration_stats_by_concept_type(PROJECTS_WITH_TIMELINE)
        assert result["FPSO"]["fid_to_first_oil"]["mean"] == pytest.approx(4.0)

    def test_projects_without_concept_type_excluded(self):
        mixed = PROJECTS_WITH_TIMELINE + [
            _make_project("Unknown", None, 2000, 2002, 2004, 2008)
        ]
        result = duration_stats_by_concept_type(mixed)
        assert "None" not in result
        assert None not in result

    def test_empty_list_returns_empty_dict(self):
        assert duration_stats_by_concept_type([]) == {}

    def test_result_is_sorted_by_concept_type(self):
        result = duration_stats_by_concept_type(PROJECTS_WITH_TIMELINE)
        keys = list(result.keys())
        assert keys == sorted(keys)

    def test_inner_dict_has_all_phase_pairs(self):
        result = duration_stats_by_concept_type(PROJECTS_WITH_TIMELINE)
        for ctype_stats in result.values():
            assert set(ctype_stats.keys()) == {
                "concept_to_feed",
                "feed_to_fid",
                "fid_to_first_oil",
                "concept_to_first_oil",
            }


# ---------------------------------------------------------------------------
# Class 5: P10/P50/P90 schedule distributions
# ---------------------------------------------------------------------------

class TestScheduleDistributions:
    """schedule_distributions() returns only P10/P50/P90 per phase pair."""

    def test_returns_four_phase_keys(self):
        dists = schedule_distributions(PROJECTS_WITH_TIMELINE)
        assert set(dists.keys()) == {
            "concept_to_feed",
            "feed_to_fid",
            "fid_to_first_oil",
            "concept_to_first_oil",
        }

    def test_only_percentile_keys_present(self):
        """count and mean must NOT appear — only p10/p50/p90."""
        dists = schedule_distributions(PROJECTS_WITH_TIMELINE)
        for pair_dists in dists.values():
            assert set(pair_dists.keys()) == {"p10", "p50", "p90"}
            assert "count" not in pair_dists
            assert "mean" not in pair_dists

    def test_p10_le_p50_le_p90(self):
        dists = schedule_distributions(PROJECTS_WITH_TIMELINE)
        for label, d in dists.items():
            assert d["p10"] <= d["p50"] <= d["p90"], (
                f"{label}: p10={d['p10']} p50={d['p50']} p90={d['p90']}"
            )

    def test_empty_list_returns_zeros(self):
        dists = schedule_distributions([])
        for d in dists.values():
            assert d == {"p10": 0.0, "p50": 0.0, "p90": 0.0}

    def test_single_project_all_percentiles_equal(self):
        """With one data point, p10 == p50 == p90 == that value."""
        projects = [_make_project("Solo", "FPSO", 2000, 2003, 2005, 2010)]
        dists = schedule_distributions(projects)
        d = dists["concept_to_feed"]
        assert d["p10"] == d["p50"] == d["p90"] == pytest.approx(3.0)

    def test_concept_to_first_oil_p50_reasonable(self):
        # concept_to_first_oil: 8, 10, 8, 9, 10 → sorted: 8, 8, 9, 10, 10
        # p50 (median) should be around 9
        dists = schedule_distributions(PROJECTS_WITH_TIMELINE)
        d = dists["concept_to_first_oil"]
        assert 8.0 <= d["p50"] <= 10.0


# ---------------------------------------------------------------------------
# Boundary tests added after Codex cross-review (#2060)
# ---------------------------------------------------------------------------

class TestBoundaryEdgeCases:
    """Zero-duration and small-n edge cases."""

    def test_zero_duration_phase_included(self):
        """to_yr == from_yr yields 0.0, not excluded (same-year phase transition)."""
        p = _make_project("Instant", "FPSO", year_feed=2010, year_fid=2010)
        stats = timeline_duration_stats([p])
        assert stats["feed_to_fid"]["count"] == 1
        assert stats["feed_to_fid"]["mean"] == pytest.approx(0.0)

    def test_zero_duration_not_confused_with_retrograde(self):
        """Ensure zero-duration (valid) vs retrograde (invalid) are handled distinctly."""
        projects = [
            _make_project("ZeroDur", "FPSO", year_feed=2010, year_fid=2010),  # 0 yrs
            _make_project("Retro",   "FPSO", year_feed=2012, year_fid=2011),  # excluded
        ]
        stats = timeline_duration_stats(projects)
        assert stats["feed_to_fid"]["count"] == 1  # only ZeroDur included
        assert stats["feed_to_fid"]["mean"] == pytest.approx(0.0)

    def test_duration_stats_by_concept_type_n2_per_type(self):
        """duration_stats_by_concept_type works for concept types with exactly 2 projects."""
        projects = [
            _make_project("A", "Semi", year_concept=2000, year_feed=2003),
            _make_project("B", "Semi", year_concept=2005, year_feed=2009),
        ]
        result = duration_stats_by_concept_type(projects)
        semi = result["Semi"]
        assert semi["concept_to_feed"]["count"] == 2
        assert semi["concept_to_feed"]["mean"] == pytest.approx(3.5)
        # With 2 data points P10 <= P50 <= P90 must hold
        d = semi["concept_to_feed"]
        assert d["p10"] <= d["p50"] <= d["p90"]
