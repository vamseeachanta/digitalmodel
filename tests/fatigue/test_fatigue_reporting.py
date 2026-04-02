"""Tests for fatigue_reporting — assessment report generation."""

import pytest

from digitalmodel.fatigue.fatigue_reporting import (
    generate_report,
    report_to_markdown,
    report_to_dict,
    damage_barchart_data,
    inspection_recommendations,
    FatigueCheckLocation,
)


def _sample_locations():
    """Create sample fatigue check locations."""
    return [
        FatigueCheckLocation(
            location_id="Frame-12-Brace-3",
            description="Chord-brace intersection",
            sn_curve="DNV D air",
            cumulative_damage=0.45,
            design_life_years=25.0,
            calculated_life_years=55.6,
            dff=1.0,
            utilisation=0.45,
            stress_range_max=120.0,
            pass_fail="PASS",
        ),
        FatigueCheckLocation(
            location_id="Frame-8-Leg",
            description="Leg splice weld",
            sn_curve="DNV E air",
            cumulative_damage=1.2,
            design_life_years=25.0,
            calculated_life_years=20.8,
            dff=1.0,
            utilisation=1.2,
            stress_range_max=180.0,
            pass_fail="FAIL",
            notes="Consider weld grinding or reinforcement",
        ),
        FatigueCheckLocation(
            location_id="Caisson-Top",
            description="Caisson top plate",
            sn_curve="DNV F seawater_cp",
            cumulative_damage=0.75,
            design_life_years=25.0,
            calculated_life_years=33.3,
            dff=1.0,
            utilisation=0.75,
            stress_range_max=95.0,
            pass_fail="PASS",
        ),
    ]


class TestReportGeneration:
    """Test report generation."""

    def test_generate_report_summary(self):
        locs = _sample_locations()
        report = generate_report(locs, project="Test Platform")
        assert report.summary["total_locations"] == 3
        assert report.summary["pass_count"] == 2
        assert report.summary["fail_count"] == 1
        assert report.summary["overall_status"] == "FAIL"

    def test_markdown_output(self):
        locs = _sample_locations()
        report = generate_report(locs, project="Alpha Platform")
        md = report_to_markdown(report)
        assert "# Fatigue Assessment Report" in md
        assert "Alpha Platform" in md
        assert "Frame-12-Brace-3" in md
        assert "**FAIL**" in md

    def test_dict_output_serialisable(self):
        locs = _sample_locations()
        report = generate_report(locs)
        d = report_to_dict(report)
        assert isinstance(d, dict)
        assert "locations" in d
        assert len(d["locations"]) == 3


class TestBarChartData:
    """Test damage barchart data extraction."""

    def test_barchart_has_correct_lengths(self):
        locs = _sample_locations()
        report = generate_report(locs)
        data = damage_barchart_data(report)
        assert len(data["labels"]) == 3
        assert len(data["damages"]) == 3
        assert len(data["colors"]) == 3

    def test_failed_location_is_red(self):
        locs = _sample_locations()
        report = generate_report(locs)
        data = damage_barchart_data(report)
        # Second location is FAIL → red
        assert data["colors"][1] == "red"

    def test_passed_location_is_green(self):
        locs = _sample_locations()
        report = generate_report(locs)
        data = damage_barchart_data(report)
        assert data["colors"][0] == "green"


class TestInspectionRecommendations:
    """Test inspection interval recommendations."""

    def test_high_utilisation_gets_short_interval(self):
        locs = _sample_locations()
        recs = inspection_recommendations(locs, base_interval_years=5.0)
        # Highest utilisation (1.2) should be first
        assert recs[0]["location_id"] == "Frame-8-Leg"
        assert recs[0]["priority"] == "CRITICAL"
        assert recs[0]["interval_years"] < 5.0

    def test_low_utilisation_gets_long_interval(self):
        locs = _sample_locations()
        recs = inspection_recommendations(locs)
        low_util_rec = [r for r in recs if r["location_id"] == "Frame-12-Brace-3"][0]
        assert low_util_rec["interval_years"] >= 5.0

    def test_recommendations_sorted_by_utilisation(self):
        locs = _sample_locations()
        recs = inspection_recommendations(locs)
        assert recs[0]["utilisation"] >= recs[1]["utilisation"]
