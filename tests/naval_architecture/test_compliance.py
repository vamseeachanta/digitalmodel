# ABOUTME: Tests for regulatory compliance engine
# ABOUTME: IMO/ABS/DNV binary checks for naval architecture
"""Tests for compliance module — automated regulatory checks."""

import pytest


@pytest.fixture
def stable_vessel():
    """Vessel that passes all stability criteria."""
    return {
        "angles_deg": [0, 10, 20, 30, 40, 50, 60, 70],
        "gz_m": [0.0, 0.30, 0.65, 1.00, 1.30, 1.35, 1.10, 0.70],
        "gm_m": 1.20,
        "freeboard_m": 5.0,
        "lwl_m": 150.0,
        "beam_m": 20.0,
        "draft_m": 7.0,
        "displacement_tonnes": 8000.0,
    }


@pytest.fixture
def unstable_vessel():
    """Vessel that fails stability criteria."""
    return {
        "angles_deg": [0, 10, 20, 30, 40, 50],
        "gz_m": [0.0, 0.04, 0.08, 0.10, 0.05, -0.10],
        "gm_m": 0.08,
        "freeboard_m": 1.0,
        "lwl_m": 100.0,
        "beam_m": 15.0,
        "draft_m": 6.0,
        "displacement_tonnes": 5000.0,
    }


class TestComplianceChecks:
    """Individual compliance criterion tests."""

    def test_imo_intact_stability_pass(self, stable_vessel):
        from digitalmodel.naval_architecture.compliance import (
            check_imo_intact,
        )

        result = check_imo_intact(
            stable_vessel["angles_deg"],
            stable_vessel["gz_m"],
            stable_vessel["gm_m"],
        )
        assert result["pass"] is True
        assert result["code"] == "IMO-IS-2008"

    def test_imo_intact_stability_fail(self, unstable_vessel):
        from digitalmodel.naval_architecture.compliance import (
            check_imo_intact,
        )

        result = check_imo_intact(
            unstable_vessel["angles_deg"],
            unstable_vessel["gz_m"],
            unstable_vessel["gm_m"],
        )
        assert result["pass"] is False

    def test_imo_freeboard_check(self, stable_vessel):
        from digitalmodel.naval_architecture.compliance import (
            check_min_freeboard,
        )

        result = check_min_freeboard(
            stable_vessel["freeboard_m"],
            stable_vessel["lwl_m"],
        )
        assert result["pass"] is True
        assert result["code"] == "ICLL-1966"

    def test_imo_freeboard_fail(self, unstable_vessel):
        from digitalmodel.naval_architecture.compliance import (
            check_min_freeboard,
        )

        result = check_min_freeboard(
            unstable_vessel["freeboard_m"],
            unstable_vessel["lwl_m"],
        )
        # 1.0 m freeboard for 100 m ship is very low
        assert result["pass"] is False

    def test_dnv_beam_draft_ratio(self, stable_vessel):
        from digitalmodel.naval_architecture.compliance import (
            check_beam_draft_ratio,
        )

        result = check_beam_draft_ratio(
            stable_vessel["beam_m"],
            stable_vessel["draft_m"],
        )
        assert result["pass"] is True
        assert result["code"] == "DNV-RU-SHIP"

    def test_abs_strength_deck_area(self, stable_vessel):
        from digitalmodel.naval_architecture.compliance import (
            check_section_modulus,
        )

        result = check_section_modulus(
            lwl_m=stable_vessel["lwl_m"],
            beam_m=stable_vessel["beam_m"],
            draft_m=stable_vessel["draft_m"],
            cb=0.65,
            actual_z_cm3=500000.0,
        )
        assert result["pass"] is True
        assert result["code"] == "ABS-SVR"


class TestComplianceReport:
    """Full compliance report generation."""

    def test_run_all_checks(self, stable_vessel):
        from digitalmodel.naval_architecture.compliance import (
            run_compliance_checks,
        )

        report = run_compliance_checks(stable_vessel)
        assert "checks" in report
        assert len(report["checks"]) >= 5
        assert "overall_pass" in report

    def test_report_includes_code_refs(self, stable_vessel):
        from digitalmodel.naval_architecture.compliance import (
            run_compliance_checks,
        )

        report = run_compliance_checks(stable_vessel)
        for check in report["checks"]:
            assert "code" in check
            assert "name" in check
            assert "pass" in check

    def test_failing_vessel_report(self, unstable_vessel):
        from digitalmodel.naval_architecture.compliance import (
            run_compliance_checks,
        )

        report = run_compliance_checks(unstable_vessel)
        assert report["overall_pass"] is False
        failed = [c for c in report["checks"] if not c["pass"]]
        assert len(failed) >= 2
