"""Tests for operability calculator and weather window analysis."""
from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.operability import (
    compute_operability,
    hs_limit_for_criterion,
    weather_window_operability,
)
from digitalmodel.marine_ops.installation.models import (
    InstallationCriteria,
    OperabilityResult,
)


class TestHsLimitForCriterion:
    """Binary search for Hs limit."""

    def test_always_feasible(self, vessel):
        """If criterion is always within allowable, return max Hs."""
        result = hs_limit_for_criterion(
            vessel, 0.0,
            criterion_fn=lambda hs: 0.0,  # Always zero
            allowable=10.0,
        )
        assert result > 0

    def test_never_feasible(self, vessel):
        """If criterion is never within allowable, return 0."""
        result = hs_limit_for_criterion(
            vessel, 0.0,
            criterion_fn=lambda hs: 100.0,  # Always exceeds
            allowable=1.0,
        )
        assert result == pytest.approx(0.0)

    def test_linear_criterion(self, vessel):
        """Linear response: response = 2 * Hs. Allowable = 3.0 → Hs_limit ≈ 1.5."""
        result = hs_limit_for_criterion(
            vessel, 0.0,
            criterion_fn=lambda hs: 2.0 * hs,
            allowable=3.0,
            hs_range=np.arange(0.1, 5.05, 0.1),
        )
        assert result == pytest.approx(1.5, abs=0.1)


class TestComputeOperability:
    """Operability assessment for vessel/structure pair."""

    def test_returns_operability_result(self, vessel, structure, criteria, tp_range):
        result = compute_operability(vessel, structure, criteria, tp_range)
        assert isinstance(result, OperabilityResult)

    def test_hs_limit_positive(self, vessel, structure, criteria, tp_range):
        result = compute_operability(vessel, structure, criteria, tp_range)
        assert result.hs_limit_m > 0

    def test_governing_criterion_is_valid(self, vessel, structure, criteria, tp_range):
        result = compute_operability(vessel, structure, criteria, tp_range)
        assert result.governing_criterion in ("crane_tip_heave", "crane_tip_velocity")

    def test_stricter_criteria_lowers_hs_limit(self, vessel, structure, tp_range):
        relaxed = InstallationCriteria(max_crane_tip_heave_m=5.0, max_crane_tip_velocity_m_s=2.0)
        strict = InstallationCriteria(max_crane_tip_heave_m=0.5, max_crane_tip_velocity_m_s=0.1)
        r_relaxed = compute_operability(vessel, structure, relaxed, tp_range)
        r_strict = compute_operability(vessel, structure, strict, tp_range)
        assert r_strict.hs_limit_m <= r_relaxed.hs_limit_m

    def test_details_contain_breakdown(self, vessel, structure, criteria, tp_range):
        result = compute_operability(vessel, structure, criteria, tp_range)
        assert "hs_limits_heave" in result.details
        assert "hs_limits_velocity" in result.details
        assert "tp_range_s" in result.details


class TestWeatherWindowOperability:
    """Operability percentage from scatter diagram."""

    def test_all_operable_gives_100(self, vessel, structure, tp_range, scatter_diagram):
        hs, tp, counts = scatter_diagram
        # Use very relaxed criteria so everything passes
        criteria = InstallationCriteria(max_crane_tip_heave_m=100.0, max_crane_tip_velocity_m_s=100.0)
        op = compute_operability(vessel, structure, criteria, tp_range)
        # Override Hs limit to very large
        op_modified = OperabilityResult(
            hs_limit_m=99.0,
            tp_limits_s=np.full_like(tp_range, 99.0),
            governing_criterion="crane_tip_heave",
            operability_pct=0.0,
            details={"tp_range_s": tp_range},
        )
        pct = weather_window_operability(op_modified, hs, tp, counts)
        assert pct == pytest.approx(100.0, abs=0.1)

    def test_none_operable_gives_zero(self, tp_range, scatter_diagram):
        hs, tp, counts = scatter_diagram
        # Hs limit of 0 — nothing passes
        op = OperabilityResult(
            hs_limit_m=0.0,
            tp_limits_s=np.full_like(tp_range, 0.0),
            governing_criterion="crane_tip_heave",
            operability_pct=0.0,
            details={"tp_range_s": tp_range},
        )
        pct = weather_window_operability(op, hs, tp, counts)
        assert pct == pytest.approx(0.0)

    def test_partial_operability(self, tp_range, scatter_diagram):
        hs, tp, counts = scatter_diagram
        # Hs limit of 1.5m — some bins pass, some don't
        op = OperabilityResult(
            hs_limit_m=1.5,
            tp_limits_s=np.full_like(tp_range, 1.5),
            governing_criterion="crane_tip_heave",
            operability_pct=0.0,
            details={"tp_range_s": tp_range},
        )
        pct = weather_window_operability(op, hs, tp, counts)
        assert 0.0 < pct < 100.0

    def test_empty_scatter_gives_zero(self, tp_range):
        hs = np.array([1.0, 2.0])
        tp = np.array([6.0, 8.0])
        counts = np.zeros((2, 2))
        op = OperabilityResult(
            hs_limit_m=5.0,
            tp_limits_s=np.full_like(tp_range, 5.0),
            governing_criterion="crane_tip_heave",
            operability_pct=0.0,
            details={"tp_range_s": tp_range},
        )
        pct = weather_window_operability(op, hs, tp, counts)
        assert pct == pytest.approx(0.0)
