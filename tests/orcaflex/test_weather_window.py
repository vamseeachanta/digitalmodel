"""Tests for digitalmodel.orcaflex.weather_window module."""

import numpy as np
import pytest

from digitalmodel.orcaflex.weather_window import (
    OperabilityInput,
    OperabilityTable,
    PersistenceResult,
    ScatterDiagram,
    SeasonalOperability,
    WoWEstimate,
    analyse_persistence,
)


class TestScatterDiagram:
    """Tests for wave scatter diagram."""

    def test_default_scatter_sums_to_one(self):
        """Default scatter diagram probabilities should sum to 1."""
        sd = ScatterDiagram()
        scatter = sd.generate_default_scatter()
        total = sum(sum(row) for row in scatter)
        assert total == pytest.approx(1.0, abs=0.01)

    def test_probability_below_large_limit(self):
        """Large Hs limit should capture most probability."""
        sd = ScatterDiagram()
        prob = sd.total_probability_below(hs_limit=100.0)
        assert prob > 0.95

    def test_probability_below_zero_limit(self):
        """Zero Hs limit should capture no probability."""
        sd = ScatterDiagram()
        prob = sd.total_probability_below(hs_limit=0.0)
        assert prob == 0.0

    def test_get_occurrences_shape(self):
        """Occurrences array should match bin dimensions."""
        sd = ScatterDiagram()
        arr = sd.get_occurrences()
        assert arr.shape == (len(sd.hs_bins), len(sd.tp_bins))


class TestOperability:
    """Tests for operability calculations."""

    def test_operability_percentage(self):
        """Operability should be between 0 and 100%."""
        op_input = OperabilityInput(hs_limit=2.5)
        result = op_input.calculate_operability()
        assert 0 <= result["operability_pct"] <= 100
        assert result["operability_pct"] + result["non_operability_pct"] == pytest.approx(100.0)

    def test_higher_limit_higher_operability(self):
        """Higher Hs limit should give higher operability."""
        sd = ScatterDiagram()
        op1 = OperabilityInput(hs_limit=1.5, scatter=sd).calculate_operability()
        op2 = OperabilityInput(hs_limit=3.5, scatter=sd).calculate_operability()
        assert op2["operability_pct"] >= op1["operability_pct"]

    def test_operability_table(self):
        """Operability table should increase monotonically with Hs limit."""
        ot = OperabilityTable(hs_limits=[1.0, 2.0, 3.0, 4.0, 5.0])
        table = ot.generate_table()
        assert len(table) == 5
        for i in range(len(table) - 1):
            assert table[i + 1]["operability_pct"] >= table[i]["operability_pct"]


class TestPersistence:
    """Tests for persistence (weather window) analysis."""

    def test_all_below_threshold(self):
        """All data below threshold should give one large window."""
        hs = np.ones(100) * 1.0  # all 1.0 m
        result = analyse_persistence(hs, hs_limit=2.0, time_step_hours=3.0)
        assert result.num_windows == 1
        assert result.mean_window_hours == pytest.approx(300.0)  # 100 * 3

    def test_all_above_threshold(self):
        """All data above threshold should give zero windows."""
        hs = np.ones(100) * 5.0
        result = analyse_persistence(hs, hs_limit=2.0)
        assert result.num_windows == 0
        assert result.mean_window_hours == 0.0

    def test_alternating_pattern(self):
        """Alternating above/below should give many short windows."""
        hs = np.array([1.0, 5.0] * 50)  # alternating
        result = analyse_persistence(hs, hs_limit=2.0, time_step_hours=1.0)
        assert result.num_windows == 50
        assert result.mean_window_hours == pytest.approx(1.0)

    def test_total_hours_below(self):
        """Total hours below should be counted correctly."""
        hs = np.array([1.0, 1.0, 5.0, 1.0])
        result = analyse_persistence(hs, hs_limit=2.0, time_step_hours=3.0)
        assert result.total_hours_below == pytest.approx(9.0)  # 3 points * 3 hours


class TestSeasonalOperability:
    """Tests for seasonal operability."""

    def test_best_months_default(self):
        """Default North Sea data should have summer months as best."""
        so = SeasonalOperability()
        best = so.best_months(min_operability=70.0)
        assert "Jul" in best
        assert "Aug" in best

    def test_annual_average(self):
        """Annual average should be computed correctly."""
        so = SeasonalOperability(operability_pct=[50.0] * 12)
        assert so.annual_average() == pytest.approx(50.0)

    def test_no_months_with_high_threshold(self):
        """Very high threshold should return no suitable months."""
        so = SeasonalOperability()
        best = so.best_months(min_operability=99.0)
        assert len(best) == 0


class TestWoW:
    """Tests for Waiting-on-Weather estimate."""

    def test_wow_basic(self):
        """WoW should be positive for less than 100% operability."""
        wow = WoWEstimate(operation_duration_hours=72.0, operability_pct=70.0)
        result = wow.estimate_wow_hours()
        assert result["base_wow_hours"] > 0
        assert result["design_wow_hours"] > result["base_wow_hours"]

    def test_wow_100_pct_operability(self):
        """100% operability should give zero WoW."""
        wow = WoWEstimate(operability_pct=100.0)
        result = wow.estimate_wow_hours()
        assert result["base_wow_hours"] == 0.0

    def test_wow_total_duration(self):
        """Total duration should be operation + design WoW."""
        wow = WoWEstimate(operation_duration_hours=48.0, operability_pct=50.0, contingency_factor=1.0)
        result = wow.estimate_wow_hours()
        assert result["total_operation_hours"] == pytest.approx(
            wow.operation_duration_hours + result["design_wow_hours"]
        )
