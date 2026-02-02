# ABOUTME: Tests for the pump diagnostics module.
# ABOUTME: Tests failure mode classification and troubleshooting report generation.

import pytest
import numpy as np
from digitalmodel.marine_ops.artificial_lift.dynacard import (
    PumpDiagnostics,
    CardData,
    AnalysisResults,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.constants import (
    PUMP_TAGGING_LOAD_THRESHOLD_LBS,
    FLUID_POUND_LOAD_DIFF_THRESHOLD_LBS,
    GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS,
)


def create_normal_card(num_points: int = 100) -> CardData:
    """Create a card representing normal operation."""
    t = np.linspace(0, 2 * np.pi, num_points)

    # Normal card: smooth transitions, reasonable load range
    position = 50 * (1 - np.cos(t))
    load = 12000 + 4000 * np.sin(t)  # Range: 8000-16000 lbs

    return CardData(
        position=position.tolist(),
        load=load.tolist()
    )


def create_pump_tagging_card(num_points: int = 100) -> CardData:
    """Create a card with pump tagging (extreme loads)."""
    t = np.linspace(0, 2 * np.pi, num_points)

    position = 50 * (1 - np.cos(t))
    # Extreme peak load exceeding threshold
    load = 30000 + 10000 * np.sin(t)  # Peak: 40000 lbs

    return CardData(
        position=position.tolist(),
        load=load.tolist()
    )


def create_fluid_pound_card(num_points: int = 100) -> CardData:
    """Create a card with fluid pound (sharp load drop)."""
    t = np.linspace(0, 2 * np.pi, num_points)

    position = 50 * (1 - np.cos(t))

    # Start with normal load pattern
    load = 12000 + 4000 * np.sin(t)

    # Add sharp drop in downstroke (second half)
    mid = num_points // 2
    drop_point = mid + num_points // 4
    # Create a sharp 8000 lb drop
    load[drop_point] = load[drop_point - 1] - 8000

    return CardData(
        position=position.tolist(),
        load=load.tolist()
    )


def create_gas_interference_card(num_points: int = 100) -> CardData:
    """Create a card with gas interference (very low minimum loads)."""
    t = np.linspace(0, 2 * np.pi, num_points)

    position = 50 * (1 - np.cos(t))
    # Very low loads indicating gas compression
    load = 5000 + 4000 * np.sin(t)  # Range: 1000-9000 lbs

    return CardData(
        position=position.tolist(),
        load=load.tolist()
    )


class TestPumpDiagnosticsFailureModes:
    """Tests for failure mode definitions."""

    def test_failure_modes_defined(self):
        """All expected failure modes should be defined."""
        expected_modes = [
            "NORMAL",
            "GAS_INTERFERENCE",
            "FLUID_POUND",
            "PUMP_TAGGING",
            "TUBING_MOVEMENT",
            "VALVE_LEAK"
        ]

        for mode in expected_modes:
            assert mode in PumpDiagnostics.FAILURE_MODES
            assert len(PumpDiagnostics.FAILURE_MODES[mode]) > 0

    def test_failure_mode_descriptions_not_empty(self):
        """Each failure mode should have a non-empty description."""
        for mode, description in PumpDiagnostics.FAILURE_MODES.items():
            assert isinstance(description, str)
            assert len(description) > 10  # Reasonable minimum length


class TestClassifyCard:
    """Tests for the classify_card static method."""

    def test_classifies_normal_operation(self):
        """Should classify normal cards as NORMAL."""
        card = create_normal_card()
        result = PumpDiagnostics.classify_card(card)
        assert result == "NORMAL"

    def test_classifies_pump_tagging(self):
        """Should classify extreme loads as PUMP_TAGGING."""
        card = create_pump_tagging_card()
        result = PumpDiagnostics.classify_card(card)
        assert result == "PUMP_TAGGING"

    def test_pump_tagging_threshold(self):
        """Pump tagging detection should use the defined threshold."""
        card = create_normal_card()
        # Just below threshold
        card.load[50] = PUMP_TAGGING_LOAD_THRESHOLD_LBS - 1
        result = PumpDiagnostics.classify_card(card)
        assert result != "PUMP_TAGGING"

        # Just above threshold
        card.load[50] = PUMP_TAGGING_LOAD_THRESHOLD_LBS + 1
        result = PumpDiagnostics.classify_card(card)
        assert result == "PUMP_TAGGING"

    def test_classifies_fluid_pound(self):
        """Should classify sharp load drops as FLUID_POUND."""
        card = create_fluid_pound_card()
        result = PumpDiagnostics.classify_card(card)
        assert result == "FLUID_POUND"

    def test_classifies_gas_interference(self):
        """Should classify very low loads as GAS_INTERFERENCE."""
        card = create_gas_interference_card()
        result = PumpDiagnostics.classify_card(card)
        assert result == "GAS_INTERFERENCE"

    def test_gas_interference_threshold(self):
        """Gas interference detection should use the defined threshold."""
        num_points = 100
        t = np.linspace(0, 2 * np.pi, num_points)
        position = 50 * (1 - np.cos(t))

        # Just above threshold (no gas interference)
        load = np.full(num_points, GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS + 1000)
        card = CardData(position=position.tolist(), load=load.tolist())
        result = PumpDiagnostics.classify_card(card)
        assert result == "NORMAL"

        # Just below threshold (gas interference)
        load = np.full(num_points, GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS - 100)
        card = CardData(position=position.tolist(), load=load.tolist())
        result = PumpDiagnostics.classify_card(card)
        assert result == "GAS_INTERFERENCE"

    def test_priority_pump_tagging_over_fluid_pound(self):
        """Pump tagging should be detected before fluid pound."""
        # Card with both extreme load AND sharp drop
        card = create_pump_tagging_card()
        # Add sharp drop
        mid = len(card.load) // 2
        card.load[mid + 10] = card.load[mid + 9] - 10000

        result = PumpDiagnostics.classify_card(card)
        assert result == "PUMP_TAGGING"  # Higher priority


class TestGenerateTroubleshootingReport:
    """Tests for the generate_troubleshooting_report method."""

    def test_returns_report_string(self):
        """Should return a non-empty report string."""
        diagnostics = PumpDiagnostics()
        card = create_normal_card()

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert isinstance(report, str)
        assert len(report) > 0

    def test_report_contains_classification(self):
        """Report should contain the failure mode classification."""
        diagnostics = PumpDiagnostics()
        card = create_normal_card()

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert "Classification:" in report
        assert "NORMAL" in report

    def test_report_contains_description(self):
        """Report should contain the failure mode description."""
        diagnostics = PumpDiagnostics()
        card = create_normal_card()

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        expected_desc = PumpDiagnostics.FAILURE_MODES["NORMAL"]
        assert expected_desc in report

    def test_report_includes_buckling_warning(self):
        """Report should include buckling warning when detected."""
        diagnostics = PumpDiagnostics()
        card = create_normal_card()

        results = AnalysisResults(
            downhole_card=card,
            buckling_detected=True
        )
        report = diagnostics.generate_troubleshooting_report(results)

        assert "buckling" in report.lower()
        assert "WARNING" in report

    def test_report_no_buckling_warning_when_not_detected(self):
        """Report should not include buckling warning when not detected."""
        diagnostics = PumpDiagnostics()
        card = create_normal_card()

        results = AnalysisResults(
            downhole_card=card,
            buckling_detected=False
        )
        report = diagnostics.generate_troubleshooting_report(results)

        assert "WARNING" not in report

    def test_report_stored_in_results(self):
        """Report should be stored in results.diagnostic_message."""
        diagnostics = PumpDiagnostics()
        card = create_normal_card()

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert results.diagnostic_message == report

    def test_pump_tagging_report(self):
        """Should generate appropriate report for pump tagging."""
        diagnostics = PumpDiagnostics()
        card = create_pump_tagging_card()

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert "PUMP_TAGGING" in report
        assert "contact" in report.lower()

    def test_fluid_pound_report(self):
        """Should generate appropriate report for fluid pound."""
        diagnostics = PumpDiagnostics()
        card = create_fluid_pound_card()

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert "FLUID_POUND" in report
        assert "fillage" in report.lower()

    def test_gas_interference_report(self):
        """Should generate appropriate report for gas interference."""
        diagnostics = PumpDiagnostics()
        card = create_gas_interference_card()

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert "GAS_INTERFERENCE" in report
        assert "gas" in report.lower()


class TestDiagnosticsIntegration:
    """Integration tests for diagnostics module."""

    def test_classify_multiple_cards(self):
        """Should correctly classify various card types."""
        test_cases = [
            (create_normal_card(), "NORMAL"),
            (create_pump_tagging_card(), "PUMP_TAGGING"),
            (create_fluid_pound_card(), "FLUID_POUND"),
            (create_gas_interference_card(), "GAS_INTERFERENCE"),
        ]

        for card, expected in test_cases:
            result = PumpDiagnostics.classify_card(card)
            assert result == expected, f"Expected {expected}, got {result}"

    def test_diagnostics_is_reusable(self):
        """Single diagnostics instance should work for multiple analyses."""
        diagnostics = PumpDiagnostics()

        cards = [
            create_normal_card(),
            create_pump_tagging_card(),
            create_fluid_pound_card(),
        ]

        for card in cards:
            results = AnalysisResults(downhole_card=card)
            report = diagnostics.generate_troubleshooting_report(results)
            assert len(report) > 0
