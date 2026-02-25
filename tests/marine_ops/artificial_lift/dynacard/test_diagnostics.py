# ABOUTME: Tests for the pump diagnostics module.
# ABOUTME: Tests failure mode classification and troubleshooting report generation.

import pytest
import numpy as np
from digitalmodel.marine_ops.artificial_lift.dynacard import (
    PumpDiagnostics,
    CardData,
    AnalysisResults,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import DiagnosticResult
from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    generate_normal_card,
    generate_pump_tagging_card,
    generate_fluid_pound_card,
    generate_gas_interference_card,
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.constants import (
    PUMP_TAGGING_LOAD_THRESHOLD_LBS,
    FLUID_POUND_LOAD_DIFF_THRESHOLD_LBS,
    GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS,
)


# --- Legacy card helpers (kept for backward compat tests) ---

def create_normal_card(num_points: int = 100) -> CardData:
    """Create a card representing normal operation."""
    t = np.linspace(0, 2 * np.pi, num_points)
    position = 50 * (1 - np.cos(t))
    load = 12000 + 4000 * np.sin(t)
    return CardData(position=position.tolist(), load=load.tolist())


def create_pump_tagging_card(num_points: int = 100) -> CardData:
    """Create a card with pump tagging (extreme loads)."""
    t = np.linspace(0, 2 * np.pi, num_points)
    position = 50 * (1 - np.cos(t))
    load = 30000 + 10000 * np.sin(t)
    return CardData(position=position.tolist(), load=load.tolist())


def create_fluid_pound_card(num_points: int = 100) -> CardData:
    """Create a card with fluid pound (sharp load drop)."""
    t = np.linspace(0, 2 * np.pi, num_points)
    position = 50 * (1 - np.cos(t))
    load = 12000 + 4000 * np.sin(t)
    mid = num_points // 2
    drop_point = mid + num_points // 4
    load[drop_point] = load[drop_point - 1] - 8000
    return CardData(position=position.tolist(), load=load.tolist())


def create_gas_interference_card(num_points: int = 100) -> CardData:
    """Create a card with gas interference (very low minimum loads)."""
    t = np.linspace(0, 2 * np.pi, num_points)
    position = 50 * (1 - np.cos(t))
    load = 5000 + 4000 * np.sin(t)
    return CardData(position=position.tolist(), load=load.tolist())


class TestPumpDiagnosticsFailureModes:
    """Tests for failure mode definitions."""

    def test_failure_modes_defined(self):
        """All expected failure modes should be defined (including legacy)."""
        expected_modes = [
            "NORMAL",
            "GAS_INTERFERENCE",
            "FLUID_POUND",
            "PUMP_TAGGING",
            "TUBING_MOVEMENT",
            "VALVE_LEAK",
        ]

        for mode in expected_modes:
            assert mode in PumpDiagnostics.FAILURE_MODES
            assert len(PumpDiagnostics.FAILURE_MODES[mode]) > 0

    def test_expanded_failure_modes_defined(self):
        """All 18 failure modes plus legacy alias should be defined."""
        assert len(PumpDiagnostics.FAILURE_MODES) == 19  # 18 + VALVE_LEAK alias

    def test_failure_mode_descriptions_not_empty(self):
        """Each failure mode should have a non-empty description."""
        for mode, description in PumpDiagnostics.FAILURE_MODES.items():
            assert isinstance(description, str)
            assert len(description) > 10  # Reasonable minimum length


class TestClassifyCard:
    """Tests for the classify_card static method using ML-trained generators."""

    def test_classifies_normal_operation(self):
        """Should classify normal cards as NORMAL."""
        card = generate_normal_card(seed=42)
        result = PumpDiagnostics.classify_card(card)
        assert result == "NORMAL"

    def test_classifies_pump_tagging(self):
        """Should classify extreme loads as PUMP_TAGGING."""
        card = generate_pump_tagging_card(seed=42)
        result = PumpDiagnostics.classify_card(card)
        assert result == "PUMP_TAGGING"

    def test_classifies_fluid_pound(self):
        """Should classify sharp load drops as FLUID_POUND."""
        card = generate_fluid_pound_card(seed=42)
        result = PumpDiagnostics.classify_card(card)
        assert result == "FLUID_POUND"

    def test_classifies_gas_interference(self):
        """Should classify very low loads as GAS_INTERFERENCE."""
        card = generate_gas_interference_card(seed=42)
        result = PumpDiagnostics.classify_card(card)
        assert result == "GAS_INTERFERENCE"

    def test_classify_returns_valid_mode(self):
        """Classification should always return a valid failure mode."""
        for name, gen_func in ALL_GENERATORS.items():
            card = gen_func(seed=99)
            result = PumpDiagnostics.classify_card(card)
            assert result in PumpDiagnostics.FAILURE_MODES, (
                f"Generator {name} produced unknown mode: {result}"
            )

    def test_classify_returns_string(self):
        """classify_card should always return a string."""
        card = generate_normal_card(seed=0)
        result = PumpDiagnostics.classify_card(card)
        assert isinstance(result, str)


class TestClassifyCardLegacy:
    """Tests for legacy threshold-based fallback classification."""

    def setup_method(self):
        """Force legacy mode by clearing model cache."""
        PumpDiagnostics.reset_model_cache()
        self._original_exists = PumpDiagnostics._load_model

    def teardown_method(self):
        """Restore model cache."""
        PumpDiagnostics.reset_model_cache()

    def test_legacy_classifies_normal(self):
        """Legacy classifier should classify normal cards as NORMAL."""
        result = PumpDiagnostics._classify_legacy(create_normal_card())
        assert result == "NORMAL"

    def test_legacy_classifies_pump_tagging(self):
        """Legacy classifier should detect pump tagging."""
        result = PumpDiagnostics._classify_legacy(create_pump_tagging_card())
        assert result == "PUMP_TAGGING"

    def test_legacy_pump_tagging_threshold(self):
        """Legacy pump tagging detection should use the defined threshold."""
        card = create_normal_card()
        card.load[50] = PUMP_TAGGING_LOAD_THRESHOLD_LBS - 1
        result = PumpDiagnostics._classify_legacy(card)
        assert result != "PUMP_TAGGING"

        card.load[50] = PUMP_TAGGING_LOAD_THRESHOLD_LBS + 1
        result = PumpDiagnostics._classify_legacy(card)
        assert result == "PUMP_TAGGING"

    def test_legacy_classifies_fluid_pound(self):
        """Legacy classifier should detect fluid pound."""
        result = PumpDiagnostics._classify_legacy(create_fluid_pound_card())
        assert result == "FLUID_POUND"

    def test_legacy_classifies_gas_interference(self):
        """Legacy classifier should detect gas interference."""
        result = PumpDiagnostics._classify_legacy(create_gas_interference_card())
        assert result == "GAS_INTERFERENCE"

    def test_legacy_gas_interference_threshold(self):
        """Legacy gas interference detection should use the defined threshold."""
        num_points = 100
        t = np.linspace(0, 2 * np.pi, num_points)
        position = 50 * (1 - np.cos(t))

        load = np.full(num_points, GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS + 1000)
        card = CardData(position=position.tolist(), load=load.tolist())
        result = PumpDiagnostics._classify_legacy(card)
        assert result == "NORMAL"

        load = np.full(num_points, GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS - 100)
        card = CardData(position=position.tolist(), load=load.tolist())
        result = PumpDiagnostics._classify_legacy(card)
        assert result == "GAS_INTERFERENCE"

    def test_legacy_priority_pump_tagging_over_fluid_pound(self):
        """Pump tagging should be detected before fluid pound in legacy mode."""
        card = create_pump_tagging_card()
        mid = len(card.load) // 2
        card.load[mid + 10] = card.load[mid + 9] - 10000
        result = PumpDiagnostics._classify_legacy(card)
        assert result == "PUMP_TAGGING"


class TestGenerateTroubleshootingReport:
    """Tests for the generate_troubleshooting_report method."""

    def test_returns_report_string(self):
        """Should return a non-empty report string."""
        diagnostics = PumpDiagnostics()
        card = generate_normal_card(seed=42)

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert isinstance(report, str)
        assert len(report) > 0

    def test_report_contains_classification(self):
        """Report should contain the failure mode classification."""
        diagnostics = PumpDiagnostics()
        card = generate_normal_card(seed=42)

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert "Classification:" in report
        assert "NORMAL" in report

    def test_report_contains_description(self):
        """Report should contain the failure mode description."""
        diagnostics = PumpDiagnostics()
        card = generate_normal_card(seed=42)

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        expected_desc = PumpDiagnostics.FAILURE_MODES["NORMAL"]
        assert expected_desc in report

    def test_report_includes_buckling_warning(self):
        """Report should include buckling warning when detected."""
        diagnostics = PumpDiagnostics()
        card = generate_normal_card(seed=42)

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
        card = generate_normal_card(seed=42)

        results = AnalysisResults(
            downhole_card=card,
            buckling_detected=False
        )
        report = diagnostics.generate_troubleshooting_report(results)

        assert "WARNING" not in report

    def test_report_stored_in_results(self):
        """Report should be stored in results.diagnostic_message."""
        diagnostics = PumpDiagnostics()
        card = generate_normal_card(seed=42)

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert results.diagnostic_message == report

    def test_pump_tagging_report(self):
        """Should generate appropriate report for pump tagging."""
        diagnostics = PumpDiagnostics()
        card = generate_pump_tagging_card(seed=42)

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert "PUMP_TAGGING" in report
        assert "contact" in report.lower()

    def test_fluid_pound_report(self):
        """Should generate appropriate report for fluid pound."""
        diagnostics = PumpDiagnostics()
        card = generate_fluid_pound_card(seed=42)

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert "FLUID_POUND" in report
        assert "fillage" in report.lower()

    def test_gas_interference_report(self):
        """Should generate appropriate report for gas interference."""
        diagnostics = PumpDiagnostics()
        card = generate_gas_interference_card(seed=42)

        results = AnalysisResults(downhole_card=card)
        report = diagnostics.generate_troubleshooting_report(results)

        assert "GAS_INTERFERENCE" in report
        assert "gas" in report.lower()


class TestClassifyWithContext:
    """Tests for the new classify_with_context method."""

    def test_returns_diagnostic_result(self):
        """Should return a DiagnosticResult."""
        diagnostics = PumpDiagnostics()
        card = generate_normal_card(seed=42)
        results = AnalysisResults(downhole_card=card)

        diag = diagnostics.classify_with_context(results)
        assert isinstance(diag, DiagnosticResult)

    def test_confidence_in_range(self):
        """Confidence should be between 0 and 1."""
        diagnostics = PumpDiagnostics()
        for name, gen_func in ALL_GENERATORS.items():
            card = gen_func(seed=42)
            results = AnalysisResults(downhole_card=card)
            diag = diagnostics.classify_with_context(results)
            assert 0.0 <= diag.confidence <= 1.0, (
                f"Mode {name}: confidence {diag.confidence} out of range"
            )

    def test_differential_has_top_3(self):
        """Differential should contain top 3 modes."""
        diagnostics = PumpDiagnostics()
        card = generate_normal_card(seed=42)
        results = AnalysisResults(downhole_card=card)
        diag = diagnostics.classify_with_context(results)

        assert len(diag.differential) == 3
        for entry in diag.differential:
            assert "mode" in entry
            assert "probability" in entry
            assert 0.0 <= entry["probability"] <= 1.0

    def test_differential_probabilities_sum_to_one(self):
        """All differential probabilities should approximate 1.0 if all shown."""
        diagnostics = PumpDiagnostics()
        card = generate_normal_card(seed=42)
        results = AnalysisResults(downhole_card=card)
        diag = diagnostics.classify_with_context(results)

        # Top 3 may not sum to exactly 1 (others exist)
        total = sum(e["probability"] for e in diag.differential)
        assert total <= 1.01  # Allow small rounding

    def test_none_card_returns_normal(self):
        """Null downhole card should return NORMAL with 0 confidence."""
        diagnostics = PumpDiagnostics()
        results = AnalysisResults(downhole_card=None)
        diag = diagnostics.classify_with_context(results)
        assert diag.classification == "NORMAL"
        assert diag.confidence == 0.0

    def test_model_version_present(self):
        """DiagnosticResult should include model version."""
        diagnostics = PumpDiagnostics()
        card = generate_normal_card(seed=42)
        results = AnalysisResults(downhole_card=card)
        diag = diagnostics.classify_with_context(results)
        assert diag.model_version is not None
        assert len(diag.model_version) > 0


class TestDiagnosticsIntegration:
    """Integration tests for diagnostics module."""

    def test_classify_multiple_cards(self):
        """Should correctly classify various card types."""
        test_cases = [
            (generate_normal_card(seed=42), "NORMAL"),
            (generate_pump_tagging_card(seed=42), "PUMP_TAGGING"),
            (generate_fluid_pound_card(seed=42), "FLUID_POUND"),
            (generate_gas_interference_card(seed=42), "GAS_INTERFERENCE"),
        ]

        for card, expected in test_cases:
            result = PumpDiagnostics.classify_card(card)
            assert result == expected, f"Expected {expected}, got {result}"

    def test_diagnostics_is_reusable(self):
        """Single diagnostics instance should work for multiple analyses."""
        diagnostics = PumpDiagnostics()

        cards = [
            generate_normal_card(seed=1),
            generate_pump_tagging_card(seed=2),
            generate_fluid_pound_card(seed=3),
        ]

        for card in cards:
            results = AnalysisResults(downhole_card=card)
            report = diagnostics.generate_troubleshooting_report(results)
            assert len(report) > 0


class TestModelLoadingAndCaching:
    """Tests for model loading, caching, and fallback."""

    def test_model_loads_successfully(self):
        """Model should load from JSON file."""
        PumpDiagnostics.reset_model_cache()
        model = PumpDiagnostics._load_model()
        assert model is not None
        assert "class_labels" in model
        assert "trees" in model
        assert "scaling" in model

    def test_model_is_cached(self):
        """Second load should return same object (cached)."""
        PumpDiagnostics.reset_model_cache()
        model1 = PumpDiagnostics._load_model()
        model2 = PumpDiagnostics._load_model()
        assert model1 is model2

    def test_reset_model_cache(self):
        """reset_model_cache should clear the cached model."""
        PumpDiagnostics._load_model()
        PumpDiagnostics.reset_model_cache()
        assert PumpDiagnostics._model is None
