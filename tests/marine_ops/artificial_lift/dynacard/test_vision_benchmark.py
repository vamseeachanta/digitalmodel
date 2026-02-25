# ABOUTME: TDD tests for dynacard vision model evaluation benchmark.
# ABOUTME: Validates test set construction, vision classifier interface, and benchmark runner.

from __future__ import annotations

import json
from pathlib import Path
from typing import List
from unittest.mock import MagicMock, patch

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import ALL_GENERATORS
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import PumpDiagnostics
from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.test_set_builder import (
    build_hold_out_test_set,
    LabelledCard,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.vision_classifier import (
    VisionModelClassifier,
    ClassifierBackend,
    VisionClassificationResult,
    StubVisionClassifier,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.runner import (
    BenchmarkRunner,
    BenchmarkResult,
    MethodResult,
    run_benchmark,
)


# ---------------------------------------------------------------------------
# Test set builder tests
# ---------------------------------------------------------------------------

class TestBuildHoldOutTestSet:
    """Tests for the hold-out test set builder."""

    def test_returns_at_least_50_cards(self):
        """Test set must have N>=50 labelled cards (acceptance criterion)."""
        test_set = build_hold_out_test_set(samples_per_mode=3)
        assert len(test_set) >= 50

    def test_covers_all_18_modes(self):
        """All 18 failure modes must appear in the test set."""
        test_set = build_hold_out_test_set(samples_per_mode=3)
        modes_present = {card.label for card in test_set}
        assert modes_present == set(ALL_GENERATORS.keys())

    def test_cards_have_valid_structure(self):
        """Every labelled card must have a valid CardData and string label."""
        test_set = build_hold_out_test_set(samples_per_mode=3)
        for lc in test_set:
            assert isinstance(lc.card, CardData)
            assert isinstance(lc.label, str)
            assert lc.label in ALL_GENERATORS
            assert len(lc.card.position) > 0
            assert len(lc.card.load) == len(lc.card.position)

    def test_hold_out_seeds_differ_from_training_seeds(self):
        """Hold-out cards must use seeds outside the training range [0, samples_per_mode)."""
        test_set = build_hold_out_test_set(samples_per_mode=3)
        for lc in test_set:
            assert lc.seed >= 1000, (
                f"Expected hold-out seed >= 1000, got {lc.seed} for {lc.label}"
            )

    def test_labelled_card_has_seed_attribute(self):
        """LabelledCard dataclass must expose the generation seed."""
        test_set = build_hold_out_test_set(samples_per_mode=2)
        assert all(hasattr(lc, "seed") for lc in test_set)

    def test_deterministic_with_same_params(self):
        """Same arguments must produce identical test sets."""
        set_a = build_hold_out_test_set(samples_per_mode=3, base_seed=1000)
        set_b = build_hold_out_test_set(samples_per_mode=3, base_seed=1000)
        labels_a = [lc.label for lc in set_a]
        labels_b = [lc.label for lc in set_b]
        assert labels_a == labels_b

    def test_larger_set_has_more_cards(self):
        """Increasing samples_per_mode must increase total card count."""
        small = build_hold_out_test_set(samples_per_mode=3)
        large = build_hold_out_test_set(samples_per_mode=6)
        assert len(large) > len(small)


# ---------------------------------------------------------------------------
# Vision classifier interface tests
# ---------------------------------------------------------------------------

class TestVisionModelClassifier:
    """Tests for the vision model classifier interface."""

    def test_stub_classifier_accepts_card(self):
        """Stub classifier must accept a CardData and return a result."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
            generate_normal_card,
        )
        card = generate_normal_card(seed=999)
        clf = StubVisionClassifier()
        result = clf.classify(card, "NORMAL")
        assert isinstance(result, VisionClassificationResult)

    def test_stub_classifier_result_has_prediction(self):
        """Result must have a non-empty prediction string."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
            generate_fluid_pound_card,
        )
        card = generate_fluid_pound_card(seed=999)
        clf = StubVisionClassifier()
        result = clf.classify(card, "FLUID_POUND")
        assert isinstance(result.prediction, str)
        assert len(result.prediction) > 0

    def test_stub_classifier_result_has_confidence(self):
        """Result must have a confidence value in [0, 1]."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
            generate_gas_interference_card,
        )
        card = generate_gas_interference_card(seed=999)
        clf = StubVisionClassifier()
        result = clf.classify(card, "GAS_INTERFERENCE")
        assert 0.0 <= result.confidence <= 1.0

    def test_stub_classifier_prediction_is_valid_failure_mode(self):
        """Stub prediction must be one of the 18 known failure modes."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
            generate_pump_tagging_card,
        )
        card = generate_pump_tagging_card(seed=999)
        clf = StubVisionClassifier()
        result = clf.classify(card, "PUMP_TAGGING")
        valid_modes = set(PumpDiagnostics.FAILURE_MODES.keys()) - {"VALVE_LEAK"}
        assert result.prediction in valid_modes, (
            f"Unexpected prediction: {result.prediction}"
        )

    def test_stub_classifier_backend_is_stub(self):
        """StubVisionClassifier must identify itself as STUB backend."""
        clf = StubVisionClassifier()
        assert clf.backend == ClassifierBackend.STUB

    def test_vision_classifier_abstract_interface(self):
        """VisionModelClassifier base class must enforce classify() contract."""
        # Must raise TypeError if classify() not implemented
        with pytest.raises(TypeError):
            VisionModelClassifier()

    def test_stub_is_deterministic_for_same_card(self):
        """Two calls with the same card + label must return the same prediction."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
            generate_normal_card,
        )
        card = generate_normal_card(seed=1234)
        clf = StubVisionClassifier()
        r1 = clf.classify(card, "NORMAL")
        r2 = clf.classify(card, "NORMAL")
        assert r1.prediction == r2.prediction

    def test_stub_achieves_reasonable_accuracy_on_test_set(self):
        """Stub must achieve >=60% accuracy (it uses the ML features as a hint)."""
        test_set = build_hold_out_test_set(samples_per_mode=3)
        clf = StubVisionClassifier()
        correct = sum(
            1 for lc in test_set
            if clf.classify(lc.card, lc.label).prediction == lc.label
        )
        accuracy = correct / len(test_set)
        assert accuracy >= 0.60, f"Stub accuracy {accuracy:.1%} < 60% floor"


# ---------------------------------------------------------------------------
# Benchmark runner tests
# ---------------------------------------------------------------------------

class TestBenchmarkRunner:
    """Tests for the benchmark runner that compares heuristic vs vision model."""

    @pytest.fixture
    def small_test_set(self) -> List[LabelledCard]:
        return build_hold_out_test_set(samples_per_mode=3)

    @pytest.fixture
    def runner(self, small_test_set) -> BenchmarkRunner:
        clf = StubVisionClassifier()
        return BenchmarkRunner(test_set=small_test_set, vision_classifier=clf)

    def test_runner_produces_benchmark_result(self, runner):
        """run() must return a BenchmarkResult."""
        result = runner.run()
        assert isinstance(result, BenchmarkResult)

    def test_benchmark_result_has_heuristic_method(self, runner):
        """BenchmarkResult must include a heuristic MethodResult."""
        result = runner.run()
        assert result.heuristic is not None
        assert isinstance(result.heuristic, MethodResult)

    def test_benchmark_result_has_vision_method(self, runner):
        """BenchmarkResult must include a vision MethodResult."""
        result = runner.run()
        assert result.vision is not None
        assert isinstance(result.vision, MethodResult)

    def test_method_result_accuracy_in_range(self, runner):
        """Both accuracy values must be within [0, 1]."""
        result = runner.run()
        assert 0.0 <= result.heuristic.accuracy <= 1.0
        assert 0.0 <= result.vision.accuracy <= 1.0

    def test_method_result_has_per_mode_breakdown(self, runner):
        """Both results must include per-mode accuracy breakdown."""
        result = runner.run()
        assert isinstance(result.heuristic.per_mode_accuracy, dict)
        assert isinstance(result.vision.per_mode_accuracy, dict)
        # Must have entries for every mode that appears in the test set
        test_modes = {lc.label for lc in runner.test_set}
        assert set(result.heuristic.per_mode_accuracy.keys()) == test_modes
        assert set(result.vision.per_mode_accuracy.keys()) == test_modes

    def test_method_result_has_confusion_count(self, runner):
        """MethodResult must include total correct/wrong counts."""
        result = runner.run()
        for mr in (result.heuristic, result.vision):
            assert mr.correct + mr.wrong == len(runner.test_set)

    def test_benchmark_result_has_recommendation(self, runner):
        """BenchmarkResult must include a non-empty recommendation string."""
        result = runner.run()
        assert isinstance(result.recommendation, str)
        assert len(result.recommendation) > 10

    def test_recommendation_contains_one_of_three_options(self, runner):
        """Recommendation must mention replace, augment, or keep."""
        result = runner.run()
        rec_lower = result.recommendation.lower()
        assert any(
            keyword in rec_lower
            for keyword in ("replace", "augment", "keep")
        ), f"Recommendation missing action keyword: {result.recommendation}"

    def test_benchmark_result_has_n_cards(self, runner):
        """BenchmarkResult must record how many cards were evaluated."""
        result = runner.run()
        assert result.n_cards == len(runner.test_set)

    def test_run_benchmark_convenience_function(self, small_test_set):
        """Convenience function run_benchmark() must produce same output."""
        clf = StubVisionClassifier()
        result = run_benchmark(test_set=small_test_set, vision_classifier=clf)
        assert isinstance(result, BenchmarkResult)
        assert result.n_cards == len(small_test_set)

    def test_heuristic_only_covers_legacy_modes(self, runner):
        """Legacy heuristic only handles 3 modes: confirm accuracy reflects this."""
        result = runner.run()
        # The legacy baseline handles NORMAL/FLUID_POUND/GAS_INTERFERENCE/PUMP_TAGGING.
        # Its accuracy on the 18-mode test set should be lower than 100%.
        # This is a sanity check (not a hard floor since test sets vary).
        assert result.heuristic.accuracy < 1.0

    def test_benchmark_result_serialisable(self, runner):
        """BenchmarkResult must be serialisable to JSON."""
        result = runner.run()
        data = result.to_dict()
        json_str = json.dumps(data)
        roundtripped = json.loads(json_str)
        assert roundtripped["n_cards"] == result.n_cards
        assert "heuristic" in roundtripped
        assert "vision" in roundtripped
        assert "recommendation" in roundtripped
