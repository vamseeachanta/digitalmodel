# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# ABOUTME: Benchmark runner comparing vision model vs heuristic dynacard classification.
# ABOUTME: Implements BenchmarkRunner, BenchmarkResult, and run_benchmark convenience fn.

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Dict, List

from ..diagnostics import PumpDiagnostics
from .test_set_builder import LabelledCard, LabelledSet
from . import measurements
from .vision_classifier import VisionModelClassifier

# ---------------------------------------------------------------------------
# Result data structures
# ---------------------------------------------------------------------------


@dataclass
class MethodResult:
    """Accuracy statistics for a single classification method."""

    method_name: str
    correct: int
    wrong: int
    per_mode_accuracy: Dict[str, float] = field(default_factory=dict)
    per_mode_statistics: Dict[str, Any] = field(default_factory=dict)

    @property
    def accuracy(self) -> float:
        """Overall accuracy in [0, 1]."""
        total = self.correct + self.wrong
        if total == 0:
            return 0.0
        return self.correct / total

    def to_dict(self) -> Dict[str, Any]:
        """Serialise to a JSON-compatible dictionary."""
        return {
            "method_name": self.method_name,
            "correct": self.correct,
            "wrong": self.wrong,
            "accuracy": round(self.accuracy, 4),
            "per_mode_statistics": self.per_mode_statistics,
            "per_mode_accuracy": {
                k: round(v, 4) for k, v in self.per_mode_accuracy.items()
                if self.per_mode_statistics.get(k, {}).get("status") == "Evaluated"
            },
        }


@dataclass
class BenchmarkResult:
    """Aggregated results comparing heuristic vs vision model classifiers."""

    n_cards: int
    heuristic: MethodResult
    vision: MethodResult
    recommendation: str
    sections: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        """Serialise to a JSON-compatible dictionary."""
        return {
            "n_cards": self.n_cards,
            "heuristic": self.heuristic.to_dict(),
            "vision": self.vision.to_dict(),
            "recommendation": self.recommendation,
            "sections": self.sections,
        }


# ---------------------------------------------------------------------------
# Benchmark runner
# ---------------------------------------------------------------------------


class BenchmarkRunner:
    """Runs accuracy benchmarks of heuristic and vision classifiers.

    Four independent sections measure supplied synthetic fold predictions,
    generator/solver round trips, plate distances and labelled real pump cards.
    Missing inputs produce Not Evaluated sections. No model is fitted here.
    minimum_count defaults to 30; confidence defaults to 0.95. These are
    reporting settings, not evidence that a sample is independent or adequate.

    The original heuristic/vision fields remain a compatibility comparison.
    The heuristic field measures only the legacy threshold rules; the vision
    stub uses the numerical ML model with deterministic noise. It is a stub
    comparison, not an evaluation of a vision service or a real-card score.

    Args:
        test_set: List of LabelledCard objects to evaluate.
        vision_classifier: VisionModelClassifier instance.
    """

    def __init__(
        self,
        test_set: List[LabelledCard] | None = None,
        vision_classifier: VisionModelClassifier | None = None,
        *,
        cv_folds=None,
        round_trip_cards=None,
        round_trip_context=None,
        plate_cards=None,
        plate_exemplars=None,
        real_set: LabelledSet | None = None,
        minimum_count: int = measurements.DEFAULT_MINIMUM_COUNT,
        confidence: float = measurements.DEFAULT_CONFIDENCE,
        ml_classifier=None,
        heuristic_classifier=None,
    ) -> None:
        """Initialize the benchmark runner.

        Args:
            test_set: List of LabelledCard objects to evaluate.
            vision_classifier: VisionModelClassifier instance to benchmark.
        """
        self.test_set = list(test_set) if test_set is not None else []
        self.vision_classifier = vision_classifier
        self.cv_folds = cv_folds
        self.round_trip_cards = list(round_trip_cards) if round_trip_cards is not None else None
        self.round_trip_context = round_trip_context
        self.plate_cards = list(plate_cards) if plate_cards is not None else None
        self.plate_exemplars = plate_exemplars
        self.real_set = real_set
        self.scoring = {"minimum_count": minimum_count, "confidence": confidence}
        measurements.score_predictions([], [], **self.scoring)
        self.ml_classifier = ml_classifier or measurements.ml_prediction
        self.heuristic_classifier = heuristic_classifier or PumpDiagnostics._classify_legacy

    def run(self) -> BenchmarkResult:
        """Execute the benchmark and return a BenchmarkResult.

        Returns:
            BenchmarkResult with per-method accuracy and recommendation.
        """
        heuristic_result = self._evaluate_heuristic()
        vision_result = self._evaluate_vision()
        recommendation = (
            _derive_recommendation(heuristic_result, vision_result)
            if self.test_set and self.vision_classifier is not None
            else "[KEEP] No vision comparison evaluated."
        )
        sections = {
            "synthetic_cross_validation": measurements.cross_validation(self.cv_folds, **self.scoring),
            "solver_round_trip": measurements.round_trip(
                self.round_trip_cards, self.round_trip_context, self.ml_classifier, **self.scoring),
            "plate_distance": measurements.plate_distances(self.plate_cards, self.plate_exemplars),
            "real_card_accuracy": measurements.real_accuracy(
                self.real_set, self.ml_classifier, self.heuristic_classifier, **self.scoring),
        }

        return BenchmarkResult(
            n_cards=len(self.test_set),
            heuristic=heuristic_result,
            vision=vision_result,
            recommendation=recommendation,
            sections=sections,
        )

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _class_statistics(self, correct: int, total: int) -> dict:
        """Apply the same count threshold to compatibility per-mode fields."""
        item = {"count": total, "correct": correct, "wrong": total - correct}
        if total < self.scoring["minimum_count"]:
            return {**item, "status": "too thin to score"}
        return {**item, "status": "Evaluated", "accuracy": correct / total,
                "confidence": self.scoring["confidence"],
                "interval": list(measurements.wilson_interval(
                    correct, total, confidence=self.scoring["confidence"]))}

    def _evaluate_heuristic(self) -> MethodResult:
        """Evaluate only legacy threshold rules, independently of the ML path."""
        correct = 0
        wrong = 0
        mode_correct: Dict[str, int] = {}
        mode_total: Dict[str, int] = {}

        for lc in self.test_set:
            mode_total[lc.label] = mode_total.get(lc.label, 0) + 1
            prediction = self.heuristic_classifier(lc.card)
            if prediction == lc.label:
                correct += 1
                mode_correct[lc.label] = mode_correct.get(lc.label, 0) + 1
            else:
                wrong += 1

        per_mode = {
            mode: (mode_correct.get(mode, 0) / mode_total[mode])
            for mode in mode_total
        }

        return MethodResult(
            method_name="heuristic_legacy_threshold",
            correct=correct,
            wrong=wrong,
            per_mode_accuracy=per_mode,
            per_mode_statistics={
                mode: self._class_statistics(mode_correct.get(mode, 0), total)
                for mode, total in mode_total.items()
            },
        )

    def _evaluate_vision(self) -> MethodResult:
        """Evaluate the vision model classifier."""
        if self.vision_classifier is None:
            return MethodResult(method_name="vision_not_evaluated", correct=0, wrong=0)
        correct = 0
        wrong = 0
        mode_correct: Dict[str, int] = {}
        mode_total: Dict[str, int] = {}

        for lc in self.test_set:
            mode_total[lc.label] = mode_total.get(lc.label, 0) + 1
            result = self.vision_classifier.classify(lc.card, lc.label)
            if result.prediction == lc.label:
                correct += 1
                mode_correct[lc.label] = mode_correct.get(lc.label, 0) + 1
            else:
                wrong += 1

        per_mode = {
            mode: (mode_correct.get(mode, 0) / mode_total[mode])
            for mode in mode_total
        }

        return MethodResult(
            method_name=f"vision_{self.vision_classifier.backend.value}",
            correct=correct,
            wrong=wrong,
            per_mode_accuracy=per_mode,
            per_mode_statistics={
                mode: self._class_statistics(mode_correct.get(mode, 0), total)
                for mode, total in mode_total.items()
            },
        )


def run_benchmark(
    test_set: List[LabelledCard] | None = None,
    vision_classifier: VisionModelClassifier | None = None,
    **section_inputs,
) -> BenchmarkResult:
    """Convenience function: construct and run BenchmarkRunner in one call.

    Args:
        test_set: Hold-out labelled cards.
        vision_classifier: Vision model classifier to evaluate.

    Returns:
        BenchmarkResult with comparison and recommendation.
    """
    runner = BenchmarkRunner(
        test_set=test_set,
        vision_classifier=vision_classifier,
        **section_inputs,
    )
    return runner.run()


# ---------------------------------------------------------------------------
# Recommendation logic
# ---------------------------------------------------------------------------


def _derive_recommendation(
    heuristic: MethodResult,
    vision: MethodResult,
) -> str:
    """Derive a textual recommendation from benchmark results.

    Decision rules (mirroring WRK-251 acceptance criterion):
    - vision_accuracy >= heuristic_accuracy + 0.05  →  replace
    - vision_accuracy >= heuristic_accuracy - 0.02  →  augment
    - otherwise                                      →  keep heuristics

    Args:
        heuristic: MethodResult for the ML/heuristic baseline.
        vision: MethodResult for the vision model.

    Returns:
        Recommendation string with accuracy figures cited.
    """
    h_acc = heuristic.accuracy
    v_acc = vision.accuracy
    delta = v_acc - h_acc

    if delta >= 0.05:
        action = "replace"
        rationale = (
            f"Vision model outperforms heuristic by {delta:.1%} "
            f"({v_acc:.1%} vs {h_acc:.1%}). Recommend replacing heuristics."
        )
    elif delta >= -0.02:
        action = "augment"
        rationale = (
            f"Vision model is within 2% of heuristic accuracy "
            f"({v_acc:.1%} vs {h_acc:.1%}, delta={delta:+.1%}). "
            "Recommend augmenting: use vision model as second opinion on "
            "uncertain cases (confidence < 0.75)."
        )
    else:
        action = "keep"
        rationale = (
            f"Vision model underperforms heuristic by {-delta:.1%} "
            f"({v_acc:.1%} vs {h_acc:.1%}). Recommend keeping heuristics; "
            "re-evaluate in 2-3 months when vision model quality improves."
        )

    return f"[{action.upper()}] {rationale}"
