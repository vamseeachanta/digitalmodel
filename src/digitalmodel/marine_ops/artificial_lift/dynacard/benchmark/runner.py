# ABOUTME: Benchmark runner comparing vision model vs heuristic dynacard classification.
# ABOUTME: Implements BenchmarkRunner, BenchmarkResult, and run_benchmark convenience fn.

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Dict, List

from ..diagnostics import PumpDiagnostics
from .test_set_builder import LabelledCard
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
            "per_mode_accuracy": {
                k: round(v, 4) for k, v in self.per_mode_accuracy.items()
            },
        }


@dataclass
class BenchmarkResult:
    """Aggregated results comparing heuristic vs vision model classifiers."""

    n_cards: int
    heuristic: MethodResult
    vision: MethodResult
    recommendation: str

    def to_dict(self) -> Dict[str, Any]:
        """Serialise to a JSON-compatible dictionary."""
        return {
            "n_cards": self.n_cards,
            "heuristic": self.heuristic.to_dict(),
            "vision": self.vision.to_dict(),
            "recommendation": self.recommendation,
        }


# ---------------------------------------------------------------------------
# Benchmark runner
# ---------------------------------------------------------------------------


class BenchmarkRunner:
    """Runs accuracy benchmarks of heuristic and vision classifiers.

    The heuristic baseline is the legacy threshold classifier plus the
    WRK-093 GradientBoosting ML classifier. The vision classifier is
    any VisionModelClassifier implementation (stub or live API).

    Args:
        test_set: List of LabelledCard objects to evaluate.
        vision_classifier: VisionModelClassifier instance.
    """

    def __init__(
        self,
        test_set: List[LabelledCard],
        vision_classifier: VisionModelClassifier,
    ) -> None:
        self.test_set = test_set
        self.vision_classifier = vision_classifier

    def run(self) -> BenchmarkResult:
        """Execute the benchmark and return a BenchmarkResult.

        Returns:
            BenchmarkResult with per-method accuracy and recommendation.
        """
        heuristic_result = self._evaluate_heuristic()
        vision_result = self._evaluate_vision()
        recommendation = _derive_recommendation(heuristic_result, vision_result)

        return BenchmarkResult(
            n_cards=len(self.test_set),
            heuristic=heuristic_result,
            vision=vision_result,
            recommendation=recommendation,
        )

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _evaluate_heuristic(self) -> MethodResult:
        """Evaluate the WRK-093 ML heuristic (GradientBoosting + legacy fallback)."""
        correct = 0
        wrong = 0
        mode_correct: Dict[str, int] = {}
        mode_total: Dict[str, int] = {}

        for lc in self.test_set:
            mode_total[lc.label] = mode_total.get(lc.label, 0) + 1
            prediction = PumpDiagnostics.classify_card(lc.card)
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
            method_name="heuristic_ml_wrk093",
            correct=correct,
            wrong=wrong,
            per_mode_accuracy=per_mode,
        )

    def _evaluate_vision(self) -> MethodResult:
        """Evaluate the vision model classifier."""
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
        )


def run_benchmark(
    test_set: List[LabelledCard],
    vision_classifier: VisionModelClassifier,
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
