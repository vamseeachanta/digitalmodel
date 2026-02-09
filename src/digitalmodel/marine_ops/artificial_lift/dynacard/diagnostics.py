# ABOUTME: AI-driven pump diagnostics for sucker rod pumps.
# ABOUTME: ML-based classifier with 18 failure modes, fallback to legacy thresholds.

import json
from pathlib import Path

import numpy as np

from .models import CardData, AnalysisResults, DiagnosticResult
from .feature_extraction import FeatureExtractor
from .constants import (
    PUMP_TAGGING_LOAD_THRESHOLD_LBS,
    FLUID_POUND_LOAD_DIFF_THRESHOLD_LBS,
    GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS,
)

_MODEL_PATH = Path(__file__).parent / "data" / "dynacard_classifier.json"


class PumpDiagnostics:
    """AI-driven troubleshooting engine for Sucker Rod Pumps.

    Uses a pre-trained GradientBoosting classifier on Bezerra vertical
    projection features to classify card patterns into 18 pump failure modes.
    Falls back to legacy threshold-based rules if the model file is missing.
    """

    FAILURE_MODES = {
        # Tier 1: Core modes
        "NORMAL": "Well is operating within normal parameters.",
        "GAS_INTERFERENCE": (
            "Compression of gas in the pump barrel detected. "
            "Consider increasing intake pressure."
        ),
        "FLUID_POUND": (
            "Incomplete pump fillage. "
            "Pump speed may be too high for current inflow."
        ),
        "PUMP_TAGGING": (
            "Mechanical contact between plunger and standing valve "
            "or top of pump."
        ),
        "TUBING_MOVEMENT": (
            "Unanchored tubing causing excessive stroke loss. "
            "Consider installing a tubing anchor."
        ),
        "VALVE_LEAK_TV": (
            "Traveling valve leak detected. Fluid bypassing on upstroke "
            "causes load loss. Inspect traveling valve and seats."
        ),
        "VALVE_LEAK_SV": (
            "Standing valve leak detected. Fluid bypassing on downstroke "
            "causes load gain. Inspect standing valve and seats."
        ),
        # Tier 2: Common field failures
        "ROD_PARTING": (
            "Rod string parting detected. Near-zero card area with very "
            "low loads. Inspect rod string for breaks."
        ),
        "STUCK_PUMP": (
            "Pump is stuck with near-zero position range. "
            "Check for debris, scale, or mechanical obstruction."
        ),
        "WORN_BARREL": (
            "Worn barrel detected. Gradual area loss with upstroke "
            "load decay indicates barrel/plunger wear."
        ),
        "GAS_LOCK": (
            "Gas lock condition. Pump trapped by gas with near-zero "
            "card area. Install gas separator or vent gas."
        ),
        "DELAYED_TV_CLOSURE": (
            "Delayed traveling valve closure. Exponential load pickup "
            "at start of upstroke. Inspect valve spring."
        ),
        # Tier 3: Mechanical and operational
        "EXCESSIVE_FRICTION": (
            "Excessive friction in pump or tubing. Large hysteresis "
            "between upstroke and downstroke loads."
        ),
        "PLUNGER_UNDERTRAVEL": (
            "Plunger undertravel detected. Short net stroke with "
            "truncated position range."
        ),
        "PARAFFIN_RESTRICTION": (
            "Paraffin restriction causing concave dents and increased "
            "friction signature. Consider hot oil treatment."
        ),
        "BENT_BARREL": (
            "Bent barrel detected. Asymmetric load distribution with "
            "shifted centroid. Inspect barrel alignment."
        ),
        "SAND_ABRASION": (
            "Sand abrasion detected. Jagged load oscillations indicate "
            "sand production. Consider sand control."
        ),
        "EXCESSIVE_VIBRATION": (
            "Excessive vibration detected. High-frequency load oscillation "
            "suggests mechanical resonance or imbalance."
        ),
        # Legacy alias (backward compat)
        "VALVE_LEAK": (
            "Fluid bypassing through traveling or standing valves."
        ),
    }

    _model = None  # Lazy-loaded class-level cache

    @staticmethod
    def classify_card(downhole_card: CardData) -> str:
        """Pattern recognition for pump failure modes.

        Backward-compatible static method. Uses ML model when available,
        falls back to legacy threshold rules otherwise.

        Args:
            downhole_card: Downhole card data (position + load).

        Returns:
            Failure mode string (e.g. "NORMAL", "FLUID_POUND").
        """
        model = PumpDiagnostics._load_model()
        if model is not None:
            features = FeatureExtractor.extract_bezerra_projections(downhole_card)
            features_norm = FeatureExtractor.normalize(features, model["scaling"])
            mode, _ = _predict_from_trees(features_norm, model)
            return mode

        return PumpDiagnostics._classify_legacy(downhole_card)

    def classify_with_context(
        self,
        results: AnalysisResults,
    ) -> DiagnosticResult:
        """Classify using full feature vector for higher accuracy.

        Args:
            results: Full analysis results including geometry, ideal card, etc.

        Returns:
            DiagnosticResult with classification, confidence, and differential.
        """
        if results.downhole_card is None:
            return DiagnosticResult(
                classification="NORMAL",
                confidence=0.0,
                differential=[],
            )

        model = self._load_model()
        if model is not None:
            features = FeatureExtractor.extract_bezerra_projections(
                results.downhole_card
            )
            features_norm = FeatureExtractor.normalize(features, model["scaling"])
            mode, probabilities = _predict_from_trees(features_norm, model)

            # Build differential (top 3)
            class_labels = model["class_labels"]
            sorted_idx = np.argsort(probabilities)[::-1]
            differential = []
            for idx in sorted_idx[:3]:
                differential.append({
                    "mode": class_labels[idx],
                    "probability": round(float(probabilities[idx]), 4),
                })

            return DiagnosticResult(
                classification=mode,
                confidence=round(float(probabilities[sorted_idx[0]]), 4),
                differential=differential,
                model_version=model.get("model_version", "1.0"),
            )

        # Fallback
        mode = self._classify_legacy(results.downhole_card)
        return DiagnosticResult(
            classification=mode,
            confidence=1.0 if mode == "NORMAL" else 0.8,
            differential=[{"mode": mode, "probability": 1.0}],
            model_version="legacy",
        )

    def generate_troubleshooting_report(self, results: AnalysisResults) -> str:
        """Generate a natural language troubleshooting report.

        Backward-compatible method. Now uses ML classifier internally
        for richer diagnostics.
        """
        diag = self.classify_with_context(results)
        mode = diag.classification
        description = self.FAILURE_MODES.get(mode, "Unknown failure mode.")

        report = f"Classification: {mode}. {description}"

        if diag.confidence > 0 and diag.model_version != "legacy":
            report += f" (Confidence: {diag.confidence:.1%})"

        if diag.differential and len(diag.differential) > 1:
            alt = diag.differential[1]
            if alt["probability"] > 0.1:
                report += (
                    f" Alternative: {alt['mode']} "
                    f"({alt['probability']:.1%})"
                )

        if results.buckling_detected:
            report += (
                " WARNING: Mechanical buckling detected in rod string."
            )

        results.diagnostic_message = report
        return report

    @classmethod
    def _load_model(cls) -> dict | None:
        """Load JSON model, cache as class variable."""
        if cls._model is not None:
            return cls._model

        if not _MODEL_PATH.exists():
            return None

        try:
            with open(_MODEL_PATH) as f:
                model = json.load(f)
            # Convert scaling arrays to numpy
            model["scaling"] = {
                "min": np.array(model["scaling"]["min"]),
                "max": np.array(model["scaling"]["max"]),
            }
            cls._model = model
            return model
        except (json.JSONDecodeError, KeyError):
            return None

    @staticmethod
    def _classify_legacy(downhole_card: CardData) -> str:
        """Legacy threshold-based classification (fallback)."""
        pos = np.array(downhole_card.position)
        load = np.array(downhole_card.load)

        if np.max(load) > PUMP_TAGGING_LOAD_THRESHOLD_LBS:
            return "PUMP_TAGGING"

        mid_point = len(pos) // 2
        downstroke_load = load[mid_point:]
        load_diff = np.diff(downstroke_load)
        if np.max(np.abs(load_diff)) > FLUID_POUND_LOAD_DIFF_THRESHOLD_LBS:
            return "FLUID_POUND"

        if np.min(load) < GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS:
            return "GAS_INTERFERENCE"

        return "NORMAL"

    @classmethod
    def reset_model_cache(cls) -> None:
        """Clear cached model (useful for testing)."""
        cls._model = None


def _predict_from_trees(
    features: np.ndarray,
    model: dict,
) -> tuple:
    """Traverse GradientBoosting decision trees from JSON model.

    Args:
        features: Normalized feature vector of shape (n_features,).
        model: Loaded model dictionary.

    Returns:
        (predicted_class_label, probability_array)
    """
    n_classes = model["n_classes"]
    learning_rate = model["learning_rate"]
    trees = model["trees"]
    class_labels = model["class_labels"]

    # Initialize raw scores (log-odds for multiclass)
    raw_scores = np.zeros(n_classes)

    # Accumulate tree predictions
    for stage in trees:
        for class_idx, tree in enumerate(stage):
            value = _traverse_tree(features, tree)
            raw_scores[class_idx] += learning_rate * value

    # Softmax to get probabilities
    exp_scores = np.exp(raw_scores - np.max(raw_scores))
    probabilities = exp_scores / np.sum(exp_scores)

    predicted_idx = int(np.argmax(probabilities))
    return class_labels[predicted_idx], probabilities


def _traverse_tree(features: np.ndarray, tree: dict) -> float:
    """Traverse a single decision tree.

    Args:
        features: Feature vector.
        tree: Dict with feature, threshold, children_left, children_right, value.

    Returns:
        Leaf value (float).
    """
    node = 0
    feature_arr = tree["feature"]
    threshold_arr = tree["threshold"]
    left_arr = tree["children_left"]
    right_arr = tree["children_right"]
    value_arr = tree["value"]

    while feature_arr[node] >= 0:  # -2 indicates leaf
        if features[feature_arr[node]] <= threshold_arr[node]:
            node = left_arr[node]
        else:
            node = right_arr[node]

    return value_arr[node]
