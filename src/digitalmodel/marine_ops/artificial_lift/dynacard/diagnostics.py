# ABOUTME: AI-driven pump diagnostics for sucker rod pumps.
# ABOUTME: ML-based classifier with 20 failure modes, fallback to legacy thresholds.

import json
from pathlib import Path

import numpy as np

from .models import CardData, AnalysisResults, DiagnosticResult
from .feature_extraction import FeatureExtractor
# PUMP_TAGGING_LOAD_THRESHOLD_LBS is deliberately not imported. It was the
# peak-load test in _classify_legacy, which could not tell tagging up from
# tagging down and did not in fact detect either (see _classify_legacy). The
# constant is left defined in constants.py for anyone who wants a barrel-rating
# exceedance check, but it no longer backs a failure-mode claim here.
from .constants import (
    FLUID_POUND_LOAD_DIFF_THRESHOLD_LBS,
    GAS_INTERFERENCE_MIN_LOAD_THRESHOLD_LBS,
)

_MODEL_PATH = Path(__file__).parent / "data" / "dynacard_classifier.json"


class PumpDiagnostics:
    """AI-driven troubleshooting engine for Sucker Rod Pumps.

    Uses a pre-trained GradientBoosting classifier on Bezerra vertical
    projection features to classify card patterns into 20 pump failure modes.
    Falls back to legacy threshold-based rules if the model file is missing --
    a narrower fallback that reports only NORMAL, FLUID_POUND and
    GAS_INTERFERENCE and abstains from anything needing card morphology,
    notably the two tagging directions. See :meth:`_classify_legacy`.
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
        "PUMP_TAGGING_UP": (
            "Plunger striking the top of the pump at maximum stroke. "
            "Space the pump down."
        ),
        "PUMP_TAGGING_DOWN": (
            "Plunger striking the standing valve or bottom of the barrel at "
            "minimum stroke. Space the pump up."
        ),
        "PLUNGER_OUT_OF_BARREL": (
            "Plunger leaving the top of the barrel part-way up the upstroke, "
            "dumping fluid load mid-stroke. Space the pump down."
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
        # Legacy aliases (backward compat)
        "VALVE_LEAK": (
            "Fluid bypassing through traveling or standing valves."
        ),
        # Retired in favour of PUMP_TAGGING_UP / PUMP_TAGGING_DOWN, which are
        # opposite mechanisms with opposite repairs. The retrained 20-class
        # model never predicts this label; it is kept so archived results and
        # stored configs carrying the old name still resolve to a description.
        "PUMP_TAGGING": (
            "Mechanical contact between plunger and standing valve "
            "or top of pump."
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
            features = FeatureExtractor.extract_classifier_vector(downhole_card)
            _check_feature_count(features, model)
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
            features = FeatureExtractor.extract_classifier_vector(
                results.downhole_card
            )
            _check_feature_count(features, model)
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

    def generate_troubleshooting_report(
        self,
        results: AnalysisResults,
        diag: DiagnosticResult | None = None,
    ) -> str:
        """Generate a natural language troubleshooting report.

        Backward-compatible method. Now uses ML classifier internally
        for richer diagnostics.
        """
        diag = diag or self.classify_with_context(results)
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
        """Legacy threshold-based classification (fallback).

        Fires only when :meth:`_load_model` returns ``None`` -- the shipped
        ``data/dynacard_classifier.json`` is missing or unparseable. That is a
        degraded install, not a normal operating mode, and it is the worst
        possible moment to guess.

        **This classifier does not diagnose pump tagging, in either
        direction.** Tagging up and tagging down are opposite mechanisms with
        opposite repairs -- space the pump down versus space it up -- so a
        one-sided answer is not a partial answer, it is a 50% chance of the
        inverse field action. Distinguishing them needs card *morphology*
        (which end of the stroke the impact sits at, and whether load spikes
        above the upstroke plateau or dips below the downstroke line), which
        is what the trained model reads and what a peak/trough threshold
        cannot. Abstaining leaves the operator to look at the card; guessing
        sends a crew to re-space a pump the wrong way.

        The rule this replaced compared peak load against
        ``PUMP_TAGGING_LOAD_THRESHOLD_LBS`` (38,000 lb) and returned
        ``PUMP_TAGGING_UP``. Two independent problems, both measurable against
        this package's own generators:

        1. It could never return ``PUMP_TAGGING_DOWN``. A down-tag is a load
           *minimum*; a maximum-load test is structurally blind to it.
        2. It did not detect tagging up either. Every card
           ``generate_pump_tagging_up_card`` produces peaks at 16-21 klb, so
           the 38 klb rule never fired on a real up-tagging card. What it
           actually fired on was absolute load above a barrel rating -- a
           magnitude excursion that carries no information about which end of
           the pump is being struck, and that a genuinely tagging-down card on
           a deep well trips just as readily.

        So the branch was not "tagging detection missing its down half"; it
        detected a different quantity and mislabelled it. Making it two-sided
        would mean writing a second, uncalibrated morphology classifier and
        shipping it under the fallback -- a fabricated capability, and exactly
        the defect class dm#1952 objects to.

        Returning the retired direction-neutral ``PUMP_TAGGING`` is not an
        available escape either: ``card_generators.MODE_ALIASES`` and
        ``report_sections.TROUBLESHOOTING_ALIASES`` both resolve it back to
        ``PUMP_TAGGING_UP``, so the report would still print "space the pump
        down".

        Returns:
            One of ``NORMAL``, ``FLUID_POUND``, ``GAS_INTERFERENCE`` -- the
            findings a threshold rule can honestly support.
        """
        pos = np.array(downhole_card.position)
        load = np.array(downhole_card.load)

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


def _check_feature_count(features: np.ndarray, model: dict) -> None:
    """Fail loudly when the shipped model predates the current feature vector.

    A stale model is the dangerous case, not a broken one: its ``scaling``
    arrays are shorter than the vector, numpy broadcasts or raises deep inside
    :meth:`FeatureExtractor.normalize`, and if it happens to line up the trees
    split on features that are not the ones they were trained on -- producing
    confident, wrong diagnoses with nothing to indicate anything went wrong.
    Adding a feature (dm#1884) without retraining is exactly how that happens.
    """
    expected = len(model["scaling"]["min"])
    if len(features) != expected:
        raise ValueError(
            f"dynacard model expects {expected} features but the extractor "
            f"produced {len(features)} -- the shipped "
            f"data/dynacard_classifier.json predates the current feature "
            f"vector. Retrain with training.train_and_export()."
        )


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

    # Initialize from prior log-odds (sklearn's init estimator)
    init_value = model.get("init_value")
    raw_scores = np.array(init_value) if init_value is not None else np.zeros(n_classes)

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
