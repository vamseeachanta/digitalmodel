# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Separate scoring paths; no training or exported model mutation.

Wilson score formula: NIST/SEMATECH e-Handbook, section 7.2.4.1,
https://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm
The minimum count is a configurable reporting threshold, not a power analysis.
Intervals describe binomial card counts; correlated cards need a separate study.
"""

from __future__ import annotations

from dataclasses import asdict
from math import isfinite, sqrt
from statistics import NormalDist

import numpy as np

from ..card_generators import ALL_GENERATORS, surface_card_from_pump_card
from ..diagnostics import PumpDiagnostics
from ..solver import DynacardWorkflow
from .test_set_builder import LabelledCard, LabelledSet, validate_card

MODES = tuple(ALL_GENERATORS)
DEFAULT_MINIMUM_COUNT = 30
DEFAULT_CONFIDENCE = 0.95


def wilson_interval(correct: int, total: int, *, z: float | None = None,
                    confidence: float = DEFAULT_CONFIDENCE) -> tuple[float, float]:
    """Return the two-sided Wilson interval without rounding the calculation."""
    if (not isinstance(correct, int) or not isinstance(total, int)
            or total <= 0 or not 0 <= correct <= total):
        raise ValueError("Require integer counts with 0 <= correct <= total and total > 0")
    if not 0 < confidence < 1:
        raise ValueError("Confidence must lie strictly between zero and one")
    z = NormalDist().inv_cdf((1 + confidence) / 2) if z is None else z
    if not isfinite(z) or z <= 0:
        raise ValueError("Normal quantile must be finite and positive")
    p = correct / total
    denominator = 1 + z * z / total
    centre = (p + z * z / (2 * total)) / denominator
    radius = z * sqrt(p * (1 - p) / total + z * z / (4 * total * total)) / denominator
    return max(0.0, centre - radius), min(1.0, centre + radius)


def score_predictions(labels, predictions, *, minimum_count=DEFAULT_MINIMUM_COUNT,
                      confidence=DEFAULT_CONFIDENCE) -> dict:
    """Count every current class; suppress percentages below the threshold."""
    labels, predictions = list(labels), list(predictions)
    if not isinstance(minimum_count, int) or minimum_count < 1:
        raise ValueError("Minimum count must be a positive integer")
    if not 0 < confidence < 1:
        raise ValueError("Confidence must lie strictly between zero and one")
    if len(labels) != len(predictions) or any(label not in MODES for label in labels):
        raise ValueError("Predictions must pair with current-mode labels")
    if any(not isinstance(p, str) or not p for p in predictions):
        raise ValueError("Each prediction must be a nonempty class name")
    # Abstentions or unexpected predictions retain separate columns, never aliases.
    columns = list(MODES) + sorted(set(predictions) - set(MODES))
    matrix = {mode: dict.fromkeys(columns, 0) for mode in MODES}
    for label, prediction in zip(labels, predictions, strict=True):
        matrix[label][prediction] += 1
    per_class = {}
    for mode, row in matrix.items():
        total, correct = sum(row.values()), row[mode]
        item = {"count": total, "correct": correct, "wrong": total - correct}
        if total < minimum_count:
            item["status"] = "too thin to score"
        else:
            item.update(status="Evaluated", accuracy=correct / total,
                        interval=list(wilson_interval(correct, total, confidence=confidence)))
        per_class[mode] = item
    return {"status": "Evaluated" if labels else "Not Evaluated", "count": len(labels),
            "minimum_count": minimum_count, "confidence": confidence,
            "interval_method": "Wilson score", "per_class": per_class,
            "confusion_matrix": matrix}


def _not_evaluated(path: str, reason: str) -> dict:
    return {"path": path, "status": "Not Evaluated", "reason": reason}


def cross_validation(folds, **scoring) -> dict:
    """Score supplied synthetic out-of-fold predictions; never fit a model."""
    path = "synthetic pump cards -> out-of-fold classifier predictions"
    if not folds:
        return _not_evaluated(path, "No synthetic out-of-fold predictions supplied; archived accuracy is not rescored")
    labels, predictions, seen = [], [], set()
    for fold in folds:
        train_ids, test_ids = fold["train_ids"], fold["test_ids"]
        if not train_ids or not test_ids or len(set(test_ids)) != len(test_ids):
            raise ValueError("Folds require training IDs and unique held-out IDs")
        if set(train_ids) & set(test_ids) or seen & set(test_ids):
            raise ValueError("Training overlap or repeated held-out sample")
        if len(test_ids) != len(fold["labels"]) or len(test_ids) != len(fold["predictions"]):
            raise ValueError("Fold IDs, labels and predictions must pair")
        seen.update(test_ids)
        labels.extend(fold["labels"])
        predictions.extend(fold["predictions"])
    return {"path": path, "fold_count": len(folds),
            "folds": folds, **score_predictions(labels, predictions, **scoring)}


def ml_prediction(card) -> str:
    """Use the shipped classifier only when its model is available."""
    if PumpDiagnostics._load_model() is None:
        raise ValueError("ML model unavailable; a heuristic fallback is not an ML measurement")
    return PumpDiagnostics.classify_card(card)


def round_trip(cards, context, classifier, **scoring) -> dict:
    """Forward-model pump cards, invert with the workflow default, then classify."""
    path = "synthetic pump card -> forward-modelled surface -> default solver pump card -> ML classification"
    if cards is None:
        return _not_evaluated(path, "No synthetic round-trip cards supplied")
    if context is None:
        raise ValueError("Round-trip cards require an explicit well context")
    labels, predictions, methods = [], [], set()
    for card in cards:
        if card.provenance.kind != "synthetic":
            raise ValueError("Round-trip input must be synthetic pump cards")
        validate_card(card.card)
        surface = surface_card_from_pump_card(card.card, context)
        workflow = DynacardWorkflow(context.model_copy(deep=True, update={"surface_card": surface}))
        recovered = workflow.solver.solve().downhole_card
        validate_card(recovered)
        labels.append(card.label)
        predictions.append(classifier(recovered))
        methods.add(workflow.solver_method)
    return {"path": path, "solver_method": next(iter(methods)) if len(methods) == 1 else sorted(methods),
            "context": context.model_dump(exclude={"surface_card", "api14"}),
            "provenance": [asdict(c.provenance) for c in cards],
            "seeds": [c.seed for c in cards],
            **score_predictions(labels, predictions, **scoring)}


def _prepare(position, load, points=128):
    axes = []
    for axis in (position, load):
        values = np.asarray(axis, dtype=float)
        span = np.ptp(values)
        axes.append((values - values.min()) / span if span else np.zeros_like(values))
    x, y = (np.append(a, a[0]) for a in axes)
    distance = np.concatenate([[0.0], np.cumsum(np.hypot(np.diff(x), np.diff(y)))])
    if distance[-1] == 0:
        return np.zeros((points, 2))
    target = np.linspace(0, distance[-1], points, endpoint=False)
    return np.stack([np.interp(target, distance, x), np.interp(target, distance, y)], axis=1)


def loop_distance(position_a, load_a, position_b, load_b) -> float:
    """Repository plate metric: normalised, resampled, phase and direction invariant."""
    from ..models import CardData
    for position, load in ((position_a, load_a), (position_b, load_b)):
        validate_card(CardData(position=position, load=load))
    a, b = _prepare(position_a, load_a), _prepare(position_b, load_b)
    return min(float(np.linalg.norm(a - np.roll(oriented, shift, axis=0), axis=1).mean())
               for oriented in (b, b[::-1]) for shift in range(len(b)))


def plate_distances(cards, exemplars) -> dict:
    """Measure each supplied generator card against its declared plate exemplar."""
    path = "synthetic generator pump card -> per-axis normalised loop -> CC0 plate exemplar distance"
    if cards is None or exemplars is None:
        return _not_evaluated(path, "Generator cards and plate exemplars must both be supplied")
    by_mode = {}
    for name, exemplar in exemplars.items():
        mode = exemplar["mode"]
        if mode not in MODES or mode in by_mode:
            raise ValueError("Plate crosswalk must identify unique current modes")
        by_mode[mode] = (name, exemplar)
    per_class = {}
    for mode in MODES:
        matching = [card for card in cards if card.label == mode]
        if mode not in by_mode:
            per_class[mode] = {"status": "No exemplar", "count": len(matching)}
            continue
        name, exemplar = by_mode[mode]
        distances = []
        for card in matching:
            if card.provenance.kind != "synthetic":
                raise ValueError("Plate comparison input must be synthetic generator cards")
            distances.append(loop_distance(card.card.position, card.card.load,
                                           exemplar["position"], exemplar["load"]))
        per_class[mode] = {"status": "Evaluated" if matching else "Not Evaluated",
                           "exemplar": name, "count": len(matching), "distances": distances,
                           "median_distance": float(np.median(distances)) if distances else None}
    return {"path": path, "status": "Evaluated" if cards else "Not Evaluated",
            "metric": "minimum mean Euclidean distance, 128 arc-length samples, both directions",
            "crosswalk": [{"exemplar": name, "mode": e["mode"]} for name, e in exemplars.items()],
            "per_class": per_class}


def real_accuracy(dataset: LabelledSet | None, ml_classifier, heuristic_classifier, **scoring) -> dict:
    """Score supplied real pump cards separately through ML and threshold paths."""
    path = "labelled real pump cards -> independent ML and legacy threshold classifications"
    if dataset is None:
        return _not_evaluated(path, "No labelled real-card set supplied")
    if dataset.card_space != "pump" or dataset.units != {"position": "in", "load": "lbf"}:
        raise ValueError("Real-card scoring requires pump cards in inches and lbf")
    for card in dataset.cards:
        if card.provenance.kind != "real":
            raise ValueError("Synthetic or plate cards cannot establish real-card accuracy")
        validate_card(card.card)
    labels = [c.label for c in dataset.cards]
    methods = {name: score_predictions(labels, [classifier(c.card) for c in dataset.cards], **scoring)
               for name, classifier in (("ml", ml_classifier), ("heuristic", heuristic_classifier))}
    return {"path": path, "status": "Evaluated" if dataset.cards else "Not Evaluated",
            "methods": methods, "crosswalk": dataset.crosswalk,
            "unmapped_labels": dataset.unmapped_labels,
            "provenance": [asdict(c.provenance) for c in dataset.cards]}
