# ABOUTME: Training pipeline for dynacard ML-based diagnostic classifier.
# ABOUTME: Generates synthetic data, trains GradientBoosting, exports model to JSON.

"""
Training script for the dynacard classifier.

Usage:
    PYTHONPATH="src:../assetutilities/src" python3 -m \
        digitalmodel.marine_ops.artificial_lift.dynacard.training

What ``cv_accuracy`` in the exported model means
------------------------------------------------
It is five-fold cross-validation on cards drawn from :mod:`card_generators`,
scored against cards drawn from the *same* generators. It measures how
separable this module's synthetic card shapes are from one another. It is
**not** a validation against field data and must never be quoted as a field
accuracy figure -- the classifier has still never been scored against a
labelled real dynamometer card (issue #1864).

Why the number jumped from 0.9048 to 0.9942, and why that is not a capability gain
----------------------------------------------------------------------------------
The 16-feature Bezerra projection normalizes both position and load to [0, 1].
That is what makes it unit- and amplitude-robust, and also what made five modes
unseparable: NORMAL, TUBING_MOVEMENT and PLUNGER_UNDERTRAVEL differ *only* in
absolute plunger stroke, so under projections alone they were not merely
similar, they were the identical vector (dm#1884). Adding three
absolute-magnitude features -- stroke length, load range, enclosed card area
(see :meth:`FeatureExtractor.extract_scale_features`) -- took five-fold CV from
0.9048 to 0.9942 and unseen-seed accuracy from 0.9130 to 0.9960. The five modes
that were failing went from 0.437 / 0.507 / 0.897 / 0.770 / 0.850 (NORMAL,
TUBING_MOVEMENT, PLUNGER_UNDERTRAVEL, GAS_LOCK, STUCK_PUMP) to 1.000 / 1.000 /
1.000 / 0.987 / 1.000 on unseen seeds.

**That gain is substantially definitional, not diagnostic.** The generators draw
stroke from ranges that do not overlap anywhere:

* PLUNGER_UNDERTRAVEL  30-55 in
* NORMAL and 16 others 80-120 in
* TUBING_MOVEMENT      130-180 in
* STUCK_PUMP           2-10 in

A hand-written two-threshold rule on stroke alone -- ``<67.5 in`` ->
PLUNGER_UNDERTRAVEL, ``67.5-125 in`` -> NORMAL, ``>125 in`` -> TUBING_MOVEMENT
-- scores 900/900 on unseen seeds for those three modes. GAS_LOCK and
STUCK_PUMP separate the same way on ``load_range`` and ``stroke_length``.
Measured across all 20 modes, 54/190 mode pairs have fully disjoint stroke
ranges, 52/190 disjoint load ranges and 54/190 disjoint card areas. The
classifier is reading a generator parameter, not a card property.

Real wells do not partition this way: stroke length is set by the surface unit
and the rod string, so a healthy well and a well with tubing movement can have
identical plunger travel. Whether these modes separate on real cards is
**unknown and untested** (issue #1864). Treat 0.9942 as an upper bound on
synthetic separability and as no evidence at all about field performance.

One physics note, deliberately not acted on here: TUBING_MOVEMENT's 130-180 in
stroke looks backwards for a *downhole* card. Unanchored tubing stretches as
fluid load transfers to it on the upstroke, and that stretch is lost motion --
the plunger should travel *less* than the surface stroke, not more. The
generator docstring ("elongated card, excessive position range") describes a
surface-card appearance applied to a pump-card generator. Changing the range is
a physics decision that would invalidate #1875's plate calibration and every
pinned value downstream, so it is recorded rather than fixed.
"""

import json
import os
from pathlib import Path

import numpy as np

from .card_generators import ALL_GENERATORS, generate_training_dataset
from .feature_extraction import FeatureExtractor

_MODEL_PATH = Path(__file__).parent / "data" / "dynacard_classifier.json"


def _extract_features_for_dataset(
    cards: list,
    labels: list,
) -> tuple:
    """Extract Bezerra feature vectors for all cards.

    Returns:
        (X, y, scaling) where X is (n_samples, n_features), y is labels array,
        and scaling is dict with 'min' and 'max' arrays.
    """
    X_list = []
    for card in cards:
        # Same entry point inference uses -- see extract_classifier_vector.
        features = FeatureExtractor.extract_classifier_vector(card)
        X_list.append(features)

    X = np.array(X_list)

    # Compute scaling bounds
    feat_min = X.min(axis=0)
    feat_max = X.max(axis=0)

    # Normalize
    scaling = {"min": feat_min, "max": feat_max}
    X_norm = np.zeros_like(X)
    for i in range(X.shape[0]):
        X_norm[i] = FeatureExtractor.normalize(X[i], scaling)

    return X_norm, np.array(labels), scaling


def _export_model_to_json(
    clf,
    class_labels: list,
    scaling: dict,
    cv_accuracy: float,
    output_path: Path,
) -> None:
    """Export a trained GradientBoostingClassifier to JSON.

    Stores the full tree structure so the model can be loaded
    and evaluated without sklearn at runtime.
    """
    estimators = clf.estimators_
    n_classes = len(class_labels)
    learning_rate = clf.learning_rate

    trees = []
    for stage_idx in range(len(estimators)):
        stage_trees = []
        for class_idx in range(estimators.shape[1]):
            tree = estimators[stage_idx, class_idx].tree_
            stage_trees.append({
                "feature": tree.feature.tolist(),
                "threshold": tree.threshold.tolist(),
                "children_left": tree.children_left.tolist(),
                "children_right": tree.children_right.tolist(),
                "value": tree.value[:, 0, 0].tolist(),
            })
        trees.append(stage_trees)

    model_dict = {
        "model_version": "1.0",
        "algorithm": "GradientBoostingClassifier",
        "n_classes": n_classes,
        "class_labels": class_labels,
        "learning_rate": learning_rate,
        "n_estimators": len(estimators),
        "init_value": clf.init_.class_prior_.tolist() if hasattr(clf.init_, 'class_prior_') else None,
        "trees": trees,
        "scaling": {
            "min": scaling["min"].tolist(),
            "max": scaling["max"].tolist(),
        },
        "training_metadata": {
            "n_features": int(estimators[0, 0].tree_.n_features),
            "cv_accuracy": float(cv_accuracy),
            "feature_names": FeatureExtractor.classifier_feature_names(),
        },
    }

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w") as f:
        json.dump(model_dict, f, indent=2)


def train_and_export(
    samples_per_mode: int = 300,
    output_path: Path = _MODEL_PATH,
) -> dict:
    """Full training pipeline: generate data -> train -> validate -> export.

    Returns:
        Dict with training metrics.
    """
    from sklearn.ensemble import GradientBoostingClassifier
    from sklearn.model_selection import cross_val_score
    from sklearn.preprocessing import LabelEncoder

    print(f"Generating {samples_per_mode} samples per mode "
          f"({len(ALL_GENERATORS)} modes)...")
    cards, labels = generate_training_dataset(samples_per_mode=samples_per_mode)

    print("Extracting features...")
    X, y_str, scaling = _extract_features_for_dataset(cards, labels)

    # Encode labels
    le = LabelEncoder()
    y = le.fit_transform(y_str)
    class_labels = le.classes_.tolist()

    print(f"Training GradientBoostingClassifier on {X.shape[0]} samples, {X.shape[1]} features...")
    clf = GradientBoostingClassifier(
        n_estimators=100,
        max_depth=4,
        learning_rate=0.15,
        subsample=0.8,
        random_state=42,
    )

    # 5-fold cross-validation
    print("Running 5-fold cross-validation...")
    cv_scores = cross_val_score(clf, X, y, cv=5, scoring="accuracy")
    cv_accuracy = float(np.mean(cv_scores))
    print(f"CV accuracy: {cv_accuracy:.4f} (+/- {np.std(cv_scores):.4f})")

    # Train on full dataset
    clf.fit(X, y)

    # Export
    print(f"Exporting model to {output_path}...")
    _export_model_to_json(clf, class_labels, scaling, cv_accuracy, output_path)

    # Verify
    file_size = output_path.stat().st_size
    print(f"Model exported: {file_size:,} bytes")

    return {
        "cv_accuracy": cv_accuracy,
        "cv_std": float(np.std(cv_scores)),
        "n_samples": X.shape[0],
        "n_features": X.shape[1],
        "n_classes": len(class_labels),
        "model_path": str(output_path),
        "file_size_bytes": file_size,
    }


if __name__ == "__main__":
    metrics = train_and_export()
    print("\n--- Training Complete ---")
    for k, v in metrics.items():
        print(f"  {k}: {v}")
