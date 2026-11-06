# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic and offline entry-point checks for real-card scoring."""

import importlib
import json
import subprocess
import sys
from pathlib import Path

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark import (
    BenchmarkRunner,
    load_labelled_set,
    score_predictions,
)

PACKAGE = "digitalmodel.marine_ops.artificial_lift.dynacard.benchmark"


def document():
    """Construct an invented transport fixture, never field evidence.

    Coordinates are the four corners of a unit square, used only to exercise
    ingestion. The recorded tag simulates an incoming record, not acquisition.
    """
    return {
        "card_space": "pump",
        "units": {"position": "in", "load": "lbf"},
        "crosswalk": [
            {"external_label": "healthy", "mode": "NORMAL", "reason": "fixture mapping"},
            {"external_label": "gas", "mode": "GAS_LOCK", "reason": "fixture mapping"},
        ],
        "cards": [
            {
                "position": [0, 1, 1, 0], "load": [0, 0, 1, 1], "label": label,
                "provenance": {
                    "source": "invented transport fixture; not field evidence",
                    "labeller": "fixture definition", "date": "2026-10-19",
                    "licence": "MIT", "kind": "real", "acquisition": "recorded",
                },
            }
            for label in ["healthy", "healthy", "gas", "gas"]
        ],
    }


def test_overall_score_matches_independent_hand_calculation():
    """Accuracy = (1 normal + 1 gas correct) / 4 = 1/2.

    NIST/SEMATECH e-Handbook 7.2.4.1 (Wilson score formula, accessed 2026-10-19):
    with confidence = Phi(2)-Phi(-2), z = 2, p = 1/2 and n = 4,
    denominator = 2, centre = 1/2, radius = sqrt(1/8).
    Thus the bounds are (1-sqrt(1/2))/2 and (1+sqrt(1/2))/2.
    The sample count threshold is a fixture input, not an acceptance limit.
    """
    from statistics import NormalDist

    confidence = 2 * NormalDist().cdf(2) - 1
    score = score_predictions(
        ["NORMAL", "NORMAL", "GAS_LOCK", "GAS_LOCK"],
        ["NORMAL", "GAS_LOCK", "GAS_LOCK", "NORMAL"],
        minimum_count=4, confidence=confidence,
    )
    overall = score["overall"]
    assert overall["status"] == "Evaluated"
    assert overall["count"] == 4
    assert overall["correct"] == overall["wrong"] == 2
    assert overall["accuracy"] == 0.5
    assert overall["interval"] == pytest.approx(
        [(1 - (1 / 2) ** 0.5) / 2, (1 + (1 / 2) ** 0.5) / 2]
    )
    assert "accuracy" not in score["per_class"]["NORMAL"]


@pytest.mark.parametrize("labels,status", [([], "Not Evaluated"), (["NORMAL"], "too thin to score")])
def test_overall_suppresses_empty_and_below_threshold_scores(labels, status):
    """Zero samples have no estimate; one sample is below the supplied two."""
    score = score_predictions(labels, labels, minimum_count=2)
    assert score["overall"]["status"] == status
    assert "accuracy" not in score["overall"]
    assert "interval" not in score["overall"]


def test_real_methods_retain_independent_predictions_and_counts():
    """All-normal predicts 2/4; all-gas also predicts 2/4, on different rows."""
    result = BenchmarkRunner(
        real_set=load_labelled_set(document()), minimum_count=4,
        ml_classifier=lambda card: "NORMAL", heuristic_classifier=lambda card: "GAS_LOCK",
    ).run().to_dict()["sections"]["real_card_accuracy"]
    for method in ("ml", "heuristic"):
        assert result["methods"][method]["overall"]["accuracy"] == 0.5
        assert result["methods"][method]["overall"]["correct"] == 2
    assert [row["expected"] for row in result["predictions"]] == [
        "NORMAL", "NORMAL", "GAS_LOCK", "GAS_LOCK",
    ]
    assert [row["external_label"] for row in result["predictions"]] == [
        "healthy", "healthy", "gas", "gas",
    ]
    assert all(row["ml"] == "NORMAL" and row["heuristic"] == "GAS_LOCK"
               for row in result["predictions"])
    assert [row["provenance_index"] for row in result["predictions"]] == list(range(4))


def test_cli_empty_corpus_has_no_field_estimate_or_model_claim(tmp_path):
    cli = importlib.import_module(PACKAGE + ".__main__")
    empty = document()
    empty["cards"] = []
    path = tmp_path / "empty.json"
    path.write_text(json.dumps(empty))
    result = cli.benchmark_document(path, minimum_count=4, confidence=0.95)
    assert result["input_sha256"]
    assert result["sections"]["real_card_accuracy"]["status"] == "Not Evaluated"
    assert result["classifier_artifact"]["status"] == "Not Evaluated"
    assert result["classifier_artifact"]["training_domain"] == "synthetic"


def test_cli_scores_with_shipped_model_and_retains_artifact_digest(tmp_path):
    """Plumbing test only: counts and digests do not establish field performance."""
    cli = importlib.import_module(PACKAGE + ".__main__")
    path = tmp_path / "fixture.json"
    path.write_text(json.dumps(document()))
    result = cli.benchmark_document(path, minimum_count=4, confidence=0.95)
    assert result["classifier_artifact"]["status"] == "Evaluated"
    assert result["classifier_artifact"]["sha256"]
    assert "cv_accuracy" not in result["classifier_artifact"]
    real = result["sections"]["real_card_accuracy"]
    for method in ("ml", "heuristic"):
        counts = real["methods"][method]["overall"]
        assert counts["correct"] + counts["wrong"] == len(document()["cards"])
    assert len(real["predictions"]) == len(document()["cards"])
    assert json.loads(json.dumps(result, allow_nan=False)) == result


def test_cli_uses_the_identified_model_instead_of_an_existing_cache(tmp_path, monkeypatch):
    """A cached model may differ from the file whose digest is reported."""
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import PumpDiagnostics

    cli = importlib.import_module(PACKAGE + ".__main__")
    path = tmp_path / "fixture.json"
    path.write_text(json.dumps(document()))
    expected = cli.benchmark_document(path, minimum_count=4, confidence=0.95)
    monkeypatch.setattr(PumpDiagnostics, "_model", {"invalid": "cached model"})
    actual = cli.benchmark_document(path, minimum_count=4, confidence=0.95)
    assert actual == expected


def test_cli_model_scores_against_hand_constructed_tree_logits(tmp_path, monkeypatch):
    """One constant tree per class yields logits NORMAL=1 and GAS_LOCK=0.

    The constructed model therefore predicts NORMAL for every input: two
    correct and two wrong in document(), accuracy 1/2. This expected class
    comes from the supplied tree leaves, independent of feature extraction.
    Leaf sentinel -2 follows diagnostics._traverse_tree's JSON schema.
    """
    cli = importlib.import_module(PACKAGE + ".__main__")
    from digitalmodel.marine_ops.artificial_lift.dynacard.feature_extraction import FeatureExtractor

    def stump(value):
        return {"feature": [-2], "threshold": [0], "children_left": [-1],
                "children_right": [-1], "value": [value]}

    size = len(FeatureExtractor.classifier_feature_names())
    model = {
        "model_version": "arithmetic-fixture", "n_classes": 2,
        "class_labels": ["NORMAL", "GAS_LOCK"], "learning_rate": 1,
        "init_value": [0, 0], "trees": [[stump(1), stump(0)]],
        "scaling": {"min": [0] * size, "max": [1] * size},
    }
    model_path = tmp_path / "model.json"
    model_path.write_text(json.dumps(model))
    monkeypatch.setattr(cli, "MODEL_PATH", model_path)
    path = tmp_path / "cards.json"
    path.write_text(json.dumps(document()))
    result = cli.benchmark_document(path, minimum_count=4, confidence=0.95)
    section = result["sections"]["real_card_accuracy"]
    assert all(row["ml"] == "NORMAL" for row in section["predictions"])
    overall = section["methods"]["ml"]["overall"]
    assert overall["correct"] == overall["wrong"] == 2
    assert overall["accuracy"] == 0.5


def test_cli_rejects_synthetic_input_before_classification(tmp_path):
    cli = importlib.import_module(PACKAGE + ".__main__")
    d = document()
    d["cards"][0]["provenance"].update(kind="synthetic", acquisition="generated")
    path = tmp_path / "synthetic.json"
    path.write_text(json.dumps(d))
    with pytest.raises(ValueError, match="Synthetic or plate"):
        cli.benchmark_document(path, minimum_count=4, confidence=0.95)


def test_module_entrypoint_and_required_reporting_settings():
    completed = subprocess.run(
        [sys.executable, "-m", PACKAGE, "--minimum-count", "4", "--confidence", "0.95"],
        check=True, capture_output=True, text=True,
    )
    result = json.loads(completed.stdout)
    assert result["sections"]["real_card_accuracy"]["status"] == "Not Evaluated"
    missing = subprocess.run(
        [sys.executable, "-m", PACKAGE], capture_output=True, text=True,
    )
    assert missing.returncode != 0
    assert "required" in missing.stderr


def test_worked_example_is_explicitly_arithmetic_only():
    """The example's two diagonal confusion counts sum to two of four."""
    import runpy

    root = Path(__file__).resolve().parents[4]
    example = runpy.run_path(str(root / "examples/domains/artificial_lift/dynacard_benchmark_demo.py"))
    result = example["worked_example"]()
    assert result["evidence_kind"] == "invented arithmetic example; not field evidence"
    assert result["score"]["overall"]["correct"] == 2
    assert result["score"]["overall"]["accuracy"] == 0.5
    assert result["field_status"] == "Not Evaluated"


@pytest.mark.parametrize("prediction", ["ABSTAIN", "OUTSIDE_TAXONOMY"])
def test_overall_counts_unrecognised_predictions_as_wrong(prediction):
    score = score_predictions(
        ["NORMAL", "GAS_LOCK"], ["NORMAL", prediction], minimum_count=2,
    )
    assert score["overall"]["count"] == 2
    assert score["overall"]["correct"] == 1
    assert score["overall"]["wrong"] == 1
    assert score["overall"]["accuracy"] == 0.5
    assert score["confusion_matrix"]["GAS_LOCK"][prediction] == 1


def test_unmapped_labels_are_retained_but_excluded_from_scoring():
    data = document()
    data["cards"].append({**data["cards"][0], "label": "unmapped"})
    result = BenchmarkRunner(
        real_set=load_labelled_set(data), minimum_count=4,
        ml_classifier=lambda card: "NORMAL", heuristic_classifier=lambda card: "GAS_LOCK",
    ).run().to_dict()["sections"]["real_card_accuracy"]
    assert result["unmapped_labels"] == {"unmapped": 1}
    assert len(result["predictions"]) == 4
    for method in ("ml", "heuristic"):
        overall = result["methods"][method]["overall"]
        assert overall["count"] == 4
        assert overall["correct"] == overall["wrong"] == 2
        assert overall["accuracy"] == 0.5


def test_public_model_classifier_matches_cached_production_path():
    from digitalmodel.marine_ops.artificial_lift.dynacard import diagnostics

    card = load_labelled_set(document()).cards[0].card
    model = json.loads(diagnostics.MODEL_PATH.read_bytes())
    expected = diagnostics.PumpDiagnostics.classify_card(card)
    assert diagnostics.classify_with_model(card, model) == expected
    stale = {**model, "scaling": {"min": [0], "max": [1]}}
    with pytest.raises(ValueError, match="expects 1 features"):
        diagnostics.classify_with_model(card, stale)
