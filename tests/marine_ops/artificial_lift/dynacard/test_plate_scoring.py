"""Classifier consistency on the CC0 plate; no field accuracy is established."""

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import (
    DiagnosticPlateCurve,
)

DATA = Path(__file__).parent / "testdata"
WORKBOOK = DATA / "mendeley_plate" / "Working condition diagnosis plate.xls"
SOURCE_DIR = str(Path(__file__).resolve().parents[4] / "src")


def rectangle():
    """Counterclockwise input, nonunit axes, open loop and a repeated vertex."""
    return DiagnosticPlateCurve(
        label="工况正常", block_index=0, columns=(0, 1), ordinate="load",
        fault_mode="NORMAL", position=(4, 4, 2, 2, 2), values=(3, 7, 7, 7, 3),
    )


def test_prepare_rectangle_closes_resamples_orients_and_scales():
    """Unit square perimeter 4, eight samples at arc increments 1/2.

    Clockwise order from the lower-left vertex is (0,0), (0,1/2),
    (0,1), (1/2,1), (1,1), (1,1/2), (1,0), (1/2,0).
    The implicit closing segment has length 1/2, with no duplicated endpoint.
    """
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import (
        prepare_plate_card,
    )
    card = prepare_plate_card(
        rectangle(), samples=8, stroke_in=20, load_range_lbf=400,
        minimum_load_lbf=100,
    )
    assert card.position == pytest.approx([0, 0, 0, 10, 20, 20, 20, 10])
    assert card.load == pytest.approx([100, 300, 500, 500, 500, 300, 100, 100])
    x, y = np.array(card.position), np.array(card.load)
    assert np.sum(x * np.roll(y, -1) - np.roll(x, -1) * y) < 0


def test_score_separates_literal_and_assumed_and_keeps_unexpected_predictions():
    """8/10 Wilson: z=1.959963984540054, d=1+z²/10=1.3841458821.

    Centre=(0.8+z²/20)/d=0.7167401607; radius=0.2265776894.
    Limits are 0.4901624715 and 0.9433178485 (rounded here only).
    The eight literal rows are all correct; both ASSUMED rows abstain.
    """
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import (
        prepare_plate_card, score_plate,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import (
        load_diagnostic_plate,
    )
    curves = [c for c in load_diagnostic_plate(WORKBOOK) if c.ordinate == "load"]
    cards = [(c, prepare_plate_card(c, stroke_in=100, load_range_lbf=6000))
             for c in curves]
    predictions = iter(c.fault_mode or "ABSTAIN" for c in curves)
    score = score_plate(lambda _: next(predictions), cards)
    assert score["literal"]["correct"] == score["literal"]["count"] == 8
    assert score["assumed"]["correct"] == 0
    assert score["assumed"]["count"] == 2
    assert score["assumed"]["crosswalk_status"] == "ASSUMED"
    assert score["including_assumed"]["count"] == 10
    assert score["including_assumed"]["correct"] == 8
    assert score["including_assumed"]["accuracy"] == 0.8
    assert score["including_assumed"]["interval"] == pytest.approx(
        [0.4901624715, 0.9433178485], abs=1e-10,
    )
    matrix = score["including_assumed"]["confusion_matrix"]
    assert matrix["FLUID_POUND"]["ABSTAIN"] == 1
    assert matrix["SAND_ABRASION"]["ABSTAIN"] == 1
    for row, curve in zip(score["rows"], curves, strict=True):
        assert row["source_label"] == curve.label
        assert row["english_gloss"]
        assert row["crosswalk_kind"] == ("literal" if curve.fault_mode else "assumed")
        assert row["crosswalk_status"] == ("LITERAL" if curve.fault_mode else "ASSUMED")
    assert sum(map(sum, (r.values() for r in matrix.values()))) == 10


def test_shape_vector_has_only_sixteen_normalised_projections():
    from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
        generate_normal_card,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.feature_extraction import (
        FeatureExtractor,
    )
    card = generate_normal_card(seed=23)
    vector = FeatureExtractor.extract_shape_vector(card)
    assert vector.shape == (16,)
    np.testing.assert_array_equal(vector, FeatureExtractor.extract_bezerra_projections(card))
    assert FeatureExtractor.shape_feature_names() == [f"bezerra_{i}" for i in range(16)]
    assert FeatureExtractor.extract_classifier_vector(card).shape == (19,)


@pytest.mark.parametrize("suffix", [".json", ".json.gz"])
def test_shape_trainer_exports_current_labels_and_reproducible_metadata(tmp_path, suffix):
    import json
    import sklearn
    from digitalmodel.marine_ops.artificial_lift.dynacard import training
    from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
        ALL_GENERATORS, SYNTHETIC_LABEL_EXCLUSIONS,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.feature_extraction import (
        FeatureExtractor,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import read_model_bytes
    artifact = tmp_path / f"shape{suffix}"
    metrics = training.train_and_export_shape(samples_per_mode=5, output_path=artifact)
    model = json.loads(read_model_bytes(artifact))
    assert model["class_labels"] == sorted(set(ALL_GENERATORS) - set(SYNTHETIC_LABEL_EXCLUSIONS))
    metadata = model["training_metadata"]
    assert metadata["feature_names"] == FeatureExtractor.shape_feature_names()
    assert metadata["samples_per_mode"] == 5
    assert metadata["excluded_labels"] == SYNTHETIC_LABEL_EXCLUSIONS
    assert metadata["sklearn_version"] == sklearn.__version__
    assert metadata["cv_folds"] == 5
    assert metadata["cv_accuracy"] == metrics["cv_accuracy"]
    assert 0 <= metrics["cv_accuracy"] <= 1
    assert model["scaling"] == {"min": [0.0] * 16, "max": [1.0] * 16}
    assert metrics["n_samples"] == 5 * len(model["class_labels"])
    assert metrics["n_features"] == 16


def test_shape_inference_uses_exported_trees_without_magnitude(tmp_path, monkeypatch):
    import json
    from sklearn.ensemble import GradientBoostingClassifier
    from digitalmodel.marine_ops.artificial_lift.dynacard import diagnostics, training
    from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
        ALL_GENERATORS,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.feature_extraction import (
        FeatureExtractor,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import (
        load_diagnostic_plate,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import (
        prepare_plate_card,
    )
    # Capture the actual fitted estimator through its public fit/predict API.
    fitted = []
    original_fit = GradientBoostingClassifier.fit

    def retain_fit(self, *args, **kwargs):
        result = original_fit(self, *args, **kwargs)
        fitted.append(self)
        return result

    monkeypatch.setattr(GradientBoostingClassifier, "fit", retain_fit)
    artifact = tmp_path / "shape.json"
    training.train_and_export_shape(samples_per_mode=5, output_path=artifact)
    monkeypatch.setattr(diagnostics, "SHAPE_MODEL_PATH", artifact)
    assert len(json.loads(artifact.read_text())["scaling"]["min"]) == 16
    for generator in ALL_GENERATORS.values():
        card = generator(seed=105)
        features = [FeatureExtractor.extract_shape_vector(card)]
        result = diagnostics.classify_shape(card)
        assert result.classification == fitted[-1].predict(features)[0]
        assert result.confidence == pytest.approx(fitted[-1].predict_proba(features).max(), abs=5.1e-5)
        assert len(result.differential) == 3
    for curve in load_diagnostic_plate(WORKBOOK):
        if curve.ordinate != "load":
            continue
        small = prepare_plate_card(curve, stroke_in=60, load_range_lbf=4000)
        large = prepare_plate_card(curve, stroke_in=140, load_range_lbf=8000,
                                   minimum_load_lbf=1234)
        np.testing.assert_allclose(FeatureExtractor.extract_shape_vector(small),
                                   FeatureExtractor.extract_shape_vector(large), atol=1e-14)
        assert diagnostics.classify_shape(small).classification == diagnostics.classify_shape(large).classification


def test_shipped_model_sweep_scores_four_assumed_scales():
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import (
        score_shipped_model_scale_sweep,
    )
    sweep = score_shipped_model_scale_sweep(WORKBOOK)
    assert [(s["stroke_in"], s["load_range_lbf"]) for s in sweep["scales"]] == [
        (60, 4000), (80, 12000), (100, 6000), (140, 8000),
    ]
    counts = [s["including_assumed"]["correct"] for s in sweep["scales"]]
    assert sweep["min_correct"] == min(counts)
    assert sweep["max_correct"] == max(counts)
    assert sweep["min_correct"] < sweep["max_correct"]
    for score in sweep["scales"]:
        assert score["scale_status"] == "ASSUMED"
        assert score["literal"]["count"] == 8
        assert score["including_assumed"]["count"] == 10


def test_recorded_score_matches_fresh_document_and_cli():
    import hashlib
    import json
    import os
    import subprocess
    import sys
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import (
        plate_score_document, score_shipped_model_scale_sweep,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
        MODEL_PATH, SHAPE_MODEL_PATH, read_model_bytes,
    )
    recorded = json.loads((DATA / "plate_classifier_score.json").read_text(encoding="utf-8"))
    fresh = plate_score_document(WORKBOOK)
    assert fresh == recorded
    assert fresh["source"]["sha256"] == hashlib.sha256(WORKBOOK.read_bytes()).hexdigest()
    for name, path in (("shape_only", SHAPE_MODEL_PATH), ("shipped", MODEL_PATH)):
        assert fresh["models"][name]["sha256"] == hashlib.sha256(read_model_bytes(path)).hexdigest()
    assert score_shipped_model_scale_sweep(WORKBOOK) == recorded["shipped_model_scale_sweep"]
    command = [sys.executable, "-m",
               "digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring",
               "--workbook", str(WORKBOOK)]
    output = subprocess.run(command, check=True, capture_output=True, text=True,
                            encoding="utf-8",
                            env={**os.environ, "PYTHONPATH": SOURCE_DIR})
    assert json.loads(output.stdout) == recorded
    assert output.stdout == (DATA / "plate_classifier_score.json").read_text(encoding="utf-8")


@pytest.mark.parametrize("change", [
    {"ordinate": "power"}, {"position": (2, 2, 2, 2, 2)},
    {"position": (4, float("nan"), 2, 2, 2)}, {"values": (0, 1)},
])
def test_prepare_rejects_nonload_or_invalid_geometry(change):
    from dataclasses import replace
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import (
        prepare_plate_card,
    )
    with pytest.raises(ValueError):
        prepare_plate_card(replace(rectangle(), **change), stroke_in=100, load_range_lbf=6000)


@pytest.mark.parametrize("kwargs", [
    {"samples": 3}, {"samples": True}, {"stroke_in": 0},
    {"stroke_in": float("inf")}, {"load_range_lbf": -1},
    {"minimum_load_lbf": float("nan")},
])
def test_prepare_rejects_invalid_assumed_scales_or_sample_count(kwargs):
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import (
        prepare_plate_card,
    )
    options = {"stroke_in": 100, "load_range_lbf": 6000, **kwargs}
    with pytest.raises(ValueError):
        prepare_plate_card(rectangle(), **options)


def test_score_empty_is_not_a_numerical_accuracy():
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import score_plate
    score = score_plate(lambda _: "NORMAL", [])
    assert score["rows"] == []
    for group in ("literal", "assumed", "including_assumed"):
        assert score[group]["count"] == 0
        assert score[group]["accuracy"] is None
        assert score[group]["interval"] is None


def test_shipped_artifact_and_default_classification_remain_pinned():
    import hashlib
    import json
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import prepare_plate_card
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import load_diagnostic_plate
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import MODEL_PATH, PumpDiagnostics
    assert hashlib.sha256(MODEL_PATH.read_bytes()).hexdigest() == (
        "c3612e1f31aad740b4ebc5f81e34bf09ae7d21e42f1f18915c2a76c94337a40c"
    )
    recorded = json.loads((DATA / "plate_classifier_score.json").read_text(encoding="utf-8"))
    expected = recorded["shipped_model_scale_sweep"]["scales"][2]["rows"]
    curves = [c for c in load_diagnostic_plate(WORKBOOK) if c.ordinate == "load"]
    PumpDiagnostics.reset_model_cache()
    for curve, row in zip(curves, expected, strict=True):
        card = prepare_plate_card(curve, stroke_in=100, load_range_lbf=6000)
        assert PumpDiagnostics.classify_card(card) == row["predicted_mode"]
    PumpDiagnostics.reset_model_cache()


def test_shape_inference_does_not_import_sklearn():
    import os
    import subprocess
    import sys
    script = '''
import importlib.abc
import sys
class NoSklearn(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname == "sklearn" or fullname.startswith("sklearn."):
            raise AssertionError("scikit-learn is unavailable at inference")
sys.meta_path.insert(0, NoSklearn())
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import classify_shape
from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
result = classify_shape(CardData(position=[0, 0, 1, 1], load=[0, 1, 1, 0]))
assert result.classification
'''
    subprocess.run([sys.executable, "-c", script], check=True, capture_output=True,
                   env={**os.environ, "PYTHONPATH": SOURCE_DIR})


@pytest.mark.parametrize("position,load", [
    ([0, 1, 0], [0, 1, 0]),
    ([0, 0, 0, 0], [0, 1, 1, 0]),
    ([0, 1, 1, 0], [2, 2, 2, 2]),
    ([0, 1, float("nan"), 0], [0, 1, 1, 0]),
    ([0, 1, 1, 0], [0, 1, 0]),
])
def test_shape_inference_rejects_cards_without_a_finite_shape(position, load):
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import classify_shape
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
    with pytest.raises(ValueError, match="shape"):
        classify_shape(CardData(position=position, load=load))


def test_cli_emits_utf8_under_an_ascii_default_encoding():
    import os
    import subprocess
    import sys
    command = [sys.executable, "-m",
               "digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring",
               "--workbook", str(WORKBOOK)]
    result = subprocess.run(command, capture_output=True,
                            env={**os.environ, "PYTHONPATH": SOURCE_DIR, "PYTHONIOENCODING": "ascii"})
    assert result.returncode == 0, result.stderr.decode("utf-8", errors="replace")
    # Text-mode standard output uses the platform line ending.
    emitted = result.stdout.decode("utf-8").replace("\r\n", "\n")
    assert emitted == (DATA / "plate_classifier_score.json").read_text(encoding="utf-8")


def test_all_wrong_plate_score_has_a_true_zero_lower_bound():
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import (
        prepare_plate_card, score_plate,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import load_diagnostic_plate
    cards = [(c, prepare_plate_card(c, stroke_in=100, load_range_lbf=6000))
             for c in load_diagnostic_plate(WORKBOOK) if c.ordinate == "load"]
    score = score_plate(lambda _: "ABSTAIN", cards)
    assert score["including_assumed"]["interval"][0] == 0.0


def test_shape_dimension_mismatch_names_the_shape_artefact(tmp_path, monkeypatch):
    import json
    from digitalmodel.marine_ops.artificial_lift.dynacard import diagnostics
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
    artifact = tmp_path / "wrong_shape.json"
    artifact.write_text(json.dumps({"scaling": {"min": [0] * 19}}), encoding="utf-8")
    monkeypatch.setattr(diagnostics, "SHAPE_MODEL_PATH", artifact)
    with pytest.raises(ValueError, match="train_and_export_shape"):
        diagnostics.classify_shape(CardData(position=[0, 0, 1, 1], load=[0, 1, 1, 0]))


def test_model_artefacts_are_declared_as_package_data():
    """A wheel must carry both classifier artefacts; classify_shape has no fallback."""
    import fnmatch
    import tomllib
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
        MODEL_PATH, SHAPE_MODEL_PATH,
    )
    root = Path(__file__).resolve().parents[4]
    patterns = tomllib.loads((root / "pyproject.toml").read_text(encoding="utf-8"))[
        "tool"]["setuptools"]["package-data"]["digitalmodel"]
    package_root = root / "src" / "digitalmodel"
    for artefact in (MODEL_PATH, SHAPE_MODEL_PATH):
        relative = artefact.resolve().relative_to(package_root).as_posix()
        assert any(fnmatch.fnmatch(relative, pattern) for pattern in patterns), relative


def test_shipped_shape_model_is_lossless_and_below_large_file_limit():
    import gzip
    import hashlib
    import json
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
        SHAPE_MODEL_PATH, read_model_bytes,
    )
    assert SHAPE_MODEL_PATH.suffixes[-2:] == [".json", ".gz"]
    stored = SHAPE_MODEL_PATH.read_bytes()
    assert len(stored) <= 1000 * 1024
    assert stored[4:8] == bytes(4)  # No build timestamp in the gzip header.
    raw = read_model_bytes(SHAPE_MODEL_PATH)
    assert raw == gzip.decompress(stored)
    recorded = json.loads((DATA / "plate_classifier_score.json").read_text(encoding="utf-8"))
    assert hashlib.sha256(raw).hexdigest() == recorded["models"]["shape_only"]["sha256"]


def test_model_reader_supports_plain_json_and_rejects_corrupt_gzip(tmp_path):
    import gzip
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import read_model_bytes
    path = tmp_path / "model.json"
    path.write_bytes(b'{"trees": []}\n')
    assert read_model_bytes(path) == path.read_bytes()
    compressed = tmp_path / "model.json.gz"
    compressed.write_bytes(gzip.compress(path.read_bytes(), mtime=0))
    assert read_model_bytes(compressed) == path.read_bytes()
    compressed.write_bytes(b"invalid")
    with pytest.raises(gzip.BadGzipFile):
        read_model_bytes(compressed)
