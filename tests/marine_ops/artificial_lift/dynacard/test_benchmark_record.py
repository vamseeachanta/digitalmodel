# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Small-fixture checks of benchmark paths, counts and evidence boundaries."""

import importlib
import json
from pathlib import Path

import pytest

PACKAGE = "digitalmodel.marine_ops.artificial_lift.dynacard.benchmark"


def api():
    return importlib.import_module(PACKAGE)


def card():
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
    return CardData(position=[0, 1, 1, 0], load=[0, 0, 1, 1])


def provenance(kind="real"):
    return api().LabellingProvenance(
        source="in-memory fixture", labeller="fixture author",
        date="2026-10-14", licence="CC0-1.0", kind=kind,
    )


def test_required_provenance_and_validation():
    a = api()
    with pytest.raises(TypeError):
        a.LabelledCard(card=card(), label="NORMAL")
    with pytest.raises(ValueError):
        a.LabellingProvenance(source="", labeller="fixture author",
                             date="2026-10-14", licence="CC0-1.0", kind="real")
    with pytest.raises(ValueError):
        a.LabelledCard(card=card(), label="unknown", provenance=provenance())


def test_builder_is_balanced_deterministic_and_synthetic():
    a = api()
    x = a.build_hold_out_test_set(samples_per_mode=1)
    y = a.build_hold_out_test_set(samples_per_mode=1)
    assert len(x) == 20
    assert [c.card.model_dump() for c in x] == [c.card.model_dump() for c in y]
    assert all(c.provenance.kind == "synthetic" for c in x)
    with pytest.raises(ValueError):
        a.build_hold_out_test_set(samples_per_mode=0)


def test_wilson_against_hand_calculation():
    # NIST score formula with z=2, n=4, p=1/2:
    # centre=(1/2+4/8)/(1+4/4)=1/2;
    # radius=2*sqrt(1/16+1/16)/2=sqrt(1/8).
    low, high = api().wilson_interval(2, 4, z=2)
    assert low == pytest.approx(0.1464466094067262)
    assert high == pytest.approx(0.8535533905932737)
    assert api().wilson_interval(0, 4, z=2) == pytest.approx((0, 0.5))
    assert api().wilson_interval(4, 4, z=2) == pytest.approx((0.5, 1))


@pytest.mark.parametrize("correct,total,z", [(0, 0, 2), (-1, 4, 2), (5, 4, 2), (2, 4, 0)])
def test_wilson_rejects_invalid_counts(correct, total, z):
    with pytest.raises(ValueError):
        api().wilson_interval(correct, total, z=z)


def test_too_thin_omits_percentages_and_scores_at_threshold():
    a = api()
    section = a.score_predictions(["NORMAL"] * 4, ["NORMAL"] * 2 + ["GAS_LOCK"] * 2,
                                  minimum_count=4, confidence=0.95)
    normal = section["per_class"]["NORMAL"]
    assert normal["count"] == 4 and normal["correct"] == 2
    assert normal["accuracy"] == 0.5
    assert normal["interval"] == pytest.approx([0.15003898915214947, 0.8499610108478506])
    thin = section["per_class"]["GAS_LOCK"]
    assert thin["status"] == "too thin to score"
    assert "accuracy" not in thin and "interval" not in thin
    below = a.score_predictions(["NORMAL"] * 3, ["NORMAL"] * 3, minimum_count=4)
    assert "accuracy" not in below["per_class"]["NORMAL"]
    assert section["confusion_matrix"]["NORMAL"]["GAS_LOCK"] == 2


def fixture_document():
    return {
        "card_space": "pump", "units": {"position": "in", "load": "lbf"},
        "crosswalk": [{"external_label": "healthy", "mode": "NORMAL", "reason": "fixture equivalence"}],
        "cards": [{"position": [0, 1, 1, 0], "load": [0, 0, 1, 1], "label": label,
                   "provenance": {"source": "in-memory fixture", "labeller": "fixture author",
                                  "date": "2026-10-14", "licence": "CC0-1.0", "kind": "real"}}
                  for label in ["healthy", "ambiguous valve"]],
    }


def test_loader_crosswalk_preserves_unmapped_labels_and_provenance():
    loaded = api().load_labelled_set(fixture_document())
    assert len(loaded.cards) == 1
    assert loaded.cards[0].label == "NORMAL"
    assert loaded.cards[0].external_label == "healthy"
    assert loaded.cards[0].provenance.source == "in-memory fixture"
    assert loaded.unmapped_labels == {"ambiguous valve": 1}
    assert loaded.crosswalk[0]["mode"] == "NORMAL"


@pytest.mark.parametrize("defect", ["units", "space", "provenance", "length", "nonfinite", "mode", "duplicate"])
def test_loader_rejects_invalid_evidence(defect):
    d = fixture_document()
    if defect == "units": d["units"]["position"] = "m"
    if defect == "space": d["card_space"] = "surface"
    if defect == "provenance": del d["cards"][0]["provenance"]["licence"]
    if defect == "length": d["cards"][0]["load"] = [0]
    if defect == "nonfinite": d["cards"][0]["load"][0] = float("nan")
    if defect == "mode": d["crosswalk"][0]["mode"] = "unknown"
    if defect == "duplicate": d["crosswalk"].append(d["crosswalk"][0].copy())
    with pytest.raises((ValueError, TypeError)):
        api().load_labelled_set(d)


def test_record_has_four_paths_and_no_real_score_without_data():
    result = api().BenchmarkRunner().run().to_dict()
    assert set(result["sections"]) == {"synthetic_cross_validation", "solver_round_trip", "plate_distance", "real_card_accuracy"}
    assert all(s["path"] for s in result["sections"].values())
    assert result["sections"]["real_card_accuracy"]["status"] == "Not Evaluated"
    assert json.loads(json.dumps(result, allow_nan=False)) == result


def test_real_paths_are_independent_and_synthetic_cards_are_rejected():
    a = api()
    dataset = a.load_labelled_set(fixture_document())
    result = a.BenchmarkRunner(real_set=dataset, minimum_count=1,
                               ml_classifier=lambda c: "NORMAL",
                               heuristic_classifier=lambda c: "GAS_LOCK").run().to_dict()
    section = result["sections"]["real_card_accuracy"]
    assert section["methods"]["ml"]["per_class"]["NORMAL"]["correct"] == 1
    assert section["methods"]["heuristic"]["per_class"]["NORMAL"]["correct"] == 0
    assert section["unmapped_labels"] == {"ambiguous valve": 1}
    dataset.cards[0].provenance = provenance("synthetic")
    with pytest.raises(ValueError):
        a.BenchmarkRunner(real_set=dataset).run()


def test_cross_validation_fold_scoring_and_leakage_guard():
    a = api()
    folds = [{"train_ids": ["b"], "test_ids": ["a"], "labels": ["NORMAL"], "predictions": ["GAS_LOCK"]},
             {"train_ids": ["a"], "test_ids": ["b"], "labels": ["GAS_LOCK"], "predictions": ["GAS_LOCK"]}]
    result = a.BenchmarkRunner(cv_folds=folds, minimum_count=1).run().to_dict()["sections"]["synthetic_cross_validation"]
    assert result["fold_count"] == 2
    assert result["confusion_matrix"]["NORMAL"]["GAS_LOCK"] == 1
    folds[0]["train_ids"] = ["a"]
    with pytest.raises(ValueError):
        a.BenchmarkRunner(cv_folds=folds).run()


def context():
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
        DynacardAnalysisContext, PumpProperties, RodSection, SurfaceUnit,
    )
    return DynacardAnalysisContext(api14="SIM-ROUNDTRIP", surface_card=card(),
                                  rod_string=[RodSection(diameter=1, length=5000)],
                                  pump=PumpProperties(diameter=1.75, depth=5000),
                                  surface_unit=SurfaceUnit(stroke_length=192), spm=6)


def test_round_trip_uses_forward_model_and_default_solver(monkeypatch):
    a = api()
    module = importlib.import_module(PACKAGE + ".measurements")
    calls = []
    surface = card().model_copy(update={"load": [100, 101, 102, 100]})
    def forward(pump, ctx):
        calls.append("forward")
        return surface
    class Workflow:
        solver_method = "default-fixture"
        def __init__(self, ctx):
            assert ctx.surface_card == surface
            self.solver = self
        def solve(self):
            calls.append("inverse")
            from types import SimpleNamespace
            return SimpleNamespace(downhole_card=card())
    monkeypatch.setattr(module, "surface_card_from_pump_card", forward)
    monkeypatch.setattr(module, "DynacardWorkflow", Workflow)
    cards = [a.LabelledCard(card=card(), label="NORMAL", provenance=provenance("synthetic"))]
    result = a.BenchmarkRunner(round_trip_cards=cards, round_trip_context=context(),
                               ml_classifier=lambda c: calls.append("classify") or "GAS_LOCK",
                               minimum_count=1).run().to_dict()["sections"]["solver_round_trip"]
    assert calls == ["forward", "inverse", "classify"]
    assert result["confusion_matrix"]["NORMAL"]["GAS_LOCK"] == 1
    assert result["solver_method"] == "default-fixture"


def test_actual_round_trip_smoke():
    a = api()
    cards = a.build_hold_out_test_set(samples_per_mode=1)[:1]
    result = a.BenchmarkRunner(round_trip_cards=cards, round_trip_context=context(),
                               ml_classifier=lambda c: "NORMAL", minimum_count=1).run().to_dict()
    section = result["sections"]["solver_round_trip"]
    assert section["solver_method"] == "everitt_jennings"
    assert section["per_class"]["NORMAL"]["count"] == 1


def test_plate_distance_identity_and_missing_exemplars():
    a = api()
    fixture = Path(__file__).parent / "testdata/plate_reference_cards.json"
    plate = json.loads(fixture.read_text())["cards"]
    from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
    normal = plate["normal"]
    cards = [a.LabelledCard(card=CardData(position=normal["position"], load=normal["load"]),
                            label="NORMAL", provenance=provenance("synthetic"))]
    result = a.BenchmarkRunner(plate_cards=cards, plate_exemplars=plate).run().to_dict()["sections"]["plate_distance"]
    assert result["per_class"]["NORMAL"]["distances"] == pytest.approx([0])
    assert result["per_class"]["GAS_LOCK"]["status"] == "No exemplar"
    assert result["per_class"]["FLUID_POUND"]["status"] == "Not Evaluated"


def test_missing_crosswalk_mode_is_not_an_explicit_unmapped_row():
    d = fixture_document()
    del d["crosswalk"][0]["mode"]
    with pytest.raises(ValueError):
        api().load_labelled_set(d)


def test_empty_record_still_validates_scoring_settings():
    with pytest.raises(ValueError):
        api().BenchmarkRunner(minimum_count=0).run()


def test_legacy_serialization_suppresses_thin_class_percentages():
    a = api()
    result = a.BenchmarkRunner(test_set=a.build_hold_out_test_set(samples_per_mode=1)).run().to_dict()
    assert result["heuristic"]["per_mode_statistics"]["NORMAL"]["status"] == "too thin to score"
    assert result["heuristic"]["per_mode_accuracy"] == {}


def test_convenience_function_accepts_section_inputs():
    result = api().run_benchmark(cv_folds=None).to_dict()
    assert result["sections"]["real_card_accuracy"]["status"] == "Not Evaluated"
