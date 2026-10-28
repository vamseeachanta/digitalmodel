# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Intake boundaries and independent counting examples, with no field data."""

import copy
import json
from datetime import date

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.intake import (
    prepare_intake,
    read_card_csv,
)


def record(well="private-well", card="private-card", **updates):
    """Illustrative inputs only; no engineering acceptance values are inferred."""
    result = {
        "well_id": well,
        "card_id": card,
        "kind": "real",
        "acquisition": "recorded",
        "card_space": "pump",
        "position": [0, 1, 1, 0],
        "load": [0, 0, 1, 1],
        "time_s": [0, 1, 2, 3],
        "relative_day": 0,
        "context": {
            "rod_string": [{"diameter_in": 1, "length_ft": 10}],
            "pump_diameter_in": 1,
            "pump_setting_depth_ft": 10,
            "stroke_in": 1,
            "spm": 1,
            "production_bpd": 0,
        },
        "label": {
            "mode": "NORMAL",
            "basis": "independent_inspection",
            "evidence_ref": "private-report",
            "reviewer_ref": "private-reviewer",
            "reviewed_on": "2026-10-19",
        },
        "rights": {
            "analysis": True,
            "publication": True,
            "evidence_ref": "private-grant",
        },
        "original_retained": True,
    }
    result.update(updates)
    return result


def batch(*records):
    return prepare_intake(records, alias_key=b"illustrative-test-key" * 2)


def test_projection_removes_identifiers_and_preserves_samples(tmp_path):
    private = record()
    private.update(
        {
            "filename": str(tmp_path / "private-source.csv"),
            "notes": "private-notes",
            "api14": "private-api",
        }
    )
    private["context"]["operator"] = "private-operator"
    before = copy.deepcopy(private)
    result = batch(private).publication_document()
    rendered = json.dumps(result)
    assert "private-" not in rendered
    assert str(tmp_path) not in rendered
    assert result["cards"][0]["position"] == private["position"]
    assert result["cards"][0]["load"] == private["load"]
    assert private == before
    assert batch(private).publication_document() == result
    assert (
        prepare_intake([private], alias_key=b"different-key" * 3).publication_document()
        != result
    )


@pytest.mark.parametrize("prefix", ["", "\ufeff"])
def test_csv_preserves_order_and_only_numeric_columns(prefix):
    data = read_card_csv(
        prefix
        + "time_s,position_in,load_lbf,well\n0,0,-1,private\n1,2,3,private\n2,0,-1,private\n"
    )
    assert data == {"time_s": [0, 1, 2], "position": [0, 2, 0], "load": [-1, 3, -1]}


@pytest.mark.parametrize(
    "csv",
    [
        "position_in,load_lbf\n0,1\n",  # time is required for this adapter
        "time_s,position_in,load_lbf\n0,0,1\n0,1,2\n1,0,1\n",
        "time_s,position_in,load_lbf\n0,0,1\n1,nan,2\n2,0,1\n",
        "time_s,position_in,load_lbf\n0,0,1\n1,1,2\n",
        "time_s,position_in,load_lbf,load_lbf\n0,0,1,2\n",
    ],
)
def test_invalid_csv_is_rejected_without_echoing_input(csv):
    with pytest.raises(ValueError) as error:
        read_card_csv(csv)
    assert "nan" not in str(error.value)


@pytest.mark.parametrize(
    "field,value",
    [
        ("position", [0, 1]),
        ("load", [0, float("inf"), 1, 0]),
        ("time_s", [0, 1, 1, 2]),
        ("relative_day", -1),
        ("card_space", "unknown"),
        ("kind", "unknown"),
        ("original_retained", "yes"),
    ],
)
def test_invalid_card_is_rejected(field, value):
    with pytest.raises(ValueError):
        batch(record(**{field: value}))


@pytest.mark.parametrize(
    "field,value",
    [
        ("spm", 0),
        ("stroke_in", True),
        ("production_bpd", -1),
        ("pump_diameter_in", "1"),
        ("rod_string", []),
        ("rod_string", [{"diameter_in": 0, "length_ft": 10}]),
    ],
)
def test_context_requires_explicit_finite_physical_inputs(field, value):
    r = record()
    r["context"][field] = value
    with pytest.raises(ValueError):
        batch(r)


def test_errors_do_not_echo_private_values():
    r = record()
    r["label"]["mode"] = "private-condition"
    with pytest.raises(ValueError) as error:
        batch(r)
    assert "private" not in str(error.value)


def test_empty_key_and_duplicate_cards_are_rejected():
    with pytest.raises(ValueError):
        prepare_intake([record()], alias_key=b"")
    with pytest.raises(ValueError):
        batch(record(), record())
    with pytest.raises(ValueError):
        batch(record(), record(well="other", card="other"))


@pytest.mark.parametrize("key", [b"", b"x", b"x" * 31, "x" * 32, None])
def test_alias_key_rejects_short_or_non_bytes_keys(key):
    with pytest.raises(ValueError, match="RFC 2104.*section 3"):
        prepare_intake([record()], alias_key=key)


@pytest.mark.parametrize("length", [32, 33, 64, 65])
def test_alias_key_accepts_digest_length_and_longer(length):
    assert prepare_intake([record()], alias_key=b"x" * length).summary()["cards"] == 1


def test_missing_evidence_and_permissions_block_scoring():
    for field, value in [
        ("original_retained", False),
        ("card_space", "surface"),
        ("kind", "synthetic"),
    ]:
        b = batch(record(**{field: value}))
        assert b.summary()["benchmark_status"] == "NOT_EVALUATED"
        with pytest.raises(ValueError):
            b.benchmark_set(b.card_ids())
    r = record()
    r["label"].update(
        mode=None, basis="unconfirmed", evidence_ref=None, reviewer_ref=None
    )
    assert batch(r).summary()["eligible_cards"] == 0
    r = record()
    r["rights"]["analysis"] = False
    b = batch(r)
    assert b.summary()["eligible_cards"] == 0
    r["rights"]["publication"] = False
    with pytest.raises(ValueError):
        batch(r).publication_document()


def test_confirmed_label_requires_report_reviewer_and_date():
    for field in ("evidence_ref", "reviewer_ref", "reviewed_on"):
        r = record()
        r["label"][field] = None
        with pytest.raises(ValueError):
            batch(r)
    r = record()
    r["rights"]["analysis"] = "yes"
    with pytest.raises(ValueError):
        batch(r)


@pytest.mark.parametrize(
    "reviewed_on,accepted",
    [
        ("2026-10-18", True),
        ("2026-10-19", True),
        ("2026-10-20", False),
        ("2099-01-01", False),
    ],
)
def test_review_date_cannot_follow_intake_date(monkeypatch, reviewed_on, accepted):
    from digitalmodel.marine_ops.artificial_lift.dynacard import intake

    class IntakeDate(date):
        @classmethod
        def today(cls):
            return cls(2026, 10, 19)

    monkeypatch.setattr(intake, "date", IntakeDate)
    r = record()
    r["label"]["reviewed_on"] = reviewed_on
    if accepted:
        assert batch(r).summary()["eligible_cards"] == 1
    else:
        with pytest.raises(ValueError, match="Invalid intake record"):
            batch(r)


def test_pairs_repeats_and_healthy_baseline_are_counted_by_well():
    """Hand count: two wells, three cards, one complete pair, one healthy well."""
    before = record(stage="before", event_id="private-event")
    before["label"]["mode"] = "FLUID_POUND"
    after = record(
        card="after",
        stage="after",
        event_id="private-event",
        relative_day=1,
        load=[0, 0, 2, 2],
    )
    other = record(well="other", card="other", load=[0, 0, 3, 3])
    other["label"]["mode"] = "FLUID_POUND"
    summary = batch(before, after, other).summary()
    assert summary["cards"] == 3
    assert summary["wells"] == 2
    assert summary["repeat_wells"] == 1
    assert summary["complete_pairs"] == 1
    assert summary["healthy_wells"] == 1
    assert summary["benchmark_status"] == "NOT_EVALUATED"


def test_pair_order_and_duplicate_stages_are_rejected():
    before = record(stage="before", event_id="event", relative_day=2)
    after = record(
        card="after", stage="after", event_id="event", relative_day=1, load=[0, 0, 2, 2]
    )
    with pytest.raises(ValueError):
        batch(before, after)
    after.update(stage="before", relative_day=3)
    with pytest.raises(ValueError):
        batch(before, after)


def test_benchmark_adapter_and_independent_expected_score():
    """Hand calculation: NORMAL prediction on NORMAL and FLUID_POUND = 1/2.

    At z=1, the NIST section 7.2.4.1 Wilson formula is checked by class:
    one success out of one yields [1/2, 1], one failure out of one yields
    [0, 1/2] at z=1.
    The sample threshold of one is an illustrative test input, not acceptance.
    """
    from statistics import NormalDist
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark import (
        BenchmarkRunner,
    )

    a = record()
    b = record(well="other", card="other", load=[0, 0, 2, 2])
    b["label"]["mode"] = "FLUID_POUND"
    prepared = batch(a, b)
    ids = [c["card_id"] for c in prepared.publication_document()["cards"]]
    dataset = prepared.benchmark_set(ids)
    result = BenchmarkRunner(
        real_set=dataset,
        minimum_count=1,
        confidence=2 * NormalDist().cdf(1) - 1,
        ml_classifier=lambda _: "NORMAL",
        heuristic_classifier=lambda _: "NORMAL",
    ).run()
    score = result.sections["real_card_accuracy"]["methods"]["ml"]["per_class"]
    assert score["NORMAL"]["interval"] == pytest.approx([0.5, 1])
    assert score["FLUID_POUND"]["interval"] == pytest.approx([0, 0.5])
    assert sum(row["correct"] for row in score.values()) == 1
    assert sum(row["count"] for row in score.values()) == 2
    assert "private" not in json.dumps(result.to_dict())


def test_benchmark_selection_rejects_repeats_empty_and_unknown_ids():
    prepared = batch(record(), record(card="repeat", load=[0, 0, 2, 2]))
    ids = [c["card_id"] for c in prepared.publication_document()["cards"]]
    for selection in ([], ["unknown"], ids, [ids[0], ids[0]]):
        with pytest.raises(ValueError):
            prepared.benchmark_set(selection)


def test_analysis_permission_is_separate_from_publication():
    r = record()
    r["rights"]["publication"] = False
    prepared = batch(r)
    assert len(prepared.benchmark_set(prepared.card_ids()).cards) == 1
    with pytest.raises(ValueError):
        prepared.publication_document()
    r["original_retained"] = False
    with pytest.raises(ValueError):
        batch(r).publication_document()


def test_real_acquisition_cannot_be_generated_or_implicitly_inferred():
    r = record(acquisition="generated")
    with pytest.raises(ValueError):
        batch(r)
    del r["acquisition"]
    with pytest.raises(ValueError):
        batch(r)


def test_copies_do_not_change_eligibility_and_signed_zero_is_a_duplicate():
    prepared = batch(record())
    document = prepared.publication_document()
    document["cards"][0]["rights"]["analysis"] = False
    assert len(prepared.benchmark_set(prepared.card_ids()).cards) == 1
    r = record(card="copy", position=[-0.0, 1, 1, 0])
    with pytest.raises(ValueError):
        batch(record(), r)


def test_empty_collection_is_not_an_accuracy_result():
    prepared = batch()
    assert prepared.summary()["cards"] == 0
    assert prepared.summary()["benchmark_status"] == "NOT_EVALUATED"


def test_incomplete_pairs_and_non_real_cards_are_not_field_pairs():
    before = record(stage="before", event_id="event")
    assert batch(before).summary()["complete_pairs"] == 0
    after = record(
        card="after",
        stage="after",
        event_id="event",
        relative_day=1,
        kind="synthetic",
        acquisition="generated",
        load=[0, 0, 2, 2],
    )
    summary = batch(before, after).summary()
    assert summary["complete_pairs"] == 0
    assert summary["non_real_cards"] == 1


def test_worked_example_has_no_field_accuracy_claim():
    """Independent count: all three inputs are synthetic; no real well exists."""
    import runpy
    from pathlib import Path

    root = Path(__file__).resolve().parents[4]
    example = root / "examples/domains/artificial_lift/labelled_card_intake.py"
    result = runpy.run_path(str(example))["run_example"]()
    assert result["projected_cards"] == 3
    assert result["summary"]["non_real_cards"] == 3
    assert result["summary"]["cards"] == 0
    assert result["summary"]["eligible_wells"] == 0
    assert result["summary"]["benchmark_status"] == "NOT_EVALUATED"
    assert result["synthetic_benchmark_rejected"] is True
