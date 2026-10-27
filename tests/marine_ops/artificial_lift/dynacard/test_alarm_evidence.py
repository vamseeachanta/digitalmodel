# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline evidence checks using invented traces and independent selection.

Fixture basis: four cards labelled a, b, c, d at successive minute marks on an
arbitrary date. The event is at c. Hand selection gives a,b,c before or at the
event, hence b,c for retention two and c for retention one. Trace coordinates
and metadata values are synthetic sentinels, not engineering limits.
"""

import json
from dataclasses import FrozenInstanceError

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.alarm_evidence import (
    AlarmEvidence,
    capture_alarm_evidence,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData


EVENT = "2026-01-01T00:02:00Z"
REASON = "Manual review requested"


def cards():
    """Return the explicitly ordered, synthetic minute-mark sequence."""
    return [
        (identifier, CardData(
            position=[0, 1, 0], load=[1, 2, 1],
            timestamp=f"2026-01-01T00:0{minute}:00Z",
        ))
        for minute, identifier in enumerate(("a", "b", "c", "d"))
    ]


def capture(rows=None, **changes):
    """Apply explicit fixture event metadata without helper defaults."""
    options = dict(retention_count=2, event_timestamp=EVENT, reason=REASON)
    options.update(changes)
    return capture_alarm_evidence(cards() if rows is None else rows, **options)


@pytest.mark.parametrize("retention,expected", [
    (1, ("c",)), (2, ("b", "c")), (3, ("a", "b", "c")),
    (4, ("a", "b", "c")),
])
def test_alarm_capture_retains_only_pre_event_cards(retention, expected):
    evidence = capture(retention_count=retention)
    assert tuple(card.card_id for card in evidence.cards) == expected
    assert evidence.retention_count == retention
    assert evidence.event_timestamp == EVENT
    assert evidence.reason == REASON


def test_alarm_capture_is_independent_of_later_mutation():
    rows = cards()
    metadata = {"review": {"tags": ["inspect"], "counts": [1, 2]}}
    evidence = capture(rows, metadata=metadata)
    before = evidence.to_json()
    # b is retained; mutate both arrays, its timestamp, and nested metadata.
    rows[1][1].position[0] = 9
    rows[1][1].load.append(9)
    rows[1][1].timestamp = "changed"
    rows.clear()
    metadata["review"]["tags"].append("changed")
    metadata["review"]["counts"][0] = 9
    assert evidence.to_json() == before
    assert evidence.cards[0].position == (0, 1, 0)
    assert evidence.cards[0].load == (1, 2, 1)
    assert evidence.metadata["review"]["tags"] == ("inspect",)
    assert evidence.metadata["review"]["counts"] == (1, 2)


def test_alarm_capture_round_trip_preserves_event_evidence():
    metadata = {"review": {"tags": ["inspect"], "confirmed": False, "note": None}}
    evidence = capture(metadata=metadata)
    expected = {
        "event_timestamp": EVENT, "reason": REASON, "retention_count": 2,
        "cards": [
            {"card_id": "b", "timestamp": "2026-01-01T00:01:00Z",
             "position": [0, 1, 0], "load": [1, 2, 1]},
            {"card_id": "c", "timestamp": EVENT,
             "position": [0, 1, 0], "load": [1, 2, 1]},
        ],
        "metadata": metadata,
    }
    assert json.loads(evidence.to_json()) == expected
    restored = AlarmEvidence.from_json(evidence.to_json())
    assert restored == evidence
    assert restored.to_dict() == expected
    restored.to_dict()["metadata"]["review"]["tags"].clear()
    assert restored.to_dict() == expected


def test_snapshot_is_deeply_immutable_after_capture_and_restore():
    evidence = capture(metadata={"review": {"tags": ["inspect"]}})
    for result in (evidence, AlarmEvidence.from_json(evidence.to_json())):
        with pytest.raises(FrozenInstanceError):
            result.reason = "changed"
        with pytest.raises(FrozenInstanceError):
            result.cards[0].timestamp = "changed"
        with pytest.raises(TypeError):
            result.cards[0].load[0] = 9
        with pytest.raises(TypeError):
            result.metadata["review"]["tags"][0] = "changed"
        with pytest.raises(TypeError):
            result.metadata["review"]["added"] = True


@pytest.mark.parametrize("retention", [0, -1, True, False, 1.5, "2", None])
def test_invalid_retention_is_rejected(retention):
    with pytest.raises(ValueError, match="retention_count"):
        capture(retention_count=retention)


@pytest.mark.parametrize("timestamp", [None, "", " ", "bad", "2026-01-01",
                                        "2026-01-01T00:02:00"])
def test_missing_or_ambiguous_event_timestamp_is_rejected(timestamp):
    with pytest.raises(ValueError, match="event_timestamp"):
        capture(event_timestamp=timestamp)


@pytest.mark.parametrize("reason", [None, "", " ", 2])
def test_missing_event_reason_is_rejected(reason):
    with pytest.raises(ValueError, match="reason"):
        capture(reason=reason)


@pytest.mark.parametrize("kind", ["duplicate_id", "blank_id", "reversed",
                                  "tied_time", "missing_time", "naive_time"])
def test_ambiguous_card_order_is_rejected(kind):
    rows = cards()
    if kind == "duplicate_id":
        rows[1] = (rows[0][0], rows[1][1])
    elif kind == "blank_id":
        rows[1] = (" ", rows[1][1])
    elif kind == "reversed":
        rows.reverse()
    elif kind == "tied_time":
        rows[1][1].timestamp = rows[0][1].timestamp
    elif kind == "missing_time":
        rows[-1][1].timestamp = None
    else:
        rows[-1][1].timestamp = "2026-01-01T00:03:00"
    with pytest.raises(ValueError):
        capture(rows)


def test_event_comparison_uses_instants_and_preserves_original_timestamps():
    # 01:01+01:00 is 00:01Z, before 00:02Z despite its lexical order.
    rows = cards()
    rows[1][1].timestamp = "2026-01-01T01:01:00+01:00"
    evidence = capture(rows)
    assert tuple(card.card_id for card in evidence.cards) == ("b", "c")
    assert evidence.cards[0].timestamp == "2026-01-01T01:01:00+01:00"


def test_same_instant_in_different_offsets_is_ambiguous():
    rows = cards()
    rows[1][1].timestamp = "2026-01-01T01:00:00+01:00"
    with pytest.raises(ValueError, match="order"):
        capture(rows)


def test_fractional_seconds_are_compared_without_truncation():
    # Hand order: .0000001 < .0000002 < .0000003 seconds. Only a,b qualify.
    rows = cards()[:3]
    for (identifier, card), fraction in zip(rows, ("1", "2", "3")):
        card.timestamp = f"2026-01-01T00:00:00.000000{fraction}Z"
    evidence = capture(rows, event_timestamp="2026-01-01T00:00:00.0000002Z")
    assert tuple(card.card_id for card in evidence.cards) == ("a", "b")


def test_empty_history_and_event_before_history_produce_empty_evidence():
    assert capture([]).cards == ()
    assert capture(event_timestamp="2025-12-31T23:59:00Z").cards == ()


@pytest.mark.parametrize("metadata", [{"bad": float("nan")}, {"bad": object()},
                                      {1: "non-string key"}, ["not a mapping"]])
def test_non_json_metadata_is_rejected(metadata):
    with pytest.raises(ValueError, match="metadata"):
        capture(metadata=metadata)


def test_unordered_input_is_rejected():
    with pytest.raises(ValueError, match="ordered sequence"):
        capture(dict(cards()))


@pytest.mark.parametrize("rows", [[None], [("a",)], [("a", object())], "cards"])
def test_malformed_card_entries_are_rejected(rows):
    with pytest.raises(ValueError):
        capture(rows)


@pytest.mark.parametrize("document", ["null", "[]", "{}", '{"cards": [null]}'])
def test_malformed_json_envelopes_are_rejected(document):
    with pytest.raises(ValueError):
        AlarmEvidence.from_json(document)


def test_cyclic_metadata_is_rejected():
    metadata = {}
    metadata["cycle"] = metadata
    with pytest.raises(ValueError, match="metadata"):
        capture(metadata=metadata)


@pytest.mark.parametrize("kind", ["future", "duplicate", "over_retained",
                                  "missing_reason", "mutable_arrays"])
def test_restore_validates_evidence_invariants(kind):
    document = capture().to_dict()
    if kind == "future":
        document["cards"][-1]["timestamp"] = "2026-01-01T00:03:00Z"
    elif kind == "duplicate":
        document["cards"][-1]["card_id"] = "b"
    elif kind == "over_retained":
        document["retention_count"] = 1
    elif kind == "missing_reason":
        document["reason"] = ""
    else:
        document["cards"][0]["load"] = [1, 2]
    with pytest.raises(ValueError):
        AlarmEvidence.from_json(json.dumps(document))
