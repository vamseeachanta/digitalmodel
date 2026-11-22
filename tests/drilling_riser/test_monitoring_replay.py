# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic journal mechanics; no engineering qualification is established."""
import copy
import hashlib
import json

import pytest

from digitalmodel.drilling_riser.monitoring_replay import append_records, restore_checkpoint

STATES = ("NORMAL", "NORMAL", "WATCH", "ALARM", "OUT_OF_DOMAIN", "STALE", "INVALID", "UNCOMPUTED")


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False, allow_nan=False).encode("utf-8")


def record(ordinal=0, state="NORMAL"):
    return dict(schema_version=1, stream_id="synthetic-stream", ordinal=ordinal,
                state=state, qualification="synthetic_unverified", baseline_sha256="a" * 64,
                telemetry_window_sha256="b" * 64, motion_boundary_version="synthetic-v1",
                motion_boundary_sha256="c" * 64, envelope_query={"offset": {"value": 0.0, "unit": "m"}},
                criterion={"id": "synthetic-limit", "citation": "synthetic reference"},
                margin={"value": 1.0, "unit": "deg"}, correction_model_version="synthetic-v1")


def stream():
    return [record(i, state) for i, state in enumerate(STATES)]


def resign(payload):
    payload.pop("sha256", None)
    return canonical(dict(payload, sha256=hashlib.sha256(canonical(payload)).hexdigest()))


def test_every_split_and_restart_matches_batch_bytes():
    records = stream()
    checkpoint, events = append_records(None, records)
    for split in range(len(records) + 1):
        first, left = append_records(None, records[:split])
        restored = canonical(restore_checkpoint(first))
        final, right = append_records(restored, records[split:])
        assert final == checkpoint
        assert canonical(left + right) == canonical(events)
    assert [e["record"]["state"] for e in events] == list(STATES)
    assert [e["state_changed"] for e in events] == [True, False] + [True] * 6
    assert [e["record"] for e in events] == records
    assert restore_checkpoint(checkpoint)["last_ordinal"] == 7


def test_empty_retries_and_mutation_isolation():
    empty, events = append_records(None, [])
    assert events == []
    assert restore_checkpoint(empty)["last_state"] is None
    records = stream()
    saved = copy.deepcopy(records)
    checkpoint, emitted = append_records(empty, records)
    assert records == saved
    assert append_records(checkpoint, list(reversed(saved))) == (checkpoint, [])
    records[0]["margin"]["value"] = 99
    emitted[0]["record"]["margin"]["value"] = 88
    assert restore_checkpoint(checkpoint)["journal"][0]["record"] == saved[0]
    assert append_records(checkpoint, []) == (checkpoint, [])


@pytest.mark.parametrize("change", [
    {"margin": {"value": 2, "unit": "deg"}}, {"state": "WATCH"},
    {"telemetry_window_sha256": "d" * 64},
])
def test_conflicting_prior_event_is_rejected_atomically(change):
    checkpoint, _ = append_records(None, stream())
    retry = dict(record(), **change)
    with pytest.raises(ValueError):
        append_records(checkpoint, [record(8), retry])
    assert restore_checkpoint(checkpoint)["last_ordinal"] == 7


@pytest.mark.parametrize("records", [[record(1)], [record(), record(2)], [record(1), record()]])
def test_gaps_and_reordered_new_records_fail(records):
    with pytest.raises(ValueError):
        append_records(None, records)


@pytest.mark.parametrize("field,value", [
    ("schema_version", 2), ("schema_version", True), ("ordinal", True), ("ordinal", -1),
    ("ordinal", 0.0), ("state", "GO"), ("state", []), ("stream_id", " "),
    ("baseline_sha256", "bad"), ("telemetry_window_sha256", None),
    ("motion_boundary_sha256", "A" * 64), ("motion_boundary_version", ""),
    ("qualification", "verified"), ("margin", {"value": float("nan"), "unit": "deg"}),
    ("margin", {"value": float("inf"), "unit": "deg"}),
    ("margin", {"value": True, "unit": "deg"}), ("margin", {"value": "1,000", "unit": "deg"}),
    ("margin", {"value": 1, "unit": ""}), ("margin", None),
    ("criterion", {}), ("criterion", {"id": "x", "citation": ""}),
    ("envelope_query", {}), ("envelope_query", {"x": {"value": float("nan"), "unit": "m"}}),
    ("correction_model_version", None),
])
def test_malformed_records_are_rejected(field, value):
    with pytest.raises(ValueError):
        append_records(None, [dict(record(), **{field: value})])


@pytest.mark.parametrize("field", list(record()))
def test_missing_fields_are_not_defaulted(field):
    item = record()
    del item[field]
    with pytest.raises(ValueError):
        append_records(None, [item])


@pytest.mark.parametrize("field", ["stream_id", "baseline_sha256", "motion_boundary_version", "motion_boundary_sha256"])
def test_changed_identity_does_not_advance_checkpoint(field):
    checkpoint, _ = append_records(None, [record()])
    changed = record(1)
    changed[field] = "d" * 64
    with pytest.raises(ValueError):
        append_records(checkpoint, [changed])
    assert append_records(checkpoint, [record(1)])[1][0]["record"]["ordinal"] == 1


@pytest.mark.parametrize("state", STATES[4:])
def test_explicit_uncomputed_evidence_is_preserved(state):
    item = record(state=state)
    for key in ("envelope_query", "criterion", "margin", "correction_model_version"):
        item[key] = None
    _, events = append_records(None, [item])
    assert events[0]["record"] == item


@pytest.mark.parametrize("field,value", [("last_ordinal", 99), ("last_state", "WATCH"),
                                         ("schema_version", 2), ("identity", None), ("extra", 1)])
def test_recomputed_digest_cannot_hide_cursor_tampering(field, value):
    checkpoint, _ = append_records(None, [record()])
    payload = json.loads(checkpoint)
    payload[field] = value
    with pytest.raises(ValueError):
        restore_checkpoint(resign(payload))


def test_journal_and_digest_tampering_and_duplicate_json_keys_fail():
    checkpoint, _ = append_records(None, stream())
    payload = json.loads(checkpoint)
    payload["journal"][0]["record"]["margin"]["value"] = 99
    with pytest.raises(ValueError):
        restore_checkpoint(canonical(payload))
    for key, value in (("record_sha256", "d" * 64), ("state_changed", False)):
        payload = json.loads(checkpoint)
        payload["journal"][0][key] = value
        with pytest.raises(ValueError):
            restore_checkpoint(resign(payload))
    for data in (b'{}', b'[]', b'{"schema_version":1,"schema_version":1}', b'{"x":NaN}'):
        with pytest.raises(ValueError):
            restore_checkpoint(data)


def test_key_order_and_unicode_preserve_canonical_evidence():
    item = record()
    item["criterion"]["citation"] = "synthetic \u03b1"
    checkpoint, events = append_records(None, [item, item])
    assert len(events) == 1
    assert append_records(None, [dict(reversed(list(item.items())))])[0] == checkpoint
    assert "\u03b1".encode() in checkpoint
