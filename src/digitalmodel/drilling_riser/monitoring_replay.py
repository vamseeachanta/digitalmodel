# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Pure, bounded synthetic state journal; no engineering qualification or control.

Supplied monitoring states neither replace nor map to DecisionState or the
shared decision-spine labels. Evidence is preserved, never calculated here.
The retained full journal is for fixture-sized offline replay, not operational
storage. Digests detect corruption, not authenticity. Persistence is caller-owned.
"""
from __future__ import annotations

import hashlib
import json
import math
import re
from collections.abc import Iterable
from typing import Any

_STATES = {"NORMAL", "WATCH", "ALARM", "OUT_OF_DOMAIN", "STALE", "INVALID", "UNCOMPUTED"}
_COMPUTED = {"NORMAL", "WATCH", "ALARM"}
_IDENTITY = ("stream_id", "baseline_sha256", "motion_boundary_version", "motion_boundary_sha256")
_EVIDENCE = ("envelope_query", "criterion", "margin", "correction_model_version")
_FIELDS = {"schema_version", "ordinal", "state", "qualification", "telemetry_window_sha256", *_IDENTITY, *_EVIDENCE}


def _canonical(value: Any) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"),
                      ensure_ascii=False, allow_nan=False).encode("utf-8")


def _digest(value: Any) -> str:
    return hashlib.sha256(_canonical(value)).hexdigest()


def _text(value: Any) -> bool:
    return isinstance(value, str) and bool(value.strip())


def _quantity(value: Any) -> bool:
    if not isinstance(value, dict) or set(value) != {"value", "unit"}:
        return False
    number = value["value"]
    return (_text(value["unit"]) and (type(number) is int or
            (type(number) is float and math.isfinite(number))))


def _validate_record(record: Any) -> None:
    if not isinstance(record, dict) or set(record) != _FIELDS:
        raise ValueError("record fields")
    if type(record["schema_version"]) is not int or record["schema_version"] != 1:
        raise ValueError("record schema_version")
    if type(record["ordinal"]) is not int or record["ordinal"] < 0:
        raise ValueError("record ordinal")
    if not isinstance(record["state"], str) or record["state"] not in _STATES:
        raise ValueError("record state")
    if record["qualification"] != "synthetic_unverified":
        raise ValueError("record qualification")
    for key in ("stream_id", "motion_boundary_version"):
        if not _text(record[key]):
            raise ValueError(f"record {key}")
    for key in ("baseline_sha256", "telemetry_window_sha256", "motion_boundary_sha256"):
        if not isinstance(record[key], str) or not re.fullmatch(r"[0-9a-f]{64}", record[key]):
            raise ValueError(f"record {key}")
    for key in _EVIDENCE:
        value = record[key]
        if value is None and record["state"] not in _COMPUTED:
            continue  # Explicit unavailable evidence is retained as null, never zero.
        if key == "envelope_query":
            valid = isinstance(value, dict) and bool(value) and all(
                _text(axis) and _quantity(point) for axis, point in value.items())
        elif key == "criterion":
            valid = (isinstance(value, dict) and set(value) == {"id", "citation"}
                     and all(_text(v) for v in value.values()))
        elif key == "margin":
            valid = _quantity(value)
        else:
            valid = _text(value)
        if not valid:
            raise ValueError(f"record {key}")


def _empty() -> dict:
    return dict(schema_version=1, identity=None, last_ordinal=-1, last_state=None, journal=[])


def _extend(payload: dict, records: Iterable[dict]) -> list[dict]:
    emitted = []
    for record in records:
        _validate_record(record)
        record = json.loads(_canonical(record))  # Detach every nested caller value.
        identity = {key: record[key] for key in _IDENTITY}
        if payload["identity"] is not None and identity != payload["identity"]:
            raise ValueError("changed stream/baseline/motion-boundary identity")
        ordinal = record["ordinal"]
        digest = _digest(record)
        if ordinal < len(payload["journal"]):
            previous = payload["journal"][ordinal]
            if previous["record_sha256"] != digest:
                raise ValueError("conflicting duplicate ordinal")
            continue  # Identical retry has already emitted its event.
        if ordinal != len(payload["journal"]):
            raise ValueError("noncontiguous ordinal")
        event = dict(record=record, record_sha256=digest,
                     state_changed=record["state"] != payload["last_state"])
        payload["journal"].append(event)
        payload.update(identity=identity, last_ordinal=ordinal, last_state=record["state"])
        emitted.append(event)
    return emitted


def _seal(payload: dict) -> bytes:
    return _canonical(dict(payload, sha256=_digest(payload)))


def _unique(pairs: list[tuple[str, Any]]) -> dict:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("duplicate JSON key")
        result[key] = value
    return result


def _reject_constant(value: str) -> None:
    raise ValueError("nonfinite JSON constant")


def restore_checkpoint(data: bytes) -> dict:
    """Verify digest, journal and all derived cursor fields; return detached data.

    A matching digest does not authenticate the supplied evidence or prevent a
    coherent rewrite of the entire journal. JSON whitespace/key order may vary.
    """
    if not isinstance(data, bytes):
        raise ValueError("checkpoint must be UTF-8 JSON bytes")
    payload = json.loads(data.decode("utf-8"), object_pairs_hook=_unique,
                         parse_constant=_reject_constant)
    if not isinstance(payload, dict) or set(payload) != {*_empty(), "sha256"}:
        raise ValueError("checkpoint fields")
    digest = payload.pop("sha256")
    if digest != _digest(payload):
        raise ValueError("checkpoint digest")
    if not isinstance(payload["journal"], list):
        raise ValueError("checkpoint journal")
    rebuilt = _empty()
    for event in payload["journal"]:
        if not isinstance(event, dict) or "record" not in event:
            raise ValueError("checkpoint event")
        _extend(rebuilt, [event["record"]])
    if _canonical(rebuilt) != _canonical(payload):
        raise ValueError("checkpoint journal/cursor mismatch")
    return dict(rebuilt, sha256=digest)


def append_records(checkpoint: bytes | None, records: Iterable[dict]) -> tuple[bytes, list[dict]]:
    """Return a checkpoint and newly emitted events, without mutating inputs.

    New ordinals start at zero and are contiguous; any identical prior record is
    an idempotent retry. New out-of-order records and altered retries raise
    ValueError with no returned partial checkpoint. Consecutive equal states
    remain separate events. Quantity units are explicit opaque labels: no unit
    conversion, criterion evaluation or review of correction bounds is performed.
    """
    payload = _empty() if checkpoint is None else restore_checkpoint(checkpoint)
    payload.pop("sha256", None)
    events = _extend(payload, records)
    return _seal(payload), events
