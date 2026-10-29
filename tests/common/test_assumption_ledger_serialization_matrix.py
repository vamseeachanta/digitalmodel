# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Serialization contracts using independently authored arithmetic fixtures.

Numeric basis: equal three-metre readings give 3 - 3 = 0 m; a one-metre
reading relative to three metres gives 1 - 3 = -2 m. These synthetic values
are serialization examples, not resolver defaults or engineering limits.
Review ranks are synthetic: neutral zero and 1 + 1 = 2. Expected records
come from fixture membership, never from the ledger's records or rows.
"""

from __future__ import annotations

import json

import pytest

from digitalmodel.common.assumption_ledger import (
    AssumptionLedger,
    AssumptionSource,
    Confidence,
)


SOURCE_CASES = (
    (AssumptionSource.USER_SUPPLIED, "user_supplied"),
    (AssumptionSource.ASSUMED_DEFAULT, "assumed_default"),
    (AssumptionSource.ESTIMATED_FROM_DATA, "estimated_from_data"),
    (AssumptionSource.DATABASE_LOOKUP, "database_lookup"),
)
CONFIDENCE_CASES = (
    (Confidence.HIGH, "high"),
    (Confidence.MEDIUM, "medium"),
    (Confidence.LOW, "low"),
)


@pytest.fixture
def authored_values():
    """Hand-calculated offsets, with both absent and explicit references."""
    # Keep the authored enum matrix exhaustive when the public enums change.
    assert {source for source, _ in SOURCE_CASES} == set(AssumptionSource)
    assert {confidence for confidence, _ in CONFIDENCE_CASES} == set(Confidence)
    return (
        {
            "field": "offset.zero",
            "value": 0.0,
            "basis": "Equal readings: 3 m - 3 m = 0 m; units: m",
            "reference": None,
            "impact": 0,
        },
        {
            "field": "offset.signed",
            "value": -2.0,
            "basis": "Relative reading: 1 m - 3 m = -2 m; units: m",
            "reference": "hand-calculation: relative reading",
            "impact": 2,
        },
    )


def _record_authored(ledger, authored):
    """Submit fixture dictionaries through the public recording method."""
    for item in authored:
        ledger.record(**item)


def _assert_json_round_trip(ledger, expected):
    """Check emitted JSON and reconstructed state against authored records."""
    payload = {"records": expected}
    canonical = json.dumps(payload, sort_keys=True)
    encoded = ledger.to_json()
    # Exact list equality rejects missing, extra and duplicated records.
    assert json.loads(encoded) == payload
    # Canonical JSON also distinguishes booleans from numeric zero in nests.
    assert json.dumps(json.loads(encoded), sort_keys=True) == canonical
    assert ledger.to_dict() == payload
    rebuilt = AssumptionLedger.from_json(encoded)
    assert rebuilt.to_dict() == payload
    assert json.dumps(json.loads(rebuilt.to_json()), sort_keys=True) == canonical
    # Also decode independent JSON, rather than only the serializer's output.
    decoded = AssumptionLedger.from_json(json.dumps(payload))
    assert decoded.to_dict() == payload
    assert json.dumps(json.loads(decoded.to_json()), sort_keys=True) == canonical
    return json.loads(encoded)["records"]


@pytest.mark.parametrize("confidence, wire_confidence", CONFIDENCE_CASES)
def test_assumption_json_preserves_all_nonuser_record_fields(
    authored_values, confidence, wire_confidence
):
    """Every source/confidence pairing retains each authored field once."""
    ledger = AssumptionLedger()
    authored = []
    expected = []
    for source, wire_source in SOURCE_CASES:
        for value in authored_values:
            item = {
                **value,
                "field": f"{wire_source}.{value['field']}",
                "source": source,
                "confidence": confidence,
            }
            authored.append(item)
            if source != AssumptionSource.USER_SUPPLIED:
                expected.append(
                    {**item, "source": wire_source, "confidence": wire_confidence}
                )
    _record_authored(ledger, authored)
    actual = _assert_json_round_trip(ledger, expected)
    # Python considers False == 0; require a real floating-point zero.
    zeros = [item for item in actual if item["field"].endswith(".offset.zero")]
    assert len(zeros) == len(SOURCE_CASES) - 1  # Exclude the user source only.
    assert all(type(item["value"]) is float and item["value"] == 0.0 for item in zeros)
    assert all(type(item["value"]) is float for item in actual)


@pytest.mark.parametrize("confidence, wire_confidence", CONFIDENCE_CASES)
def test_user_only_assumptions_serialize_as_empty(
    authored_values, confidence, wire_confidence
):
    """Public recording excludes supplied values in both JSON directions."""
    ledger = AssumptionLedger()
    _record_authored(
        ledger,
        [
            {**value, "source": AssumptionSource.USER_SUPPLIED, "confidence": confidence}
            for value in authored_values
        ],
    )
    _assert_json_round_trip(ledger, [])
    assert not ledger
    assert ledger.records == []
    assert not AssumptionLedger.from_json('{"records": []}')


@pytest.mark.parametrize("source, wire_source", SOURCE_CASES)
@pytest.mark.parametrize("confidence, wire_confidence", CONFIDENCE_CASES)
def test_assumption_json_preserves_nested_values(
    authored_values, source, wire_source, confidence, wire_confidence
):
    """Nested dictionaries/lists retain offsets, booleans, nulls and units."""
    authored = {
        "field": "offset.profile",
        "value": {
            "offsets": [item["value"] for item in authored_values],
            "units": "m",
            "metadata": {"label": "authored", "active": True, "note": None},
            "samples": [{"offset": 0.0, "active": False}],
        },
        "source": source,
        "basis": "Hand calculation: [3 - 3, 1 - 3] = [0, -2]; units: m",
        "reference": "hand-calculation: offset profile",
        "confidence": confidence,
        "impact": 2,
    }
    ledger = AssumptionLedger()
    _record_authored(ledger, [authored])
    expected = (
        []
        if source == AssumptionSource.USER_SUPPLIED
        else [{**authored, "source": wire_source, "confidence": wire_confidence}]
    )
    _assert_json_round_trip(ledger, expected)
