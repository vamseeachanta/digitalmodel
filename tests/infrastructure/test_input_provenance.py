# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic structural examples; all budgets are test inputs, not design limits.

Source: RFC 8259 (2017), sections 4, 6 and 9. Expected paths, counts and
boundary values below are enumerated by hand independently of the helper.
"""

import hashlib
import json

import pytest

from digitalmodel.infrastructure.input_provenance import (
    bounded_leaves,
    load_bounded_json,
    verify_payload_inventory,
)


def test_paths_do_not_collide_and_empty_containers_survive():
    """Root + three scalars + empty list + empty dict + nested dict = 7 nodes."""
    value = {"a.b": 1, "a": {"b": 2}, "": 3, "list": [], "dict": {}}
    assert bounded_leaves(value, max_nodes=7, max_depth=2) == {
        ("a.b",): 1, ("a", "b"): 2, ("",): 3, ("list",): [], ("dict",): {},
    }
    with pytest.raises(ValueError, match="node budget"):
        bounded_leaves(value, max_nodes=6, max_depth=2)


def test_sequence_indices_differ_from_mapping_keys():
    """Array index 0 and the object key '0' have different typed paths."""
    assert bounded_leaves([{"0": None}, False], max_nodes=4, max_depth=2) == {
        (0, "0"): None, (1,): False,
    }


def test_shared_alias_is_charged_for_each_occurrence():
    """Root + two shared-list visits + two scalar visits = 5 occurrences."""
    shared = ["synthetic"]
    value = {"a": shared, "b": shared}
    assert bounded_leaves(value, max_nodes=5, max_depth=2) == {
        ("a", 0): "synthetic", ("b", 0): "synthetic",
    }
    with pytest.raises(ValueError, match="node budget"):
        bounded_leaves(value, max_nodes=4, max_depth=2)


def test_alias_expansion_refuses_before_exponential_work():
    """Three doublings give 1+2+4+8=15 visits, despite four unique objects."""
    value = "leaf"
    for _ in range(3):
        value = [value, value]
    assert len(bounded_leaves(value, max_nodes=15, max_depth=3)) == 8
    with pytest.raises(ValueError, match="node budget"):
        bounded_leaves(value, max_nodes=14, max_depth=3)


@pytest.mark.parametrize("kind", ["list", "dict"])
def test_cycles_refuse(kind):
    value = [] if kind == "list" else {}
    if kind == "list":
        value.append(value)
    else:
        value["self"] = value
    with pytest.raises(ValueError, match="cycle"):
        bounded_leaves(value, max_nodes=10, max_depth=10)


def test_depth_counts_edges_and_root_is_zero():
    """The scalar in [[0]] has depth 2; root scalar has depth 0."""
    assert bounded_leaves(0, max_nodes=1, max_depth=0) == {(): 0}
    assert bounded_leaves([[0]], max_nodes=3, max_depth=2) == {(0, 0): 0}
    with pytest.raises(ValueError, match="depth"):
        bounded_leaves([[0]], max_nodes=3, max_depth=1)


@pytest.mark.parametrize("value", [{1: "bad"}, (1,), b"bytes", {"bad": object()},
                                   float("nan"), float("inf"), float("-inf")])
def test_unsupported_values_refuse(value):
    with pytest.raises(ValueError):
        bounded_leaves(value, max_nodes=10, max_depth=10)


@pytest.mark.parametrize("name,value", [("max_nodes", 0), ("max_nodes", True),
                                      ("max_depth", -1), ("max_depth", 1.5)])
def test_invalid_traversal_budgets_refuse(name, value):
    limits = {"max_nodes": 1, "max_depth": 0, name: value}
    with pytest.raises(ValueError, match="limit"):
        bounded_leaves(None, **limits)


def test_deep_traversal_does_not_depend_on_python_recursion():
    """A synthetic chain of 1500 lists has 1501 visits and one leaf."""
    value = None
    for _ in range(1500):
        value = [value]
    leaves = bounded_leaves(value, max_nodes=1501, max_depth=1500)
    assert leaves == {(0,) * 1500: None}


def test_json_exact_boundaries_and_quotes():
    """Root + string + list + scalar = 4 visits; the last has depth 2."""
    raw = json.dumps({"text": '[{\\\"', "values": [0]}).encode()
    assert load_bounded_json(raw, max_bytes=len(raw), max_nodes=4, max_depth=2) == {
        "text": '[{\\\"', "values": [0],
    }
    with pytest.raises(ValueError, match="byte"):
        load_bounded_json(raw, max_bytes=len(raw) - 1, max_nodes=4, max_depth=2)
    with pytest.raises(ValueError, match="node budget"):
        load_bounded_json(raw, max_bytes=len(raw), max_nodes=3, max_depth=2)
    with pytest.raises(ValueError, match="depth"):
        load_bounded_json(raw, max_bytes=len(raw), max_nodes=4, max_depth=1)


@pytest.mark.parametrize("raw", [b'{"a":1,"a":2}', b'{"a":{"b":0,"b":1}}',
                               b'{"a":0,"\\u0061":1}', b'NaN', b'Infinity',
                               b'-Infinity', b'1e9999', b'\xff', b'{', b'[] []'])
def test_json_ambiguous_or_invalid_values_refuse(raw):
    with pytest.raises(ValueError):
        load_bounded_json(raw, max_bytes=len(raw), max_nodes=10, max_depth=10)


def test_json_depth_refuses_before_decoder(monkeypatch):
    """The child container is at depth 1, beyond the supplied root-only limit."""
    def forbidden(*args, **kwargs):
        pytest.fail("Decoder ran before nesting preflight")

    monkeypatch.setattr(json, "loads", forbidden)
    with pytest.raises(ValueError, match="depth"):
        load_bounded_json(b"[[]]", max_bytes=4, max_nodes=2, max_depth=0)


@pytest.mark.parametrize("raw", ["{}", bytearray(b"{}"), None])
def test_json_requires_bytes(raw):
    with pytest.raises(ValueError, match="bytes"):
        load_bounded_json(raw, max_bytes=2, max_nodes=1, max_depth=0)


@pytest.mark.parametrize("limit", [0, True, -1, 1.5])
def test_json_invalid_byte_budget_refuses(limit):
    with pytest.raises(ValueError, match="limit"):
        load_bounded_json(b"0", max_bytes=limit, max_nodes=1, max_depth=0)


def inventory():
    """Synthetic retained source, derivative, ledger and receipt; no solver data."""
    payloads = {"source": b"input", "derived": b"output", "ledger": b"transform",
                "receipt": b"checked"}
    pins = {key: hashlib.sha256(value).hexdigest() for key, value in payloads.items()}
    edges = {"source": [], "derived": ["source", "ledger", "receipt"],
             "ledger": ["source"], "receipt": ["derived"]}
    return payloads, pins, edges


def test_complete_inventory_resolves_receipt_and_preserves_trust_limits():
    """Four IDs and 3+1+1=5 declared references, all resolved by hand."""
    assert verify_payload_inventory(*inventory()) == {
        "integrity": "VERIFIED", "references": "VERIFIED",
        "artifact_count": 4, "reference_count": 5,
        "inventory_completeness": "NOT_EVALUATED",
        "source_rights": "NOT_EVALUATED", "retention": "NOT_EVALUATED",
        "engineering_qualification": "NOT_EVALUATED",
    }


@pytest.mark.parametrize("fault", ["omitted_source", "omitted_derived", "omitted_ledger",
                                 "omitted_receipt", "extra", "bytes", "digest", "unknown",
                                 "missing_edges", "extra_edges", "duplicate_edge",
                                 "bad_digest", "bad_id", "string_edges", "not_bytes"])
def test_inventory_mutations_refuse(fault):
    payloads, pins, edges = inventory()
    if fault.startswith("omitted_"):
        payloads.pop(fault.removeprefix("omitted_"))
    elif fault == "extra":
        payloads["other"] = b"unbound"
    elif fault == "bytes":
        payloads["derived"] += b"changed"
    elif fault == "digest":
        pins["derived"] = pins["source"]
    elif fault == "unknown":
        edges["receipt"] = ["unresolved"]
    elif fault == "missing_edges":
        edges.pop("ledger")
    elif fault == "extra_edges":
        edges["other"] = []
    elif fault == "duplicate_edge":
        edges["source"] = ["ledger", "ledger"]
    elif fault == "bad_digest":
        pins["source"] = "invalid"
    elif fault == "bad_id":
        payloads[""] = payloads.pop("source")
    elif fault == "string_edges":
        edges["source"] = "ledger"
    else:
        payloads["source"] = "input"
    with pytest.raises(ValueError):
        verify_payload_inventory(payloads, pins, edges)


def test_inventory_cannot_authenticate_a_consistently_replaced_package():
    """Self-consistent caller pins establish equality only, never permission."""
    payloads, _, edges = inventory()
    payloads["source"] = b"replacement"
    pins = {key: hashlib.sha256(value).hexdigest() for key, value in payloads.items()}
    result = verify_payload_inventory(payloads, pins, edges)
    assert result["integrity"] == "VERIFIED"
    assert result["source_rights"] == result["retention"] == "NOT_EVALUATED"


def test_empty_inventory_is_not_evidence():
    with pytest.raises(ValueError):
        verify_payload_inventory({}, {}, {})


def test_consistent_omission_is_outside_the_inventory_trust_boundary():
    """One isolated source has no edges: a caller can omit it consistently.

    This shows why an independent required-artifact set is a caller obligation;
    the helper cannot infer a derivative or a receipt from opaque source bytes.
    """
    payloads = {"source": b"synthetic"}
    pins = {"source": hashlib.sha256(payloads["source"]).hexdigest()}
    result = verify_payload_inventory(payloads, pins, {"source": []})
    assert result["artifact_count"] == 1
    assert result["inventory_completeness"] == "NOT_EVALUATED"
    assert result["engineering_qualification"] == "NOT_EVALUATED"


@pytest.mark.parametrize("term", ["revoked", "expires", "conditions", "scope_excludes"])
def test_digest_bound_rights_terms_never_become_permission(term):
    """Opaque rights records are integrity inputs only, regardless of their terms."""
    raw = json.dumps({"decision": "permitted", term: "limiting condition"}).encode()
    result = verify_payload_inventory(
        {"rights": raw}, {"rights": hashlib.sha256(raw).hexdigest()}, {"rights": []},
    )
    assert result["integrity"] == "VERIFIED"
    assert result["source_rights"] == "NOT_EVALUATED"


@pytest.mark.parametrize("raw,expected", [
    (b'""', ""), (b'null', None), (b'true', True),
    (b'{}', {}), (b'[]', []), (b'2.5', 2.5),
])
def test_json_root_values_are_not_silently_discarded(raw, expected):
    """Every listed JSON text has one root node and no child edges."""
    value = load_bounded_json(raw, max_bytes=len(raw), max_nodes=1, max_depth=0)
    assert bounded_leaves(value, max_nodes=1, max_depth=0) == {(): expected}


def test_decoder_recursion_error_is_a_controlled_refusal(monkeypatch):
    def exhausted(*args, **kwargs):
        raise RecursionError("synthetic decoder limit")

    monkeypatch.setattr(json, "loads", exhausted)
    with pytest.raises(ValueError, match="excessive JSON"):
        load_bounded_json(b"0", max_bytes=1, max_nodes=1, max_depth=0)
