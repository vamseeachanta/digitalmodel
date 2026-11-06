# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline structural integrity checks with explicit caller-controlled budgets.

No files, solvers, rights resolvers or execution entrypoints are accessed.
Limits are required inputs, not engineering criteria. Callers must supply an
independent complete inventory and extract every reference from their own format.
Self-consistent caller assertions do not establish authority or reproducibility.
"""

from __future__ import annotations

import hashlib
import json
import math
import re
from collections.abc import Iterator
from typing import Any, NoReturn

FieldPath = tuple[str | int, ...]


def _limit(value: int, name: str, *, allow_zero: bool = False) -> None:
    if type(value) is not int or value < 0 or (value == 0 and not allow_zero):
        raise ValueError(f"Invalid {name} limit")


def _children(value: dict | list, path: FieldPath) -> Iterator[tuple[FieldPath, Any]]:
    entries = value.items() if type(value) is dict else enumerate(value)
    for key, child in entries:
        if type(value) is dict and type(key) is not str:
            raise ValueError("Object keys must be strings")
        yield path + (key,), child


def bounded_leaves(value: Any, *, max_nodes: int, max_depth: int) -> dict:
    """Return typed tuple paths only after the entire structure is checked.

    Root depth is zero; every value visit consumes one node, including repeated
    aliases and empty containers. Keys are path labels, not additional nodes.
    Dotted and empty string keys remain distinct; array indices are integers.
    Ancestor cycles refuse; shared acyclic containers are expanded per use.
    Values must be JSON types with finite floats. Empty containers remain leaves.
    Returned leaf containers are references; concurrent mutation is unsupported.
    """
    _limit(max_nodes, "node")
    _limit(max_depth, "depth", allow_zero=True)
    frames: list[tuple[Iterator[tuple[FieldPath, Any]], int | None]] = [
        (iter((((), value),)), None),
    ]
    active: set[int] = set()
    leaves: dict[FieldPath, Any] = {}
    visited = 0
    while frames:
        try:
            path, node = next(frames[-1][0])
        except StopIteration:
            _, identity = frames.pop()
            if identity is not None:
                active.remove(identity)
            continue
        visited += 1
        if visited > max_nodes:
            raise ValueError("Source traversal node budget exceeded")
        if len(path) > max_depth:
            raise ValueError("Source traversal depth limit exceeded")
        if type(node) in (dict, list):
            if id(node) in active:
                raise ValueError("Source traversal cycle")
            if node:
                active.add(id(node))
                frames.append((iter(_children(node, path)), id(node)))
                continue
        elif type(node) not in (str, int, float, bool, type(None)):
            raise ValueError("Unsupported source value type")
        elif type(node) is float and not math.isfinite(node):
            raise ValueError("Non-finite source value")
        leaves[path] = node
    return leaves


def _check_json_nesting(text: str, max_depth: int) -> None:
    # RFC 8259 (2017), sections 7 and 9: strings escape structural characters;
    # parser limits are implementation policy. Zero denotes the root depth.
    depth = -1
    quoted = escaped = False
    for char in text:
        if quoted:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                quoted = False
        elif char == '"':
            quoted = True
        elif char in "[{":
            depth += 1
            if depth > max_depth:
                raise ValueError("JSON nesting depth limit exceeded")
        elif char in "]}":
            depth -= 1


def _unique_pairs(pairs: list) -> dict:
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("Duplicate JSON key")
        result[key] = value
    return result


def _reject_constant(value: str) -> NoReturn:
    raise ValueError("Non-finite JSON value")


def load_bounded_json(raw: bytes, *, max_bytes: int, max_nodes: int,
                      max_depth: int) -> Any:
    """Decode strict UTF-8 JSON after byte-size and nesting preflight.

    RFC 8259 (2017), sections 4, 6, 8.1 and 9: duplicate names refuse,
    non-finite numbers refuse, and limits remain explicit implementation policy.
    Node limits apply after decoding; byte/depth limits bound decoder exposure.
    The standard decoder's numeric and recursion limits can refuse earlier.
    This is JSON-only: YAML constructors and alias parsing are not provided.
    """
    _limit(max_bytes, "byte")
    _limit(max_nodes, "node")
    _limit(max_depth, "depth", allow_zero=True)
    if type(raw) is not bytes or len(raw) > max_bytes:
        raise ValueError("JSON requires bounded bytes")
    try:
        text = raw.decode("utf-8")
        _check_json_nesting(text, max_depth)
        value = json.loads(text, object_pairs_hook=_unique_pairs,
                           parse_constant=_reject_constant)
    except (UnicodeError, json.JSONDecodeError, RecursionError) as error:
        raise ValueError("Invalid or excessive JSON input") from error
    bounded_leaves(value, max_nodes=max_nodes, max_depth=max_depth)
    return value


def verify_payload_inventory(payloads: dict, expected_sha256: dict,
                             references: dict) -> dict:
    """Verify an exact nonempty inventory and its declared reference endpoints.

    Every ID must appear in payloads, independently supplied digest pins and the
    reference map (an empty list declares no outgoing references). Cyclic
    references are allowed; structural cycles in bounded_leaves are not.
    The caller owns completeness of required IDs and reference extraction.
    No runtime discovery, retention, rights or engineering assessment occurs.
    The receipt applies to the supplied bytes at the time of the call only.
    """
    if any(type(item) is not dict for item in (payloads, expected_sha256, references)):
        raise ValueError("Inventory, pins and references must be objects")
    if not payloads or any(type(key) is not str or not key.strip() for key in payloads):
        raise ValueError("Nonempty inventory and IDs required")
    if set(payloads) != set(expected_sha256) or set(payloads) != set(references):
        raise ValueError("Payload, pin and reference inventories differ")
    # hashlib SHA-256 digest_size specifies the digest width; hex uses two
    # characters per byte. This is a representation length, not a design limit.
    hex_width = hashlib.sha256().digest_size * 2
    reference_count = 0
    for identity, raw in payloads.items():
        pin = expected_sha256[identity]
        if (type(raw) is not bytes or type(pin) is not str
                or not re.fullmatch(f"[0-9a-f]{{{hex_width}}}", pin)
                or hashlib.sha256(raw).hexdigest() != pin):
            raise ValueError("Payload digest mismatch or invalid pin/bytes")
        edges = references[identity]
        if (type(edges) not in (list, tuple)
                or any(type(edge) is not str or edge not in payloads for edge in edges)):
            raise ValueError("Unresolved or invalid payload reference")
        if len(set(edges)) != len(edges):
            raise ValueError("Duplicate payload reference")
        reference_count += len(edges)
    return {"integrity": "VERIFIED", "references": "VERIFIED",
            "artifact_count": len(payloads), "reference_count": reference_count,
            "inventory_completeness": "NOT_EVALUATED",
            "source_rights": "NOT_EVALUATED", "retention": "NOT_EVALUATED",
            "engineering_qualification": "NOT_EVALUATED"}
