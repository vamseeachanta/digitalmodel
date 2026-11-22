# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline binding diagnostics; no publication or engineering authentication."""

from __future__ import annotations

import hashlib
import json
import re
from collections.abc import Mapping
from typing import Any

INTERFACE_VERSION = "riser-monitoring-preflight/v1"


def _required_equalities(contract: Any) -> list[str] | None:
    if not isinstance(contract, Mapping):
        return None
    graph = contract.get("non_blocking_consumer_graph")
    if not isinstance(graph, Mapping):
        return None
    fields = graph.get("required_baseline_equalities")
    if not isinstance(fields, list) or not fields:
        return None
    if not all(
        isinstance(field, str)
        and (
            field in {"HF_commit_sha", "operating_envelope_version"}
            or re.fullmatch(r"[a-zA-Z][a-zA-Z0-9_]*_sha256", field)
        )
        for field in fields
    ) or len(fields) != len(set(fields)):
        return None
    return fields


def _value_problem(field: str, value: Any) -> str | None:
    if not isinstance(value, str):
        return "invalid_type"
    if not value.strip():
        return "empty"
    if field == "HF_commit_sha" and not re.fullmatch(r"[0-9a-f]{40}", value):
        return "malformed_commit"
    if field.endswith("_sha256") and not re.fullmatch(r"[0-9a-f]{64}", value):
        return "malformed_sha256"
    return None


def _validated_fields(
    record: Any, role: str, fields: list[str], diagnostics: list[dict[str, str]]
) -> set[str]:
    if not isinstance(record, Mapping):
        diagnostics.append(
            {"code": "invalid_mapping", "field": "record", "source_role": role}
        )
        return set()
    if record.get("schema_version") != INTERFACE_VERSION:
        diagnostics.append(
            {
                "code": "unsupported_schema_version",
                "field": "schema_version",
                "source_role": role,
            }
        )
        return set()
    valid = set()
    for field in fields:
        problem = (
            "missing" if field not in record else _value_problem(field, record[field])
        )
        if problem:
            diagnostics.append({"code": problem, "field": field, "source_role": role})
        else:
            valid.add(field)
    return valid


def check_monitoring_bindings(
    contract: Mapping[str, Any],
    candidate: Mapping[str, Any],
    release_receipt: Mapping[str, Any],
    operating_envelope: Mapping[str, Any],
) -> dict[str, Any]:
    """Compare supplied projections without authenticating their evidence.

    The contract is the output of ``validate_parent_contract``. All three
    projections use the explicit preflight interface version, which does not
    represent an upstream receipt or operating envelope schema version.
    Commit and digest strings use full lowercase hexadecimal identities.
    The diagnostic digest commits to diagnostics, never to evidence values.
    """
    diagnostics: list[dict[str, str]] = []
    fields = _required_equalities(contract)
    if fields is None:
        diagnostics.append(
            {
                "code": "invalid_contract",
                "field": "required_baseline_equalities",
                "source_role": "contract",
            }
        )
        fields = []
    sources = {
        "release_receipt": release_receipt,
        "operating_envelope": operating_envelope,
    }
    groups = {
        role: [
            field
            for field in fields
            if field.startswith("operating_") == (role == "operating_envelope")
        ]
        for role in sources
    }
    valid_candidate = _validated_fields(candidate, "candidate", fields, diagnostics)
    for role, source in sources.items():
        valid_source = _validated_fields(source, role, groups[role], diagnostics)
        for field in sorted(valid_candidate & valid_source):
            if candidate[field] != source[field]:
                diagnostics.append(
                    {"code": "unequal", "field": field, "source_role": role}
                )
    return _diagnostic_record(fields, diagnostics)


def _diagnostic_record(
    fields: list[str], diagnostics: list[dict[str, str]]
) -> dict[str, Any]:
    record: dict[str, Any] = {
        "schema_version": INTERFACE_VERSION,
        "scope": "preflight-only",
        "authentication": "unauthenticated",
        "status": "binding-inconsistent" if diagnostics else "binding-consistent",
        "required_equalities": sorted(fields),
        "diagnostics": sorted(
            diagnostics,
            key=lambda item: (item["source_role"], item["field"], item["code"]),
        ),
    }
    payload = json.dumps(
        record, sort_keys=True, separators=(",", ":"), ensure_ascii=False
    )
    record["diagnostic_sha256"] = hashlib.sha256(payload.encode("utf-8")).hexdigest()
    return record
