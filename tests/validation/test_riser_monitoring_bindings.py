# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Synthetic offline evidence for monitoring binding preflight only."""

from __future__ import annotations

import hashlib
import json
import builtins
import copy
import os
from pathlib import Path
import socket
import subprocess
from types import MappingProxyType

import pytest

from digitalmodel.contract_validation.riser_monitoring_bindings import (
    check_monitoring_bindings,
)
from digitalmodel.contract_validation.riser_parent_contract import (
    validate_parent_contract,
)
from tests.validation.test_riser_parent_contract import _bundle

INTERFACE_VERSION = "riser-monitoring-preflight/v1"


@pytest.fixture(scope="module")
def contract(tmp_path_factory):
    manifest, output = _bundle(tmp_path_factory.mktemp("monitoring-contract"))
    return validate_parent_contract(manifest, output)


def _equalities(contract):
    return contract["non_blocking_consumer_graph"]["required_baseline_equalities"]


def _triplet(contract):
    """Construct arbitrary digest-shaped strings, never engineering evidence."""
    candidate = {"schema_version": INTERFACE_VERSION}
    receipt = {"schema_version": INTERFACE_VERSION}
    envelope = {"schema_version": INTERFACE_VERSION}
    for index, field in enumerate(_equalities(contract), start=1):
        value = f"{index:064x}"
        if field == "HF_commit_sha":
            value = "a" * 40
        elif field == "operating_envelope_version":
            value = "synthetic-envelope-v1"
        candidate[field] = value
        source = envelope if field.startswith("operating_") else receipt
        source[field] = value
    return candidate, receipt, envelope


def _canonical(record):
    return json.dumps(record, sort_keys=True, separators=(",", ":"), ensure_ascii=False)


def test_matching_arbitrary_hashes_are_only_unauthenticated_preflight(contract):
    result = check_monitoring_bindings(contract, *_triplet(contract))

    assert result["schema_version"] == INTERFACE_VERSION
    assert result["scope"] == "preflight-only"
    assert result["authentication"] == "unauthenticated"
    assert result["status"] == "binding-consistent"
    assert result["required_equalities"] == sorted(_equalities(contract))
    assert len(result["required_equalities"]) == 9
    assert result["diagnostics"] == []
    payload = {
        key: value for key, value in result.items() if key != "diagnostic_sha256"
    }
    assert (
        result["diagnostic_sha256"]
        == hashlib.sha256(_canonical(payload).encode("utf-8")).hexdigest()
    )
    assert result["status"] not in {
        "accepted",
        "green",
        "oracle-passing",
        "publication-verified",
    }


@pytest.mark.parametrize("index", range(9))
@pytest.mark.parametrize("mutated_role", [0, 1])
def test_each_required_equality_rejects_a_different_identity(
    contract, index, mutated_role
):
    candidate, receipt, envelope = _triplet(contract)
    field = _equalities(contract)[index]
    source_role = (
        "operating_envelope" if field.startswith("operating_") else "release_receipt"
    )
    source = envelope if field.startswith("operating_") else receipt
    target = candidate if mutated_role == 0 else source
    if field == "operating_envelope_version":
        target[field] = "synthetic-envelope-v2"
    else:
        target[field] = "b" * len(target[field])

    result = check_monitoring_bindings(contract, candidate, receipt, envelope)

    assert result["status"] == "binding-inconsistent"
    assert result["diagnostics"] == [
        {"code": "unequal", "field": field, "source_role": source_role}
    ]


@pytest.mark.parametrize("index", range(9))
@pytest.mark.parametrize("bad_value", [None, "", "   ", 0, False, [], {}])
def test_identical_invalid_values_never_compare_equal(contract, index, bad_value):
    candidate, receipt, envelope = _triplet(contract)
    field = _equalities(contract)[index]
    source_role = (
        "operating_envelope" if field.startswith("operating_") else "release_receipt"
    )
    source = envelope if field.startswith("operating_") else receipt
    candidate[field] = source[field] = bad_value

    result = check_monitoring_bindings(contract, candidate, receipt, envelope)

    assert result["status"] == "binding-inconsistent"
    assert {item["source_role"] for item in result["diagnostics"]} == {
        "candidate",
        source_role,
    }
    assert all(
        item["field"] == field and item["code"] != "unequal"
        for item in result["diagnostics"]
    )


@pytest.mark.parametrize("index", range(9))
@pytest.mark.parametrize("missing_role", [0, 1, 2])
def test_missing_fields_are_reported_without_default_equality(
    contract, index, missing_role
):
    candidate, receipt, envelope = _triplet(contract)
    field = _equalities(contract)[index]
    source_role = (
        "operating_envelope" if field.startswith("operating_") else "release_receipt"
    )
    source = envelope if field.startswith("operating_") else receipt
    removed_roles = []
    if missing_role in (0, 2):
        del candidate[field]
        removed_roles.append("candidate")
    if missing_role in (1, 2):
        del source[field]
        removed_roles.append(source_role)

    result = check_monitoring_bindings(contract, candidate, receipt, envelope)

    assert result["status"] == "binding-inconsistent"
    assert result["diagnostics"] == sorted(
        [
            {"code": "missing", "field": field, "source_role": role}
            for role in removed_roles
        ],
        key=lambda item: (item["source_role"], item["field"], item["code"]),
    )


@pytest.mark.parametrize("role_index", range(3))
@pytest.mark.parametrize("bad_record", [None, [], "synthetic", False])
def test_non_mapping_projection_fails_closed(contract, role_index, bad_record):
    records = list(_triplet(contract))
    records[role_index] = bad_record

    result = check_monitoring_bindings(contract, *records)

    assert result["status"] == "binding-inconsistent"
    assert result["diagnostics"] == [
        {
            "code": "invalid_mapping",
            "field": "record",
            "source_role": ["candidate", "release_receipt", "operating_envelope"][
                role_index
            ],
        }
    ]


@pytest.mark.parametrize("role_index", range(3))
@pytest.mark.parametrize(
    "schema",
    [None, "", "upstream/v1", "riser-monitoring-preflight/v2", 1, False, [], {}],
)
def test_unknown_or_invalid_interface_version_fails_closed(
    contract, role_index, schema
):
    records = list(_triplet(contract))
    records[role_index]["schema_version"] = schema

    result = check_monitoring_bindings(contract, *records)

    assert result["status"] == "binding-inconsistent"
    assert result["diagnostics"] == [
        {
            "code": "unsupported_schema_version",
            "field": "schema_version",
            "source_role": ["candidate", "release_receipt", "operating_envelope"][
                role_index
            ],
        }
    ]


@pytest.mark.parametrize("role_index", range(3))
def test_missing_interface_version_fails_closed(contract, role_index):
    records = list(_triplet(contract))
    del records[role_index]["schema_version"]
    result = check_monitoring_bindings(contract, *records)
    assert result["status"] == "binding-inconsistent"
    assert result["diagnostics"][0]["field"] == "schema_version"


@pytest.mark.parametrize(
    "commit",
    [
        "main",
        "refs/heads/main",
        "refs/tags/v1",
        "a" * 7,
        "a" * 39,
        "a" * 41,
        "G" * 40,
        "A" * 40,
        "a" * 40 + "\n",
    ],
)
def test_mutable_or_malformed_commit_is_rejected_even_when_equal(contract, commit):
    candidate, receipt, envelope = _triplet(contract)
    candidate["HF_commit_sha"] = receipt["HF_commit_sha"] = commit
    result = check_monitoring_bindings(contract, candidate, receipt, envelope)
    assert result["status"] == "binding-inconsistent"
    assert result["diagnostics"] == [
        {"code": "malformed_commit", "field": "HF_commit_sha", "source_role": role}
        for role in ("candidate", "release_receipt")
    ]


@pytest.mark.parametrize("index", range(7))
@pytest.mark.parametrize(
    "digest",
    ["a" * 63, "a" * 65, "A" * 64, "g" * 64, "sha256:" + "a" * 64, "a" * 64 + "\n"],
)
def test_every_digest_field_requires_canonical_sha256(contract, index, digest):
    candidate, receipt, envelope = _triplet(contract)
    digest_fields = [
        field for field in _equalities(contract) if field.endswith("_sha256")
    ]
    assert len(digest_fields) == 7
    field = digest_fields[index]
    source = envelope if field.startswith("operating_") else receipt
    candidate[field] = source[field] = digest
    result = check_monitoring_bindings(contract, candidate, receipt, envelope)
    assert result["status"] == "binding-inconsistent"
    assert len(result["diagnostics"]) == 2
    assert all(item["code"] == "malformed_sha256" for item in result["diagnostics"])


@pytest.mark.parametrize(
    "fields", [None, [], {}, [None], ["unknown"], ["HF_commit_sha", "HF_commit_sha"]]
)
def test_malformed_contract_equalities_cannot_produce_consistency(contract, fields):
    malformed = copy.deepcopy(contract)
    malformed["non_blocking_consumer_graph"]["required_baseline_equalities"] = fields
    result = check_monitoring_bindings(malformed, *_triplet(contract))
    assert result["status"] == "binding-inconsistent"
    assert result["required_equalities"] == []
    assert result["diagnostics"] == [
        {
            "code": "invalid_contract",
            "field": "required_baseline_equalities",
            "source_role": "contract",
        }
    ]


@pytest.mark.parametrize(
    "malformed",
    [
        None,
        [],
        {},
        {"non_blocking_consumer_graph": None},
        {"non_blocking_consumer_graph": {}},
    ],
)
def test_missing_contract_structure_fails_closed(contract, malformed):
    result = check_monitoring_bindings(malformed, *_triplet(contract))
    assert result["status"] == "binding-inconsistent"
    assert result["diagnostics"][0]["code"] == "invalid_contract"


def test_diagnostics_are_value_free_and_identical_across_order_and_repeat_calls(
    contract,
):
    candidate, receipt, envelope = _triplet(contract)
    candidate["logical_release_sha256"] = "b" * 64
    envelope["operating_envelope_version"] = "synthetic-envelope-v2"
    result = check_monitoring_bindings(contract, candidate, receipt, envelope)
    reordered_contract = copy.deepcopy(contract)
    _equalities(reordered_contract).reverse()
    reversed_records = [
        dict(reversed(list(record.items())))
        for record in (candidate, receipt, envelope)
    ]
    again = check_monitoring_bindings(reordered_contract, *reversed_records)
    assert _canonical(result) == _canonical(again)
    assert result == check_monitoring_bindings(contract, candidate, receipt, envelope)
    assert result["diagnostics"] == sorted(
        result["diagnostics"],
        key=lambda item: (item["source_role"], item["field"], item["code"]),
    )
    assert all(
        value not in _canonical(result)
        for record in (candidate, receipt, envelope)
        for field, value in record.items()
        if field != "schema_version"
    )


def test_read_only_mappings_and_caller_data_are_preserved(contract):
    records = _triplet(contract)
    before = copy.deepcopy((contract, records))
    result = check_monitoring_bindings(
        MappingProxyType(contract), *(MappingProxyType(record) for record in records)
    )
    assert result["status"] == "binding-consistent"
    assert (contract, records) == before


def test_preflight_performs_no_file_network_process_or_solver_access(
    contract, monkeypatch
):
    records = _triplet(contract)

    def forbidden(*args, **kwargs):
        pytest.fail("preflight attempted external access")

    with monkeypatch.context() as context:
        context.setattr(Path, "open", forbidden)
        context.setattr(os, "open", forbidden)
        context.setattr(socket, "socket", forbidden)
        context.setattr(subprocess, "Popen", forbidden)
        context.setattr(builtins, "open", forbidden)
        context.setattr(builtins, "__import__", forbidden)
        result = check_monitoring_bindings(contract, *records)
    assert result["status"] == "binding-consistent"
