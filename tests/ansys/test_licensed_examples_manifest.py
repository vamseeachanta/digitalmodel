# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline preparation contracts; no APDL parsing or solver invocation."""

import copy
import os
from pathlib import Path
import re
import subprocess

import pytest

from digitalmodel.infrastructure.input_provenance import (
    load_bounded_json,
    verify_payload_inventory,
)

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "examples/ansys/licensed-examples-manifest.json"
EXPECTED = {
    "padeye": ("examples/ansys/padeye/input.yml", "examples/ansys/padeye/padeye.inp"),
    "pressure-vessel": (
        "examples/ansys/pressure-vessel/input.yml",
        "examples/ansys/pressure-vessel/pv.inp",
    ),
    "mudmat": ("examples/ansys/mudmat/input.yml", "examples/ansys/mudmat/mudmat.inp"),
}


def _load():
    # Offline parser budgets only; these are not solver resource bounds.
    return load_bounded_json(
        MANIFEST.read_bytes(), max_bytes=16384, max_nodes=128, max_depth=5,
    )


def _blob(path):
    # Do not let a caller's repository/index/object bindings select other bytes.
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("GIT_")}
    return subprocess.run(
        ["git", "cat-file", "blob", f"HEAD:{path}"], cwd=ROOT, env=env,
        check=True, capture_output=True,
    ).stdout


def _inventory(manifest):
    """Validate the preparation schema, independent of the manifest inventory."""
    if type(manifest) is not dict or set(manifest) != {
        "schema_version", "workflow_id", "status", "digest_convention",
        "source_revision", "cases",
    }:
        raise ValueError("Invalid manifest fields")
    if type(manifest["schema_version"]) is not int or manifest["schema_version"] != 1:
        raise ValueError("Unknown schema version")
    if (manifest["workflow_id"] != "ansys-mechanical-solve"
            or manifest["status"] != "preparation-only"
            or manifest["digest_convention"] != "sha256-raw-git-blob"
            or type(manifest["source_revision"]) is not str
            or not re.fullmatch(r"[0-9a-f]{40}", manifest["source_revision"])):
        raise ValueError("Invalid preparation metadata")
    cases = manifest["cases"]
    if type(cases) is not list or len(cases) != len(EXPECTED):
        raise ValueError("Exactly three cases required")
    seen, pins, sizes, edges = set(), {}, {}, {}
    for case in cases:
        if type(case) is not dict or set(case) != {"case_id", "files"}:
            raise ValueError("Invalid case fields")
        identity = case["case_id"]
        if type(identity) is not str or identity not in EXPECTED or identity in seen:
            raise ValueError("Unknown or duplicate case")
        seen.add(identity)
        files = case["files"]
        if type(files) is not list or len(files) != 2:
            raise ValueError("One input and one companion required")
        roles = {}
        for item in files:
            if type(item) is not dict or set(item) != {"role", "path", "bytes", "sha256"}:
                raise ValueError("Invalid file fields")
            role, path = item["role"], item["path"]
            if type(role) is not str or role not in {"input", "companion"} or role in roles:
                raise ValueError("Invalid or duplicate role")
            # A strict lexical grammar preserves empty/dot segments for refusal.
            if (type(path) is not str
                    or not re.fullmatch(r"[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*", path)
                    or any(part in {".", ".."} for part in path.split("/"))
                    or path in pins):
                raise ValueError("Noncanonical or duplicate path")
            expected_path = EXPECTED[identity][0 if role == "input" else 1]
            if path != expected_path:
                raise ValueError("Unmanifested companion or input")
            if type(item["bytes"]) is not int or item["bytes"] <= 0:
                raise ValueError("Invalid byte count")
            if type(item["sha256"]) is not str or not re.fullmatch(r"[0-9a-f]{64}", item["sha256"]):
                raise ValueError("Invalid digest")
            roles[role] = path
            pins[path], sizes[path] = item["sha256"], item["bytes"]
        edges[roles["input"]] = [roles["companion"]]
        edges[roles["companion"]] = []
    return pins, sizes, edges


def _verify(manifest, payloads):
    pins, sizes, edges = _inventory(manifest)
    receipt = verify_payload_inventory(payloads, pins, edges)
    if any(len(payloads[path]) != size for path, size in sizes.items()):
        raise ValueError("Byte count mismatch")
    return receipt


@pytest.fixture
def manifest():
    return _load()


@pytest.fixture
def payloads():
    return {path: _blob(path) for pair in EXPECTED.values() for path in pair}


def test_three_pairs_match_head_blobs_and_preserve_trust_limits(manifest, payloads):
    receipt = _verify(manifest, payloads)
    assert receipt["artifact_count"] == 6
    assert receipt["reference_count"] == 3
    assert receipt["integrity"] == receipt["references"] == "VERIFIED"
    assert receipt["engineering_qualification"] == "NOT_EVALUATED"
    assert receipt["source_rights"] == "NOT_EVALUATED"


def test_git_repository_bindings_cannot_redirect_blob_reads(monkeypatch):
    expected = _blob(EXPECTED["padeye"][0])
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_OBJECT_DIRECTORY"):
        monkeypatch.setenv(name, "nonexistent-synthetic-repository")
    assert _blob(EXPECTED["padeye"][0]) == expected


@pytest.mark.parametrize("path", [
    "/absolute.inp", "C:/case/deck.inp", "C:deck.inp", "//server/share/deck.inp",
    r"case\deck.inp", "", ".", "..", "../deck.inp", "case/../deck.inp",
    "case/./deck.inp", "case//deck.inp", "case/deck.inp/", "case/deck.inp\n",
    "examples/ansys/padeye/unreviewed.inp",
])
def test_noncanonical_or_unreviewed_paths_refuse(manifest, path):
    manifest["cases"][0]["files"][1]["path"] = path
    with pytest.raises(ValueError):
        _inventory(manifest)


@pytest.mark.parametrize("fault", [
    "version", "boolean_version", "status", "workflow", "convention", "revision",
    "missing_case", "extra_case", "duplicate_case", "missing_companion",
    "extra_companion", "duplicate_role", "duplicate_path", "extra_field",
    "bad_digest", "uppercase_digest", "short_digest", "zero_bytes", "boolean_bytes",
])
def test_schema_and_inventory_mutations_refuse(manifest, fault):
    case = manifest["cases"][0]
    item = case["files"][1]
    metadata = {
        "version": ("schema_version", 2), "boolean_version": ("schema_version", True),
        "status": ("status", "ready"), "workflow": ("workflow_id", "other"),
        "convention": ("digest_convention", "normalized-text"),
        "revision": ("source_revision", "unknown"),
    }
    if fault in metadata:
        key, value = metadata[fault]
        manifest[key] = value
    elif fault == "missing_case":
        manifest["cases"].pop()
    elif fault == "extra_case":
        manifest["cases"].append(copy.deepcopy(case))
    elif fault == "duplicate_case":
        manifest["cases"][1] = copy.deepcopy(case)
    elif fault == "missing_companion":
        case["files"].pop()
    elif fault == "extra_companion":
        case["files"].append(copy.deepcopy(item))
    elif fault == "duplicate_role":
        item["role"] = "input"
    elif fault == "duplicate_path":
        item["path"] = case["files"][0]["path"]
    elif fault == "extra_field":
        item["unexpected"] = True
    else:
        key, value = {
            "bad_digest": ("sha256", "invalid"),
            "uppercase_digest": ("sha256", "A" * 64),
            "short_digest": ("sha256", "a" * 63),
            "zero_bytes": ("bytes", 0), "boolean_bytes": ("bytes", True),
        }[fault]
        item[key] = value
    with pytest.raises(ValueError):
        _inventory(manifest)


@pytest.mark.parametrize("fault", ["bytes", "digest", "count", "missing", "extra", "newline"])
def test_payload_mutations_refuse(manifest, payloads, fault):
    item = manifest["cases"][1]["files"][1]
    path = item["path"]
    if fault == "bytes":
        payloads[path] += b"synthetic mutation"
    elif fault == "digest":
        item["sha256"] = "0" * 64
    elif fault == "count":
        item["bytes"] += 1
    elif fault == "missing":
        payloads.pop(path)
    elif fault == "extra":
        payloads["unreviewed.inp"] = b"synthetic"
    else:
        assert b"\r\n" in payloads[path]
        payloads[path] = payloads[path].replace(b"\r\n", b"\n")
    with pytest.raises(ValueError):
        _verify(manifest, payloads)
