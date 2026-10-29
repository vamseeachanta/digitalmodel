# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Retained-input checks with independent inventory and mutation expectations.

Source: 2026-09-13 issue-2121-canary-proposal.json, cases and budget;
cylinder_preparation.py, prepare_inputs/_artifacts, define the saved format.
Hand count: proposal + basis/criteria + (deck + metadata) for each of the
four declared cases = 2 + 2*4 = 10 artifacts, plus the preparation receipt.
These are input-integrity tests; no numerical response is manufactured.
"""

import hashlib
import json
import shutil
import stat
from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.ansys import cylinder_preparation_audit
from digitalmodel.ansys.cylinder_preparation import prepare_inputs
from digitalmodel.ansys.cylinder_preparation_audit import audit_inputs as _audit_inputs

PROPOSAL = (
    Path(__file__).resolve().parents[2]
    / "docs/plans/evidence/2026-09-13-issue-2121-canary-proposal.json"
)
CASES = (
    "ocv-zero-t60-n16",
    "ocv-t60-p10-n4",
    "ocv-t60-p10-n8",
    "ocv-t60-p10-n16",
)
ARTIFACTS = {"proposal.json", "basis-criteria.json"} | {
    case + suffix for case in CASES for suffix in (".inp", ".json")
}


def audit_inputs(root):
    return _audit_inputs(root, json.loads(PROPOSAL.read_bytes()))


@pytest.fixture(scope="module")
def prepared(tmp_path_factory):
    root = tmp_path_factory.mktemp("source") / "inputs"
    prepare_inputs(root, json.loads(PROPOSAL.read_bytes()))
    return root


@pytest.fixture
def bundle(tmp_path, prepared):
    root = tmp_path / "inputs"
    shutil.copytree(prepared, root)
    return root


def receipt(root):
    return json.loads((root / "preparation.json").read_bytes())


def save_receipt(root, value):
    (root / "preparation.json").write_text(
        json.dumps(value, sort_keys=True, separators=(",", ":")), encoding="utf-8"
    )


def repin(root, name, raw):
    """Simulate tampering with both an artifact and its self-declared digest."""
    (root / name).write_bytes(raw)
    value = receipt(root)
    row = next(row for row in value["files"] if row["path"] == name)
    row.update(bytes=len(raw), sha256=hashlib.sha256(raw).hexdigest())
    save_receipt(root, value)


def snapshot(root):
    return {path.name: path.read_bytes() for path in root.iterdir()}


def test_complete_retained_inputs_are_read_only_and_unqualified(bundle):
    before = snapshot(bundle)
    result = audit_inputs(bundle)
    assert result["status"] == "VERIFIED_INPUTS_ONLY"
    assert result["case_order"] == list(CASES)
    assert {row["path"] for row in result["files"]} == ARTIFACTS
    assert len(result["files"]) == 10  # Independent hand count in module docstring.
    for row in result["files"]:
        raw = before[row["path"]]
        assert row["bytes"] == len(raw)
        assert row["sha256"] == hashlib.sha256(raw).hexdigest()
    assert result["preparation_sha256"] == hashlib.sha256(
        before["preparation.json"]
    ).hexdigest()
    assert result["native_execution"] is False
    assert result["engineering_qualified"] is False
    assert result["reference_agreement"] == "NOT_EVALUATED"
    assert result["execution_readiness"] == "NOT_EVALUATED"
    assert snapshot(bundle) == before


@pytest.mark.parametrize("name", sorted(ARTIFACTS | {"preparation.json"}))
def test_every_missing_member_is_refused(bundle, name):
    # Move the synthetic member aside; retained evidence is never deleted.
    (bundle / name).rename(bundle.parent / name)
    with pytest.raises(ValueError):
        audit_inputs(bundle)


@pytest.mark.parametrize("kind", ["file", "directory", "failure_receipt"])
def test_additional_members_are_refused(bundle, kind):
    if kind == "directory":
        (bundle / "extra").mkdir()
    else:
        name = "preparation-failure.json" if kind == "failure_receipt" else "extra"
        (bundle / name).write_bytes(b"unreviewed")
    with pytest.raises(ValueError):
        audit_inputs(bundle)


@pytest.mark.parametrize("rehash", [False, True])
@pytest.mark.parametrize("name", ["ocv-t60-p10-n4.inp", "ocv-t60-p10-n8.json",
                                  "basis-criteria.json", "proposal.json"])
def test_changed_artifact_cannot_pass_by_rehashing(bundle, name, rehash):
    raw = (bundle / name).read_bytes() + b"\n"
    if rehash:
        repin(bundle, name, raw)
    else:
        (bundle / name).write_bytes(raw)
    with pytest.raises(ValueError):
        audit_inputs(bundle)


@pytest.mark.parametrize("mutation", ["radial_restraint", "pressure_removed",
                                     "station_order"])
def test_rehashed_physical_input_changes_are_refused(bundle, mutation):
    """Proposal basis/restraints forbids UX constraints; pressure and stations freeze.

    Each mutation violates a named proposal requirement. Expected refusal is
    independent of the generator: a new digest cannot authorize changed physics.
    """
    name = "ocv-t60-p10-n4.inp"
    raw = (bundle / name).read_bytes()
    if mutation == "radial_restraint":
        raw += b"D,1,UX,0\n"  # Deliberate forbidden restraint, not a design input.
    elif mutation == "pressure_removed":
        lines = raw.splitlines(keepends=True)
        raw = b"".join(line for line in lines if not line.startswith(b"SFE,"))
    else:
        name = "ocv-t60-p10-n4.json"
        data = json.loads((bundle / name).read_bytes())
        data["stations"].reverse()
        raw = json.dumps(data, sort_keys=True, separators=(",", ":")).encode()
    assert raw != (bundle / name).read_bytes()
    repin(bundle, name, raw)
    with pytest.raises(ValueError):
        audit_inputs(bundle)


@pytest.mark.parametrize("field,value", [
    ("status", "ready"), ("native_execution", True),
    ("native_execution", 0), ("independent_reference_established", True),
    ("independent_reference_established", 0), ("schema", "unknown"),
])
def test_receipt_cannot_assert_readiness_or_coerce_booleans(bundle, field, value):
    data = receipt(bundle)
    data[field] = value
    save_receipt(bundle, data)
    with pytest.raises(ValueError):
        audit_inputs(bundle)


@pytest.mark.parametrize("mutation", ["duplicate", "omission", "reorder", "escape",
                                     "wrong_size", "boolean_size", "wrong_digest",
                                     "extra_field", "provenance", "case_order"])
def test_receipt_inventory_and_provenance_are_exact(bundle, mutation):
    data = receipt(bundle)
    if mutation == "duplicate":
        data["files"].append(data["files"][0])
    elif mutation == "omission":
        data["files"].pop()
    elif mutation == "reorder":
        data["files"].reverse()
    elif mutation == "escape":
        data["files"][0]["path"] = "../outside.json"
    elif mutation == "wrong_size":
        data["files"][0]["bytes"] += 1
    elif mutation == "boolean_size":
        data["files"][0]["bytes"] = True
    elif mutation == "wrong_digest":
        data["files"][0]["sha256"] = "unbound"
    elif mutation == "extra_field":
        data["extra"] = "unreviewed"
    elif mutation == "provenance":
        data["provenance"]["proposal_canonical_sha256"] = "unbound"
    else:
        data["case_order"].reverse()
    save_receipt(bundle, data)
    with pytest.raises(ValueError):
        audit_inputs(bundle)


@pytest.mark.parametrize("name", ["preparation.json", "proposal.json"])
@pytest.mark.parametrize("mutation", ["duplicate_key", "bom", "nonfinite", "list"])
def test_nonconforming_json_is_refused(bundle, name, mutation):
    raw = (bundle / name).read_bytes()
    if mutation == "duplicate_key":
        raw = b'{"extra":true,"extra":false,' + raw[1:]
    elif mutation == "bom":
        raw = b"\xef\xbb\xbf" + raw
    elif mutation == "nonfinite":
        raw = b'{"extra":NaN,' + raw[1:]
    else:
        raw = b"[]"
    (bundle / name).write_bytes(raw)
    with pytest.raises(ValueError):
        audit_inputs(bundle)


@pytest.mark.parametrize("kind", ["root", "parent", "artifact", "receipt", "hardlink"])
def test_redirected_or_shared_inputs_are_refused(bundle, kind):
    if kind in ("root", "parent"):
        alias = bundle.parent / "alias"
        alias.symlink_to(bundle if kind == "root" else bundle.parent,
                         target_is_directory=True)
        target = alias if kind == "root" else alias / bundle.name
    else:
        name = "preparation.json" if kind == "receipt" else "proposal.json"
        path = bundle / name
        original = bundle.parent / name
        path.rename(original)
        if kind == "hardlink":
            import os
            os.link(original, path)
        else:
            path.symlink_to(original)
        target = bundle
    with pytest.raises(ValueError):
        audit_inputs(target)


@pytest.mark.parametrize("kind", ["reparse", "nonregular"])
def test_redirected_or_nonregular_member_is_refused_before_opening(
    bundle, monkeypatch, kind
):
    member = bundle / "proposal.json"
    original_lstat = Path.lstat
    monkeypatch.setattr(stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0x400, raising=False)

    def member_lstat(path, *args, **kwargs):
        info = original_lstat(path, *args, **kwargs)
        if path == member:
            assert info.st_nlink == 1
            return SimpleNamespace(
                st_mode=info.st_mode if kind == "reparse" else stat.S_IFIFO,
                st_nlink=info.st_nlink,
                st_file_attributes=(
                    stat.FILE_ATTRIBUTE_REPARSE_POINT if kind == "reparse" else 0
                ),
            )
        return info

    def forbidden_open(*args, **kwargs):
        pytest.fail("Redirected or nonregular members must be refused before opening")

    monkeypatch.setattr(Path, "lstat", member_lstat)
    monkeypatch.setattr(cylinder_preparation_audit.os, "open", forbidden_open)
    with pytest.raises(ValueError, match="Preparation member is redirected"):
        audit_inputs(bundle)


def test_missing_root_is_not_created(tmp_path):
    target = tmp_path / "absent"
    with pytest.raises(ValueError):
        audit_inputs(target)
    assert not target.exists()


def test_changed_required_proposal_is_refused_before_reading(bundle, monkeypatch):
    proposal = json.loads(PROPOSAL.read_bytes())
    proposal["basis"]["restraints"] = "unreviewed"

    def forbidden(*args, **kwargs):
        pytest.fail("Invalid proposal must be refused before filesystem access")

    monkeypatch.setattr(Path, "lstat", forbidden)
    with pytest.raises(ValueError):
        _audit_inputs(bundle, proposal)


def test_equivalent_proposal_decimals_normalize(bundle):
    proposal = json.loads(PROPOSAL.read_bytes())
    # Decimal spelling only; values are the cited proposal's case/basis inputs.
    proposal["basis"]["wall_thickness_mm"] += ".000"
    for case in proposal["cases"]:
        case["pressure_mpa"] += ".000"
    assert _audit_inputs(bundle, proposal) == audit_inputs(bundle)
