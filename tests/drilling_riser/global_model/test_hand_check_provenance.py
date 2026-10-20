# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Provenance evidence is distinct from standards compliance and numeric results."""

from __future__ import annotations

import ast
import json
from pathlib import Path

import pytest

from digitalmodel.drilling_riser.global_model import hand_checks
from digitalmodel.drilling_riser.global_model.provenance import hand_check_provenance


def test_inventory_covers_every_hand_check_function():
    """The expected inventory is read independently from the implementation AST."""
    tree = ast.parse(Path(hand_checks.__file__).read_text(encoding="utf-8"))
    expected = {node.name for node in tree.body if isinstance(node, ast.FunctionDef)}
    assert set(hand_check_provenance()["methods"]) == expected


def test_missing_standard_metadata_cannot_claim_compliance():
    sidecar = hand_check_provenance()
    assert sidecar["standards_status"] == "NOT_EVALUATED"
    assert sidecar["standards_compliance_established"] is False
    assert sidecar["citations"] == []
    assert {ref["code_id"] for ref in sidecar["standards_context"]} == {
        "API-RP-16Q", "ISO-13624-1",
    }
    for ref in sidecar["standards_context"]:
        assert ref["edition"] is None
        assert ref["clause_title"] is None
        assert ref["status"] == "UNVERIFIED"
    for method in sidecar["methods"].values():
        assert method["basis"] == "implementation_identity"
        assert method["standards_status"] == "NOT_EVALUATED"
        assert method["equation"]
        assert method["limitations"]
        assert method["source"]


def test_sidecar_is_serializable_and_does_not_share_mutable_state():
    sidecar = hand_check_provenance()
    assert json.loads(json.dumps(sidecar)) == sidecar
    sidecar["methods"].clear()
    sidecar["standards_context"][0]["status"] = "VERIFIED"
    assert hand_check_provenance()["methods"]
    assert hand_check_provenance()["standards_context"][0]["status"] == "UNVERIFIED"


@pytest.mark.parametrize("name", ["hang_off.py", "events.py", "stackup.py"])
def test_context_references_resolve_to_repository_files(name):
    """Standards names are context references, not a standards-to-formula mapping."""
    root = Path(__file__).resolve().parents[3]
    paths = hand_check_provenance()["context_paths"]
    assert any(Path(path).name == name and (root / path).is_file() for path in paths)


def test_beam_reference_identifies_source_and_reproducible_derivation():
    reference = hand_check_provenance()["independent_references"]["clamped_beam"]
    assert reference["year"] == "2004"
    assert reference["locator"] == "Lecture 2, page 15"
    assert reference["url"].startswith("https://ocw.mit.edu/")
    assert reference["characteristic_equation"] == "cosh(b) * cos(b) = 1"
    assert reference["derivation_status"] == "derived_from_published_ODE"
    assert "test_stiff_end_springs_approach_clamped_beam" in reference["test"]
