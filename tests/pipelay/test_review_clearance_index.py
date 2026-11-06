# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Traceability checks for the review index specified by issue #2767.

Expected gate membership comes from agent-pack.md section 5 and audit #2745
section 12. These are document identifiers, not engineering coefficients.
No numerical method or acceptance limit is introduced by this index.
"""

import ast
import json
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
INDEX = ROOT / "docs/registry/pipelay-review-clearance.json"
PACK = ROOT / "src/digitalmodel/pipelay/docs/agent-pack.md"


@pytest.fixture(scope="module")
def index():
    return json.loads(INDEX.read_text(encoding="utf-8"))


def resolve_file(relative):
    path = (ROOT / relative).resolve()
    assert path.is_relative_to(ROOT)
    assert path.is_file(), relative
    return path


def resolve_function(reference):
    relative, name = reference.split("::")
    tree = ast.parse(resolve_file(relative).read_text(encoding="utf-8"))
    return next(
        node
        for node in tree.body
        if isinstance(node, ast.FunctionDef) and node.name == name
    )


def test_index_is_discoverable_from_pack_and_shared_docs(index):
    pack = resolve_file(index["pack"])
    assert pack == PACK
    assert "../../../../docs/registry/pipelay-review-clearance.json" in pack.read_text()
    assert (
        "registry/pipelay-review-clearance.json"
        in (ROOT / "docs/README.md").read_text()
    )
    for source in index["repository_sources"]:
        resolve_file(source)
    assert index["tracking_issue"] == "#2767"
    assert index["pack_status"] == "DRAFT"


def test_responsibilities_do_not_claim_assignments(index):
    assert set(index["responsibilities"]) == {
        "maintainer",
        "independent_reviewer",
        "alternate_reviewer",
    }
    for role in index["responsibilities"].values():
        assert role["assignment_status"] == "NOT_EVALUATED"
        assert role["assignment_evidence"] is None
        assert role["duties"]
    assert "independent" in " ".join(
        index["responsibilities"]["alternate_reviewer"]["duties"]
    )


@pytest.mark.parametrize(
    "check, gate",
    [
        ("C01", "G0"),
        ("C02", "G0"),
        ("C03", "G2"),
        ("C04", "G2"),
        ("C05", "G2"),
        ("C06", "G5"),
        ("C07", "G5"),
    ],
)
def test_gate_evidence_resolves_to_functions_and_executable_assertions(
    index, check, gate
):
    record = index["checks"][check]
    assert record["gate"] == gate
    assert record["evidence_scope"] == "offline_regression"
    assert record["assertion"]
    assert record["outputs"]
    assert record["functions"]
    for reference in record["functions"]:
        resolve_function(reference)
    assert record["tests"]
    for reference in record["tests"]:
        test = resolve_function(reference)
        assert test.name.startswith("test_")
        assert any(
            isinstance(node, ast.Assert)
            or (isinstance(node, ast.Attribute) and node.attr == "raises")
            for node in ast.walk(test)
        ), reference
    assert record["execution_status"] == "NOT_EVALUATED"
    assert record["execution_evidence"] is None


def test_lessons_link_pack_findings_to_regression_prevention(index):
    assert set(index["checks"]) == {"C01", "C02", "C03", "C04", "C05", "C06", "C07"}
    lessons = index["lessons"]
    assert {lesson["id"] for lesson in lessons} == {
        "pipelay-screening-scope",
        "pipelay-independent-anchors",
    }
    for lesson in lessons:
        assert lesson["id"] in PACK.read_text(encoding="utf-8")
        assert lesson["finding"] and lesson["prevention"]
        assert lesson["checks"]
        assert set(lesson["checks"]) <= index["checks"].keys()
        assert lesson["source_issue"] in {"#2656", "#2745"}


def test_clearance_remains_unevaluated_and_requires_revision_bound_evidence(index):
    assert set(index["clearance"]) == {"pack_review", "native_installation"}
    for record in index["clearance"].values():
        assert record["status"] == "NOT_EVALUATED"
        assert record["evidence"] == []
        assert record["required_evidence"]
    requirements = index["clearance"]["pack_review"]["required_evidence"]
    for field in (
        "revision",
        "assignments",
        "independent_review",
        "check_results",
        "disposition",
    ):
        assert field in requirements
    assert "not installation clearance" in index["clearance"]["pack_review"]["scope"]
    native = index["clearance"]["native_installation"]
    assert native["tracking_issue"] == "#2431"
    assert native["offline_regression_sufficient"] is False
    assert {
        "authorized_scope",
        "solver_evidence",
        "criteria",
        "independent_review",
    } <= native["required_evidence"].keys()
