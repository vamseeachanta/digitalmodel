"""Withdrawal, freshness and exact-key refusal tests: no solver invocation."""
import copy
import json

import pytest

from tests.ansys.test_analysis_evidence import intake
from digitalmodel.ansys.analysis_evidence import build_package, digest_bytes
from digitalmodel.ansys.analysis_lookup import (
    create_ledger, append_withdrawal, lookup, validate_ledger,
)


def query():
    return {"dataset_id": "dataset-1", "revision": "r1", "analysis_id": "analysis-1",
            "component_id": "plate", "model_revision": "model-1",
            "parameters": {"thickness_mm": "10"}, "response_name": "stress",
            "definition": "test scalar", "location": "fixture", "unit": "MPa",
            "intended_use": "screening"}


def test_diagnostic_returns_labelled_value_but_synthetic_cannot_qualify(intake):
    package = build_package(*intake)
    answer = lookup(package, query(), intake[1], diagnostic=True)
    assert answer["value"] == "12.5"
    assert answer["source_kind"] == "synthetic"
    assert not answer["qualified"]
    with pytest.raises(ValueError):
        lookup(package, query(), intake[1])


@pytest.mark.parametrize("field,value", [("revision", "r2"), ("unit", "Pa"),
    ("definition", "different stress"), ("component_id", "vessel"),
    ("model_revision", "model-2"), ("intended_use", "design")])
def test_query_requires_exact_identity(intake, field, value):
    q = query()
    q[field] = value
    with pytest.raises(ValueError):
        lookup(build_package(*intake), q, intake[1], diagnostic=True)


def test_offgrid_never_snaps_to_a_match(intake):
    q = query()
    q["parameters"] = {"thickness_mm": "10.00001"}
    with pytest.raises(ValueError, match="match"):
        lookup(build_package(*intake), q, intake[1], diagnostic=True)


def test_ledger_withdrawal_and_external_head_detect_rollback():
    ledger, head = create_ledger("dataset-1")
    updated, new_head = append_withdrawal(ledger, head, revision="r1", case_id="case-1",
        response_name="stress", reason="new finding", authority="owner-record-1",
        evidence_id="review-2", timestamp="2026-09-13T01:00:00Z")
    updated, new_head = append_withdrawal(updated, new_head, revision="r1", case_id="case-2",
        response_name="stress", reason="second finding", authority="owner-record-1",
        evidence_id="review-3", timestamp="2026-09-13T02:00:00Z")
    validate_ledger(updated, new_head)
    for bad in [ledger, {**updated, "entries": list(reversed(updated["entries"]))}]:
        if bad == updated:
            continue
        with pytest.raises(ValueError):
            validate_ledger(bad, new_head)
    with pytest.raises(ValueError):
        validate_ledger(updated, head)
    corrupt = copy.deepcopy(updated)
    corrupt["entries"][0]["reason"] = "erased"
    with pytest.raises(ValueError):
        validate_ledger(corrupt, new_head)


def test_missing_authority_and_changed_review_refuse_engineering(intake, tmp_path):
    study, resolver = intake
    study["cases"][0]["source_kind"] = "native"
    package = build_package(study, resolver)
    with pytest.raises(ValueError, match="authority"):
        lookup(package, query(), resolver)
    source = tmp_path / "review.txt"
    source.write_text("review-1")
    ledger, head = create_ledger("dataset-1")
    authority = {"dataset_id": "dataset-1", "head": head,
                 "review_sources": [{"id": "review", "sha256": digest_bytes(source.read_bytes())}],
                 "criteria_revision": package["criteria_revision"], "verification": {}}
    source.write_text("review-2")
    resolver["review"] = source
    criteria = {"id": "capture", "sha256": digest_bytes(resolver["capture"].read_bytes())}
    study["criteria_reference"] = criteria
    package = build_package(study, resolver)
    authority["criteria_reference"] = criteria
    with pytest.raises(ValueError, match="changed"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)
