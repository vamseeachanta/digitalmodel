"""Controlled authority protocol tests, not engineering evidence or real approvals.

Native eligibility is simulated in isolated test state to exercise the positive
branch. No fixture or verification authority from this module is published.
Explicitly labelled synthetic evidence must still refuse engineering lookup.
"""
import copy
import json

import pytest

from tests.ansys.test_analysis_evidence import intake
from tests.ansys.test_analysis_lookup import query
from digitalmodel.ansys.analysis_evidence import build_package, digest_bytes
from digitalmodel.ansys.analysis_lookup import lookup, create_ledger, append_withdrawal


@pytest.fixture
def protocol(intake, tmp_path):
    study, resolver = intake
    case = study["cases"][0]
    case.update(source_kind="native", use_rights="approved",
                capture_role="qualified_native", engineering_qualified=True,
                campaign_assessment_status="PASS")
    case["responses"][0]["limitations"] = []
    for role in ["input", "log", "native", "source"]:
        path = tmp_path / role
        path.write_text("isolated protocol fixture: " + role)
        resolver[role] = path
        case["evidence"].append({"id": role, "sha256": digest_bytes(path.read_bytes()),
                                 "role": role, "required": True})
    review = {"id": "source", "sha256": digest_bytes(resolver["source"].read_bytes())}
    study["review_sources"] = [review]
    study["criteria_reference"] = review
    preliminary = build_package(study, resolver)
    entry = {"checker": "protocol-checker", "competence": "test-only", "authority": "test",
        "date": "2026-09-13", "row_hash": preliminary["cases"][0]["row_hash"],
        "intended_uses": ["screening"], "expected": "12.5", "absolute_tolerance": "0",
        "criterion_id": "isolated-protocol-test", "reference_evidence": [review],
        "required_roles": [], "allowed_limitations": [],
        "checks": {k: "passed" for k in ("source_rights", "equilibrium",
                                        "numerical_quality", "independent_method")}}
    register = tmp_path / "register.json"
    register.write_text(json.dumps({"entries": {"case-1:stress": entry}}))
    ref = {"id": "register", "sha256": digest_bytes(register.read_bytes())}
    resolver["register"] = register
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    ledger, head = create_ledger("dataset-1")
    authority = {"dataset_id": "dataset-1", "head": head, "review_sources": [review],
        "criteria_revision": package["criteria_revision"], "verification_reference": ref,
        "blocking_findings": False, "criteria_reference": review, "finding_dispositions": {},
        "revisions": {"r1": package["package_hash"]}}
    return study, resolver, package, ledger, authority


def test_positive_protocol_branch_and_withdrawal(protocol):
    _, resolver, package, ledger, authority = protocol
    answer = lookup(package, query(), resolver, ledger=ledger, authority=authority)
    assert answer["qualified"] and answer["mode"] == "exact"
    ledger, head = append_withdrawal(ledger, authority["head"], revision="r1",
        case_id="case-1", response_name="stress", reason="test withdrawal",
        authority="test authority", evidence_id="test finding", timestamp="2026-09-13")
    authority["head"] = head
    with pytest.raises(ValueError, match="withdrawn"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


@pytest.mark.parametrize("identity", ["capture", "input", "log", "native", "source", "register"])
def test_every_required_file_is_checked_at_lookup(protocol, identity):
    _, resolver, package, ledger, authority = protocol
    resolver[identity].write_text("changed")
    with pytest.raises(ValueError):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


@pytest.mark.parametrize("damage", ["same_author", "wrong_row", "negative_tolerance",
    "not_verified", "wrong_use", "no_reference", "precision", "wrong_expected"])
def test_verification_gate_refusals(protocol, damage):
    study, resolver, _, ledger, authority = protocol
    register = json.loads(resolver["register"].read_text())
    entry = register["entries"]["case-1:stress"]
    changes = {"same_author": ("checker", "fixture-author"), "wrong_row": ("row_hash", "wrong"),
        "negative_tolerance": ("absolute_tolerance", "-1"),
        "not_verified": ("checks", {}), "wrong_use": ("intended_uses", []),
        "no_reference": ("reference_evidence", []),
        "precision": ("expected", "12.50000000000000000000000000001"),
        "wrong_expected": ("expected", "13")}
    key, value = changes[damage]
    entry[key] = value
    resolver["register"].write_text(json.dumps(register))
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    with pytest.raises(ValueError):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_optional_but_response_referenced_file_cannot_escape_check(protocol):
    study, resolver, _, ledger, authority = protocol
    study["cases"][0]["evidence"][0]["required"] = False
    package = build_package(study, resolver)
    authority["revisions"] = {"r1": package["package_hash"]}
    resolver["capture"].unlink()
    with pytest.raises(ValueError, match="unavailable"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_synthetic_refuses_even_with_complete_authority(protocol):
    study, resolver, _, ledger, authority = protocol
    study["cases"][0]["source_kind"] = "synthetic"
    package = build_package(study, resolver)
    authority["revisions"] = {"r1": package["package_hash"]}
    with pytest.raises(ValueError, match="synthetic"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_comparison_does_not_round_a_difference_down_to_tolerance(protocol):
    study, resolver, _, ledger, authority = protocol
    register = json.loads(resolver["register"].read_text())
    entry = register["entries"]["case-1:stress"]
    entry.update(expected="-0.00000000000000000000000000001",
                 absolute_tolerance="12.5")
    resolver["register"].write_text(json.dumps(register))
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    with pytest.raises(ValueError, match="comparison"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_duplicate_register_keys_refuse_even_with_matching_hash(protocol):
    study, resolver, _, ledger, authority = protocol
    text = resolver["register"].read_text().replace('"expected": "12.5"',
                                                '"expected": "99", "expected": "12.5"')
    resolver["register"].write_text(text)
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    with pytest.raises(ValueError, match="duplicate"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_null_blocking_findings_does_not_mean_clear(protocol):
    _, resolver, package, ledger, authority = protocol
    authority["blocking_findings"] = None
    with pytest.raises(ValueError):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_criteria_revision_label_alone_cannot_establish_current_criteria(protocol):
    _, resolver, package, ledger, authority = protocol
    authority.pop("criteria_reference")
    with pytest.raises(ValueError, match="criteria"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_string_intended_uses_cannot_match_by_substring(protocol):
    study, resolver, _, ledger, authority = protocol
    register = json.loads(resolver["register"].read_text())
    register["entries"]["case-1:stress"]["intended_uses"] = "non-screening"
    resolver["register"].write_text(json.dumps(register))
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    with pytest.raises(ValueError):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)
