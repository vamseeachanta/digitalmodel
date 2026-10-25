"""Engineering lookup must require separate sourced acceptance criteria."""
import json

import pytest

from digitalmodel.ansys.analysis_evidence import build_package, digest_bytes
from digitalmodel.ansys.analysis_lookup import lookup
from tests.ansys.test_analysis_evidence import intake as intake
from tests.ansys.test_analysis_lookup import query
from tests.ansys.test_analysis_qualification import protocol as protocol

__all__ = ["intake", "protocol"]


def rebind(protocol, criteria):
    study, resolver, _, ledger, authority = protocol
    register = json.loads(resolver["register"].read_bytes())
    entry = register["entries"]["case-1:stress"]
    entry.pop("acceptance_criteria", None)
    if criteria is not None:
        entry["acceptance_criteria"] = criteria
    resolver["register"].write_text(json.dumps(register), encoding="utf-8", newline="\n")
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    return resolver, package, ledger, authority


@pytest.mark.parametrize("criteria", [None, {}])
def test_legacy_register_cannot_imply_strength_or_convergence(protocol, criteria):
    resolver, package, ledger, authority = rebind(protocol, criteria)
    with pytest.raises(ValueError, match="acceptance criteria"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)
    assert lookup(package, query(), resolver, diagnostic=True)["qualified"] is False


def test_demand_readiness_does_not_claim_unresolved_capacity(protocol):
    study, resolver, _, _, _ = protocol
    register = json.loads(resolver["register"].read_bytes())
    matrix = register["entries"]["case-1:stress"]["acceptance_criteria"]
    del matrix["strength"]
    resolver, package, ledger, authority = rebind(protocol, matrix)
    register = json.loads(resolver["register"].read_bytes())
    register["entries"]["case-1:stress"]["acceptance_requirements"] = {
        "screening": ["equilibrium", "convergence"]}
    resolver["register"].write_text(json.dumps(register), encoding="utf-8", newline="\n")
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    answer = lookup(package, query(), resolver, ledger=ledger, authority=authority)
    assert answer["qualified"] is True
    assert {name: item["status"] for name, item in answer["acceptance_results"].items()} == {
        "equilibrium": "PASS", "convergence": "PASS", "strength": "NOT_EVALUATED"}
    assert all(value is None for key, value in answer["acceptance_results"]["strength"].items()
               if key != "status")


@pytest.mark.parametrize("field,value", [
    ("unit", "Pa"), ("quantity", "another response"), ("observed", "1"),
    ("limit", "12"), ("limit", None),
])
def test_strength_binding_and_acceptance_refusals(protocol, field, value):
    """12.5 exceeds the explicit test limit 12; a missing limit is unresolved."""
    resolver = protocol[1]
    register = json.loads(resolver["register"].read_bytes())
    matrix = register["entries"]["case-1:stress"]["acceptance_criteria"]
    matrix["strength"][field] = value
    resolver, package, ledger, authority = rebind(protocol, matrix)
    with pytest.raises(ValueError, match="acceptance criteria"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_success_returns_separate_acceptance_results(protocol):
    _, resolver, package, ledger, authority = protocol
    answer = lookup(package, query(), resolver, ledger=ledger, authority=authority)
    assert {name: item["status"] for name, item in answer["acceptance_results"].items()} == {
        "equilibrium": "PASS", "convergence": "PASS", "strength": "PASS"}


def test_diagnostic_lookup_drops_embedded_acceptance_results(protocol):
    study, resolver, _, _, _ = protocol
    study["cases"][0]["responses"][0]["acceptance_results"] = {"strength": "PASS"}
    package = build_package(study, resolver)
    answer = lookup(package, query(), resolver, diagnostic=True)
    assert answer["qualified"] is False
    assert "acceptance_results" not in answer
    assert package["cases"][0]["responses"][0]["acceptance_results"] == {"strength": "PASS"}


def test_acceptance_results_carry_the_evaluated_basis(protocol):
    _, resolver, package, ledger, authority = protocol
    answer = lookup(package, query(), resolver, ledger=ledger, authority=authority)
    register = json.loads(resolver["register"].read_bytes())
    matrix = register["entries"]["case-1:stress"]["acceptance_criteria"]
    for name, criterion in matrix.items():
        assert answer["acceptance_results"][name] == {**criterion, "status": "PASS"}


@pytest.mark.parametrize("source", [
    "TBD", "n/a", "-", "unknown", "x",
    {"document": "Synthetic protocol basis", "edition": "Revision 1", "clause": "TBD"},
])
def test_lookup_refuses_unsourced_acceptance(protocol, source):
    resolver = protocol[1]
    register = json.loads(resolver["register"].read_bytes())
    matrix = register["entries"]["case-1:stress"]["acceptance_criteria"]
    matrix["strength"]["source"] = source
    resolver, package, ledger, authority = rebind(protocol, matrix)
    with pytest.raises(ValueError, match="source"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


def test_negative_strength_response_cannot_pass_absolute_limit(protocol):
    """Hand calculation: |-300| = 300 MPa exceeds the synthetic 250 MPa limit."""
    study, resolver, _, _, _ = protocol
    study["cases"][0]["responses"][0]["value"] = "-300"
    preliminary = build_package(study, resolver)
    register = json.loads(resolver["register"].read_bytes())
    entry = register["entries"]["case-1:stress"]
    entry.update(expected="-300", row_hash=preliminary["cases"][0]["row_hash"])
    matrix = entry["acceptance_criteria"]
    matrix["strength"].update(observed="-300", limit="250", comparator="abs_le")
    resolver["register"].write_text(json.dumps(register), encoding="utf-8", newline="\n")
    resolver, package, ledger, authority = rebind(protocol, matrix)
    with pytest.raises(ValueError, match="acceptance criteria incomplete or failed"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)


@pytest.mark.parametrize("requirements", [None, {}, {"screening": "strength"},
    {"screening": []}, {"screening": ["strength"]},
    {"screening": ["equilibrium", "convergence", "strenght"]},
    {"screening": ["equilibrium", "convergence", "convergence"]},
    {"screening": ["equilibrium", "convergence", {}]}])
def test_requirements_cannot_be_absent_or_weaken_numerical_checks(protocol, requirements):
    study, resolver, _, ledger, authority = protocol
    register = json.loads(resolver["register"].read_bytes())
    register["entries"]["case-1:stress"]["acceptance_requirements"] = requirements
    resolver["register"].write_text(json.dumps(register), encoding="utf-8", newline="\n")
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    with pytest.raises(ValueError, match="acceptance criteria"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)
