"""Engineering lookup must require separate sourced acceptance criteria."""
import json

import pytest

from digitalmodel.ansys.analysis_evidence import build_package, digest_bytes
from digitalmodel.ansys.analysis_lookup import lookup
from tests.ansys.test_analysis_evidence import intake as intake
from tests.ansys.test_analysis_lookup import query
from tests.ansys.test_analysis_qualification import protocol as protocol


def rebind(protocol, criteria):
    study, resolver, _, ledger, authority = protocol
    register = json.loads(resolver["register"].read_bytes())
    entry = register["entries"]["case-1:stress"]
    entry.pop("acceptance_criteria", None)
    if criteria is not None:
        entry["acceptance_criteria"] = criteria
    resolver["register"].write_text(json.dumps(register))
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
    resolver["register"].write_text(json.dumps(register))
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    answer = lookup(package, query(), resolver, ledger=ledger, authority=authority)
    assert answer["qualified"] is True
    assert answer["acceptance_results"] == {
        "equilibrium": "PASS", "convergence": "PASS", "strength": "NOT_EVALUATED"}


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
    assert answer["acceptance_results"] == {
        "equilibrium": "PASS", "convergence": "PASS", "strength": "PASS"}


@pytest.mark.parametrize("requirements", [None, {}, {"screening": "strength"},
    {"screening": []}, {"screening": ["strength"]},
    {"screening": ["equilibrium", "convergence", "strenght"]},
    {"screening": ["equilibrium", "convergence", "convergence"]},
    {"screening": ["equilibrium", "convergence", {}]}])
def test_requirements_cannot_be_absent_or_weaken_numerical_checks(protocol, requirements):
    study, resolver, _, ledger, authority = protocol
    register = json.loads(resolver["register"].read_bytes())
    register["entries"]["case-1:stress"]["acceptance_requirements"] = requirements
    resolver["register"].write_text(json.dumps(register))
    ref = {"id": "register", "sha256": digest_bytes(resolver["register"].read_bytes())}
    study["verification_reference"] = ref
    package = build_package(study, resolver)
    authority.update(verification_reference=ref, revisions={"r1": package["package_hash"]})
    with pytest.raises(ValueError, match="acceptance criteria"):
        lookup(package, query(), resolver, ledger=ledger, authority=authority)
