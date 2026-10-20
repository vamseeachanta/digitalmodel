"""Offline criteria protocol: invented numbers test comparisons, not design limits.

Independent arithmetic: 12.5 <= 13 passes, 12.5 <= 12 fails; 12.5 >= 12
passes. All limits are explicit test inputs, with no standards-derived constants.
Source contract: ENG-ANALYSIS-CRITERIA Revision 1 (2026), AC-01, AC-07–08.
"""
import copy

import pytest

from digitalmodel.ansys.analysis_criteria import evaluate_criteria
from digitalmodel.ansys.analysis_records import digest_bytes


@pytest.fixture
def criteria(tmp_path):
    source = tmp_path / "basis.txt"
    source.write_text("Synthetic comparison basis, revision 1; not an engineering limit.")
    ref = {"id": "basis", "sha256": digest_bytes(source.read_bytes())}
    item = {"criterion_id": "test-only", "quantity": "test scalar", "unit": "MPa",
            "observed": "12.5", "limit": "13", "comparator": "le",
            "source": "Synthetic test basis, revision 1, comparison example",
            "evidence": [ref]}
    return {name: copy.deepcopy(item) for name in
            ("equilibrium", "convergence", "strength")}, {"basis": source}


def test_separate_criteria_and_no_mutation(criteria):
    matrix, resolver = criteria
    original = copy.deepcopy(matrix)
    matrix["strength"]["limit"] = "12"
    result = evaluate_criteria(matrix, resolver)
    assert result == {"equilibrium": "PASS", "convergence": "PASS", "strength": "FAIL"}
    matrix["strength"]["limit"] = "13"
    assert matrix == original


@pytest.mark.parametrize("observed,limit,comparator,status", [
    ("0", "0", "le", "PASS"), ("12.5", "12.5", "le", "PASS"), ("12.5", "12", "ge", "PASS"),
    ("12.5", "13", "ge", "FAIL"),
    ("12.50000000000000000000000000001", "12.5", "le", "FAIL"),
    (None, "13", "le", "NOT_EVALUATED"), ("12.5", None, "le", "NOT_EVALUATED"),
])
def test_explicit_comparisons(criteria, observed, limit, comparator, status):
    matrix, resolver = criteria
    matrix["strength"].update(observed=observed, limit=limit, comparator=comparator)
    assert evaluate_criteria(matrix, resolver)["strength"] == status


def test_missing_criterion_stays_not_evaluated(criteria):
    matrix, resolver = criteria
    del matrix["strength"]
    assert evaluate_criteria(matrix, resolver)["strength"] == "NOT_EVALUATED"
    assert set(evaluate_criteria({}, {}).values()) == {"NOT_EVALUATED"}


@pytest.mark.parametrize("field,value", [
    ("observed", "NaN"), ("limit", 13), ("comparator", "lt"),
    ("source", ""), ("evidence", []), ("unit", ""), ("criterion_id", None),
])
def test_malformed_criterion_refuses(criteria, field, value):
    matrix, resolver = criteria
    matrix["strength"][field] = value
    with pytest.raises(ValueError):
        evaluate_criteria(matrix, resolver)


def test_evidence_rechecked_even_for_missing_value(criteria):
    matrix, resolver = criteria
    matrix["strength"]["observed"] = None
    resolver["basis"].write_text("changed source")
    with pytest.raises(ValueError, match="changed"):
        evaluate_criteria(matrix, resolver)


def test_unknown_checks_and_untyped_matrix_refuse(criteria):
    matrix, resolver = criteria
    for invalid in ([], None, {**matrix, "strenght": matrix["strength"]}):
        with pytest.raises(ValueError):
            evaluate_criteria(invalid, resolver)


def test_demand_only_retains_explicit_unobserved_strength(criteria):
    from digitalmodel.ansys.analysis_criteria import qualify_response_criteria

    matrix, resolver = criteria
    matrix["strength"]["observed"] = None
    entry = {"acceptance_criteria": matrix,
             "acceptance_requirements": {"demand-only": ["equilibrium", "convergence"]}}
    response = {"definition": "test scalar", "unit": "MPa", "value": "12.5"}
    result = qualify_response_criteria(entry, response, "demand-only", resolver)
    assert result["strength"] == "NOT_EVALUATED"
