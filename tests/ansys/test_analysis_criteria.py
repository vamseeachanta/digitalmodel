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
    source.write_text("Synthetic comparison basis, revision 1; not an engineering limit.",
                      encoding="utf-8", newline="\n")
    ref = {"id": "basis", "sha256": digest_bytes(source.read_bytes())}
    item = {"criterion_id": "test-only", "quantity": "test scalar", "unit": "MPa",
            "observed": "12.5", "limit": "13", "comparator": "le",
            "source": {"document": "Synthetic comparison basis",
                       "edition": "Revision 1", "clause": "Comparison example"},
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
    ("12.5", "13", "ge", "FAIL"), ("12.5", "12.5", "ge", "PASS"),
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
    resolver["basis"].write_text("changed source", encoding="utf-8", newline="\n")
    with pytest.raises(ValueError, match="changed"):
        evaluate_criteria(matrix, resolver)


def test_unknown_checks_and_untyped_matrix_refuse(criteria):
    matrix, resolver = criteria
    for invalid in ([], None, {**matrix, "strenght": matrix["strength"]}):
        with pytest.raises(ValueError):
            evaluate_criteria(invalid, resolver)


@pytest.mark.parametrize("source", [
    "TBD", "n/a", "-", "unknown", "x",
    "Synthetic test basis, revision 1, comparison example",
])
def test_unstructured_source_refuses(criteria, source):
    matrix, resolver = criteria
    matrix["strength"]["source"] = source
    with pytest.raises(ValueError, match="source"):
        evaluate_criteria(matrix, resolver)


def test_structured_source_accepts(criteria):
    matrix, resolver = criteria
    for item in matrix.values():
        item["source"] = {"document": "Synthetic comparison basis",
                          "edition": "Revision 1", "clause": "Comparison example"}
    assert set(evaluate_criteria(matrix, resolver).values()) == {"PASS"}


@pytest.mark.parametrize("field", ["document", "edition", "clause"])
@pytest.mark.parametrize("value", ["TBD", " N/A ", "-", "UNKNOWN", "x", "", None])
def test_source_fields_refuse_placeholders(criteria, field, value):
    matrix, resolver = criteria
    for item in matrix.values():
        item["source"] = {"document": "Synthetic comparison basis",
                          "edition": "Revision 1", "clause": "Comparison example"}
    matrix["strength"]["source"][field] = value
    with pytest.raises(ValueError, match="source"):
        evaluate_criteria(matrix, resolver)


@pytest.mark.parametrize("source", [
    {}, {"document": "Synthetic comparison basis", "edition": "Revision 1"},
    {"document": "Synthetic comparison basis", "edition": "Revision 1",
     "clause": "Comparison example", "unrecognized": "extra"},
])
def test_source_schema_requires_exact_fields(criteria, source):
    matrix, resolver = criteria
    matrix["strength"]["source"] = source
    with pytest.raises(ValueError, match="source"):
        evaluate_criteria(matrix, resolver)


@pytest.mark.parametrize("extra", [{"lower_limit": "-250"}, {"absolute_value": True}])
def test_unknown_record_keys_refuse(criteria, extra):
    matrix, resolver = criteria
    matrix["strength"].update(extra)
    with pytest.raises(ValueError, match="fields"):
        evaluate_criteria(matrix, resolver)


@pytest.mark.parametrize("observed,limit,status", [
    ("-300", "250", "FAIL"), ("-250", "250", "PASS"),
    ("-249", "250", "PASS"), ("300", "250", "FAIL"),
    ("-250.00000000000000000000000000001", "250", "FAIL"),
])
def test_absolute_comparison_uses_signed_observation(criteria, observed, limit, status):
    """Hand ordering: |-300| = 300 > 250; |-250| = 250; |-249| = 249 < 250."""
    matrix, resolver = criteria
    matrix["strength"].update(observed=observed, limit=limit, comparator="abs_le")
    assert evaluate_criteria(matrix, resolver)["strength"] == status


def test_absolute_comparison_refuses_negative_limit(criteria):
    matrix, resolver = criteria
    matrix["strength"].update(observed=None, limit="-250", comparator="abs_le")
    with pytest.raises(ValueError, match="negative"):
        evaluate_criteria(matrix, resolver)


def test_demand_only_refuses_supplied_strength_failure(criteria):
    from digitalmodel.ansys.analysis_criteria import qualify_response_criteria

    matrix, resolver = criteria
    matrix["strength"]["limit"] = "12"
    entry = {"acceptance_criteria": matrix,
             "acceptance_requirements": {"demand-only": ["equilibrium", "convergence"]}}
    response = {"definition": "test scalar", "unit": "MPa", "value": "12.5"}
    with pytest.raises(ValueError, match="acceptance criteria"):
        qualify_response_criteria(entry, response, "demand-only", resolver)


def test_null_response_value_raises_validation_error(criteria):
    from digitalmodel.ansys.analysis_criteria import qualify_response_criteria

    matrix, resolver = criteria
    entry = {"acceptance_criteria": matrix,
             "acceptance_requirements": {"strength-use": list(matrix)}}
    response = {"definition": "test scalar", "unit": "MPa", "value": None}
    with pytest.raises(ValueError, match="response"):
        qualify_response_criteria(entry, response, "strength-use", resolver)


def test_demand_only_retains_explicit_unobserved_strength(criteria):
    from digitalmodel.ansys.analysis_criteria import qualify_response_criteria

    matrix, resolver = criteria
    matrix["strength"]["observed"] = None
    entry = {"acceptance_criteria": matrix,
             "acceptance_requirements": {"demand-only": ["equilibrium", "convergence"]}}
    response = {"definition": "test scalar", "unit": "MPa", "value": "12.5"}
    result = qualify_response_criteria(entry, response, "demand-only", resolver)
    assert result["strength"]["status"] == "NOT_EVALUATED"
