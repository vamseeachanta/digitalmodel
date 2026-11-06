# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for the explicit analysis operators."""

from decimal import Context, Decimal, Inexact, ROUND_DOWN, localcontext
from fractions import Fraction
import json

import pytest

from digitalmodel.infrastructure.analysis_criteria import Criterion, Quantity


@pytest.mark.parametrize("response", ["-2", "2"])
def test_absolute_residual_refuses_both_signs(response):
    """Synthetic hand calculation: |-2 - 0| = |2 - 0| = 2 > 1 kN.

    Definition: NIST DLMF 1.2.8 (2026), section 3.1(v), eq. 3.1.8.
    The selected limit is a test input, not an engineering acceptance limit.
    """
    criterion = Criterion(
        operator="absolute_residual",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("1", "kN"),
        source="Test basis: hand calculation above",
    )
    result = criterion.evaluate(Quantity(response, "kN"), Quantity("0", "kN"))
    assert result.status == "FAIL"
    assert result.metric == Fraction(2)
    assert result.unit == "kN"


@pytest.mark.parametrize(
    "comparator,response,expected",
    [
        ("<=", "-2", "PASS"),
        ("<=", "1", "PASS"),
        ("<", "1", "FAIL"),
        (">=", "1", "PASS"),
        (">", "1", "FAIL"),
        (">", "2", "PASS"),
    ],
)
def test_signed_value_preserves_sign_and_comparator(comparator, response, expected):
    """Hand comparisons to 1 kN: -2 <= 1; 1 <= 1; 1 !< 1; 2 > 1.

    Signed-value means the identity q, with no magnitude operation. Limits and
    responses are synthetic inputs; the explicit comparator determines equality.
    """
    criterion = Criterion(
        operator="signed_value",
        comparator=comparator,
        response_unit="kN",
        limit=Quantity("1", "kN"),
        source="Test basis: signed comparisons above",
    )
    result = criterion.evaluate(Quantity(response, "kN"))
    assert result.status == expected
    assert result.metric == Fraction(response)


@pytest.mark.parametrize("value", [1.0, True, "NaN", "Infinity", "sNaN", "bad"])
def test_quantity_refuses_inexact_or_nonfinite_inputs(value):
    """Invalid values are type/finite-value probes, not engineering inputs."""
    with pytest.raises(ValueError):
        Quantity(value, "kN")


@pytest.mark.parametrize(
    "changes",
    [
        {"operator": "implicit"},
        {"comparator": "=="},
        {"source": " "},
        {"response_unit": ""},
        {"limit": Quantity("1", "N")},
        {"limit": Quantity("-1", "kN")},
    ],
)
def test_invalid_criterion_is_refused_before_comparison(changes):
    """A magnitude limit cannot be negative; unit aliases are not conversions."""
    definition = dict(
        operator="absolute_residual",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("1", "kN"),
        source="Test basis: required metadata",
    )
    with pytest.raises(ValueError):
        Criterion(**(definition | changes))


@pytest.mark.parametrize(
    "value,reference",
    [
        (None, Quantity("0", "kN")),
        (Quantity("1", "kN"), None),
        (Quantity("1", "N"), Quantity("0", "kN")),
        (Quantity("1", "kN"), Quantity("0", "N")),
    ],
)
def test_missing_or_incompatible_evidence_is_not_evaluated(value, reference):
    """Unknown evidence is distinct from the true-zero reference in the first test."""
    criterion = Criterion(
        operator="absolute_residual",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("1", "kN"),
        source="Test basis: missing evidence probes",
    )
    result = criterion.evaluate(value, reference)
    assert result.status == "NOT_EVALUATED"
    assert result.metric is None
    assert result.reason


def test_signed_value_refuses_an_ignored_reference():
    """A reference cannot silently turn a signed-value check into a residual."""
    criterion = Criterion(
        operator="signed_value",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("1", "kN"),
        source="Test basis: explicit operator",
    )
    with pytest.raises(ValueError, match="reference"):
        criterion.evaluate(Quantity("2", "kN"), Quantity("2", "kN"))


@pytest.mark.parametrize(
    "value,reference,expected_metric,expected_status",
    [
        ("12", "10", Fraction(1, 5), "PASS"),
        ("8", "10", Fraction(1, 5), "PASS"),
        ("-12", "-10", Fraction(1, 5), "PASS"),
        ("-8", "-10", Fraction(1, 5), "PASS"),
        ("13", "10", Fraction(3, 10), "FAIL"),
        ("7", "10", Fraction(3, 10), "FAIL"),
    ],
)
def test_relative_error_uses_absolute_reference(
    value, reference, expected_metric, expected_status
):
    """Hand calculations: |12-10|/|10| = |8-10|/|10| = 2/10 = 1/5.

    Negative references give the same magnitudes; |13-10|/10 = |7-10|/10
    = 3/10 > 0.2. Definition: NIST DLMF 1.2.8 (2026), eq. 3.1.9.
    The dimensionless 0.2 limit is a synthetic test input.
    """
    criterion = Criterion(
        operator="relative_error",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("0.2", "1"),
        source="Test basis: relative hand calculation",
    )
    result = criterion.evaluate(Quantity(value, "kN"), Quantity(reference, "kN"))
    assert result.status == expected_status
    assert result.metric == expected_metric
    assert result.unit == "1"


@pytest.mark.parametrize("response", ["0", "1", "-1"])
def test_zero_reference_without_floor_is_not_evaluated(response):
    """Relative error divides by |reference|; division by zero is undefined.

    NIST DLMF 1.2.8 (2026), eq. 3.1.9 explicitly requires a nonzero reference,
    including when the observed value is also zero. Inputs are synthetic.
    """
    criterion = Criterion(
        operator="relative_error",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("0.2", "1"),
        source="Test basis: zero-reference refusal",
    )
    result = criterion.evaluate(Quantity(response, "kN"), Quantity("0", "kN"))
    assert result.status == "NOT_EVALUATED"
    assert result.metric is None
    assert result.reason == "Zero reference requires an explicit denominator floor"


@pytest.mark.parametrize(
    "value,reference,metric,status",
    [
        ("0.1", "0", Fraction(1, 10), "PASS"),
        ("-0.1", "0", Fraction(1, 10), "PASS"),
        ("0.3", "0", Fraction(3, 10), "FAIL"),
        ("-0.3", "0", Fraction(3, 10), "FAIL"),
        ("0.11", "0.01", Fraction(1, 10), "PASS"),
        ("-0.11", "-0.01", Fraction(1, 10), "PASS"),
        ("12", "10", Fraction(1, 5), "PASS"),
    ],
)
def test_dimensioned_floor_is_an_explicit_denominator_policy(
    value, reference, metric, status
):
    """Hand calculation with synthetic floor 1 kN and relative limit 0.2.

    e = |q-r|/max(|r|,1): |0.1-0|/1 = 1/10; |0.3-0|/1 = 3/10;
    |0.11-0.01|/1 = 1/10; |12-10|/10 = 1/5. Negative pairs are symmetric.
    The max policy is an explicit extension of DLMF eq. 3.1.9, not a NIST limit.
    """
    criterion = Criterion(
        operator="relative_error",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("0.2", "1"),
        source="Test basis: dimensionless limit",
        reference_floor=Quantity("1", "kN"),
        floor_source="Test basis: synthetic denominator floor, hand calculation above",
    )
    result = criterion.evaluate(Quantity(value, "kN"), Quantity(reference, "kN"))
    assert (result.metric, result.status) == (metric, status)


@pytest.mark.parametrize(
    "changes",
    [
        {"reference_floor": Quantity("0", "kN")},
        {"reference_floor": Quantity("-1", "kN")},
        {"reference_floor": Quantity("1", "1")},
        {"floor_source": None},
        {"floor_source": " "},
        {"reference_floor": None},
    ],
)
def test_denominator_floor_requires_positive_dimensioned_value_and_source(changes):
    """Synthetic validation probes; a floor and its provenance must occur together."""
    definition = dict(
        operator="relative_error",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("0.2", "1"),
        source="Test basis: relative limit",
        reference_floor=Quantity("1", "kN"),
        floor_source="Test basis: floor",
    )
    with pytest.raises(ValueError):
        Criterion(**(definition | changes))


@pytest.mark.parametrize("operator", ["signed_value", "absolute_residual"])
def test_nonrelative_operators_refuse_unused_floor(operator):
    """A denominator policy must not silently enter a dimensioned comparison."""
    with pytest.raises(ValueError, match="relative_error"):
        Criterion(
            operator=operator,
            comparator="<=",
            response_unit="kN",
            limit=Quantity("1", "kN"),
            source="Test basis: magnitude limit",
            reference_floor=Quantity("1", "kN"),
            floor_source="Test basis: unused",
        )


def test_relative_record_retains_exact_ratio_inputs_and_comparator_provenance():
    """Hand calculation: |4-3|/max(3,1) = 1/3 > the finite decimal 0.333.

    NIST DLMF 1.2.8 (2026), eq. 3.1.9. All numbers are synthetic inputs.
    A repeating ratio remains a rational pair in JSON, without rounded equality.
    """
    criterion = Criterion(
        operator="relative_error",
        comparator="<",
        response_unit="kN",
        limit=Quantity("0.333", "1"),
        source="Test basis: strict comparison to 0.333",
        reference_floor=Quantity("1", "kN"),
        floor_source="Test basis: floor 1 kN",
    )
    result = criterion.evaluate(Quantity("4", "kN"), Quantity("3", "kN"))
    record = result.to_record()
    assert json.loads(json.dumps(record, allow_nan=False)) == record
    assert record == {
        "status": "FAIL",
        "metric": {"numerator": "1", "denominator": "3"},
        "unit": "1",
        "reason": None,
        "value": {"value": "4", "unit": "kN"},
        "reference": {"value": "3", "unit": "kN"},
        "criterion": {
            "operator": "relative_error",
            "comparator": "<",
            "response_unit": "kN",
            "limit": {"value": "0.333", "unit": "1"},
            "source": "Test basis: strict comparison to 0.333",
            "reference_floor": {"value": "1", "unit": "kN"},
            "floor_source": "Test basis: floor 1 kN",
        },
    }


def test_missing_record_cannot_masquerade_as_true_zero():
    """Undefined 0/0 is absent, while a true-zero dimensional residual is zero."""
    definition = dict(comparator="<=", response_unit="kN", source="Test basis: zero")
    relative = Criterion(
        operator="relative_error", limit=Quantity("0.2", "1"), **definition
    )
    absolute = Criterion(
        operator="absolute_residual", limit=Quantity("1", "kN"), **definition
    )
    zero = Quantity("0", "kN")
    assert relative.evaluate(zero, zero).to_record()["metric"] is None
    assert absolute.evaluate(zero, zero).to_record()["metric"] == {
        "numerator": "0",
        "denominator": "1",
    }


def test_comparison_is_exact_under_hostile_decimal_context():
    """Hand calculation: 1/3 exceeds any finite string of decimal threes.

    No approximate epsilon is used. DLMF eq. 3.1.9 supplies the relative metric;
    the long finite decimal is a synthetic boundary probe, not an acceptance limit.
    """
    criterion = Criterion(
        operator="relative_error",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("0." + "3" * 80, "1"),
        source="Test basis: exact boundary",
    )
    with localcontext(Context(prec=6, rounding=ROUND_DOWN)) as context:
        context.traps[Inexact] = True
        result = criterion.evaluate(Quantity(Decimal("4"), "kN"), Quantity("3", "kN"))
        assert result.status == "FAIL"
        assert result.metric == Fraction(1, 3)


@pytest.mark.parametrize(
    "changes",
    [
        {"operator": None},
        {"comparator": []},
        {"limit": None},
        {"reference_floor": "1", "floor_source": "Test basis: invalid type"},
    ],
)
def test_malformed_definition_types_raise_value_error(changes):
    """Configuration type errors are refused without implicit defaults."""
    definition = dict(
        operator="relative_error",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("0.2", "1"),
        source="Test basis: invalid type probes",
    )
    with pytest.raises(ValueError):
        Criterion(**(definition | changes))


@pytest.mark.parametrize("unit", ["", " ", " kN", "kN ", None])
def test_quantity_requires_an_exact_nonempty_unit(unit):
    """Unit spelling is evidence; whitespace is not silently normalized."""
    with pytest.raises(ValueError):
        Quantity("1", unit)


@pytest.mark.parametrize(
    "operator", ["signed_value", "absolute_residual", "relative_error"]
)
def test_evidence_requires_quantity_objects(operator):
    """Malformed API evidence is refused rather than treated as a number."""
    criterion = Criterion(
        operator=operator,
        comparator="<=",
        response_unit="kN",
        limit=Quantity("1", "1" if operator == "relative_error" else "kN"),
        source="Test basis: invalid evidence type",
    )
    with pytest.raises(ValueError):
        criterion.evaluate("1")


@pytest.mark.parametrize("reference", ["0.000001", "-0.000001"])
def test_nonzero_near_zero_reference_has_no_implicit_floor(reference):
    """Hand calculation: |0 - (+/- 0.000001)| / |+/- 0.000001| = 1.

    NIST DLMF 1.2.8 (2026), eq. 3.1.9. The synthetic limit 0.2 is exceeded;
    the reference is nonzero and needs no special cutoff to be evaluated.
    """
    criterion = Criterion(
        operator="relative_error",
        comparator="<=",
        response_unit="kN",
        limit=Quantity("0.2", "1"),
        source="Test basis: near-zero reference",
    )
    result = criterion.evaluate(Quantity("0", "kN"), Quantity(reference, "kN"))
    assert result.status == "FAIL"
    assert result.metric == Fraction(1)


@pytest.mark.parametrize("response", ["-1", "1"])
@pytest.mark.parametrize("comparator,expected", [("<=", "PASS"), ("<", "FAIL")])
def test_absolute_boundary_is_explicit_for_both_signs(response, comparator, expected):
    """Hand calculation: |+/-1-0| = 1; equality passes <= and fails <."""
    criterion = Criterion(
        operator="absolute_residual",
        comparator=comparator,
        response_unit="kN",
        limit=Quantity("1", "kN"),
        source="Test basis: exact boundary",
    )
    assert (
        criterion.evaluate(Quantity(response, "kN"), Quantity("0", "kN")).status
        == expected
    )
