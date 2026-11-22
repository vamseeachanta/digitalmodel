"""Synthetic evidence fixtures; no native retrieval or engineering limit is implied.

Fixture basis: one station at (10, 20) in mm and response SX in MPa.
Coordinates are hand-chosen exact decimals. A displacement of 0.001 mm is
inside the illustrative 0.01 mm coordinate tolerance; 0.02 mm is outside it.
All thresholds are test inputs, not recommended acceptance limits.
"""

from dataclasses import replace
from decimal import Inexact, ROUND_UP, localcontext
from fractions import Fraction

import pytest

from digitalmodel.infrastructure.solver_evidence import (
    EvidenceIdentity,
    ExpectedRecord,
    GateStatus,
    PrecisionPolicy,
    RetrievalEnvelope,
    RetrievalState,
    RetrievedRecord,
    check_export_precision,
    check_retrieval,
    parse_decimal_field,
)


IDENTITY = EvidenceIdentity("case-a", "set-a", "element-nodal", "global", "MPa", "mm")
KEY = ("station-a", "SX")
EXPECTED = (ExpectedRecord(KEY, ("10", "20")),)
RECORD = RetrievedRecord(KEY, ("10", "20"), "0.000E+0", RetrievalState.VALID)
ENVELOPE = RetrievalEnvelope(IDENTITY, (RECORD,))
BASIS = "Synthetic hand calculation in this test module; no engineering use"


def retrieval(envelope=ENVELOPE, **kwargs):
    return check_retrieval(
        envelope,
        expected_identity=IDENTITY,
        expected_records=EXPECTED,
        coordinate_tolerance="0.01",
        criteria_source=BASIS,
        **kwargs,
    )


def test_true_zero_remains_a_valid_value():
    """Hand calculation: exact 0 has value 0, with complete matching identity."""
    result = retrieval()
    assert result.status is GateStatus.PASS
    assert result.values == ((KEY, Fraction(0)),)


@pytest.mark.parametrize(
    "field", ["case_id", "result_set", "recovery_source", "frame", "unit"]
)
def test_identity_mismatch_refuses_usable_values(field):
    result = retrieval(
        replace(ENVELOPE, identity=replace(IDENTITY, **{field: "other"}))
    )
    assert result.status is GateStatus.INVALID_EVIDENCE
    assert not result.values


@pytest.mark.parametrize(
    "records", [(), (RECORD, RECORD), (replace(RECORD, key=("other", "SX")),)]
)
def test_key_count_missing_duplicate_and_unexpected_records_refuse(records):
    result = retrieval(replace(ENVELOPE, records=records))
    assert result.status is GateStatus.INVALID_EVIDENCE
    assert not result.values


@pytest.mark.parametrize(
    "coordinates, passes",
    [
        (("10.001", "20"), True),
        (("10.01", "20"), True),
        (("10.02", "20"), False),
        (("10",), False),
        (("NaN", "20"), False),
    ],
)
def test_coordinate_dimension_and_boundary(coordinates, passes):
    result = retrieval(
        replace(ENVELOPE, records=(replace(RECORD, coordinates=coordinates),))
    )
    assert (result.status is GateStatus.PASS) is passes


@pytest.mark.parametrize(
    "state, value, status",
    [
        (RetrievalState.MISSING, None, GateStatus.NOT_EVALUATED),
        (RetrievalState.MISSING, "0", GateStatus.NOT_EVALUATED),
        (RetrievalState.DEFAULTED, "0", GateStatus.INVALID_EVIDENCE),
        (RetrievalState.INVALID, "0", GateStatus.INVALID_EVIDENCE),
        (RetrievalState.NUMERICAL_FAILURE, "0", GateStatus.NUMERICAL_FAILURE),
        (RetrievalState.VALID, None, GateStatus.NOT_EVALUATED),
        (RetrievalState.VALID, "NaN", GateStatus.INVALID_EVIDENCE),
        (RetrievalState.VALID, float("inf"), GateStatus.INVALID_EVIDENCE),
        ("VALID", "0", GateStatus.INVALID_EVIDENCE),
    ],
)
def test_missing_invalid_defaulted_and_failed_are_never_zero(state, value, status):
    result = retrieval(
        replace(ENVELOPE, records=(replace(RECORD, state=state, value=value),))
    )
    assert result.status is status
    assert not result.values


def test_absent_retrieval_is_not_evaluated():
    assert retrieval(None).status is GateStatus.NOT_EVALUATED


@pytest.mark.parametrize("source", ["", "auto", "automatic", "default"])
def test_policy_refuses_implicit_recovery(source):
    with pytest.raises(ValueError):
        check_retrieval(
            ENVELOPE,
            expected_identity=replace(IDENTITY, recovery_source=source),
            expected_records=EXPECTED,
            coordinate_tolerance="0.01",
            criteria_source=BASIS,
        )


@pytest.mark.parametrize("tolerance", ["-0.01", "NaN", 0.01])
def test_invalid_coordinate_policy_raises(tolerance):
    with pytest.raises(ValueError):
        check_retrieval(
            ENVELOPE,
            expected_identity=IDENTITY,
            expected_records=EXPECTED,
            coordinate_tolerance=tolerance,
            criteria_source=BASIS,
        )


def test_duplicate_expectations_cannot_hide_in_a_map():
    with pytest.raises(ValueError):
        check_retrieval(
            ENVELOPE,
            expected_identity=IDENTITY,
            expected_records=EXPECTED * 2,
            coordinate_tolerance="0.01",
            criteria_source=BASIS,
        )


@pytest.mark.parametrize(
    "text, value, half_quantum",
    [
        ("1.234E+2", Fraction(617, 5), Fraction(1, 20)),
        ("-1.234E+2", Fraction(-617, 5), Fraction(1, 20)),
        ("0.000E+0", Fraction(0), Fraction(1, 2000)),
        ("1.20", Fraction(6, 5), Fraction(1, 200)),
        ("0.1234E+3", Fraction(617, 5), Fraction(1, 20)),
    ],
)
def test_decimal_field_quantum_is_calculated_from_printed_places(
    text, value, half_quantum
):
    """Hand calculation: 1.234E2 = 123.4 = 617/5; last place = 0.1.

    Half a last place is 0.05 = 1/20. Likewise 0.000 has last place
    0.001, half 0.0005 = 1/2000; 1.20 has half 0.005 = 1/200.
    What Every Computer Scientist Should Know About Floating-Point Arithmetic
    (1991), Relative Error and Ulps, supplies the half-ulp bound.
    """
    field = parse_decimal_field(text)
    assert field.value == value
    assert field.half_quantum == half_quantum


@pytest.mark.parametrize(
    "text", ["NaN", "Infinity", "", " 1.2", "1_000", "1D+2", "***", True, 0.0]
)
def test_decimal_parser_rejects_nonliteral_or_nonfinite_values(text):
    with pytest.raises(ValueError):
        parse_decimal_field(text)


POLICY = PrecisionPolicy(
    unit="MPa",
    dimensional_floor="0.1",
    max_half_quantum="0.0005",
    readback_limit="0.002",
    criteria_source=BASIS,
)


def precision(export="1.234E+0", readback="1.234E+0", **changes):
    args = dict(
        export_fields={KEY: export},
        readback_fields={KEY: readback},
        expected_keys=(KEY,),
        policy=POLICY,
        witness_text="1.234E+0",
        witness_expected="1.234",
        witness_max_half_quantum="0.0005",
        export_evidence_id="export-capture",
        readback_evidence_id="listing-capture",
        expected_identity=IDENTITY,
        export_identity=IDENTITY,
        readback_identity=IDENTITY,
    )
    args.update(changes)
    return check_export_precision(**args)


def test_independent_readback_boundary_is_inclusive():
    """Hand calculation: 1.236 - 1.234 = 0.002, equal to the input limit."""
    assert precision(readback="1.236E+0").status is GateStatus.PASS


@pytest.mark.parametrize("value", ["1.237E+0", "1.231E+0"])
def test_readback_disagreement_is_invalid_evidence(value):
    """Hand calculation: absolute differences are both 0.003 > 0.002."""
    assert precision(readback=value).status is GateStatus.INVALID_EVIDENCE


@pytest.mark.parametrize("where", ["export", "readback"])
def test_rounded_zero_cannot_hide_a_dimensional_floor(where):
    """Hand calculation: 0E0 has half quantum 0.5 > budget 0.0005."""
    assert precision(**{where: "0E+0"}).status is GateStatus.INVALID_EVIDENCE


def test_fine_true_zero_passes_precision_gate():
    assert precision(export="0.000E+0", readback="0.000E+0").status is GateStatus.PASS


@pytest.mark.parametrize(
    "changes",
    [
        {"witness_text": None},
        {"readback_fields": None},
        {"export_fields": None},
    ],
)
def test_missing_precision_evidence_is_not_evaluated(changes):
    assert precision(**changes).status is GateStatus.NOT_EVALUATED


@pytest.mark.parametrize(
    "changes",
    [
        {"witness_text": "1.23E+0"},
        {"witness_text": "1.235E+0"},
        {"export_fields": {}},
        {"readback_fields": {}},
        {"readback_fields": {("other", "SX"): "1.234E+0"}},
        {"readback_fields": {KEY: "NaN"}},
    ],
)
def test_bad_witness_or_keyed_representation_refuses(changes):
    assert precision(**changes).status is GateStatus.INVALID_EVIDENCE


@pytest.mark.parametrize(
    "changes",
    [
        {"unit": ""},
        {"criteria_source": ""},
        {"dimensional_floor": "0"},
        {"max_half_quantum": "0.1"},
        {"readback_limit": "0.1"},
        {"readback_limit": "-0.002"},
    ],
)
def test_precision_policy_requires_explicit_subfloor_budgets(changes):
    with pytest.raises(ValueError):
        precision(policy=replace(POLICY, **changes))


def test_arithmetic_is_independent_of_decimal_context():
    """Exact differences and quantum above must survive hostile caller context."""
    with localcontext() as context:
        context.prec = 1
        context.rounding = ROUND_UP
        context.Emin = -1
        context.Emax = 1
        context.traps[Inexact] = True
        assert precision(readback="1.236E+0").status is GateStatus.PASS
        assert parse_decimal_field("1.234E+2").value == Fraction(617, 5)


@pytest.mark.parametrize(
    "identity_field",
    ["case_id", "result_set", "recovery_source", "frame", "unit", "coordinate_unit"],
)
def test_readback_attribution_must_match_the_frozen_identity(identity_field):
    bad_identity = replace(IDENTITY, **{identity_field: "other"})
    result = precision(readback_identity=bad_identity)
    assert result.status is GateStatus.INVALID_EVIDENCE


def test_policy_unit_must_match_the_response_unit():
    assert (
        precision(policy=replace(POLICY, unit="mm")).status
        is GateStatus.INVALID_EVIDENCE
    )


def test_relabeling_the_export_as_readback_is_refused():
    assert (
        precision(readback_evidence_id="export-capture").status
        is GateStatus.INVALID_EVIDENCE
    )


def test_the_same_mapping_cannot_be_its_own_independent_readback():
    fields = {KEY: "1.234E+0"}
    assert (
        precision(export_fields=fields, readback_fields=fields).status
        is GateStatus.INVALID_EVIDENCE
    )


@pytest.mark.parametrize("text", ["1E1001", "1E-1001", "1" * 129])
def test_representation_resource_bounds_refuse(text):
    """Bounds are software policy in solver-evidence-gates.html rev 0."""
    with pytest.raises(ValueError):
        parse_decimal_field(text)


def test_expected_keys_cannot_repeat():
    with pytest.raises(ValueError):
        precision(expected_keys=(KEY, KEY))


def test_coordinate_units_are_part_of_retrieval_identity():
    bad_identity = replace(IDENTITY, coordinate_unit="m")
    assert (
        retrieval(replace(ENVELOPE, identity=bad_identity)).status
        is GateStatus.INVALID_EVIDENCE
    )


@pytest.mark.parametrize("records", [None, 1, "station"])
def test_malformed_expected_inventory_raises_value_error(records):
    with pytest.raises(ValueError):
        check_retrieval(
            ENVELOPE,
            expected_identity=IDENTITY,
            expected_records=records,
            coordinate_tolerance="0.01",
            criteria_source=BASIS,
        )


@pytest.mark.parametrize("keys", [None, 1, "station"])
def test_malformed_expected_precision_keys_raise_value_error(keys):
    with pytest.raises(ValueError):
        precision(expected_keys=keys)


@pytest.mark.parametrize(
    "extra, status",
    [
        (replace(RECORD, key=("other", "SX")), GateStatus.INVALID_EVIDENCE),
        (
            replace(RECORD, state=RetrievalState.NUMERICAL_FAILURE),
            GateStatus.NUMERICAL_FAILURE,
        ),
        (replace(RECORD, state=RetrievalState.MISSING), GateStatus.NOT_EVALUATED),
    ],
)
def test_no_partial_values_escape_refused_record_sets(extra, status):
    second_key = ("station-b", "SX")
    second = replace(RECORD, key=second_key)
    result = check_retrieval(
        replace(ENVELOPE, records=(second, extra)),
        expected_identity=IDENTITY,
        expected_records=EXPECTED + (ExpectedRecord(second_key, ("10", "20")),),
        coordinate_tolerance="0.01",
        criteria_source=BASIS,
    )
    assert result.status is status
    assert result.values == ()
