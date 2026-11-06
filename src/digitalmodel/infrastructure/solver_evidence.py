"""Provider-neutral checks of supplied retrieval/export evidence; no solver calls.

These checks do not authenticate native provenance or qualify engineering results.
Contracts, rounding assumptions and adapter inventory are documented in
``docs/standards/solver-evidence-gates.html`` (revision 0).
"""

from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from fractions import Fraction
import re
from typing import Mapping


RecordKey = tuple[str, str]
# Representation policy, solver-evidence-gates.html rev 0, Representation limits.
# Software resource bounds only; these are not engineering acceptance criteria.
_MAX_CHARACTERS = 128
_MAX_EXPONENT = 1000
_LITERAL = re.compile(r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[Ee][+-]?[0-9]+)?")


class GateStatus(Enum):
    PASS = "PASS"
    NOT_EVALUATED = "NOT_EVALUATED"
    INVALID_EVIDENCE = "INVALID_EVIDENCE"
    NUMERICAL_FAILURE = "NUMERICAL_FAILURE"


class RetrievalState(Enum):
    VALID = "VALID"
    MISSING = "MISSING"
    DEFAULTED = "DEFAULTED"
    INVALID = "INVALID"
    NUMERICAL_FAILURE = "NUMERICAL_FAILURE"


@dataclass(frozen=True)
class GateResult:
    status: GateStatus
    reasons: tuple[str, ...] = ()
    values: tuple[tuple[RecordKey, Fraction], ...] = ()


@dataclass(frozen=True)
class EvidenceIdentity:
    case_id: str
    result_set: str
    recovery_source: str
    frame: str
    unit: str
    coordinate_unit: str


@dataclass(frozen=True)
class ExpectedRecord:
    key: RecordKey
    coordinates: tuple[str, ...]


@dataclass(frozen=True)
class RetrievedRecord:
    key: RecordKey
    coordinates: tuple[str, ...]
    value: str | None
    state: RetrievalState


@dataclass(frozen=True)
class RetrievalEnvelope:
    identity: EvidenceIdentity
    records: tuple[RetrievedRecord, ...]


@dataclass(frozen=True)
class DecimalField:
    value: Fraction
    half_quantum: Fraction


def parse_decimal_field(text: str) -> DecimalField:
    """Parse ASCII decimal text exactly, retaining its last printed place.

    For mantissa with d fractional places and exponent e, q = 10**(e-d)/2.
    Source: What Every Computer Scientist Should Know About Floating-Point
    Arithmetic (1991), Relative Error and Ulps, nearest rounding bound.
    q assumes rounding to nearest; it does not measure stored-result precision.
    """
    if (
        not isinstance(text, str)
        or len(text) > _MAX_CHARACTERS
        or _LITERAL.fullmatch(text) is None
    ):
        raise ValueError("Expected bounded finite ASCII decimal text")
    mantissa, _, exponent_text = text.lower().partition("e")
    exponent = int(exponent_text) if exponent_text else 0
    if abs(exponent) > _MAX_EXPONENT:
        raise ValueError("Decimal exponent exceeds representation policy")
    places = len(mantissa.partition(".")[2])
    power = exponent - places
    # Decimal string construction and Fraction conversion are context-independent.
    value = Fraction(Decimal(text))
    quantum = Fraction(10**power) if power >= 0 else Fraction(1, 10**-power)
    return DecimalField(value, quantum / 2)


def _text(value):
    return isinstance(value, str) and bool(value.strip())


def _key(value):
    return (
        isinstance(value, tuple)
        and len(value) == 2
        and all(_text(part) for part in value)
    )


def _identity(identity):
    return (
        isinstance(identity, EvidenceIdentity)
        and all(_text(value) for value in vars(identity).values())
        and identity.recovery_source.strip().casefold()
        not in {"auto", "automatic", "default"}
    )


def _limit(text, *, positive=False):
    value = parse_decimal_field(text).value
    if value < 0 or (positive and value == 0):
        raise ValueError("Limit has an invalid sign")
    return value


def _coordinates(values):
    if not isinstance(values, tuple) or not values:
        raise ValueError("Expected nonempty coordinate tuple")
    return tuple(parse_decimal_field(value).value for value in values)


def _expected(records):
    if not isinstance(records, (tuple, list)):
        raise ValueError("Expected an explicit record sequence")
    expected = {}
    for record in records:
        if (
            not isinstance(record, ExpectedRecord)
            or not _key(record.key)
            or record.key in expected
        ):
            raise ValueError("Invalid or duplicate expected record")
        expected[record.key] = _coordinates(record.coordinates)
    if not expected:
        raise ValueError("Expected record set is empty")
    return expected


def _record_value(record):
    if not isinstance(record.state, RetrievalState):
        return GateStatus.INVALID_EVIDENCE, "Unknown retrieval state", None
    if record.state in (RetrievalState.DEFAULTED, RetrievalState.INVALID):
        return GateStatus.INVALID_EVIDENCE, "Defaulted or invalid retrieval", None
    if record.state is RetrievalState.NUMERICAL_FAILURE:
        return GateStatus.NUMERICAL_FAILURE, "Reported native numerical failure", None
    if record.state is RetrievalState.MISSING or record.value is None:
        return GateStatus.NOT_EVALUATED, "Native value unavailable", None
    try:
        return GateStatus.PASS, "", parse_decimal_field(record.value).value
    except ValueError:
        return GateStatus.INVALID_EVIDENCE, "Malformed native value", None


def check_retrieval(
    envelope,
    *,
    expected_identity,
    expected_records,
    coordinate_tolerance,
    criteria_source,
) -> GateResult:
    """Check complete keyed evidence; only PASS exposes values, including zero.

    Invalid policy raises ValueError. Missing envelopes/values remain unavailable;
    malformed, duplicate, unexpected and defaulted records are invalid evidence.
    Coordinates use the expected frame and coordinate unit supplied by the adapter;
    ``identity.unit`` describes the response unit. No unit conversion is inferred.
    """
    if not _identity(expected_identity) or not _text(criteria_source):
        raise ValueError("Explicit identity and criterion source are required")
    expected = _expected(expected_records)
    tolerance = _limit(coordinate_tolerance)
    if envelope is None:
        return GateResult(GateStatus.NOT_EVALUATED, ("Retrieval envelope unavailable",))
    if (
        not isinstance(envelope, RetrievalEnvelope)
        or not _identity(envelope.identity)
        or envelope.identity != expected_identity
        or not isinstance(envelope.records, tuple)
    ):
        return GateResult(
            GateStatus.INVALID_EVIDENCE, ("Retrieval identity/layout differs",)
        )
    return _check_records(envelope.records, expected, tolerance)


def _check_records(records, expected, tolerance):
    seen, values, failures = set(), [], []
    for record in records:
        if (
            not isinstance(record, RetrievedRecord)
            or not _key(record.key)
            or record.key not in expected
            or record.key in seen
        ):
            failures.append(
                (GateStatus.INVALID_EVIDENCE, "Unexpected or duplicate record")
            )
            continue
        seen.add(record.key)
        try:
            coordinates = _coordinates(record.coordinates)
            target = expected[record.key]
            if len(coordinates) != len(target) or any(
                abs(got - want) > tolerance for got, want in zip(coordinates, target)
            ):
                raise ValueError("Coordinate identity differs")
        except ValueError:
            failures.append(
                (GateStatus.INVALID_EVIDENCE, "Coordinate identity differs")
            )
            continue
        status, reason, value = _record_value(record)
        if status is GateStatus.PASS:
            values.append((record.key, value))
        else:
            failures.append((status, reason))
    if seen != expected.keys():
        failures.append((GateStatus.INVALID_EVIDENCE, "Record coverage differs"))
    # Structural invalidity takes precedence over numerical or missing results.
    for status in (
        GateStatus.INVALID_EVIDENCE,
        GateStatus.NUMERICAL_FAILURE,
        GateStatus.NOT_EVALUATED,
    ):
        if any(failure[0] is status for failure in failures):
            return GateResult(status, tuple(reason for _, reason in failures))
    return GateResult(GateStatus.PASS, values=tuple(sorted(values)))


@dataclass(frozen=True)
class PrecisionPolicy:
    unit: str
    dimensional_floor: str
    max_half_quantum: str
    readback_limit: str
    criteria_source: str

    def limits(self):
        """All dimensional limits are required, sourced inputs; no defaults."""
        if not _text(self.unit) or not _text(self.criteria_source):
            raise ValueError("Unit and criterion source are required")
        floor = _limit(self.dimensional_floor, positive=True)
        budget = _limit(self.max_half_quantum, positive=True)
        readback = _limit(self.readback_limit)
        # Contract: solver-evidence-gates.html rev 0, Precision and readback.
        if budget >= floor or readback >= floor:
            raise ValueError(
                "Precision and readback limits must be below the dimensional floor"
            )
        return budget, readback


def check_export_precision(
    *,
    export_fields,
    readback_fields,
    expected_keys,
    policy,
    witness_text,
    witness_expected,
    witness_max_half_quantum,
    export_evidence_id,
    readback_evidence_id,
    expected_identity,
    export_identity,
    readback_identity,
) -> GateResult:
    """Check attributed captures; native independence/rounding remain adapter duties."""
    budget, readback_limit = _precision_limits(policy, expected_identity)
    witness_value = parse_decimal_field(witness_expected).value
    witness_budget = _limit(witness_max_half_quantum, positive=True)
    keys = _expected_keys(expected_keys)
    if not _text(export_evidence_id) or not _text(readback_evidence_id):
        raise ValueError("Explicit capture identities are required")
    if export_fields is None or readback_fields is None or witness_text is None:
        return GateResult(GateStatus.NOT_EVALUATED, ("Precision evidence unavailable",))
    if (
        not _identity(export_identity)
        or not _identity(readback_identity)
        or export_identity != expected_identity
        or readback_identity != expected_identity
        or policy.unit != expected_identity.unit
        or export_evidence_id.strip() == readback_evidence_id.strip()
        or export_fields is readback_fields
        or not isinstance(export_fields, Mapping)
        or not isinstance(readback_fields, Mapping)
        or set(export_fields) != set(keys)
        or set(readback_fields) != set(keys)
    ):
        return GateResult(
            GateStatus.INVALID_EVIDENCE, ("Readback identity/coverage differs",)
        )
    return _precision_comparison(
        export_fields,
        readback_fields,
        keys,
        budget,
        readback_limit,
        witness_text,
        witness_value,
        witness_budget,
    )


def _precision_limits(policy, expected_identity):
    if not isinstance(policy, PrecisionPolicy):
        raise ValueError("PrecisionPolicy is required")
    if not _identity(expected_identity):
        raise ValueError("Explicit expected result identity is required")
    return policy.limits()


def _expected_keys(expected_keys):
    if not isinstance(expected_keys, (tuple, list)):
        raise ValueError("Expected an explicit key sequence")
    keys = tuple(expected_keys)
    if not keys or any(not _key(key) for key in keys) or len(set(keys)) != len(keys):
        raise ValueError("Distinct nonempty expected keys are required")
    return keys


def _precision_comparison(
    export_fields,
    readback_fields,
    keys,
    budget,
    readback_limit,
    witness_text,
    witness_value,
    witness_budget,
):
    try:
        witness = parse_decimal_field(witness_text)
        if witness.value != witness_value or witness.half_quantum > witness_budget:
            raise ValueError("Formatting witness differs or lacks precision")
        for key in keys:
            exported = parse_decimal_field(export_fields[key])
            readback = parse_decimal_field(readback_fields[key])
            if max(exported.half_quantum, readback.half_quantum) > budget:
                raise ValueError("Representation exceeds the fixed precision budget")
            if abs(exported.value - readback.value) > readback_limit:
                raise ValueError("Independent readback exceeds the fixed limit")
    except ValueError as exc:
        return GateResult(GateStatus.INVALID_EVIDENCE, (str(exc),))
    return GateResult(GateStatus.PASS)
