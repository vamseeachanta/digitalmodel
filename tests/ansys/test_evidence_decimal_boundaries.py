# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exact, synthetic representation cases; no engineering acceptance is inferred.

Sources: analysis_records.decimal_text and analysis_replay.canonical_measurement
at repository revision 6476218e define the existing representation limits.
Expected strings below are independent decimal-point shifts, not solver results.
"""
from decimal import Context, Inexact, ROUND_DOWN, localcontext

import pytest

from digitalmodel.ansys.analysis_records import decimal_text
from digitalmodel.ansys.analysis_replay import canonical_measurement
from digitalmodel.ansys.cylinder_results import EvidenceError, parse_e24


@pytest.mark.parametrize("raw", [
    "1." + "1" * 27 + "e-100",
    "-1." + "1" * 26 + "e-100",
])
def test_decimal_expansion_must_fit_the_canonical_contract(raw):
    """Point shift gives 129 characters: 2 + 99 + 28, or 3 + 99 + 27.

    Both exceed the existing 128-character contract, despite short raw input.
    The expected disposition is refusal before an unusable value is emitted.
    """
    with pytest.raises(ValueError, match="bounded"):
        decimal_text(raw)


@pytest.mark.parametrize("raw,expected", [
    ("1e100", "1" + "0" * 100),
    ("1e-100", "0." + "0" * 99 + "1"),
    ("1." + "1" * 26 + "e-100", "0." + "0" * 99 + "1" * 27),
    ("-1." + "1" * 25 + "e-100", "-0." + "0" * 99 + "1" * 26),
    ("-0.000", "0"),
])
def test_canonical_output_is_exact_and_can_be_validated_again(raw, expected):
    """Multiplication by powers of ten shifts the point without rounding.

    The last nonzero strings contain exactly 128 characters, including sign.
    Signed zero has the existing canonical spelling '0'.
    """
    with localcontext(Context(prec=2, rounding=ROUND_DOWN)) as context:
        context.traps[Inexact] = True
        assert decimal_text(raw) == expected
        assert decimal_text(expected) == expected


@pytest.mark.parametrize("raw,expected", [
    ("1e63", "1" + "0" * 63),
    ("1e-100", "0." + "0" * 99 + "1"),
    ("1." + "1" * 26 + "e-100", "0." + "0" * 99 + "1" * 27),
    ("-0.000", "0"),
])
def test_replay_output_can_be_validated_again(raw, expected):
    """1e63 expands to 64 digits, the existing replay coefficient bound.

    Other expected strings follow from point shifts and signed-zero removal.
    Replaying the emitted string must not fail the replay's own raw bounds.
    """
    assert canonical_measurement(raw) == expected
    assert canonical_measurement(expected) == expected


@pytest.mark.parametrize("raw", [
    "1e101", "1e-101", "1e128", "1e64", "1e100", "\u0661", "\uff11.0",
    "1." + "1" * 27 + "e-100", "NaN", "Infinity",
])
def test_replay_cannot_emit_a_package_incompatible_value(raw):
    """Adjusted exponents exceed +/-100, digits are non-ASCII, or text overflows.

    1e64 expands to 65 digits, exceeding the existing replay coefficient bound.
    Other bounds and ASCII grammar are the existing decimal_text contract;
    the test introduces no numerical engineering tolerance.
    """
    with pytest.raises(ValueError):
        canonical_measurement(raw)


@pytest.mark.parametrize("mantissa,exponent", [
    ("0.1000000000000000", "-100"),
    ("0.0000000000000001", "-100"),
    ("1.0000000000000000", "+101"),
])
def test_e24_refuses_values_outside_package_exponents(mantissa, exponent):
    """0.1e-100 = 1e-101; 1e-16 * 1e-100 = 1e-116; 1e101 > 1e100.

    All are outside the existing adjusted-exponent bound even though two
    written exponents are at its lower edge.
    """
    field = (mantissa + "E" + exponent).rjust(24).encode("ascii")
    with pytest.raises(EvidenceError):
        parse_e24(field, "MPa")


@pytest.mark.parametrize("mantissa,exponent,expected,quantum", [
    ("1.0000000000000000", "-100", "0." + "0" * 99 + "1", "5e-117"),
    ("1.0000000000000000", "+100", "1" + "0" * 100, "5e83"),
    ("0.0000000000000000", "+00", "0", "5e-17"),
])
def test_e24_boundary_values_remain_exact(mantissa, exponent, expected, quantum):
    """E24.16 half-quantum is 0.5 * 10**(exponent - 16).

    Source: cylinder_results.parse_e24 at revision 6476218e. Thus exponents
    -100, +100 and 0 give 5e-117, 5e83 and 5e-17, respectively.
    Values follow by moving the decimal point; these are representation tests.
    """
    from decimal import Decimal

    field = (mantissa + "E" + exponent).rjust(24).encode("ascii")
    value, half_quantum = parse_e24(field, "MPa")
    assert value == Decimal(expected)
    assert half_quantum == Decimal(quantum)
    assert decimal_text(expected) == expected
