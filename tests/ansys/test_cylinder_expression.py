"""Restricted grammar, exact arithmetic and envelope refusal tests."""
import json
from decimal import Decimal, Inexact, ROUND_DOWN, localcontext
from fractions import Fraction

import pytest

from digitalmodel.ansys.cylinder_expression import (
    ExpressionError, evaluate_expression, evaluate_checker, parse_checker_output,
)

SYMBOLS = {"a": "2", "b": "3", "p": "10", "E": "200000", "nu": "0.3",
           "r": "4", "y": "120"}
NAMES = ("sigma_r", "sigma_theta", "sigma_z", "tau_rz", "sigma_vm", "u_r", "u_z")


def payload(expression="0"):
    return json.dumps({"expressions": dict.fromkeys(NAMES, expression),
                       "derivation": "Synthetic arithmetic fixture only."}).encode()


@pytest.mark.parametrize("expression,expected", [
    (".1 + .2", Fraction(3, 10)), ("1e-3", Fraction(1, 1000)),
    ("2.", 2), ("+2", 2), ("r**-2", Fraction(1, 16)),
    ("r**(-2)", Fraction(1, 16)), ("r**(+2)", 16), ("r**+2", 16),
    ("-a**2", -4), ("(-a)**2", 4), ("8/4/2", 1), ("8-4-2", 2),
    ("(a**2)**3", 64), ("sqrt(4/9)", Fraction(2, 3)),
    (" \t a +\r\n b\n", 5), ("--a", 2),
])
def test_exact_grammar_and_precedence(expression, expected):
    assert evaluate_expression(expression, SYMBOLS) == expected


def test_irrational_sqrt_has_fixed_precision_not_ambient_context():
    with localcontext() as ctx:
        ctx.prec = 5
        result = evaluate_expression("sqrt(2)", SYMBOLS)
    assert isinstance(result, Decimal)
    assert str(result) == "1.4142135623730950488016887242096980785696718753769"


@pytest.mark.parametrize("expression", [
    "r**2.0", "r**2e0", "r**((2))", "r**.5", "r**2**3", "r**(2+1)",
    "a^2", "2a", "a.b", "a[0]", "abs(a)", "__import__('os')", "nan",
    "0x10", "1_000", "a//b", "a%2", "1\n2", "a\u00a0+b", "sqrt(-1)",
    "1/0", "0**-1", "1e999999", "2**999999", "(" * 200 + "1" + ")" * 200,
    "1+" * 5000 + "1", "sqrt(" * 100 + "2" + ")" * 100,
], ids=lambda value: str(value)[:60])
def test_unsupported_or_unbounded_expression_refused(expression):
    with pytest.raises(ExpressionError):
        evaluate_expression(expression, SYMBOLS)


def test_bare_envelope_and_raw_preservation():
    raw = b" \r\n" + payload("a+\nb") + b"\t"
    before = bytes(raw)
    parsed = parse_checker_output(raw)
    assert parsed["expressions"]["sigma_r"] == "a+\nb"
    assert raw == before
    assert set(evaluate_checker(raw, SYMBOLS).values()) == {Decimal(5)}


@pytest.mark.parametrize("raw", [
    b"\xef\xbb\xbf" + payload(), b"```json\n" + payload() + b"\n```",
    b"prose " + payload(), payload() + b" {}", b"\xff", b"[]",
    payload().replace(b'"derivation":', b'"extra": 1, "derivation":'),
    payload().replace(b'"derivation":', b'"derivation": "first", "derivation":'),
    payload().replace(b'"sigma_r": "0"', b'"sigma_r": "1", "sigma_r": "0"'),
    payload().replace(b'"sigma_r": "0"', b'"sigma_r": 0'),
    payload().replace(b'Synthetic arithmetic fixture only.', b''),
    b'"' + b'x' * 70000 + b'"',
], ids=lambda value: str(value)[:60])
def test_invalid_envelope_refused(raw):
    with pytest.raises(ExpressionError):
        parse_checker_output(raw)


@pytest.mark.parametrize("bad", [0.1, True, "NaN", "1e999999", "--2", "+-2", "++2"])
def test_inexact_or_unbounded_symbols_refused(bad):
    with pytest.raises(ExpressionError):
        evaluate_expression("a", {**SYMBOLS, "a": bad})


def test_synthetic_signed_gain_and_offset_detected():
    baseline = evaluate_checker(payload("a+b"), SYMBOLS)
    for expr in ["-(a+b)", "1.01*(a+b)", "a+b+.01", "a+b-.01"]:
        mutated = evaluate_checker(payload(expr), SYMBOLS)
        assert abs(mutated["sigma_r"] - baseline["sigma_r"]) > Decimal("1e-10")


def test_decimal_traps_and_rounding_do_not_change_evaluator():
    expected = evaluate_expression("sqrt(2)+1/3", SYMBOLS)
    with localcontext() as ctx:
        ctx.rounding = ROUND_DOWN
        ctx.traps[Inexact] = True
        assert evaluate_expression("sqrt(2)+1/3", SYMBOLS) == expected


@pytest.mark.parametrize('expression', ['1e0256/1e256', '1e+0001/10', 'a**0002/a**2'])
def test_bounded_leading_zero_exponents_follow_contract(expression):
    assert evaluate_expression(expression, SYMBOLS) == 1


@pytest.mark.parametrize('value', [Fraction(2), Decimal('2')])
def test_exact_numeric_symbol_types(value):
    assert evaluate_expression('a', {**SYMBOLS, 'a': value}) == 2


@pytest.mark.parametrize('value', [Decimal('NaN'), Decimal('Infinity')])
def test_nonfinite_decimal_symbols_are_refused(value):
    with pytest.raises(ExpressionError):
        evaluate_expression('a', {**SYMBOLS, 'a': value})
