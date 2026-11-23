"""Standalone arithmetic/checker protocol; no producer imports or code execution.

Resource limits are part of the pre-invocation contract: 65536 envelope bytes,
8192 expression characters, 1024 tokens, 48 parse levels, 128 literal characters,
decimal exponent magnitude 256, power magnitude 64, 4096-bit rational parts.
Arithmetic is rational until an irrational sqrt, then Decimal at 50 digits.
No independent-checker invocation or reference acceptance occurs here.
"""
import json
import math
import re
from decimal import (Context, Decimal, localcontext, ROUND_HALF_EVEN,
                     InvalidOperation, DivisionByZero, Overflow)
from fractions import Fraction

SYMBOLS = frozenset(("a", "b", "p", "E", "nu", "r", "y"))
NUMERIC_CONTEXT = Context(prec=50, rounding=ROUND_HALF_EVEN, Emin=-4096, Emax=4096,
                          capitals=1, clamp=0, flags=[],
                          traps=[InvalidOperation, DivisionByZero, Overflow])
QUANTITIES = ("sigma_r", "sigma_theta", "sigma_z", "tau_rz", "sigma_vm", "u_r", "u_z")
NUMBER = r"(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?"
TOKEN = re.compile(rf"{NUMBER}|[A-Za-z_][A-Za-z_0-9]*|\*\*|[()+*/-]")


class ExpressionError(ValueError):
    """Nonconforming or unbounded checker evidence; acceptance stays unestablished."""


def _bounded(value):
    if isinstance(value, Fraction):
        if max(value.numerator.bit_length(), value.denominator.bit_length()) > 4096:
            raise ExpressionError("rational intermediate exceeds 4096 bits")
    elif not value.is_finite() or (value and abs(value.adjusted()) > 1024):
        raise ExpressionError("decimal intermediate outside bounds")
    return value


def _number(token):
    if len(token) > 128 or re.fullmatch(NUMBER, token) is None:
        raise ExpressionError("invalid or oversized decimal literal")
    parts = re.split("[eE]", token)
    if len(parts) == 2 and abs(int(parts[1])) > 256:
        raise ExpressionError("decimal exponent outside bounds")
    return _bounded(Fraction(Decimal(token)))


def _symbols(values):
    if not isinstance(values, dict) or set(values) != SYMBOLS:
        raise ExpressionError("exact seven input symbols required")
    result = {}
    for key, value in values.items():
        if isinstance(value, bool) or not isinstance(value, (str, int, Fraction, Decimal)):
            raise ExpressionError("exact numeric symbols required; no binary float")
        if isinstance(value, Fraction):
            result[key] = _bounded(value)
        elif isinstance(value, int):
            result[key] = _bounded(Fraction(value))
        else:
            text = str(value)
            sign = -1 if text.startswith("-") else 1
            unsigned = text[1:] if text.startswith(("+", "-")) else text
            result[key] = sign * _number(unsigned)
    return result


def _decimal(value):
    if isinstance(value, Fraction):
        return Decimal(value.numerator) / Decimal(value.denominator)
    return value


def _binary(left, op, right):
    if isinstance(left, Decimal) or isinstance(right, Decimal):
        left, right = _decimal(left), _decimal(right)
    if op == "+":
        value = left + right
    elif op == "-":
        value = left - right
    elif op == "*":
        value = left * right
    else:
        value = left / right
    return _bounded(value)


def _sqrt(value):
    if value < 0:
        raise ExpressionError("negative square-root argument")
    if isinstance(value, Fraction):
        n, d = math.isqrt(value.numerator), math.isqrt(value.denominator)
        if n * n == value.numerator and d * d == value.denominator:
            return Fraction(n, d)
    return _bounded(_decimal(value).sqrt())


def _tokens(expression):
    if not isinstance(expression, str) or not expression or len(expression) > 8192:
        raise ExpressionError("expression must contain 1..8192 characters")
    tokens, position = [], 0
    while position < len(expression):
        if expression[position] in " \t\r\n":
            position += 1
            continue
        match = TOKEN.match(expression, position)
        if match is None:
            raise ExpressionError("unsupported expression token")
        tokens.append(match.group())
        position = match.end()
        if len(tokens) > 1024:
            raise ExpressionError("expression exceeds 1024 tokens")
    return tokens + [""]


class _Parser:
    def __init__(self, expression, symbols):
        self.tokens, self.values = _tokens(expression), symbols
        self.index = 0

    def peek(self):
        return self.tokens[self.index]

    def pop(self):
        token = self.peek()
        if token:
            self.index += 1
        return token

    def require(self, token):
        if self.pop() != token:
            raise ExpressionError("required delimiter missing")

    def expression(self, depth=0):
        if depth > 48:
            raise ExpressionError("expression exceeds 48 parse levels")
        value = self.term(depth)
        while self.peek() in ("+", "-"):
            op = self.pop()
            value = _binary(value, op, self.term(depth))
        return value

    def term(self, depth):
        value = self.unary(depth)
        while self.peek() in ("*", "/"):
            op = self.pop()
            value = _binary(value, op, self.unary(depth))
        return value

    def unary(self, depth):
        if depth > 48:
            raise ExpressionError("expression exceeds 48 parse levels")
        if self.peek() in ("+", "-"):
            sign = -1 if self.pop() == "-" else 1
            return sign * self.unary(depth + 1)
        value = self.atom(depth)
        if self.peek() == "**":
            self.pop()
            exponent = self.exponent()
            if isinstance(value, Fraction) and exponent:
                bits = max(value.numerator.bit_length(), value.denominator.bit_length())
                if bits * abs(exponent) > 8192:
                    raise ExpressionError("power intermediate exceeds bounds")
            value = _bounded(value ** exponent)
        return value

    def exponent(self):
        grouped = self.peek() == "("
        if grouped:
            self.pop()
        sign = 1
        if self.peek() in ("+", "-"):
            sign = -1 if self.pop() == "-" else 1
        digits = self.pop()
        if not re.fullmatch(r"[0-9]{1,128}", digits):
            raise ExpressionError("power requires signed digits with at most one pair")
        value = sign * int(digits)
        if abs(value) > 64:
            raise ExpressionError("power magnitude exceeds 64")
        if grouped:
            self.require(")")
        return value

    def atom(self, depth):
        token = self.pop()
        if token == "(":
            value = self.expression(depth + 1)
            self.require(")")
            return value
        if token == "sqrt":
            self.require("(")
            value = self.expression(depth + 1)
            self.require(")")
            return _sqrt(value)
        if token in self.values:
            return self.values[token]
        return _number(token)


def evaluate_expression(expression, symbols):
    """Evaluate the restricted grammar using exact inputs; never eval/exec."""
    try:
        with localcontext(NUMERIC_CONTEXT):
            parser = _Parser(expression, _symbols(symbols))
            value = parser.expression()
            if parser.peek():
                raise ExpressionError("trailing tokens or ungrouped chained power")
            return _bounded(value)
    except (ArithmeticError, RecursionError, ValueError) as exc:
        raise ExpressionError(str(exc)) from exc


def _unique(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ExpressionError("duplicate JSON key")
        result[key] = value
    return result


def parse_checker_output(raw):
    """Validate bare UTF-8 JSON without repair; caller retains the original bytes."""
    if not isinstance(raw, bytes) or not 0 < len(raw) <= 65536:
        raise ExpressionError("checker envelope must contain 1..65536 bytes")
    try:
        text = raw.decode("utf-8").strip(" \t\r\n")
        value = json.loads(text, object_pairs_hook=_unique)
        if not isinstance(value, dict) or set(value) != {"expressions", "derivation"}:
            raise ExpressionError("incorrect top-level checker keys")
        expressions = value["expressions"]
        if not isinstance(expressions, dict) or set(expressions) != set(QUANTITIES):
            raise ExpressionError("incorrect checker quantity keys")
        if not isinstance(value["derivation"], str) or not value["derivation"].strip():
            raise ExpressionError("nonempty derivation required")
        value["derivation"].encode("utf-8")
        for expression in expressions.values():
            _tokens(expression)
        return value
    except (ValueError, UnicodeError, RecursionError) as exc:
        raise ExpressionError(str(exc)) from exc


def evaluate_checker(raw, symbols):
    """Evaluate all seven returned expressions; this does not accept the reference."""
    parsed = parse_checker_output(raw)
    with localcontext(NUMERIC_CONTEXT):
        return {name: _decimal(evaluate_expression(parsed["expressions"][name], symbols))
                for name in QUANTITIES}
