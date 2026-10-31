"""CPU-independent power for the fatigue engine.

``np.power`` dispatches to processor-specific vector kernels, and the scalar
libm ``pow`` behind ``math.pow`` and ``float ** float`` selects a per-CPU
implementation. Their last bits can differ between machines, so committed
outputs that embed unrounded fatigue results did not reproduce between CI
runners. This module evaluates powers with integer arithmetic only: exact
rational arithmetic for integer exponents and the ``decimal`` module (libmpdec)
otherwise.

Rounding. Integer exponents are exact and rounded once to the nearest float.
Other results are computed in decimal at 40 significant digits, then at 80, 160
and 320 digits while the decimal value lies so close to a binary64 rounding
boundary (a midpoint between adjacent floats) that the float it rounds to is
not settled by the decimal precision; each attempt allows the decimal result
an error of 10**5 units in its last digit. The settled result is the correctly
rounded float. A value still unsettled at 320 digits, such as
``deterministic_power(208065.0 ** 2, 1.5)`` whose exact value 208065**3 lies on
a midpoint, is taken to lie on the boundary itself and is rounded half to even,
which is the IEEE rule for an exact midpoint.

The decimal arithmetic runs in a private local context: the caller's decimal
context, including a trapped ``FloatOperation``, has no effect on the result.
"""

from __future__ import annotations

import math
from decimal import (Context, Decimal, Inexact, InvalidOperation, MAX_EMAX, MIN_EMIN,
                     localcontext)
from fractions import Fraction

# Decimal precisions tried in turn; see the module docstring.
_PRECISIONS = (40, 80, 160, 320)
# Error allowed on each decimal result, as a power of ten in units of its last digit.
_GUARD_DIGITS = 5
# Integer exponents up to this magnitude are evaluated exactly.
_MAX_EXACT_EXPONENT = 64


def _context(precision: int) -> Context:
    # A fresh context traps InvalidOperation only; FloatOperation is not trapped.
    return Context(prec=precision, Emax=MAX_EMAX, Emin=MIN_EMIN, traps=[InvalidOperation])


def _rounded_once(evaluate) -> float:
    """Nearest float to the value that ``evaluate(context)`` approximates."""
    for precision in _PRECISIONS:
        with localcontext(_context(precision)) as context:
            value = evaluate(context)
            if not context.flags[Inexact] or not value.is_finite() or value.is_zero():
                return float(value)
            guard = Decimal(1).scaleb(value.adjusted() - precision + 1 + _GUARD_DIGITS)
            wide = _context(precision + 2)
            if float(wide.subtract(value, guard)) == float(wide.add(value, guard)):
                return float(value)
    return float(value)


def deterministic_power(base: float, exponent: float) -> float:
    """``base ** exponent`` for finite ``base >= 0`` and finite ``exponent``.

    The result is independent of CPU and libm and is correctly rounded (see the
    module docstring for an exact midpoint). Like IEEE ``pow``, overflow gives
    ``inf``, underflow gives ``0.0`` or a subnormal, and zero raised to a
    negative exponent gives ``inf``.
    """
    base = float(base)
    exponent = float(exponent)
    if not (math.isfinite(base) and math.isfinite(exponent)) or base < 0:
        raise ValueError("deterministic_power requires finite base >= 0 and finite exponent")
    if exponent.is_integer() and abs(exponent) <= _MAX_EXACT_EXPONENT:
        try:
            return float(Fraction(base) ** int(exponent))
        except ZeroDivisionError:
            return math.inf
        except OverflowError:
            return math.inf
    if base == 0.0:
        return 0.0 if exponent > 0 else math.inf
    return _rounded_once(lambda context: context.power(Decimal.from_float(base),
                                                       Decimal.from_float(exponent)))


def deterministic_log10(value: float) -> float:
    """Correctly rounded base-10 logarithm of a finite positive float, independent of libm."""
    value = float(value)
    if not math.isfinite(value) or value <= 0:
        raise ValueError("deterministic_log10 requires a finite positive value")
    return _rounded_once(lambda context: context.log10(Decimal.from_float(value)))
