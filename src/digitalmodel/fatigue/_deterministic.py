"""CPU-independent power for the fatigue engine.

``np.power`` dispatches to processor-specific vector kernels, and the scalar
libm ``pow`` behind ``math.pow`` and ``float ** float`` selects a per-CPU
implementation. Their last bits can differ between machines, so committed
outputs that embed unrounded fatigue results did not reproduce between CI
runners. This module evaluates powers with integer arithmetic only: exact
rational arithmetic for integer exponents and the ``decimal`` module (libmpdec)
otherwise, each rounded once to the nearest float.
"""

from __future__ import annotations

import math
from decimal import Context, Decimal, InvalidOperation, MAX_EMAX, MIN_EMIN
from fractions import Fraction

# 40 significant digits keeps the decimal result far inside one float ulp
# before the final rounding.
_CONTEXT = Context(prec=40, Emax=MAX_EMAX, Emin=MIN_EMIN, traps=[InvalidOperation])
# Integer exponents up to this magnitude are evaluated exactly.
_MAX_EXACT_EXPONENT = 64


def deterministic_power(base: float, exponent: float) -> float:
    """``base ** exponent`` for finite ``base >= 0`` and finite ``exponent``.

    The result is independent of CPU and libm. Like IEEE ``pow``, overflow
    gives ``inf``, underflow gives ``0.0`` or a subnormal, and zero raised to a
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
    value = _CONTEXT.power(Decimal(base), Decimal(exponent))
    return float(value)


def deterministic_log10(value: float) -> float:
    """Base-10 logarithm of a finite positive float, independent of libm."""
    value = float(value)
    if not math.isfinite(value) or value <= 0:
        raise ValueError("deterministic_log10 requires a finite positive value")
    return float(_CONTEXT.log10(Decimal(value)))
