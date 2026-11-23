"""Derived open-ended Lamé reference for the issue-2121 approved inputs.

MPa/mm, tension positive; radial X, axial Y, hoop Z. These are mathematical
elasticity identities and approved case inputs, not standards allowables.
Independent checker agreement and engineering qualification are not implied.
"""
from decimal import (Context, Decimal, localcontext, ROUND_HALF_EVEN,
                     InvalidOperation, DivisionByZero, Overflow)
from fractions import Fraction

from digitalmodel.ansys.analysis_records import decimal_text

PRECISION = 50
NUMERIC_CONTEXT = Context(prec=50, rounding=ROUND_HALF_EVEN, Emin=-4096, Emax=4096,
                          capitals=1, clamp=0, flags=[],
                          traps=[InvalidOperation, DivisionByZero, Overflow])
BASIS = {"a": Fraction(750), "b": Fraction(810), "E": Fraction(200000),
         "nu": Fraction(3, 10)}


def _input(value):
    if not isinstance(value, str) or len(value) > 128:
        raise ValueError("reference inputs must be bounded exact decimal strings")
    try:
        number = Decimal(decimal_text(value))
        if not number.is_finite() or abs(number.adjusted()) > 100:
            raise ValueError("reference input outside numeric bounds")
        return Fraction(number)
    except (ArithmeticError, ValueError) as exc:
        raise ValueError("invalid reference decimal") from exc


def symbols_for(pressure, r, y):
    """Return exact symbols within the fixed physical basis (not lookup keys)."""
    p, radius, axial = map(_input, (pressure, r, y))
    if p not in (0, 10) or not 750 <= radius <= 810 or not 0 <= axial <= 240:
        raise ValueError("outside approved pressure or cylinder domain")
    return {**BASIS, "p": p, "r": radius, "y": axial}


def _decimal(value):
    with localcontext(NUMERIC_CONTEXT):
        if isinstance(value, Fraction):
            return Decimal(value.numerator) / Decimal(value.denominator)
        return +value


def reference_exact(pressure, r, y):
    """Return exact rational components; equivalent stress uses 50-digit sqrt."""
    s = symbols_for(pressure, r, y)
    a, b, p, radius, axial, e, nu = [s[k] for k in
                                    ("a", "b", "p", "r", "y", "E", "nu")]
    aa = p * a**2 / (b**2 - a**2)
    bb = aa * b**2
    radial, hoop = aa - bb / radius**2, aa + bb / radius**2
    with localcontext(NUMERIC_CONTEXT):
        equivalent = _decimal(radial**2 + hoop**2 - radial * hoop).sqrt()
    return {"sigma_r": radial, "sigma_theta": hoop, "sigma_z": Fraction(0),
            "tau_rz": Fraction(0), "sigma_vm": equivalent,
            "u_r": ((1 - nu) * aa * radius + (1 + nu) * bb / radius) / e,
            "u_z": -2 * nu * aa * axial / e}


def reference(pressure, r, y):
    """Return seven Decimal values at fixed 50-digit precision."""
    return {key: _decimal(value) for key, value in reference_exact(pressure, r, y).items()}


def reference_text(pressure, r, y):
    """Serialize using the existing canonical-decimal identity contract."""
    return {key: decimal_text(str(value)) for key, value in reference(pressure, r, y).items()}
