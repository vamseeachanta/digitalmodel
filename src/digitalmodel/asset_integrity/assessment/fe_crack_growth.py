"""Exact integration of a user-supplied, piecewise-linear DeltaK(a) history.

Source: AFGROW Handbook for Damage Tolerant Design (2002), section 2.2.5,
equation 2.2.7 and integration discussion:
https://afgrow.net/applications/DTDHandbook/sections/page2_2_5.aspx
No material coefficients or code acceptance limits are built in.
"""

from __future__ import annotations

import math


GROWTH_SOURCE = (
    "AFGROW Handbook for Damage Tolerant Design (2002), section 2.2.5, "
    "equation 2.2.7; piecewise-linear integration is an analytical derivation"
)


def scalar(value, name, *, positive=False):
    """Require finite numeric engineering inputs; booleans are not numbers."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or (value <= 0 if positive else value < 0):
        raise ValueError(
            f"{name} must be finite and {'positive' if positive else 'nonnegative'}"
        )
    return float(value)


def basis(value, name):
    """A nonempty basis records provenance without asserting qualification."""
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} requires a nonempty basis")
    return value


def table_pairs(table, name):
    """Require at least two finite pairs and a strictly increasing abscissa."""
    if not isinstance(table, (list, tuple)) or len(table) < 2:
        raise ValueError(f"{name} requires at least two pairs")
    pairs = []
    for row in table:
        if not isinstance(row, (list, tuple)) or len(row) != 2:
            raise ValueError(f"{name} requires pairs")
        x, y = (scalar(v, name) for v in row)
        if pairs and x <= pairs[-1][0]:
            raise ValueError(f"{name} abscissae must strictly increase")
        pairs.append((x, y))
    return pairs


def interpolate(table, x):
    """Piecewise-linear interpolation; extrapolation is prohibited."""
    if not table[0][0] <= x <= table[-1][0]:
        raise ValueError("extrapolation beyond supplied table is prohibited")
    for (x0, y0), (x1, y1) in zip(table, table[1:]):
        if x <= x1:
            return y0 + (y1 - y0) * (x - x0) / (x1 - x0)
    raise ValueError("table interval not found")


def _law(law):
    expected = {"c", "m", "threshold", "e_ratio", "threshold_rule", "basis"}
    if not isinstance(law, dict) or set(law) != expected:
        raise ValueError(
            "growth requires c, m, threshold, e_ratio, threshold_rule and basis"
        )
    basis(law["basis"], "growth basis")
    c = scalar(law["c"], "c", positive=True)
    m = scalar(law["m"], "m", positive=True)
    threshold = scalar(law["threshold"], "threshold")
    e_ratio = scalar(law["e_ratio"], "e_ratio", positive=True)
    if law["threshold_rule"] not in ("unchanged", "e_ratio"):
        raise ValueError("threshold_rule must be unchanged or e_ratio")
    # E scaling is a declared user sensitivity, not an automatic code correction.
    if law["threshold_rule"] == "e_ratio":
        threshold *= e_ratio
    scalar(threshold, "scaled threshold")
    return c, m, threshold, e_ratio


def _segment_cycles(da, k0, k1, c, m):
    """Integrate da/(C*K(a)^m) analytically, with stable near-constant limits.

    For nonconstant K: da*K0^-m * expm1((1-m)*ln(K1/K0)) /
    ((1-m)*(K1/K0-1)); m=1 takes the logarithmic limit.
    The integers here are mathematical identities, not engineering coefficients.
    """
    if k0 == k1:
        return da / (c * k0**m)
    relative_change = (k1 - k0) / k0
    logarithm = math.log1p(relative_change)
    exponent = 1 - m
    integral = (
        logarithm if exponent == 0 else math.expm1(exponent * logarithm) / exponent
    )
    return da / (c * k0**m) * integral / relative_change


def growth_life(table, law, *, multiplier=1.0):
    """Cycles to last supplied depth, or arrest before reaching that depth.

    Depth: m; DeltaK: MPa sqrt(m); C: m/cycle/(MPa sqrt(m))**m.
    Threshold equality arrests. Linear segments make endpoint checks sufficient
    to find every threshold crossing; no sampling can miss a narrow crossing.
    This Paris model does not establish near-threshold material behaviour.
    """
    pairs = table_pairs(table, "DeltaK table")
    if pairs[0][0] <= 0:
        raise ValueError("crack depths must be positive")
    c, m, threshold, e_ratio = _law(law)
    scale = e_ratio * scalar(multiplier, "DeltaK multiplier", positive=True)
    scalar(scale, "DeltaK scale", positive=True)
    pairs = [(a, scalar(k * scale, "scaled DeltaK")) for a, k in pairs]
    result = {
        "status": "FINITE",
        "cycles": None,
        "arrest_a_m": None,
        "minimum_threshold_ratio": min(k for _, k in pairs) / threshold
        if threshold
        else None,
        "source": GROWTH_SOURCE,
    }
    total = 0.0
    for (a0, k0), (a1, k1) in zip(pairs, pairs[1:]):
        if min(k0, k1) <= threshold:
            arrest = (
                a0 if k0 <= threshold else a0 + (a1 - a0) * (k0 - threshold) / (k0 - k1)
            )
            result.update(status="ARRESTED", arrest_a_m=arrest)
            return result
        try:
            total += _segment_cycles(a1 - a0, k0, k1, c, m)
        except (OverflowError, ZeroDivisionError, ValueError) as error:
            raise ValueError("growth integration exceeds numerical range") from error
    result["cycles"] = scalar(total, "integrated cycles", positive=True)
    return result
