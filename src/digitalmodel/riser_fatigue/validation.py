"""Evidence and numerical validation for stress-based fatigue assessment."""

import math
from numbers import Real


def finite_number(value, name, *, positive=True):
    """Parse a finite scalar without treating booleans as engineering values."""
    if isinstance(value, bool) or not isinstance(value, (Real, str)):
        raise ValueError(f"{name} must be a finite number")
    try:
        number = float(value)
    except (ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must be a finite number") from exc
    if not math.isfinite(number) or (positive and number <= 0):
        raise ValueError(f"{name} must be finite and positive")
    return number


def evidence_text(value, name):
    """Require a caller-supplied basis; factual adequacy needs engineering review."""
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} requires a non-empty evidence basis")
    return value.strip()


def positive_damage(value, name):
    """Reject unknown, negative and unqualified zero damage before a verdict."""
    return finite_number(value, name)


def histogram_basis(value, period):
    """Cross-check the scaling duration against the supplied cycle-count basis."""
    if not isinstance(value, dict) or set(value) != {"period_years", "evidence"}:
        raise ValueError("histogram_basis requires period_years and evidence")
    basis_period = finite_number(value["period_years"], "histogram_basis period_years")
    evidence_text(value["evidence"], "histogram_basis evidence")
    if basis_period != period:
        raise ValueError("histogram_period_years must match histogram_basis period_years")
    return value
