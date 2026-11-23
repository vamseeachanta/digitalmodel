"""Protection relay coordination per IEEE C37.112.

Implements overcurrent (50/51) and differential (87) relay models,
IEEE C37.112 standard inverse-time characteristic curves, coordination
grading margin checks, and time-current intersection finding.

References
----------
IEEE C37.112-2018 — IEEE Standard Inverse-Time Characteristic
Equations for Overcurrent Relays.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum
from typing import Optional


# ── Enums ───────────────────────────────────────────────────────────


class RelayType(Enum):
    """Protection relay function number classification.

    Attributes
    ----------
    OVERCURRENT_50 : str
        Instantaneous overcurrent (ANSI 50).
    OVERCURRENT_51 : str
        Time-overcurrent (ANSI 51).
    DIFFERENTIAL_87 : str
        Differential protection (ANSI 87).
    """

    OVERCURRENT_50 = "overcurrent_50"
    OVERCURRENT_51 = "overcurrent_51"
    DIFFERENTIAL_87 = "differential_87"


class CurveType(Enum):
    """IEEE C37.112 inverse-time characteristic curve types.

    Attributes
    ----------
    MODERATELY_INVERSE : str
        Moderately inverse (MI) curve.
    VERY_INVERSE : str
        Very inverse (VI) curve.
    EXTREMELY_INVERSE : str
        Extremely inverse (EI) curve.
    DEFINITE_TIME : str
        Definite-time characteristic (flat).
    """

    MODERATELY_INVERSE = "moderately_inverse"
    VERY_INVERSE = "very_inverse"
    EXTREMELY_INVERSE = "extremely_inverse"
    DEFINITE_TIME = "definite_time"


# ── IEEE C37.112 curve constants ────────────────────────────────────
# t = TDS * (A / ((I/Ip)^p - 1) + B)

_CURVE_CONSTANTS: dict[CurveType, tuple[float, float, float]] = {
    CurveType.MODERATELY_INVERSE: (0.0515, 0.114, 0.02),
    CurveType.VERY_INVERSE: (19.61, 0.491, 2.0),
    CurveType.EXTREMELY_INVERSE: (28.2, 0.1217, 2.0),
}


# ── Dataclasses ─────────────────────────────────────────────────────


@dataclass
class OvercurrentRelay:
    """Overcurrent relay settings (ANSI 50/51).

    Parameters
    ----------
    relay_id : str
        Unique relay identifier.
    relay_type : RelayType
        OVERCURRENT_50 or OVERCURRENT_51.
    curve_type : CurveType
        Inverse-time curve characteristic.
    pickup_current_A : float
        Pickup (threshold) current [A]. Must be positive.
    time_dial_setting : float
        Time dial setting (TDS). Must be positive.
    instantaneous_pickup_A : float | None
        Instantaneous element pickup [A], if enabled.
    """

    relay_id: str
    relay_type: RelayType
    curve_type: CurveType
    pickup_current_A: float
    time_dial_setting: float
    instantaneous_pickup_A: Optional[float] = None

    def __post_init__(self) -> None:
        if self.pickup_current_A <= 0:
            raise ValueError(
                f"pickup_current_A must be positive, got {self.pickup_current_A}"
            )
        if self.time_dial_setting <= 0:
            raise ValueError(
                f"time_dial_setting must be positive, got {self.time_dial_setting}"
            )


@dataclass
class DifferentialRelay:
    """Differential relay settings (ANSI 87).

    Parameters
    ----------
    relay_id : str
        Unique relay identifier.
    slope_percent : float
        Percentage slope setting [%]. Must be between 0 and 100.
    minimum_operate_current_A : float
        Minimum differential current to operate [A]. Must be positive.
    restraint_current_A : float
        Restraint current setting [A].
    """

    relay_id: str
    slope_percent: float
    minimum_operate_current_A: float
    restraint_current_A: float

    def __post_init__(self) -> None:
        if not (0 < self.slope_percent <= 100):
            raise ValueError(
                f"slope_percent must be between 0 and 100, got {self.slope_percent}"
            )
        if self.minimum_operate_current_A <= 0:
            raise ValueError(
                "minimum_operate_current_A must be positive, "
                f"got {self.minimum_operate_current_A}"
            )


# ── IEEE C37.112 inverse-time curve ────────────────────────────────


def inverse_time_curve(
    fault_current_A: float,
    pickup_current_A: float,
    time_dial_setting: float,
    curve_type: CurveType,
) -> float:
    """Calculate relay operating time per IEEE C37.112.

    t = TDS * (A / ((I/Ip)^p - 1) + B)

    For DEFINITE_TIME, operating time equals the time dial setting.

    Parameters
    ----------
    fault_current_A : float
        Fault current magnitude [A].
    pickup_current_A : float
        Relay pickup current [A].
    time_dial_setting : float
        Time dial setting (TDS).
    curve_type : CurveType
        Inverse-time curve characteristic.

    Returns
    -------
    float
        Operating time [s].

    Raises
    ------
    ValueError
        If fault current does not exceed pickup current.
    """
    if fault_current_A <= pickup_current_A:
        raise ValueError(
            f"Fault current ({fault_current_A} A) must exceed "
            f"pickup current ({pickup_current_A} A)"
        )

    if curve_type == CurveType.DEFINITE_TIME:
        return time_dial_setting

    A, B, p = _CURVE_CONSTANTS[curve_type]
    M = (fault_current_A / pickup_current_A) ** p - 1.0
    return time_dial_setting * (A / M + B)


# ── Coordination check ─────────────────────────────────────────────


def coordination_check(
    upstream: OvercurrentRelay,
    downstream: OvercurrentRelay,
    fault_current_A: float,
    min_margin_s: float = 0.3,
) -> dict:
    """Check grading margin between upstream and downstream relays.

    The upstream relay must operate slower than the downstream relay
    by at least ``min_margin_s`` seconds at the given fault current.

    Parameters
    ----------
    upstream : OvercurrentRelay
        Upstream (backup) relay.
    downstream : OvercurrentRelay
        Downstream (primary) relay.
    fault_current_A : float
        Fault current for the coordination study [A].
    min_margin_s : float
        Minimum required grading margin [s]. Default 0.3 s.

    Returns
    -------
    dict
        Keys: ``pass`` (bool), ``margin_s`` (float),
        ``upstream_time_s`` (float), ``downstream_time_s`` (float).
    """
    t_up = inverse_time_curve(
        fault_current_A,
        upstream.pickup_current_A,
        upstream.time_dial_setting,
        upstream.curve_type,
    )
    t_down = inverse_time_curve(
        fault_current_A,
        downstream.pickup_current_A,
        downstream.time_dial_setting,
        downstream.curve_type,
    )
    margin = t_up - t_down
    return {
        "pass": margin >= min_margin_s,
        "margin_s": margin,
        "upstream_time_s": t_up,
        "downstream_time_s": t_down,
    }


# ── Time-current intersection ──────────────────────────────────────


def time_current_intersection(
    relay1: OvercurrentRelay,
    relay2: OvercurrentRelay,
    current_range: tuple[float, float],
    tolerance: float = 1e-3,
    max_iterations: int = 100,
) -> tuple[float, float] | None:
    """Find the current at which two relays have equal operating time.

    Uses the bisection method on the time difference function
    across the specified current range.

    Parameters
    ----------
    relay1 : OvercurrentRelay
        First relay.
    relay2 : OvercurrentRelay
        Second relay.
    current_range : tuple[float, float]
        (min_current_A, max_current_A) search range.
    tolerance : float
        Convergence tolerance on current [A].
    max_iterations : int
        Maximum bisection iterations.

    Returns
    -------
    tuple[float, float] | None
        (intersection_current_A, operating_time_s) or None if no
        intersection exists in the given range.
    """
    i_lo, i_hi = current_range
    # Ensure both relays can operate at range boundaries
    min_pickup = max(relay1.pickup_current_A, relay2.pickup_current_A)
    i_lo = max(i_lo, min_pickup * 1.01)

    if i_lo >= i_hi:
        return None

    def _delta(current: float) -> float:
        t1 = inverse_time_curve(
            current, relay1.pickup_current_A,
            relay1.time_dial_setting, relay1.curve_type,
        )
        t2 = inverse_time_curve(
            current, relay2.pickup_current_A,
            relay2.time_dial_setting, relay2.curve_type,
        )
        return t1 - t2

    d_lo = _delta(i_lo)
    d_hi = _delta(i_hi)

    # No sign change means no intersection in range
    if d_lo * d_hi > 0:
        return None

    for _ in range(max_iterations):
        i_mid = (i_lo + i_hi) / 2.0
        d_mid = _delta(i_mid)

        if abs(d_mid) < 1e-6 or (i_hi - i_lo) / 2.0 < tolerance:
            t_at_cross = inverse_time_curve(
                i_mid, relay1.pickup_current_A,
                relay1.time_dial_setting, relay1.curve_type,
            )
            return (i_mid, t_at_cross)

        if d_lo * d_mid < 0:
            i_hi = i_mid
            d_hi = d_mid
        else:
            i_lo = i_mid
            d_lo = d_mid

    return None
