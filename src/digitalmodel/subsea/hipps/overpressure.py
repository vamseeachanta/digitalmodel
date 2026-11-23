"""HIPPS overpressure-protection envelope — surge vs. design-pressure check.

API 17O requires the HIPPS to bring the protected downstream segment to a safe
state (valve closed) *before* the pressure at the protected boundary exceeds its
design / maximum-allowable operating pressure (MAOP).  This module provides the
deterministic envelope check that links the HIPPS response time to a bounding
linear pressure-rise (surge) rate.

Standard model (deterministic, conservative)
--------------------------------------------
The total HIPPS response time from the onset of the overpressure demand to a
fully blocked flow path is, per IEC 61511 SIF response-time accounting:

    t_response = t_detect + t_logic + t_close                            (1)

where ``t_detect`` is the transmitter / trip-amplifier delay, ``t_logic`` is the
logic-solver scan/processing delay, and ``t_close`` is the valve stroke (closure)
time.  During this window the protected pressure is bounded above by a linear
rise at the worst-case surge rate ``dpdt`` [pressure/time] from the initial
operating pressure ``p_initial``:

    p_at_close = p_initial + dpdt * t_response                           (2)

The protection envelope is satisfied when the pressure at closure plus a margin
stays at or below the segment design pressure ``p_design``:

    p_at_close <= p_design                                              (PASS)   (3)

    margin       = p_design - p_at_close                                 (4)
    utilisation  = p_at_close / p_design                                 (5)

This is intentionally a bounding linear-surge envelope, not a transient solver:
it gives a conservative screening result. A full method-of-characteristics water-
hammer simulation is deferred (see package docstring).

Units are consistent and caller-defined (e.g. all pressures in MPa, all times in
seconds, ``dpdt`` in MPa/s).  No unit conversion is performed.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class OverpressureResult:
    """Outcome of the HIPPS overpressure-protection envelope check.

    Attributes
    ----------
    t_response:
        Total response time = detect + logic + close (same time unit as inputs).
    p_at_close:
        Bounding protected-segment pressure at the instant of full closure.
    margin:
        ``p_design - p_at_close`` (positive = protected, negative = breach).
    utilisation:
        ``p_at_close / p_design`` (<= 1.0 means protected).
    passed:
        ``True`` when ``p_at_close <= p_design``.
    """

    t_response: float
    p_at_close: float
    margin: float
    utilisation: float
    passed: bool


def overpressure_envelope(
    p_initial: float,
    p_design: float,
    dpdt: float,
    t_close: float,
    t_detect: float = 0.0,
    t_logic: float = 0.0,
) -> OverpressureResult:
    """Evaluate the HIPPS overpressure-protection envelope (API 17O / IEC 61511).

    Parameters
    ----------
    p_initial:
        Operating pressure at the protected boundary at the onset of the demand.
    p_design:
        Design / maximum-allowable operating pressure of the protected segment
        (the envelope ceiling), must be > 0.
    dpdt:
        Worst-case (bounding) pressure-rise rate of the surge, must be >= 0,
        same pressure unit as ``p_*`` per unit time.
    t_close:
        HIPPS valve closure (stroke) time, must be >= 0.
    t_detect:
        Transmitter / trip-amplifier detection delay (default 0).
    t_logic:
        Logic-solver scan/processing delay (default 0).

    Returns
    -------
    OverpressureResult

    Raises
    ------
    ValueError
        On non-positive ``p_design``, negative ``dpdt``, or any negative time.
    """
    if p_design <= 0.0:
        raise ValueError(f"p_design must be > 0, got {p_design!r}")
    if dpdt < 0.0:
        raise ValueError(f"dpdt must be >= 0, got {dpdt!r}")
    for name, val in (("t_close", t_close), ("t_detect", t_detect), ("t_logic", t_logic)):
        if val < 0.0:
            raise ValueError(f"{name} must be >= 0, got {val!r}")

    t_response = t_detect + t_logic + t_close          # eq. (1)
    p_at_close = p_initial + dpdt * t_response          # eq. (2)
    margin = p_design - p_at_close                      # eq. (4)
    utilisation = p_at_close / p_design                 # eq. (5)
    passed = p_at_close <= p_design                     # eq. (3)

    return OverpressureResult(
        t_response=t_response,
        p_at_close=p_at_close,
        margin=margin,
        utilisation=utilisation,
        passed=passed,
    )
