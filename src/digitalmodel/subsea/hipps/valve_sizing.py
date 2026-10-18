# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Caller-coefficient valve sizing and actuator closure laws.

Public equation basis: Emerson, Control Valve Handbook, sixth edition,
sections 5.8.2, 5.8.3 and 5.9.2–5.9.3, available at
https://www.emerson.com/is/content/emerson/en/final-control/flow-controls/documents/d101881x012.pdf
(read 2026-10-14). The liquid worked arithmetic is in section 5.8.4.

The handbook describes the ISA-75.01 / IEC 60534-2-1 sizing method. This
module does not certify standards compliance. Unit constants N1/N6, adjusted
piping/recovery factors, gas choking ratio and fluid properties are required
caller inputs; no licensed tables or default equipment ratings are supplied.
Absolute pressure is required for vapor-pressure and gas relations. Cv or Kv
may be used only with its corresponding caller-supplied unit constant.

Liquid: q = N1 * Fp * C * sqrt(min(dp, (FLP/Fp)^2*(p1-FF*pv))/SG).
Gas mass: w = N6 * Fp * C * Y * sqrt(x_sizing*p1*rho1), where
x_sizing = min((p1-p2)/p1, x_choked), Y = 1-x_sizing/(3*x_choked).
Gas output is mass rate, not standard or actual volume rate. Returned units
are those specified by N1/N6. Factors remain fixed during coefficient inversion;
a catalogue-dependent iteration is the caller's responsibility.
"""

from __future__ import annotations

import math
from dataclasses import dataclass


@dataclass(frozen=True)
class ValveCapacity:
    """Flow capacity in caller-selected units with its choking disposition."""

    rate: float
    effective_drop: float
    choked: bool


def _positive(**values: float) -> None:
    """Reject non-finite or non-positive required quantities."""
    for name, value in values.items():
        if not math.isfinite(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")


def _nonnegative(**values: float) -> None:
    """Reject non-finite or negative quantities."""
    for name, value in values.items():
        if not math.isfinite(value) or value < 0:
            raise ValueError(f"{name} must be finite and nonnegative")


def liquid_capacity(
    *, coefficient: float, p_upstream: float, p_downstream: float,
    specific_gravity: float, unit_constant: float, piping_factor: float,
    recovery_factor: float, critical_pressure_ratio: float, vapor_pressure: float,
) -> ValveCapacity:
    """Calculate liquid capacity with explicit adjusted sizing factors.

    Args:
        coefficient: Cv or Kv consistent with unit_constant, nonnegative.
        p_upstream: Absolute inlet pressure in caller-selected pressure units.
        p_downstream: Absolute outlet pressure in the same units.
        specific_gravity: Fluid density divided by reference water density.
        unit_constant: N1 for the selected coefficient, flow and pressure units.
        piping_factor: Fp, including attached fittings.
        recovery_factor: FLP, including attached fittings.
        critical_pressure_ratio: FF from a qualified fluid-property basis.
        vapor_pressure: Absolute vapor pressure in the same pressure units.

    Returns:
        Capacity, sizing pressure drop and whether liquid flow is choked.

    Raises:
        ValueError: For invalid, reverse-flow or flashing-inlet inputs.
    """
    _positive(p_upstream=p_upstream, specific_gravity=specific_gravity,
              unit_constant=unit_constant, piping_factor=piping_factor,
              recovery_factor=recovery_factor,
              critical_pressure_ratio=critical_pressure_ratio)
    _nonnegative(coefficient=coefficient, p_downstream=p_downstream,
                 vapor_pressure=vapor_pressure)
    if p_downstream > p_upstream or vapor_pressure >= p_upstream:
        raise ValueError("forward flow and a liquid inlet are required")
    limit = (recovery_factor / piping_factor)**2 * (
        p_upstream - critical_pressure_ratio * vapor_pressure
    )
    _positive(choked_pressure_drop=limit)
    drop = p_upstream - p_downstream
    effective = min(drop, limit)
    rate = unit_constant * piping_factor * coefficient * math.sqrt(
        effective / specific_gravity
    )
    return ValveCapacity(rate, effective, drop >= limit)


def required_liquid_coefficient(*, rate: float, **inputs: float) -> float:
    """Invert the liquid relation while holding caller factors fixed.

    Args:
        rate: Required nonnegative liquid rate in the units defined by N1.
        **inputs: All liquid_capacity inputs except coefficient.

    Returns:
        Required Cv or Kv in the coefficient convention defined by N1.

    Raises:
        ValueError: If a positive rate is requested at zero pressure drop.
    """
    _nonnegative(rate=rate)
    unit = liquid_capacity(coefficient=1.0, **inputs).rate
    if rate == 0:
        return 0.0
    if unit == 0:
        raise ValueError("positive flow requires positive pressure drop")
    return rate / unit


def gas_mass_capacity(
    *, coefficient: float, p_upstream: float, p_downstream: float,
    density: float, unit_constant: float, piping_factor: float, choked_ratio: float,
) -> ValveCapacity:
    """Calculate gas mass capacity using a caller-qualified choking ratio.

    Args:
        coefficient: Cv or Kv consistent with unit_constant, nonnegative.
        p_upstream: Absolute inlet pressure in units selected for N6.
        p_downstream: Absolute outlet pressure in the same units.
        density: Inlet gas density in the units selected for N6.
        unit_constant: N6 for the chosen mass-rate, pressure and density units.
        piping_factor: Adjusted Fp, including attached fittings.
        choked_ratio: F_gamma*xTP, a positive pressure-drop ratio at most one.

    Returns:
        Mass capacity, sizing pressure drop and whether flow is choked.

    Raises:
        ValueError: For invalid physical inputs or reverse flow.
    """
    _positive(p_upstream=p_upstream, density=density, unit_constant=unit_constant,
              piping_factor=piping_factor, choked_ratio=choked_ratio)
    _nonnegative(coefficient=coefficient, p_downstream=p_downstream)
    if choked_ratio > 1 or p_downstream > p_upstream:
        raise ValueError("choked_ratio must be <= 1 and forward flow is required")
    ratio = (p_upstream - p_downstream) / p_upstream
    effective = min(ratio, choked_ratio)
    expansion = 1 - effective / (3 * choked_ratio)
    rate = unit_constant * piping_factor * coefficient * expansion * math.sqrt(
        effective * p_upstream * density
    )
    return ValveCapacity(rate, effective * p_upstream, ratio >= choked_ratio)


def stroke_time(*, stroke_length: float, actuator_speed: float) -> float:
    """Calculate stroke time from measured travel and loaded actuator speed.

    Args:
        stroke_length: Full travel in metres or radians, positive.
        actuator_speed: Loaded travel rate in matching units per second.

    Returns:
        Stroke time in seconds, excluding detection and logic delays.
    """
    _positive(stroke_length=stroke_length, actuator_speed=actuator_speed)
    return stroke_length / actuator_speed


def closure_fraction(
    elapsed: float, closure_time: float, characteristic: str,
    *, rangeability: float | None = None,
) -> float:
    """Return normalized valve conductance during idealized closure.

    Args:
        elapsed: Time after closure demand [s], nonnegative.
        closure_time: Full stroke time [s]; zero means step closure.
        characteristic: linear or equal_percentage conductance law.
        rangeability: Required ratio > 1 for equal_percentage.

    Returns:
        Remaining conductance fraction. Equal-percentage travel reaches
        1/rangeability just before seating, then closes fully at stroke end.

    Raises:
        ValueError: For invalid times, an unknown law or missing rangeability.
    """
    _nonnegative(elapsed=elapsed, closure_time=closure_time)
    if characteristic not in ("linear", "equal_percentage"):
        raise ValueError("unknown closure characteristic")
    if characteristic == "equal_percentage":
        if rangeability is None or not math.isfinite(rangeability) or rangeability <= 1:
            raise ValueError("equal_percentage requires finite rangeability > 1")
    if elapsed >= closure_time:
        return 0.0
    travel = elapsed / closure_time
    if characteristic == "linear":
        return 1 - travel
    if rangeability is None:
        raise ValueError("equal_percentage requires rangeability")
    return math.pow(rangeability, -travel)
