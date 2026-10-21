# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Liquid-only Cv and inherent-trim screening for tree valves and chokes.

Source: Emerson, Control Valve Handbook, sixth edition (August 2023),
sections 1.4 (Control Valve Functions and Characteristics Terminology) and
5.8 (Sizing Valves for Liquids), particularly 5.8.3 (Calculating the Required
Flow Coefficient). Public source:
https://www.emerson.com/documents/automation/control-valve-handbook-en-3661206.pdf

For turbulent, incompressible liquid without attached fitting corrections:
Q [US gpm] = Cv * sqrt(delta_p [psi] / specific_gravity).
Linear trim: Cv(x)=Cv_full*x. Ideal equal-percentage trim:
Cv(x)=Cv_full*R**(x-1) for x>0; x=0 is a separately assumed tight shutoff.
Installed characteristics, leakage, viscosity corrections, gas, flashing and
multiphase flow are not modelled. Gate and ball valves use supplied effective
Cv values, not an assumed universal trim or loss coefficient.

API 17D / API 6A are equipment-scope references only: their verified edition
and clause locators are unavailable here. No API qualification is asserted.
The liquid choking differential is a REQUIRED caller input, already evaluated
from the fluid vapour pressure and valve recovery data; it has no default.

``liquid_critical_drop`` supplies that differential for a valve without
attached fittings: FL**2 * (P1 - FF*Pv). Source: Fisher, Control Valve
Sourcebook — Oil & Gas (2013), ch. 3, p. 3-5, “Determining Allowable Sizing
Pressure Drop”; US gpm/psi flow basis: p. 3-3, Table 3-2. Public source:
https://www.emerson.com/is/content/emerson/en/final-control/pressure-management/jt-valves/documents/guide-control-valve-sourcebook-oil-gas-fisher-en-135836.pdf
FL and FF are required, independently qualified inputs. This helper accepts
positive unit-interval factors only; it does not determine equipment factors,
viscosity applicability, flashing behaviour or cavitation damage.
"""

from __future__ import annotations

import math
from dataclasses import dataclass


def _nonnegative(*values: float) -> None:
    if any(not math.isfinite(value) or value < 0 for value in values):
        raise ValueError("values must be finite and nonnegative")


def _positive(*values: float) -> None:
    _nonnegative(*values)
    if any(value == 0 for value in values):
        raise ValueError("values must be positive")


def liquid_critical_drop(
    upstream_psi: float,
    vapor_pressure_psi: float,
    recovery_factor: float,
    critical_pressure_ratio: float,
) -> float:
    """Return the no-attached-fittings liquid choking differential in psi.

    Both pressures are absolute; vapor pressure is evaluated at inlet
    temperature and must be below upstream pressure. Supply FL at the valve
    opening and FF from a qualified fluid basis. The helper's input domain is
    positive unit-interval factors, not equipment acceptance limits. Pass the
    result to ``liquid_choke_flow`` for the same inlet and valve condition.

    Fisher (2013), ch. 3 p. 3-5 gives the equation. Attached fittings require
    FLP/Fp instead of FL and remain outside this helper. Invalid inputs or a
    differential that underflows to zero raise ValueError.
    """
    _positive(upstream_psi, recovery_factor, critical_pressure_ratio)
    _nonnegative(vapor_pressure_psi)
    if vapor_pressure_psi >= upstream_psi:
        raise ValueError("vapor pressure must be below upstream pressure")
    # Unity is the normalized ratio boundary for this helper's input domain,
    # not a qualified valve limit (Fisher 2013, Table 3-1, pp. 3-4–3-5).
    if recovery_factor > 1 or critical_pressure_ratio > 1:
        raise ValueError("recovery and critical pressure factors must be <= 1")
    # Square and pressure correction: Fisher (2013), ch. 3 p. 3-5,
    # “Determining Allowable Sizing Pressure Drop”, no-attached-fittings case.
    corrected_pressure = upstream_psi - critical_pressure_ratio * vapor_pressure_psi
    # Evaluate FL * (FL * pressure) to retain representable final values when
    # FL squared alone underflows; algebraically identical to the cited square.
    critical_drop = recovery_factor * (recovery_factor * corrected_pressure)
    _positive(critical_drop)
    return critical_drop


def trim_cv(full_open_cv: float, opening: float, characteristic: str,
            rangeability: float | None = None) -> float:
    """Return ideal inherent Cv at fractional travel, with tight shutoff."""
    _positive(full_open_cv)
    _nonnegative(opening)
    if opening > 1 or characteristic not in ("linear", "equal-percentage"):
        raise ValueError("opening or characteristic is invalid")
    if characteristic == "linear":
        return full_open_cv * opening
    if rangeability is None or not math.isfinite(rangeability) or rangeability <= 1:
        raise ValueError("equal-percentage trim requires rangeability > 1")
    coefficient: float = full_open_cv * rangeability**(opening - 1) if opening else 0.0
    return coefficient


def valve_pressure_drop(flow_gpm: float, cv: float, specific_gravity: float) -> float:
    """Return the liquid loss in psi from effective Cv and US gpm."""
    _nonnegative(flow_gpm, cv)
    _positive(specific_gravity)
    if cv == 0:
        if flow_gpm:
            raise ValueError("a closed valve cannot pass nonzero flow")
        return 0.0
    return specific_gravity * (flow_gpm / cv)**2


def series_pressure_drop(flow_gpm: float, valve_cvs: tuple[float, ...],
                         specific_gravity: float) -> float:
    """Sum gate or ball valve losses in a series path at the same liquid rate."""
    _nonnegative(flow_gpm)
    _positive(specific_gravity)
    return sum(valve_pressure_drop(flow_gpm, cv, specific_gravity) for cv in valve_cvs)


@dataclass(frozen=True)
class ChokeFlow:
    """Liquid rate, capped differential and critical/subcritical regime."""

    flow_gpm: float
    effective_drop_psi: float
    regime: str


def liquid_choke_flow(upstream_psi: float, downstream_psi: float, cv: float,
                      specific_gravity: float, critical_drop_psi: float) -> ChokeFlow:
    """Limit liquid flow using the supplied choking differential in psi.

    Pressures are absolute and must be nonnegative; the downstream pressure
    cannot exceed the upstream pressure. Reverse flow is outside this model.
    """
    _nonnegative(upstream_psi, downstream_psi, cv)
    _positive(specific_gravity, critical_drop_psi)
    drop = upstream_psi - downstream_psi
    if drop < 0:
        raise ValueError("reverse pressure gradient is outside this model")
    effective = min(drop, critical_drop_psi)
    regime = "critical" if drop >= critical_drop_psi else "subcritical"
    return ChokeFlow(cv * math.sqrt(effective / specific_gravity), effective, regime)
