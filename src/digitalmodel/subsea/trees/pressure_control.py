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
