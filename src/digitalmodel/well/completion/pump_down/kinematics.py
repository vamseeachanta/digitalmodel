"""Steady line speed from continuity and restriction-force balance.

Derived task model: solve Fres=K*rho*[(Q-vt*Ac)/Ab]**2*Ac/2.
Continuity and minor loss: DOE-HDBK-1012/3-92, Fluid Flow. Field-unit
conversions: NIST SP 811 (2008), Appendix B.8; shared Pint registry defines
the petroleum barrel as 42 US gallons. No adopted design-slip coefficient.
"""

import math
from dataclasses import dataclass

from digitalmodel.units import Q_

from ._validation import scalar
from .geometry import BypassGeometry
from .restriction import restriction


@dataclass(frozen=True)
class LineSpeed:
    """Speed [m/s], bypass fraction sigma=1-vt/(Q/Ac), and stall state.

    Sigma is None when Q=0 because that ratio is undefined.
    """

    line_speed: Q_
    slip_fraction: float | None
    stall: bool


def ideal_line_speed(rate_bpm, id_in) -> float:
    """Return rounded field shortcut 1029.4*Q/ID**2 [ft/min].

    Derived from Q/Ac and NIST SP 811 (2008), Appendix B.8 unit conversions.
    The task's rounded 1029.4 differs by about 0.0014% from exact conversions;
    the solved speed uses exact shared-registry units, not this shortcut.
    """
    rate = scalar(rate_bpm, "bbl/min", "rate")
    diameter = scalar(id_in, "inch", "casing ID", positive=True)
    return 1029.4 * rate / diameter**2


def solve_line_speed(
    rate, geometry: BypassGeometry, density, resistance, k=2.0
) -> LineSpeed:
    """Return vt=Q/Ac-beta*sqrt(2*Fres/(K*rho*Ac)), bounded at stall.

    Derived force balance cited in module docstring. K basis: restriction.py.
    Negative resistance denotes gravity-assisted conveyance; that branch
    needs cable braking/reverse-bypass physics and is rejected explicitly.
    Quantity inputs or bare SI values; Fres>=Fstatic gives exactly zero speed.
    """
    q = scalar(rate, "m**3/s", "rate")
    load = scalar(
        resistance, "N", "resistance (gravity-assisted loads are unsupported)"
    )
    result = restriction(rate, Q_(0, "m/s"), geometry, density, k)
    static = result.driving_force.to("N").magnitude
    if q == 0:
        return LineSpeed(Q_(0, "m/s"), None, True)
    if load >= static:
        return LineSpeed(Q_(0, "m/s"), 1.0, True)
    sigma = math.sqrt(load / static)
    speed = q / geometry.casing_area.to("m**2").magnitude * (1 - sigma)
    return LineSpeed(Q_(speed, "m/s"), sigma, False)


@dataclass(frozen=True)
class StageTransit:
    """Constant-speed travel time [s] and pumped volume [m3]."""

    time: Q_
    volume: Q_ | None


def stage_transit(distance, line_speed, rate) -> StageTransit:
    """Return t=L/v and V=Qt for a constant-speed stage (kinematic identities).

    DOE-HDBK-1012/3-92, Fluid Flow, 'Volumetric Flow Rate'. A positive
    distance at zero speed gives infinite time and no finite stage volume.
    A zero-distance stage takes zero time, including at zero rate/speed.
    """
    length = scalar(distance, "m", "stage distance")
    speed = scalar(line_speed, "m/s", "line speed")
    q = scalar(rate, "m**3/s", "rate")
    if length == 0:
        return StageTransit(Q_(0, "s"), Q_(0, "m**3"))
    if speed == 0:
        return StageTransit(Q_(math.inf, "s"), None)
    time = length / speed
    return StageTransit(Q_(time, "s"), Q_(q * time, "m**3"))
