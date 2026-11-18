# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Single-phase acoustic pressure-step screening; quantities or bare SI.

Joukowsky: dp=rho*a*dv; critical round-trip time tc=2L/a. Public basis:
US Bureau of Reclamation, Engineering Monograph 3, Welded Steel Penstocks,
'Effect of Water Hammer', equation (4), and figure 13 (elastic wave speed):
https://www.usbr.gov/tsc/techreferences/hydraulics_lab/pubs/EM/EM03.pdf

The task's min(1, tc/ramp_time) attenuation is an approximate linear-ramp
screen, not the monograph's gate-closure solution. This is not a time-domain
solver: reflections, changing cross sections, cavitation, gas, frictional
damping and fluid/formation coupling are excluded. No default acoustic or
material properties are provided; use measured or qualified properties.
"""

import math
from dataclasses import dataclass

from digitalmodel.units import Q_

from ._validation import finite, scalar


@dataclass(frozen=True)
class TransientResult:
    """Signed pressure [Pa], full-bore force [N], velocity [m/s], time [s].

    Positive velocity change means removed flow, giving a positive pressure
    change at a closing downstream boundary. Force is dp times the supplied
    flow area, not an evaluated tool activation load. Reflected wave signs
    and loads elsewhere in the well are outside this screen.
    """

    pressure_change: Q_
    force_change: Q_
    velocity_change: Q_
    critical_time: Q_
    attenuation: float


def pipe_wave_speed(
    density,
    bulk_modulus,
    diameter,
    wall_thickness,
    youngs_modulus,
    *,
    restraint_factor=1.0,
):
    """Return a=sqrt(K/[rho*(1+C*K*D/(E*t))]) [m/s].

    Thin-wall linear elastic pipe with homogeneous single-phase fluid.
    Figure 13 of the monograph uses C=1; the optional compliance multiplier
    C must be supplied for the actual axial support condition (0<=C<=1).
    C=0 is the rigid-wall limiting case. The default is representative,
    not a qualified casing/cement/formation support model.
    """
    rho = scalar(density, "kg/m**3", "density", positive=True)
    bulk = scalar(bulk_modulus, "Pa", "fluid bulk modulus", positive=True)
    d = scalar(diameter, "m", "internal diameter", positive=True)
    wall = scalar(wall_thickness, "m", "wall thickness", positive=True)
    elastic = scalar(youngs_modulus, "Pa", "pipe Young's modulus", positive=True)
    support = finite(restraint_factor, "restraint factor", maximum=1)
    compliance = 1 + support * bulk * d / (elastic * wall)
    return Q_(math.sqrt(bulk / (rho * compliance)), "m/s")


def critical_time(length, wave_speed):
    """Return 2L/a [s]; L is measured acoustic path length, not TVD."""
    path = scalar(length, "m", "acoustic path length", positive=True)
    speed = scalar(wave_speed, "m/s", "wave speed", positive=True)
    return Q_(2 * path / speed, "s")


def ramp_attenuation(length, wave_speed, ramp_time=0.0) -> float:
    """Return 1 for dt<=tc, otherwise tc/dt; zero dt denotes an abrupt step.

    Task-specified linear-ramp approximation, not an arbitrary pump/valve
    control law or a proven upper bound for a network with wave reflections.
    """
    tc = critical_time(length, wave_speed).to("s").magnitude
    duration = scalar(ramp_time, "s", "ramp time")
    return 1.0 if duration <= tc else tc / duration


def surge_pressure(density, wave_speed, velocity_change, length, ramp_time=0.0):
    """Return signed rho*a*dv*attenuation [Pa]; dv is removed velocity.

    Negative dv gives the pressure-drop counterpart at the same boundary;
    absolute pressure and vapor-pressure checks require a separate model.
    """
    rho = scalar(density, "kg/m**3", "density", positive=True)
    speed = scalar(wave_speed, "m/s", "wave speed", positive=True)
    change = scalar(velocity_change, "m/s", "removed velocity", minimum=-math.inf)
    attenuation = ramp_attenuation(length, wave_speed, ramp_time)
    return Q_(rho * speed * change * attenuation, "Pa")


def rate_step_transient(
    initial_rate,
    final_rate,
    flow_area,
    density,
    wave_speed,
    length,
    ramp_time=0.0,
) -> TransientResult:
    """Return a uniform-bore rate-step screen using dv=(Qi-Qf)/A.

    Rates must be nonnegative. Supply casing flow area for pump-down (not
    bypass area); this column-velocity step differs from local restriction
    loss. Reversal of column flow is outside this convenience interface.
    """
    initial = scalar(initial_rate, "m**3/s", "initial rate")
    final = scalar(final_rate, "m**3/s", "final rate")
    area = Q_(scalar(flow_area, "m**2", "flow area", positive=True), "m**2")
    change = Q_((initial - final) / area.magnitude, "m/s")
    pressure = surge_pressure(density, wave_speed, change, length, ramp_time)
    return TransientResult(
        pressure,
        (pressure * area).to("N"),
        change,
        critical_time(length, wave_speed),
        ramp_attenuation(length, wave_speed, ramp_time),
    )


def maximum_rate_step(
    allowable_pressure,
    density,
    wave_speed,
    flow_area,
    length,
    ramp_time=0.0,
):
    """Return abs(dQ) bound [m3/s] for a supplied acoustic pressure budget.

    Inverts dp=rho*a*(dQ/A)*attenuation. This is an incremental pressure
    budget, not a total surface-pressure rating or a pump operating window.
    The caller must also bound the available initial/final pump rate.
    """
    budget = scalar(allowable_pressure, "Pa", "allowable pressure increment")
    rho = scalar(density, "kg/m**3", "density", positive=True)
    speed = scalar(wave_speed, "m/s", "wave speed", positive=True)
    area = scalar(flow_area, "m**2", "flow area", positive=True)
    attenuation = ramp_attenuation(length, wave_speed, ramp_time)
    return Q_(budget * area / (rho * speed * attenuation), "m**3/s")
