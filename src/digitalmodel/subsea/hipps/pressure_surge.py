# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Elastic single-reach liquid surge with reservoir and valve boundaries.

The Joukowsky relation requested by issue 1996 is dp = rho*a*dv. Wave speed
is derived from the combined fluid and wall compliance:
1/(rho*a^2) = 1/K + restraint*D/(E*t). The caller supplies restraint for the
applicable thin-wall support model; no material or standards constants default.

The frictionless continuity and momentum equations imply characteristic
invariants p +/- Z*q along dx/dt = +/-a, where Z=rho*a/area. Their intersection
sets interior pressure and flow. The upstream reservoir fixes pressure. The
valve obeys q = conductance*opening*sqrt(max(p-p_downstream, 0)), with initial
conductance calibrated from the supplied steady state. The exact characteristic
grid has dt=dx/a. Initial pressure is uniform because friction is excluded.

All units are SI (Pa absolute, m, kg/m3, m3/s, s). This is a single-phase,
frictionless, constant-property model, without cavitation or column separation.
A computed non-positive absolute pressure fails closed. Positive pressure alone
does not establish absence of cavitation; a qualified vapor-pressure margin is
still required. Peak pressure covers all grid nodes; dpdt is the maximum sampled
positive valve-pressure slope for overpressure_envelope in Pa/s. A step closure
has unbounded physical slope: its reported finite slope depends on dt and is
not a grid-independent safety bound or a qualified HIPPS response-time basis.
API 17O and IEC 61511 edition/clause compliance is not established here.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from .valve_sizing import _nonnegative, _positive, closure_fraction


@dataclass(frozen=True)
class SurgeResult:
    """Computed SI valve history, all-node peak and sampled rise rate."""

    time_s: tuple[float, ...]
    valve_pressure_pa: tuple[float, ...]
    valve_flow_m3_s: tuple[float, ...]
    peak_pressure_pa: float
    dpdt_pa_s: float
    time_step_s: float


def wave_speed(
    *, density: float, bulk_modulus: float, diameter: float,
    wall_thickness: float, elastic_modulus: float, restraint_factor: float,
) -> float:
    """Calculate acoustic speed from fluid and thin-wall elastic compliance.

    Args:
        density: Liquid density [kg/m3].
        bulk_modulus: Liquid bulk modulus [Pa].
        diameter: Internal diameter [m].
        wall_thickness: Pipe wall thickness [m].
        elastic_modulus: Wall Young's modulus [Pa].
        restraint_factor: Nonnegative caller-qualified wall-support factor.

    Returns:
        Acoustic wave speed [m/s].
    """
    _positive(density=density, bulk_modulus=bulk_modulus, diameter=diameter,
              wall_thickness=wall_thickness, elastic_modulus=elastic_modulus)
    _nonnegative(restraint_factor=restraint_factor)
    compliance = 1 / bulk_modulus + restraint_factor * diameter / (
        elastic_modulus * wall_thickness
    )
    return math.sqrt(1 / (density * compliance))


def joukowsky_rise(density: float, acoustic_speed: float, velocity_change: float) -> float:
    """Calculate instantaneous-closure pressure rise from a speed reduction.

    Args:
        density: Liquid density [kg/m3].
        acoustic_speed: Wave speed [m/s].
        velocity_change: Nonnegative reduction in axial velocity [m/s].

    Returns:
        Bounding pressure rise [Pa] for the stated frictionless liquid model.
    """
    _positive(density=density, acoustic_speed=acoustic_speed)
    _nonnegative(velocity_change=velocity_change)
    return density * acoustic_speed * velocity_change


def _valve_boundary(characteristic: float, impedance: float,
                    downstream: float, conductance: float) -> tuple[float, float]:
    """Solve p+Z*q=C and a one-way square-root valve relation."""
    drop = max(characteristic - downstream, 0.0)
    denominator = math.hypot(impedance * conductance, 2 * math.sqrt(drop))
    denominator += impedance * conductance
    flow = 2 * conductance * drop / denominator if denominator else 0.0
    return characteristic - impedance * flow, flow


def simulate_surge(
    *, length: float, diameter: float, density: float, acoustic_speed: float,
    initial_flow: float, reservoir_pressure: float, downstream_pressure: float,
    closure_time: float, duration: float, cells: int,
    characteristic: str = "linear", rangeability: float | None = None,
) -> SurgeResult:
    """Advance a liquid reach on a Courant-one characteristic grid.

    Args:
        length: Reach length [m].
        diameter: Internal diameter [m].
        density: Constant liquid density [kg/m3].
        acoustic_speed: Caller-qualified wave speed [m/s].
        initial_flow: Initial positive volumetric flow [m3/s].
        reservoir_pressure: Constant upstream absolute pressure [Pa].
        downstream_pressure: Absolute pressure after the valve [Pa].
        closure_time: Valve stroke time [s]; zero means instantaneous closure.
        duration: Requested simulation duration [s], at least one time step.
        cells: Number of equal reach intervals, integer >= 2.
        characteristic: linear or equal_percentage conductance law.
        rangeability: Required for equal_percentage closure.

    Returns:
        Pressure/flow history, peak and sampled dpdt. The last time rounds up
        to the next characteristic-grid instant, by less than one time step.

    Raises:
        ValueError: For invalid geometry, boundary data or model-domain exit.
    """
    _positive(length=length, diameter=diameter, density=density,
              acoustic_speed=acoustic_speed, initial_flow=initial_flow,
              reservoir_pressure=reservoir_pressure, duration=duration)
    _nonnegative(downstream_pressure=downstream_pressure, closure_time=closure_time)
    if isinstance(cells, bool) or not isinstance(cells, int) or cells < 2:
        raise ValueError("cells must be an integer >= 2")
    if reservoir_pressure <= downstream_pressure:
        raise ValueError("initial steady forward flow requires reservoir > downstream")
    closure_fraction(0, closure_time, characteristic, rangeability=rangeability)
    dt = length / (cells * acoustic_speed)
    if duration < dt:
        raise ValueError("duration must include at least one characteristic step")
    area = math.pi * diameter**2 / 4
    impedance = density * acoustic_speed / area
    conductance = initial_flow / math.sqrt(reservoir_pressure - downstream_pressure)
    return _march(cells, dt, math.ceil(duration / dt), impedance, conductance,
                  initial_flow, reservoir_pressure, downstream_pressure,
                  closure_time, characteristic, rangeability)


def _march(
    cells: int, dt: float, steps: int, impedance: float, conductance: float,
    initial_flow: float, reservoir: float, downstream: float,
    closure_time: float, law: str, rangeability: float | None,
) -> SurgeResult:
    """Intersect characteristics and enforce both boundaries at each step."""
    pressure = [reservoir] * (cells + 1)
    flow = [initial_flow] * (cells + 1)
    times, valve_p, valve_q = [0.0], [reservoir], [initial_flow]
    peak, slope = reservoir, 0.0
    for step in range(1, steps + 1):
        plus = [p + impedance * q for p, q in zip(pressure, flow)]
        minus = [p - impedance * q for p, q in zip(pressure, flow)]
        pressure = [reservoir] + [
            (plus[i - 1] + minus[i + 1]) / 2 for i in range(1, cells)
        ] + [0.0]
        flow = [(reservoir - minus[1]) / impedance] + [
            (plus[i - 1] - minus[i + 1]) / (2 * impedance)
            for i in range(1, cells)
        ] + [0.0]
        opening = closure_fraction(step * dt, closure_time, law,
                                   rangeability=rangeability)
        pressure[-1], flow[-1] = _valve_boundary(
            plus[-2], impedance, downstream, conductance * opening
        )
        if min(pressure) <= 0 or not all(math.isfinite(p) for p in pressure):
            raise ValueError("single-phase model invalid: non-positive/non-finite pressure")
        peak = max(peak, max(pressure))
        slope = max(slope, (pressure[-1] - valve_p[-1]) / dt)
        times.append(step * dt)
        valve_p.append(pressure[-1])
        valve_q.append(flow[-1])
    return SurgeResult(tuple(times), tuple(valve_p), tuple(valve_q), peak, slope, dt)
