# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Single-density containment operating points and caller-bounded shut-in.

The inflow equations reuse the forms already documented in
production_engineering/ipr_models.py: linear PI, Vogel and the empirical
back-pressure equation. The vertical lift uses hydrostatic head plus the
Darcy-Weisbach bore loss documented in subsea/umbilicals/chemical_flow.py:
p_bottom = p_wellhead + rho*g*h + f*(L/D)*rho*(q/area)^2/2.

The native bisection core intersects inflow with vertical lift and the
caller-supplied pressure-dependent choke capacity. A choke callback may wrap
hipps.valve_sizing liquid_capacity or gas_mass_capacity; its output must be
converted to the same local volumetric-rate basis as inflow. A fixed-density
gas approximation requires caller-qualified local volume conversion. This is
not a compressible, multiphase or transient wellbore model.

All pressures are absolute Pa and rates are local m3/s. Geometry, gravity,
density, friction and inflow/choke coefficients are explicit inputs. No
catalogue rating or proprietary calibration is supplied. The staged trajectory
is a sequence of quasi-steady states, with required caller-qualified transient
pressure allowances. These allowances are inputs, not computed surge results.
The maximum bounded wellhead pressure, including each stage's allowance, is
passed to check_containment via an explicitly equivalent reservoir pressure
(peak + rho*g*h). The unchanged pressure check also evaluates the physical
terminal shut-in state separately. No API 17W / API STD 53 edition or clause
compliance is claimed; that qualification remains open under issue 1997.
"""

from __future__ import annotations

import math
from collections.abc import Callable, Sequence
from dataclasses import dataclass
from typing import Any

from ..hipps.valve_sizing import _nonnegative, _positive
from .containment_capacity import ContainmentResult, G, check_containment


@dataclass(frozen=True)
class FlowOperatingPoint:
    """Local-volume operating point with both intersection residuals."""

    flow_rate_m3_s: float
    wellhead_pressure_pa: float
    bottomhole_pressure_pa: float
    opening: float
    ipr_residual_m3_s: float
    choke_residual_m3_s: float


@dataclass(frozen=True)
class StagedContainment:
    """Quasi-steady states, explicit transient bounds and pressure checks."""

    time_s: tuple[float, ...]
    stages: tuple[FlowOperatingPoint, ...]
    transient_pressure_allowance_pa: tuple[float, ...]
    peak_pressure_pa: float
    peak_time_s: float
    terminal_containment: ContainmentResult
    peak_containment: ContainmentResult


def _inflow_inputs(pressure: float, reservoir_pressure: float) -> None:
    """Validate absolute pressures without inferring an engineering basis."""
    _positive(reservoir_pressure=reservoir_pressure)
    _nonnegative(pressure=pressure)


def linear_inflow(pressure: float, reservoir_pressure: float, productivity: float) -> float:
    """Calculate q = PI*(p_res-p_bottom), clamped against reverse production.

    Args:
        pressure: Absolute bottomhole pressure [Pa].
        reservoir_pressure: Absolute reservoir pressure [Pa].
        productivity: Caller-qualified PI [m3/(s*Pa)].

    Returns:
        Local volumetric flow [m3/s]; valid for the qualified linear regime.
    """
    _inflow_inputs(pressure, reservoir_pressure)
    _positive(productivity=productivity)
    return productivity * max(reservoir_pressure - pressure, 0.0)


def vogel_inflow(pressure: float, reservoir_pressure: float, maximum_rate: float) -> float:
    """Calculate the existing repository Vogel form in a local SI basis.

    Args:
        pressure: Absolute bottomhole pressure [Pa].
        reservoir_pressure: Absolute reservoir pressure [Pa].
        maximum_rate: Caller-qualified zero-bottomhole-pressure rate [m3/s].

    Returns:
        Local volumetric flow [m3/s] for a qualified solution-gas-drive regime.
    """
    _inflow_inputs(pressure, reservoir_pressure)
    _positive(maximum_rate=maximum_rate)
    ratio = min(pressure / reservoir_pressure, 1.0)
    return max(maximum_rate * (1 - 0.2 * ratio - 0.8 * ratio**2), 0.0)


def gas_backpressure_inflow(
    pressure: float, reservoir_pressure: float, coefficient: float, exponent: float,
) -> float:
    """Calculate the repository's empirical pressure-squared inflow form.

    Args:
        pressure: Absolute bottomhole pressure [Pa].
        reservoir_pressure: Absolute reservoir pressure [Pa].
        coefficient: Caller-qualified C [m3/(s*Pa**(2*exponent))].
        exponent: Caller-qualified exponent in the existing model's 0.5–1 range.

    Returns:
        Local volumetric rate [m3/s], after caller qualification of gas basis.

    Raises:
        ValueError: For invalid coefficients or exponent.
    """
    _inflow_inputs(pressure, reservoir_pressure)
    _positive(coefficient=coefficient, exponent=exponent)
    if not 0.5 <= exponent <= 1:
        raise ValueError("back-pressure exponent must be between 0.5 and 1")
    return coefficient * math.pow(max(reservoir_pressure**2 - pressure**2, 0), exponent)


def _rate(function: Callable[[float], float], pressure: float) -> float:
    """Reject invalid callback rates before root solving or reporting."""
    value = function(pressure)
    _nonnegative(callback_rate=value)
    return value


def _bisect(function: Callable[[float], float], low: float, high: float) -> float:
    """Solve a bracketed residual to floating-point resolution, up to 100 steps."""
    left, right = function(low), function(high)
    if left == 0:
        return low
    if right == 0:
        return high
    if not math.isfinite(left) or not math.isfinite(right) or left * right > 0:
        raise ValueError("finite opposite-sign root bracket is required")
    for _ in range(100):
        middle = (low + high) / 2
        if middle == low or middle == high:
            return middle
        value = function(middle)
        if not math.isfinite(value):
            raise ValueError("non-finite operating-point residual")
        if value == 0:
            return middle
        if (value > 0) == (left > 0):
            low, left = middle, value
        else:
            high = middle
    raise ValueError("operating-point bisection did not converge")


def solve_operating_point(
    *, inflow: Callable[[float], float], reservoir_pressure: float,
    column_density: float, vertical_depth: float, gravity: float,
    bore_length: float, bore_diameter: float, friction_factor: float,
    outlet_pressure: float, choke_capacity: Callable[[float], float], opening: float,
) -> FlowOperatingPoint:
    """Intersect monotonic inflow, constant-density lift and choke capacity.

    Args:
        inflow: Nonnegative decreasing q(p_bottom) [m3/s], zero at reservoir.
        reservoir_pressure: Absolute reservoir pressure [Pa].
        column_density: Qualified constant fluid density [kg/m3].
        vertical_depth: Reservoir datum to wellhead vertical rise [m].
        gravity: Gravitational acceleration [m/s2].
        bore_length: Combined wellbore and stack flow-path length [m].
        bore_diameter: Equivalent constant internal bore [m].
        friction_factor: Caller-qualified Darcy friction factor, nonnegative.
        outlet_pressure: Absolute choke outlet pressure [Pa].
        choke_capacity: Nonnegative increasing full-open q(p_wellhead) [m3/s].
        opening: Remaining capacity fraction in [0, 1].

    Returns:
        Local volumetric rate and pressures, with IPR and choke residuals.
        A blocked or fully closed path returns the physical static state.

    Raises:
        ValueError: For invalid inputs, a dry-column limit or invalid callbacks.
    """
    _positive(reservoir_pressure=reservoir_pressure, column_density=column_density,
              gravity=gravity, bore_diameter=bore_diameter)
    _nonnegative(vertical_depth=vertical_depth, bore_length=bore_length,
                 friction_factor=friction_factor, outlet_pressure=outlet_pressure,
                 opening=opening)
    if opening > 1:
        raise ValueError("opening must be <= 1")
    head = column_density * gravity * vertical_depth
    static = reservoir_pressure - head
    if static <= 0 or _rate(inflow, reservoir_pressure) != 0:
        raise ValueError("positive static pressure and zero inflow at reservoir are required")
    if opening == 0 or outlet_pressure >= static:
        return FlowOperatingPoint(0, static, reservoir_pressure, opening, 0, 0)
    area = math.pi * bore_diameter**2 / 4
    resistance = friction_factor * bore_length / bore_diameter * column_density / (2 * area**2)
    return _intersect(inflow, choke_capacity, head, resistance, outlet_pressure,
                      static, opening)


def _intersect(
    inflow: Callable[[float], float], choke: Callable[[float], float],
    head: float, resistance: float, outlet: float, static: float, opening: float,
) -> FlowOperatingPoint:
    """Nest the IPR–VLP root inside the pressure-dependent choke intersection."""
    def supply(pressure: float) -> float:
        upper = _rate(inflow, pressure + head)
        return _bisect(
            lambda q: _rate(inflow, pressure + head + resistance*q*q) - q,
            0.0, upper,
        )

    def discharge(pressure: float) -> float:
        if pressure <= outlet:
            return 0.0
        return opening * _rate(choke, pressure)

    pressure = _bisect(
        lambda p: supply(p) - discharge(p), outlet, static,
    )
    flow = supply(pressure)
    bottom = pressure + head + resistance * flow**2
    return FlowOperatingPoint(flow, pressure, bottom, opening,
                              _rate(inflow, bottom) - flow,
                              discharge(pressure) - flow)


def staged_shut_in(
    *, time_s: Sequence[float], openings: Sequence[float],
    transient_pressure_allowance_pa: Sequence[float], rated_working_pressure: float,
    **well_inputs: Any,
) -> StagedContainment:
    """Evaluate quasi-steady closure stages with explicit transient bounds.

    Args:
        time_s: Strictly increasing nonnegative stage times [s].
        openings: Nonincreasing capacity fractions, ending at zero.
        transient_pressure_allowance_pa: Required nonnegative transient
            pressure bounds at each stage [Pa]; these are not computed here.
        rated_working_pressure: Qualified stack rated working pressure [Pa].
        **well_inputs: solve_operating_point inputs, excluding opening.

    Returns:
        Stages and peak/terminal checks. The peak check's equivalent reservoir
        pressure includes the supplied transient bound, not a new reservoir value.

    Raises:
        ValueError: For incomplete closure, invalid history or mismatched units.
    """
    _validate_stages(time_s, openings, transient_pressure_allowance_pa)
    _positive(rated_working_pressure=rated_working_pressure)
    stages = tuple(solve_operating_point(opening=o, **well_inputs) for o in openings)
    bounded = [s.wellhead_pressure_pa + a for s, a in zip(
        stages, transient_pressure_allowance_pa
    )]
    index = max(range(len(bounded)), key=bounded.__getitem__)
    reservoir = float(well_inputs["reservoir_pressure"])
    depth = float(well_inputs["vertical_depth"])
    density = float(well_inputs["column_density"])
    gravity = float(well_inputs["gravity"])
    # The existing pressure check uses its own G. Convert the equivalent depth
    # so rho*G*depth_equivalent equals the explicitly supplied hydrostatic head.
    equivalent_depth = depth * gravity / G
    terminal = check_containment(rated_working_pressure, reservoir, equivalent_depth, density)
    equivalent_reservoir = bounded[index] + density * gravity * depth
    peak = check_containment(rated_working_pressure, equivalent_reservoir,
                             equivalent_depth, density)
    return StagedContainment(tuple(time_s), stages, tuple(transient_pressure_allowance_pa),
                             bounded[index], time_s[index], terminal, peak)


def _validate_stages(
    times: Sequence[float], openings: Sequence[float], allowances: Sequence[float],
) -> None:
    """Require an ordered closure history with an explicit bound at every stage."""
    if not times or len(times) != len(openings) or len(times) != len(allowances):
        raise ValueError("nonempty equally sized stage histories are required")
    for time, opening, allowance in zip(times, openings, allowances):
        _nonnegative(time=time, opening=opening, transient_allowance=allowance)
        if opening > 1:
            raise ValueError("opening must be <= 1")
    if any(b <= a for a, b in zip(times, times[1:])):
        raise ValueError("stage times must increase strictly")
    if any(b > a for a, b in zip(openings, openings[1:])) or openings[-1] != 0:
        raise ValueError("stages must close monotonically to zero")
