# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Bounded lift-riser flow screens, not engineering acceptance decisions.

F1: Slurry Transport (2021 web edition), section 4.2, equation (4.2-5).
F2: same text, section 7.8, opening definition of the deposit Froude number.
F3: phase-volume continuity, independently derived in the companion guide.
F4: University Physics Volume 2 (2016), section 3.3, ideal isothermal work.
References, derivations and applicability: docs/domains/risers/lift-riser-analysis.md.

All empirical inputs and applicability envelopes are supplied by callers.
Missing or out-of-domain calibration returns NOT_EVALUATED; invalid mathematical
inputs raise ValueError. EVALUATED describes arithmetic within the attested
domain, not qualification of either the domain or the equipment.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isfinite, log1p, sqrt
from types import MappingProxyType
from typing import Mapping

from digitalmodel.subsea.lift_design_basis import Source, finite_number


@dataclass(frozen=True)
class Interval:
    """Finite inclusive bounds, in the named input's SI unit; points allowed."""

    lower: float
    upper: float

    def __post_init__(self) -> None:
        if (
            not finite_number(self.lower)
            or not finite_number(self.upper)
            or self.lower > self.upper
        ):
            raise ValueError("interval requires ordered finite real bounds")

    def contains(self, value: float) -> bool:
        return finite_number(value) and self.lower <= value <= self.upper


@dataclass(frozen=True)
class Calibration:
    """Caller-attested model-specific domain with required source conditions.

    Ranges must cover every named numeric input exactly. For F1 the derived
    particle_reynolds is also required. Source conditions must additionally
    establish geometry, particle shape/grading, fluid, temperature, pressure
    and other physical restrictions; numeric bounds alone do not establish use.
    """

    model: str
    source: Source
    ranges: Mapping[str, Interval]

    def __post_init__(self) -> None:
        if not isinstance(self.model, str) or not self.model.strip():
            raise ValueError("calibration model is required")
        if not isinstance(self.source, Source):
            raise ValueError("calibration source is required")
        if (
            not isinstance(self.ranges, Mapping)
            or not self.ranges
            or any(
                not isinstance(k, str) or not k.strip() or not isinstance(v, Interval)
                for k, v in self.ranges.items()
            )
        ):
            raise ValueError("calibration requires named intervals")
        object.__setattr__(self, "ranges", MappingProxyType(dict(self.ranges)))


@dataclass(frozen=True)
class FlowScreen:
    """EVALUATED or NOT_EVALUATED, without a design acceptance claim."""

    status: str
    value: float | None
    unit: str
    reason: str


def _positive(values: Mapping[str, float]) -> None:
    for name, value in values.items():
        if not finite_number(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")


def _domain(
    model: str,
    values: Mapping[str, float],
    calibration: Calibration | None,
    *,
    derived: tuple[str, ...] = (),
) -> str:
    if calibration is None:
        return "source-backed calibration is required"
    if not isinstance(calibration, Calibration):
        raise ValueError("calibration must be a Calibration record")
    if calibration.model != model:
        return "calibration model does not match this screen"
    if set(calibration.ranges) != set(values) | set(derived):
        return "calibration must cover every input and required derived quantity"
    for name, value in values.items():
        if not calibration.ranges[name].contains(value):
            return f"outside calibration range: {name}"
    return ""


def _speed(reason: str, value: float | None = None) -> FlowScreen:
    if value is not None and (not isfinite(value) or value <= 0):
        return FlowScreen(
            "NOT_EVALUATED", None, "m/s", "calculation exceeded finite positive range"
        )
    return FlowScreen("NOT_EVALUATED" if reason else "EVALUATED", value, "m/s", reason)


def settling_screen(
    *,
    particle_diameter_m: float,
    solids_density_kg_m3: float,
    liquid_density_kg_m3: float,
    dynamic_viscosity_pa_s: float,
    gravity_m_s2: float,
    drag_coefficient: float,
    calibration: Calibration | None,
) -> FlowScreen:
    """F1 isolated sphere, steady terminal settling in a Newtonian liquid.

    F1 (4.2-5), sphere shape factor unity: w²=4*g*d*(rho_s/rho_l-1)/(3*Cd).
    The 4/3 comes from sphere volume/projected area and dynamic pressure.
    Re=rho_l*w*d/mu must fall in the supplied Cd domain. No shape, wall,
    hindered-settling or polydisperse correction is inferred.
    """
    values = dict(
        particle_diameter_m=particle_diameter_m,
        solids_density_kg_m3=solids_density_kg_m3,
        liquid_density_kg_m3=liquid_density_kg_m3,
        dynamic_viscosity_pa_s=dynamic_viscosity_pa_s,
        gravity_m_s2=gravity_m_s2,
        drag_coefficient=drag_coefficient,
    )
    _positive(values)
    if solids_density_kg_m3 <= liquid_density_kg_m3:
        raise ValueError("settling requires a heavier solid")
    reason = _domain(
        "isolated_sphere", values, calibration, derived=("particle_reynolds",)
    )
    if reason:
        return _speed(reason)
    speed = sqrt(
        4
        * gravity_m_s2
        * particle_diameter_m
        * (solids_density_kg_m3 / liquid_density_kg_m3 - 1)
        / (3 * drag_coefficient)
    )
    reynolds = (
        liquid_density_kg_m3 * speed * particle_diameter_m / dynamic_viscosity_pa_s
    )
    if not calibration.ranges["particle_reynolds"].contains(reynolds):
        return _speed("derived particle_reynolds outside drag calibration")
    return _speed("", speed)


def deposition_screen(
    *,
    pipe_id_m: float,
    particle_diameter_m: float,
    solids_density_kg_m3: float,
    liquid_density_kg_m3: float,
    dynamic_viscosity_pa_s: float,
    solids_volume_fraction: float,
    gravity_m_s2: float,
    deposition_factor: float,
    calibration: Calibration | None,
) -> FlowScreen:
    """F2 horizontal slurry deposit-limit velocity, never a vertical limit.

    v=FL*sqrt(2*g*D*(rho_s/rho_l-1)); the 2 is F2's Froude convention.
    FL must be sourced for this particle distribution, concentration and fluid.
    No universal FL, margin or inclined-pipe extrapolation is supplied.
    """
    values = dict(
        pipe_id_m=pipe_id_m,
        particle_diameter_m=particle_diameter_m,
        solids_density_kg_m3=solids_density_kg_m3,
        liquid_density_kg_m3=liquid_density_kg_m3,
        dynamic_viscosity_pa_s=dynamic_viscosity_pa_s,
        solids_volume_fraction=solids_volume_fraction,
        gravity_m_s2=gravity_m_s2,
        deposition_factor=deposition_factor,
    )
    _positive(values)
    if (
        particle_diameter_m >= pipe_id_m
        or solids_density_kg_m3 <= liquid_density_kg_m3
        or solids_volume_fraction >= 1
    ):
        raise ValueError("require d<D, rho_s>rho_l and a solids fraction below unity")
    reason = _domain("horizontal_deposition", values, calibration)
    if reason:
        return _speed(reason)
    speed = deposition_factor * sqrt(
        2 * gravity_m_s2 * pipe_id_m * (solids_density_kg_m3 / liquid_density_kg_m3 - 1)
    )
    return _speed("", speed)


def vertical_transport_screen(
    *,
    slip_velocity_m_s: float,
    solids_volume_fraction: float,
    required_solids_velocity_m_s: float,
    calibration: Calibration | None,
) -> FlowScreen:
    """F3 steady upward two-phase continuity with sourced effective slip.

    j=(1-C)*v_l+C*v_s, w=v_l-v_s => j=(1-C)*w+v_s. Unit coefficients
    follow volume conservation. C is local inventory, never delivered fraction.
    This excludes gas, restart, beds and unqualified dilute-slip substitution.
    """
    values = dict(
        slip_velocity_m_s=slip_velocity_m_s,
        solids_volume_fraction=solids_volume_fraction,
        required_solids_velocity_m_s=required_solids_velocity_m_s,
    )
    _positive(values)
    if solids_volume_fraction >= 1:
        raise ValueError("solids volume fraction must be below unity")
    reason = _domain("vertical_slip", values, calibration)
    if reason:
        return _speed(reason)
    return _speed(
        "",
        (1 - solids_volume_fraction) * slip_velocity_m_s + required_solids_velocity_m_s,
    )


@dataclass(frozen=True)
class AirliftEnergy:
    """Isothermal expansion energy and liquid-flow bound, not an operating point."""

    status: str
    gas_power_w: float | None
    liquid_flow_m3_s: float | None
    reason: str


def airlift_energy_screen(
    *,
    injection_pressure_abs_pa: float,
    outlet_pressure_abs_pa: float,
    gas_flow_at_outlet_m3_s: float,
    liquid_density_kg_m3: float,
    gravity_m_s2: float,
    lift_above_surface_m: float,
    lift_efficiency: float,
    calibration: Calibration | None,
) -> AirliftEnergy:
    """F4 ideal isothermal expansion: Pout*Qg_out*ln(Pin/Pout).

    Derivation uses F4 W=nRT*ln(Vout/Vin), ideal-gas pressure-volume balance,
    and liquid potential-power rho*g*Q*H. Efficiency (0,1] is an explicit
    expansion-to-lift assumption, not compressor efficiency. Reservoir surface
    and discharge pressures are equal. Pressure losses and injection pressure
    must be established separately; inputs are absolute pressures and outlet
    gas volume at the same isothermal temperature. No solids, gas dissolution,
    deep-pressure nonideality or three-phase capacity prediction is included.
    """
    values = dict(
        injection_pressure_abs_pa=injection_pressure_abs_pa,
        outlet_pressure_abs_pa=outlet_pressure_abs_pa,
        gas_flow_at_outlet_m3_s=gas_flow_at_outlet_m3_s,
        liquid_density_kg_m3=liquid_density_kg_m3,
        gravity_m_s2=gravity_m_s2,
        lift_above_surface_m=lift_above_surface_m,
        lift_efficiency=lift_efficiency,
    )
    _positive(values)
    if injection_pressure_abs_pa <= outlet_pressure_abs_pa or lift_efficiency > 1:
        raise ValueError("require expansion and efficiency no greater than unity")
    reason = _domain("isothermal_energy", values, calibration)
    if reason:
        return AirliftEnergy("NOT_EVALUATED", None, None, reason)
    power = (
        outlet_pressure_abs_pa
        * gas_flow_at_outlet_m3_s
        * log1p(
            (injection_pressure_abs_pa - outlet_pressure_abs_pa)
            / outlet_pressure_abs_pa
        )
    )
    potential_power_per_flow = (
        liquid_density_kg_m3 * gravity_m_s2 * lift_above_surface_m
    )
    if not isfinite(potential_power_per_flow) or potential_power_per_flow <= 0:
        return AirliftEnergy(
            "NOT_EVALUATED", None, None, "calculation exceeded finite positive range"
        )
    flow = lift_efficiency * power / potential_power_per_flow
    if not all(isfinite(v) and v > 0 for v in (power, flow)):
        return AirliftEnergy(
            "NOT_EVALUATED", None, None, "calculation exceeded finite positive range"
        )
    return AirliftEnergy("EVALUATED", power, flow, "")


def slugging_screen() -> FlowScreen:
    """Expose the unavailable regime check rather than assuming a slug limit.

    Candidate source: Fluids 5(4), 216 (2020), doi:10.3390/fluids5040216.
    The publisher returned HTTP 403 when checked. No equation, constant or
    transition threshold from this unread source is implemented.
    """
    return FlowScreen(
        "NOT_EVALUATED",
        None,
        "1",
        "readable public source and qualified flow-regime calibration are required",
    )
