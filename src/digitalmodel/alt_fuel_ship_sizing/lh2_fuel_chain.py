# ABOUTME: LH2 fuel-chain sizing: endurance/powering -> fuel mass, boil-off (BOG)
# ABOUTME: heat-leak model, and net/gross tank volume with ullage and margins.
"""LH2 fuel-chain sizing (concept-design screening).

Energy chain (all efficiencies on a lower-heating-value basis)::

    shaft power / eta_electric + hotel load = electrical demand
    electrical demand / eta_fuel_cell       = fuel LHV power
    fuel LHV power / LHV                    = fuel mass flow

Boil-off gas (BOG) model: a plain cylindrical tank (flat ends) of gross
volume ``V`` and slenderness ``L/D`` gives ``V = pi D^2 L / 4`` and wetted
surface ``A = pi D L + pi D^2 / 2``. The insulation heat leak is either a
user heat flux ``q`` (W/m2 of tank surface, the usual concept-stage
characterisation of a vacuum-insulated LH2 tank) or ``U (T_ambient -
T_tank)``. The BOG mass rate is ``Q / h_vap`` with the published latent
heat of vaporization (see :mod:`digitalmodel.alt_fuel_ship_sizing.constants`);
the conventional boil-off rate (BOR, %/day) is referenced to the full-load
liquid mass ``rho * V_gross * fill_fraction``.

Volume roll-up: net (liquid) volume = required fuel mass / liquid density;
gross volume = net / (1 - ullage fraction). When BOG is *lost* (vented, not
routed to the fuel cell) the lost mass over the voyage is added to the
required fuel mass, which grows the tank, which grows the heat leak -- a
fixed-point iteration that converges in a handful of passes at concept
scale. When BOG is *consumed* (fed to the fuel-cell plant, the normal LH2
arrangement) no extra fuel is carried, and the result flags whether the BOG
generation rate ever exceeds the average consumption rate (in which case
excess BOG must be vented or reliquefied and the "consumed" assumption is
optimistic).

Scope: concept-design screening only. Shaft-power demand is an *input*
(bare-hull resistance and propulsion are a separate lane); tank structure,
supports, and hold arrangement are out of scope.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field

from digitalmodel.alt_fuel_ship_sizing.constants import (
    H2_LOWER_HEATING_VALUE_MJ_PER_KG,
    HOURS_PER_DAY,
    LH2_DENSITY_KG_PER_M3,
    LH2_HEAT_OF_VAPORIZATION_KJ_PER_KG,
    LH2_NBP_K,
    SECONDS_PER_DAY,
)

BOG_HANDLING_MODES = ("consumed", "lost")


@dataclass(frozen=True)
class FuelProperties:
    """Fuel properties (defaults: published LH2 values, see ``constants``)."""

    density_kg_per_m3: float = LH2_DENSITY_KG_PER_M3
    lhv_mj_per_kg: float = H2_LOWER_HEATING_VALUE_MJ_PER_KG
    heat_of_vaporization_kj_per_kg: float = LH2_HEAT_OF_VAPORIZATION_KJ_PER_KG
    storage_temperature_k: float = LH2_NBP_K

    def __post_init__(self) -> None:
        for name in (
            "density_kg_per_m3",
            "lhv_mj_per_kg",
            "heat_of_vaporization_kj_per_kg",
            "storage_temperature_k",
        ):
            if getattr(self, name) <= 0.0:
                raise ValueError(f"FuelProperties.{name} must be positive")


@dataclass(frozen=True)
class TankParameters:
    """Concept-stage tank description.

    ``insulation_heat_flux_w_per_m2`` is the direct characterisation; when it
    is None, ``u_value_w_per_m2_k`` x (ambient - storage temperature) is used
    instead (one of the two must be given).
    """

    ullage_fraction: float = 0.08
    length_to_diameter: float = 5.0
    insulation_heat_flux_w_per_m2: float | None = None
    u_value_w_per_m2_k: float | None = None
    ambient_temperature_k: float = 293.15
    bog_handling: str = "consumed"

    def __post_init__(self) -> None:
        if not 0.0 <= self.ullage_fraction < 1.0:
            raise ValueError("TankParameters.ullage_fraction must be in [0, 1)")
        if self.length_to_diameter <= 0.0:
            raise ValueError("TankParameters.length_to_diameter must be positive")
        if self.bog_handling not in BOG_HANDLING_MODES:
            raise ValueError(
                f"TankParameters.bog_handling must be one of {BOG_HANDLING_MODES}"
            )
        if self.insulation_heat_flux_w_per_m2 is None:
            if self.u_value_w_per_m2_k is None:
                raise ValueError(
                    "TankParameters needs insulation_heat_flux_w_per_m2 or "
                    "u_value_w_per_m2_k"
                )
            if self.u_value_w_per_m2_k <= 0.0:
                raise ValueError("TankParameters.u_value_w_per_m2_k must be positive")
        elif self.insulation_heat_flux_w_per_m2 <= 0.0:
            raise ValueError(
                "TankParameters.insulation_heat_flux_w_per_m2 must be positive"
            )

    def heat_flux_w_per_m2(self, storage_temperature_k: float) -> float:
        if self.insulation_heat_flux_w_per_m2 is not None:
            return self.insulation_heat_flux_w_per_m2
        delta_t = self.ambient_temperature_k - storage_temperature_k
        if delta_t <= 0.0:
            raise ValueError(
                "ambient_temperature_k must exceed the fuel storage temperature"
            )
        return self.u_value_w_per_m2_k * delta_t


@dataclass(frozen=True)
class TankGeometry:
    """Plain cylinder (flat ends): V = pi D^2 L / 4, A = pi D L + pi D^2 / 2."""

    gross_volume_m3: float
    diameter_m: float
    length_m: float
    surface_area_m2: float


@dataclass(frozen=True)
class BoilOffResult:
    heat_leak_w: float
    bog_kg_per_day: float
    bog_rate_percent_per_day: float
    full_load_liquid_mass_kg: float


@dataclass(frozen=True)
class FuelChainResult:
    endurance_hours: float
    electrical_power_kw: float
    fuel_lhv_power_kw: float
    fuel_mass_flow_kg_per_h: float
    voyage_fuel_kg: float
    bog_lost_kg: float
    required_fuel_mass_kg: float
    net_tank_volume_m3: float
    gross_tank_volume_m3: float
    tank: TankGeometry
    boil_off: BoilOffResult
    bog_handling: str
    bog_exceeds_consumption: bool
    warnings: list[str] = field(default_factory=list)


def endurance_hours(distance_nm: float, speed_kn: float) -> float:
    """Route endurance in hours = distance [nm] / speed [kn]."""
    if distance_nm <= 0.0:
        raise ValueError("distance_nm must be positive")
    if speed_kn <= 0.0:
        raise ValueError("speed_kn must be positive")
    return distance_nm / speed_kn


def electrical_power_kw(
    shaft_power_kw: float,
    hotel_load_kw: float,
    eta_electric_drivetrain: float,
) -> float:
    """Electrical demand = shaft power / drivetrain efficiency + hotel load."""
    if shaft_power_kw < 0.0:
        raise ValueError("shaft_power_kw must be non-negative")
    if hotel_load_kw < 0.0:
        raise ValueError("hotel_load_kw must be non-negative")
    if not 0.0 < eta_electric_drivetrain <= 1.0:
        raise ValueError("eta_electric_drivetrain must be in (0, 1]")
    return shaft_power_kw / eta_electric_drivetrain + hotel_load_kw


def fuel_mass_flow_kg_per_h(
    electrical_demand_kw: float,
    eta_fuel_cell_lhv: float,
    lhv_mj_per_kg: float = H2_LOWER_HEATING_VALUE_MJ_PER_KG,
) -> float:
    """Fuel mass flow: (P_elec / eta_fc) [kW] * 3600 [s/h] / (LHV [kJ/kg])."""
    if electrical_demand_kw < 0.0:
        raise ValueError("electrical_demand_kw must be non-negative")
    if not 0.0 < eta_fuel_cell_lhv <= 1.0:
        raise ValueError("eta_fuel_cell_lhv must be in (0, 1]")
    if lhv_mj_per_kg <= 0.0:
        raise ValueError("lhv_mj_per_kg must be positive")
    fuel_lhv_power_kw = electrical_demand_kw / eta_fuel_cell_lhv
    return fuel_lhv_power_kw * 3.6 / lhv_mj_per_kg


def cylindrical_tank_geometry(
    gross_volume_m3: float, length_to_diameter: float
) -> TankGeometry:
    """Size a plain cylinder of given gross volume and L/D.

    ``V = pi D^2 L / 4`` with ``L = (L/D) D`` gives
    ``D = (4 V / (pi (L/D)))^(1/3)``; wetted surface (shell + 2 flat ends)
    ``A = pi D L + pi D^2 / 2``.
    """
    if gross_volume_m3 <= 0.0:
        raise ValueError("gross_volume_m3 must be positive")
    if length_to_diameter <= 0.0:
        raise ValueError("length_to_diameter must be positive")
    diameter = (4.0 * gross_volume_m3 / (math.pi * length_to_diameter)) ** (1.0 / 3.0)
    length = length_to_diameter * diameter
    area = math.pi * diameter * length + math.pi * diameter**2 / 2.0
    return TankGeometry(
        gross_volume_m3=gross_volume_m3,
        diameter_m=diameter,
        length_m=length,
        surface_area_m2=area,
    )


def boil_off(
    tank_geometry: TankGeometry,
    tank: TankParameters,
    fuel: FuelProperties = FuelProperties(),
) -> BoilOffResult:
    """Heat-leak boil-off: ``Q = q A``, ``mdot = Q / h_vap``.

    The conventional BOR (%/day) is referenced to the full-load liquid mass
    ``rho * V_gross * (1 - ullage)``.
    """
    q = tank.heat_flux_w_per_m2(fuel.storage_temperature_k)
    heat_leak_w = q * tank_geometry.surface_area_m2
    bog_kg_per_s = heat_leak_w / (fuel.heat_of_vaporization_kj_per_kg * 1000.0)
    bog_kg_per_day = bog_kg_per_s * SECONDS_PER_DAY
    full_load_mass = (
        fuel.density_kg_per_m3
        * tank_geometry.gross_volume_m3
        * (1.0 - tank.ullage_fraction)
    )
    bor = 100.0 * bog_kg_per_day / full_load_mass if full_load_mass > 0.0 else 0.0
    return BoilOffResult(
        heat_leak_w=heat_leak_w,
        bog_kg_per_day=bog_kg_per_day,
        bog_rate_percent_per_day=bor,
        full_load_liquid_mass_kg=full_load_mass,
    )


def size_fuel_chain(
    distance_nm: float,
    speed_kn: float,
    shaft_power_kw: float,
    hotel_load_kw: float,
    eta_electric_drivetrain: float,
    eta_fuel_cell_lhv: float,
    tank: TankParameters,
    fuel: FuelProperties = FuelProperties(),
    fuel_margin_fraction: float = 0.10,
    max_iterations: int = 50,
    tolerance_kg: float = 1e-6,
) -> FuelChainResult:
    """Compose the fuel chain into required fuel mass and tank volumes.

    ``fuel_margin_fraction`` is a flat design margin on the voyage fuel
    (weather / reserve, concept stage). With ``bog_handling == "lost"`` the
    BOG mass vented over the voyage is added to the required fuel and the
    tank volume is converged by fixed-point iteration.
    """
    if fuel_margin_fraction < 0.0:
        raise ValueError("fuel_margin_fraction must be non-negative")
    hours = endurance_hours(distance_nm, speed_kn)
    p_elec = electrical_power_kw(shaft_power_kw, hotel_load_kw, eta_electric_drivetrain)
    fuel_lhv_power_kw = p_elec / eta_fuel_cell_lhv
    mdot = fuel_mass_flow_kg_per_h(p_elec, eta_fuel_cell_lhv, fuel.lhv_mj_per_kg)
    voyage_fuel = mdot * hours * (1.0 + fuel_margin_fraction)

    required = voyage_fuel
    bog_lost = 0.0
    geometry = None
    bog = None
    for _ in range(max_iterations):
        net_volume = required / fuel.density_kg_per_m3
        gross_volume = net_volume / (1.0 - tank.ullage_fraction)
        geometry = cylindrical_tank_geometry(gross_volume, tank.length_to_diameter)
        bog = boil_off(geometry, tank, fuel)
        if tank.bog_handling == "lost":
            bog_lost = bog.bog_kg_per_day * hours / HOURS_PER_DAY
        new_required = voyage_fuel + bog_lost
        if abs(new_required - required) <= tolerance_kg:
            required = new_required
            break
        required = new_required
    else:
        raise RuntimeError(
            "fuel-chain tank sizing did not converge; check BOG inputs"
        )

    net_volume = required / fuel.density_kg_per_m3
    gross_volume = net_volume / (1.0 - tank.ullage_fraction)
    geometry = cylindrical_tank_geometry(gross_volume, tank.length_to_diameter)
    bog = boil_off(geometry, tank, fuel)

    consumption_kg_per_day = mdot * HOURS_PER_DAY
    bog_exceeds = bog.bog_kg_per_day > consumption_kg_per_day
    warnings: list[str] = []
    if tank.bog_handling == "consumed" and bog_exceeds:
        warnings.append(
            "BOG generation exceeds the average fuel-cell consumption rate; "
            "excess BOG must be vented or reliquefied (the 'consumed' "
            "assumption is optimistic)"
        )

    return FuelChainResult(
        endurance_hours=hours,
        electrical_power_kw=p_elec,
        fuel_lhv_power_kw=fuel_lhv_power_kw,
        fuel_mass_flow_kg_per_h=mdot,
        voyage_fuel_kg=voyage_fuel,
        bog_lost_kg=bog_lost,
        required_fuel_mass_kg=required,
        net_tank_volume_m3=net_volume,
        gross_tank_volume_m3=gross_volume,
        tank=geometry,
        boil_off=bog,
        bog_handling=tank.bog_handling,
        bog_exceeds_consumption=bog_exceeds,
        warnings=warnings,
    )
