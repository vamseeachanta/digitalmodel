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

Voyage profiles: :func:`size_fuel_chain_voyage` generalises the single sea
passage to an ordered list of legs -- :class:`SeaLeg` (distance at speed)
and :class:`PortLeg` (dwell at zero consumption). Port dwell accrues BOG
against the tank inventory with no burn to absorb it, so port BOG is always
added to the required fuel (even under "consumed" handling), which often
drives concept tank size for small LH2 ships on dwell-heavy schedules. The
same fixed-point tank-volume convergence applies; the result carries
per-leg BOG accounting. :func:`size_fuel_chain` (single passage) is kept as
a thin wrapper over a one-sea-leg profile, fully backward-compatible.

Scope: concept-design screening only. Shaft-power demand is an *input*
(bare-hull resistance and propulsion are a separate lane); tank structure,
supports, and hold arrangement are out of scope.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field

from digitalmodel.alt_fuel_ship_sizing.constants import (
    AMBIENT_TEMPERATURE_K,
    H2_LOWER_HEATING_VALUE_MJ_PER_KG,
    HOURS_PER_DAY,
    LH2_DENSITY_KG_PER_M3,
    LH2_HEAT_OF_VAPORIZATION_KJ_PER_KG,
    LH2_NBP_K,
    METHANOL_DENSITY_KG_PER_M3,
    METHANOL_HEAT_OF_VAPORIZATION_KJ_PER_KG,
    METHANOL_LOWER_HEATING_VALUE_MJ_PER_KG,
    NH3_HEAT_OF_VAPORIZATION_KJ_PER_KG,
    NH3_LIQUID_DENSITY_KG_PER_M3,
    NH3_LOWER_HEATING_VALUE_MJ_PER_KG,
    NH3_NBP_K,
    SECONDS_PER_DAY,
)

BOG_HANDLING_MODES = ("consumed", "lost")


@dataclass(frozen=True)
class FuelProperties:
    """Fuel properties (defaults: published LH2 values, see ``constants``).

    Cited presets for LH2, refrigerated anhydrous ammonia and methanol are
    available via :func:`fuel_preset` / :data:`FUEL_PRESETS`.
    """

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


def lh2_fuel_properties() -> FuelProperties:
    """Liquid parahydrogen at the NBP (20.28 K, 101.325 kPa).

    Sources: NBS Monograph 168 (1981); NIST Chemistry WebBook (Leachman et
    al. 2009); DOE Hydrogen Analysis Resource Center for the LHV. Validity:
    vacuum-insulated cryogenic storage at ~1 atm; the boil-off heat-leak
    model applies directly.
    """
    return FuelProperties()


def nh3_refrigerated_fuel_properties() -> FuelProperties:
    """Anhydrous ammonia, saturated liquid at the NBP (239.82 K, ~-33 degC).

    Sources: NIST Chemistry WebBook saturation properties (density ~682
    kg/m3, h_vap ~1370 kJ/kg); LHV 18.6 MJ/kg from NIST standard enthalpies
    of formation (gaseous-NH3 basis; handbooks quote 18.6-18.8 MJ/kg).

    Validity: the *fully refrigerated* storage condition only (insulated
    tank at ~1 atm); the boil-off heat-leak model applies. Pressurized
    ambient-temperature storage (~8.6 bar(a) at 20 degC) differs materially
    -- lower liquid density (~610 kg/m3) and no continuous boil-off -- and
    is *not* represented by this preset.
    """
    return FuelProperties(
        density_kg_per_m3=NH3_LIQUID_DENSITY_KG_PER_M3,
        lhv_mj_per_kg=NH3_LOWER_HEATING_VALUE_MJ_PER_KG,
        heat_of_vaporization_kj_per_kg=NH3_HEAT_OF_VAPORIZATION_KJ_PER_KG,
        storage_temperature_k=NH3_NBP_K,
    )


def methanol_fuel_properties() -> FuelProperties:
    """Methanol, ambient liquid (293.15 K reference).

    Sources: CRC Handbook (density 791.4 kg/m3 at 20 degC); NIST Chemistry
    WebBook (h_vap ~1100 kJ/kg at the 337.8 K NBP); LHV 19.9 MJ/kg derived
    from the NIST enthalpy of combustion (published values span 19.9-20.1).

    Validity: ambient-pressure liquid storage well below the boiling point
    -- there is *no* boil-off, so the BOG heat-leak model does not apply
    (storage temperature equals ambient; a U-value tank characterisation
    would see zero temperature difference and raise). The energy-chain and
    tank-volume roll-up (density, LHV) remain valid; ``h_vap`` is carried
    at its NBP value for API completeness only.
    """
    return FuelProperties(
        density_kg_per_m3=METHANOL_DENSITY_KG_PER_M3,
        lhv_mj_per_kg=METHANOL_LOWER_HEATING_VALUE_MJ_PER_KG,
        heat_of_vaporization_kj_per_kg=METHANOL_HEAT_OF_VAPORIZATION_KJ_PER_KG,
        storage_temperature_k=AMBIENT_TEMPERATURE_K,
    )


#: Cited fuel-property presets by key (see the factory docstrings for
#: sources and per-fuel storage-condition validity notes).
FUEL_PRESETS: dict[str, FuelProperties] = {
    "lh2": lh2_fuel_properties(),
    "nh3_refrigerated": nh3_refrigerated_fuel_properties(),
    "methanol": methanol_fuel_properties(),
}


def fuel_preset(name: str) -> FuelProperties:
    """Look up a cited fuel-property preset by key.

    Keys: ``lh2``, ``nh3_refrigerated``, ``methanol``.
    """
    try:
        return FUEL_PRESETS[name]
    except KeyError:
        raise ValueError(
            f"unknown fuel preset {name!r}; available: {sorted(FUEL_PRESETS)}"
        ) from None


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
class SeaLeg:
    """Sea passage leg: ``distance_nm`` at ``speed_kn``.

    ``shaft_power_kw`` optionally overrides the voyage-level shaft power for
    this leg (e.g. from a speed-power curve); ``None`` uses the voyage-level
    value.
    """

    distance_nm: float
    speed_kn: float
    shaft_power_kw: float | None = None

    def __post_init__(self) -> None:
        if self.distance_nm <= 0.0:
            raise ValueError("SeaLeg.distance_nm must be positive")
        if self.speed_kn <= 0.0:
            raise ValueError("SeaLeg.speed_kn must be positive")
        if self.shaft_power_kw is not None and self.shaft_power_kw < 0.0:
            raise ValueError("SeaLeg.shaft_power_kw must be non-negative")

    @property
    def duration_h(self) -> float:
        return self.distance_nm / self.speed_kn


@dataclass(frozen=True)
class PortLeg:
    """Port dwell leg: ``dwell_h`` at zero fuel consumption.

    The tank keeps leaking heat during the dwell, so BOG accrues against the
    inventory with no burn to absorb it.
    """

    dwell_h: float

    def __post_init__(self) -> None:
        if self.dwell_h <= 0.0:
            raise ValueError("PortLeg.dwell_h must be positive")


VoyageLeg = SeaLeg | PortLeg


@dataclass(frozen=True)
class VoyageLegResult:
    """Per-leg accounting at the converged tank size."""

    index: int
    leg_type: str  # "sea" | "port"
    duration_h: float
    distance_nm: float  # 0.0 for port legs
    speed_kn: float  # 0.0 for port legs
    fuel_burned_kg: float
    bog_generated_kg: float
    bog_consumed_kg: float
    bog_lost_kg: float
    bog_exceeds_consumption: bool


@dataclass(frozen=True)
class VoyageFuelChainResult:
    total_voyage_hours: float
    sea_hours: float
    port_dwell_h: float
    total_distance_nm: float
    voyage_fuel_kg: float
    bog_lost_kg: float
    required_fuel_mass_kg: float
    net_tank_volume_m3: float
    gross_tank_volume_m3: float
    tank: TankGeometry
    boil_off: BoilOffResult
    bog_handling: str
    bog_exceeds_consumption: bool
    legs: list[VoyageLegResult] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)


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


def size_fuel_chain_voyage(
    legs: list[VoyageLeg],
    shaft_power_kw: float,
    hotel_load_kw: float,
    eta_electric_drivetrain: float,
    eta_fuel_cell_lhv: float,
    tank: TankParameters,
    fuel: FuelProperties = FuelProperties(),
    fuel_margin_fraction: float = 0.10,
    max_iterations: int = 50,
    tolerance_kg: float = 1e-6,
) -> VoyageFuelChainResult:
    """Size the fuel chain over an ordered voyage profile of legs.

    Sea legs burn fuel at their leg speed (leg shaft power, defaulting to
    the voyage-level ``shaft_power_kw``, plus hotel load through the
    drivetrain / fuel-cell efficiency chain). Port legs burn nothing but
    keep generating BOG against the inventory:

    - ``bog_handling == "consumed"``: sea-leg BOG feeds the fuel cell (no
      extra fuel carried; flagged when generation exceeds the leg's average
      consumption rate), while *port* BOG has no burn to absorb it and is
      always added to the required fuel.
    - ``bog_handling == "lost"``: all BOG (sea and port) is vented and
      added to the required fuel.

    ``fuel_margin_fraction`` is a flat design margin on the burned voyage
    fuel (weather / reserve, concept stage); BOG losses are added on top
    without margin. The tank volume is converged by the same fixed-point
    iteration as the single-passage sizing: lost BOG grows the required
    fuel, which grows the tank, which grows the heat leak.
    """
    if fuel_margin_fraction < 0.0:
        raise ValueError("fuel_margin_fraction must be non-negative")
    if not legs:
        raise ValueError("voyage profile needs at least one leg")
    if not any(isinstance(leg, SeaLeg) for leg in legs):
        raise ValueError("voyage profile needs at least one sea leg")

    # Per-leg quantities that do not depend on tank size.
    durations_h: list[float] = []
    burns_kg: list[float] = []
    mdots_kg_per_h: list[float] = []
    for leg in legs:
        if isinstance(leg, SeaLeg):
            leg_power = (
                shaft_power_kw if leg.shaft_power_kw is None else leg.shaft_power_kw
            )
            p_elec = electrical_power_kw(
                leg_power, hotel_load_kw, eta_electric_drivetrain
            )
            mdot = fuel_mass_flow_kg_per_h(
                p_elec, eta_fuel_cell_lhv, fuel.lhv_mj_per_kg
            )
            hours = endurance_hours(leg.distance_nm, leg.speed_kn)
        elif isinstance(leg, PortLeg):
            mdot = 0.0
            hours = leg.dwell_h
        else:
            raise TypeError(f"voyage legs must be SeaLeg or PortLeg, got {leg!r}")
        durations_h.append(hours)
        mdots_kg_per_h.append(mdot)
        burns_kg.append(mdot * hours)

    voyage_fuel = sum(burns_kg) * (1.0 + fuel_margin_fraction)

    def _bog_lost_total(bog_kg_per_day: float) -> float:
        lost = 0.0
        for leg, hours in zip(legs, durations_h):
            generated = bog_kg_per_day * hours / HOURS_PER_DAY
            if tank.bog_handling == "lost" or isinstance(leg, PortLeg):
                lost += generated
        return lost

    required = voyage_fuel
    for _ in range(max_iterations):
        net_volume = required / fuel.density_kg_per_m3
        gross_volume = net_volume / (1.0 - tank.ullage_fraction)
        geometry = cylindrical_tank_geometry(gross_volume, tank.length_to_diameter)
        bog = boil_off(geometry, tank, fuel)
        new_required = voyage_fuel + _bog_lost_total(bog.bog_kg_per_day)
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

    # Per-leg accounting at the converged tank size.
    leg_results: list[VoyageLegResult] = []
    bog_lost = 0.0
    exceeding_legs: list[int] = []
    for index, (leg, hours, mdot, burn) in enumerate(
        zip(legs, durations_h, mdots_kg_per_h, burns_kg)
    ):
        generated = bog.bog_kg_per_day * hours / HOURS_PER_DAY
        is_sea = isinstance(leg, SeaLeg)
        if tank.bog_handling == "lost" or not is_sea:
            leg_lost = generated
            leg_consumed = 0.0
        else:
            leg_lost = 0.0
            leg_consumed = generated
        bog_lost += leg_lost
        exceeds = is_sea and bog.bog_kg_per_day > mdot * HOURS_PER_DAY
        if exceeds and tank.bog_handling == "consumed":
            exceeding_legs.append(index)
        leg_results.append(
            VoyageLegResult(
                index=index,
                leg_type="sea" if is_sea else "port",
                duration_h=hours,
                distance_nm=leg.distance_nm if is_sea else 0.0,
                speed_kn=leg.speed_kn if is_sea else 0.0,
                fuel_burned_kg=burn,
                bog_generated_kg=generated,
                bog_consumed_kg=leg_consumed,
                bog_lost_kg=leg_lost,
                bog_exceeds_consumption=exceeds,
            )
        )

    bog_exceeds = any(item.bog_exceeds_consumption for item in leg_results)
    warnings: list[str] = []
    if exceeding_legs:
        legs_note = ", ".join(str(i) for i in exceeding_legs)
        warnings.append(
            "BOG generation exceeds the average fuel-cell consumption rate; "
            "excess BOG must be vented or reliquefied (the 'consumed' "
            f"assumption is optimistic) on sea leg(s) {legs_note}"
        )

    return VoyageFuelChainResult(
        total_voyage_hours=sum(durations_h),
        sea_hours=sum(
            hours
            for leg, hours in zip(legs, durations_h)
            if isinstance(leg, SeaLeg)
        ),
        port_dwell_h=sum(
            hours
            for leg, hours in zip(legs, durations_h)
            if isinstance(leg, PortLeg)
        ),
        total_distance_nm=sum(
            leg.distance_nm for leg in legs if isinstance(leg, SeaLeg)
        ),
        voyage_fuel_kg=voyage_fuel,
        bog_lost_kg=bog_lost,
        required_fuel_mass_kg=required,
        net_tank_volume_m3=net_volume,
        gross_tank_volume_m3=gross_volume,
        tank=geometry,
        boil_off=bog,
        bog_handling=tank.bog_handling,
        bog_exceeds_consumption=bog_exceeds,
        legs=leg_results,
        warnings=warnings,
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

    Single sea passage; kept as a backward-compatible wrapper over a
    one-sea-leg voyage profile (see :func:`size_fuel_chain_voyage`).
    ``fuel_margin_fraction`` is a flat design margin on the voyage fuel
    (weather / reserve, concept stage). With ``bog_handling == "lost"`` the
    BOG mass vented over the voyage is added to the required fuel and the
    tank volume is converged by fixed-point iteration.
    """
    hours = endurance_hours(distance_nm, speed_kn)
    p_elec = electrical_power_kw(shaft_power_kw, hotel_load_kw, eta_electric_drivetrain)
    mdot = fuel_mass_flow_kg_per_h(p_elec, eta_fuel_cell_lhv, fuel.lhv_mj_per_kg)
    voyage = size_fuel_chain_voyage(
        legs=[SeaLeg(distance_nm=distance_nm, speed_kn=speed_kn)],
        shaft_power_kw=shaft_power_kw,
        hotel_load_kw=hotel_load_kw,
        eta_electric_drivetrain=eta_electric_drivetrain,
        eta_fuel_cell_lhv=eta_fuel_cell_lhv,
        tank=tank,
        fuel=fuel,
        fuel_margin_fraction=fuel_margin_fraction,
        max_iterations=max_iterations,
        tolerance_kg=tolerance_kg,
    )
    return FuelChainResult(
        endurance_hours=hours,
        electrical_power_kw=p_elec,
        fuel_lhv_power_kw=p_elec / eta_fuel_cell_lhv,
        fuel_mass_flow_kg_per_h=mdot,
        voyage_fuel_kg=voyage.voyage_fuel_kg,
        bog_lost_kg=voyage.bog_lost_kg,
        required_fuel_mass_kg=voyage.required_fuel_mass_kg,
        net_tank_volume_m3=voyage.net_tank_volume_m3,
        gross_tank_volume_m3=voyage.gross_tank_volume_m3,
        tank=voyage.tank,
        boil_off=voyage.boil_off,
        bog_handling=voyage.bog_handling,
        bog_exceeds_consumption=voyage.bog_exceeds_consumption,
        warnings=voyage.warnings,
    )
