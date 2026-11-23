# ABOUTME: Endurance / fuel-cost trade-study generator: sweeps speed and
# ABOUTME: wind-assist on/off through the LH2 fuel chain -> comparison table.
"""Endurance / fuel-cost trade-study generator (concept-design screening).

Sweeps design variables (service speed, wind-assist on/off) through the
LH2 fuel-chain sizing and emits one comparison row per case. Rows are plain
dicts so the table drops straight into a CSV and into the ``report_pack``
workflow's ``results.tables`` input.

Shaft power vs speed: either a user ``speed_power_curve`` (piecewise-linear
interpolation between calm-water prediction points from the resistance
lane) or, absent a curve, the classic concept-stage cube law
``P = P_ref * (V / V_ref)^n`` with ``n = 3`` by default (Admiralty-type
scaling; acceptable for small speed deltas at screening stage only).
"""

from __future__ import annotations

from dataclasses import dataclass, field

from digitalmodel.alt_fuel_ship_sizing.lh2_fuel_chain import (
    FuelProperties,
    TankParameters,
    size_fuel_chain,
)
from digitalmodel.alt_fuel_ship_sizing.wind_assist import (
    ThrustMatrix,
    WindRoseBin,
    wind_assist_saving,
)


@dataclass(frozen=True)
class WindAssistInputs:
    matrix: ThrustMatrix
    wind_rose: list[WindRoseBin]
    propulsive_efficiency: float


@dataclass(frozen=True)
class TradeStudyInputs:
    distance_nm: float
    reference_speed_kn: float
    reference_shaft_power_kw: float
    hotel_load_kw: float
    eta_electric_drivetrain: float
    eta_fuel_cell_lhv: float
    tank: TankParameters
    fuel: FuelProperties = field(default_factory=FuelProperties)
    fuel_margin_fraction: float = 0.10
    power_speed_exponent: float = 3.0
    speed_power_curve: list[tuple[float, float]] | None = None
    wind: WindAssistInputs | None = None
    fuel_price_per_kg: float | None = None


def shaft_power_at_speed(inputs: TradeStudyInputs, speed_kn: float) -> float:
    """Shaft power [kW] at ``speed_kn`` from the curve or the cube law."""
    if speed_kn <= 0.0:
        raise ValueError("speed_kn must be positive")
    curve = inputs.speed_power_curve
    if curve:
        points = sorted((float(s), float(p)) for s, p in curve)
        if len(points) < 2:
            raise ValueError("speed_power_curve needs at least 2 points")
        if not points[0][0] <= speed_kn <= points[-1][0]:
            raise ValueError(
                f"speed {speed_kn} kn outside speed_power_curve range "
                f"[{points[0][0]}, {points[-1][0]}]"
            )
        for (s0, p0), (s1, p1) in zip(points, points[1:]):
            if s0 <= speed_kn <= s1:
                if s1 == s0:
                    return p0
                return p0 + (p1 - p0) * (speed_kn - s0) / (s1 - s0)
    return inputs.reference_shaft_power_kw * (
        speed_kn / inputs.reference_speed_kn
    ) ** inputs.power_speed_exponent


def run_trade_study(
    inputs: TradeStudyInputs,
    speeds_kn: list[float],
    wind_assist_options: list[bool],
) -> list[dict]:
    """One comparison row per (speed, wind-assist) case."""
    if not speeds_kn:
        raise ValueError("speeds_kn must be a non-empty list")
    if not wind_assist_options:
        raise ValueError("wind_assist_options must be a non-empty list")
    if any(opt for opt in wind_assist_options) and inputs.wind is None:
        raise ValueError(
            "wind_assist_options includes True but no wind-assist inputs given"
        )

    rows: list[dict] = []
    for speed in speeds_kn:
        shaft_power = shaft_power_at_speed(inputs, speed)
        for wind_on in wind_assist_options:
            saving_kw = 0.0
            saving_fraction = 0.0
            effective_power = shaft_power
            if wind_on:
                wind = wind_assist_saving(
                    inputs.wind.matrix,
                    inputs.wind.wind_rose,
                    ship_speed_kn=speed,
                    required_shaft_power_kw=shaft_power,
                    propulsive_efficiency=inputs.wind.propulsive_efficiency,
                )
                saving_kw = wind.expected_power_saving_kw
                saving_fraction = wind.saving_fraction
                effective_power = wind.effective_shaft_power_kw

            chain = size_fuel_chain(
                distance_nm=inputs.distance_nm,
                speed_kn=speed,
                shaft_power_kw=effective_power,
                hotel_load_kw=inputs.hotel_load_kw,
                eta_electric_drivetrain=inputs.eta_electric_drivetrain,
                eta_fuel_cell_lhv=inputs.eta_fuel_cell_lhv,
                tank=inputs.tank,
                fuel=inputs.fuel,
                fuel_margin_fraction=inputs.fuel_margin_fraction,
            )

            row = {
                "case_id": f"v{speed:g}kn_{'wind' if wind_on else 'nowind'}",
                "speed_kn": speed,
                "wind_assist": wind_on,
                "endurance_hours": chain.endurance_hours,
                "shaft_power_kw": shaft_power,
                "wind_power_saving_kw": saving_kw,
                "wind_saving_fraction": saving_fraction,
                "effective_shaft_power_kw": effective_power,
                "fuel_mass_flow_kg_per_h": chain.fuel_mass_flow_kg_per_h,
                "required_fuel_mass_kg": chain.required_fuel_mass_kg,
                "net_tank_volume_m3": chain.net_tank_volume_m3,
                "gross_tank_volume_m3": chain.gross_tank_volume_m3,
                "bog_rate_percent_per_day": chain.boil_off.bog_rate_percent_per_day,
                "bog_exceeds_consumption": chain.bog_exceeds_consumption,
            }
            if inputs.fuel_price_per_kg is not None:
                row["fuel_cost"] = (
                    inputs.fuel_price_per_kg * chain.required_fuel_mass_kg
                )
            rows.append(row)

    # relative fuel column: saving vs the no-wind case at the same speed
    no_wind = {
        row["speed_kn"]: row["required_fuel_mass_kg"]
        for row in rows
        if not row["wind_assist"]
    }
    for row in rows:
        base = no_wind.get(row["speed_kn"])
        if base and base > 0.0:
            row["fuel_saving_vs_no_wind_percent"] = (
                100.0 * (base - row["required_fuel_mass_kg"]) / base
            )
        else:
            row["fuel_saving_vs_no_wind_percent"] = 0.0
    return rows
