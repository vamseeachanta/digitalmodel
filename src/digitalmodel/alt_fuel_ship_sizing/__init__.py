# ABOUTME: Alternative-fuel (LH2) ship-design sizing toolkit: fuel-chain sizing,
# ABOUTME: wind-assist integration, trade study and ITC 69 tonnage estimate.
"""Alternative-fuel (LH2) ship-design sizing toolkit.

Concept-design screening: public physics and published data only (see
:mod:`digitalmodel.alt_fuel_ship_sizing.constants` for sources). Routed via
the engine basename ``alt_fuel_ship_sizing``.
"""

from digitalmodel.alt_fuel_ship_sizing.lh2_fuel_chain import (
    BoilOffResult,
    FuelChainResult,
    FuelProperties,
    TankGeometry,
    TankParameters,
    boil_off,
    cylindrical_tank_geometry,
    electrical_power_kw,
    endurance_hours,
    fuel_mass_flow_kg_per_h,
    size_fuel_chain,
)
from digitalmodel.alt_fuel_ship_sizing.tonnage import (
    TonnageResult,
    gross_tonnage,
    tonnage,
)
from digitalmodel.alt_fuel_ship_sizing.trade_study import (
    TradeStudyInputs,
    WindAssistInputs,
    run_trade_study,
    shaft_power_at_speed,
)
from digitalmodel.alt_fuel_ship_sizing.wind_assist import (
    ThrustMatrix,
    WindAssistResult,
    WindRoseBin,
    interpolate_thrust,
    wind_assist_saving,
)

__all__ = [
    "BoilOffResult",
    "FuelChainResult",
    "FuelProperties",
    "TankGeometry",
    "TankParameters",
    "ThrustMatrix",
    "TonnageResult",
    "TradeStudyInputs",
    "WindAssistInputs",
    "WindAssistResult",
    "WindRoseBin",
    "boil_off",
    "cylindrical_tank_geometry",
    "electrical_power_kw",
    "endurance_hours",
    "fuel_mass_flow_kg_per_h",
    "gross_tonnage",
    "interpolate_thrust",
    "run_trade_study",
    "shaft_power_at_speed",
    "size_fuel_chain",
    "tonnage",
    "wind_assist_saving",
]
