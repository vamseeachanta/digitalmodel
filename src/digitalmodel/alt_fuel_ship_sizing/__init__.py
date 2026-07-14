# ABOUTME: Alternative-fuel (LH2/NH3/methanol) ship-design sizing toolkit:
# ABOUTME: fuel chain, voyage-profile BOG, wind assist, trade study, tonnage.
"""Alternative-fuel ship-design sizing toolkit (LH2, NH3, methanol).

Concept-design screening: public physics and published data only (see
:mod:`digitalmodel.alt_fuel_ship_sizing.constants` for sources). Routed via
the engine basename ``alt_fuel_ship_sizing``.
"""

from digitalmodel.alt_fuel_ship_sizing.lh2_fuel_chain import (
    FUEL_PRESETS,
    BoilOffResult,
    FuelChainResult,
    FuelProperties,
    PortLeg,
    SeaLeg,
    TankGeometry,
    TankParameters,
    VoyageFuelChainResult,
    VoyageLeg,
    VoyageLegResult,
    boil_off,
    cylindrical_tank_geometry,
    electrical_power_kw,
    endurance_hours,
    fuel_mass_flow_kg_per_h,
    fuel_preset,
    lh2_fuel_properties,
    methanol_fuel_properties,
    nh3_refrigerated_fuel_properties,
    size_fuel_chain,
    size_fuel_chain_voyage,
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
    "FUEL_PRESETS",
    "BoilOffResult",
    "FuelChainResult",
    "FuelProperties",
    "PortLeg",
    "SeaLeg",
    "TankGeometry",
    "TankParameters",
    "ThrustMatrix",
    "TonnageResult",
    "TradeStudyInputs",
    "VoyageFuelChainResult",
    "VoyageLeg",
    "VoyageLegResult",
    "WindAssistInputs",
    "WindAssistResult",
    "WindRoseBin",
    "boil_off",
    "cylindrical_tank_geometry",
    "electrical_power_kw",
    "endurance_hours",
    "fuel_mass_flow_kg_per_h",
    "fuel_preset",
    "gross_tonnage",
    "interpolate_thrust",
    "lh2_fuel_properties",
    "methanol_fuel_properties",
    "nh3_refrigerated_fuel_properties",
    "run_trade_study",
    "shaft_power_at_speed",
    "size_fuel_chain",
    "size_fuel_chain_voyage",
    "tonnage",
    "wind_assist_saving",
]
