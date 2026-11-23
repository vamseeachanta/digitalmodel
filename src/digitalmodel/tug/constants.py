# ABOUTME: Tug toolkit shared constants — propulsion BP factors, class safety factors, fuel factors
# ABOUTME: Single source of truth so calc modules never hardcode coefficients (workspace YAML rule)
"""Shared constants for the tug toolkit.

References:
    - ITS / tug industry typical bollard-pull-to-power ratios by propulsion type
    - Classification-society towing-gear breaking-strength factors
      (LR ~2.5x, ABS/DNV ~2.0x of design/static load); see PWSRCAC class review
    - IMO MARPOL Annex VI fuel CO2 carbon factors; representative MGO/methanol/H2 figures
"""

from enum import Enum


class Propulsion(Enum):
    """Tug propulsion type."""

    CONVENTIONAL = "conventional"  # single/twin screw, open propeller
    CONVENTIONAL_NOZZLE = "conventional_nozzle"  # ducted/Kort-nozzle propeller
    ASD = "asd"  # azimuth stern drive (Z-drive), ducted
    TRACTOR_VSP = "tractor_vsp"  # Voith-Schneider cycloidal tractor
    ROTOR = "rotor"  # rotor/RAVE twin-rotor concept


# Bollard pull per installed propulsion power, tonnes-force per kW.
# Ducted/azimuth thrusters convert power to static thrust more efficiently
# than open propellers; ranges are typical industry values, not class minima.
BP_FACTOR_T_PER_KW = {
    Propulsion.CONVENTIONAL: 0.011,
    Propulsion.CONVENTIONAL_NOZZLE: 0.0135,
    Propulsion.ASD: 0.0145,
    Propulsion.TRACTOR_VSP: 0.012,
    Propulsion.ROTOR: 0.013,
}

# Astern bollard pull as a fraction of ahead BP. Azimuthing/cycloidal tugs are
# near-symmetric; conventional screw tugs lose more going astern.
ASTERN_FRACTION = {
    Propulsion.CONVENTIONAL: 0.55,
    Propulsion.CONVENTIONAL_NOZZLE: 0.60,
    Propulsion.ASD: 0.95,
    Propulsion.TRACTOR_VSP: 0.95,
    Propulsion.ROTOR: 0.90,
}

# Class minimum towline breaking-strength factor (MBL / design line load).
CLASS_TOWLINE_SAFETY_FACTOR = {
    "LR": 2.5,  # Lloyd's Register
    "ABS": 2.0,  # American Bureau of Shipping
    "DNV": 2.0,  # DNV
}

# DNV stability check assumes the transverse pull is at least this fraction of
# the maximum continuous bollard pull for conventional-propeller tugs.
DNV_TRANSVERSE_PULL_FRACTION = 0.60

# Transverse (heeling) pull as a fraction of ahead BP, by propulsion type.
# Conventional-screw tugs vector only part of their thrust sideways (~0.6, the
# DNV value); azimuth/cycloidal tugs can direct nearly their full thrust
# transversely, so they heel themselves with close to the full bollard pull.
TRANSVERSE_PULL_FRACTION = {
    Propulsion.CONVENTIONAL: 0.60,
    Propulsion.CONVENTIONAL_NOZZLE: 0.60,
    Propulsion.ASD: 1.00,
    Propulsion.TRACTOR_VSP: 1.00,
    Propulsion.ROTOR: 0.95,
}

# Fuel CO2 carbon factor (t CO2 / t fuel) and lower heating value (MJ/kg).
# methanol/hydrogen carbon factors are tank-to-wake (combustion) only.
FUEL_CO2_FACTOR = {
    "mgo": 3.206,  # marine gas oil (IMO MARPOL Annex VI)
    "mdo": 3.206,
    "methanol": 1.375,
    "hydrogen": 0.0,
}
FUEL_LHV_MJ_PER_KG = {
    "mgo": 42.7,
    "mdo": 42.7,
    "methanol": 19.9,
    "hydrogen": 120.0,
}

GRAVITY_M_S2 = 9.80665
SEAWATER_DENSITY_T_M3 = 1.025
