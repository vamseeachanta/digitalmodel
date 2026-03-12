"""Cathodic protection calculations — API RP 1632, ISO 15589-2, DNV-RP-B401."""

from digitalmodel.cathodic_protection.api_rp_1632 import (
    anode_driving_voltage,
    anode_life_years,
    anode_resistance_vertical_rod,
    check_protection_potential,
    current_demand,
    current_per_anode,
    number_of_anodes,
)
from digitalmodel.cathodic_protection.iso_15589_2 import (
    anode_mass_requirement,
    anode_output_current,
    anode_resistance,
    coating_breakdown_factor,
    initial_current_density,
    pipeline_current_demand,
)
from digitalmodel.cathodic_protection.iso_15589_2 import (
    check_protection_potential as iso_check_protection_potential,
)
from digitalmodel.cathodic_protection.dnv_rp_b401 import (
    anode_current_output as dnv_anode_current_output,
    anode_mass_requirement as dnv_anode_mass_requirement,
    anode_resistance_slender_standoff,
    coating_breakdown_factor as dnv_coating_breakdown_factor,
    current_demand as dnv_current_demand,
    equivalent_radius_from_mass,
    flush_anode_resistance,
    number_of_anodes as dnv_number_of_anodes,
    protected_length,
)

__all__ = [
    "anode_driving_voltage",
    "anode_resistance_vertical_rod",
    "current_demand",
    "current_per_anode",
    "number_of_anodes",
    "anode_life_years",
    "check_protection_potential",
    "initial_current_density",
    "coating_breakdown_factor",
    "pipeline_current_demand",
    "anode_resistance",
    "anode_output_current",
    "anode_mass_requirement",
    "iso_check_protection_potential",
    "dnv_current_demand",
    "dnv_anode_mass_requirement",
    "dnv_coating_breakdown_factor",
    "anode_resistance_slender_standoff",
    "dnv_anode_current_output",
    "equivalent_radius_from_mass",
    "flush_anode_resistance",
    "dnv_number_of_anodes",
    "protected_length",
]
