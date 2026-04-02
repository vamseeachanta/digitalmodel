"""Anode sizing calculator per DNV-RP-B401.

Provides sacrificial anode sizing for offshore structures, pipelines, and
marine vessels. Supports stand-off, bracelet, and flush-mount anode types
with McCoy and Dwight resistance formulas.

References
----------
- DNV-RP-B401 (2017) "Cathodic Protection Design" §5-7, §10
- DNV-RP-F103 (2016) "Cathodic Protection of Submarine Pipelines"
- McCoy (1974) "Corrosion Control in Offshore Structures"
- Dwight (1936) "Calculation of Resistances to Ground"
"""

from __future__ import annotations

import math
from enum import Enum

from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Constants (DNV-RP-B401)
# ---------------------------------------------------------------------------

# Protection potentials vs Ag/AgCl (DNV-RP-B401 §5.4.1)
PROTECTION_POTENTIAL_AGAGCL: float = -0.800  # V vs Ag/AgCl
ANODE_CLOSED_CIRCUIT_POTENTIAL: float = -1.050  # V vs Ag/AgCl (Al-Zn-In)

# Design driving voltage
DESIGN_DRIVING_VOLTAGE: float = abs(
    PROTECTION_POTENTIAL_AGAGCL - ANODE_CLOSED_CIRCUIT_POTENTIAL
)  # 0.25 V

# Al-Zn-In anode properties (DNV-RP-B401 Table 10-6)
DEFAULT_ANODE_CAPACITY: float = 2000.0  # A-h/kg
DEFAULT_UTILIZATION_STANDOFF: float = 0.90
DEFAULT_UTILIZATION_FLUSH: float = 0.85
DEFAULT_UTILIZATION_BRACELET: float = 0.80


class AnodeType(str, Enum):
    """Anode installation type."""

    STAND_OFF = "stand_off"
    BRACELET = "bracelet"
    FLUSH_MOUNT = "flush_mount"


class AnodeSizingInput(BaseModel):
    """Input parameters for CP system anode sizing."""

    surface_area_m2: float = Field(
        ..., gt=0, description="Total surface area to protect [m²]"
    )
    coating_breakdown_factor: float = Field(
        ..., ge=0.0, le=1.0, description="Coating breakdown factor (0-1)"
    )
    current_density_mA_m2: float = Field(
        ..., gt=0, description="Design current density [mA/m²]"
    )
    design_life_years: float = Field(
        ..., gt=0, description="CP system design life [years]"
    )
    anode_type: AnodeType = Field(
        default=AnodeType.STAND_OFF, description="Anode installation type"
    )
    anode_length_m: float = Field(
        ..., gt=0, description="Anode length [m]"
    )
    anode_radius_m: float = Field(
        ..., gt=0, description="Anode equivalent radius [m]"
    )
    anode_net_mass_kg: float = Field(
        ..., gt=0, description="Net mass of a single anode [kg]"
    )
    resistivity_ohm_m: float = Field(
        default=0.30, gt=0, description="Electrolyte resistivity [ohm-m]"
    )
    anode_capacity_Ah_kg: float = Field(
        default=DEFAULT_ANODE_CAPACITY,
        gt=0,
        description="Electrochemical capacity [A-h/kg]",
    )
    utilization_factor: float = Field(
        default=DEFAULT_UTILIZATION_STANDOFF,
        gt=0,
        le=1.0,
        description="Anode utilization factor",
    )
    driving_voltage_V: float = Field(
        default=DESIGN_DRIVING_VOLTAGE,
        gt=0,
        description="Design driving voltage [V]",
    )


class AnodeSizingResult(BaseModel):
    """Output of CP system anode sizing design."""

    current_demand_A: float = Field(
        ..., description="Total current demand [A]"
    )
    total_anode_mass_kg: float = Field(
        ..., description="Total required anode mass [kg]"
    )
    number_of_anodes: int = Field(
        ..., description="Number of anodes required"
    )
    anode_resistance_ohm: float = Field(
        ..., description="Individual anode resistance [ohm]"
    )
    anode_current_output_A: float = Field(
        ..., description="Current output per anode [A]"
    )
    driving_voltage_V: float = Field(
        ..., description="Design driving voltage [V]"
    )
    anode_type: str = Field(
        ..., description="Anode type used"
    )
    mass_check_ok: bool = Field(
        ..., description="Whether anode count provides sufficient total mass"
    )


def calculate_current_demand(
    surface_area_m2: float,
    coating_breakdown_factor: float,
    current_density_mA_m2: float,
) -> float:
    """Calculate current demand for a coated structure (DNV-RP-B401 §7.4.1).

    I_c = A * f_c * i_c / 1000

    Parameters
    ----------
    surface_area_m2 : float
        Total surface area to protect [m²].
    coating_breakdown_factor : float
        Coating breakdown factor (0 = perfect coating, 1 = bare steel).
    current_density_mA_m2 : float
        Design current density [mA/m²].

    Returns
    -------
    float
        Current demand [A].
    """
    return surface_area_m2 * coating_breakdown_factor * current_density_mA_m2 / 1000.0


def calculate_anode_mass(
    current_demand_A: float,
    design_life_years: float,
    utilization_factor: float = DEFAULT_UTILIZATION_STANDOFF,
    anode_capacity_Ah_kg: float = DEFAULT_ANODE_CAPACITY,
) -> float:
    """Calculate total anode mass requirement (DNV-RP-B401 §7.7.1, Eq 2).

    M_a = (I_cm * t_f * 8760) / (u * epsilon)

    Parameters
    ----------
    current_demand_A : float
        Mean current demand [A].
    design_life_years : float
        Design life [years].
    utilization_factor : float
        Anode utilization factor (0-1).
    anode_capacity_Ah_kg : float
        Electrochemical capacity [A-h/kg].

    Returns
    -------
    float
        Required total anode mass [kg].
    """
    return (current_demand_A * design_life_years * 8760.0) / (
        anode_capacity_Ah_kg * utilization_factor
    )


def calculate_anode_resistance(
    anode_type: AnodeType,
    length_m: float,
    radius_m: float,
    resistivity_ohm_m: float,
) -> float:
    """Calculate anode-to-electrolyte resistance per McCoy/Dwight formulas.

    Stand-off (McCoy/DNV-RP-B401 Table 10-7):
        R_a = (rho / (2*pi*L)) * (ln(4*L/r) - 1)

    Bracelet (Dwight, short cylinder approximation):
        R_a = (rho / (2*pi*L)) * (ln(4*L/r) - 1 + r/(2*L))

    Flush-mount (half-space radiation):
        R_a = (rho / (pi*L)) * (ln(2*L/r) - 0.5)

    Parameters
    ----------
    anode_type : AnodeType
        Anode installation type.
    length_m : float
        Anode length [m].
    radius_m : float
        Anode equivalent radius [m].
    resistivity_ohm_m : float
        Electrolyte resistivity [ohm-m].

    Returns
    -------
    float
        Anode-to-electrolyte resistance [ohm].
    """
    rho = resistivity_ohm_m
    L = length_m
    r = radius_m

    if anode_type == AnodeType.STAND_OFF:
        # McCoy formula (DNV-RP-B401 Table 10-7)
        R_a = (rho / (2.0 * math.pi * L)) * (
            math.log(4.0 * L / r) - 1.0
        )
    elif anode_type == AnodeType.BRACELET:
        # Dwight formula with short-cylinder correction
        R_a = (rho / (2.0 * math.pi * L)) * (
            math.log(4.0 * L / r) - 1.0 + r / (2.0 * L)
        )
    elif anode_type == AnodeType.FLUSH_MOUNT:
        # Half-space (flush on hull) — McCoy simplified
        R_a = (rho / (math.pi * L)) * (
            math.log(2.0 * L / r) - 0.5
        )
    else:
        # Default to stand-off
        R_a = (rho / (2.0 * math.pi * L)) * (
            math.log(4.0 * L / r) - 1.0
        )

    return R_a


def design_cp_system(input_params: AnodeSizingInput) -> AnodeSizingResult:
    """Design a complete sacrificial anode CP system.

    End-to-end calculation: current demand → anode mass → anode count →
    resistance check → current output verification.

    Parameters
    ----------
    input_params : AnodeSizingInput
        Full design input parameters.

    Returns
    -------
    AnodeSizingResult
        Complete CP system design result.
    """
    # Step 1: Current demand
    i_demand = calculate_current_demand(
        surface_area_m2=input_params.surface_area_m2,
        coating_breakdown_factor=input_params.coating_breakdown_factor,
        current_density_mA_m2=input_params.current_density_mA_m2,
    )

    # Step 2: Total anode mass
    total_mass = calculate_anode_mass(
        current_demand_A=i_demand,
        design_life_years=input_params.design_life_years,
        utilization_factor=input_params.utilization_factor,
        anode_capacity_Ah_kg=input_params.anode_capacity_Ah_kg,
    )

    # Step 3: Number of anodes (mass-based)
    n_mass = max(1, math.ceil(total_mass / input_params.anode_net_mass_kg))

    # Step 4: Anode resistance
    R_a = calculate_anode_resistance(
        anode_type=input_params.anode_type,
        length_m=input_params.anode_length_m,
        radius_m=input_params.anode_radius_m,
        resistivity_ohm_m=input_params.resistivity_ohm_m,
    )

    # Step 5: Current output per anode
    i_anode = input_params.driving_voltage_V / R_a

    # Step 6: Check if current output is sufficient
    # Need: n_anodes * i_anode >= i_demand (final condition check)
    n_current = max(1, math.ceil(i_demand / i_anode))

    # Take the larger of mass-based and current-based anode count
    n_anodes = max(n_mass, n_current)

    # Mass check: does the anode count provide enough total mass?
    provided_mass = n_anodes * input_params.anode_net_mass_kg
    mass_check_ok = provided_mass >= total_mass

    return AnodeSizingResult(
        current_demand_A=round(i_demand, 4),
        total_anode_mass_kg=round(total_mass, 2),
        number_of_anodes=n_anodes,
        anode_resistance_ohm=round(R_a, 6),
        anode_current_output_A=round(i_anode, 4),
        driving_voltage_V=input_params.driving_voltage_V,
        anode_type=input_params.anode_type.value,
        mass_check_ok=mass_check_ok,
    )
