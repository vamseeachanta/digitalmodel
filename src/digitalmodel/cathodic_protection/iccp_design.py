"""Impressed current cathodic protection (ICCP) system design.

Covers rectifier sizing (voltage and current), anode bed design (deep well
and shallow horizontal), cable sizing, and monitoring point placement for
ICCP systems protecting pipelines, tanks, and marine structures.

References
----------
- NACE SP0169 (2013) "Control of External Corrosion on Underground or
  Submerged Metallic Piping Systems"
- NACE TM0497 "Measurement Techniques Related to Criteria for Cathodic
  Protection on Underground or Submerged Metallic Piping Systems"
- API RP 1632 (1996) §7 — Impressed Current Systems
"""

from __future__ import annotations

import math
from enum import Enum
from typing import Optional

from pydantic import BaseModel, Field


class AnodeBedType(str, Enum):
    """Anode bed configuration."""

    SHALLOW_HORIZONTAL = "shallow_horizontal"
    SHALLOW_VERTICAL = "shallow_vertical"
    DEEP_WELL = "deep_well"
    DISTRIBUTED = "distributed"


class AnodeMaterial(str, Enum):
    """Impressed current anode materials."""

    HIGH_SILICON_CAST_IRON = "high_silicon_cast_iron"
    MIXED_METAL_OXIDE = "mixed_metal_oxide"
    GRAPHITE = "graphite"
    PLATINIZED_TITANIUM = "platinized_titanium"
    MAGNETITE = "magnetite"


# Maximum recommended current density per anode material [A/m²]
ANODE_MAX_CURRENT_DENSITY: dict[AnodeMaterial, float] = {
    AnodeMaterial.HIGH_SILICON_CAST_IRON: 10.0,
    AnodeMaterial.MIXED_METAL_OXIDE: 100.0,
    AnodeMaterial.GRAPHITE: 5.0,
    AnodeMaterial.PLATINIZED_TITANIUM: 500.0,
    AnodeMaterial.MAGNETITE: 50.0,
}

# Anode consumption rate [kg/A-year]
ANODE_CONSUMPTION_RATE: dict[AnodeMaterial, float] = {
    AnodeMaterial.HIGH_SILICON_CAST_IRON: 0.25,
    AnodeMaterial.MIXED_METAL_OXIDE: 0.001,
    AnodeMaterial.GRAPHITE: 0.45,
    AnodeMaterial.PLATINIZED_TITANIUM: 0.000006,
    AnodeMaterial.MAGNETITE: 0.05,
}

# Copper cable resistivity [ohm-mm²/m] at 20°C
COPPER_RESISTIVITY = 0.0175  # ohm-mm²/m


class RectifierSizingInput(BaseModel):
    """Input parameters for rectifier sizing."""

    total_current_A: float = Field(..., gt=0, description="Total current demand [A]")
    ground_bed_resistance_ohm: float = Field(
        ..., gt=0, description="Anode bed resistance [ohm]"
    )
    structure_coating_resistance_ohm: float = Field(
        default=1.0, ge=0, description="Structure/coating resistance [ohm]"
    )
    cable_resistance_ohm: float = Field(
        default=0.0, ge=0, description="Total cable resistance [ohm]"
    )
    back_emf_V: float = Field(
        default=2.0,
        ge=0,
        description="Back EMF from anode/structure potential difference [V]",
    )
    safety_factor: float = Field(
        default=1.25, gt=1.0, description="Safety factor on voltage (typically 1.2-1.5)"
    )


class RectifierSizingResult(BaseModel):
    """Output of rectifier sizing calculation."""

    dc_voltage_V: float = Field(..., description="Required DC output voltage [V]")
    dc_current_A: float = Field(..., description="Required DC output current [A]")
    power_W: float = Field(..., description="Required rectifier power [W]")
    recommended_rating_V: float = Field(
        ..., description="Recommended standard rectifier voltage rating [V]"
    )
    recommended_rating_A: float = Field(
        ..., description="Recommended standard rectifier current rating [A]"
    )


class AnodeBedResult(BaseModel):
    """Output of anode bed design."""

    bed_type: str = Field(..., description="Anode bed type")
    anode_material: str = Field(..., description="Anode material")
    number_of_anodes: int = Field(..., description="Number of anodes")
    anode_length_m: float = Field(..., description="Individual anode length [m]")
    anode_diameter_m: float = Field(..., description="Individual anode diameter [m]")
    anode_spacing_m: float = Field(..., description="Anode center-to-center spacing [m]")
    bed_resistance_ohm: float = Field(
        ..., description="Total anode bed resistance [ohm]"
    )
    estimated_life_years: float = Field(
        ..., description="Estimated anode bed life [years]"
    )


def rectifier_sizing(
    input_params: RectifierSizingInput,
) -> RectifierSizingResult:
    """Size a transformer-rectifier for an ICCP system.

    Calculates required DC voltage as:
        V_dc = I * (R_gb + R_struct + R_cable) + V_back_emf

    Then applies safety factor and rounds up to standard ratings.

    Parameters
    ----------
    input_params : RectifierSizingInput
        System parameters for sizing.

    Returns
    -------
    RectifierSizingResult
        Required voltage, current, and power with standard ratings.
    """
    total_resistance = (
        input_params.ground_bed_resistance_ohm
        + input_params.structure_coating_resistance_ohm
        + input_params.cable_resistance_ohm
    )

    v_dc = (
        input_params.total_current_A * total_resistance
        + input_params.back_emf_V
    ) * input_params.safety_factor

    power = v_dc * input_params.total_current_A

    # Standard rectifier ratings (common sizes)
    standard_voltages = [12, 24, 36, 48, 72, 96, 120]
    standard_currents = [5, 10, 16, 25, 40, 50, 75, 100, 150, 200]

    rec_voltage = next(
        (v for v in standard_voltages if v >= v_dc),
        standard_voltages[-1],
    )
    rec_current = next(
        (c for c in standard_currents if c >= input_params.total_current_A),
        standard_currents[-1],
    )

    return RectifierSizingResult(
        dc_voltage_V=round(v_dc, 2),
        dc_current_A=input_params.total_current_A,
        power_W=round(power, 2),
        recommended_rating_V=float(rec_voltage),
        recommended_rating_A=float(rec_current),
    )


def anode_bed_design(
    total_current_A: float,
    soil_resistivity_ohm_m: float,
    design_life_years: float = 25.0,
    bed_type: AnodeBedType = AnodeBedType.DEEP_WELL,
    anode_material: AnodeMaterial = AnodeMaterial.HIGH_SILICON_CAST_IRON,
    anode_length_m: float = 1.5,
    anode_diameter_m: float = 0.075,
) -> AnodeBedResult:
    """Design an impressed current anode bed.

    Determines number of anodes based on current capacity and consumption
    rate, and calculates bed resistance using the Dwight equation with
    parallel resistance adjustment.

    Parameters
    ----------
    total_current_A : float
        Total protection current [A].
    soil_resistivity_ohm_m : float
        Soil resistivity [ohm-m].
    design_life_years : float
        Anode bed design life [years].
    bed_type : AnodeBedType
        Anode bed configuration.
    anode_material : AnodeMaterial
        Anode material type.
    anode_length_m : float
        Length of each anode [m].
    anode_diameter_m : float
        Diameter of each anode [m].

    Returns
    -------
    AnodeBedResult
        Anode bed design parameters.
    """
    # Max current per anode surface area
    anode_surface_area = math.pi * anode_diameter_m * anode_length_m
    max_current_density = ANODE_MAX_CURRENT_DENSITY[anode_material]
    max_current_per_anode = anode_surface_area * max_current_density

    # Number from current capacity
    n_current = math.ceil(total_current_A / max_current_per_anode)

    # Number from consumption/life
    consumption_rate = ANODE_CONSUMPTION_RATE[anode_material]
    anode_mass_kg = (
        math.pi * (anode_diameter_m / 2) ** 2 * anode_length_m * 7200.0
    )  # approximate density for cast iron
    mass_consumed_per_year = total_current_A * consumption_rate
    if mass_consumed_per_year > 0:
        life_from_mass = anode_mass_kg * n_current / mass_consumed_per_year
        if life_from_mass < design_life_years:
            n_life = math.ceil(
                mass_consumed_per_year * design_life_years / anode_mass_kg
            )
        else:
            n_life = n_current
    else:
        n_life = n_current

    n_anodes = max(n_current, n_life, 1)

    # Anode spacing
    if bed_type == AnodeBedType.DEEP_WELL:
        spacing = max(3.0, anode_length_m * 5.0)
    else:
        spacing = max(3.0, anode_length_m * 3.0)

    # Single anode resistance (Dwight equation)
    L = anode_length_m
    d = anode_diameter_m
    r_single = (soil_resistivity_ohm_m / (2.0 * math.pi * L)) * (
        math.log(8.0 * L / d) - 1.0
    )

    # Parallel resistance with mutual interference factor
    # Sunde's formula: R_total = R_single/N * (1 + interference)
    if n_anodes > 1:
        # Simplified interference factor
        interference = 0.05 * math.log(n_anodes)
        r_bed = r_single / n_anodes * (1.0 + interference)
    else:
        r_bed = r_single

    # Deep well correction: lower resistivity at depth
    if bed_type == AnodeBedType.DEEP_WELL:
        r_bed *= 0.6  # deep anode beds have ~40% lower resistance

    # Estimated life
    if mass_consumed_per_year > 0:
        estimated_life = anode_mass_kg * n_anodes / mass_consumed_per_year
    else:
        estimated_life = 100.0

    return AnodeBedResult(
        bed_type=bed_type.value,
        anode_material=anode_material.value,
        number_of_anodes=n_anodes,
        anode_length_m=anode_length_m,
        anode_diameter_m=anode_diameter_m,
        anode_spacing_m=spacing,
        bed_resistance_ohm=round(r_bed, 4),
        estimated_life_years=round(estimated_life, 1),
    )


def cable_sizing(
    current_A: float,
    cable_length_m: float,
    max_voltage_drop_V: float = 2.0,
    temperature_c: float = 20.0,
) -> dict[str, float]:
    """Calculate minimum cable cross-section for ICCP system.

    Uses copper resistivity with temperature correction to determine
    minimum cross-sectional area for acceptable voltage drop.

    Parameters
    ----------
    current_A : float
        Maximum cable current [A].
    cable_length_m : float
        One-way cable length [m].
    max_voltage_drop_V : float
        Maximum acceptable voltage drop [V] (default 2.0 V).
    temperature_c : float
        Cable operating temperature [°C].

    Returns
    -------
    dict[str, float]
        Cable sizing results including area, resistance, and voltage drop.
    """
    # Temperature correction for copper
    rho = COPPER_RESISTIVITY * (1.0 + 0.00393 * (temperature_c - 20.0))

    # Minimum cross-section: A = rho * I * 2L / V_drop (2L for round trip)
    min_area_mm2 = rho * current_A * 2.0 * cable_length_m / max_voltage_drop_V

    # Standard cable sizes [mm²]
    standard_sizes = [2.5, 4.0, 6.0, 10.0, 16.0, 25.0, 35.0, 50.0, 70.0, 95.0, 120.0]
    selected_size = next(
        (s for s in standard_sizes if s >= min_area_mm2),
        standard_sizes[-1],
    )

    actual_resistance = rho * 2.0 * cable_length_m / selected_size
    actual_voltage_drop = current_A * actual_resistance

    return {
        "min_area_mm2": round(min_area_mm2, 2),
        "selected_area_mm2": selected_size,
        "cable_resistance_ohm": round(actual_resistance, 4),
        "voltage_drop_V": round(actual_voltage_drop, 3),
        "cable_length_m": cable_length_m,
    }
