"""Impressed current CP design for data center generator fuel piping systems.

Extends API RP 1632 galvanic anode calculations to impressed current systems,
sizing ground beds and rectifiers for buried fuel supply and return piping.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum

from digitalmodel.cathodic_protection.api_rp_1632 import (
    CURRENT_DENSITY_BARE,
    PROTECTION_POTENTIAL_CSE,
    anode_resistance_vertical_rod,
)


class CoatingType(Enum):
    """Pipe coating classification."""

    BARE = "bare"
    FBE = "fbe"
    POLYETHYLENE = "polyethylene"
    COAL_TAR = "coal_tar"


COATING_BREAKDOWN_FACTOR: dict[CoatingType, float] = {
    CoatingType.BARE: 1.0,
    CoatingType.FBE: 0.02,
    CoatingType.POLYETHYLENE: 0.01,
    CoatingType.COAL_TAR: 0.05,
}


@dataclass
class FuelPipeSegment:
    """A single buried fuel pipe segment."""

    segment_id: str
    length_m: float
    outer_diameter_m: float
    coating_type: CoatingType
    soil_resistivity_ohm_m: float
    burial_depth_m: float = 0.9

    def __post_init__(self) -> None:
        if self.length_m <= 0:
            raise ValueError("length_m must be positive")
        if self.outer_diameter_m <= 0:
            raise ValueError("outer_diameter_m must be positive")
        if self.soil_resistivity_ohm_m <= 0:
            raise ValueError("soil_resistivity_ohm_m must be positive")


@dataclass
class ImpressedCurrentGroundBed:
    """Impressed current ground bed design output."""

    anode_type: str
    anode_length_m: float
    anode_diameter_m: float
    anode_count: int
    anode_spacing_m: float
    ground_bed_resistance_ohm: float


@dataclass
class RectifierOutput:
    """Rectifier sizing output."""

    dc_voltage_v: float
    dc_current_a: float
    power_w: float


def pipe_surface_area(segment: FuelPipeSegment) -> float:
    """External surface area of a pipe segment [m^2]."""
    return math.pi * segment.outer_diameter_m * segment.length_m


def effective_bare_area(segment: FuelPipeSegment, years: int = 20) -> float:
    """Effective bare area accounting for coating breakdown over time [m^2].

    Breakdown factor increases linearly: f_final = f_initial + 0.01 * years,
    capped at 1.0.
    """
    f_initial = COATING_BREAKDOWN_FACTOR[segment.coating_type]
    f_final = min(f_initial + 0.01 * years, 1.0)
    return pipe_surface_area(segment) * f_final


def current_demand_segment(
    segment: FuelPipeSegment, years: int = 20
) -> float:
    """Protection current required for a single segment [A].

    Uses bare-steel current density from API RP 1632 applied to effective
    bare area.
    """
    bare_area = effective_bare_area(segment, years)
    return bare_area * CURRENT_DENSITY_BARE / 1000.0


def total_current_demand(
    segments: list[FuelPipeSegment], years: int = 20
) -> float:
    """Total protection current for all pipe segments [A]."""
    return sum(current_demand_segment(s, years) for s in segments)


def design_ground_bed(
    total_current_a: float,
    soil_resistivity_ohm_m: float,
    anode_length_m: float = 1.5,
    anode_diameter_m: float = 0.075,
) -> ImpressedCurrentGroundBed:
    """Size an impressed current ground bed using high-silicon cast iron anodes.

    Uses Dwight equation (via api_rp_1632) for single-anode resistance and
    parallel combination for the ground bed. Anode count is determined by
    keeping individual anode current output below 2 A (manufacturer limit).
    """
    max_current_per_anode = 2.0
    n_anodes = max(1, math.ceil(total_current_a / max_current_per_anode))

    r_single = anode_resistance_vertical_rod(
        soil_resistivity_ohm_m, anode_length_m, anode_diameter_m
    )

    # Parallel resistance with utilisation factor (anodes not fully remote)
    utilisation = 0.8 if n_anodes > 1 else 1.0
    r_ground_bed = r_single / (n_anodes * utilisation)

    anode_spacing = max(3.0, anode_length_m * 3.0)

    return ImpressedCurrentGroundBed(
        anode_type="high_silicon_cast_iron",
        anode_length_m=anode_length_m,
        anode_diameter_m=anode_diameter_m,
        anode_count=n_anodes,
        anode_spacing_m=anode_spacing,
        ground_bed_resistance_ohm=r_ground_bed,
    )


def design_rectifier(
    total_current_a: float,
    ground_bed: ImpressedCurrentGroundBed,
    structure_resistance_ohm: float = 0.5,
    cable_resistance_ohm: float = 0.2,
) -> RectifierOutput:
    """Size the rectifier for the impressed current system.

    Voltage = I * (R_groundbed + R_structure + R_cable), with 20% safety margin.
    """
    total_resistance = (
        ground_bed.ground_bed_resistance_ohm
        + structure_resistance_ohm
        + cable_resistance_ohm
    )
    voltage = total_current_a * total_resistance * 1.2  # 20% margin
    power = voltage * total_current_a

    return RectifierOutput(
        dc_voltage_v=voltage,
        dc_current_a=total_current_a,
        power_w=power,
    )


def check_protection(
    rectifier: RectifierOutput,
    ground_bed: ImpressedCurrentGroundBed,
    structure_resistance_ohm: float,
) -> dict:
    """Evaluate whether the impressed current system meets -0.85 V CSE criterion.

    Estimates pipe-to-soil potential from the IR drop across the structure
    resistance and compares against the API RP 1632 protection criterion.
    """
    ir_drop = rectifier.dc_current_a * structure_resistance_ohm
    # More negative potential = more protected; natural potential ~-0.55 V
    natural_potential = -0.55
    estimated_potential = natural_potential - ir_drop

    is_protected = estimated_potential <= PROTECTION_POTENTIAL_CSE

    return {
        "pass": is_protected,
        "potential_v_cse": estimated_potential,
        "criterion_v_cse": PROTECTION_POTENTIAL_CSE,
        "ir_drop_v": ir_drop,
    }
