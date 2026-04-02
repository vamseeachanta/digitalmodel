"""Pipeline cathodic protection design per NACE SP0169 and ISO 15589-1.

Covers current density selection for buried and submerged pipelines, anode
spacing calculations, interference effect assessment, and holiday detection
criteria.

References
----------
- NACE SP0169 (2013) "Control of External Corrosion on Underground or
  Submerged Metallic Piping Systems"
- ISO 15589-1 (2015) "Petroleum, Petrochemical and Natural Gas Industries —
  Cathodic Protection of Pipeline Systems — Part 1: On-land Pipelines"
- NACE SP0502 "Pipeline External Corrosion Direct Assessment Methodology"
"""

from __future__ import annotations

import math
from enum import Enum
from typing import Optional

from pydantic import BaseModel, Field


class PipelineEnvironment(str, Enum):
    """Pipeline installation environment."""

    BURIED_SOIL = "buried_soil"
    BURIED_SAND = "buried_sand"
    BURIED_CLAY = "buried_clay"
    BURIED_ROCK = "buried_rock"
    SUBMERGED_FRESHWATER = "submerged_freshwater"
    SUBMERGED_BRACKISH = "submerged_brackish"
    SUBMERGED_SEAWATER = "submerged_seawater"
    HDD_CROSSING = "hdd_crossing"


# Current density recommendations [mA/m²] per NACE SP0169 / ISO 15589-1
# Format: (bare_steel, coated_pipeline_initial)
CURRENT_DENSITY_TABLE: dict[PipelineEnvironment, tuple[float, float]] = {
    PipelineEnvironment.BURIED_SOIL: (10.0, 0.5),
    PipelineEnvironment.BURIED_SAND: (15.0, 0.8),
    PipelineEnvironment.BURIED_CLAY: (5.0, 0.3),
    PipelineEnvironment.BURIED_ROCK: (20.0, 1.0),
    PipelineEnvironment.SUBMERGED_FRESHWATER: (25.0, 1.5),
    PipelineEnvironment.SUBMERGED_BRACKISH: (40.0, 2.5),
    PipelineEnvironment.SUBMERGED_SEAWATER: (100.0, 5.0),
    PipelineEnvironment.HDD_CROSSING: (15.0, 1.0),
}

# Protection criteria per NACE SP0169 §6.2
PROTECTION_POTENTIAL_CSE: float = -0.850  # V vs Cu/CuSO4
PROTECTION_POTENTIAL_CSE_ANAEROBIC: float = -0.950  # V vs CSE for SRB environments
POLARIZATION_SHIFT_MIN: float = 0.100  # V minimum polarization shift


class PipelineCPInput(BaseModel):
    """Input parameters for pipeline CP design."""

    outer_diameter_m: float = Field(..., gt=0, description="Pipe outer diameter [m]")
    wall_thickness_m: float = Field(..., gt=0, description="Pipe wall thickness [m]")
    length_m: float = Field(..., gt=0, description="Pipeline length [m]")
    environment: PipelineEnvironment = Field(
        default=PipelineEnvironment.BURIED_SOIL,
        description="Installation environment",
    )
    coating_breakdown_factor: float = Field(
        default=0.03,
        ge=0.0,
        le=1.0,
        description="Coating breakdown factor (0-1)",
    )
    soil_resistivity_ohm_m: float = Field(
        default=50.0, gt=0, description="Soil/electrolyte resistivity [ohm-m]"
    )
    design_life_years: float = Field(default=25.0, gt=0, description="Design life [years]")


class PipelineCPResult(BaseModel):
    """Output of pipeline CP design calculation."""

    current_density_mA_m2: float = Field(
        ..., description="Design current density [mA/m²]"
    )
    total_surface_area_m2: float = Field(
        ..., description="Total pipeline external surface area [m²]"
    )
    effective_bare_area_m2: float = Field(
        ..., description="Effective bare area [m²]"
    )
    current_demand_A: float = Field(
        ..., description="Total current demand [A]"
    )
    recommended_anode_spacing_m: Optional[float] = Field(
        None, description="Recommended anode spacing [m]"
    )


class AnodeSpacingResult(BaseModel):
    """Result of anode spacing calculation."""

    anode_spacing_m: float = Field(..., description="Anode center-to-center spacing [m]")
    number_of_anodes: int = Field(..., description="Total number of anodes required")
    current_per_anode_A: float = Field(
        ..., description="Required current per anode [A]"
    )


def pipeline_current_demand(
    input_params: PipelineCPInput,
) -> PipelineCPResult:
    """Calculate pipeline CP current demand per ISO 15589-1 / NACE SP0169.

    Current demand = pi * D * L * f_c * i_c

    where f_c is the coating breakdown factor and i_c is the bare-steel
    current density for the given environment.

    Parameters
    ----------
    input_params : PipelineCPInput
        Pipeline design parameters.

    Returns
    -------
    PipelineCPResult
        Current demand and related parameters.
    """
    bare_density, _ = CURRENT_DENSITY_TABLE[input_params.environment]

    total_area = math.pi * input_params.outer_diameter_m * input_params.length_m
    effective_bare = total_area * input_params.coating_breakdown_factor
    current_demand = effective_bare * bare_density / 1000.0  # mA to A

    return PipelineCPResult(
        current_density_mA_m2=bare_density,
        total_surface_area_m2=total_area,
        effective_bare_area_m2=effective_bare,
        current_demand_A=current_demand,
    )


def anode_spacing(
    pipeline_length_m: float,
    current_demand_A: float,
    anode_current_output_A: float,
    min_spacing_m: float = 100.0,
    max_spacing_m: float = 2000.0,
) -> AnodeSpacingResult:
    """Calculate anode spacing for a galvanic anode pipeline CP system.

    Anode spacing is determined by the current demand per unit length
    and the current output of each anode.

    Parameters
    ----------
    pipeline_length_m : float
        Total pipeline length [m].
    current_demand_A : float
        Total current demand [A].
    anode_current_output_A : float
        Current output per anode [A].
    min_spacing_m : float
        Minimum allowable spacing [m] (default 100 m).
    max_spacing_m : float
        Maximum allowable spacing [m] (default 2000 m).

    Returns
    -------
    AnodeSpacingResult
        Spacing, count, and current per anode.
    """
    if current_demand_A <= 0:
        return AnodeSpacingResult(
            anode_spacing_m=max_spacing_m,
            number_of_anodes=1,
            current_per_anode_A=0.0,
        )

    n_anodes = max(1, math.ceil(current_demand_A / anode_current_output_A))
    spacing = pipeline_length_m / n_anodes

    # Clamp to min/max
    spacing = max(min_spacing_m, min(spacing, max_spacing_m))
    n_anodes = max(1, math.ceil(pipeline_length_m / spacing))
    current_per_anode = current_demand_A / n_anodes

    return AnodeSpacingResult(
        anode_spacing_m=spacing,
        number_of_anodes=n_anodes,
        current_per_anode_A=current_per_anode,
    )


def holiday_detection_voltage(
    wall_thickness_mm: float,
    coating_type: str = "FBE",
) -> float:
    """Calculate holiday detection test voltage per NACE SP0490 / ISO 21809.

    For thin-film coatings (<0.5 mm): V = 5 * sqrt(T_microns)
    For thick-film coatings (>=0.5 mm): V = 3.7 * sqrt(T_microns)

    Parameters
    ----------
    wall_thickness_mm : float
        Coating dry film thickness [mm].
    coating_type : str
        Coating type: "FBE" for thin-film, "PE" or "PP" for thick-film.

    Returns
    -------
    float
        Holiday detection test voltage [V].
    """
    t_microns = wall_thickness_mm * 1000.0

    if coating_type.upper() in ("FBE", "EPOXY"):
        # Low-voltage wet sponge for thin film, or spark test
        voltage = 5.0 * math.sqrt(t_microns)
    else:
        # High-voltage spark test for thick coatings
        voltage = 3.7 * math.sqrt(t_microns)

    # Cap at practical limits
    return min(voltage, 25000.0)


# ═══════════════════════════════════════════════════════════════════════
# Extended pipeline CP design — API RP 1169 / ISO 15589-1 additions
# ═══════════════════════════════════════════════════════════════════════

OVERPROTECTION_THRESHOLD_V: float = -1.200  # V vs CSE — hydrogen embrittlement risk


class CriteriaResult(BaseModel):
    """Result of protection potential criteria check."""

    measured_potential_V: float = Field(
        ..., description="Measured pipe-to-soil potential [V vs CSE]"
    )
    criterion_V: float = Field(
        ..., description="Protection criterion potential [V vs CSE]"
    )
    is_protected: bool = Field(
        ..., description="Whether measured potential meets criterion"
    )
    is_overprotected: bool = Field(
        default=False,
        description="Whether overprotection risk exists (< -1.2 V CSE)",
    )
    environment: str = Field(
        ..., description="Environment type (aerobic/anaerobic)"
    )


class PipelineDesignResult(BaseModel):
    """End-to-end pipeline CP design result."""

    current_demand_A: float = Field(
        ..., description="Total current demand [A]"
    )
    total_surface_area_m2: float = Field(
        ..., description="Pipeline external surface area [m²]"
    )
    effective_bare_area_m2: float = Field(
        ..., description="Effective bare area after coating breakdown [m²]"
    )
    number_of_anodes: int = Field(
        ..., description="Number of anodes required"
    )
    anode_spacing_m: float = Field(
        ..., description="Anode spacing along pipeline [m]"
    )
    soil_resistivity_correction_factor: float = Field(
        ..., description="Soil resistivity correction factor applied"
    )
    protection_criteria: CriteriaResult = Field(
        ..., description="Protection criteria assessment"
    )


def calculate_pipeline_current_demand(
    input_params: PipelineCPInput,
) -> float:
    """Calculate pipeline current demand [A] with soil correction.

    I = pi * D * L * f_c * i_c * k_soil / 1000

    Parameters
    ----------
    input_params : PipelineCPInput
        Pipeline parameters.

    Returns
    -------
    float
        Current demand [A].
    """
    bare_density, _ = CURRENT_DENSITY_TABLE[input_params.environment]
    total_area = math.pi * input_params.outer_diameter_m * input_params.length_m
    effective_bare = total_area * input_params.coating_breakdown_factor
    k_soil = soil_resistivity_correction(input_params.soil_resistivity_ohm_m)
    return effective_bare * bare_density * k_soil / 1000.0


def calculate_anode_spacing(
    pipeline_length_m: float,
    total_current_demand_A: float,
    anode_output_A: float,
    max_spacing_m: float = 5000.0,
) -> tuple[float, int]:
    """Calculate anode spacing and count for a pipeline.

    Parameters
    ----------
    pipeline_length_m : float
        Total pipeline length [m].
    total_current_demand_A : float
        Total current demand [A].
    anode_output_A : float
        Current output per anode [A].
    max_spacing_m : float
        Maximum allowable spacing [m] (default 5000 m).

    Returns
    -------
    tuple[float, int]
        (anode_spacing_m, number_of_anodes)
    """
    if total_current_demand_A <= 0 or anode_output_A <= 0:
        return (max_spacing_m, 1)

    n_anodes = max(1, math.ceil(total_current_demand_A / anode_output_A))
    spacing = pipeline_length_m / n_anodes

    # Cap spacing
    if spacing > max_spacing_m:
        spacing = max_spacing_m
        n_anodes = max(1, math.ceil(pipeline_length_m / spacing))

    return (spacing, n_anodes)


def soil_resistivity_correction(
    resistivity_ohm_m: float,
    reference_ohm_m: float = 50.0,
) -> float:
    """Soil resistivity correction factor per ISO 15589-1 guidance.

    Low resistivity (< reference) → higher corrosion activity → higher demand.
    High resistivity (> reference) → lower corrosion activity → lower demand.

    Uses a logarithmic correction model:
        k = (reference / resistivity) ^ 0.3

    Parameters
    ----------
    resistivity_ohm_m : float
        Measured soil resistivity [ohm-m].
    reference_ohm_m : float
        Reference resistivity for correction factor = 1.0 (default 50 ohm-m).

    Returns
    -------
    float
        Correction factor (>1 for low resistivity, <1 for high).
    """
    if resistivity_ohm_m <= 0:
        return 1.0
    return (reference_ohm_m / resistivity_ohm_m) ** 0.3


def check_potential_criteria(
    measured_potential_V_CSE: float,
    environment: str = "aerobic",
) -> CriteriaResult:
    """Check if measured potential meets protection criteria per NACE SP0169 §6.2.

    Criteria:
        - Aerobic soil: -0.850 V vs Cu/CuSO4 (CSE)
        - Anaerobic (SRB): -0.950 V vs CSE

    Parameters
    ----------
    measured_potential_V_CSE : float
        Measured pipe-to-soil potential [V vs CSE].
    environment : str
        "aerobic" or "anaerobic" (SRB environments).

    Returns
    -------
    CriteriaResult
        Protection status including overprotection check.
    """
    if environment.lower() == "anaerobic":
        criterion = PROTECTION_POTENTIAL_CSE_ANAEROBIC
    else:
        criterion = PROTECTION_POTENTIAL_CSE

    # Protection: measured must be more negative than (or equal to) criterion
    is_protected = measured_potential_V_CSE <= criterion

    # Overprotection: very negative potentials risk hydrogen embrittlement
    is_overprotected = measured_potential_V_CSE < OVERPROTECTION_THRESHOLD_V

    return CriteriaResult(
        measured_potential_V=measured_potential_V_CSE,
        criterion_V=criterion,
        is_protected=is_protected,
        is_overprotected=is_overprotected,
        environment=environment.lower(),
    )


def design_pipeline_cp(
    input_params: PipelineCPInput,
    anode_output_A: float = 0.5,
    assumed_potential_V_CSE: float = -0.900,
) -> PipelineDesignResult:
    """End-to-end pipeline CP design per API RP 1169 / ISO 15589-1.

    Combines current demand, anode spacing, soil correction, and
    protection criteria into a single design result.

    Parameters
    ----------
    input_params : PipelineCPInput
        Pipeline design parameters.
    anode_output_A : float
        Current output per anode [A] (default 0.5 A).
    assumed_potential_V_CSE : float
        Assumed/measured protection potential [V vs CSE].

    Returns
    -------
    PipelineDesignResult
        Complete pipeline CP design result.
    """
    # Current demand
    i_demand = calculate_pipeline_current_demand(input_params)

    # Surface area
    total_area = math.pi * input_params.outer_diameter_m * input_params.length_m
    effective_bare = total_area * input_params.coating_breakdown_factor

    # Soil correction
    k_soil = soil_resistivity_correction(input_params.soil_resistivity_ohm_m)

    # Anode spacing
    spacing, n_anodes = calculate_anode_spacing(
        pipeline_length_m=input_params.length_m,
        total_current_demand_A=i_demand,
        anode_output_A=anode_output_A,
    )

    # Determine environment type for criteria check
    env_str = "aerobic"  # default
    if input_params.environment in (
        PipelineEnvironment.BURIED_CLAY,
        PipelineEnvironment.HDD_CROSSING,
    ):
        env_str = "anaerobic"

    criteria = check_potential_criteria(
        measured_potential_V_CSE=assumed_potential_V_CSE,
        environment=env_str,
    )

    return PipelineDesignResult(
        current_demand_A=round(i_demand, 4),
        total_surface_area_m2=round(total_area, 2),
        effective_bare_area_m2=round(effective_bare, 4),
        number_of_anodes=n_anodes,
        anode_spacing_m=round(spacing, 2),
        soil_resistivity_correction_factor=round(k_soil, 4),
        protection_criteria=criteria,
    )
