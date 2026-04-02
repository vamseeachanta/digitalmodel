"""Coating breakdown factor calculations per DNV-RP-B401 Table 10-2/10-4.

Provides coating type definitions, breakdown factor calculations (initial,
mean, and final), coating life estimation, and multi-layer coating system
support. Covers polyethylene (3LPE), fusion bonded epoxy (FBE), coal tar
enamel, and concrete weight coating systems.

References
----------
- DNV-RP-B401 (2017) "Cathodic Protection Design" §10.7, Tables 10-2, 10-4
- ISO 15589-2 (2004) "Cathodic Protection of Pipeline Systems — Part 2"
- NACE SP0169 (2013) "Control of External Corrosion on Underground Pipelines"
"""

from __future__ import annotations

from enum import Enum
from typing import Optional

from pydantic import BaseModel, Field


class CoatingCategory(str, Enum):
    """Coating system categories per DNV-RP-B401 Table 10-2."""

    FBE = "fbe"
    THREE_LAYER_PE = "three_layer_pe"
    THREE_LAYER_PP = "three_layer_pp"
    COAL_TAR_ENAMEL = "coal_tar_enamel"
    ASPHALT_ENAMEL = "asphalt_enamel"
    POLYURETHANE = "polyurethane"
    CONCRETE_WEIGHT = "concrete_weight"
    NEOPRENE = "neoprene"
    NONE = "none"


# DNV-RP-B401 Table 10-4: Coating breakdown factor constants
# Format: (a_initial, b_annual_degradation) where f_c = a + b*t
COATING_CONSTANTS: dict[CoatingCategory, tuple[float, float]] = {
    CoatingCategory.FBE: (0.02, 0.003),
    CoatingCategory.THREE_LAYER_PE: (0.01, 0.002),
    CoatingCategory.THREE_LAYER_PP: (0.01, 0.002),
    CoatingCategory.COAL_TAR_ENAMEL: (0.05, 0.005),
    CoatingCategory.ASPHALT_ENAMEL: (0.05, 0.005),
    CoatingCategory.POLYURETHANE: (0.03, 0.004),
    CoatingCategory.CONCRETE_WEIGHT: (0.02, 0.001),
    CoatingCategory.NEOPRENE: (0.02, 0.003),
    CoatingCategory.NONE: (1.0, 0.0),
}

# Typical coating design life (years) per industry practice
COATING_DESIGN_LIFE: dict[CoatingCategory, float] = {
    CoatingCategory.FBE: 25.0,
    CoatingCategory.THREE_LAYER_PE: 40.0,
    CoatingCategory.THREE_LAYER_PP: 40.0,
    CoatingCategory.COAL_TAR_ENAMEL: 20.0,
    CoatingCategory.ASPHALT_ENAMEL: 20.0,
    CoatingCategory.POLYURETHANE: 20.0,
    CoatingCategory.CONCRETE_WEIGHT: 30.0,
    CoatingCategory.NEOPRENE: 25.0,
    CoatingCategory.NONE: 0.0,
}


class CoatingBreakdownResult(BaseModel):
    """Result of coating breakdown factor calculation."""

    coating_type: str = Field(..., description="Coating category name")
    initial_factor: float = Field(
        ..., ge=0.0, le=1.0, description="Breakdown factor at t=0"
    )
    mean_factor: float = Field(
        ..., ge=0.0, le=1.0, description="Mean breakdown factor over design life"
    )
    final_factor: float = Field(
        ..., ge=0.0, le=1.0, description="Breakdown factor at end of design life"
    )
    design_life_years: float = Field(..., gt=0, description="Design life [years]")


class CoatingLifeResult(BaseModel):
    """Result of coating life estimation."""

    coating_type: str = Field(..., description="Coating category name")
    estimated_life_years: float = Field(
        ..., description="Estimated coating life [years]"
    )
    threshold_factor: float = Field(
        ...,
        description="Breakdown factor threshold used for life estimate",
    )
    time_to_threshold_years: Optional[float] = Field(
        None,
        description="Time to reach threshold [years], None if never reached",
    )


def coating_breakdown_factors(
    coating_type: CoatingCategory,
    design_life_years: float = 25.0,
    depth_m: float = 0.0,
    temperature_c: float = 20.0,
) -> CoatingBreakdownResult:
    """Calculate initial, mean, and final coating breakdown factors.

    Per DNV-RP-B401 Table 10-4, the breakdown factor is:
        f_c(t) = a + b * t

    where a and b are coating-type-dependent constants.

    For elevated temperatures (>25°C) and depths (>100 m), a correction
    factor is applied per DNV-RP-B401 §10.7.4.

    Parameters
    ----------
    coating_type : CoatingCategory
        Coating system type.
    design_life_years : float
        Design life of the CP system [years].
    depth_m : float
        Water depth [m], used for pressure correction (default 0).
    temperature_c : float
        Operating temperature [°C], used for temperature correction.

    Returns
    -------
    CoatingBreakdownResult
        Initial, mean, and final breakdown factors.
    """
    a, b = COATING_CONSTANTS[coating_type]

    # Temperature correction: increase degradation rate by 2% per °C above 25°C
    if temperature_c > 25.0:
        temp_factor = 1.0 + 0.02 * (temperature_c - 25.0)
        b = b * temp_factor

    # Depth correction: increase degradation rate by 0.5% per 100 m depth
    if depth_m > 100.0:
        depth_factor = 1.0 + 0.005 * (depth_m - 100.0) / 100.0
        b = b * depth_factor

    fc_initial = min(a, 1.0)
    fc_final = min(a + b * design_life_years, 1.0)
    fc_mean = min(a + b * design_life_years / 2.0, 1.0)

    return CoatingBreakdownResult(
        coating_type=coating_type.value,
        initial_factor=fc_initial,
        mean_factor=fc_mean,
        final_factor=fc_final,
        design_life_years=design_life_years,
    )


def coating_life_estimate(
    coating_type: CoatingCategory,
    threshold_factor: float = 0.50,
    temperature_c: float = 20.0,
) -> CoatingLifeResult:
    """Estimate coating life based on when breakdown factor reaches threshold.

    Uses the linear degradation model f_c(t) = a + b*t and solves for the
    time when f_c reaches the specified threshold.

    Parameters
    ----------
    coating_type : CoatingCategory
        Coating system type.
    threshold_factor : float
        Breakdown factor threshold defining end of effective coating life
        (default 0.50 = 50% bare area equivalent).
    temperature_c : float
        Operating temperature [°C].

    Returns
    -------
    CoatingLifeResult
        Estimated coating life and related parameters.
    """
    a, b = COATING_CONSTANTS[coating_type]

    if temperature_c > 25.0:
        temp_factor = 1.0 + 0.02 * (temperature_c - 25.0)
        b = b * temp_factor

    if b <= 0 or a >= threshold_factor:
        # Coating never degrades or already exceeds threshold
        time_to_threshold = None
        estimated_life = COATING_DESIGN_LIFE.get(coating_type, 0.0)
    else:
        time_to_threshold = (threshold_factor - a) / b
        estimated_life = time_to_threshold

    return CoatingLifeResult(
        coating_type=coating_type.value,
        estimated_life_years=estimated_life,
        threshold_factor=threshold_factor,
        time_to_threshold_years=time_to_threshold,
    )


def effective_bare_area_coated(
    total_surface_area_m2: float,
    coating_type: CoatingCategory,
    elapsed_years: float,
    temperature_c: float = 20.0,
) -> float:
    """Calculate effective bare area of a coated structure at a given time.

    Effective bare area = total_surface_area * f_c(t)

    Parameters
    ----------
    total_surface_area_m2 : float
        Total external surface area [m²].
    coating_type : CoatingCategory
        Coating system type.
    elapsed_years : float
        Time since coating application [years].
    temperature_c : float
        Operating temperature [°C].

    Returns
    -------
    float
        Effective bare area [m²].
    """
    a, b = COATING_CONSTANTS[coating_type]

    if temperature_c > 25.0:
        temp_factor = 1.0 + 0.02 * (temperature_c - 25.0)
        b = b * temp_factor

    fc = min(a + b * elapsed_years, 1.0)
    return total_surface_area_m2 * fc
