"""Anode consumption tracking and remaining life assessment.

Provides tools for tracking sacrificial anode depletion over time,
generating depletion profiles, calculating remaining anode life from
inspection data, and recommending inspection intervals.

References
----------
- DNV-RP-B401 (2017) "Cathodic Protection Design" §7.7, §10.8
- ISO 15589-2 (2004) §8.5 — Anode mass requirements
- NACE SP0176 "Corrosion Control of Submerged Areas of Permanently
  Installed Steel Offshore Structures"
"""

from __future__ import annotations

import math
from typing import Optional

from pydantic import BaseModel, Field


# Standard anode properties
ANODE_CAPACITY_ALZNI: float = 2000.0  # A-h/kg (Al-Zn-In)
ANODE_CAPACITY_ZN: float = 780.0  # A-h/kg (Zinc)
ANODE_CAPACITY_MG: float = 500.0  # A-h/kg (Magnesium H-1)
UTILIZATION_FACTOR_STANDOFF: float = 0.90
UTILIZATION_FACTOR_FLUSH: float = 0.85
UTILIZATION_FACTOR_BRACELET: float = 0.80


class AnodeStatus(BaseModel):
    """Current status of an anode or anode group."""

    anode_id: str = Field(..., description="Anode or group identifier")
    original_mass_kg: float = Field(
        ..., gt=0, description="Original net anode mass [kg]"
    )
    current_mass_kg: float = Field(
        ..., ge=0, description="Current estimated net mass [kg]"
    )
    elapsed_years: float = Field(
        ..., ge=0, description="Years since installation"
    )
    mean_current_A: float = Field(
        ..., ge=0, description="Mean current demand [A]"
    )
    anode_capacity_Ah_kg: float = Field(
        default=ANODE_CAPACITY_ALZNI,
        description="Electrochemical capacity [A-h/kg]",
    )
    utilization_factor: float = Field(
        default=UTILIZATION_FACTOR_STANDOFF,
        gt=0,
        le=1.0,
        description="Anode utilization factor",
    )


class DepletionResult(BaseModel):
    """Result of anode depletion assessment."""

    anode_id: str = Field(..., description="Anode or group identifier")
    mass_consumed_kg: float = Field(
        ..., description="Mass consumed to date [kg]"
    )
    remaining_mass_kg: float = Field(
        ..., description="Remaining anode mass [kg]"
    )
    depletion_percentage: float = Field(
        ..., description="Percentage of anode mass consumed [%]"
    )
    remaining_life_years: float = Field(
        ..., description="Estimated remaining life [years]"
    )
    is_depleted: bool = Field(
        ..., description="Whether anode is effectively depleted"
    )


class DepletionProfile(BaseModel):
    """Time-series depletion profile."""

    years: list[float] = Field(
        ..., description="Time points [years]"
    )
    remaining_mass_kg: list[float] = Field(
        ..., description="Remaining mass at each time point [kg]"
    )
    depletion_percentage: list[float] = Field(
        ..., description="Depletion percentage at each time point [%]"
    )
    end_of_life_year: float = Field(
        ..., description="Year when utilizable mass is exhausted"
    )


class InspectionRecommendation(BaseModel):
    """Recommended inspection interval and actions."""

    next_inspection_years: float = Field(
        ..., description="Recommended time to next inspection [years from now]"
    )
    inspection_type: str = Field(
        ..., description="Recommended inspection type"
    )
    urgency: str = Field(
        ..., description="Urgency level: routine, priority, urgent, critical"
    )
    notes: str = Field(default="", description="Additional notes")


def calculate_remaining_life(
    anode_status: AnodeStatus,
) -> DepletionResult:
    """Calculate remaining anode life from current status.

    Mass consumed = I_mean * t * 8760 / (capacity * u_f)
    Remaining life = remaining_usable_mass * capacity * u_f / (I_mean * 8760)

    Parameters
    ----------
    anode_status : AnodeStatus
        Current anode condition data.

    Returns
    -------
    DepletionResult
        Depletion status and remaining life estimate.
    """
    capacity = anode_status.anode_capacity_Ah_kg
    u_f = anode_status.utilization_factor

    # Mass consumed based on current draw
    mass_consumed = (
        anode_status.mean_current_A * anode_status.elapsed_years * 8760.0
    ) / (capacity * u_f)

    # Use minimum of calculated consumption and actual loss
    actual_loss = anode_status.original_mass_kg - anode_status.current_mass_kg
    mass_consumed = max(mass_consumed, actual_loss)

    remaining = max(0.0, anode_status.original_mass_kg - mass_consumed)
    usable_remaining = remaining * u_f

    depletion_pct = (mass_consumed / anode_status.original_mass_kg) * 100.0
    depletion_pct = min(depletion_pct, 100.0)

    # Remaining life
    if anode_status.mean_current_A > 0:
        remaining_life = (usable_remaining * capacity) / (
            anode_status.mean_current_A * 8760.0
        )
    else:
        remaining_life = float("inf")

    is_depleted = depletion_pct >= (u_f * 100.0)

    return DepletionResult(
        anode_id=anode_status.anode_id,
        mass_consumed_kg=round(mass_consumed, 2),
        remaining_mass_kg=round(remaining, 2),
        depletion_percentage=round(depletion_pct, 1),
        remaining_life_years=round(remaining_life, 2),
        is_depleted=is_depleted,
    )


def generate_depletion_profile(
    original_mass_kg: float,
    mean_current_A: float,
    design_life_years: float,
    anode_capacity_Ah_kg: float = ANODE_CAPACITY_ALZNI,
    utilization_factor: float = UTILIZATION_FACTOR_STANDOFF,
    time_step_years: float = 1.0,
) -> DepletionProfile:
    """Generate a time-series anode depletion profile.

    Creates a year-by-year projection of remaining anode mass assuming
    constant current demand.

    Parameters
    ----------
    original_mass_kg : float
        Original anode net mass [kg].
    mean_current_A : float
        Mean current demand [A].
    design_life_years : float
        Projection period [years].
    anode_capacity_Ah_kg : float
        Electrochemical capacity [A-h/kg].
    utilization_factor : float
        Anode utilization factor.
    time_step_years : float
        Time step for the profile [years].

    Returns
    -------
    DepletionProfile
        Time series of remaining mass and depletion percentage.
    """
    years: list[float] = []
    masses: list[float] = []
    depletions: list[float] = []

    # Consumption rate [kg/year]
    if mean_current_A > 0:
        consumption_rate = (mean_current_A * 8760.0) / (
            anode_capacity_Ah_kg * utilization_factor
        )
    else:
        consumption_rate = 0.0

    usable_mass = original_mass_kg * utilization_factor
    if consumption_rate > 0:
        eol_year = usable_mass / consumption_rate
    else:
        eol_year = design_life_years

    t = 0.0
    while t <= design_life_years + time_step_years / 2:
        consumed = consumption_rate * t
        remaining = max(0.0, original_mass_kg - consumed)
        depletion = min((consumed / original_mass_kg) * 100.0, 100.0)

        years.append(round(t, 2))
        masses.append(round(remaining, 2))
        depletions.append(round(depletion, 1))

        t += time_step_years

    return DepletionProfile(
        years=years,
        remaining_mass_kg=masses,
        depletion_percentage=depletions,
        end_of_life_year=round(eol_year, 2),
    )


def recommend_inspection_interval(
    depletion_result: DepletionResult,
    design_life_years: float,
    elapsed_years: float,
) -> InspectionRecommendation:
    """Recommend inspection interval based on anode depletion status.

    Follows general industry guidelines:
    - <50% depleted: routine inspection every 5 years
    - 50-75% depleted: priority inspection every 2-3 years
    - 75-90% depleted: urgent inspection every 1 year
    - >90% depleted: critical — immediate action

    Parameters
    ----------
    depletion_result : DepletionResult
        Current depletion assessment.
    design_life_years : float
        Original design life [years].
    elapsed_years : float
        Years since installation.

    Returns
    -------
    InspectionRecommendation
        Recommended inspection interval, type, and urgency.
    """
    pct = depletion_result.depletion_percentage
    remaining_life = depletion_result.remaining_life_years
    remaining_design = design_life_years - elapsed_years

    if depletion_result.is_depleted or pct >= 90.0:
        return InspectionRecommendation(
            next_inspection_years=0.0,
            inspection_type="visual_and_potential_survey",
            urgency="critical",
            notes=(
                f"Anode {depletion_result.anode_id} is {pct:.0f}% depleted. "
                "Immediate retrofit or replacement required."
            ),
        )
    elif pct >= 75.0:
        interval = min(1.0, remaining_life / 2.0)
        return InspectionRecommendation(
            next_inspection_years=round(interval, 1),
            inspection_type="detailed_underwater_inspection",
            urgency="urgent",
            notes=(
                f"Anode {depletion_result.anode_id} is {pct:.0f}% depleted. "
                f"Estimated remaining life: {remaining_life:.1f} years. "
                "Plan for retrofit."
            ),
        )
    elif pct >= 50.0:
        interval = min(3.0, remaining_life / 3.0)
        return InspectionRecommendation(
            next_inspection_years=round(interval, 1),
            inspection_type="potential_survey_and_visual",
            urgency="priority",
            notes=(
                f"Anode {depletion_result.anode_id} is {pct:.0f}% depleted. "
                f"Estimated remaining life: {remaining_life:.1f} years."
            ),
        )
    else:
        interval = min(5.0, remaining_life / 3.0)
        return InspectionRecommendation(
            next_inspection_years=round(interval, 1),
            inspection_type="general_visual_inspection",
            urgency="routine",
            notes=(
                f"Anode {depletion_result.anode_id} is {pct:.0f}% depleted. "
                f"CP system performing within design parameters."
            ),
        )
