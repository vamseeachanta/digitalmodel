"""Anode consumption tracking and remaining life assessment.

Provides tools for tracking sacrificial anode depletion over time,
generating depletion profiles, calculating remaining anode life from
inspection data, and recommending inspection intervals.

Consumption model (issue #2211): the metal dissolved is the Faraday mass
``I * t * 8760 / epsilon``; the utilisation factor bounds the *usable*
mass ``M * u`` and does not divide the consumption. Remaining life is
``(M * u - consumed) * epsilon / (I * 8760)``, an anode is depleted when
``consumed >= M * u`` and the profile's end of life is the zero crossing
of its usable mass.

References
----------
- DNV-RP-B401 (2017) "Cathodic Protection Design" §7.7, §10.8
- ISO 15589-2 (2004) §8.5 — Anode mass requirements
- NACE SP0176 "Corrosion Control of Submerged Areas of Permanently
  Installed Steel Offshore Structures"
"""

from __future__ import annotations

from pydantic import BaseModel, Field

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection._edition import DEFAULT_EDITION, Edition
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    AnodeShape,
    anode_capacity,
    utilisation_factor,
)

# Anode properties derived from the cited DNV-RP-B401 table lookups at import
# time (issue #2207). Values are identical across all supported editions, so
# the package default edition is used here without a warning.
_TABLE_EDITION: Edition = DEFAULT_EDITION

ANODE_CAPACITY_ALZNI: float = anode_capacity(
    AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, _TABLE_EDITION
).value  # A-h/kg, Table 10-6 Al-based in seawater
ANODE_CAPACITY_ZN: float = anode_capacity(
    AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, _TABLE_EDITION
).value  # A-h/kg, Table 10-6 Zn-based in seawater
# Not a B401 value (Table 10-6 has no magnesium row): API RP 1632 practical
# Mg H-1 capacity, ~500 A-h/lb = 1100 A-h/kg (the earlier 500 was the
# per-pound figure; #2209 fixed the same slip in api_rp_1632).
ANODE_CAPACITY_MG: float = 1100.0  # A-h/kg (Magnesium H-1)
UTILIZATION_FACTOR_STANDOFF: float = utilisation_factor(
    AnodeShape.LONG_SLENDER_STANDOFF, _TABLE_EDITION
).value  # Table 10-8, long slender stand-off
UTILIZATION_FACTOR_FLUSH: float = utilisation_factor(
    AnodeShape.LONG_FLUSH, _TABLE_EDITION
).value  # Table 10-8, long flush-mounted
UTILIZATION_FACTOR_BRACELET: float = utilisation_factor(
    AnodeShape.SHORT_FLUSH_BRACELET, _TABLE_EDITION
).value  # Table 10-8, short flush-mounted, bracelet and other types


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
        ..., description="Remaining gross mass at each time point [kg]"
    )
    usable_mass_kg: list[float] = Field(
        default_factory=list,
        description="Remaining usable mass M * u - consumed at each time point [kg]",
    )
    depletion_percentage: list[float] = Field(
        ..., description="Depletion percentage at each time point [%]"
    )
    end_of_life_year: float = Field(
        ..., description="Year when the usable mass M * u is exhausted"
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

    Mass consumed = max(I_mean * t * 8760 / capacity, measured loss)
    Usable mass = M * u_f
    Remaining life = (M * u_f - consumed) * capacity / (I_mean * 8760)
    Depleted when consumed >= M * u_f

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
    original = anode_status.original_mass_kg

    # Metal consumed by the current drawn (Faraday), without the utilisation
    # factor, which bounds the usable mass instead
    mass_consumed = kernel.mass_consumed(
        anode_status.mean_current_A, anode_status.elapsed_years, capacity
    )

    # Use the larger of calculated consumption and measured loss
    actual_loss = original - anode_status.current_mass_kg
    mass_consumed = min(max(mass_consumed, actual_loss), original)

    remaining = max(0.0, original - mass_consumed)
    usable_mass = original * u_f
    usable_remaining = max(0.0, usable_mass - mass_consumed)

    depletion_pct = min((mass_consumed / original) * 100.0, 100.0)

    # Remaining life until the usable mass is exhausted
    if anode_status.mean_current_A > 0:
        remaining_life = (usable_remaining * capacity) / (
            anode_status.mean_current_A * kernel.HOURS_PER_YEAR
        )
    else:
        remaining_life = float("inf")

    is_depleted = mass_consumed >= usable_mass

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
    constant current demand. The consumption rate is the Faraday rate
    ``I * 8760 / epsilon`` [kg/yr]; ``end_of_life_year`` is when the usable
    mass ``M * u`` is consumed, the zero crossing of ``usable_mass_kg``.

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
    usable: list[float] = []
    depletions: list[float] = []

    # Faraday consumption rate [kg/year], no utilisation factor
    consumption_rate = kernel.mass_consumed(mean_current_A, 1.0, anode_capacity_Ah_kg)

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
        usable.append(round(max(0.0, usable_mass - consumed), 2))
        depletions.append(round(depletion, 1))

        t += time_step_years

    return DepletionProfile(
        years=years,
        remaining_mass_kg=masses,
        usable_mass_kg=usable,
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
    del design_life_years, elapsed_years  # thresholds depend on depletion only

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
