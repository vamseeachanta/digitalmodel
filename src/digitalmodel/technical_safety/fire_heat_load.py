"""Select credible sustained fires and cited incident heat-flux inputs.

Public method basis: Fire and Explosion Guidance, issue 2 (March 2018),
2.10.1–2.10.2 (jet/spray and pool release modes), 3.4.2.2 and 8.7.4
(water effectiveness and reliability). No numeric fire-water factor is assumed.
https://oeuk.org.uk/wp-content/uploads/2022/11/OEUK-Fire-and-Explosion-Guidelines-Issue-2.pdf

The selection is a screening policy using caller-established release and
ventilation conditions. Inventory and rate must be positive; no universal
mass/rate threshold or hydraulic release model is inferred. Both cases are
retained when liquid accumulation and a pressurised release are credible.

The evidence checklist below is an explicit screening policy, not a claim of
verified compliance with NORSOK. Fluxes are required edition-keyed values.
Only the issue-supplied (#3821) S-DP-001 Rev.2 (1996), 11.3 step 3, jet peak of
250 kW/m2 is bundled, as an unverified superseded test input. Pool, average
and reduced-average values remain required inputs. Peak and spatial-average
loads are kept distinct; water credit is confined to the latter.

These are incident fluxes, not absorbed wall heat, total fire power, duration,
temperature or rupture time. Flash fires, fireballs, changing releases and
consequence modelling require separate assessment.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Literal

from pydantic import StrictBool, model_validator

from .criteria import CitedValue, EditionCriteria, Positive, ScreeningModel

FireType = Literal["jet", "pool_fuel_controlled", "pool_ventilation_controlled"]


class FireScenario(ScreeningModel):
    """A credible sustained, ignited release; phase is the released inventory.

    Pool potential is assessed by the caller using drainage, flashing and
    accumulation conditions. Pressure alone is insufficient to eliminate it.
    Enclosure and ventilation are required inputs, not inferred from mass.
    """

    inventory_kg: Positive
    release_rate_kg_s: Positive
    phase: Literal["gas", "liquid", "two_phase"]
    pressurised_release: StrictBool
    liquid_pool_possible: StrictBool
    enclosure: Literal["open", "enclosed"]
    ventilation: Literal["fuel_controlled", "ventilation_controlled"]

    @model_validator(mode="after")
    def check_scenario(self) -> FireScenario:
        if self.phase == "gas" and self.liquid_pool_possible:
            raise ValueError(
                "gas-only release cannot form a liquid pool; resolve phase"
            )
        if self.enclosure == "open" and self.ventilation == "ventilation_controlled":
            raise ValueError("ventilation-controlled case requires an enclosed area")
        if not self.pressurised_release and not self.liquid_pool_possible:
            raise ValueError(
                "no supported sustained jet or pool fire; separate assessment required"
            )
        return self


class FireWaterEvidence(ScreeningModel):
    """Caller-confirmed conditions, all false until established for the target.

    Policy basis: Fire and Explosion Guidance issue 2 (2018), 3.4.2.2 and
    8.7.4. These flags require documented evidence, including effectiveness
    for the actual impinging jet where applicable. General area deluge alone
    cannot establish that effectiveness. Equipment/piping credit cannot be
    transferred to primary partitions or load-bearing structure.
    """

    spray_from_below: StrictBool = False
    spray_from_both_sides: StrictBool = False
    spray_from_above: StrictBool = False
    all_surfaces_covered: StrictBool = False
    supply_survives_accident: StrictBool = False
    immediate_detection_and_activation: StrictBool = False
    availability_assured: StrictBool = False
    scenario_effectiveness_demonstrated: StrictBool = False
    equipment_or_piping_target: StrictBool = False


@dataclass(frozen=True)
class FireHeatLoadResult:
    scenario: FireScenario
    criteria: EditionCriteria
    fire_type: FireType
    flux_basis: str
    criterion: CitedValue
    fire_water: FireWaterEvidence | None
    fire_water_credit_used: bool
    unmet_water_conditions: tuple[str, ...]

    @property
    def heat_flux_kw_m2(self) -> float:
        return self.criterion.value


def select_fire_types(scenario: FireScenario) -> tuple[FireType, ...]:
    """Select from release physics; Guidance issue 2 (2018), 2.10.1–2.10.2.

    A sustained pressurised release is screened as jet/spray; a credible
    liquid accumulation adds the caller-established pool ventilation case.
    """
    types: list[FireType] = []
    if scenario.pressurised_release:
        types.append("jet")
    if scenario.liquid_pool_possible:
        types.append(f"pool_{scenario.ventilation}")
    return tuple(types)


def screen_fire_heat_load(
    scenario: FireScenario,
    criteria: EditionCriteria,
    *,
    flux_basis: Literal["peak", "average"] = "peak",
    fire_water: FireWaterEvidence | None = None,
    allow_test_criteria: bool = False,
) -> tuple[FireHeatLoadResult, ...]:
    """Return every credible case with its absolute, cited flux [kW/m2].

    No reduction is calculated. A cited reduced-average criterion is selected
    only when every water-evidence condition is established. Otherwise the
    unreduced value is retained with the unmet conditions. Missing required
    values raise an error instead of silently substituting another fire type.
    """
    if flux_basis not in ("peak", "average"):
        raise ValueError("flux_basis must be peak or average")
    unmet = ()
    if fire_water is not None:
        unmet = tuple(
            key
            for key in FireWaterEvidence.model_fields
            if not getattr(fire_water, key)
        )
        if flux_basis != "average":
            unmet += ("average_flux_required",)
    credit = fire_water is not None and not unmet
    results = []
    for fire_type in select_fire_types(scenario):
        baseline = criteria.require(
            f"{fire_type}_{flux_basis}", allow_test_criteria=allow_test_criteria
        )
        criterion = baseline
        if credit:
            criterion = criteria.require(
                f"{fire_type}_water_average", allow_test_criteria=allow_test_criteria
            )
            if criterion.value >= baseline.value:
                raise ValueError(
                    "reduced average flux must be below uncredited average flux"
                )
        results.append(
            FireHeatLoadResult(
                scenario,
                criteria,
                fire_type,
                flux_basis,
                criterion,
                fire_water,
                credit,
                unmet,
            )
        )
    return tuple(results)
