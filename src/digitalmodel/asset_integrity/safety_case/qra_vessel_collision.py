"""Passing-traffic collision QRA with an explicit external-model boundary.

Basis: UK SCR-2015 regulation 16(1)(c)-(d); IMO MSC-MEPC.2/Circ.12/Rev.2
(2018), annex section 2 (frequency/probability), section 6.2.2 (data basis),
appendix 3 section 2 (event trees) and appendix 5 section 3.2.3 (PLL).
https://www.legislation.gov.uk/uksi/2015/398/regulation/16
https://www.imo.org/en/OurWork/Safety/Pages/FormalSafetyAssessment.aspx

Actual dependency on hydrodynamics.passing_ship: optional surge/sway/yaw
output from PassingShipCalculator.calculate_forces is retained as interaction
load context only, with units N, N and N*m. That package supplies neither a
collision probability nor collision damage, exposure or fatality consequences.
It also supplies no mooring response at this integration boundary. Interaction
force is not impact force or collision energy; even finite nonzero loads cannot
fill missing collision inputs. No force-to-fatality coefficient is invented.

The caller supplies annual transits, probability of a collision conditional
on one counted transit, mean fatalities conditional on a collision, and the
bases for all three. Traffic geometry, causation, avoidance and exposure belong
in those external models; this module does not estimate them. Missing
probability or consequence gives NOT_EVALUATED with the missing fields listed.

Worked synthetic example: 1000 transits/year * 0.0002 collisions/transit =
0.2 collisions/year; * 0.5 fatalities/collision = 0.1 fatalities/year. These
are arithmetic assumptions. They do not define a collision acceptance limit
or an individual annual fatality probability.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Annotated, Mapping

from pydantic import BaseModel, ConfigDict, Field

from .mah_register import MajorAccidentHazard, NonNegative, Text


class VesselCollisionInputs(BaseModel):
    """Explicit transit exposure, conditional probability and consequence."""

    model_config = ConfigDict(extra="forbid", frozen=True)

    transits_per_year: NonNegative
    # IMO FSA (2018), annex section 2: probability is bounded by zero and one;
    # these are mathematical bounds, not calibrated collision coefficients.
    collision_probability_per_transit: (
        Annotated[float, Field(strict=True, ge=0, le=1, allow_inf_nan=False)] | None
    )
    fatalities_per_collision: NonNegative | None
    traffic_basis: Text
    probability_basis: Text
    consequence_basis: Text


class PassingShipForceEvidence(BaseModel):
    """One interaction-force sample; signed loads, not collision consequences."""

    model_config = ConfigDict(extra="forbid", frozen=True)
    surge: float = Field(strict=True, allow_inf_nan=False, description="N")
    sway: float = Field(strict=True, allow_inf_nan=False, description="N")
    yaw: float = Field(strict=True, allow_inf_nan=False, description="N*m")


@dataclass(frozen=True)
class VesselCollisionResult:
    """Register-ready scenario and optional hydrodynamic context."""

    hazard: MajorAccidentHazard
    inputs: VesselCollisionInputs
    interaction_forces: PassingShipForceEvidence | None
    missing_inputs: tuple[str, ...]


def assess_vessel_collision(
    inputs: VesselCollisionInputs,
    *,
    hazard_id: str,
    description: str,
    barriers: tuple[str, ...],
    responsible_roles: tuple[str, ...],
    passing_ship_forces: Mapping[str, float] | None = None,
) -> VesselCollisionResult:
    """Calculate scenario frequency and expected fatalities, retaining provenance.

    EVALUATED means that the arithmetic has inputs; it is not an acceptance or
    ALARP verdict. Annual collision frequency can exceed one. No Poisson
    conversion to an annual occurrence probability is applied.

    The hazard's frequency_basis combines traffic and probability bases into
    one string. A MAHRegister CSV round-trip preserves that text but cannot
    reconstruct the separate bases; retain result.inputs for their structure.
    """
    missing = tuple(
        field
        for field in ("collision_probability_per_transit", "fatalities_per_collision")
        if getattr(inputs, field) is None
    )
    frequency = None
    if inputs.collision_probability_per_transit is not None:
        # Conditional event-tree multiplication: IMO FSA (2018), appendix 3
        # section 2. Transits/year times collisions/transit gives collisions/year.
        frequency = inputs.transits_per_year * inputs.collision_probability_per_transit
    hazard = MajorAccidentHazard(
        hazard_id=hazard_id,
        description=description,
        frequency_per_year=frequency,
        fatalities_per_event=inputs.fatalities_per_collision,
        frequency_basis=f"Traffic: {inputs.traffic_basis}; probability: {inputs.probability_basis}",
        consequence_basis=inputs.consequence_basis,
        barriers=barriers,
        responsible_roles=responsible_roles,
    )
    forces = (
        PassingShipForceEvidence.model_validate(dict(passing_ship_forces))
        if passing_ship_forces is not None
        else None
    )
    return VesselCollisionResult(hazard, inputs, forces, missing)
