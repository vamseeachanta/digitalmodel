"""Reliability -> OPEX driver for floating-wind LCOE (issue #1222).

The single biggest OPEX lever the Wood/Whooley deck identifies is **reliability**:
the API 17N whole-of-life reliability process, credited with reducing subsea
failure rates "by an order of magnitude". This module turns a **failure-rate
reduction** into its LCOE effect, on top of the core engine (:mod:`.economics`,
issue #1221).

Two physical mechanisms, both driven by the same failure-rate reduction ``r``
(0 = base reliability, 1 = failures eliminated):

1. **OPEX** -- a fraction :attr:`ReliabilityScenario.failure_related_opex_fraction`
   of O&M is unplanned/corrective (inspection, repair, mobilisation). Reducing
   failures cuts that fraction proportionally: ``OPEX(r) = OPEX_0 (1 - phi r)``.
2. **Availability** -- failures also cost energy (downtime). A base availability
   loss :attr:`ReliabilityScenario.base_availability_loss` is recovered in
   proportion to ``r``, raising the effective capacity factor.

Applied to the base case these defaults reproduce the deck's operations
sensitivity (slide 17): failure-rate reduction of 25 / 50 / 100% ->
**$173 / $162 / $139 per MWh** (from the $184 base), to <0.5%.

The API 17N Technology Readiness Level (TRL) tags carried here are metadata for
the technology-qualification gate (issue #1225); they do not alter the LCOE.

References
----------
* API RP 17N, *Recommended Practice for Subsea Production System Reliability,
  Technical Risk, and Integrity Management* -- whole-of-life reliability process.
* Wood plc / A. Whooley (2021), operations focus-area sensitivity (slide 17).
* ORE Catapult floating-wind O&M cost breakdown -- corrective/unplanned share.
"""

from __future__ import annotations

from pydantic import BaseModel, Field

from .economics import LCOEResult, ProjectEconomics, compute_lcoe

__all__ = [
    "ReliabilityScenario",
    "apply_reliability",
    "lcoe_with_reliability",
]


class ReliabilityScenario(BaseModel):
    """A failure-rate reduction and how it maps to OPEX and availability.

    The two coefficients are calibrated so the base case reproduces the deck's
    operations sensitivity; both are inputs and can be overridden per project.
    """

    failure_rate_reduction: float = Field(
        ...,
        ge=0.0,
        le=1.0,
        description="Fractional reduction in failure rate vs base (0..1)",
    )
    failure_related_opex_fraction: float = Field(
        0.93,
        ge=0.0,
        le=1.0,
        description="Fraction of OPEX that is failure-driven (unplanned/corrective)",
    )
    base_availability_loss: float = Field(
        0.033,
        ge=0.0,
        lt=1.0,
        description="Energy fraction lost to failure downtime at base reliability",
    )

    @property
    def opex_multiplier(self) -> float:
        """Multiplier applied to OPEX for this failure-rate reduction."""
        return 1.0 - self.failure_related_opex_fraction * self.failure_rate_reduction

    @property
    def availability_gain(self) -> float:
        """Fractional uplift to energy production from recovered availability."""
        lam = self.base_availability_loss
        return lam * self.failure_rate_reduction / (1.0 - lam)


def apply_reliability(
    econ: ProjectEconomics, scenario: ReliabilityScenario
) -> ProjectEconomics:
    """Return a copy of ``econ`` with OPEX and capacity factor adjusted.

    OPEX is scaled down by the failure-related fraction; the capacity factor is
    raised by the recovered availability (capped at 1.0).
    """
    new_opex = econ.opex_per_kw_year * scenario.opex_multiplier
    new_cf = min(1.0, econ.capacity_factor * (1.0 + scenario.availability_gain))
    return econ.model_copy(
        update={"opex_per_kw_year": new_opex, "capacity_factor": new_cf}
    )


def lcoe_with_reliability(
    econ: ProjectEconomics, scenario: ReliabilityScenario
) -> LCOEResult:
    """Compute LCOE for ``econ`` under a reliability scenario."""
    return compute_lcoe(apply_reliability(econ, scenario))
