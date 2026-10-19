# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Proposed proportional chemical allocation with finite-arithmetic screening.

Demand d=W*c, allocation q=Q*d/sum(d), dose=q/W and dose_ratio=q/d
use volume-ratio definitions and a proportional policy conserving flow:
OpenStax, University Physics vol. 1 (2016), sec. 14.5, eqs. 14.13–14.14.
Node pressure is supply + hydrostatic head - shared-tube friction, using
chemical_flow.check_delivery and static_pressure without duplicating them.
Head: OpenStax (2016), sec. 14.1, eq. 14.9; pressure balance: DOE-HDBK-1012/3-92
(1992), HT-03, Extended Bernoulli, pp. 25–26. Branch loss=R*q^2 and additional
restriction=(node-backpressure-R*q^2)/q^2 follow DOE HT-03 eq. 3-15, p. 34
with a caller-qualified quadratic resistance, not a universal laminar law.
No dosage, resistance, design factor or equipment rating is supplied.

Rates are proposed, not a pressure-driven network solution; feasibility only
screens pressure and does not certify dosage. Inactive branches are isolated.
Zero flow bypasses the shared model's undefined zero-Reynolds calculation.
Unrepresentable derived quantities raise ValueError instead of returning a
misleading isolated branch or feasibility result. No engineering cutoff is
introduced. API 17P qualification is NOT_EVALUATED: edition/clauses and
upstream gravity/regime/correlation provenance remain OPEN. See
``docs/subsea/manifold-chemical-distribution.md`` for sources and worked cases.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.subsea.umbilicals.chemical_flow import (
    ChemicalFluid,
    DeliveryResult,
    UmbilicalTube,
    check_delivery,
    static_pressure,
)


def _finite(stage: str, *values: float) -> None:
    if any(not math.isfinite(value) for value in values):
        raise ValueError(f"{stage}: result outside finite numeric range")


def _nonnegative(*values: float) -> None:
    # Nonnegative domain: docs/subsea/manifold-chemical-distribution.md,
    # Input domain (slice 1); zero is not a standard acceptance limit.
    if any(not math.isfinite(value) or value < 0 for value in values):
        raise ValueError("values must be finite and nonnegative")


@dataclass(frozen=True)
class InjectionDemand:
    """Water rate, chemical/water volume ratio, backpressure and resistance.

    ``target_fraction`` is chemical volume divided by water volume, not the
    chemical fraction of the combined stream. This screen accepts ratios up
    to unity; chemical selection and target dosage remain caller inputs.
    """

    water_rate_m3_s: float
    target_fraction: float
    backpressure_pa: float
    branch_resistance_pa_s2_m6: float

    def __post_init__(self) -> None:
        """Require valid inputs and a representable nonzero demand if active."""
        _nonnegative(
            self.water_rate_m3_s, self.target_fraction, self.backpressure_pa,
            self.branch_resistance_pa_s2_m6,
        )
        # Zero excludes an undefined dose denominator (OpenStax 2016, sec. 14.5).
        # Unity preserves InjectionDemand.__post_init__ from digitalmodel #2494
        # (2026); see the worked example, Input domain. It is not a standard dose.
        if self.water_rate_m3_s == 0 or self.target_fraction > 1:
            raise ValueError("water rate must be positive and fraction at most one")
        _finite("target rate", self.target_rate_m3_s)
        if self.target_fraction and not self.target_rate_m3_s:
            raise ValueError("target rate: underflow of active demand")

    @property
    def target_rate_m3_s(self) -> float:
        """Return the chemical demand in m3/s."""
        return self.water_rate_m3_s * self.target_fraction


@dataclass(frozen=True)
class WellDose:
    """Proposed well rate and dose with balancing or pressure-deficit evidence."""

    rate_m3_s: float
    delivered_fraction: float
    dose_ratio: float | None
    restriction_pa_s2_m6: float | None
    pressure_deficit_pa: float


@dataclass(frozen=True)
class ChemicalDistribution:
    """Common supply result and ordered branch allocations."""

    delivery: DeliveryResult | None
    node_pressure_pa: float
    wells: tuple[WellDose, ...]

    @property
    def hydraulically_feasible(self) -> bool:
        """Return whether every active branch has a nonnegative pressure margin."""
        # Zero deficit means pressure demand is met: DOE HT-03 (1992), pp. 25–26.
        return all(well.pressure_deficit_pa == 0 for well in self.wells)


def _well_dose(demand: InjectionDemand, rate: float, node_pressure: float) -> WellDose:
    # Isolation/zero-dose definitions: worked example, Input domain (slice 1);
    # Q=dV/dt, OpenStax (2016), sec. 14.5, eq. 14.13.
    if rate == 0:
        ratio = 0.0 if demand.target_fraction else None
        return WellDose(0.0, 0.0, ratio, None, 0.0)
    # Quadratic exponent: DOE-HDBK-1012/3-92 (1992), HT-03, eq. 3-15, p. 34.
    squared_rate = rate * rate
    _finite("squared branch rate", squared_rate)
    if not squared_rate:
        raise ValueError("squared branch rate: underflow of active allocation")
    branch_loss = demand.branch_resistance_pa_s2_m6 * squared_rate
    _finite("branch pressure loss", branch_loss)
    margin = node_pressure - demand.backpressure_pa - branch_loss
    _finite("branch pressure margin", margin)
    # Zero margin is exact pressure equality: DOE HT-03 (1992), pp. 25–26.
    restriction = margin / squared_rate if margin >= 0 else None
    fraction = rate / demand.water_rate_m3_s
    ratio = rate / demand.target_rate_m3_s
    _finite("well dose", fraction, ratio)
    if not fraction or not ratio:
        raise ValueError("well dose: underflow of active allocation")
    if restriction is not None:
        _finite("balancing restriction", restriction)
    # Positive-part pressure deficit: DOE HT-03 (1992), pp. 25–26; zero
    # denotes exact pressure equality, not a standard acceptance factor.
    return WellDose(rate, fraction, ratio, restriction, max(-margin, 0))


def distribute_chemical(
    total_rate_m3_s: float,
    demands: tuple[InjectionDemand, ...],
    tube: UmbilicalTube,
    fluid: ChemicalFluid,
    available_pressure_pa: float,
    vertical_drop_m: float,
) -> ChemicalDistribution:
    """Allocate a stated rate and screen pressure, rejecting range failures.

    All pressures use a common datum. Water/chemical rates use the same volume
    conditions. Branches share node elevation; their resistance is independently
    qualified. Arithmetic range checks are computational, not design limits.
    """
    _nonnegative(total_rate_m3_s, available_pressure_pa)
    if not available_pressure_pa or not demands:
        raise ValueError("positive available pressure and at least one well required")
    _nonnegative(tube.inner_diameter, tube.length, fluid.density, fluid.viscosity)
    if not all((tube.inner_diameter, tube.length, fluid.density, fluid.viscosity)):
        raise ValueError("tube and fluid properties must be positive")
    if not math.isfinite(vertical_drop_m):
        raise ValueError("vertical drop must be finite")
    try:
        target = math.fsum(demand.target_rate_m3_s for demand in demands)
    except OverflowError as exc:
        raise ValueError("total target rate: result outside finite numeric range") from exc
    _finite("total target rate", target)
    if not target and total_rate_m3_s:
        raise ValueError("cannot distribute nonzero chemical flow without demand")
    head = static_pressure(fluid, vertical_drop_m)
    _finite("hydrostatic head", head)
    node_pressure = available_pressure_pa + head
    _finite("node pressure", node_pressure)
    delivery = None
    if total_rate_m3_s:
        try:
            delivery = check_delivery(
                total_rate_m3_s, tube, fluid, available_pressure_pa,
                vertical_drop=vertical_drop_m,
            )
        except (ArithmeticError, ValueError) as exc:
            raise ValueError("shared delivery: arithmetic outside numeric range") from exc
        _finite(
            "shared delivery", delivery.flow_rate, delivery.velocity, delivery.reynolds,
            delivery.friction_factor, delivery.friction_drop, delivery.static_pressure,
            delivery.required_pressure, delivery.available_pressure, delivery.utilisation,
        )
        node_pressure -= delivery.friction_drop
        _finite("node pressure", node_pressure)
    wells = []
    for demand in demands:
        # Proportional policy, conserving Q (OpenStax 2016, sec. 14.5). Divide
        # before multiplying to avoid overflow of a representable allocation.
        rate = total_rate_m3_s * (demand.target_rate_m3_s / target) if target else 0.0
        _finite("allocated rate", rate)
        if total_rate_m3_s and demand.target_rate_m3_s and not rate:
            raise ValueError("allocated rate: underflow of active allocation")
        wells.append(_well_dose(demand, rate, node_pressure))
    return ChemicalDistribution(delivery, node_pressure, tuple(wells))
