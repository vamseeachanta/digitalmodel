# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Proportional chemical allocation with shared umbilical hydraulics.

API 17P (Design and Operation of Subsea Production Systems: Subsea Structures
and Manifolds) identifies the equipment scope. Verified edition and clause
locators are unavailable; API design qualification is not established.

A well target is water rate [m3/s] times target chemical volume fraction.
The supplied total chemical rate is allocated in proportion to these demands.
The existing chemical_flow.check_delivery supplies common-tube friction and
hydrostatic head. At the node, branch loss=R*Q^2; an added restriction is
(node pressure - backpressure - branch loss)/Q^2 [Pa*s2/m6]. A negative
pressure margin gives a deficit and no feasible restriction. Dose results
are PROPOSED rates, realised only if hydraulically_feasible is true; this is
not a pressure-driven network solution. Under/over-dosing remains explicit
through dose_ratio, and is independent of hydraulic feasibility.

The reused tube model is steady, Newtonian and smooth-bore; its transitional
friction convention is inherited without claiming a conservative bound.
Zero flow bypasses check_delivery, whose zero-Reynolds input is undefined.
Inactive branches are assumed isolated. No standard dosage or coefficient is
supplied. Separate branch tubes and transient mixing are outside this screen.
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


def _nonnegative(*values: float) -> None:
    if any(not math.isfinite(value) or value < 0 for value in values):
        raise ValueError("values must be finite and nonnegative")


@dataclass(frozen=True)
class InjectionDemand:
    """Water rate, target volume fraction, backpressure and branch resistance."""

    water_rate_m3_s: float
    target_fraction: float
    backpressure_pa: float
    branch_resistance_pa_s2_m6: float

    def __post_init__(self) -> None:
        """Require positive water flow and finite nonnegative branch data."""
        _nonnegative(self.water_rate_m3_s, self.target_fraction, self.backpressure_pa,
                     self.branch_resistance_pa_s2_m6)
        if self.water_rate_m3_s == 0 or self.target_fraction > 1:
            raise ValueError("water rate must be positive and fraction at most one")

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
        return all(well.pressure_deficit_pa == 0 for well in self.wells)


def _well_dose(demand: InjectionDemand, rate: float, node_pressure: float) -> WellDose:
    if rate == 0:
        ratio = 0.0 if demand.target_fraction else None
        return WellDose(0.0, 0.0, ratio, None, 0.0)
    margin = (node_pressure - demand.backpressure_pa
              - demand.branch_resistance_pa_s2_m6 * rate**2)
    restriction = margin / rate**2 if margin >= 0 else None
    return WellDose(rate, rate / demand.water_rate_m3_s,
                    rate / demand.target_rate_m3_s, restriction, max(-margin, 0))


def distribute_chemical(total_rate_m3_s: float, demands: tuple[InjectionDemand, ...],
                        tube: UmbilicalTube, fluid: ChemicalFluid,
                        available_pressure_pa: float, vertical_drop_m: float) -> ChemicalDistribution:
    """Allocate a stated rate and screen the balancing pressure at each well."""
    _nonnegative(total_rate_m3_s, available_pressure_pa)
    if not available_pressure_pa or not demands:
        raise ValueError("positive available pressure and at least one well required")
    _nonnegative(tube.inner_diameter, tube.length, fluid.density, fluid.viscosity)
    if not all((tube.inner_diameter, tube.length, fluid.density, fluid.viscosity)):
        raise ValueError("tube and fluid properties must be positive")
    if not math.isfinite(vertical_drop_m):
        raise ValueError("vertical drop must be finite")
    target = sum(demand.target_rate_m3_s for demand in demands)
    if target == 0 and total_rate_m3_s:
        raise ValueError("cannot distribute nonzero chemical flow without demand")
    delivery = None
    node_pressure = available_pressure_pa + static_pressure(fluid, vertical_drop_m)
    if total_rate_m3_s:
        delivery = check_delivery(total_rate_m3_s, tube, fluid, available_pressure_pa,
                                  vertical_drop=vertical_drop_m)
        node_pressure -= delivery.friction_drop
    wells = tuple(_well_dose(demand, total_rate_m3_s * demand.target_rate_m3_s / target
                             if target else 0.0, node_pressure) for demand in demands)
    return ChemicalDistribution(delivery, node_pressure, wells)
