"""LCOE as a first-class trade-space objective (issue #1224).

The concept-screening trade space (:mod:`.tradespace`, issue #1026) ranks
variants on *physical* objectives -- steel mass, peak motion, stability margin.
This module adds the **economics axis**: it attaches an ``lcoe_usd_per_mwh``
metric to each variant record (via the core engine, :mod:`.economics`, issue
#1221) so the Pareto front can trade **cost against feasibility**, which is the
whole point of the Wood/Whooley "multi-dimensional evaluation" (slide 11).

Variant -> LCOE mapping
-----------------------
A screening variant differs from the base project mainly in the **floater**, and
the dominant floater cost driver is hull **steel mass**. So each variant's LCOE
is the base project economics with the *substructure* CAPEX scaled by the
variant's steel mass relative to a reference::

    substructure_$/kW(variant) = base.substructure_$/kW * steel_mass_t / ref_t

All other cost bases (turbine, mooring, cables, OPEX, financial) come from the
base :class:`.economics.ProjectEconomics`. The reference steel mass defaults to
the mean over the (feasible) variant set, so LCOE differences across the trade
space reflect relative hull cost; pass ``reference_steel_mass_t`` to anchor it
to a known design.

The Pareto machinery (dominance, objective coercion) is reused from
:mod:`.tradespace` unchanged -- this module only enriches the records, so a
mixed objective set like ``[("lcoe_usd_per_mwh", "min"), ("steel_mass_t", "min")]``
just works.

References
----------
* Wood plc / A. Whooley (2021), technology selection / multi-dimensional
  evaluation (slide 11).
"""

from __future__ import annotations

from typing import Any, Iterable

from digitalmodel.floating_wind.economics import ProjectEconomics, compute_lcoe
from digitalmodel.floating_wind.screening import ScreeningResults, VariantScreening
from digitalmodel.floating_wind.tradespace import (
    Objective,
    _coerce_objectives,
    _dominates,
    _variants,
    variant_metrics,
)

__all__ = [
    "LCOE_METRIC_KEY",
    "variant_lcoe",
    "mean_steel_mass_t",
    "lcoe_records",
    "pareto_front_with_lcoe",
]

LCOE_METRIC_KEY = "lcoe_usd_per_mwh"


def variant_lcoe(
    variant: VariantScreening,
    base_econ: ProjectEconomics,
    *,
    reference_steel_mass_t: float,
) -> float:
    """LCOE ($/MWh) for one variant, scaling substructure CAPEX by steel mass."""
    if reference_steel_mass_t <= 0.0:
        raise ValueError("reference_steel_mass_t must be > 0")
    scale = variant.properties.steel_mass_t / reference_steel_mass_t
    capex = base_econ.capex.model_copy(
        update={"substructure": base_econ.capex.substructure * scale}
    )
    return compute_lcoe(base_econ.model_copy(update={"capex": capex})).lcoe_usd_per_mwh


def mean_steel_mass_t(variants: Iterable[VariantScreening]) -> float:
    """Mean hull steel mass over ``variants`` (the default LCOE reference)."""
    masses = [v.properties.steel_mass_t for v in variants]
    if not masses:
        raise ValueError("no variants to derive a reference steel mass from")
    return sum(masses) / len(masses)


def lcoe_records(
    results: ScreeningResults | Iterable[VariantScreening],
    base_econ: ProjectEconomics,
    *,
    reference_steel_mass_t: float | None = None,
    feasible_only: bool = True,
) -> list[dict[str, Any]]:
    """Metric records enriched with an ``lcoe_usd_per_mwh`` column."""
    variants = _variants(results)
    if feasible_only:
        variants = [v for v in variants if v.feasible]
    if not variants:
        return []
    ref = (
        reference_steel_mass_t
        if reference_steel_mass_t is not None
        else mean_steel_mass_t(variants)
    )
    records: list[dict[str, Any]] = []
    for v in variants:
        rec = variant_metrics(v)
        rec[LCOE_METRIC_KEY] = variant_lcoe(v, base_econ, reference_steel_mass_t=ref)
        records.append(rec)
    return records


def pareto_front_with_lcoe(
    results: ScreeningResults | Iterable[VariantScreening],
    base_econ: ProjectEconomics,
    objectives: list[Objective] | list[tuple[str, str]] | list[dict],
    *,
    reference_steel_mass_t: float | None = None,
    passed_only: bool = False,
) -> list[dict[str, Any]]:
    """Non-dominated records over ``objectives``, with LCOE available as a key.

    Identical semantics to :func:`.tradespace.pareto_front`, but records carry
    ``lcoe_usd_per_mwh`` so it can appear among the objectives, e.g.
    ``[("lcoe_usd_per_mwh", "min"), ("steel_mass_t", "min")]``.
    """
    objs = _coerce_objectives(objectives)
    records = lcoe_records(
        results,
        base_econ,
        reference_steel_mass_t=reference_steel_mass_t,
        feasible_only=True,
    )
    if passed_only:
        records = [r for r in records if r.get("passed")]

    front: list[dict[str, Any]] = []
    for i, rec in enumerate(records):
        dominated = any(
            _dominates(other, rec, objs)
            for j, other in enumerate(records)
            if j != i
        )
        if not dominated:
            front.append(rec)
    return front
