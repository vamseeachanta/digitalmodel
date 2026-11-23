# ABOUTME: W7 resource demand — deterministic M/D/1-style rig/vessel utilisation.
# ABOUTME: Screening-grade comparative queue, not a commitment-grade logistics model.
"""
digitalmodel.well_access.resource_queue
=======================================

Turns realized intervention events into resource-pool day-demand and a
deterministic M/D/1-style utilisation :math:`\\rho = \\text{demand-days} /
\\text{available-days}`. When :math:`\\rho > 1` the pool is oversubscribed and a
screening-grade waiting time is reported. Subsea pools add a mobilisation lead
:math:`L` and optional campaign batching.

This queue is **screening-grade and comparative** — it ranks architectures by
resource pressure; it is NOT a commitment-grade logistics/scheduling model.
"""

from __future__ import annotations

from .models import EventExpectation, InterventionType, ResourceDemand, ResourcePool


def compute_resource_demand(
    events: list[EventExpectation],
    intervention_types: list[InterventionType],
    pools: list[ResourcePool],
    field_life_years: float,
) -> list[ResourceDemand]:
    """Aggregate realized events into per-pool day-demand and utilisation.

    Parameters
    ----------
    events : list[EventExpectation]
        Realized (access-permitted) event counts per intervention type.
    intervention_types : list[InterventionType]
        Provides duration_days and resource_class per type.
    pools : list[ResourcePool]
        Resource pools (available days/yr, mobilisation lead, batching).
    field_life_years : float
        Used to scale available-days over the full life.

    Returns
    -------
    list[ResourceDemand]
        One per pool, sorted by resource_class for deterministic output.
    """
    dur_by_type = {t.name: t.duration_days for t in intervention_types}
    class_by_type = {t.name: t.resource_class for t in intervention_types}
    realized_by_type = {e.intervention_type: e.realized for e in events}

    # demand-days and event-count per resource class
    demand_days: dict[str, float] = {}
    event_counts: dict[str, float] = {}
    for tname, realized in realized_by_type.items():
        rclass = class_by_type.get(tname, "modu")
        demand_days[rclass] = demand_days.get(rclass, 0.0) + realized * dur_by_type.get(
            tname, 0.0
        )
        event_counts[rclass] = event_counts.get(rclass, 0.0) + realized

    results: list[ResourceDemand] = []
    for pool in sorted(pools, key=lambda p: p.resource_class):
        rclass = pool.resource_class
        base_demand = demand_days.get(rclass, 0.0)
        n_events = event_counts.get(rclass, 0.0)

        # Mobilisation overhead: lead per campaign. With batching, campaigns are
        # bounded by life (one per year max); without, one mob per event.
        if pool.batch_campaign:
            campaigns = min(n_events, float(field_life_years))
        else:
            campaigns = n_events
        mob_days = round(campaigns * pool.mobilisation_lead_days, 2)

        demand_total = round(base_demand + mob_days, 2)
        available = round(pool.available_days_per_year * field_life_years, 2)
        rho = round(demand_total / available, 4) if available > 0 else float("inf")
        # Deterministic screening waiting time: excess demand beyond capacity.
        wait_days = round(max(0.0, demand_total - available), 2)

        results.append(
            ResourceDemand(
                resource_class=rclass,
                demand_days=demand_total,
                available_days=available,
                rho=rho,
                wait_days=wait_days,
                mobilisation_days=mob_days,
            )
        )
    return results
