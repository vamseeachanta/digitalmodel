# ABOUTME: W7 lifecycle summary orchestrator — events + queue + uptime + dry-tree/subsea compare.
# ABOUTME: Deterministic; consumes a real-anchored calibration.yml. No fabricated rates.
"""
digitalmodel.well_access.lifecycle_summary
==========================================

Orchestrates the W7 engine for one or more architectures and emits a deterministic
lifecycle summary plus a dry-tree-vs-subsea **comparison block** (uptime delta,
backlog delta, mobilisation delta — the access->recovery thesis quantified).

All rates come from a calibration dict (``calibration.yml``). The empirical
extraction of those rates from BSEE history is the **worldenergydata-side input**
(cross-repo) — this engine consumes them but never invents them.
"""

from __future__ import annotations

from typing import Optional

from .intervention_rates import (
    build_intervention_types,
    expected_events,
    resolve_access_multiplier,
)
from .models import (
    AccessClass,
    LifecycleSummary,
    ResourcePool,
    WellProgram,
)
from .resource_queue import compute_resource_demand
from .uptime_rollup import rollup_uptime


def _build_pools(resources_cfg: list[dict]) -> list[ResourcePool]:
    pools: list[ResourcePool] = []
    for r in resources_cfg or []:
        pools.append(
            ResourcePool(
                resource_class=str(r["resource_class"]),
                available_days_per_year=float(r["available_days_per_year"]),
                mobilisation_lead_days=float(r.get("mobilisation_lead_days", 0.0)),
                batch_campaign=bool(r.get("batch_campaign", False)),
            )
        )
    return pools


def run_lifecycle(
    well_program: WellProgram,
    calibration: dict,
    access_multiplier_table: dict[str, dict[str, float]],
    resources_cfg: list[dict],
    deferred_loss_per_event_days: float,
) -> LifecycleSummary:
    """Run the full W7 chain for ONE architecture and return its lifecycle summary."""
    itypes = build_intervention_types(calibration)
    pools = _build_pools(resources_cfg)

    mult, basis = resolve_access_multiplier(
        well_program.access_class, access_multiplier_table
    )
    events = expected_events(well_program, itypes, mult)
    resources = compute_resource_demand(
        events, itypes, pools, well_program.field_life_years
    )
    uptime = rollup_uptime(
        events, itypes, well_program, deferred_loss_per_event_days
    )

    notes: list[str] = [basis]
    if any(e.low_confidence for e in events):
        notes.append(
            "Some intervention types are LOW-CONFIDENCE (sparse / pooled-analog "
            "calibration) — treat their counts as indicative only."
        )

    return LifecycleSummary(
        access_class=well_program.access_class,
        access_multiplier_basis=basis,
        events=events,
        resources=resources,
        uptime=uptime,
        total_demand_events=round(sum(e.demand for e in events), 4),
        total_realized_events=round(sum(e.realized for e in events), 4),
        total_deferred_events=round(sum(e.deferred for e in events), 4),
        notes=notes,
    )


def compare_architectures(
    base_program: WellProgram,
    dry_tree_class: AccessClass,
    subsea_class: AccessClass,
    calibration: dict,
    access_multiplier_table: dict[str, dict[str, float]],
    resources_cfg: list[dict],
    deferred_loss_per_event_days: float,
) -> dict:
    """Run two architectures (dry-tree vs subsea) and return the comparison block."""
    from dataclasses import replace

    dry = run_lifecycle(
        replace(base_program, access_class=dry_tree_class),
        calibration,
        access_multiplier_table,
        resources_cfg,
        deferred_loss_per_event_days,
    )
    sub = run_lifecycle(
        replace(base_program, access_class=subsea_class),
        calibration,
        access_multiplier_table,
        resources_cfg,
        deferred_loss_per_event_days,
    )

    def _mob(s: LifecycleSummary) -> float:
        return round(sum(r.mobilisation_days for r in s.resources), 2)

    return {
        "dry_tree": {"access_class": dry.access_class.value, "summary": dry},
        "subsea": {"access_class": sub.access_class.value, "summary": sub},
        "deltas": {
            "uptime_delta": round(
                (dry.uptime.field_uptime - sub.uptime.field_uptime), 4
            ),
            "deferred_backlog_delta_events": round(
                (sub.total_deferred_events - dry.total_deferred_events), 4
            ),
            "mobilisation_delta_days": round(_mob(sub) - _mob(dry), 2),
        },
        "interpretation": (
            "Uptime delta (dry-tree minus subsea): positive favours dry-tree continuous "
            "access. The deferred-backlog delta (subsea minus dry-tree) is the recovery-cap "
            "signal: subsea defers more interventions because access is mobilisation-gated. "
            "Mobilisation delta (subsea minus dry-tree total mob-days) is sign-aware: subsea "
            "carries a long per-campaign lead, but dry-tree realises every demanded event so "
            "it can log more total mob-days at much lower per-event overhead — read it with "
            "the deferred-backlog delta, not alone."
        ),
    }
