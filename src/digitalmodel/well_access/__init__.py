# ABOUTME: W7 well-access / intervention / lifecycle screening package.
# ABOUTME: Statistical/parametric; BSEE-anchored calibration is a worldenergydata-side input.
"""digitalmodel.well_access — parametric well-access / intervention / lifecycle model (W7).

Statistical/parametric reliability-and-logistics screening engine for the
access->recovery thesis (dry-tree continuous access vs subsea mobilisation-gated
access). NOT a reservoir/flow/downhole simulator. Calibration (intervention rates,
durations, uptime) is empirical/BSEE-anchored and supplied as an INPUT — the
empirical extraction lives on the worldenergydata side (cross-repo).
"""

from .models import (
    AccessClass,
    AgeBandRate,
    InterventionType,
    ResourcePool,
    WellProgram,
    EventExpectation,
    ResourceDemand,
    UptimeResult,
    LifecycleSummary,
)
from .intervention_rates import (
    expected_demand_per_well,
    expected_events,
    resolve_access_multiplier,
    build_intervention_types,
)
from .resource_queue import compute_resource_demand
from .uptime_rollup import rollup_uptime
from .lifecycle_summary import run_lifecycle, compare_architectures
from .router import router

__all__ = [
    "AccessClass",
    "AgeBandRate",
    "InterventionType",
    "ResourcePool",
    "WellProgram",
    "EventExpectation",
    "ResourceDemand",
    "UptimeResult",
    "LifecycleSummary",
    "expected_demand_per_well",
    "expected_events",
    "resolve_access_multiplier",
    "build_intervention_types",
    "compute_resource_demand",
    "rollup_uptime",
    "run_lifecycle",
    "compare_architectures",
    "router",
]
