# ABOUTME: W7 well-access data models — access classes, well programs, calibration, results.
# ABOUTME: Statistical/parametric only. No reservoir/flow/downhole sim.
"""
digitalmodel.well_access.models
===============================

Data models for the well-access / intervention / lifecycle screening engine (W7).

Scope honesty (enforced everywhere in this package)
---------------------------------------------------
Statistical / parametric model, NOT reservoir / flow / downhole simulation.
"Uptime" is a **time-availability fraction**, not a production rate. There is no
cost engine here — it feeds the existing economics module. Calibration is
empirical/frequentist; the base intervention rates are an INPUT (sourced from
BSEE history on the worldenergydata side), never invented here.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Optional


class AccessClass(str, Enum):
    """Well-access architecture classes."""

    SUBSEA_HUB_SPOKE = "subsea_hub_spoke"   # wet trees, mobilisation-gated
    DRY_TREE_SPAR = "dry_tree_spar"         # surface trees on a spar
    DRY_TREE_TLP = "dry_tree_tlp"           # surface trees on a TLP
    DRY_TREE_FRPS = "dry_tree_frps"         # FrPS dry-tree unit (no operating analog)


@dataclass
class AgeBandRate:
    """Per-age-band intervention demand rate for one intervention type.

    ``rate_per_well_year`` is the **demand** rate (events per well per year) within
    the age band ``[age_min_yr, age_max_yr)``. Demand is calibrated from dry-tree-
    capable analogs to avoid the access-suppression circularity (see calibration).
    """

    age_min_yr: float
    age_max_yr: float
    rate_per_well_year: float


@dataclass
class InterventionType:
    """One intervention type with an age-banded demand-rate curve and duration."""

    name: str
    age_curve: list[AgeBandRate]
    duration_days: float          # mean campaign/event duration in resource-days
    resource_class: str           # which resource pool services it
    low_confidence: bool = False  # flag when calibrated on sparse data / pooled analog


@dataclass
class ResourcePool:
    """A rig/vessel resource pool servicing interventions."""

    resource_class: str
    available_days_per_year: float
    mobilisation_lead_days: float = 0.0   # subsea adds lead; ~0 for dry-tree
    batch_campaign: bool = False          # subsea batches events into campaigns


@dataclass
class WellProgram:
    """The field's well program over its life."""

    n_wells: int
    field_life_years: int
    access_class: AccessClass
    first_production_year: int = 0        # calendar offset for age accounting
    facility_availability: float = 0.95   # A_facility (topsides/host uptime fraction)

    def __post_init__(self) -> None:
        if self.n_wells <= 0:
            raise ValueError(f"n_wells must be > 0, got {self.n_wells!r}")
        if self.field_life_years <= 0:
            raise ValueError(
                f"field_life_years must be > 0, got {self.field_life_years!r}"
            )
        if not 0.0 < self.facility_availability <= 1.0:
            raise ValueError(
                f"facility_availability must be in (0, 1], got "
                f"{self.facility_availability!r}"
            )


@dataclass
class EventExpectation:
    """Demand / realized / deferred event counts for one intervention type."""

    intervention_type: str
    demand: float       # what the wells WANT (access-neutral)
    realized: float     # what access actually permits = demand * m_access
    deferred: float     # demand - realized = deferred-access backlog
    low_confidence: bool = False


@dataclass
class ResourceDemand:
    """Resource-pool loading result."""

    resource_class: str
    demand_days: float
    available_days: float
    rho: float                # utilisation = demand_days / available_days
    wait_days: float          # screening-grade waiting time if rho > 1
    mobilisation_days: float  # total mobilisation overhead (subsea lead x campaigns)


@dataclass
class UptimeResult:
    """Lifecycle uptime rollup."""

    facility_availability: float
    f_intervention: float     # availability loss to realized interventions
    f_deferred: float         # availability loss to deferred-access backlog
    field_uptime: float       # U = A_facility * (1 - f_intervention - f_deferred)


@dataclass
class LifecycleSummary:
    """Top-level well-access lifecycle summary for one architecture."""

    access_class: AccessClass
    access_multiplier_basis: str
    events: list[EventExpectation] = field(default_factory=list)
    resources: list[ResourceDemand] = field(default_factory=list)
    uptime: Optional[UptimeResult] = None
    total_demand_events: float = 0.0
    total_realized_events: float = 0.0
    total_deferred_events: float = 0.0
    notes: list[str] = field(default_factory=list)
