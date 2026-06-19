# ABOUTME: W7 lifecycle uptime rollup — time-availability fraction, not a rate.
# ABOUTME: Avoids A_facility double-count by netting intervention/deferred losses off A.
"""
digitalmodel.well_access.uptime_rollup
======================================

Rolls realized interventions + deferred backlog into a lifecycle uptime fraction:

    U = A_facility * (1 - f_intervention - f_deferred)

where
  * ``A_facility`` is the host/topsides availability (an INPUT),
  * ``f_intervention`` is the production-time fraction lost to *realized*
    interventions (well shut in while worked over),
  * ``f_deferred`` is the production-time fraction lost to *deferred-access*
    backlog (wells that NEED intervention but cannot get access — the
    recovery-cap signal that differentiates subsea from dry-tree).

Double-count guard (load-bearing): intervention/deferred losses are applied as a
fraction *of* the facility-available time, so A_facility is never counted twice.
``f_intervention`` is bounded so the bracket cannot go negative.
"""

from __future__ import annotations

from .models import EventExpectation, InterventionType, UptimeResult, WellProgram


def rollup_uptime(
    events: list[EventExpectation],
    intervention_types: list[InterventionType],
    well_program: WellProgram,
    deferred_loss_per_event_days: float,
) -> UptimeResult:
    """Compute lifecycle field uptime from realized + deferred interventions.

    Parameters
    ----------
    events : list[EventExpectation]
        Demand/realized/deferred event counts per type.
    intervention_types : list[InterventionType]
        Provides duration_days per type (well downtime per realized event).
    well_program : WellProgram
        Provides n_wells, field_life_years, facility_availability.
    deferred_loss_per_event_days : float
        Production-days lost per DEFERRED event (a deferred workover means the
        affected well runs degraded/shut until access arrives — an input).

    Returns
    -------
    UptimeResult
    """
    dur_by_type = {t.name: t.duration_days for t in intervention_types}

    # Total well-available production-days over life across all wells.
    total_well_days = (
        well_program.n_wells * well_program.field_life_years * 365.0
    )

    realized_downtime_days = sum(
        e.realized * dur_by_type.get(e.intervention_type, 0.0) for e in events
    )
    deferred_downtime_days = sum(
        e.deferred * deferred_loss_per_event_days for e in events
    )

    f_intervention = (
        realized_downtime_days / total_well_days if total_well_days > 0 else 0.0
    )
    f_deferred = (
        deferred_downtime_days / total_well_days if total_well_days > 0 else 0.0
    )

    # Guard: keep the availability bracket in [0, 1].
    bracket = max(0.0, 1.0 - f_intervention - f_deferred)
    field_uptime = well_program.facility_availability * bracket

    return UptimeResult(
        facility_availability=round(well_program.facility_availability, 4),
        f_intervention=round(f_intervention, 6),
        f_deferred=round(f_deferred, 6),
        field_uptime=round(field_uptime, 4),
    )
