# ABOUTME: W7 intervention frequency — NHPP age-banded demand/realized/deferred events.
# ABOUTME: Demand calibrated from dry-tree analogs; access multiplier applied separately.
"""
digitalmodel.well_access.intervention_rates
===========================================

Expected intervention events per type via a **non-homogeneous Poisson process
(NHPP)** with an age-banded rate :math:`\\lambda_t(a)`. For a single well over
field life L the expected demand count is the integral of the rate over age:

    demand_t = N_wells * \\sum_bands rate_band * years_in_band

The **realized** count applies the access multiplier ``m_access`` for the
architecture/type; the **deferred-access backlog** is ``demand - realized``.

Circularity rule (load-bearing honesty)
---------------------------------------
Subsea-calibrated rates are already access-suppressed (you only see the
interventions access permitted). So the base demand rate must be calibrated from
**dry-tree-capable analogs**, and subsea is modelled only via the ``m_access``
multiplier. For the FrPS dry-tree unit there is **no operating analog**, so its
multiplier is an ENGINEERING ASSUMPTION supplied as input, never a result.
"""

from __future__ import annotations

from typing import Optional

from .models import (
    AccessClass,
    AgeBandRate,
    EventExpectation,
    InterventionType,
    WellProgram,
)


def _years_in_band(band: AgeBandRate, life_years: float) -> float:
    """Overlap (years) between an age band and the field life window [0, life]."""
    lo = max(0.0, band.age_min_yr)
    hi = min(float(life_years), band.age_max_yr)
    return max(0.0, hi - lo)


def expected_demand_per_well(itype: InterventionType, life_years: float) -> float:
    """NHPP integral: expected demand events for ONE well over field life.

    :math:`\\int_0^L \\lambda(a)\\,da` evaluated as a sum over age bands.
    """
    return round(
        sum(
            band.rate_per_well_year * _years_in_band(band, life_years)
            for band in itype.age_curve
        ),
        6,
    )


def expected_events(
    well_program: WellProgram,
    base_rates: list[InterventionType],
    access_multiplier: dict[str, float],
) -> list[EventExpectation]:
    """Compute demand / realized / deferred event counts per intervention type.

    Parameters
    ----------
    well_program : WellProgram
        Field well program (n_wells, life, access class).
    base_rates : list[InterventionType]
        Age-banded DEMAND rates (calibrated from dry-tree-capable analogs).
    access_multiplier : dict[str, float]
        Per intervention-type access multiplier ``m_access`` for this
        architecture (1.0 dry-tree; < 1 subsea). Missing types default to 1.0.

    Returns
    -------
    list[EventExpectation]
        One per intervention type, sorted by name for deterministic output.
    """
    results: list[EventExpectation] = []
    for itype in sorted(base_rates, key=lambda t: t.name):
        per_well = expected_demand_per_well(itype, well_program.field_life_years)
        demand = round(per_well * well_program.n_wells, 4)
        m = float(access_multiplier.get(itype.name, 1.0))
        m = max(0.0, min(1.0, m))
        realized = round(demand * m, 4)
        deferred = round(demand - realized, 4)
        results.append(
            EventExpectation(
                intervention_type=itype.name,
                demand=demand,
                realized=realized,
                deferred=deferred,
                low_confidence=itype.low_confidence,
            )
        )
    return results


def resolve_access_multiplier(
    access_class: AccessClass,
    multiplier_table: dict[str, dict[str, float]],
) -> tuple[dict[str, float], str]:
    """Resolve the per-type access multiplier dict for an architecture.

    ``multiplier_table`` is keyed by AccessClass value -> {intervention_type: m}.
    Returns the dict plus a basis string flagging assumption status for FrPS.
    """
    key = access_class.value
    table = multiplier_table.get(key, {})
    if access_class is AccessClass.DRY_TREE_FRPS:
        basis = (
            "FrPS dry-tree-unit access multiplier is an ENGINEERING ASSUMPTION "
            "(no operating analog) — supplied as input, not a calibrated result."
        )
    elif access_class in (AccessClass.DRY_TREE_SPAR, AccessClass.DRY_TREE_TLP):
        basis = "Dry-tree continuous access; multiplier ~1.0 (calibration anchor)."
    else:
        basis = (
            "Subsea access multiplier < 1.0 applied to dry-tree-calibrated demand "
            "(circularity guard: demand is NOT calibrated from subsea history)."
        )
    return table, basis


def build_intervention_types(calibration: dict) -> list[InterventionType]:
    """Build InterventionType objects from a calibration dict (calibration.yml).

    Expected shape::

        base_rates:
          <type_name>:
            duration_days: <float>
            resource_class: <str>
            low_confidence: <bool>            # optional
            age_curve:
              - {age_min_yr: 0, age_max_yr: 5, rate_per_well_year: 0.04}
              - ...
    """
    types: list[InterventionType] = []
    for name, spec in (calibration.get("base_rates") or {}).items():
        curve = [
            AgeBandRate(
                age_min_yr=float(b["age_min_yr"]),
                age_max_yr=float(b["age_max_yr"]),
                rate_per_well_year=float(b["rate_per_well_year"]),
            )
            for b in spec.get("age_curve", [])
        ]
        types.append(
            InterventionType(
                name=str(name),
                age_curve=curve,
                duration_days=float(spec.get("duration_days", 0.0)),
                resource_class=str(spec.get("resource_class", "modu")),
                low_confidence=bool(spec.get("low_confidence", False)),
            )
        )
    return types
