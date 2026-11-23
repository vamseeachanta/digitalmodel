"""Subsea-manifold multi-tree flow aggregation (production gathering).

A subsea manifold (API 17P) commingles the production of ``N`` trees into a common
header.  Under steady state the header total is the sum of the contributing well
streams (conservation of mass / volume at the gathering node):

    q_total      = sum_i q_i                                              (1)

For each phase ``p`` (oil, gas, water) the header rate is likewise additive:

    q_total_p    = sum_i q_i_p                                            (2)

Two derived gathering quantities used for header/flowline sizing follow directly:

    water_cut    = q_water / (q_oil + q_water)                           (3)
    gor          = q_gas / q_oil          (gas-oil ratio)                (4)

The commingled header rate must not exceed the header / downstream-flowline
hydraulic capacity ``q_capacity`` for the manifold to be within its rating:

    q_total <= q_capacity                                       (WITHIN)  (5)

This is a steady-state additive balance (eqs. 1-5).  Pressure-interaction effects
between commingled wells are handled separately by ``pressure_balance``; a full
multiphase network solver is deferred (see package docstring).

Units are consistent and caller-defined (e.g. all rates in stb/d, gas in
Mscf/d).  No unit conversion is performed.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class AggregateResult:
    """Outcome of the manifold multi-tree flow aggregation.

    Attributes
    ----------
    n_wells:
        Number of contributing wells/trees.
    q_oil, q_gas, q_water:
        Commingled header rates per phase (eq. 2).
    q_liquid:
        ``q_oil + q_water`` (header liquid rate).
    q_total:
        Total gathered rate = liquid + gas (eq. 1).
    water_cut:
        ``q_water / (q_oil + q_water)`` (eq. 3); ``None`` when liquid is zero.
    gor:
        ``q_gas / q_oil`` gas-oil ratio (eq. 4); ``None`` when oil is zero.
    within_capacity:
        ``True`` when ``q_total <= q_capacity`` (eq. 5); ``None`` when no
        capacity was supplied.
    """

    n_wells: int
    q_oil: float
    q_gas: float
    q_water: float
    q_liquid: float
    q_total: float
    water_cut: float | None
    gor: float | None
    within_capacity: bool | None


def aggregate_flow(
    wells: list[dict],
    q_capacity: float | None = None,
) -> AggregateResult:
    """Aggregate the production of ``N`` trees into a common manifold header.

    Parameters
    ----------
    wells:
        List of per-well dicts, each with optional phase-rate keys ``q_oil``,
        ``q_gas`` and ``q_water`` (each defaults to 0.0, must be >= 0).
    q_capacity:
        Optional header / downstream-flowline capacity in the same total-rate
        units as ``q_oil + q_water + q_gas``.  When supplied the result reports
        whether the commingled total stays within it (eq. 5).

    Returns
    -------
    AggregateResult
        Commingled header rates and derived gathering quantities.
    """
    if not wells:
        raise ValueError("at least one well is required")
    if q_capacity is not None and q_capacity < 0:
        raise ValueError("q_capacity must be >= 0 when provided")

    q_oil = q_gas = q_water = 0.0
    for w in wells:
        o = float(w.get("q_oil", 0.0))
        g = float(w.get("q_gas", 0.0))
        wat = float(w.get("q_water", 0.0))
        if o < 0 or g < 0 or wat < 0:
            raise ValueError("phase rates must be >= 0")
        q_oil += o
        q_gas += g
        q_water += wat

    q_liquid = q_oil + q_water          # eq. 2 (liquid)
    q_total = q_liquid + q_gas          # eq. 1
    water_cut = q_water / q_liquid if q_liquid > 0 else None   # eq. 3
    gor = q_gas / q_oil if q_oil > 0 else None                 # eq. 4
    within_capacity = (
        None if q_capacity is None else q_total <= q_capacity  # eq. 5
    )

    return AggregateResult(
        n_wells=len(wells),
        q_oil=q_oil,
        q_gas=q_gas,
        q_water=q_water,
        q_liquid=q_liquid,
        q_total=q_total,
        water_cut=water_cut,
        gor=gor,
        within_capacity=within_capacity,
    )
