"""Subsea-manifold well-to-header pressure-balance (commingling feasibility) check.

A subsea production manifold (API 17P) gathers flow from ``N`` subsea trees into a
common production header.  For a well to commingle into that header its flowing
pressure *delivered at the manifold header tap* must exceed the common header
pressure; the surplus differential is dissipated across the well's production
choke.  A well whose delivered pressure is at or below the header pressure cannot
flow into the header (it would be back-pressured out / require its own route).

Standard model (deterministic nodal balance)
--------------------------------------------
For each well ``i`` the pressure arriving at the manifold header tap is the tree
(tubing-head) flowing pressure less the friction/elevation loss in the jumper and
manifold branch piping between the tree and the header:

    p_arrival_i = p_tree_i - dp_jumper_i                                  (1)

Commingling into a shared header at pressure ``p_header`` is feasible for well
``i`` when:

    p_arrival_i >= p_header                                       (FEASIBLE)  (2)

The pressure that must be taken across the choke (the choke differential) to drop
the well from its arrival pressure to the header pressure is:

    dp_choke_i = p_arrival_i - p_header                                   (3)

A negative ``dp_choke_i`` means the well is *back-pressured*: it cannot deliver to
the header and is flagged infeasible.  The choke must also stay within its
available authority; if ``dp_choke_i`` exceeds ``dp_choke_max_i`` the well cannot
be throttled enough to match the header and the choke is over-ranged.

The whole manifold balances when every routed well is feasible and within choke
authority.  This is a steady-state nodal screening (equations 1-3); a full
transient / multiphase pressure-traverse solver is deferred (see package
docstring).

Units are consistent and caller-defined (e.g. all pressures in bar or psi, all
differentials in the same unit).  No unit conversion is performed.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class WellBalance:
    """Per-well outcome of the manifold commingling balance.

    Attributes
    ----------
    well_id:
        Caller-supplied identifier for the tree/well.
    p_arrival:
        Flowing pressure delivered at the manifold header tap (eq. 1).
    dp_choke:
        Differential pressure to be taken across the choke to match the header
        (eq. 3); negative means the well is back-pressured.
    feasible:
        ``True`` when the well can commingle (eq. 2) *and* the required choke
        differential is within the well's choke authority.
    back_pressured:
        ``True`` when ``p_arrival < p_header`` (cannot deliver to the header).
    choke_over_ranged:
        ``True`` when the required ``dp_choke`` exceeds ``dp_choke_max``.
    """

    well_id: str
    p_arrival: float
    dp_choke: float
    feasible: bool
    back_pressured: bool
    choke_over_ranged: bool


@dataclass(frozen=True)
class ManifoldBalanceResult:
    """Manifold-level outcome of the commingling pressure balance.

    Attributes
    ----------
    p_header:
        Common production-header pressure used for the balance.
    wells:
        Per-well :class:`WellBalance` results, in input order.
    balanced:
        ``True`` only when every routed well is feasible.
    n_infeasible:
        Count of wells that cannot commingle.
    """

    p_header: float
    wells: tuple[WellBalance, ...]
    balanced: bool
    n_infeasible: int


def well_balance(
    well_id: str,
    p_tree: float,
    p_header: float,
    dp_jumper: float = 0.0,
    dp_choke_max: float | None = None,
) -> WellBalance:
    """Evaluate commingling feasibility of one well into the manifold header.

    Parameters
    ----------
    well_id:
        Identifier for the well/tree.
    p_tree:
        Tree (tubing-head) flowing pressure of the well, must be >= 0.
    p_header:
        Common production-header pressure, must be >= 0.
    dp_jumper:
        Friction + elevation pressure loss in the jumper / branch piping between
        the tree and the header tap, must be >= 0 (default 0.0).
    dp_choke_max:
        Maximum differential the well's production choke can take (its authority).
        If ``None`` (default) the choke authority is treated as unbounded.

    Returns
    -------
    WellBalance
        Per-well balance outcome (eqs. 1-3).
    """
    if p_tree < 0:
        raise ValueError("p_tree must be >= 0")
    if p_header < 0:
        raise ValueError("p_header must be >= 0")
    if dp_jumper < 0:
        raise ValueError("dp_jumper must be >= 0")
    if dp_choke_max is not None and dp_choke_max < 0:
        raise ValueError("dp_choke_max must be >= 0 when provided")

    p_arrival = p_tree - dp_jumper          # eq. 1
    dp_choke = p_arrival - p_header         # eq. 3
    back_pressured = p_arrival < p_header   # eq. 2 violated
    choke_over_ranged = (
        dp_choke_max is not None and dp_choke > dp_choke_max
    )
    feasible = (not back_pressured) and (not choke_over_ranged)

    return WellBalance(
        well_id=well_id,
        p_arrival=p_arrival,
        dp_choke=dp_choke,
        feasible=feasible,
        back_pressured=back_pressured,
        choke_over_ranged=choke_over_ranged,
    )


def manifold_balance(
    wells: list[dict],
    p_header: float,
) -> ManifoldBalanceResult:
    """Evaluate the commingling balance for all wells routed to a manifold.

    Parameters
    ----------
    wells:
        List of per-well dicts, each with keys ``well_id`` and ``p_tree`` and the
        optional keys ``dp_jumper`` (default 0.0) and ``dp_choke_max``
        (default ``None``).
    p_header:
        Common production-header pressure (same unit as the well pressures).

    Returns
    -------
    ManifoldBalanceResult
        Aggregate balance; ``balanced`` is ``True`` only if every well is
        feasible.
    """
    if not wells:
        raise ValueError("at least one well is required")

    results = tuple(
        well_balance(
            well_id=str(w["well_id"]),
            p_tree=float(w["p_tree"]),
            p_header=p_header,
            dp_jumper=float(w.get("dp_jumper", 0.0)),
            dp_choke_max=(
                None if w.get("dp_choke_max") is None
                else float(w["dp_choke_max"])
            ),
        )
        for w in wells
    )
    n_infeasible = sum(1 for r in results if not r.feasible)

    return ManifoldBalanceResult(
        p_header=p_header,
        wells=results,
        balanced=n_infeasible == 0,
        n_infeasible=n_infeasible,
    )
