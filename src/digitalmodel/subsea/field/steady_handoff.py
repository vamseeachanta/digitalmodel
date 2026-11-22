# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Wire existing hydraulic screens without solving well deliverability.

Rates are prescribed liquid m3/s at one common reference condition; pressures
and supplied losses are Pa at one common datum. Only one liquid phase (oil or
water) is supported per case. Reference labels record the caller's basis, not
a unit conversion or independent qualification. All losses and finite choke
authorities are required inputs. No geometry or connector rating is inferred.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from digitalmodel.subsea.manifolds.flow_aggregator import (
    AggregateResult,
    aggregate_flow,
)
from digitalmodel.subsea.manifolds.pressure_balance import (
    ManifoldBalanceResult,
    WellBalance,
    manifold_balance,
)
from digitalmodel.subsea.trees.flow_capacity import (
    BoreCapacity,
    FlowHandoff,
    flow_handoff,
)


@dataclass(frozen=True)
class LiquidBasis:
    """Caller-stated single liquid phase, rate reference and pressure datum."""

    phase: str
    reference_condition: str
    pressure_datum: str


@dataclass(frozen=True)
class TreeNode:
    """Proposed production, capacity and supplied loss inside one tree."""

    id: str
    production_capacity: BoreCapacity
    production_rate_m3_s: float
    annulus_rate_m3_s: float
    pressure_pa: float
    tree_loss_pa: float
    loss_basis: str
    basis: LiquidBasis


@dataclass(frozen=True)
class JumperEdge:
    """Tree-to-manifold edge with supplied loss and finite choke authority."""

    id: str
    tree_id: str
    manifold_id: str
    pressure_loss_pa: float
    loss_basis: str
    choke_max_pa: float


@dataclass(frozen=True)
class ManifoldNode:
    """Proposed header pressure in Pa and positive liquid capacity in m3/s."""

    id: str
    pressure_pa: float
    capacity_m3_s: float
    basis: LiquidBasis


@dataclass(frozen=True)
class FlowlineEndpoint:
    """Named proposed inlet connected directly to the gathering header."""

    id: str
    manifold_id: str


@dataclass(frozen=True)
class FieldTopology:
    """Star graph: exactly one jumper per tree, one header and one outlet."""

    trees: tuple[TreeNode, ...]
    jumpers: tuple[JumperEdge, ...]
    manifold: ManifoldNode
    flowline: FlowlineEndpoint


@dataclass(frozen=True)
class FlowlineInlet:
    """Liquid inlet proposal in SI units under its supplied common basis."""

    flowline_id: str
    rate_m3_s: float
    pressure_pa: float
    basis: LiquidBasis


@dataclass(frozen=True)
class BranchHandoff:
    """Unclipped tree handoff and manifold pressure diagnostic for one edge."""

    tree_id: str
    jumper_id: str
    tree: FlowHandoff
    pressure: WellBalance


@dataclass(frozen=True)
class FieldHandoff:
    """Retain all proposals and diagnostics; acceptance is supplied-input feasibility."""

    topology: FieldTopology
    branches: tuple[BranchHandoff, ...]
    aggregate: AggregateResult
    balance: ManifoldBalanceResult
    proposed_inlet: FlowlineInlet
    accepted_inlet: FlowlineInlet | None


def _label(value: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("IDs and basis labels must be nonblank strings")


def _finite(*values: float) -> None:
    if any(
        isinstance(v, bool) or not isinstance(v, (int, float)) or not math.isfinite(v)
        for v in values
    ):
        raise ValueError("numeric inputs and derived outputs must be finite numbers")


def _nonnegative(*values: float) -> None:
    _finite(*values)
    if any(v < 0 for v in values):
        raise ValueError("numeric inputs must be nonnegative")


def _validate(topology: FieldTopology) -> None:
    trees, edges, header, outlet = (
        topology.trees,
        topology.jumpers,
        topology.manifold,
        topology.flowline,
    )
    if not trees or len(edges) != len(trees):
        raise ValueError("exactly one jumper per tree is required; at least one tree")
    ids = [n.id for n in (*trees, *edges, header, outlet)]
    for identifier in ids:
        _label(identifier)
    if len(set(ids)) != len(ids):
        raise ValueError("node and edge IDs must be unique")
    tree_ids = {tree.id for tree in trees}
    if (
        {edge.tree_id for edge in edges} != tree_ids
        or any(edge.manifold_id != header.id for edge in edges)
        or outlet.manifold_id != header.id
    ):
        raise ValueError("edge endpoints must connect each tree to the common header")
    _nonnegative(header.pressure_pa, header.capacity_m3_s)
    if header.capacity_m3_s == 0:
        raise ValueError("header capacity must be positive")
    for basis in (header.basis, *(tree.basis for tree in trees)):
        _label(basis.reference_condition)
        _label(basis.pressure_datum)
        if basis.phase not in ("oil", "water") or basis != header.basis:
            raise ValueError(
                "one common liquid phase, reference condition and datum required"
            )
    by_tree = {edge.tree_id: edge for edge in edges}
    for tree in trees:
        edge = by_tree[tree.id]
        _label(tree.loss_basis)
        _label(edge.loss_basis)
        _nonnegative(
            tree.production_rate_m3_s,
            tree.annulus_rate_m3_s,
            tree.pressure_pa,
            tree.tree_loss_pa,
            edge.pressure_loss_pa,
            edge.choke_max_pa,
        )
        if tree.annulus_rate_m3_s != 0:
            raise ValueError("nonzero annulus flow is unsupported")
        if not isinstance(tree.production_capacity, BoreCapacity):
            raise ValueError("production capacity must be a BoreCapacity")
        if (
            tree.tree_loss_pa > tree.pressure_pa
            or edge.pressure_loss_pa > tree.pressure_pa - tree.tree_loss_pa
        ):
            raise ValueError("supplied losses exceed available tree pressure")


def steady_handoff(topology: FieldTopology) -> FieldHandoff:
    """Screen all proposed branches; withhold acceptance if any screen fails.

    Invalid topology, unsupported bases and nonfinite results raise ValueError.
    Capacity and pressure infeasibility retain every branch and its proposed
    rate. No clipping, well removal or alternate operating point is performed.
    Equality at the pressure/capacity limits follows the existing leaf checks.
    """
    _validate(topology)
    header = topology.manifold
    edges = {edge.tree_id: edge for edge in topology.jumpers}
    handoffs = []
    pressure_inputs = []
    rate_inputs = []
    for tree in topology.trees:
        edge = edges[tree.id]
        handoff = flow_handoff(
            production_rate_m3_s=tree.production_rate_m3_s,
            annulus_rate_m3_s=tree.annulus_rate_m3_s,
            production_capacity=tree.production_capacity,
            annulus_capacity=None,
            upstream_pressure_pa=tree.pressure_pa,
            pressure_drop_pa=tree.tree_loss_pa,
        )
        _finite(
            handoff.production_rate_m3_s,
            handoff.annulus_rate_m3_s,
            handoff.downstream_pressure_pa,
        )
        handoffs.append(handoff)
        rate_inputs.append(
            {f"q_{header.basis.phase}": handoff.production_rate_m3_s, "q_gas": 0.0}
        )
        pressure_inputs.append(
            {
                "well_id": tree.id,
                "p_tree": handoff.downstream_pressure_pa,
                "dp_jumper": edge.pressure_loss_pa,
                "dp_choke_max": edge.choke_max_pa,
            }
        )
    aggregate = aggregate_flow(rate_inputs, q_capacity=header.capacity_m3_s)
    _finite(
        aggregate.q_oil,
        aggregate.q_water,
        aggregate.q_gas,
        aggregate.q_liquid,
        aggregate.q_total,
        *(v for v in (aggregate.water_cut, aggregate.gor) if v is not None),
    )
    balance = manifold_balance(pressure_inputs, p_header=header.pressure_pa)
    _finite(
        balance.p_header,
        *(v for well in balance.wells for v in (well.p_arrival, well.dp_choke)),
    )
    branches = tuple(
        BranchHandoff(tree.id, edges[tree.id].id, handoff, pressure)
        for tree, handoff, pressure in zip(topology.trees, handoffs, balance.wells)
    )
    proposed = FlowlineInlet(
        topology.flowline.id, aggregate.q_total, header.pressure_pa, header.basis
    )
    feasible = (
        all(h.within_capacity for h in handoffs)
        and aggregate.within_capacity
        and balance.balanced
    )
    return FieldHandoff(
        topology, branches, aggregate, balance, proposed, proposed if feasible else None
    )
