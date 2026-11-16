# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent two-tree arithmetic and composition-boundary checks."""

import importlib
from dataclasses import replace

import pytest

from digitalmodel.subsea.trees.flow_capacity import BoreCapacity


@pytest.fixture
def api():
    return importlib.import_module("digitalmodel.subsea.field")


@pytest.fixture
def case(api):
    basis = api.LiquidBasis("oil", "synthetic common liquid condition", "common datum")
    trees = tuple(
        api.TreeNode(
            id=name,
            production_capacity=BoreCapacity(2, 0.01),
            production_rate_m3_s=rate,
            annulus_rate_m3_s=0,
            pressure_pa=pressure,
            tree_loss_pa=0,
            loss_basis="synthetic supplied tree loss",
            basis=basis,
        )
        for name, rate, pressure in (("tree-a", 0.001, 12e6), ("tree-b", 0.002, 11e6))
    )
    jumpers = tuple(
        api.JumperEdge(name, tree.id, "header", loss, "synthetic supplied loss", 4e6)
        for name, tree, loss in zip(("edge-a", "edge-b"), trees, (1e6, 2e6))
    )
    return api.FieldTopology(
        trees,
        jumpers,
        api.ManifoldNode("header", 8e6, 0.004, basis),
        api.FlowlineEndpoint("outlet", "header"),
    )


def change_tree(case, **kwargs):
    return replace(case, trees=(replace(case.trees[0], **kwargs), case.trees[1]))


def change_edge(case, **kwargs):
    return replace(case, jumpers=(replace(case.jumpers[0], **kwargs), case.jumpers[1]))


def test_two_tree_handoff(api, case):
    result = api.steady_handoff(case)
    assert [b.pressure.p_arrival for b in result.branches] == [11e6, 9e6]
    assert [b.pressure.dp_choke for b in result.branches] == [3e6, 1e6]
    assert [b.tree.production_rate_m3_s for b in result.branches] == [0.001, 0.002]
    assert [b.tree_id for b in result.branches] == ["tree-a", "tree-b"]
    assert [b.jumper_id for b in result.branches] == ["edge-a", "edge-b"]
    assert result.aggregate.q_total == pytest.approx(0.003)
    assert result.aggregate.q_gas == result.aggregate.q_water == 0
    assert result.aggregate.within_capacity and result.balance.balanced
    assert result.accepted_inlet == result.proposed_inlet
    assert result.accepted_inlet.rate_m3_s == pytest.approx(0.003)
    assert result.accepted_inlet.pressure_pa == 8e6
    assert result.accepted_inlet.flowline_id == "outlet"
    assert result.accepted_inlet.basis == case.manifold.basis
    assert result.topology == case  # Supplied-loss provenance is retained.


def test_tree_loss_applied_once_and_edges_matched_by_endpoint(api, case):
    case = change_tree(case, tree_loss_pa=0.5e6)
    case = replace(case, jumpers=tuple(reversed(case.jumpers)))
    result = api.steady_handoff(case)
    assert result.branches[0].tree.downstream_pressure_pa == 11.5e6
    assert result.branches[0].pressure.p_arrival == 10.5e6
    assert result.branches[0].pressure.dp_choke == 2.5e6


@pytest.mark.parametrize("failure", ["tree", "header", "back-pressure", "choke"])
def test_infeasible_proposal_is_preserved_but_not_accepted(api, case, failure):
    if failure == "tree":
        case = change_tree(case, production_rate_m3_s=0.02)
    elif failure == "header":
        case = replace(case, manifold=replace(case.manifold, capacity_m3_s=0.002))
    elif failure == "back-pressure":
        case = change_tree(case, pressure_pa=8e6)
    else:
        case = change_edge(case, choke_max_pa=2e6)
    result = api.steady_handoff(case)
    assert result.accepted_inlet is None
    assert len(result.branches) == result.aggregate.n_wells == 2
    expected_rate = 0.022 if failure == "tree" else 0.003
    assert result.proposed_inlet.rate_m3_s == pytest.approx(expected_rate)
    assert (
        result.branches[0].tree.production_rate_m3_s
        == case.trees[0].production_rate_m3_s
    )
    if failure == "tree":
        assert not result.branches[0].tree.within_capacity
    elif failure == "header":
        assert not result.aggregate.within_capacity
    elif failure == "back-pressure":
        assert result.branches[0].pressure.back_pressured
    else:
        assert result.branches[0].pressure.choke_over_ranged


@pytest.mark.parametrize(
    "change",
    [
        lambda c: replace(c, trees=()),
        lambda c: replace(c, jumpers=c.jumpers[:1]),
        lambda c: replace(c, jumpers=c.jumpers + (replace(c.jumpers[0], id="extra"),)),
        lambda c: change_edge(c, tree_id="unknown"),
        lambda c: change_edge(c, manifold_id="unknown"),
        lambda c: replace(c, flowline=replace(c.flowline, manifold_id="unknown")),
        lambda c: change_tree(c, id="tree-b"),
        lambda c: change_tree(c, id="header"),
        lambda c: change_edge(c, id="edge-b"),
        lambda c: change_edge(c, id="outlet"),
        lambda c: change_tree(c, id=" "),
        lambda c: change_edge(c, id=""),
        lambda c: replace(c, flowline=replace(c.flowline, id="")),
        lambda c: replace(c, manifold=replace(c.manifold, id="")),
    ],
)
def test_malformed_topology_rejected_before_leaf_calls(api, case, change, monkeypatch):
    module = importlib.import_module("digitalmodel.subsea.field.steady_handoff")

    def unexpected(*args, **kwargs):
        pytest.fail("invalid topology reached a leaf")

    for name in ("flow_handoff", "aggregate_flow", "manifold_balance"):
        monkeypatch.setattr(module, name, unexpected)
    with pytest.raises(ValueError):
        api.steady_handoff(change(case))


@pytest.mark.parametrize(
    "field",
    [
        "production_rate_m3_s",
        "annulus_rate_m3_s",
        "pressure_pa",
        "tree_loss_pa",
    ],
)
@pytest.mark.parametrize("value", [-1, float("nan"), float("inf"), -float("inf")])
def test_invalid_tree_numbers(api, case, field, value):
    with pytest.raises(ValueError):
        api.steady_handoff(change_tree(case, **{field: value}))


@pytest.mark.parametrize(
    "target,field",
    [
        ("edge", "pressure_loss_pa"),
        ("edge", "choke_max_pa"),
        ("manifold", "pressure_pa"),
        ("manifold", "capacity_m3_s"),
    ],
)
@pytest.mark.parametrize("value", [-1, float("nan"), float("inf"), -float("inf")])
def test_invalid_edge_and_header_numbers(api, case, target, field, value):
    case = (
        change_edge(case, **{field: value})
        if target == "edge"
        else replace(case, manifold=replace(case.manifold, **{field: value}))
    )
    with pytest.raises(ValueError):
        api.steady_handoff(case)


@pytest.mark.parametrize(
    "change",
    [
        lambda c: change_tree(c, annulus_rate_m3_s=0.001),
        lambda c: change_tree(c, tree_loss_pa=13e6),
        lambda c: change_edge(c, pressure_loss_pa=13e6),
        lambda c: change_edge(change_tree(c, tree_loss_pa=11e6), pressure_loss_pa=2e6),
        lambda c: change_tree(c, loss_basis=" "),
        lambda c: change_edge(c, loss_basis=""),
        lambda c: replace(c, manifold=replace(c.manifold, capacity_m3_s=0)),
        lambda c: change_tree(c, basis=replace(c.trees[0].basis, phase="gas")),
        lambda c: change_tree(c, basis=replace(c.trees[0].basis, phase="water")),
        lambda c: change_tree(
            c, basis=replace(c.trees[0].basis, reference_condition="other")
        ),
        lambda c: change_tree(
            c, basis=replace(c.trees[0].basis, pressure_datum="other")
        ),
        lambda c: change_tree(
            c, basis=replace(c.trees[0].basis, reference_condition="")
        ),
        lambda c: change_tree(c, basis=replace(c.trees[0].basis, pressure_datum=" ")),
    ],
)
def test_unsupported_or_unqualified_inputs(api, case, change):
    with pytest.raises(ValueError):
        api.steady_handoff(change(case))


def test_delegates_to_existing_leaves(api, case, monkeypatch):
    module = importlib.import_module("digitalmodel.subsea.field.steady_handoff")
    calls = {}
    for name in ("flow_handoff", "aggregate_flow", "manifold_balance"):
        original = getattr(module, name)

        def spy(*args, _name=name, _original=original, **kwargs):
            calls[_name] = calls.get(_name, 0) + 1
            return _original(*args, **kwargs)

        monkeypatch.setattr(module, name, spy)
    api.steady_handoff(case)
    assert calls == {"flow_handoff": 2, "aggregate_flow": 1, "manifold_balance": 1}


@pytest.mark.parametrize(
    "leaf,field",
    [
        ("flow_handoff", "downstream_pressure_pa"),
        ("aggregate_flow", "q_total"),
        ("aggregate_flow", "gor"),
        ("manifold_balance", "p_header"),
        ("manifold_balance", "wells"),
    ],
)
def test_nonfinite_leaf_outputs_rejected(api, case, monkeypatch, leaf, field):
    module = importlib.import_module("digitalmodel.subsea.field.steady_handoff")
    original = getattr(module, leaf)

    def nonfinite(*args, **kwargs):
        result = original(*args, **kwargs)
        value = (
            tuple(replace(w, p_arrival=float("inf")) for w in result.wells)
            if field == "wells"
            else float("inf")
        )
        return replace(result, **{field: value})

    monkeypatch.setattr(module, leaf, nonfinite)
    with pytest.raises(ValueError, match="finite"):
        api.steady_handoff(case)


def test_overflow_in_aggregation_rejected(api, case):
    capacity = BoreCapacity(2, 1.7e308)
    case = replace(
        case,
        trees=tuple(
            replace(
                tree,
                production_rate_m3_s=1e308,
                production_capacity=capacity,
            )
            for tree in case.trees
        ),
    )
    with pytest.raises(ValueError, match="finite"):
        api.steady_handoff(case)


def test_zero_rates_and_exact_boundaries(api, case):
    case = replace(
        case, trees=tuple(replace(tree, production_rate_m3_s=0) for tree in case.trees)
    )
    assert api.steady_handoff(case).accepted_inlet.rate_m3_s == 0
    case = change_tree(case, production_rate_m3_s=0.01, pressure_pa=9e6)
    case = replace(case, manifold=replace(case.manifold, capacity_m3_s=0.01))
    result = api.steady_handoff(case)
    assert result.accepted_inlet.rate_m3_s == 0.01
    assert result.branches[0].pressure.dp_choke == 0


def test_common_water_basis(api, case):
    basis = replace(case.manifold.basis, phase="water")
    case = replace(
        case,
        manifold=replace(case.manifold, basis=basis),
        trees=tuple(replace(tree, basis=basis) for tree in case.trees),
    )
    result = api.steady_handoff(case)
    assert result.aggregate.q_water == pytest.approx(0.003)
    assert result.aggregate.q_oil == result.aggregate.q_gas == 0
    assert result.accepted_inlet.basis.phase == "water"
