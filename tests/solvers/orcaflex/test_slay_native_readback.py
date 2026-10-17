# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Solver-free regressions for the native S-lay readback boundary."""

from types import SimpleNamespace

import pytest

from digitalmodel.solvers.orcaflex.modular_generator import slay_readback


@pytest.mark.parametrize("count", [25, 31])
def test_individual_support_readback_uses_one_based_indices_and_line_contact(count):
    """A double verifies calls, without claiming native API qualification."""
    calls = []

    class Vessel:
        def vars(self, *, objectExtra):
            assert objectExtra.SupportIndex == 1
            return [
                "Supports force",
                "Connections + supports force",
                "Supports Lx force",
                "Support reaction x force",
                "Support reaction force",
                "Support contact arc length",
            ]

        def StaticResult(self, name, extra):
            assert 1 <= extra.SupportIndex <= count
            calls.append((name, extra))
            if name == "Support reaction force":
                assert not hasattr(extra, "SupportedLineName")
                return extra.SupportIndex * 2.0
            assert name == "Support contact arc length"
            assert extra.SupportedLineName == "pipe"
            return extra.SupportIndex * 5.0

    forces, contacts = slay_readback.support_readback(
        Vessel(), SimpleNamespace, count, "pipe"
    )
    assert forces == [i * 2.0 for i in range(1, count + 1)]
    assert contacts == [i * 5.0 for i in range(1, count + 1)]
    assert len(calls) == 2 * count


@pytest.mark.parametrize(
    "aggregate",
    [
        "Supports force",
        "Supports Lx force",
        "Supports Ly force",
        "Supports Lz force",
        "Connections + supports force",
        "Connections + supports Lx force",
        "Connections + supports Ly force",
        "Connections + supports Lz force",
    ],
)
def test_aggregate_force_cannot_substitute_for_individual_reaction(aggregate):
    vessel = SimpleNamespace(vars=lambda **_: [aggregate, "Support contact arc length"])
    with pytest.raises(ValueError, match="Individual Support reaction force"):
        slay_readback.support_readback(vessel, SimpleNamespace, 25, "pipe")


def graph(stations, readings):
    return SimpleNamespace(X=stations, Mean=readings)


def test_staggered_grids_map_positions_and_preserve_native_force_extrema():
    nodes = [0, 5, 10]
    forces = [0, 2.5, 7.5, 10]
    moments = [1, 99, 3, 4]
    tensions = [10, 2, -99, 20]
    arcs, values = slay_readback.aligned_static_graphs(
        graph(nodes, [0, -5, -10]),
        graph(nodes, [0, -2, -4]),
        graph(forces, moments),
        graph(forces, tensions),
    )
    assert arcs == forces
    assert values == [[0, -2.5, -7.5, -10], [0, -1, -3, -4], moments, tensions]
    mapped = slay_readback.material_node_indices(nodes, arcs)
    assert mapped == [0, 0, 1, 2]  # Midpoint ties choose lower material station.
    assert max(values[2]) == 99
    assert min(values[3]) == -99
    assert max(value for i, value in enumerate(values[2]) if mapped[i] == 0) == 99


@pytest.mark.parametrize(
    "nodes,readings",
    [
        ([], []),
        ([0], [1]),
        ([0, 0, 10], [1, 2, 3]),
        ([0, 10, 5], [1, 2, 3]),
        ([0, float("nan"), 10], [1, 2, 3]),
        ([0, float("inf")], [1, 2]),
        ([0, 10], [1]),
        ([0, 10], [1, float("nan")]),
        ([0, 10], [1, float("inf")]),
    ],
)
def test_invalid_native_grids_are_rejected(nodes, readings):
    with pytest.raises(ValueError, match="stations|readings"):
        slay_readback.aligned_static_graphs(
            graph(nodes, readings),
            graph([0, 10], [0, 0]),
            graph([0, 10], [1, 1]),
            graph([0, 10], [1, 1]),
        )


@pytest.mark.parametrize("target", [-0.001, 10.001, float("nan"), float("inf")])
def test_contact_station_mapping_never_clamps_or_extrapolates(target):
    with pytest.raises(ValueError, match="station"):
        slay_readback.material_node_indices([0, 5, 10], [target])


@pytest.mark.parametrize(
    "changed,stations",
    [(1, [0, 4, 10]), (3, [0, 4, 10]), (2, [-0.001, 5, 10]), (2, [0, 5, 10.001])],
)
def test_incompatible_native_graphs_are_rejected(changed, stations):
    graphs = [graph([0, 5, 10], [1, 2, 3]) for _ in range(4)]
    graphs[changed] = graph(stations, [1, 2, 3])
    with pytest.raises(ValueError, match="stations|endpoints"):
        slay_readback.aligned_static_graphs(*graphs)


def test_reduction_keeps_native_node_departure_and_half_step_extrema():
    """Geometry uses native nodes even when forces have staggered stations."""
    nodes = [0, 5, 10, 15, 20, 25]
    force_arcs = [0, 2.5, 7.5, 12.5, 17.5, 22.5, 25]
    result = slay_readback.static_readback(
        graph(nodes, [0, -5, -10, -15, -20, -25]),
        graph(nodes, [0, -1, -3, -6, -7, -7.5]),
        graph(force_arcs, [1, 2, -100, 3, 80, 4, 5]),
        graph(force_arcs, [10, 20, 30, 40, 50, -99, 60]),
        contact_station=5,
        seabed_z=-7,
    )
    import math

    assert result["departure_angle"] == pytest.approx(math.degrees(math.atan2(3, 10)))
    assert result["overbend_moment"] == 100
    assert result["sag_bend_moment"] == 80
    assert result["grounded_tension"] == [-99, 60]
    assert result["touchdown_x"] == -20
    assert result["end_a_x"] == 0


@pytest.mark.parametrize("nodes", [[0, "5", 10], [False, 10]])
def test_non_numeric_or_boolean_material_stations_are_rejected(nodes):
    with pytest.raises(ValueError, match="stations"):
        slay_readback.material_node_indices(nodes, [5])


@pytest.mark.parametrize("target", [None, "5", True])
def test_non_numeric_or_boolean_mapping_targets_are_rejected(target):
    with pytest.raises(ValueError, match="station"):
        slay_readback.material_node_indices([0, 5, 10], [target])


@pytest.mark.parametrize(
    "count,line_name",
    [(0, "pipe"), (-1, "pipe"), (True, "pipe"), (1.5, "pipe"), (25, ""), (25, None)],
)
def test_support_selection_requires_count_and_supported_line(count, line_name):
    with pytest.raises(ValueError, match="support"):
        slay_readback.support_readback(None, SimpleNamespace, count, line_name)


def test_decimal_midpoint_uses_lower_material_station_without_epsilon():
    left, right = 0.1, 0.2
    midpoint = left + (right - left) / 2
    assert slay_readback.material_node_indices([left, right], [midpoint]) == [0]


def test_matching_force_grids_with_different_endpoints_are_rejected():
    with pytest.raises(ValueError, match="must share endpoints"):
        slay_readback.aligned_static_graphs(
            graph([0, 5, 10], [1, 2, 3]),
            graph([0, 5, 10], [1, 2, 3]),
            graph([0, 5, 11], [1, 2, 3]),
            graph([0, 5, 11], [1, 2, 3]),
        )


@pytest.mark.parametrize("graph_index", range(4))
def test_each_graph_rejects_nonfinite_readings(graph_index):
    graphs = [graph([0, 5, 10], [1, 2, 3]) for _ in range(4)]
    graphs[graph_index] = graph([0, 5, 10], [1, float("nan"), 3])
    with pytest.raises(ValueError, match="Finite readings"):
        slay_readback.aligned_static_graphs(*graphs)


def test_last_contact_retains_support_order_even_with_a_contact_gap():
    assert slay_readback.last_contact([0, 5, 10], [0, 1, 0, 2], 1) == (3, 10)


@pytest.mark.parametrize(
    "arcs,forces",
    [
        ([0, 5, 5], [0, 1, 0, 2]),
        ([0, 10, 5], [0, 1, 0, 2]),
        ([0, 5, 10], [0, 1, 2]),
        ([0, 5, 10], [0, 0, 0, 0]),
    ],
)
def test_last_contact_rejects_invalid_order_count_or_missing_contact(arcs, forces):
    with pytest.raises(ValueError, match="stations|support|contact"):
        slay_readback.last_contact(arcs, forces, 1)
