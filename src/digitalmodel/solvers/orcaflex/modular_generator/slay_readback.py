# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Solver-free reductions of native S-lay support and material-station results."""

import math
from numbers import Real


def _finite_number(value):
    return (
        isinstance(value, Real) and not isinstance(value, bool) and math.isfinite(value)
    )


def support_readback(vessel, make_extra, count, line_name):
    """Read individual reactions and line-specific contact material stations.

    Orcina, TObjectExtra2, SupportIndex and SupportedLineName:
    https://www.orcina.com/webhelp/OrcFxAPI/Content/html/TObjectExtra2.htm
    The extra factory is injected; this module never imports the native API.
    Force is independent of supported line; contact arc length requires it.
    """
    if isinstance(count, bool) or not isinstance(count, int) or count <= 0:
        raise ValueError("A positive support count is required")
    if not isinstance(line_name, str) or not line_name.strip():
        raise ValueError("A supported line name is required")
    force_name = "Support reaction force"
    contact_name = "Support contact arc length"
    names = vessel.vars(objectExtra=make_extra(SupportIndex=1))
    if force_name not in names or contact_name not in names:
        raise ValueError(
            "Individual Support reaction force and contact arc length required"
        )
    forces, contacts = [], []
    for index in range(1, count + 1):
        forces.append(vessel.StaticResult(force_name, make_extra(SupportIndex=index)))
        contacts.append(
            vessel.StaticResult(
                contact_name,
                make_extra(SupportIndex=index, SupportedLineName=line_name),
            )
        )
    if not all(_finite_number(value) for value in [*forces, *contacts]):
        raise ValueError("Finite support forces and contact stations required")
    return forces, contacts


def _stations(values):
    stations = list(values)
    if (
        len(stations) < 2
        or not all(_finite_number(value) for value in stations)
        or any(right <= left for left, right in zip(stations, stations[1:]))
    ):
        raise ValueError("Finite strictly increasing material stations required")
    return stations


def _graph(graph):
    stations, readings = _stations(graph.X), list(graph.Mean)
    if len(stations) != len(readings) or not all(
        _finite_number(value) for value in readings
    ):
        raise ValueError("Finite readings paired with material stations required")
    return stations, readings


def material_node_indices(node_stations, targets):
    """Map to native nodes; the representable interval midpoint uses lower arc."""
    from bisect import bisect_left

    nodes = _stations(node_stations)
    indices = []
    for target in targets:
        if not _finite_number(target) or not nodes[0] <= target <= nodes[-1]:
            raise ValueError("Material station outside native node bounds")
        right = bisect_left(nodes, target)
        if right == 0 or nodes[right] == target:
            indices.append(right)
        else:
            midpoint = nodes[right - 1] + (nodes[right] - nodes[right - 1]) / 2
            indices.append(right - 1 if target <= midpoint else right)
    return indices


def aligned_static_graphs(x_graph, z_graph, moment_graph, tension_graph):
    """Retain native forces; interpolate positions within identical endpoints.

    Positions must share a node grid and moment/tension must share a force
    grid. Native geometry (slopes and curvature) must still use position nodes,
    not the interpolated positions returned for grounded-force classification.
    """
    from bisect import bisect_left

    nodes, x = _graph(x_graph)
    z_nodes, z = _graph(z_graph)
    arcs, moments = _graph(moment_graph)
    tension_arcs, tensions = _graph(tension_graph)
    if nodes != z_nodes or arcs != tension_arcs:
        raise ValueError(
            "Position pairs and force pairs must use common material stations"
        )
    if (nodes[0], nodes[-1]) != (arcs[0], arcs[-1]):
        raise ValueError("Position and force material stations must share endpoints")
    positions = [[], []]
    for arc in arcs:
        right = bisect_left(nodes, arc)
        for readings, output in zip((x, z), positions):
            if nodes[right] == arc:
                output.append(readings[right])
            else:
                left = right - 1
                fraction = (arc - nodes[left]) / (nodes[right] - nodes[left])
                output.append(
                    readings[left] + fraction * (readings[right] - readings[left])
                )
    return arcs, [*positions, moments, tensions]


def sag_bend_indices(x, z, lift_off, touchdown):
    """Select decreasing departure slope on native position nodes only."""
    slopes = [
        math.atan2(z[i] - z[i + 1], abs(x[i + 1] - x[i])) for i in range(len(x) - 1)
    ]
    return [
        i for i in range(max(1, lift_off + 1), touchdown) if slopes[i] < slopes[i - 1]
    ]


def static_readback(
    x_graph, z_graph, moment_graph, tension_graph, *, contact_station, seabed_z
):
    """Reduce forces on their own stations and geometry on native nodes.

    Force stations inherit the nearest native node's curvature region, with
    lower-station ties. Interpolation only locates grounded force samples;
    force extrema are never interpolated. Mesh qualification remains separate.
    """
    if not _finite_number(seabed_z):
        raise ValueError("Finite seabed position required")
    arcs, (_, force_z, moments, tensions) = aligned_static_graphs(
        x_graph, z_graph, moment_graph, tension_graph
    )
    nodes, x = _graph(x_graph)
    _, z = _graph(z_graph)
    lift = material_node_indices(nodes, [contact_station])[0]
    grounded_nodes = [
        i for i, height in enumerate(z) if i > lift and height <= seabed_z
    ]
    if not grounded_nodes:
        raise ValueError("Grounded pipe readback is required")
    touchdown = grounded_nodes[0]
    if not 0 < lift < touchdown:
        raise ValueError("Interior lift-off before touchdown required")
    mapped_nodes = material_node_indices(nodes, arcs)
    sag = set(sag_bend_indices(x, z, lift, touchdown))
    overbend = [
        abs(value)
        for arc, node, value in zip(arcs, mapped_nodes, moments)
        if arc <= nodes[touchdown] and node not in sag
    ]
    sag_moments = [
        abs(value) for node, value in zip(mapped_nodes, moments) if node in sag
    ]
    grounded = [
        value
        for arc, height, value in zip(arcs, force_z, tensions)
        if arc > nodes[lift] and height <= seabed_z
    ]
    if not overbend or not sag_moments or not grounded:
        raise ValueError("Overbend, sag-bend and grounded readbacks required")
    return {
        "end_a_x": x[0],
        "touchdown_x": x[touchdown],
        "departure_angle": math.degrees(
            math.atan2(z[lift - 1] - z[lift + 1], abs(x[lift + 1] - x[lift - 1]))
        ),
        "overbend_moment": max(overbend),
        "sag_bend_moment": max(sag_moments),
        "grounded_tension": grounded,
    }


def last_contact(roller_arcs, forces, firing_count):
    """Return native-array index and declared arc of the last loaded roller.

    Gaps do not imply early lift-off: later loaded supports remain visible.
    Counts refer to the input support table; native count needs confirmation.
    """
    arcs = _stations(roller_arcs)
    if (
        isinstance(firing_count, bool)
        or not isinstance(firing_count, int)
        or firing_count < 0
        or len(forces) != firing_count + len(arcs)
        or not all(_finite_number(force) for force in forces)
    ):
        raise ValueError("Aligned finite support forces and roller stations required")
    active = [i for i in range(len(arcs)) if forces[firing_count + i] > 0]
    if not active:
        raise ValueError("Stinger contact readback is required")
    last = active[-1]
    return firing_count + last, arcs[last]
