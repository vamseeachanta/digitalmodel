# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Solver-neutral line geometry; no elements are solved or qualified.

Straight interpolation follows the two-node linear shape functions derived in
the calculation basis F1. Catenary coordinates follow Catenary Curve (2019),
equations (9)-(10), with a vertex-height translation. Geometry only is returned.
"""

import math
from dataclasses import dataclass
from typing import Sequence

from ._validation import element_count
from .jumper_catenary_config import CatenaryConfig

Point = tuple[float, float, float]


@dataclass(frozen=True)
class JumperMesh:
    """Nodes in metres, one-based consecutive connectivity, and chord lengths.

    Materials, element formulations, loads, constraints, bend radii and mesh
    convergence are not defined by this geometry container.
    """

    nodes_m: tuple[Point, ...]
    elements: tuple[tuple[int, int], ...]
    chord_lengths_m: tuple[float, ...]


def _mesh(nodes: Sequence[Point]) -> JumperMesh:
    if not all(math.isfinite(v) for p in nodes for v in p):
        raise ValueError("mesh coordinates must be finite")
    chords = tuple(math.dist(p, q) for p, q in zip(nodes, nodes[1:]))
    if not all(math.isfinite(v) and v > 0 for v in chords):
        raise ValueError("mesh contains nonfinite or zero-length elements")
    return JumperMesh(
        tuple(nodes), tuple((i, i + 1) for i in range(1, len(nodes))), chords
    )


def mesh_polyline(
    vertices_m: Sequence[Point], elements_per_segment: Sequence[int]
) -> JumperMesh:
    """Subdivide straight 3D segments, retaining each shared corner once.

    Sharp corners remain sharp: this is not a swept bend or a fabrication model.
    """
    if len(vertices_m) < 2 or len(elements_per_segment) != len(vertices_m) - 1:
        raise ValueError("provide at least two vertices and a count per segment")
    if any(len(p) != 3 or not all(math.isfinite(v) for v in p) for p in vertices_m):
        raise ValueError("vertices must have three finite coordinates")
    nodes = [tuple(float(v) for v in vertices_m[0])]
    for p, q, count in zip(vertices_m, vertices_m[1:], elements_per_segment):
        element_count(count)
        # F1: p(t)=(1-t)*p+t*q, t=i/count; no empirical coefficients.
        nodes.extend(
            tuple((1 - i / count) * v + (i / count) * w for v, w in zip(p, q))
            for i in range(1, count + 1)
        )
    return _mesh(nodes)


def mesh_catenary(config: CatenaryConfig, n_elements: int) -> JumperMesh:
    """Mesh the suspended arc only at equal arc-length intervals, in the x-z plane.

    The grounded straight portion is excluded. Chords are shorter than their
    reference arcs; chord_lengths_m shall not be used as unstretched arc lengths.
    """
    element_count(n_elements)
    if config.suspended_length_m <= 0 or config.parameter_m <= 0:
        raise ValueError("a nonzero suspended catenary is required")
    a = config.parameter_m
    nodes = []
    for i in range(n_elements + 1):
        s = (i / n_elements) * config.suspended_length_m
        # Catenary Curve (2019), (9)-(10): x=a*asinh(s/a), z=hypot(a,s)-a.
        # Rationalized z avoids subtractive cancellation at small s/a.
        x = config.grounded_length_m + a * math.asinh(s / a)
        z = s * (s / math.hypot(a, s)) / (1 + a / math.hypot(a, s))
        nodes.append((x, 0.0, z))
    return _mesh(nodes)
