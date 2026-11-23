# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rigid-body aggregation using the parallel-axis identity.

Source: pinned IEA-15-240-RWT tabular component properties, cited in basis.yml.
Tower quadrature integrates piecewise-linear density times a quadratic exactly.
"""
from __future__ import annotations

from typing import Any

import numpy as np


def parallel_axis(mass: float, center: Any) -> np.ndarray:
    """Return the inertia shift m[(r dot r)I - r outer r]."""
    vector = np.asarray(center, dtype=float)
    return mass * (vector @ vector * np.eye(3) - np.outer(vector, vector))


def aggregate(basis: dict, reading: str) -> dict:
    """Aggregate platform, distributed tower and RNA at still water level."""
    system = basis["system"]
    tower = np.asarray(system["tower_table"]["value"])
    nodes, weights = np.polynomial.legendre.leggauss(3)
    tower_integrals = np.zeros(4)
    for left, right in zip(tower[:-1], tower[1:]):
        z = (right[0] + left[0]) / 2 + (right[0] - left[0]) / 2 * nodes
        density = np.interp(z, [left[0], right[0]], [left[3], right[3]])
        section = np.interp(z, [left[0], right[0]], [left[4], right[4]])
        weight = weights * (right[0] - left[0]) / 2
        tower_integrals += np.array([
            weight @ density, weight @ (density * z),
            weight @ (density * z**2 + section), weight @ (2 * section),
        ])
    tower_mass, tower_first, transverse, polar = tower_integrals
    platform_mass = system["platform_mass"]["value"]
    platform_cg = np.asarray(system["platform_center_of_gravity"]["value"])
    rna_mass = system["rna_mass"]["value"]
    rna_cg = np.asarray(system["rna_center_of_gravity"]["value"])
    rna_inertia = np.asarray(system["rna_inertia_cg"]["value"])
    # Retain the same published F:K endpoint for both origin interpretations.
    # The other published tensor remains an independent identity comparator.
    relative = rna_cg - np.array([0, 0, system["tower_top"]["value"]])
    readings = {"at_cg": rna_inertia,
                "at_tower_top": rna_inertia - parallel_axis(rna_mass, relative)}
    rna_at_cg = readings[reading]
    inertia = (np.asarray(system["platform_inertia_cg"]["value"])
               + parallel_axis(platform_mass, platform_cg)
               + np.diag([transverse, transverse, polar])
               + rna_at_cg + parallel_axis(rna_mass, rna_cg))
    mass = platform_mass + tower_mass + rna_mass
    cg = (platform_mass * platform_cg + rna_mass * rna_cg
          + np.array([0, 0, tower_first])) / mass
    return {"mass": float(mass), "cg": cg, "inertia_swl": inertia,
            "inertia_cg": inertia - parallel_axis(mass, cg)}
