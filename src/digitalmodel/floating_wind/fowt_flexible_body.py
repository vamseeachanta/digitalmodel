"""Rigid-equivalent body and tower deflection of a flexible fowt spec.

Split from ``fowt_static_checks``, which still provides every name here.
SI units in the results (kg, m, kg m2, rad).
"""

from __future__ import annotations

import numpy as np

from digitalmodel.floating_wind.fowt_static_checks import KILO, rotation


def combine_mass_properties(parts: list[tuple[float, np.ndarray, np.ndarray]]
                            ) -> tuple[float, np.ndarray, np.ndarray]:
    """Mass, centre and inertia about that centre of (mass, centre, inertia) parts."""
    mass = sum(m for m, _, _ in parts)
    centre = sum(m * np.asarray(c, dtype=float) for m, c, _ in parts) / mass
    inertia = np.zeros((3, 3))
    for m, c, i in parts:
        d = np.asarray(c, dtype=float) - centre
        inertia += np.asarray(i, dtype=float) + m * (d @ d * np.eye(3) - np.outer(d, d))
    return mass, centre, inertia


def flexible_mass_properties(fowt, principal_rna_inertia: bool = False
                             ) -> tuple[float, np.ndarray, np.ndarray]:
    """Rigid-equivalent mass (kg), centre (m) and inertia (kg m2) of hull, tower and RNA.

    Tower sections are thin-walled tubes on the hull z axis. With
    ``principal_rna_inertia`` the RNA products of inertia are dropped, to
    measure their effect; the generated model keeps them (the RNA buoy carries
    mass and centre, and a principal-axes child buoy carries the moments).
    """
    hull = fowt.hull
    parts = [(hull.mass * KILO, hull.centre_of_mass,
              np.array(hull.inertia_about_centre_of_mass) * KILO)]
    z = fowt.tower.base_elevation
    for section in fowt.tower.sections:
        m = section.mass_per_length * KILO * section.length
        radial = m * section.outer_diameter ** 2 / 8.0
        axial = m * section.length ** 2 / 12.0
        parts.append((m, [0.0, 0.0, z + section.length / 2.0],
                      np.diag([radial + axial, radial + axial, 2.0 * radial])))
        z += section.length
    rna = fowt.rna
    inertia = np.array(rna.inertia_about_centre_of_mass) * KILO
    if principal_rna_inertia:
        inertia = np.diag(np.diag(inertia))
    parts.append((rna.mass * KILO, np.array(rna.centre_of_mass_from_tower_top) + [0.0, 0.0, z],
                  inertia))
    return combine_mass_properties(parts)


def tower_top_deflection(fowt, hull_rotation: tuple[float, float, float] = (0.0, 0.0, 0.0),
                         steps_per_section: int = 50) -> dict:
    """First-order static deflection of the flexible tower in hull axes.

    The tower is a cantilever fixed to the hull at ``base_elevation``. Its own
    weight and the RNA weight, resolved in hull axes for the given hull
    rotation (rad), load it laterally; the RNA weight also acts at its offset
    from the tower top. Euler-Bernoulli bending, linear, P-delta excluded.
    Returns the tower-top displacement (m) and rotation (rad) in hull x and y.
    """
    g_hull = rotation(*hull_rotation).T @ np.array([0.0, 0.0, -fowt.gravity])
    base = fowt.tower.base_elevation
    z_nodes, ei_nodes, w_nodes = [base], [], []
    for section in fowt.tower.sections:
        dz = section.length / steps_per_section
        for _ in range(steps_per_section):
            z_nodes.append(z_nodes[-1] + dz)
            ei_nodes.append(section.bending_stiffness * KILO)
            w_nodes.append(section.mass_per_length * KILO * dz)
    z = np.array(z_nodes)
    mid = (z[:-1] + z[1:]) / 2.0
    top = z[-1]
    rna = fowt.rna
    rna_force = rna.mass * KILO * g_hull
    rna_point = np.array(rna.centre_of_mass_from_tower_top) + [0.0, 0.0, top]
    result = {}
    for axis, (lateral, sign) in {"x": (0, 1.0), "y": (1, -1.0)}.items():
        # Bending moment about the axis normal to the deflection plane, at each
        # element mid-point, from all loads above it (positive bends toward +lateral).
        moments = []
        for zm in mid:
            above = mid > zm
            m = np.sum(np.array(w_nodes)[above] * g_hull[lateral] * (mid[above] - zm))
            m += rna_force[lateral] * (rna_point[2] - zm)
            m -= rna_force[2] * (rna_point[lateral] - 0.0)
            moments.append(m)
        curvature = np.array(moments) / np.array(ei_nodes)
        dz = np.diff(z)
        slope = np.concatenate([[0.0], np.cumsum(curvature * dz)])
        deflection = np.concatenate([[0.0], np.cumsum((slope[:-1] + slope[1:]) / 2.0 * dz)])
        result[f"top_displacement_{axis}"] = float(deflection[-1])
        result[f"top_rotation_about_{'y' if axis == 'x' else 'x'}"] = float(sign * slope[-1])
    return result
