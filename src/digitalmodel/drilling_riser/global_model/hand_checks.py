"""Independent hand checks for the W2 qualification gates.

* submerged weight and the effective-tension chain of a vertical string;
* natural periods of a tensioned Euler-Bernoulli beam (finite-element solution of
  ``EI w'''' - (Te w')' + m w_tt = 0`` with Hermite beam elements, consistent mass and
  the element-average effective tension as geometric stiffness);
* a WKB string estimate ``T_n = 2 / n * integral dz / sqrt(Te / m)``.

These use only the spec data, never the OrcaFlex solver, so they can serve as the
reference side of a model-versus-hand comparison. SI units throughout.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Iterable, Sequence

import numpy as np
from scipy.linalg import eigh

from .spec import LineSection, RiserGlobalModelSpec

G = 9.80665


def _bore_area(section: LineSection) -> float:
    return math.pi / 4 * section.bore_id_m**2


def submerged_length_m(top_z_m: float, length_m: float) -> float:
    """Length of a vertical section below MSL (z = 0), given its top elevation."""
    bottom = top_z_m - length_m
    return max(0.0, min(length_m, -bottom if top_z_m > 0 else length_m))


def submerged_weight_n(section: LineSection, *, top_z_m: float, rho_water: float,
                       rho_contents: float) -> float:
    """Weight in water of a vertical section: structure + contents - buoyancy of the wet part."""
    wet = submerged_length_m(top_z_m, section.length_m)
    mass = section.mass_per_m_kg + rho_contents * _bore_area(section)
    return (mass * section.length_m - rho_water * section.displaced_volume_per_m_m3 * wet) * G


def effective_tension_chain(sections: Sequence[LineSection], *, top_z_m: float, top_tension_n: float,
                            rho_water: float, rho_contents: float) -> list[dict]:
    """Effective tension down a vertical string, sections listed top to bottom."""
    out, t, z = [], top_tension_n, top_z_m
    for s in sections:
        w = submerged_weight_n(s, top_z_m=z, rho_water=rho_water, rho_contents=rho_contents)
        out.append({"name": s.name, "top_z_m": z, "bottom_z_m": z - s.length_m,
                    "te_top_n": t, "te_bottom_n": t - w, "submerged_weight_n": w})
        t -= w
        z -= s.length_m
    return out


def ring_weight_n(spec: RiserGlobalModelSpec) -> float:
    ring = spec.tension_ring
    wet = 1.0 if ring.z_static_m < 0 else 0.0
    return (ring.mass_kg - wet * spec.environment.water_density_kg_m3 * ring.volume_m3) * G


def tension_references(spec: RiserGlobalModelSpec) -> dict:
    """Hand references for the tension and weight gates (G1, G2)."""
    rho_w, rho_c = spec.environment.water_density_kg_m3, spec.contents.density_kg_m3
    top = spec.tensioners.total_vertical_tension_n - ring_weight_n(spec)
    z0 = spec.tension_ring.z_static_m
    riser = effective_tension_chain(spec.riser, top_z_m=z0, top_tension_n=top,
                                    rho_water=rho_w, rho_contents=rho_c)
    z_lfj = riser[-1]["bottom_z_m"]
    # The stack carries no main-bore contents in the model convention (its fluid is in the mass).
    stack = effective_tension_chain(spec.stack, top_z_m=z_lfj, top_tension_n=riser[-1]["te_bottom_n"],
                                    rho_water=rho_w, rho_contents=rho_c)
    return {
        "riser_top_n": top,
        "riser_bottom_n": riser[-1]["te_bottom_n"],
        "stack_bottom_n": stack[-1]["te_bottom_n"],
        "submerged_weight_n": top - stack[-1]["te_bottom_n"],
        "ring_weight_n": ring_weight_n(spec),
        "riser_chain": riser,
        "stack_chain": stack,
    }


@dataclass(frozen=True)
class BeamSegment:
    """One beam element: length, bending stiffness, end effective tensions and mass per metre
    (structure + contents + added mass)."""

    length_m: float
    ei_nm2: float
    te_top_n: float
    te_bottom_n: float
    mass_per_m_kg: float


def tensioned_beam_periods(segments: Sequence[BeamSegment], n_modes: int, *,
                           top_rot_stiffness_nm_per_rad: float = 0.0,
                           bottom_rot_stiffness_nm_per_rad: float = 0.0,
                           point_masses: dict[int, float] | None = None,
                           point_springs: dict[int, float] | None = None) -> list[float]:
    """Transverse natural periods (s), ascending, of a beam pinned in translation at both ends.

    Nodes are numbered 0 (top) to len(segments) (bottom); ``point_masses`` and
    ``point_springs`` are keyed by node number (kg, N/m, translational).
    """
    n_el = len(segments)
    if n_el < 2:
        raise ValueError("need at least two elements")
    ndof = 2 * (n_el + 1)
    K = np.zeros((ndof, ndof))
    M = np.zeros((ndof, ndof))
    for e, s in enumerate(segments):
        L = s.length_m
        te = 0.5 * (s.te_top_n + s.te_bottom_n)
        kb = s.ei_nm2 / L**3 * np.array([[12, 6 * L, -12, 6 * L], [6 * L, 4 * L**2, -6 * L, 2 * L**2],
                                         [-12, -6 * L, 12, -6 * L], [6 * L, 2 * L**2, -6 * L, 4 * L**2]])
        kg = te / (30 * L) * np.array([[36, 3 * L, -36, 3 * L], [3 * L, 4 * L**2, -3 * L, -L**2],
                                       [-36, -3 * L, 36, -3 * L], [3 * L, -L**2, -3 * L, 4 * L**2]])
        me = s.mass_per_m_kg * L / 420 * np.array([[156, 22 * L, 54, -13 * L], [22 * L, 4 * L**2, 13 * L, -3 * L**2],
                                                   [54, 13 * L, 156, -22 * L], [-13 * L, -3 * L**2, -22 * L, 4 * L**2]])
        idx = slice(2 * e, 2 * e + 4)
        K[idx, idx] += kb + kg
        M[idx, idx] += me
    K[1, 1] += top_rot_stiffness_nm_per_rad
    K[ndof - 1, ndof - 1] += bottom_rot_stiffness_nm_per_rad
    for node, mass in (point_masses or {}).items():
        M[2 * node, 2 * node] += mass
    for node, k in (point_springs or {}).items():
        K[2 * node, 2 * node] += k
    free = [i for i in range(ndof) if i not in (0, ndof - 2)]
    Kf, Mf = K[np.ix_(free, free)], M[np.ix_(free, free)]
    # tiny rotational inertia keeps M positive definite where massless elements meet
    Mf = Mf + np.eye(len(free)) * 1e-9 * max(1.0, float(np.max(np.abs(Mf))))
    n = min(n_modes, len(free))
    w2 = eigh(Kf, Mf, eigvals_only=True, subset_by_index=[0, n - 1])
    if np.any(w2 <= 0):
        raise ValueError("non-positive eigenvalue: the beam is unstable (compression?)")
    return [2 * math.pi / math.sqrt(v) for v in w2]


def wkb_string_periods(segments: Iterable[BeamSegment], n_modes: int) -> list[float]:
    """WKB estimate for a string with slowly varying tension and mass (information only)."""
    travel = 0.0
    for s in segments:
        if s.te_top_n <= 0 or s.te_bottom_n <= 0:
            raise ValueError("WKB string estimate needs positive tension throughout")
        te = 0.5 * (s.te_top_n + s.te_bottom_n)
        travel += s.length_m / math.sqrt(te / s.mass_per_m_kg)
    return [2 * travel / n for n in range(1, n_modes + 1)]


def _split(section: LineSection, max_len: float) -> int:
    return max(1, math.ceil(section.length_m / max_len - 1e-9))


def tensioner_lateral_stiffness_n_m(spec: RiserGlobalModelSpec) -> float:
    """In-plane (x) geometric stiffness of constant-tension tensioner lines at the ring."""
    t = spec.tensioners
    dx = t.sheave_radius_m - t.ring_attach_radius_m
    dz = t.sheave_z_m - spec.tension_ring.z_static_m
    length = math.hypot(dx, dz)
    tension = t.total_vertical_tension_n / (t.count * dz / length)
    k = 0.0
    for i in range(t.count):
        az = math.radians(t.first_azimuth_deg + 360.0 * i / t.count)
        ux = dx * math.cos(az) / length
        k += tension / length * (1.0 - ux * ux)
    return k


def beam_model(spec: RiserGlobalModelSpec, *, max_element_m: float = 1.0):
    """Beam segments from the upper to the lower flex-joint pivot, with the ring node."""
    rho_w, rho_c = spec.environment.water_density_kg_m3, spec.contents.density_kg_m3
    segs: list[BeamSegment] = []

    def mass_per_m(s: LineSection, top_z: float, le: float) -> float:
        wet = submerged_length_m(top_z, le) / le
        return s.mass_per_m_kg + rho_c * _bore_area(s) + s.ca_normal * rho_w * s.displaced_volume_per_m_m3 * wet

    # Inner barrel hangs from the upper flex joint; the slip section carries ~0.
    ib = spec.inner_barrel
    ib_w = [(s.mass_per_m_kg + rho_c * _bore_area(s)) * G * s.length_m for s in ib]
    below = [sum(ib_w[i + 1:]) if s.ea_n > 1e6 else 0.0 for i, s in enumerate(ib)]
    z = spec.upper_flex_joint.pivot_z_m
    for s, w_sec, w_below in zip(ib, ib_w, below):
        n = _split(s, max_element_m)
        le = s.length_m / n
        carries = s.ea_n > 1e6  # the slip section carries no axial load
        for k in range(n):
            t_top = (w_below + w_sec * (n - k) / n) if carries else 0.0
            t_bot = (w_below + w_sec * (n - k - 1) / n) if carries else 0.0
            segs.append(BeamSegment(le, s.ei_nm2, t_top, t_bot, mass_per_m(s, z, le)))
            z -= le
    ring_node = len(segs)
    ref = tension_references(spec)
    z = spec.tension_ring.z_static_m
    for s, link in zip(spec.riser, ref["riser_chain"]):
        n = _split(s, max_element_m)
        le = s.length_m / n
        dt = (link["te_top_n"] - link["te_bottom_n"]) / n
        for k in range(n):
            segs.append(BeamSegment(le, s.ei_nm2, link["te_top_n"] - k * dt,
                                    link["te_top_n"] - (k + 1) * dt, mass_per_m(s, z, le)))
            z -= le
    return segs, ring_node


def reference_periods(spec: RiserGlobalModelSpec, n_modes: int = 5, *, max_element_m: float = 1.0) -> list[float]:
    """G3 reference: tensioned-beam periods between the flex-joint pivots (stack rigid)."""
    segs, ring_node = beam_model(spec, max_element_m=max_element_m)
    return tensioned_beam_periods(
        segs, n_modes,
        top_rot_stiffness_nm_per_rad=spec.upper_flex_joint.rotational_stiffness_nm_per_rad,
        bottom_rot_stiffness_nm_per_rad=spec.lower_flex_joint.rotational_stiffness_nm_per_rad,
        point_masses={ring_node: spec.tension_ring.mass_kg},
        point_springs={ring_node: tensioner_lateral_stiffness_n_m(spec)},
    )


def wkb_reference_periods(spec: RiserGlobalModelSpec, n_modes: int = 5) -> list[float]:
    """WKB string estimate over the tensioned riser (ring to lower flex joint), information only."""
    segs, ring_node = beam_model(spec)
    return wkb_string_periods(segs[ring_node:], n_modes)
