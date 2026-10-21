# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Quasi-static surface pull-in for straight, continuously supported segments.

The sliding resistance is an equivalent axial friction factor times normal
load. See Yan, White and Randolph (2014), *Cyclic consolidation and axial
friction for seabed pipelines*, DOI 10.1680/geolett.14.00032, Results.
Resolving net weight along a straight incline gives the gravity contribution.
This does not model HDD, bends, breakout, hydrodynamic drag or bending stress.
"""

from dataclasses import dataclass
import math


@dataclass(frozen=True)
class RouteSegment:
    """Describe an ordered segment, from pipe tail towards the shore winch.

    Args:
        length_m: Actual supported arc length, not horizontal projection.
        weight_kn_m: Submerged weight without the additional buoyancy aids.
        friction: Equivalent axial friction factor for the support surface.
        rise_m: Elevation increase towards the winch; negative is downhill.
        buoyancy_kn_m: Distributed net upward force from additional aids.
    """

    length_m: float
    weight_kn_m: float
    friction: float
    rise_m: float = 0.0
    buoyancy_kn_m: float = 0.0


def pull_in(
    segments: list[RouteSegment],
    *,
    tail_tension_kn: float = 0.0,
    efficiency: float = 1.0,
    capacity_kn: float | None = None,
) -> dict:
    """Resolve supported-pipe tension and winch demand without a solver.

    Args:
        segments: Nonempty sequence in tail-to-head order.
        tail_tension_kn: Nonnegative prescribed trailing tension.
        efficiency: Mechanical efficiency of the pull system, in (0, 1].
        capacity_kn: Optional winch capacity on the same demand basis.

    Returns:
        Force components, segment endpoint tensions and capacity comparison.

    Raises:
        ValueError: Inputs are nonfinite, contact is lost, or compression occurs.
    """
    if (not segments or not math.isfinite(tail_tension_kn) or tail_tension_kn < 0
            or not math.isfinite(efficiency) or not 0 < efficiency <= 1
            or (capacity_kn is not None and
                (not math.isfinite(capacity_kn) or capacity_kn <= 0))):
        raise ValueError("Finite route, tail tension and winch inputs are required")
    tension = tail_tension_kn
    friction_total = gravity_total = 0.0
    endpoints = []
    normals = []
    for segment in segments:
        values = (segment.length_m, segment.weight_kn_m, segment.friction,
                  segment.rise_m, segment.buoyancy_kn_m)
        if (not all(math.isfinite(v) for v in values) or segment.length_m <= 0
                or segment.friction < 0 or segment.buoyancy_kn_m < 0
                or abs(segment.rise_m) >= segment.length_m
                or segment.weight_kn_m <= segment.buoyancy_kn_m):
            raise ValueError("Finite supported segments with positive net weight are required")
        net_weight = segment.weight_kn_m - segment.buoyancy_kn_m
        sine = segment.rise_m / segment.length_m
        normal = net_weight * segment.length_m * math.sqrt(1 - sine**2)
        friction_force = segment.friction * normal
        gravity_force = net_weight * segment.rise_m
        tension += friction_force + gravity_force
        if tension < 0:
            raise ValueError("Route requires compression; tension-only method cannot represent it")
        friction_total += friction_force
        gravity_total += gravity_force
        endpoints.append(tension)
        normals.append(normal)
    demand = tension / efficiency
    utilisation = None if capacity_kn is None else demand / capacity_kn
    status = ("not_evaluated" if utilisation is None else
              "within_declared_capacity" if utilisation <= 1 else "exceeded")
    return {"pipe_head_tension_kn": tension, "peak_pipe_tension_kn": max([tail_tension_kn, *endpoints]),
            "segment_end_tensions_kn": endpoints, "segment_normal_loads_kn": normals,
            "friction_kn": friction_total, "gravity_kn": gravity_total,
            "winch_demand_kn": demand, "capacity_utilisation": utilisation,
            "capacity_status": status}
