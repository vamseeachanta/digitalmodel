# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Integrate static cable equilibrium without production solver dependencies.

Horizontal force H is constant; dV/ds = w. Consequently dx/dV =
H/(w*hypot(H,V)) and dz/dV = V/(w*hypot(H,V)). Simpson integration
supplies geometry independently of the solvers' hyperbolic shape formulas.
The comparison is inextensible, uniform-weight and planar, with zero vertical
reaction at touchdown. It does not model bending stiffness or dynamic response.
"""

import math


def _integrate(function, upper, panels):
    """Integrate from zero to upper with composite Simpson quadrature."""
    if panels <= 0 or panels % 2:
        raise ValueError("Quadrature requires a positive even panel count")
    step = upper / panels
    interior = math.fsum((4 if index % 2 else 2) * function(index * step)
                         for index in range(1, panels))
    return step * (function(0.0) + interior + function(upper)) / 3.0


def _segment(horizontal, weight, height, panels):
    """Integrate a vertex-to-end segment of prescribed height and weight."""
    magnitude = abs(weight)
    # Work/force identity: end tension minus vertex tension = |w|*height.
    end_vertical = math.sqrt((horizontal + magnitude * height)**2 - horizontal**2)
    length = end_vertical / magnitude
    offset = _integrate(lambda v: horizontal / (magnitude * math.hypot(horizontal, v)),
                        end_vertical, panels)
    drop = _integrate(lambda v: v / (magnitude * math.hypot(horizontal, v)),
                      end_vertical, panels)
    return {"length": length, "offset": offset, "drop": drop, "weight": weight}


def _horizontal(drop, angle, weight):
    """Resolve constant horizontal force from endpoint direction and work."""
    if drop <= 0 or weight <= 0 or not 0 < angle < 90:
        raise ValueError("Positive drop/weight and an angle between zero and ninety are required")
    sine = math.sin(math.radians(angle))
    return weight * drop * sine / (1 - sine)


def _summarise(horizontal, segments, signs):
    """Sum geometry and signed distributed weight over the entire cable."""
    vertical = segments[0]["weight"] * segments[0]["length"]
    return {
        "horizontal": horizontal, "vertical": vertical,
        "tension": math.hypot(horizontal, vertical),
        "angle": math.degrees(math.atan2(horizontal, vertical)),
        "global_weight": math.fsum(segment["weight"] * segment["length"] for segment in segments),
        "length": math.fsum(segment["length"] for segment in segments),
        "offset": math.fsum(segment["offset"] for segment in segments),
        "drop": math.fsum(sign * segment["drop"] for sign, segment in zip(signs, segments, strict=True)),
        "segments": segments,
    }


def scr(drop, angle, weight, panels=4096):
    """Check an inextensible SCR with a horizontal touchdown tangent."""
    horizontal = _horizontal(drop, angle, weight)
    return _summarise(horizontal, [_segment(horizontal, weight, drop, panels)], [1])


def slwr(drop, angle, weight, buoyed_weight, sag, hog, panels=4096):
    """Check a five-segment SLWR with continuous tension at weight changes."""
    if buoyed_weight >= 0 or not 0 < sag < hog < drop:
        raise ValueError("A net-buoyant section and ordered sag/hog elevations are required")
    horizontal = _horizontal(drop - sag, angle, weight)
    uplift = -buoyed_weight
    # Equal vertical tension at a bare/buoyed interface requires
    # |w|*height to agree on its two vertex-to-interface segments.
    rising_bare = (hog - sag) * uplift / (weight + uplift)
    falling_bare = hog * uplift / (weight + uplift)
    heights = [drop - sag, rising_bare, hog - sag - rising_bare,
               hog - falling_bare, falling_bare]
    weights = [weight, weight, buoyed_weight, buoyed_weight, weight]
    segments = [_segment(horizontal, w, h, panels)
                for w, h in zip(weights, heights, strict=True)]
    return _summarise(horizontal, segments, [1, -1, -1, 1, 1])
