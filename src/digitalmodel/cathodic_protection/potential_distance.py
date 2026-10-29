# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Screening electrolyte potential fields from one prescribed current source.

First-principles basis: Ohm's law E = rho J and current conservation in a
uniform infinite electrolyte. A point source has J = I/(4*pi*r**2), from
the area of a sphere. Integrating E from r to infinity, with V(infinity)=0,
gives V(r)=rho*I/(4*pi*r). For a uniform line of total current I and length
L, dI=(I/L) dz. At perpendicular distance r from its midpoint, integrate
rho*dI/(4*pi*sqrt(r**2+z**2)) from -L/2 to L/2:
V(r)=rho*I/(2*pi*L)*asinh(L/(2*r)). Multiply volts by 1000 for mV.
These geometric factors and the SI milli prefix are identities, not
empirical constants. See potential-distance-derivation.md, equations P/L.

Assumptions: steady prescribed outward current, uniform infinite electrolyte,
no structure, no polarisation, no other anodes and no boundaries or return
electrode at finite distance. A line has uniform current per unit length;
it is not an equipotential finite cylinder. Positive output is electrolyte
potential relative to remote electrolyte, not an Ag/AgCl electrode potential.
This is a screening model, not a structure potential prediction.

See also Telford, Geldart and Sheriff, Applied Geophysics, second edition,
Cambridge University Press (1990), chapter 8, Resistivity methods; page to be
confirmed by the maintainer. No unseen textbook page is the derivation basis.
"""

from __future__ import annotations

import math


def _validate(current_a: float, resistivity_ohm_m: float, distance_m: float) -> None:
    """Reject nonphysical inputs while permitting the infinite-distance limit."""
    if not math.isfinite(current_a) or current_a < 0:
        raise ValueError("current_a must be finite and non-negative")
    if not math.isfinite(resistivity_ohm_m) or resistivity_ohm_m <= 0:
        raise ValueError("resistivity_ohm_m must be finite and positive")
    if math.isnan(distance_m) or distance_m <= 0:
        raise ValueError("distance_m must be positive; the source is singular at zero")


def point_source_potential_shift_mv(
    current_a: float, resistivity_ohm_m: float, distance_m: float
) -> float:
    """Return the point-source electrolyte potential rise in mV.

    Args:
        current_a: Prescribed outward current in A; zero is permitted.
        resistivity_ohm_m: Uniform electrolyte resistivity in ohm.m.
        distance_m: Distance from the point centre in m, strictly positive.
            Positive infinity gives zero. For a surface-gap chart, pass
            anode radius plus gap explicitly; zero is never substituted here.

    Returns:
        Non-negative potential relative to remote electrolyte, in mV.

    Raises:
        ValueError: If the inputs violate the documented domain.
    """
    _validate(current_a, resistivity_ohm_m, distance_m)
    return 1000.0 * resistivity_ohm_m * current_a / (4.0 * math.pi * distance_m)


def finite_line_source_potential_shift_mv(
    current_a: float,
    resistivity_ohm_m: float,
    anode_length_m: float,
    distance_m: float,
) -> float:
    """Return the uniform finite-line electrolyte potential rise in mV.

    Args:
        current_a: Total prescribed outward current in A, distributed uniformly.
        resistivity_ohm_m: Uniform electrolyte resistivity in ohm.m.
        anode_length_m: Positive finite line length in m.
        distance_m: Perpendicular distance in m from the line midpoint,
            strictly positive. Positive infinity gives zero. Radius is only
            needed to convert a surface gap to this axis distance.

    Returns:
        Non-negative potential relative to remote electrolyte, in mV.

    Raises:
        ValueError: If the inputs violate the documented domain.
    """
    _validate(current_a, resistivity_ohm_m, distance_m)
    if not math.isfinite(anode_length_m) or anode_length_m <= 0:
        raise ValueError("anode_length_m must be finite and positive")
    return (
        1000.0 * resistivity_ohm_m * current_a
        / (2.0 * math.pi * anode_length_m)
        * math.asinh((anode_length_m / 2.0) / distance_m)
    )
