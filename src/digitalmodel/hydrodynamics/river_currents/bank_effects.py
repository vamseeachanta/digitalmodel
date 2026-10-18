# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rectangular-channel screening corrections applied to existing profiles.

References
----------
NASA Glenn, Conservation of Mass:
https://www.grc.nasa.gov/www/k-12/airplane/mass.html (opened 2026-10-14).
Constant-density continuity gives the section acceleration A/(A-A_v).
OpenStax, University Physics volume 1, section 6.3, Centripetal Force:
https://openstax.org/books/university-physics-volume-1/pages/6-3-centripetal-force
(opened 2026-10-14). Combining radial acceleration u**2/r with hydrostatic
balance gives d(eta)/dr = u**2/(g*r); integration gives superelevation.

The lateral quadratic and bend velocity bias are explicitly assumed shapes,
not literature-derived calibration coefficients. The quadratic integrates to
one over the width; the bend bias integrates to one and retains that property
when multiplied by the symmetric quadratic. Neither is a navigation model.
The vertical law remains the bed-referenced law in ``profiles``.
"""

import math
from collections.abc import Callable


def _positive(**values: float) -> None:
    for name, value in values.items():
        if not math.isfinite(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")


def _position(position: float, width: float) -> float:
    _positive(width=width)
    if not math.isfinite(position) or not 0 <= position <= width:
        raise ValueError("position must be finite and within the channel")
    return position / width


def lateral_factor(position: float, width: float, retardation: float) -> float:
    """Return a unit-mean shape with bank retardation and centre acceleration.

    Args:
        position: Distance from the left bank (m).
        width: Rectangular channel width (m).
        retardation: Assumed fraction in [0,1]; 0 gives a uniform section,
            1 gives zero bank speed and a centre factor of 1.5.

    Returns:
        Dimensionless velocity multiplier, 1-r+6*r*x*(1-x).
    """
    x = _position(position, width)
    if not math.isfinite(retardation) or not 0 <= retardation <= 1:
        raise ValueError("retardation must be finite and in [0,1]")
    return 1 - retardation + 6 * retardation * x * (1 - x)


def blockage_factor(width: float, depth: float, beam: float, draft: float) -> float:
    """Return continuity acceleration through the remaining rectangular area.

    This is a section-average screening factor, not a near-hull flow solution.
    Beam and draft must be strictly less than channel width and depth.
    """
    _positive(width=width, depth=depth, beam=beam, draft=draft)
    if beam >= width or draft >= depth:
        raise ValueError("beam and draft must be below channel width and depth")
    return 1 / (1 - beam * draft / (width * depth))


def bend_factor(position: float, width: float, outer_bias: float) -> float:
    """Apply an assumed linear bias; the right bank is the outer bank.

    ``outer_bias`` is a user-supplied assumption in [0,1), independent of
    superelevation. It does not predict secondary circulation or separation.
    """
    x = _position(position, width)
    if not math.isfinite(outer_bias) or not 0 <= outer_bias < 1:
        raise ValueError("outer_bias must be finite and in [0,1)")
    return 1 + outer_bias * (2 * x - 1)


def bend_superelevation(speed: float, width: float, radius: float,
                       gravity: float) -> float:
    """Return outer-minus-inner water level (m) for constant-speed radial balance.

    The assumed rectangular bend spans radii R-width/2 to R+width/2.
    No empirical bend coefficient is introduced. Gravity is supplied in m/s².
    """
    _positive(width=width, radius=radius, gravity=gravity)
    if not math.isfinite(speed) or radius <= width / 2:
        raise ValueError("speed must be finite and radius must exceed half-width")
    return speed ** 2 / gravity * math.log((radius + width / 2) / (radius - width / 2))


def corrected_profile(profile: Callable[[float], float], factor: float
                      ) -> Callable[[float], float]:
    """Wrap a shipped scalar profile with a fixed nonnegative velocity factor."""
    if not math.isfinite(factor) or factor < 0:
        raise ValueError("factor must be finite and nonnegative")

    def evaluate(height: float) -> float:
        value = float(profile(height))
        if not math.isfinite(value):
            raise ValueError("profile must return finite velocities")
        return factor * value

    return evaluate
