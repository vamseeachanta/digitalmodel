"""Jumper bending / curvature stress (API 17R, DNV-ST-F101 conventions).

For a rigid jumper bend (or an over-bend on a flexible/vertical jumper) the
bending stress is set by the radius of curvature:

    kappa   = 1 / R                                                 (1)
    sigma_b = E * (D_o / 2) / R = M / Z                             (2)
    M       = E I / R                                               (3)

The minimum bend radius (MBR) check ensures the as-installed/operating
curvature stays below the material/manufacturer limit.

References
----------
API 17R jumper bend design; flexible-pipe MBR per API 17J/17B.
"""

from __future__ import annotations

from dataclasses import dataclass

from .section_properties import PipeSection


@dataclass
class BendingResult:
    """Bending-from-curvature result."""

    curvature: float            # kappa = 1/R [1/m]
    bending_stress: float       # sigma_b [Pa]
    bending_moment: float       # M [N.m]
    utilisation: float          # sigma_b / allowable [-]
    passed: bool


def bending_from_radius(
    section: PipeSection,
    radius: float,
    youngs_modulus: float = 207e9,
    allowable_stress: float = float("inf"),
) -> BendingResult:
    """Bending stress / moment from a bend radius, Eqs. (1)-(3).

    Parameters
    ----------
    section : PipeSection
        Jumper cross section.
    radius : float
        Radius of curvature R [m] (> 0).
    youngs_modulus : float
        E [Pa].
    allowable_stress : float
        Allowable bending stress [Pa] (default inf -> utilisation 0).

    Returns
    -------
    BendingResult
    """
    if radius <= 0:
        raise ValueError("radius must be > 0")
    kappa = 1.0 / radius
    sigma_b = youngs_modulus * (section.outer_diameter / 2.0) / radius
    moment = youngs_modulus * section.moment_of_inertia / radius
    util = sigma_b / allowable_stress if allowable_stress not in (0, float("inf")) else (
        0.0 if allowable_stress == float("inf") else float("inf")
    )
    return BendingResult(
        curvature=kappa,
        bending_stress=sigma_b,
        bending_moment=moment,
        utilisation=util,
        passed=util <= 1.0,
    )
