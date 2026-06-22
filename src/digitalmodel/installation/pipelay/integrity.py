"""S-lay pipelay installation integrity calculations.

Generic, code/method-based screening calculations used when analysing the
installation of a rigid subsea pipeline by conventional S-lay from a lay
barge. The four methods ported here are:

1. Concrete-coating crushing check (Verley & Ness limiting-strain criterion).
2. Minimum stable route / lay radius: ``R = F / (S * W)``.
3. Allowable weld-repair / cut-out length (DNV-ST-F101 percentage limits).
4. S-lay submerged pipe weight and lay (top) tension from a catenary.

All formulas are consistent-unit unless a docstring states otherwise. No unit
conversion is performed internally except where a function explicitly says so.

References
----------
- Verley, R. & Ness, T. (1995) — concrete-coating crushing / composite-section
  limiting-strain behaviour of concrete-weight-coated (CWC) pipe in the
  overbend. The practical design check is a limiting compressive bending strain
  in the concrete relative to its crushing strain.
- DNV-ST-F101 — Submarine Pipeline Systems (weld-repair allowable length).
- Standard buoyancy (Archimedes) for submerged weight and the steel/free-hanging
  catenary relation for lay-tension estimation.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

# Standard acceleration of gravity [m/s^2].
G_STANDARD: float = 9.80665


# ---------------------------------------------------------------------------
# 1. Concrete-coating crushing check (Verley & Ness limiting-strain criterion)
# ---------------------------------------------------------------------------
#
# Bending of the steel pipe over the stinger (overbend) is transferred through
# the anti-corrosion coating into the concrete weight coating. Excessive
# compressive strain crushes the concrete. For a section bent to a radius of
# curvature R, the outer-fibre bending strain of the steel at the concrete
# interface is approximately:
#
#       eps_bend = (OD_steel / 2) / R            (= curvature * neutral-axis dist)
#
# The Verley & Ness work shows the strain in the concrete is amplified relative
# to the steel by a strain-concentration factor SCF (>= 1) arising from the
# composite (steel + corrosion-coat + concrete) section response and sliding at
# the interface. The crushing check compares the concentrated concrete strain
# against the concrete crushing strain (with a factor of safety):
#
#       eps_concrete = SCF * eps_bend
#       utilisation  = eps_concrete / (eps_crush / FoS)
#
# A utilisation < 1 passes. This is the practical, hand-verifiable form of the
# limiting-strain criterion (the full Verley & Ness sectional model derives SCF
# and eps_crush from the material non-linearities; here they are supplied as
# inputs so the screening check is exact and transparent).


def bending_strain_from_radius(outer_diameter: float, bend_radius: float) -> float:
    """Outer-fibre bending strain of a tube bent to a radius of curvature.

    ``eps = (OD / 2) / R``, i.e. the neutral-axis-to-fibre distance times the
    curvature ``1/R``.

    Parameters
    ----------
    outer_diameter : float
        Outer diameter of the steel pipe [length].
    bend_radius : float
        Radius of curvature of the bent pipe [length].

    Returns
    -------
    float
        Dimensionless outer-fibre bending strain.
    """
    if bend_radius <= 0.0:
        raise ValueError("bend_radius must be positive")
    return (outer_diameter / 2.0) / bend_radius


def concrete_crushing_strain(bending_strain: float, strain_concentration_factor: float) -> float:
    """Compressive strain seen by the concrete coating.

    ``eps_concrete = SCF * eps_bend`` where ``SCF >= 1`` is the strain
    concentration factor from the composite-section (Verley & Ness) response.
    """
    if strain_concentration_factor < 1.0:
        raise ValueError("strain_concentration_factor must be >= 1.0")
    return strain_concentration_factor * bending_strain


@dataclass(frozen=True)
class CrushingCheck:
    """Result of a concrete-coating crushing check."""

    bending_strain: float  # outer-fibre steel bending strain [-]
    concrete_strain: float  # concentrated concrete strain [-]
    allowable_strain: float  # crushing strain / FoS [-]
    utilisation: float  # concrete_strain / allowable_strain [-]
    passes: bool  # True if utilisation < 1


def concrete_crushing_check(
    outer_diameter: float,
    bend_radius: float,
    crushing_strain_limit: float,
    strain_concentration_factor: float = 1.0,
    factor_of_safety: float = 1.0,
) -> CrushingCheck:
    """Verley & Ness concrete-crushing limiting-strain check in the overbend.

    Parameters
    ----------
    outer_diameter : float
        Steel-pipe outer diameter [length].
    bend_radius : float
        Overbend radius of curvature [length].
    crushing_strain_limit : float
        Concrete crushing strain (e.g. ~0.002 = 0.2%) [-].
    strain_concentration_factor : float, optional
        Composite-section strain amplification SCF (>= 1). Default 1.0.
    factor_of_safety : float, optional
        Factor of safety applied to the crushing strain. Default 1.0.

    Returns
    -------
    CrushingCheck
        Strains, allowable, utilisation and pass/fail flag.
    """
    if factor_of_safety <= 0.0:
        raise ValueError("factor_of_safety must be positive")
    eps_bend = bending_strain_from_radius(outer_diameter, bend_radius)
    eps_conc = concrete_crushing_strain(eps_bend, strain_concentration_factor)
    allowable = crushing_strain_limit / factor_of_safety
    util = eps_conc / allowable
    return CrushingCheck(
        bending_strain=eps_bend,
        concrete_strain=eps_conc,
        allowable_strain=allowable,
        utilisation=util,
        passes=util < 1.0,
    )


# ---------------------------------------------------------------------------
# 2. Minimum stable route / lay radius:  R = F / (S * W)
# ---------------------------------------------------------------------------
#
# Where the route has curves/turning points, the laid pipe must not slip
# laterally off the planned radius under bottom (touch-down) tension. The
# minimum stable radius balances the lateral component of the bottom tension
# against the available pipe-soil friction:
#
#       R_min = F / (S * W)
#
#   R_min = minimum stable route radius
#   F     = touch-down (bottom) tension
#   S     = coefficient of lateral pipe-soil friction
#   W     = submerged unit weight of the pipeline (force per unit length)


def minimum_route_radius(
    bottom_tension: float, lateral_friction: float, submerged_weight_per_length: float
) -> float:
    """Minimum stable route / lay radius ``R = F / (S * W)``.

    Parameters
    ----------
    bottom_tension : float
        Touch-down (bottom) tension F [force].
    lateral_friction : float
        Coefficient of lateral pipe-soil friction S [-].
    submerged_weight_per_length : float
        Submerged unit weight W of the pipeline [force/length].

    Returns
    -------
    float
        Minimum stable route radius [length].
    """
    if lateral_friction <= 0.0:
        raise ValueError("lateral_friction must be positive")
    if submerged_weight_per_length <= 0.0:
        raise ValueError("submerged_weight_per_length must be positive")
    return bottom_tension / (lateral_friction * submerged_weight_per_length)


def route_radius_factor_of_safety(planned_radius: float, minimum_radius: float) -> float:
    """Factor of safety of a planned route radius against the minimum stable one.

    ``FoS = planned_radius / minimum_radius`` (target ~1.5).
    """
    if minimum_radius <= 0.0:
        raise ValueError("minimum_radius must be positive")
    return planned_radius / minimum_radius


# ---------------------------------------------------------------------------
# 3. Allowable weld-repair / cut-out length (DNV-ST-F101 percentage limits)
# ---------------------------------------------------------------------------
#
# The maximum length of girth weld that may be removed/repaired in place is
# limited to a fraction of the total weld length, subject to an absolute minimum
# repair length. Typical limiting criteria (per DNV-ST-F101):
#   - Partial-penetration repair: max 30% of total weld length.
#   - Full-penetration repair:    max 20% of total weld length.
#   - Minimum weld-repair length:  50 mm.

PARTIAL_PENETRATION_FRACTION: float = 0.30
FULL_PENETRATION_FRACTION: float = 0.20
DEFAULT_MIN_REPAIR_LENGTH: float = 0.050  # 50 mm in metres


def weld_repair_allowable_length(
    total_weld_length: float,
    allowable_fraction: float,
    minimum_repair_length: float = DEFAULT_MIN_REPAIR_LENGTH,
) -> float:
    """Allowable weld-repair (cut-out) length.

    The allowable length is the larger of the fractional limit and the absolute
    minimum repair length (you cannot make a useful repair shorter than the
    minimum, so the minimum governs the *floor* of the allowable length).

    Parameters
    ----------
    total_weld_length : float
        Total girth-weld length (e.g. pi * OD) [length].
    allowable_fraction : float
        Permitted fraction of the total weld length (0 < f <= 1), e.g. 0.30 for
        partial-penetration or 0.20 for full-penetration repairs.
    minimum_repair_length : float, optional
        Absolute minimum weld-repair length [length]. Default 50 mm.

    Returns
    -------
    float
        Allowable weld-repair length [length].
    """
    if not 0.0 < allowable_fraction <= 1.0:
        raise ValueError("allowable_fraction must be in (0, 1]")
    return max(allowable_fraction * total_weld_length, minimum_repair_length)


def is_weld_repair_acceptable(
    repair_length: float,
    total_weld_length: float,
    allowable_fraction: float,
    minimum_repair_length: float = DEFAULT_MIN_REPAIR_LENGTH,
) -> bool:
    """True if a proposed repair length is between the minimum and the fractional limit."""
    fractional_limit = allowable_fraction * total_weld_length
    return minimum_repair_length <= repair_length <= fractional_limit


# ---------------------------------------------------------------------------
# 4. S-lay submerged pipe weight and lay (top) tension
# ---------------------------------------------------------------------------
#
# Submerged (wet) weight per unit length of a pipe is the dry weight minus the
# buoyancy of the displaced water (Archimedes), using the *total* outer diameter
# of the laid pipe (including any coatings):
#
#       w_sub = w_dry - rho_water * g * (pi/4) * OD_total^2
#
# For a free-hanging suspended span (catenary) of a near-neutrally-stiff line in
# water depth d, with horizontal tension H, the top (lay) tension is:
#
#       T_top = H + w_sub * d
#
# where H = w_sub * R is the horizontal tension implied by the sagbend radius R
# at touch-down (H = w_sub * R for a catenary whose minimum radius is R).


def displaced_weight_per_length(
    outer_diameter: float, water_density: float, gravity: float = G_STANDARD
) -> float:
    """Buoyancy force per unit length of a fully submerged circular pipe.

    ``b = rho_water * g * (pi/4) * OD^2`` [force/length].

    Parameters
    ----------
    outer_diameter : float
        Total outer diameter of the laid pipe (incl. coatings) [m].
    water_density : float
        Water density [kg/m^3].
    gravity : float, optional
        Gravitational acceleration [m/s^2]. Default 9.80665.
    """
    return water_density * gravity * (math.pi / 4.0) * outer_diameter**2


def submerged_weight_per_length(
    dry_weight_per_length: float,
    outer_diameter: float,
    water_density: float,
    gravity: float = G_STANDARD,
) -> float:
    """Submerged (wet) unit weight ``w_sub = w_dry - buoyancy``.

    Parameters
    ----------
    dry_weight_per_length : float
        Dry (in-air) weight per unit length [N/m].
    outer_diameter : float
        Total outer diameter of the laid pipe (incl. coatings) [m].
    water_density : float
        Water density [kg/m^3].
    gravity : float, optional
        Gravitational acceleration [m/s^2]. Default 9.80665.

    Returns
    -------
    float
        Submerged unit weight [N/m] (positive = net-down / bottom-stable).
    """
    return dry_weight_per_length - displaced_weight_per_length(
        outer_diameter, water_density, gravity
    )


def horizontal_lay_tension(submerged_weight_per_length: float, sagbend_radius: float) -> float:
    """Horizontal tension of a catenary sagbend ``H = w_sub * R``.

    Parameters
    ----------
    submerged_weight_per_length : float
        Submerged unit weight w_sub [force/length].
    sagbend_radius : float
        Minimum (touch-down) radius of curvature of the sagbend [length].
    """
    return submerged_weight_per_length * sagbend_radius


def top_lay_tension(
    submerged_weight_per_length: float, water_depth: float, sagbend_radius: float
) -> float:
    """Top (lay) tension of a free-hanging catenary span.

    ``T_top = H + w_sub * d = w_sub * (R + d)`` for a catenary of minimum radius
    R laid in water depth d.

    Parameters
    ----------
    submerged_weight_per_length : float
        Submerged unit weight w_sub [force/length].
    water_depth : float
        Water depth d [length].
    sagbend_radius : float
        Minimum (touch-down) sagbend radius R [length].

    Returns
    -------
    float
        Top tension at the lay vessel [force].
    """
    h = horizontal_lay_tension(submerged_weight_per_length, sagbend_radius)
    return h + submerged_weight_per_length * water_depth
