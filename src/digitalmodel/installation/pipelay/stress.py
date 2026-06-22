"""Overbend / sagbend longitudinal-stress (%SMYS) acceptance check for S-lay.

Generic, method-based screening check used during conventional S-lay pipeline
installation. In S-lay the pipe forms two high-curvature zones — the supported
**overbend** over the stinger and the unsupported **sagbend** suspended span.
The outer-fibre longitudinal stress (or, equivalently, strain) in each zone is
checked against an allowable expressed as a percentage of the steel's Specified
Minimum Yield Strength (SMYS).

The outer-fibre longitudinal stress combines the axial (membrane) stress from
effective/wall tension and the bending stress from curvature:

    sigma_axial = N / A
    sigma_bend  = E * (OD / 2) / R          (= E * curvature * neutral-axis dist)
    sigma_long  = sigma_axial + sigma_bend

The acceptance criterion is

    sigma_long <= allowable_fraction * SMYS

with typical allowable fractions of 85% in the overbend and 72% in the sagbend
(exposed as parameters here so any project's acceptance values can be supplied).

A pure-bending strain form is also provided for the strain-based screening that
some methods use:

    eps_bend = (OD / 2) / R
    eps_long <= (allowable_fraction * SMYS) / E

All functions are unit-consistent: pass E, SMYS and the resulting stresses in
the same unit system (e.g. MPa with N in newtons and A in m^2, or psi with N in
lbf and A in in^2). No internal unit conversion is performed.

References
----------
- DNV-ST-F101 / DNV-OS-F101 — Submarine Pipeline Systems (installation
  longitudinal-stress %SMYS acceptance in the over/sagbend).
"""

from __future__ import annotations

from dataclasses import dataclass

# Common installation %SMYS acceptance values (fractions, not percentages).
DEFAULT_OVERBEND_FRACTION: float = 0.85
DEFAULT_SAGBEND_FRACTION: float = 0.72


def bending_stress_from_radius(
    outer_diameter: float, bend_radius: float, youngs_modulus: float
) -> float:
    """Outer-fibre longitudinal bending stress of a tube bent to a radius.

    ``sigma = E * (OD / 2) / R`` — Young's modulus times the outer-fibre
    bending strain (neutral-axis-to-fibre distance times the curvature ``1/R``).

    Parameters
    ----------
    outer_diameter : float
        Outer diameter of the steel pipe [length].
    bend_radius : float
        Radius of curvature of the bent pipe [length].
    youngs_modulus : float
        Young's modulus of the steel [stress].

    Returns
    -------
    float
        Outer-fibre bending stress [stress].
    """
    if bend_radius <= 0.0:
        raise ValueError("bend_radius must be positive")
    return youngs_modulus * (outer_diameter / 2.0) / bend_radius


def axial_stress(axial_force: float, steel_area: float) -> float:
    """Membrane (axial) stress ``sigma = N / A``.

    Parameters
    ----------
    axial_force : float
        Wall (axial) force carried by the steel cross-section [force].
    steel_area : float
        Steel cross-sectional area [length^2].
    """
    if steel_area <= 0.0:
        raise ValueError("steel_area must be positive")
    return axial_force / steel_area


@dataclass(frozen=True)
class SmysCheck:
    """Result of an over/sagbend longitudinal-stress %SMYS acceptance check."""

    longitudinal_stress: float  # combined outer-fibre longitudinal stress [stress]
    allowable_stress: float  # allowable_fraction * SMYS [stress]
    utilisation: float  # longitudinal_stress / allowable_stress [-]
    percent_smys: float  # longitudinal_stress / SMYS * 100 [%]
    passes: bool  # True if longitudinal_stress <= allowable_stress


def smys_stress_check(
    outer_diameter: float,
    bend_radius: float,
    youngs_modulus: float,
    smys: float,
    allowable_fraction: float,
    axial_force: float = 0.0,
    steel_area: float | None = None,
) -> SmysCheck:
    """Longitudinal-stress %SMYS acceptance check for an over/sagbend section.

    Combines the outer-fibre bending stress from the section curvature with an
    optional axial (membrane) stress, and compares the total against
    ``allowable_fraction * SMYS``.

    Parameters
    ----------
    outer_diameter : float
        Steel-pipe outer diameter [length].
    bend_radius : float
        Radius of curvature of the section (overbend or sagbend) [length].
    youngs_modulus : float
        Young's modulus of the steel [stress].
    smys : float
        Specified minimum yield strength [stress].
    allowable_fraction : float
        Allowable longitudinal stress as a fraction of SMYS (e.g. 0.85 overbend,
        0.72 sagbend). Must be in (0, 1].
    axial_force : float, optional
        Wall (axial) force carried by the steel section [force]. Default 0.
    steel_area : float, optional
        Steel cross-sectional area [length^2]. Required only when
        ``axial_force`` is non-zero.

    Returns
    -------
    SmysCheck
        Combined stress, allowable, utilisation, %SMYS and pass/fail flag.
    """
    if not 0.0 < allowable_fraction <= 1.0:
        raise ValueError("allowable_fraction must be in (0, 1]")
    if smys <= 0.0:
        raise ValueError("smys must be positive")

    sigma_bend = bending_stress_from_radius(outer_diameter, bend_radius, youngs_modulus)
    sigma_axial = 0.0
    if axial_force != 0.0:
        if steel_area is None:
            raise ValueError("steel_area is required when axial_force is non-zero")
        sigma_axial = axial_stress(axial_force, steel_area)

    sigma_long = sigma_axial + sigma_bend
    allowable = allowable_fraction * smys
    return SmysCheck(
        longitudinal_stress=sigma_long,
        allowable_stress=allowable,
        utilisation=sigma_long / allowable,
        percent_smys=sigma_long / smys * 100.0,
        passes=sigma_long <= allowable,
    )


def allowable_bend_radius(
    outer_diameter: float,
    youngs_modulus: float,
    smys: float,
    allowable_fraction: float,
) -> float:
    """Minimum bend radius for pure bending to stay within ``f * SMYS``.

    Solving ``E * (OD/2) / R <= f * SMYS`` for R gives the smallest admissible
    radius ``R_min = E * (OD/2) / (f * SMYS)``. (Axial stress not included; this
    is the pure-bending screening radius.)

    Parameters
    ----------
    outer_diameter : float
        Steel-pipe outer diameter [length].
    youngs_modulus : float
        Young's modulus of the steel [stress].
    smys : float
        Specified minimum yield strength [stress].
    allowable_fraction : float
        Allowable bending stress as a fraction of SMYS. Must be in (0, 1].

    Returns
    -------
    float
        Minimum admissible bend radius [length].
    """
    if not 0.0 < allowable_fraction <= 1.0:
        raise ValueError("allowable_fraction must be in (0, 1]")
    if smys <= 0.0:
        raise ValueError("smys must be positive")
    return youngs_modulus * (outer_diameter / 2.0) / (allowable_fraction * smys)
