"""Drilling-riser section properties and stiffness stack-up.

Computes the cross-sectional and stiffness properties of a tubular riser
section (area, second moment of area, polar moment, axial / bending /
torsional stiffness) from its outer and inner diameter, Young's modulus and
Poisson's ratio. These feed the per-joint stack-up that prepares an
OrcaFlex line-type input for a drilling riser string.

Geometry conventions (consistent-unit; no internal unit conversion unless a
helper explicitly converts):
    A   = pi/4 * (OD^2 - ID^2)
    I   = pi/64 * (OD^4 - ID^4)
    J   = 2 * I                       (polar second moment, thin/thick tube)
    G   = E / (2 * (1 + nu))          (shear modulus)
    EA  = E * A                       (axial stiffness)
    EI  = E * I                       (bending stiffness)
    GJ  = G * J                       (torsional stiffness)

References:
    - Roark's Formulas for Stress and Strain (hollow circular section).
    - API RP 16Q / 2RD tubular section properties.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

# Imperial -> SI helper constants (used by the *_imperial convenience layer).
INCH_TO_METER: float = 0.0254
KSI_TO_PA: float = 6_894_757.293178  # 1 ksi = 6.894757293178e6 Pa


@dataclass(frozen=True)
class SectionProperties:
    """Cross-sectional and stiffness properties of a tubular section.

    All values are in the unit system implied by the inputs:
    if OD/ID are in metres and E in Pa, then area is m^2, I and J are m^4,
    EA is N, EI and GJ are N.m^2.
    """

    area: float  # cross-sectional steel area [length^2]
    moment_of_inertia: float  # second moment of area I [length^4]
    polar_moment: float  # polar second moment J = 2I [length^4]
    shear_modulus: float  # G = E / (2(1+nu)) [force/length^2]
    axial_stiffness: float  # EA [force]
    bending_stiffness: float  # EI [force.length^2]
    torsional_stiffness: float  # GJ [force.length^2]


def cross_sectional_area(outer_diameter: float, inner_diameter: float) -> float:
    """Steel area of a hollow circular section: A = pi/4 (OD^2 - ID^2)."""
    return math.pi / 4.0 * (outer_diameter**2 - inner_diameter**2)


def moment_of_inertia(outer_diameter: float, inner_diameter: float) -> float:
    """Second moment of area of a hollow circular section.

    I = pi/64 (OD^4 - ID^4).
    """
    return math.pi / 64.0 * (outer_diameter**4 - inner_diameter**4)


def polar_moment_of_inertia(outer_diameter: float, inner_diameter: float) -> float:
    """Polar second moment of area: J = 2 * I for a circular section."""
    return 2.0 * moment_of_inertia(outer_diameter, inner_diameter)


def shear_modulus(youngs_modulus: float, poisson_ratio: float) -> float:
    """Shear modulus G = E / (2 (1 + nu))."""
    return youngs_modulus / (2.0 * (1.0 + poisson_ratio))


def section_properties(
    outer_diameter: float,
    inner_diameter: float,
    youngs_modulus: float,
    poisson_ratio: float,
) -> SectionProperties:
    """Compute full section-property set for a tubular riser section.

    Parameters
    ----------
    outer_diameter : float
        Outer diameter [length].
    inner_diameter : float
        Inner diameter [length].
    youngs_modulus : float
        Young's modulus E [force/length^2].
    poisson_ratio : float
        Poisson's ratio nu [-].

    Returns
    -------
    SectionProperties
        Area, I, J, G, EA, EI, GJ in the consistent unit system of the inputs.
    """
    area = cross_sectional_area(outer_diameter, inner_diameter)
    inertia = moment_of_inertia(outer_diameter, inner_diameter)
    polar = 2.0 * inertia
    g = shear_modulus(youngs_modulus, poisson_ratio)
    return SectionProperties(
        area=area,
        moment_of_inertia=inertia,
        polar_moment=polar,
        shear_modulus=g,
        axial_stiffness=youngs_modulus * area,
        bending_stiffness=youngs_modulus * inertia,
        torsional_stiffness=g * polar,
    )


def section_properties_imperial(
    outer_diameter_in: float,
    inner_diameter_in: float,
    youngs_modulus_ksi: float,
    poisson_ratio: float,
) -> SectionProperties:
    """Section properties from imperial inputs, returned in SI units.

    Diameters in inches are converted to metres and E in ksi to Pa, so the
    returned area is m^2, I/J are m^4, EA is N and EI/GJ are N.m^2.

    Parameters
    ----------
    outer_diameter_in : float
        Outer diameter [inch].
    inner_diameter_in : float
        Inner diameter [inch].
    youngs_modulus_ksi : float
        Young's modulus [ksi].
    poisson_ratio : float
        Poisson's ratio [-].

    Returns
    -------
    SectionProperties
        Properties in SI units (m, m^2, m^4, Pa, N, N.m^2).
    """
    return section_properties(
        outer_diameter=outer_diameter_in * INCH_TO_METER,
        inner_diameter=inner_diameter_in * INCH_TO_METER,
        youngs_modulus=youngs_modulus_ksi * KSI_TO_PA,
        poisson_ratio=poisson_ratio,
    )
