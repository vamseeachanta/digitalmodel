"""Thin-/thick-wall pipe section properties for subsea jumper mechanics.

These are the standard geometric properties of a hollow circular cross
section used throughout the connector / jumper load-capacity checks.  They
are pure functions (SI units) so they can be reused and unit-tested without
pulling in the config/router machinery of ``subsea/pipeline``.

References
----------
Roark's Formulas for Stress and Strain, 8th ed., Table 9.1 (circular
hollow section).  Identical geometry conventions are used by API 17R /
ISO 13628-1 connector and jumper sizing.
"""

from __future__ import annotations

import math
from dataclasses import dataclass


@dataclass(frozen=True)
class PipeSection:
    """Hollow circular cross section.

    Parameters
    ----------
    outer_diameter : float
        Pipe outer diameter D_o [m].
    wall_thickness : float
        Wall thickness t [m].
    """

    outer_diameter: float
    wall_thickness: float

    def __post_init__(self) -> None:
        if self.outer_diameter <= 0:
            raise ValueError("outer_diameter must be > 0")
        if not 0 < self.wall_thickness < self.outer_diameter / 2:
            raise ValueError("wall_thickness must be in (0, D_o/2)")

    @property
    def inner_diameter(self) -> float:
        """Inner diameter D_i = D_o - 2 t [m]."""
        return self.outer_diameter - 2.0 * self.wall_thickness

    @property
    def area(self) -> float:
        """Steel (annulus) cross-sectional area A = pi/4 (D_o^2 - D_i^2) [m^2]."""
        d_o = self.outer_diameter
        d_i = self.inner_diameter
        return math.pi / 4.0 * (d_o**2 - d_i**2)

    @property
    def bore_area(self) -> float:
        """Internal bore area A_i = pi/4 D_i^2 [m^2] (pressure end-cap area)."""
        return math.pi / 4.0 * self.inner_diameter**2

    @property
    def moment_of_inertia(self) -> float:
        """Second moment of area I = pi/64 (D_o^4 - D_i^4) [m^4]."""
        d_o = self.outer_diameter
        d_i = self.inner_diameter
        return math.pi / 64.0 * (d_o**4 - d_i**4)

    @property
    def section_modulus(self) -> float:
        """Elastic section modulus Z = 2 I / D_o [m^3]."""
        return 2.0 * self.moment_of_inertia / self.outer_diameter
