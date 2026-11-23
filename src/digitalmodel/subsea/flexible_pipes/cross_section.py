"""Unbonded flexible pipe cross-section builder (API 17B / API 17J / ISO 13628-2).

An unbonded flexible pipe is built up from concentric, independently moving
layers.  From inside out a typical dynamic-riser cross-section is:

    1. carcass            (interlocked stainless strip — collapse resistance)
    2. internal pressure sheath (extruded polymer — fluid barrier)
    3. pressure armour    (interlocked Z/C section — hoop/radial pressure)
    4. anti-wear / anti-friction tape
    5. tensile armour     (helically wound flat wires, +/- lay angle pairs)
    6. high-strength tapes
    7. external sheath     (outer polymer barrier)

This module is the **geometric / property** core: each layer is described by
its mean diameter, thickness, material density and (for steel structural
layers) its metallic cross-sectional area, lay angle and modulus.  From the
layer stack we deterministically derive the quantities the downstream
design checks (API 17J) and bend-stiffener sizing need:

    * outer / inner (bore) diameter,
    * dry and submerged mass per unit length,
    * net metallic tensile-armour area (both armour packages),
    * axial stiffness ``EA`` and bending stiffness ``EI`` contributions of
      the metallic layers.

Equations
---------
Layer mean diameter and metallic area for a wound armour layer of ``n`` wires
of width ``w`` and thickness ``t`` wound at lay angle ``alpha`` (from pipe
axis), per ISO 13628-2 / API 17B nomenclature:

    A_metallic = n * w * t                                          (1)

The *axial* projected stiffness of a helically wound layer is reduced by the
lay angle.  A first-order (small-strain, wires acting as axial springs)
contribution is

    EA_layer = E * A_metallic * cos^3(alpha)                        (2)

and the bending contribution of a thin armour cylinder of mean radius ``R``
treated as a smeared shell of equivalent wall area ``A_metallic`` is

    EI_layer = E * A_metallic * R^2 / 2 * cos(alpha)                (3)

(the ``cos(alpha)`` accounts for the helical projection; the ``/2`` is the
mean-of-sin^2 distribution of wires around the circumference).  These are
*indicative* smeared-property estimates — detailed flexible-pipe analysis
solves the full layer-interaction problem; equations (2)-(3) give a
deterministic, repeatable property for sizing and screening.

Mass per unit length of a layer of mean diameter ``D_m``, thickness ``t`` and
material density ``rho`` (treated as a full cylindrical shell of that wall):

    m_layer = rho * pi * D_m * t          [kg/m]                    (4)

References
----------
* API 17B, Recommended Practice for Flexible Pipe.
* API 17J / ISO 13628-2, Unbonded flexible pipe specification.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional

WATER_DENSITY = 1025.0  # kg/m^3, seawater
GRAVITY = 9.80665       # m/s^2


class LayerType(str, Enum):
    """Functional class of an unbonded flexible pipe layer."""

    CARCASS = "carcass"
    PRESSURE_SHEATH = "pressure_sheath"
    PRESSURE_ARMOUR = "pressure_armour"
    ANTI_WEAR = "anti_wear"
    TENSILE_ARMOUR = "tensile_armour"
    OUTER_SHEATH = "outer_sheath"
    TAPE = "tape"


@dataclass(frozen=True)
class Layer:
    """A single concentric layer of an unbonded flexible pipe.

    Parameters
    ----------
    layer_type : LayerType
        Functional class.
    inner_diameter : float
        Inner diameter of this layer [m] (= outer diameter of the layer
        immediately inside it).
    thickness : float
        Radial build of the layer [m].
    density : float
        Material density [kg/m^3].
    youngs_modulus : float, optional
        Axial modulus of the (metallic) wires [Pa].  Used only for
        structural layers (pressure / tensile armour, carcass).
    metallic_area : float, optional
        Net metallic cross-sectional area of the layer [m^2] (sum of all
        wires, eq. 1).  If omitted, the full annular wall area is used
        (appropriate for solid polymer sheaths, not wound armour).
    lay_angle_deg : float, optional
        Helix lay angle measured from the pipe axis [deg].  0 for an
        axial element, ~90 for a pure hoop element.  Tensile armours are
        typically 20-55 deg, pressure armour ~85-90 deg.
    """

    layer_type: LayerType
    inner_diameter: float
    thickness: float
    density: float
    youngs_modulus: float = 0.0
    metallic_area: Optional[float] = None
    lay_angle_deg: float = 0.0

    def __post_init__(self) -> None:
        if self.inner_diameter <= 0:
            raise ValueError("inner_diameter must be positive")
        if self.thickness <= 0:
            raise ValueError("thickness must be positive")
        if self.density < 0:
            raise ValueError("density must be non-negative")

    @property
    def mean_diameter(self) -> float:
        """Mean diameter of the layer wall [m]."""
        return self.inner_diameter + self.thickness

    @property
    def outer_diameter(self) -> float:
        """Outer diameter of the layer [m]."""
        return self.inner_diameter + 2.0 * self.thickness

    @property
    def annular_area(self) -> float:
        """Full annular cross-sectional area of the layer wall [m^2]."""
        ro = self.outer_diameter / 2.0
        ri = self.inner_diameter / 2.0
        return math.pi * (ro * ro - ri * ri)

    @property
    def structural_area(self) -> float:
        """Metallic area used for stiffness (eq. 1), else annular area."""
        return self.metallic_area if self.metallic_area is not None else self.annular_area

    def mass_per_length(self) -> float:
        """Dry mass per unit length [kg/m] (eq. 4, full-wall shell)."""
        return self.density * math.pi * self.mean_diameter * self.thickness

    def axial_stiffness(self) -> float:
        """EA contribution of this layer [N] (eq. 2)."""
        if self.youngs_modulus <= 0:
            return 0.0
        alpha = math.radians(self.lay_angle_deg)
        return self.youngs_modulus * self.structural_area * math.cos(alpha) ** 3

    def bending_stiffness(self) -> float:
        """EI contribution of this layer [N.m^2] (eq. 3)."""
        if self.youngs_modulus <= 0:
            return 0.0
        alpha = math.radians(self.lay_angle_deg)
        radius = self.mean_diameter / 2.0
        return (
            self.youngs_modulus
            * self.structural_area
            * radius * radius
            / 2.0
            * math.cos(alpha)
        )


@dataclass
class FlexiblePipeSection:
    """Assembled unbonded flexible pipe cross-section (API 17B / 17J).

    Layers must be supplied inner -> outer and be radially contiguous
    (each layer's ``inner_diameter`` equals the previous layer's
    ``outer_diameter`` within ``tol``).
    """

    layers: List[Layer]
    name: str = "flexible_pipe"
    tol: float = 1e-6

    def __post_init__(self) -> None:
        if not self.layers:
            raise ValueError("at least one layer is required")
        for prev, nxt in zip(self.layers, self.layers[1:]):
            gap = abs(nxt.inner_diameter - prev.outer_diameter)
            if gap > self.tol:
                raise ValueError(
                    f"layers are not contiguous: {prev.layer_type.value} OD "
                    f"{prev.outer_diameter:.6f} != {nxt.layer_type.value} ID "
                    f"{nxt.inner_diameter:.6f} (gap {gap:.2e} m)"
                )

    @property
    def bore_diameter(self) -> float:
        """Internal bore diameter [m] (inside of innermost layer)."""
        return self.layers[0].inner_diameter

    @property
    def outer_diameter(self) -> float:
        """Overall outer diameter [m]."""
        return self.layers[-1].outer_diameter

    def dry_mass_per_length(self) -> float:
        """Total dry mass per unit length [kg/m]."""
        return sum(layer.mass_per_length() for layer in self.layers)

    def displaced_mass_per_length(self, fluid_density: float = WATER_DENSITY) -> float:
        """Mass of fluid displaced by the pipe outer envelope [kg/m]."""
        ro = self.outer_diameter / 2.0
        return fluid_density * math.pi * ro * ro

    def content_mass_per_length(self, content_density: float = 0.0) -> float:
        """Mass of bore contents per unit length [kg/m]."""
        rb = self.bore_diameter / 2.0
        return content_density * math.pi * rb * rb

    def submerged_weight_per_length(
        self,
        content_density: float = 0.0,
        fluid_density: float = WATER_DENSITY,
    ) -> float:
        """Submerged (effective) weight per unit length [N/m].

        Positive = heavier than water (sinks).  Includes bore contents.
        """
        net_mass = (
            self.dry_mass_per_length()
            + self.content_mass_per_length(content_density)
            - self.displaced_mass_per_length(fluid_density)
        )
        return net_mass * GRAVITY

    def tensile_armour_area(self) -> float:
        """Total net metallic area of all tensile-armour layers [m^2]."""
        return sum(
            layer.structural_area
            for layer in self.layers
            if layer.layer_type is LayerType.TENSILE_ARMOUR
        )

    def axial_stiffness(self) -> float:
        """Total EA of the cross-section [N] (sum of metallic layers)."""
        return sum(layer.axial_stiffness() for layer in self.layers)

    def bending_stiffness(self) -> float:
        """Total EI of the cross-section [N.m^2] (sum of metallic layers)."""
        return sum(layer.bending_stiffness() for layer in self.layers)
