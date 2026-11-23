# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Annular pipe properties and fully submerged, filled-pipe weight in SI.

Sources: Engineering Statics: Open and Interactive (2020), section 10.2.4;
OpenStax University Physics Volume 1 (2016), section 14.4. The calculation
basis derives annular integrals; geometric factors are not empirical values.
"""

import math
from dataclasses import astuple, dataclass, fields

from ._validation import finite_value


@dataclass(frozen=True)
class SectionInputs:
    """Explicit inputs; coating is nonstructural and contents fill the bore.

    Zero density represents an omitted coating or evacuated bore. No density,
    modulus or gravity is inferred. Pipe material is homogeneous and isotropic.
    """

    outer_diameter_m: float
    wall_thickness_m: float
    coating_thickness_m: float
    pipe_density_kg_m3: float
    coating_density_kg_m3: float
    contents_density_kg_m3: float
    water_density_kg_m3: float
    youngs_modulus_pa: float
    gravity_m_s2: float


@dataclass(frozen=True)
class SectionProperties:
    """Centroidal elastic properties and masses per length; downward weight positive."""

    inner_diameter_m: float
    coated_diameter_m: float
    pipe_area_m2: float
    bore_area_m2: float
    coating_area_m2: float
    displaced_area_m2: float
    second_moment_m4: float
    section_modulus_m3: float
    dry_mass_kg_m: float
    filled_mass_kg_m: float
    submerged_weight_n_m: float
    axial_stiffness_n: float
    bending_stiffness_n_m2: float


def calculate_section_properties(inputs: SectionInputs) -> SectionProperties:
    """Calculate a sealed, fully submerged coated annulus; reject invalid geometry."""
    positive = {
        "outer_diameter_m",
        "wall_thickness_m",
        "pipe_density_kg_m3",
        "youngs_modulus_pa",
        "gravity_m_s2",
    }
    for field in fields(inputs):
        finite_value(
            field.name, getattr(inputs, field.name), positive=field.name in positive
        )
    d, t, c = (
        inputs.outer_diameter_m,
        inputs.wall_thickness_m,
        inputs.coating_thickness_m,
    )
    # Diameter/radius identity and annular integrals: calculation basis S1.
    inner, coated = d - 2 * t, d + 2 * c
    if not 0 < inner < d or not math.isfinite(coated):
        raise ValueError("wall thickness must leave a finite, positive bore")
    try:
        # Engineering Statics (2020), 10.2.4: subtract concentric disks.
        pipe_area = math.pi * t * (d - t)
        bore_area = math.pi * inner**2 / 4
        coat_area = math.pi * c * (d + c)
        displaced_area = math.pi * coated**2 / 4
        inertia = math.pi * (d - inner) * (d + inner) * (d * d + inner * inner) / 64
        dry_mass = (
            inputs.pipe_density_kg_m3 * pipe_area
            + inputs.coating_density_kg_m3 * coat_area
        )
        filled_mass = dry_mass + inputs.contents_density_kg_m3 * bore_area
        # OpenStax University Physics Volume 1 (2016), 14.4: F_b=rho*g*V.
        weight = (
            filled_mass - inputs.water_density_kg_m3 * displaced_area
        ) * inputs.gravity_m_s2
        result = SectionProperties(
            inner,
            coated,
            pipe_area,
            bore_area,
            coat_area,
            displaced_area,
            inertia,
            2 * inertia / d,
            dry_mass,
            filled_mass,
            weight,
            inputs.youngs_modulus_pa * pipe_area,
            inputs.youngs_modulus_pa * inertia,
        )
    except OverflowError as error:
        raise ValueError("section properties exceed numeric range") from error
    if (
        not all(math.isfinite(v) for v in astuple(result))
        or min(pipe_area, inertia) <= 0
    ):
        raise ValueError("section properties exceed numeric range")
    return result
