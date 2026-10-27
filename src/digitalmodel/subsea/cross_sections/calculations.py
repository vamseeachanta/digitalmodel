# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Uniform concentric radial sections in SI units (m, kg, s, Pa).

No implicit unit conversion, strength, packed-component mechanics or design-code
acceptance is evaluated. Density, environment and moduli are caller-supplied.
Composite stiffness assumes perfect bonding and common axial strain/curvature.
"""

from collections.abc import Sequence
from dataclasses import dataclass
import math
from typing import Literal

from digitalmodel.subsea.cross_sections.schema import RadialLayer


@dataclass(frozen=True)
class AnnularLayerProperties:
    """Area in m² and dry mass per axial length in kg/m."""

    area_m2: float
    mass_per_length_kg_m: float


@dataclass(frozen=True)
class RadialSectionProperties:
    """Dry material mass and downward-positive submerged weight per length."""

    layers: tuple[AnnularLayerProperties, ...]
    outer_diameter_m: float
    mass_per_length_kg_m: float
    displaced_area_m2: float
    submerged_weight_n_m: float


@dataclass(frozen=True)
class ElasticSectionProperties:
    """Geometric section properties and perfectly bonded linear stiffness."""

    area_m2: float
    second_moment_m4: float
    section_modulus_m3: float
    axial_stiffness_n: float
    bending_stiffness_n_m2: float


def _diameters(layer: RadialLayer) -> tuple[float, float]:
    # Nested models permit assignment; recheck their current public input data.
    layer = RadialLayer.model_validate(layer.model_dump())
    dimensions = (layer.inner_diameter, layer.outer_diameter, layer.thickness)
    if any(value is not None and value.unit != "m" for value in dimensions):
        raise ValueError("all radial dimensions must use metres for this calculation")
    inner = layer.derived_inner_diameter
    outer = layer.derived_outer_diameter
    if (inner is None or outer is None
            or not all(math.isfinite(v.value) for v in (inner, outer))
            or not 0 <= inner.value < outer.value):
        raise ValueError("diameters must satisfy finite 0 <= inner < outer")
    return inner.value, outer.value


def _positive(value: float, name: str) -> None:
    if not math.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")


def _stack_diameters(layers: Sequence[RadialLayer]) -> list[tuple[float, float]]:
    if not layers:
        raise ValueError("at least one radial layer is required")
    diameters = [_diameters(layer) for layer in layers]
    for previous, current in zip(diameters, diameters[1:]):
        # Exact shared boundaries in metres; no physical gap tolerance is assumed.
        if previous[1] != current[0]:
            raise ValueError("radial layers must be contiguous and ordered without gaps or overlaps")
    return diameters


def calculate_annular_layer(layer: RadialLayer) -> AnnularLayerProperties:
    """Calculate a uniform concentric layer from metre-valued dimensions.

    All supplied dimensions must be in metres, including any thickness used to
    derive a missing diameter. Missing density raises rather than assuming a
    material property. Zero bore denotes a solid circular layer.
    """
    if layer.density is None:
        raise ValueError("density is required to calculate mass per length")
    inner, outer = _diameters(layer)
    # OpenStax, Prealgebra 2e (2020), §9.5: A=pi*r² and d=2r.
    # Subtract concentric circle areas: pi*(OD-ID)*(OD+ID)/4.
    # https://openstax.org/books/prealgebra-2e/pages/9-5-solve-geometry-applications-circles-and-irregular-figures
    area = math.pi * (outer - inner) * (outer + inner) / 4
    # OpenStax, University Physics Volume 1 (2016), §14.1, Eq. 14.1:
    # rho=m/V; for a uniform prismatic layer V=A*L, hence m/L=rho*A.
    # https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure
    mass_per_length = layer.density.value * area
    if not all(math.isfinite(value) and value > 0 for value in (area, mass_per_length)):
        raise ValueError("area and mass per length must be finite and positive")
    return AnnularLayerProperties(area_m2=area, mass_per_length_kg_m=mass_per_length)


def calculate_radial_section(
    layers: Sequence[RadialLayer], *, water_density_kg_m3: float,
    gravity_m_s2: float, bore_condition: Literal["sealed", "flooded"],
) -> RadialSectionProperties:
    """Sum contiguous material layers; require an explicit buoyancy convention.

    Sealed means an empty sealed bore: displace the full outer envelope, with
    no bore contents mass. Flooded means a freely flooded bore: only material
    displaces water. Contents mass, trapped gas pressure, water uptake and end
    effects are excluded. Negative submerged weight denotes net upward force.
    """
    _positive(water_density_kg_m3, "water density")
    _positive(gravity_m_s2, "gravity")
    if bore_condition not in {"sealed", "flooded"}:
        raise ValueError("bore_condition must be sealed or flooded")
    diameters = _stack_diameters(layers)
    properties = tuple(calculate_annular_layer(layer) for layer in layers)
    mass = sum(item.mass_per_length_kg_m for item in properties)
    outer = diameters[-1][1]
    # OpenStax University Physics Volume 1 (2016), §14.4, Archimedes' principle:
    # F_B = weight of displaced fluid; per length F_B/L = rho_water*g*A_displaced.
    # https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy
    displaced = (math.pi * outer * outer / 4 if bore_condition == "sealed"
                 else sum(item.area_m2 for item in properties))
    weight = (mass - water_density_kg_m3 * displaced) * gravity_m_s2
    _positive(mass, "total mass per length")
    _positive(displaced, "displaced area")
    if not math.isfinite(weight):
        raise ValueError("submerged weight must be finite")
    return RadialSectionProperties(properties, outer, mass, displaced, weight)


def calculate_elastic_section(
    layers: Sequence[RadialLayer], *, youngs_moduli_pa: Sequence[float],
) -> ElasticSectionProperties:
    """Calculate centroidal I, geometric Z, EA and EI; require every layer's E.

    Density is unnecessary for stiffness. Z is a geometric property of the
    complete material section, not a composite stress or strength calculation.
    All layers are concentric and assumed bonded; slip and ovalisation are
    outside this small-strain, linear, common-curvature model.
    """
    diameters = _stack_diameters(layers)
    if len(youngs_moduli_pa) != len(layers):
        raise ValueError("one Young's modulus is required per layer")
    areas, moments = [], []
    for (inner, outer), modulus in zip(diameters, youngs_moduli_pa, strict=True):
        _positive(modulus, "Young's modulus")
        area = math.pi * (outer - inner) * (outer + inner) / 4
        # Engineering Statics: Open and Interactive, §10.2.4, Eq. 10.2.10,
        # undated public edition accessed 2026-10-22:
        # I_circle=pi*r^4/4. Subtraction gives I_annulus=pi*(OD^4-ID^4)/64.
        # https://engineeringstatics.org/MOI-common-shapes.html#MOI-circles
        # Factoring the fourth-power difference avoids thin-wall cancellation.
        moment = area * (outer * outer + inner * inner) / 16
        _positive(area, "area")
        _positive(moment, "second moment")
        areas.append(area)
        moments.append(moment)
    # OpenStax University Physics Volume 1 (2016), §12.3: sigma=E*epsilon.
    # Integrating N=int(sigma dA), M=int(y*sigma dA) with epsilon=epsilon0+k*y
    # gives EA=sum(E_i*A_i), EI=sum(E_i*I_i). Geometric Z=I/c, c=OD/2.
    # https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus
    result = ElasticSectionProperties(
        sum(areas), sum(moments), sum(moments) / (diameters[-1][1] / 2),
        sum(e * a for e, a in zip(youngs_moduli_pa, areas, strict=True)),
        sum(e * i for e, i in zip(youngs_moduli_pa, moments, strict=True)),
    )
    for name, value in vars(result).items():
        _positive(value, name)
    return result
