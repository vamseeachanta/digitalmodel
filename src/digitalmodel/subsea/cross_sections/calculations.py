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

from pydantic import BaseModel, ConfigDict, Field

from digitalmodel.subsea.cross_sections.schema import Provenance, RadialLayer


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
            raise ValueError(
                "radial layers must be contiguous and ordered without gaps or overlaps; "
                "thickness-derived boundaries must match neighbouring diameters bit-for-bit"
            )
    return diameters


def _annular_geometry(layer: RadialLayer) -> tuple[float, float, float]:
    """Return validated SI diameters and finite positive material area."""
    inner, outer = _diameters(layer)
    # OpenStax, Prealgebra 2e (2020), §9.5: A=pi*r² and d=2r.
    # Subtract concentric circle areas: pi*(OD-ID)*(OD+ID)/4.
    # https://openstax.org/books/prealgebra-2e/pages/9-5-solve-geometry-applications-circles-and-irregular-figures
    area = math.pi * (outer - inner) * (outer + inner) / 4
    _positive(area, "area")
    return inner, outer, area


def calculate_annular_layer(layer: RadialLayer) -> AnnularLayerProperties:
    """Calculate a uniform concentric layer from metre-valued dimensions.

    All supplied dimensions must be in metres, including any thickness used to
    derive a missing diameter. Missing density raises rather than assuming a
    material property. Zero bore denotes a solid circular layer.
    """
    if layer.density is None:
        raise ValueError("density is required to calculate mass per length")
    _, _, area = _annular_geometry(layer)
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


class BondedAxialLayer(BaseModel):
    """Geometry paired with a caller-supplied isotropic Young's modulus.

    Geometry provenance is stored on ``layer``. Modulus provenance and the
    literal unit ``Pa`` are independently required; no material lookup or
    implicit unit conversion is performed. Density is unnecessary.
    """

    model_config = ConfigDict(extra="forbid")

    layer: RadialLayer
    youngs_modulus_pa: float = Field(gt=0, allow_inf_nan=False)
    modulus_unit: Literal["Pa"]
    modulus_provenance: Provenance


@dataclass(frozen=True)
class BondedAxialProperties:
    """Axial rigidity EA in N, material area in m² and equivalent modulus in Pa.

    ``equivalent_modulus_pa`` is sum(E_i*A_i)/sum(A_i), excluding the bore.
    ``axial_stiffness_n`` is force per unit axial strain, not EA/L in N/m.
    """

    axial_stiffness_n: float
    area_m2: float
    equivalent_modulus_pa: float


def bonded_axial_stiffness(layers: Sequence[BondedAxialLayer]) -> BondedAxialProperties:
    """Calculate elementary equal-strain axial rigidity of fully bonded layers.

    Layers are concentric, isotropic, homogeneous and linearly elastic under
    uniform axial strain. Sort complete geometry/modulus pairs by inner
    diameter; neighbouring diameters must match exactly in metres. Gaps and
    overlaps are rejected without a geometric tolerance. A bore or solid core
    is allowed. At least two independent dimensions and geometry provenance
    are required per layer, as enforced by RadialLayer. Every modulus must be
    finite, positive, explicitly in Pa and supplied with its own provenance.

    From stress = E*strain and force = stress*area (OpenStax, University
    Physics Volume 1, 2016, section 12.3), common strain and additive forces
    give EA = sum(E_i*A_i). This is an elementary parallel-load derivation:
    https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus

    Unbonded flexible pipes, anisotropy, bond strength, Poisson constraints,
    bending, failure and DNV-F202 qualification are outside this model.
    """
    if not layers:
        raise ValueError("at least one bonded layer is required")
    sections = []
    for item in layers:
        # Validate a snapshot: callers may have mutated nested schema models.
        item = BondedAxialLayer.model_validate(item.model_dump())
        inner, outer, area = _annular_geometry(item.layer)
        rigidity = item.youngs_modulus_pa * area
        if not math.isfinite(rigidity) or rigidity <= 0:
            raise ValueError("layer axial stiffness must be finite and positive")
        sections.append((inner, outer, area, rigidity))
    sections.sort(key=lambda section: section[0])
    for previous, current in zip(sections, sections[1:]):
        if current[0] != previous[1]:
            raise ValueError(
                "bonded layers must be contiguous without gaps or overlaps; "
                "thickness-derived boundaries must match neighbouring diameters bit-for-bit"
            )
    try:
        area = math.fsum(section[2] for section in sections)
        stiffness = math.fsum(section[3] for section in sections)
    except OverflowError as exc:
        raise ValueError("total area and axial stiffness must be finite and positive") from exc
    equivalent_modulus = stiffness / area
    if not all(
        math.isfinite(value) and value > 0
        for value in (area, stiffness, equivalent_modulus)
    ):
        raise ValueError("area, axial stiffness and modulus must be finite and positive")
    return BondedAxialProperties(stiffness, area, equivalent_modulus)


def calculate_elastic_section(
    layers: Sequence[BondedAxialLayer],
) -> ElasticSectionProperties:
    """Calculate centroidal I, geometric Z, EA and EI from documented moduli.

    Use the bonded axial input contract: each geometry/modulus pair requires
    an explicit Pa unit and separate modulus provenance. Complete pairs are
    sorted by inner diameter. Density is unnecessary. Z is geometric, not a
    composite stress or strength calculation. Slip and ovalisation are outside
    this small-strain, linear, common-curvature model.
    """
    axial = bonded_axial_stiffness(layers)
    # Revalidate snapshots, retaining each modulus with its own geometry.
    items = [BondedAxialLayer.model_validate(item.model_dump()) for item in layers]
    items.sort(key=lambda item: _diameters(item.layer)[0])
    moments, bending = [], []
    for item in items:
        inner, outer, area = _annular_geometry(item.layer)
        # Engineering Statics: Open and Interactive, §10.2.4, Eq. 10.2.10,
        # undated public edition accessed 2026-10-22:
        # I_circle=pi*r^4/4. Subtraction gives I_annulus=pi*(OD^4-ID^4)/64.
        # https://engineeringstatics.org/MOI-common-shapes.html#MOI-circles
        # Factoring the fourth-power difference avoids thin-wall cancellation.
        moment = area * (outer * outer + inner * inner) / 16
        _positive(moment, "second moment")
        moments.append(moment)
        bending.append(item.youngs_modulus_pa * moment)
    # OpenStax University Physics Volume 1 (2016), §12.3: sigma=E*epsilon.
    # Integrating M=int(y*sigma dA) with epsilon=epsilon0+k*y gives EI=sum(E_i*I_i).
    # Geometric Z=I/c, c=OD/2. EA comes directly from bonded_axial_stiffness.
    # https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus
    try:
        inertia = math.fsum(moments)
        rigidity = math.fsum(bending)
    except OverflowError as exc:
        raise ValueError("second moment and bending stiffness must be finite and positive") from exc
    result = ElasticSectionProperties(
        axial.area_m2, inertia, inertia / (_diameters(items[-1].layer)[1] / 2),
        axial.axial_stiffness_n, rigidity,
    )
    for name, value in vars(result).items():
        _positive(value, name)
    return result
