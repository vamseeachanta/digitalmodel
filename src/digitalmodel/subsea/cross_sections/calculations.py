# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Radial-layer geometry, dry mass and fully bonded axial rigidity in SI units.

No strength, buoyancy, packed-component, or design-code acceptance is evaluated.
Density is caller-supplied; geometry and density provenance remain on the layer.
"""

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


def _annular_geometry(layer: RadialLayer) -> tuple[float, float, float]:
    """Return validated SI diameters and finite positive material area."""
    dimensions = (layer.inner_diameter, layer.outer_diameter, layer.thickness)
    if any(value is not None and value.unit != "m" for value in dimensions):
        raise ValueError("all radial dimensions must use metres for this calculation")
    inner = layer.derived_inner_diameter
    outer = layer.derived_outer_diameter
    if inner is None or outer is None or not 0 <= inner.value < outer.value:
        raise ValueError("diameters must satisfy 0 <= inner < outer")
    # OpenStax, Prealgebra 2e (2020), §9.5: A=pi*r² and d=2r.
    # Subtract concentric circle areas: pi*(OD-ID)*(OD+ID)/4.
    # https://openstax.org/books/prealgebra-2e/pages/9-5-solve-geometry-applications-circles-and-irregular-figures
    area = math.pi * (outer.value - inner.value) * (outer.value + inner.value) / 4
    if not math.isfinite(area) or area <= 0:
        raise ValueError("area must be finite and positive")
    return inner.value, outer.value, area


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


def bonded_axial_stiffness(layers: list[BondedAxialLayer]) -> BondedAxialProperties:
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
            raise ValueError("bonded layers must be contiguous without gaps or overlaps")
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
