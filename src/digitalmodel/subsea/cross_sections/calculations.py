"""Single homogeneous radial-layer geometry and dry mass in SI units.

No strength, buoyancy, packed-component, or design-code acceptance is evaluated.
Density is caller-supplied; geometry and density provenance remain on the layer.
"""

from dataclasses import dataclass
import math

from digitalmodel.subsea.cross_sections.schema import RadialLayer


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
    # OpenStax, University Physics Volume 1 (2016), §14.1, Eq. 14.1:
    # rho=m/V; for a uniform prismatic layer V=A*L, hence m/L=rho*A.
    # https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure
    mass_per_length = layer.density.value * area
    if not all(math.isfinite(value) and value > 0 for value in (area, mass_per_length)):
        raise ValueError("area and mass per length must be finite and positive")
    return AnnularLayerProperties(area_m2=area, mass_per_length_kg_m=mass_per_length)
