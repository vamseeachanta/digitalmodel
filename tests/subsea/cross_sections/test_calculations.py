"""Value checks for homogeneous annular layers; no design-code acceptance claim.

Sources: OpenStax Prealgebra 2e (2020), section 9.5, A = pi*r**2,
d = 2*r; University Physics Volume 1 (2016), section 14.1, rho = m/V.
https://openstax.org/books/prealgebra-2e/pages/9-5-solve-geometry-applications-circles-and-irregular-figures
https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure

All dimensions and densities below are synthetic arithmetic inputs, not material
properties or acceptance limits. Expected values are independent hand calculations.
"""

import pytest

from digitalmodel.subsea.cross_sections.calculations import calculate_annular_layer
from digitalmodel.subsea.cross_sections.schema import Provenance, RadialLayer, UnitValue


def make_layer(**dimensions):
    """Build a synthetic layer with explicit units and assumption provenance."""
    return RadialLayer(
        name="arithmetic annulus",
        role="test",
        material="unspecified",
        provenance=Provenance(
            source_id="hand-calculation", source_type="project_assumption"
        ),
        **{
            key: UnitValue(value=value, unit=unit)
            for key, (value, unit) in dimensions.items()
        },
    )


@pytest.mark.parametrize(
    "geometry",
    [
        {"inner_diameter": (2, "m"), "outer_diameter": (4, "m")},
        {"inner_diameter": (2, "m"), "thickness": (1, "m")},
        {"outer_diameter": (4, "m"), "thickness": (1, "m")},
    ],
)
def test_annular_area_and_mass_per_length(geometry):
    """R=2 m, r=1 m: A=(4-1)*pi=9.42477796076938 m².

    For a synthetic rho=10 kg/m³ and L=1 m, m/L=10*A
    =94.2477796076938 kg/m. Decimal references use pi to 16 digits;
    relative tolerance 1e-12 allows floating-point rounding only.
    """
    result = calculate_annular_layer(make_layer(**geometry, density=(10, "kg/m^3")))
    assert result.area_m2 == pytest.approx(9.42477796076938, rel=1e-12)
    assert result.mass_per_length_kg_m == pytest.approx(94.2477796076938, rel=1e-12)


def test_missing_density_is_not_silently_assumed():
    """Geometry alone cannot establish mass per length."""
    layer = make_layer(inner_diameter=(2, "m"), outer_diameter=(4, "m"))
    with pytest.raises(ValueError, match="density is required"):
        calculate_annular_layer(layer)


def test_non_si_geometry_requires_explicit_conversion():
    """Slice 1 accepts metres only; unit conversion is deferred."""
    layer = make_layer(
        inner_diameter=(2, "mm"), outer_diameter=(4, "mm"), density=(10, "kg/m^3")
    )
    with pytest.raises(ValueError, match="metres"):
        calculate_annular_layer(layer)


def test_negative_derived_bore_is_rejected():
    """OD=4 m and t=3 m implies ID=-2 m, not a physical annulus."""
    layer = make_layer(
        outer_diameter=(4, "m"), thickness=(3, "m"), density=(10, "kg/m^3")
    )
    with pytest.raises(ValueError, match="diameters"):
        calculate_annular_layer(layer)


def test_solid_layer_area_and_mass():
    """r=1 m, rho=10 kg/m³: A=pi m²; m/L=10*pi kg/m.

    Uses the circle and density equations cited in the module docstring;
    decimal references use pi to 16 digits with rounding tolerance 1e-12.
    """
    layer = make_layer(
        inner_diameter=(0, "m"), outer_diameter=(2, "m"), density=(10, "kg/m^3")
    )
    result = calculate_annular_layer(layer)
    assert result.area_m2 == pytest.approx(3.141592653589793, rel=1e-12)
    assert result.mass_per_length_kg_m == pytest.approx(31.41592653589793, rel=1e-12)


@pytest.mark.parametrize(
    "dimensions",
    [
        {"inner_diameter": (2, "m"), "thickness": (1, "inch")},
        {"inner_diameter": (2, "inch"), "outer_diameter": (4, "inch")},
    ],
)
def test_every_supplied_dimension_requires_metres(dimensions):
    """No calculation may mix or silently relabel length units."""
    with pytest.raises(ValueError, match="metres"):
        calculate_annular_layer(make_layer(**dimensions, density=(10, "kg/m^3")))


@pytest.mark.parametrize("scale", [1e200, 1e-200])
def test_unrepresentable_results_are_rejected(scale):
    """Extreme synthetic dimensions overflow/underflow binary64 area.

    A=(3*pi/4)*scale², outside the representable range for these scales.
    A zero or infinite result is not a calculated engineering value.
    """
    layer = make_layer(
        inner_diameter=(scale, "m"),
        outer_diameter=(2 * scale, "m"),
        density=(10, "kg/m^3"),
    )
    with pytest.raises(ValueError, match="finite and positive"):
        calculate_annular_layer(layer)
