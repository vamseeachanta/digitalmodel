"""Illustrative examples and independent identities; no source data fixtures."""

from dataclasses import FrozenInstanceError, replace
from fractions import Fraction
import math

import pytest

from digitalmodel.hydrodynamics.buoyancy_tangential import (
    BuoyancySection,
    buoyancy_tangential_properties,
)


def example_section():
    """Illustrative example: coated diameter is half the buoyancy diameter."""
    return BuoyancySection(
        pipe_outer_diameter_m=0.4,
        insulation_thickness_m=0.05,
        buoyancy_outer_diameter_m=1.0,
        buoyancy_length_m=2.0,
    )


def test_dimensionless_identity_and_geometry_terms():
    result = buoyancy_tangential_properties(example_section())
    # Illustrative example: r=d/Db=1/2 and ell=L/Db=2. Independent
    # nondimensional expression: (2/3)*(1-r)^2 / ((1+2*r^2)*ell).
    assert result.added_mass_coefficient == pytest.approx(1.0 / 18.0)
    assert result.length_to_diameter_ratio == 2.0
    assert result.numerator_m3 == pytest.approx(1.0 / 24.0)
    assert result.denominator_m3 == pytest.approx(3.0 / 4.0)


@pytest.mark.parametrize("scale", [0.001, 8.0, 100.0])
def test_coefficient_is_invariant_to_uniform_length_scaling(scale):
    section = example_section()
    scaled = BuoyancySection(
        section.pipe_outer_diameter_m * scale,
        section.insulation_thickness_m * scale,
        section.buoyancy_outer_diameter_m * scale,
        section.buoyancy_length_m * scale,
    )
    base = buoyancy_tangential_properties(section)
    result = buoyancy_tangential_properties(scaled)
    assert result.added_mass_coefficient == pytest.approx(base.added_mass_coefficient)
    assert result.length_to_diameter_ratio == pytest.approx(base.length_to_diameter_ratio)
    assert result.numerator_m3 == pytest.approx(base.numerator_m3 * scale**3)
    assert result.denominator_m3 == pytest.approx(base.denominator_m3 * scale**3)


def test_doubling_length_halves_coefficient():
    section = example_section()
    result = buoyancy_tangential_properties(replace(section, buoyancy_length_m=4.0))
    assert result.added_mass_coefficient == pytest.approx(1.0 / 36.0)


def test_equal_coated_and_buoyancy_diameters_gives_zero():
    section = replace(example_section(), buoyancy_outer_diameter_m=0.5)
    result = buoyancy_tangential_properties(section)
    assert result.numerator_m3 == 0.0
    assert result.added_mass_coefficient == 0.0
    assert result.denominator_m3 > 0.0


def test_zero_insulation_is_supported():
    section = replace(example_section(), insulation_thickness_m=0.0)
    result = buoyancy_tangential_properties(section)
    r, ell = 0.4, 2.0
    expected = (2.0 / 3.0) * (1 - r)**2 / ((1 + 2*r*r) * ell)
    assert result.added_mass_coefficient == pytest.approx(expected)


def test_inputs_and_outputs_are_frozen():
    section = example_section()
    with pytest.raises(FrozenInstanceError):
        section.buoyancy_length_m = 4.0
    result = buoyancy_tangential_properties(section)
    with pytest.raises(FrozenInstanceError):
        result.added_mass_coefficient = 0.0


@pytest.mark.parametrize("field", list(BuoyancySection.__dataclass_fields__))
@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf"), True, "1", None])
def test_nonfinite_or_nonreal_dimensions_are_rejected(field, bad):
    with pytest.raises(ValueError, match=field):
        replace(example_section(), **{field: bad})


@pytest.mark.parametrize(
    "field,value",
    [
        ("pipe_outer_diameter_m", 0.0),
        ("pipe_outer_diameter_m", -0.1),
        ("insulation_thickness_m", -0.01),
        ("buoyancy_outer_diameter_m", 0.0),
        ("buoyancy_outer_diameter_m", -1.0),
        ("buoyancy_length_m", 0.0),
        ("buoyancy_length_m", -1.0),
    ],
)
def test_nonphysical_dimensions_are_rejected(field, value):
    with pytest.raises(ValueError, match=field):
        replace(example_section(), **{field: value})


def test_coated_diameter_cannot_exceed_buoyancy_diameter():
    with pytest.raises(ValueError, match="coated"):
        replace(example_section(), insulation_thickness_m=0.4)


@pytest.mark.parametrize("scale", [1e-200, 1e200])
def test_unrepresentable_geometry_terms_are_rejected(scale):
    with pytest.raises(ValueError, match="representable"):
        buoyancy_tangential_properties(BuoyancySection(scale, 0.0, 2*scale, scale))


def test_coated_diameter_overflow_is_rejected():
    with pytest.raises(ValueError, match="coated"):
        BuoyancySection(1e308, 1e308, 1e308, 1.0)


def test_unrepresentable_input_integer_is_rejected():
    with pytest.raises(ValueError, match="buoyancy_length_m"):
        replace(example_section(), buoyancy_length_m=10**1000)


@pytest.mark.parametrize(
    "field", ["pipe_outer_diameter_m", "insulation_thickness_m", "buoyancy_length_m"]
)
def test_positive_real_underflow_during_float_conversion_is_rejected(field):
    with pytest.raises(ValueError, match=field):
        replace(example_section(), **{field: Fraction(1, 10**400)})


def test_all_results_are_finite_for_illustrative_example():
    result = buoyancy_tangential_properties(example_section())
    assert all(math.isfinite(value) for value in vars(result).values())
