"""Illustrative example cases; expectations follow mechanics independently."""

from dataclasses import FrozenInstanceError, replace
import math
from types import SimpleNamespace

import pytest

from digitalmodel.drilling_riser.section import cross_sectional_area
from digitalmodel.marine_ops.installation.jumper_lift import compute_sling_stiffness
from digitalmodel.marine_ops.marine_analysis.catenary.utils import calculate_elongation
from digitalmodel.specialized.rigging.axial_response import (
    ElasticRope,
    EqualStrainInputs,
    SlingGeometry,
    TensilePoint,
    elastic_response,
    equal_strain_adjustment,
    incremental_rigidity,
    rigidity_from_force_strain,
    strain_from_extension,
    tensile_properties,
)


def test_existing_sling_stiffness_illustrative_example():
    # Independent neutral mass, fractional extension and local gravity.
    data = SimpleNamespace(sling_mbl_te=25.0, sling_elongation=0.04)
    assert compute_sling_stiffness(data)["sling_ea_kn"] == pytest.approx(
        25.0 * 9.81 / 0.04
    )


def test_existing_circular_area_illustrative_example():
    assert cross_sectional_area(0.08, 0.0) == pytest.approx(math.pi * 0.04**2)


def test_existing_elongation_illustrative_example():
    extension = calculate_elongation(24000.0, 8.0, 600000.0)
    assert extension == pytest.approx(0.32)
    assert elastic_response(ElasticRope(8.0, 600000.0, 24000.0)).extension_m == extension


def test_si_adapters_preserve_mechanics():
    assert strain_from_extension(8.0, 0.08) == pytest.approx(0.01)
    assert strain_from_extension(8.0, -0.08) == pytest.approx(-0.01)
    assert rigidity_from_force_strain(24000.0, 0.04) == pytest.approx(600000.0)


def test_rigidity_adapter_gravity_cancellation():
    assert rigidity_from_force_strain(9810.0, 0.01) == pytest.approx(981000.0, rel=1e-12)


def test_elastic_response_and_zero_load():
    rope = ElasticRope(length_m=8.0, axial_rigidity_n=600000.0, tension_n=24000.0)
    response = elastic_response(rope)
    assert response.line_stiffness_n_per_m == 75000.0
    assert response.strain_ratio == pytest.approx(0.04)
    assert response.extension_m == pytest.approx(0.32)
    unloaded = elastic_response(replace(rope, tension_n=0.0))
    assert unloaded.extension_m == unloaded.strain_ratio == 0.0


def test_corrected_tensile_point_uses_reference_length():
    point = TensilePoint(
        reference_length_m=8.0, extension_m=0.24, tension_n=24000.0,
        area_m2=0.002, residual_strain_ratio=0.01,
    )
    response = tensile_properties(point)
    # Elastic strain is total strain less independently supplied residual strain.
    expected_strain = 0.24 / 8.0 - 0.01
    assert response.measured_strain_ratio == pytest.approx(0.03)
    assert response.elastic_strain_ratio == pytest.approx(expected_strain)
    assert response.axial_rigidity_n == pytest.approx(24000.0 / expected_strain)
    assert response.modulus_pa == pytest.approx(24000.0 / expected_strain / 0.002)


@pytest.mark.parametrize("sign", [1.0, -1.0])
def test_incremental_rigidity_uses_supplied_gauge_length(sign):
    assert incremental_rigidity(sign * 6000.0, sign * 0.04, 8.0) == pytest.approx(
        6000.0 * 8.0 / 0.04
    )


@pytest.mark.parametrize("horizontal,lateral,vertical", [
    (6.0, 0.0, 8.0), (-6.0, 2.0, 8.0), (6.0, -2.0, 12.0),
    (0.0, 0.0, 8.0), (0.0, 0.0, 4.0),
])
def test_equal_strain_shortening_and_vertical_lengthening(horizontal, lateral, vertical):
    data = EqualStrainInputs(
        reference_vertical_m=8.0, vertical_displacement_m=0.4,
        leg=SlingGeometry(vertical_m=vertical, horizontal_m=horizontal, lateral_m=lateral),
    )
    result = equal_strain_adjustment(data)
    original = math.sqrt(vertical**2 + horizontal**2 + lateral**2)
    stretched = math.sqrt((vertical + 0.4)**2 + horizontal**2 + lateral**2)
    expected_reference_strain = 0.4 / 8.0
    expected_rest = stretched / (1.0 + expected_reference_strain)
    assert result.original_length_m == pytest.approx(original)
    assert result.displaced_length_m == pytest.approx(stretched)
    assert result.adjusted_length_m == pytest.approx(expected_rest)
    assert result.shortening_m == pytest.approx(original - expected_rest, abs=1e-14)
    assert result.adjusted_extension_m == pytest.approx(stretched - expected_rest)
    assert (stretched - result.adjusted_length_m) / result.adjusted_length_m == pytest.approx(
        expected_reference_strain
    )
    unadjusted_strain = (stretched - original) / original
    reference_rest = 8.0 + result.reference_added_length_m
    reference_extension = 0.4 - result.reference_added_length_m
    assert reference_extension / reference_rest == pytest.approx(unadjusted_strain)
    assert result.reference_extension_m == pytest.approx(reference_extension)
    # Equal strain gives equal tension only when EA values are equal.
    assert 600000.0 * result.adjusted_strain_ratio == pytest.approx(30000.0)
    assert 1200000.0 * result.adjusted_strain_ratio == pytest.approx(60000.0)


def test_equal_strain_literal_illustrative_example():
    result = equal_strain_adjustment(
        EqualStrainInputs(8.0, 0.4, SlingGeometry(8.0, 6.0, 0.0))
    )
    assert result.original_length_m == 10.0
    assert result.displaced_length_m == pytest.approx(10.32279, rel=1e-6)
    assert result.adjusted_length_m == pytest.approx(9.83123, abs=1e-5)
    assert result.shortening_m == pytest.approx(0.16877, abs=1e-5)
    assert result.reference_extension_m == pytest.approx(0.26267, abs=1e-5)
    assert result.reference_added_length_m == pytest.approx(0.13733, abs=1e-5)


def test_zero_displacement_has_no_adjustment():
    data = EqualStrainInputs(8.0, 0.0, SlingGeometry(8.0, 6.0, 2.0))
    result = equal_strain_adjustment(data)
    assert result.shortening_m == result.adjusted_extension_m == 0.0
    assert result.reference_added_length_m == result.reference_extension_m == 0.0
    assert result.adjusted_length_m == result.original_length_m


def test_small_displacement_keeps_extension_resolvable():
    result = equal_strain_adjustment(
        EqualStrainInputs(8.0, 1e-16, SlingGeometry(8.0, 6.0, 0.0))
    )
    # Differential identity dL/dz = V/L at zero displacement.
    assert result.unadjusted_extension_m / 1e-16 == pytest.approx(8.0 / 10.0)
    assert result.adjusted_strain_ratio == pytest.approx(1e-16 / 8.0, abs=0.0)


def test_inputs_are_frozen():
    point = TensilePoint(8.0, 0.24, 24000.0, 0.002, 0.01)
    with pytest.raises(FrozenInstanceError):
        point.tension_n = 1.0


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf"), True, "2"])
def test_invalid_scalar_types_or_nonfinite_rejected(bad):
    with pytest.raises((TypeError, ValueError)):
        ElasticRope(bad, 600000.0, 24000.0)
    with pytest.raises((TypeError, ValueError)):
        SlingGeometry(8.0, bad, 0.0)
    with pytest.raises((TypeError, ValueError)):
        rigidity_from_force_strain(24000.0, bad)


@pytest.mark.parametrize("length,rigidity,tension", [
    (0.0, 600000.0, 1.0), (-1.0, 600000.0, 1.0),
    (8.0, 0.0, 1.0), (8.0, -1.0, 1.0), (8.0, 600000.0, -1.0),
])
def test_invalid_elastic_domains(length, rigidity, tension):
    with pytest.raises(ValueError):
        ElasticRope(length, rigidity, tension)


@pytest.mark.parametrize("residual", [0.03, 0.04, -0.01])
def test_nonpositive_elastic_strain_or_negative_residual_rejected(residual):
    with pytest.raises(ValueError):
        tensile_properties(TensilePoint(8.0, 0.24, 24000.0, 0.002, residual))


@pytest.mark.parametrize("force,extension,length", [
    (1.0, 0.0, 8.0), (1.0, -0.1, 8.0), (0.0, 0.1, 8.0),
    (1.0, 0.1, 0.0), (float("nan"), 0.1, 8.0),
])
def test_degenerate_increment_rejected(force, extension, length):
    with pytest.raises(ValueError):
        incremental_rigidity(force, extension, length)


@pytest.mark.parametrize("data", [
    (0.0, 0.1, SlingGeometry(8.0, 6.0, 0.0)),
    (8.0, -0.1, SlingGeometry(8.0, 6.0, 0.0)),
    (8.0, 0.1, None),
])
def test_invalid_equal_strain_inputs(data):
    with pytest.raises((TypeError, ValueError)):
        EqualStrainInputs(*data)


def test_nonpositive_vertical_geometry_rejected():
    with pytest.raises(ValueError):
        SlingGeometry(0.0, 6.0, 2.0)


def test_nonrepresentable_results_rejected():
    with pytest.raises(ValueError):
        elastic_response(ElasticRope(1e308, 1e-308, 1e308))
    with pytest.raises(ValueError):
        equal_strain_adjustment(EqualStrainInputs(8.0, 1e308, SlingGeometry(1e308, 1.0, 0.0)))
    with pytest.raises(ValueError):
        rigidity_from_force_strain(1e308, 1e-308)
