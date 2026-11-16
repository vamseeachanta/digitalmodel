"""Illustrative example inputs; analytical identities independent of source data."""

from dataclasses import FrozenInstanceError, replace
from math import pi, sqrt

import numpy as np
import pytest

from digitalmodel.common.units import length_in_metres
from digitalmodel.drilling_riser.section import cross_sectional_area, moment_of_inertia
from digitalmodel.subsea.flexible_pipes.hose_properties import (
    DiameterSegment,
    HoseMassInput,
    HoseStiffness,
    equivalent_buoyancy_diameter_m,
    flange_stiffness_ratio,
    flange_transition_stiffness,
    hose_mass_properties,
)


def test_equivalent_diameter_preserves_displaced_volume():
    # Illustrative example: two cylinders, total volume = 3.5*pi m3.
    segments = (DiameterSegment(2.0, 0.5), DiameterSegment(6.0, 1.5))
    diameter = equivalent_buoyancy_diameter_m(segments)
    assert diameter == pytest.approx(sqrt(1.75), rel=1e-14)
    assert pi / 4 * diameter**2 * 8 == pytest.approx(3.5 * pi)


def test_equivalent_diameter_is_invariant_to_segmentation():
    whole = (DiameterSegment(8.0, 1.0),)
    split = (DiameterSegment(3.0, 1.0), DiameterSegment(5.0, 1.0))
    assert equivalent_buoyancy_diameter_m(whole) == 1.0
    assert equivalent_buoyancy_diameter_m(split) == 1.0


def test_diameter_segment_accepts_numpy_integer_inputs():
    segment = DiameterSegment(np.int64(2), np.int64(1))
    assert equivalent_buoyancy_diameter_m((segment,)) == 1.0


def test_equivalent_diameter_lies_between_extremes():
    segments = (DiameterSegment(1.0, 0.4), DiameterSegment(9.0, 1.2))
    result = equivalent_buoyancy_diameter_m(segments)
    assert 0.4 < result < 1.2


def test_empty_segments_rejected():
    with pytest.raises(ValueError, match="segment"):
        equivalent_buoyancy_diameter_m(())


@pytest.mark.parametrize("field", ["length_m", "buoyancy_diameter_m"])
@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf"), True, "2"])
def test_invalid_diameter_segment(field, value):
    args = {"length_m": 2.0, "buoyancy_diameter_m": 1.0, field: value}
    with pytest.raises(ValueError):
        DiameterSegment(**args)


def mass_input(**changes):
    # Illustrative example: supplied dry mass and two explicit fluid densities.
    args = dict(length_m=8.0, bore_diameter_m=0.4, buoyancy_diameter_m=0.9,
                dry_mass_kg=800.0, water_density_kg_m3=1000.0,
                contents_density_kg_m3=800.0)
    return HoseMassInput(**(args | changes))


def test_hose_mass_uses_supplied_dry_mass_and_distinct_fluid_densities():
    result = hose_mass_properties(mass_input())
    # Hand cylinders: contents = 256*pi kg, displaced water = 1620*pi kg.
    assert result.dry_mass_per_length_kg_m == 100.0
    assert result.contents_mass_kg == pytest.approx(256 * pi)
    assert result.displaced_mass_kg == pytest.approx(1620 * pi)
    assert result.submerged_mass_kg == pytest.approx(800 - 1364 * pi)
    assert result.empty_submerged_mass_kg == pytest.approx(800 - 1620 * pi)


def test_empty_hose_and_negative_effective_mass_are_supported():
    result = hose_mass_properties(mass_input(contents_density_kg_m3=0.0))
    assert result.contents_mass_kg == 0.0
    assert result.submerged_mass_kg == result.empty_submerged_mass_kg
    assert result.submerged_mass_kg < 0  # Buoyant; this is mass, not force.


def test_hose_mass_scales_with_length_when_dry_mass_scales():
    a = hose_mass_properties(mass_input())
    b = hose_mass_properties(mass_input(length_m=16.0, dry_mass_kg=1600.0))
    assert b.submerged_mass_kg == pytest.approx(2 * a.submerged_mass_kg)
    assert b.dry_mass_per_length_kg_m == a.dry_mass_per_length_kg_m


@pytest.mark.parametrize("field", ["length_m", "bore_diameter_m", "buoyancy_diameter_m",
                                  "dry_mass_kg", "water_density_kg_m3", "contents_density_kg_m3"])
@pytest.mark.parametrize("value", [-1, float("nan"), float("inf"), True, "1"])
def test_invalid_mass_inputs(field, value):
    with pytest.raises(ValueError):
        mass_input(**{field: value})


@pytest.mark.parametrize("changes", [dict(length_m=0), dict(bore_diameter_m=0),
                                     dict(buoyancy_diameter_m=0.4),
                                     dict(buoyancy_diameter_m=0.3),
                                     dict(water_density_kg_m3=0)])
def test_mass_geometry_and_zero_length_rejected(changes):
    with pytest.raises(ValueError):
        mass_input(**changes)


def test_inputs_are_frozen_and_fluid_density_is_required():
    inputs = mass_input()
    with pytest.raises(FrozenInstanceError):
        inputs.length_m = 1.0
    with pytest.raises(TypeError):
        HoseMassInput(length_m=8.0, bore_diameter_m=0.4, buoyancy_diameter_m=0.9,
                      dry_mass_kg=800.0, water_density_kg_m3=1000.0)
    with pytest.raises(FrozenInstanceError):
        DiameterSegment(2.0, 1.0).length_m = 3.0


def test_existing_area_and_inertia_solid_limit():
    # Illustrative example: radius one, hence A=pi and I=pi/4.
    assert cross_sectional_area(2.0, 0.0) == pytest.approx(pi)
    assert moment_of_inertia(2.0, 0.0) == pytest.approx(pi / 4)
    assert cross_sectional_area(2.0, 1.0) == pytest.approx(3 * pi / 4)


def test_existing_exact_international_foot_conversion():
    assert length_in_metres(25.0, "ft") == 7.62
    assert length_in_metres(7.62, "m") == 7.62
    with pytest.raises(ValueError):
        length_in_metres(-1.0, "ft")


def test_flange_ratio_and_transition_endpoints():
    # Illustrative example: radius ratio two with a common bore.
    ratio = flange_stiffness_ratio(2.0, 1.0, 0.5)
    assert ratio == pytest.approx(17.0)  # (16 - 1/16)/(1 - 1/16)
    base = HoseStiffness(1200.0, 5000.0, 800.0)
    start = flange_transition_stiffness(base, ratio, 0.0)
    end = flange_transition_stiffness(base, ratio, 1.0)
    mid = flange_transition_stiffness(base, ratio, 0.5)
    assert start == base
    assert end == HoseStiffness(20400.0, 85000.0, 13600.0)
    assert mid == HoseStiffness(10800.0, 45000.0, 7200.0)
    assert flange_stiffness_ratio(1.0, 1.0, 0.5) == 1.0


def test_flange_transition_third_points():
    base = HoseStiffness(1500.0, 6000.0, 900.0)
    first = flange_transition_stiffness(base, 4.0, 1 / 3)
    second = flange_transition_stiffness(base, 4.0, 2 / 3)
    assert first == HoseStiffness(3000.0, 12000.0, 1800.0)
    assert second == HoseStiffness(4500.0, 18000.0, 2700.0)


@pytest.mark.parametrize("args", [(1, 0.5, 0.5), (0.5, 1, 0.5),
                                  (2, 1, -1), (float("nan"), 1, 0.5),
                                  (True, 1, 0.5)])
def test_invalid_flange_geometry(args):
    with pytest.raises(ValueError):
        flange_stiffness_ratio(*args)


@pytest.mark.parametrize("fraction", [-0.1, 1.1, float("nan"), float("inf"), True])
def test_transition_does_not_extrapolate(fraction):
    with pytest.raises(ValueError):
        flange_transition_stiffness(HoseStiffness(1, 2, 3), 2.0, fraction)


@pytest.mark.parametrize("ratio", [0, -1, float("nan"), float("inf"), True])
def test_invalid_stiffness_ratio(ratio):
    with pytest.raises(ValueError):
        flange_transition_stiffness(HoseStiffness(1, 2, 3), ratio, 0.5)


@pytest.mark.parametrize("field", ["bending_stiffness_n_m2", "axial_stiffness_n",
                                  "torsional_stiffness_n_m2"])
@pytest.mark.parametrize("value", [-1, float("nan"), float("inf"), True])
def test_invalid_stiffness(field, value):
    with pytest.raises(ValueError):
        replace(HoseStiffness(1, 2, 3), **{field: value})
