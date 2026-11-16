"""Illustrative examples; expected values follow independent geometric identities."""

from dataclasses import FrozenInstanceError
import importlib
import math

import pytest

from digitalmodel.common.units import length_in_metres
from digitalmodel.subsea.flexible_pipes.cross_section import (
    FlexiblePipeSection, Layer, LayerType,
)


def methods():
    return importlib.import_module("digitalmodel.subsea.flexible_pipes.hose_properties")


def test_equivalent_diameter_preserves_volume():
    h = methods()
    # Illustrative example: unequal cylindrical pieces, lengths 2 and 7 m.
    segments = (h.HoseSegment(2.0, 0.8), h.HoseSegment(7.0, 0.3))
    diameter = h.equivalent_buoyancy_diameter_m(segments)
    assert diameter == pytest.approx(math.sqrt(1.91 / 9), rel=1e-12)
    volume = math.pi * ((0.8 / 2) ** 2 * 2 + (0.3 / 2) ** 2 * 7)
    assert math.pi * (diameter / 2) ** 2 * 9 == pytest.approx(volume, rel=1e-13)


def test_equivalent_diameter_subdivision_and_bounds():
    h = methods()
    original = (h.HoseSegment(7.0, 0.8), h.HoseSegment(2.0, 0.3))
    divided = (h.HoseSegment(3.0, 0.8), h.HoseSegment(4.0, 0.8), original[1])
    diameter = h.equivalent_buoyancy_diameter_m(original)
    assert 0.3 < diameter < 0.8
    assert h.equivalent_buoyancy_diameter_m(divided) == pytest.approx(diameter)
    assert h.equivalent_buoyancy_diameter_m((h.HoseSegment(7.0, 0.8),)) == pytest.approx(0.8, rel=1e-15)


def test_reference_property_homothetic_scaling_and_basis():
    h = methods()
    reference = h.AnnularSection(0.8, 0.3)
    target = h.AnnularSection(1.6, 0.6)
    # Illustrative example: doubling every radius gives A'=4A and I'=16I.
    force = h.ForceReference(70000.0, reference)
    rotation = h.RigidityReference(30000.0, reference)
    assert h.scale_force_property_n(force, target) == pytest.approx(280000.0)
    by_area = h.scale_rotational_rigidity_n_m2(rotation, target, basis="area")
    by_moment = h.scale_rotational_rigidity_n_m2(rotation, target, basis="second_moment")
    assert by_area == pytest.approx(120000.0)
    assert by_moment == pytest.approx(480000.0)
    assert by_area != by_moment
    assert h.scale_force_property_n(force, reference) == force.value_n


def test_existing_annulus_properties_against_hand_integrals():
    from digitalmodel.subsea.connectors.section_properties import PipeSection
    # Illustrative example: radii 0.4 and 0.15 m.
    section = PipeSection(0.8, 0.25)
    assert section.area == pytest.approx(math.pi * (0.4**2 - 0.15**2))
    assert section.moment_of_inertia == pytest.approx(math.pi * (0.4**4 - 0.15**4) / 4)


def test_existing_content_mass_and_length_conversion():
    # Illustrative example: density and geometry are independently chosen.
    section = FlexiblePipeSection([Layer(LayerType.OUTER_SHEATH, 0.3, 0.25, 800.0)])
    assert section.content_mass_per_length(1100.0) == pytest.approx(1100 * math.pi * 0.15**2)
    assert section.displaced_mass_per_length(1100.0) == pytest.approx(1100 * math.pi * 0.4**2)
    assert length_in_metres(7.0, "ft") == pytest.approx(7 * 3048 / 10000)
    with pytest.raises(ValueError):
        length_in_metres(7.0, "unknown")


def test_existing_anchor_coordinates_cardinal_direction():
    from digitalmodel.subsea.mooring_analysis.dynmoor import DynmoorLine, anchor_position
    line = DynmoorLine(1, 2.0, 7.0, 180.0, 0.0)
    assert anchor_position(line, 80.0) == pytest.approx((-78.0, 7.0))


def test_buoyancy_reserve_round_trip():
    h = methods()
    # Illustrative example: the circular envelope must displace 1.25*160 kg/m.
    inputs = h.BuoyancyRequirement(90.0, 70.0, 1100.0, 0.25)
    mass = h.required_displaced_mass_kg_m(inputs)
    diameter = h.required_buoyancy_diameter_m(inputs)
    assert diameter == pytest.approx(math.sqrt(800 / (1100 * math.pi)), rel=1e-12)
    assert mass == 200.0
    assert 1100 * math.pi * diameter**2 / 4 == pytest.approx(200.0, rel=1e-13)
    unreserved = h.BuoyancyRequirement(90.0, 70.0, 1100.0, 0.0)
    assert h.required_buoyancy_diameter_m(unreserved) < diameter


@pytest.mark.parametrize("values", [
    (90.0, 70.0, 1100.0, 0.25),
    (1e308, 0.0, 1100.0, 0.0),
])
def test_buoyancy_diameter_matches_existing_circular_geometry(values):
    from digitalmodel.drilling_riser.global_model.build import hydrodynamic_od_m
    h = methods()
    inputs = h.BuoyancyRequirement(*values)
    volume = h.required_displaced_mass_kg_m(inputs) / inputs.fluid_density_kg_m3
    expected = math.sqrt(volume * (4 / math.pi))
    assert hydrodynamic_od_m(volume) == pytest.approx(expected, rel=1e-12)
    assert h.required_buoyancy_diameter_m(inputs) == pytest.approx(hydrodynamic_od_m(volume))


def test_buoyancy_diameter_rejects_underflowed_volume():
    h = methods()
    inputs = h.BuoyancyRequirement(1e-200, 0.0, 1e200, 0.0)
    with pytest.raises(ValueError):
        h.required_buoyancy_diameter_m(inputs)


def test_buoyancy_diameter_rejects_unrepresentable_existing_result():
    h = methods()
    inputs = h.BuoyancyRequirement(1e308, 0.0, 1.0, 0.0)
    with pytest.raises(ValueError):
        h.required_buoyancy_diameter_m(inputs)


def test_zero_dry_or_content_mass_is_supported():
    h = methods()
    assert h.required_displaced_mass_kg_m(h.BuoyancyRequirement(0.0, 70.0, 1100.0, 0.0)) == 70.0
    assert h.required_displaced_mass_kg_m(h.BuoyancyRequirement(90.0, 0.0, 1100.0, 0.0)) == 90.0


def test_component_inverse_buoyancy_round_trip():
    h = methods()
    # Illustrative example: non-unit length catches total/linear mass confusion.
    outer, bore, length, density = 0.8, 0.3, 2.0, 1100.0
    displaced = density * math.pi * ((outer / 2) ** 2 - (bore / 2) ** 2) * length
    dry_mass = 1700.0
    component = h.ImmersedComponent(outer, length, dry_mass, dry_mass - displaced, density)
    assert h.equivalent_annular_bore_m(component) == pytest.approx(bore, rel=1e-13)
    doubled = h.ImmersedComponent(outer, 2 * length, 2 * dry_mass, 2 * (dry_mass - displaced), density)
    assert h.equivalent_annular_bore_m(doubled) == pytest.approx(bore, rel=1e-13)


@pytest.mark.parametrize("value", [-1.0, 0.0, math.nan, math.inf, -math.inf, True, "bad"])
def test_segment_rejects_invalid_geometry(value):
    h = methods()
    with pytest.raises(ValueError):
        h.HoseSegment(value, 0.8)
    with pytest.raises(ValueError):
        h.HoseSegment(7.0, value)


@pytest.mark.parametrize("outer,inner", [(0.3, 0.8), (0.8, 0.8), (0.8, 0.0), (math.inf, 0.3), (0.8, math.nan), (True, 0.3)])
def test_annulus_rejects_invalid_geometry(outer, inner):
    with pytest.raises(ValueError):
        methods().AnnularSection(outer, inner)


@pytest.mark.parametrize("value", [-1.0, 0.0, math.nan, math.inf, True, "bad"])
def test_property_reference_rejects_invalid_value(value):
    h = methods()
    section = h.AnnularSection(0.8, 0.3)
    for record in (h.ForceReference, h.RigidityReference):
        with pytest.raises(ValueError):
            record(value, section)


@pytest.mark.parametrize("index,value", [(0, -1.0), (1, -1.0), (2, 0.0), (2, math.inf), (3, -0.1), (3, math.nan), (3, True)])
def test_buoyancy_rejects_invalid_input(index, value):
    values = [90.0, 70.0, 1100.0, 0.25]
    values[index] = value
    with pytest.raises(ValueError):
        methods().BuoyancyRequirement(*values)


def test_invalid_aggregate_and_basis():
    h = methods()
    with pytest.raises(ValueError):
        h.equivalent_buoyancy_diameter_m(())
    with pytest.raises(ValueError):
        h.BuoyancyRequirement(0.0, 0.0, 1100.0, 0.0)
    section = h.AnnularSection(0.8, 0.3)
    ref = h.RigidityReference(30000.0, section)
    with pytest.raises(ValueError):
        h.scale_rotational_rigidity_n_m2(ref, section, basis="other")
    with pytest.raises(TypeError):
        h.scale_rotational_rigidity_n_m2(ref, section)


@pytest.mark.parametrize("values", [
    (0.8, 0.0, 1700.0, 900.0, 1100.0),
    (0.8, 2.0, 1700.0, 1700.0, 1100.0),
    (0.8, 2.0, 1700.0, -900.0, 1100.0),
    (0.8, 2.0, 1700.0, 1900.0, 1100.0),
    (0.8, 2.0, math.nan, 900.0, 1100.0),
    (0.8, 2.0, 1700.0, 900.0, 0.0),
])
def test_component_rejects_invalid_or_impossible_displacement(values):
    h = methods()
    with pytest.raises(ValueError):
        h.equivalent_annular_bore_m(h.ImmersedComponent(*values))


def test_input_records_are_frozen():
    segment = methods().HoseSegment(7.0, 0.8)
    with pytest.raises(FrozenInstanceError):
        segment.length_m = 2.0


def test_unrepresentable_calculations_raise():
    h = methods()
    with pytest.raises(ValueError):
        h.required_displaced_mass_kg_m(h.BuoyancyRequirement(1e308, 1e308, 1100.0, 0.25))
    with pytest.raises(ValueError):
        h.AnnularSection(1e100, 1e99)


def test_component_unrepresentable_intermediate_raises_value_error():
    h = methods()
    inputs = h.ImmersedComponent(0.8, 1e-200, 1700.0, 900.0, 1e-200)
    with pytest.raises(ValueError):
        h.equivalent_annular_bore_m(inputs)


def test_component_rejects_bore_rounded_to_outer_diameter():
    h = methods()
    inputs = h.ImmersedComponent(0.8, 2.0, 1700.0, 900.0, 1e200)
    with pytest.raises(ValueError, match="displacement cannot be resolved"):
        h.equivalent_annular_bore_m(inputs)


def test_scaling_rejects_malformed_records():
    h = methods()
    section = h.AnnularSection(0.8, 0.3)
    for record in (h.ForceReference, h.RigidityReference):
        with pytest.raises(ValueError):
            record(70000.0, None)
    with pytest.raises(ValueError):
        h.scale_force_property_n(h.ForceReference(70000.0, section), None)
    with pytest.raises(ValueError):
        h.scale_rotational_rigidity_n_m2(h.RigidityReference(30000.0, section), None, basis="area")


def test_equivalent_diameter_snapshots_iterable():
    h = methods()
    pieces = (h.HoseSegment(7.0, 0.8), h.HoseSegment(2.0, 0.3))
    assert h.equivalent_buoyancy_diameter_m(iter(pieces)) == pytest.approx(h.equivalent_buoyancy_diameter_m(pieces))


def test_remaining_unrepresentable_results_raise():
    h = methods()
    with pytest.raises(ValueError):
        h.equivalent_buoyancy_diameter_m((h.HoseSegment(7.0, 1e200),))
    section = h.AnnularSection(0.8, 0.3)
    larger = h.AnnularSection(1.6, 0.6)
    with pytest.raises(ValueError):
        h.scale_force_property_n(h.ForceReference(1e308, section), larger)
    with pytest.raises(ValueError):
        h.scale_rotational_rigidity_n_m2(h.RigidityReference(1e308, section), larger, basis="second_moment")
    with pytest.raises(ValueError):
        h.required_buoyancy_diameter_m(h.BuoyancyRequirement(1e308, 0.0, 0.25, 0.0))
