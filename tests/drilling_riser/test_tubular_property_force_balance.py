"""Neutral regressions for existing tubular properties and force balance.

Every dimension, load and material value here is an illustrative example,
chosen independently of a legacy engineering workbook. No design is qualified.
The source names API-1111: clause not stated in source. Its title alone does
not establish wall-thickness sizing, an allowable, or standards compliance.

Method and units: A = pi*(ro**2-ri**2), I = pi*(ro**4-ri**4)/4,
J = 2I, EA = E*A, EI = E*I, GJ = E*J/(2*(1+nu)). SI section inputs
are metres and pascals; outputs are m², m⁴, N and N.m². Measured component
inputs use inches, feet, and thousand-pound mass equivalents; that existing
interface returns kg/m and kN-based stiffness. Its flooded equivalent diameter
satisfies rho*pi*(D**2-ID**2)/4 = dry_mass_per_m - wet_mass_per_m.
The component wet-weight key is kg/m; the service-weight API uses kN/m after
the caller converts specified mass equivalents to force. The scalar
tension helper subtracts the supplied service weight from the top tension.

Input gaps: the standard clause, material provenance, gravity, fluid density,
empirical weight factors and source load boundaries need caller evidence.
The existing measured path embeds rounded conversions. Strict expected
failures expose its exact-mass and stiffness-identity discrepancies and the
section helper's missing input guards; existing implementations are unchanged.
The geometry size checks and explicit length boundary are tested separately.

Limits and what is left: no native simulation, overpull/tensioner assembly,
auxiliary-line override, calculated-weight branch, soil constitutive model,
fatigue or wall-thickness assessment is qualified by these regressions.
"""

from dataclasses import FrozenInstanceError
from fractions import Fraction
import math
from types import SimpleNamespace

import pandas as pd
import pytest

from digitalmodel.common.units import length_in_metres
from digitalmodel.drilling_riser.section import (
    section_properties,
    section_properties_imperial,
)
from digitalmodel.drilling_riser.stackup import effective_tension
from digitalmodel.infrastructure.base_solvers.marine.typical_riser_stack_up_calculations import (
    TypicalRiserStackUpCalculations,
)
from digitalmodel.subsea.connectors.section_properties import PipeSection
from digitalmodel.subsea.vertical_riser.vertical_riser_components import VerticalRiser
from digitalmodel.units import Q_


@pytest.mark.parametrize("radius_m, wall_m", [(0.8, 0.2), (0.6, 0.05)])
def test_section_properties_against_integrated_annulus(radius_m, wall_m):
    """Illustrative example: integrate a ring about its centroidal axes."""
    mean_radius = radius_m - wall_m / 2
    # Integral of r dr dtheta gives 2*pi*mean_radius*wall.
    area = 2 * math.pi * mean_radius * wall_m
    # Integral of r**3*sin(theta)**2 dr dtheta, factored about the mean radius.
    inertia = math.pi * mean_radius * wall_m * (mean_radius**2 + wall_m**2 / 4)
    modulus_pa, poisson = 180e9, 0.2
    geometry = PipeSection(2 * radius_m, wall_m)
    result = section_properties(
        geometry.outer_diameter, geometry.inner_diameter, modulus_pa, poisson
    )
    expected = {
        "area": area,
        "moment_of_inertia": inertia,
        "polar_moment": 2 * inertia,
        "shear_modulus": modulus_pa / 2.4,
        "axial_stiffness": modulus_pa * area,
        "bending_stiffness": modulus_pa * inertia,
        "torsional_stiffness": modulus_pa * inertia / 1.2,
    }
    for field, value in expected.items():
        assert getattr(result, field) == pytest.approx(value, rel=1e-9, abs=1e-12)
    with pytest.raises(FrozenInstanceError):
        result.area = 0


def test_imperial_section_helper_unit_basis():
    """Illustrative example; the existing helper has a rounded ksi factor."""
    inch_m = float(Fraction(127, 5000))
    pound_kg = float(Fraction(45359237, 100000000))
    standard_gravity = float(Fraction(196133, 20000))
    ksi_pa = 1000 * pound_kg * standard_gravity / inch_m**2
    expected = section_properties(18 * inch_m, 16.5 * inch_m, 25000 * ksi_pa, 0.2)
    actual = section_properties_imperial(18, 16.5, 25000, 0.2)
    for field in vars(expected):
        assert getattr(actual, field) == pytest.approx(
            getattr(expected, field), rel=1e-9, abs=1e-12
        )


def test_exact_shared_unit_boundary_and_soil_resistance_dimensions():
    """Illustrative example: divide force per length by projected diameter."""
    assert Q_(1, "inch").to("m").magnitude == pytest.approx(
        float(Fraction(127, 5000)), rel=0, abs=1e-12
    )
    assert Q_(1, "lb").to("kg").magnitude == pytest.approx(
        float(Fraction(45359237, 100000000)), rel=0, abs=1e-12
    )
    resistance = (Q_(3, "N") / Q_(2, "m") / Q_(0.8, "m")).to("Pa")
    assert resistance.magnitude == pytest.approx(1.875, rel=0, abs=1e-12)
    # Exact rational boundary: 25 * 381/1250 metres, rounded once to float.
    assert length_in_metres(25, "ft") == float(Fraction(381, 50))
    assert length_in_metres(0, "m") == 0


def measured_component(dry_mass_klb=12.0, wet_mass_klb=8.0):
    """Prepare an illustrative example for the real measured-component API."""
    component = VerticalRiser({"Material": {
        # The grade key is required by the existing API. These strengths and
        # density are unused placeholders, not properties assigned to that grade.
        "Steel": {"E": 25e6, "Poissionsratio": 0.2, "Rho": 0.28,
                  "Grades": {"API 5L X80": {"SMYS": 50000, "SMUS": 80000}}},
        "SeaWater": {"Rho": 1000.0},
    }})
    component.fea_type = "Fatigue"
    component.auxiliary_lines_properties = {"Ai": 0.0}
    component.stack_up_table = pd.DataFrame({"Aux Lines Present": [0]})
    component.riser_joints = pd.DataFrame([{
        "Component": "Illustrative tube", "Pipe OD": 18.0, "Pipe WT": 0.75,
        "Weight Method": "measured", "Dry wt.": dry_mass_klb,
        "Wet wt.": wet_mass_klb, "Length": 25.0, "Drag Diameter": float("nan"),
        "Lateral Drag Coefficient, Fatigue, Cd": 1.1,
        "Axial Drag Coefficient, Fatigue, Cd": 0.1,
        "Lateral Added mass, Ca": 1.4, "Axial Added mass, Ca": 0.0,
    }])
    component.get_fea_properties(0)
    return component.pipe_properties[0]["section_properties"]["pipe"]


def test_equal_measured_masses_have_zero_flooded_displacement():
    """Algebraic zero-displacement limit; not a physically consistent steel joint."""
    result = measured_component(8.0, 8.0)
    assert result["OD"] == pytest.approx(
        Q_(16.5, "inch").to("m").magnitude, rel=1e-9, abs=1e-12
    )
    assert result["drag_diameter"] == result["OD"]
    assert result["MassPerUnitLength"] == result["wet_weight_per_unit_length"]


def test_measured_diameter_satisfies_flooded_displacement_identity():
    result = measured_component()
    displaced_mass_per_m = (
        1000 * math.pi * (result["OD"]**2 - result["ID"]**2) / 4
    )
    assert displaced_mass_per_m == pytest.approx(
        result["MassPerUnitLength"] - result["wet_weight_per_unit_length"],
        rel=1e-9, abs=1e-12,
    )


def test_measured_component_negative_radicand_raises_value_error():
    with pytest.raises(ValueError):
        measured_component(0.0, 100.0)


@pytest.mark.xfail(strict=True, raises=pytest.fail.Exception, reason="Engineering guard for wet mass above dry mass is absent")
def test_measured_component_mass_order_guard_gap():
    with pytest.raises(ValueError):
        measured_component(8.0, 8.01)


def test_existing_measured_conversion_discrepancy_magnitudes():
    """Pin the known legacy factors so unrelated mismatches remain failures."""
    result = measured_component()
    exact_mass = Q_(12000, "lb").to("kg").magnitude / Q_(25, "ft").to("m").magnitude
    # Factors below are read from the existing implementation under test.
    assert result["MassPerUnitLength"] / exact_mass == pytest.approx(
        0.4536 / Q_(1, "lb").to("kg").magnitude, rel=1e-12, abs=1e-12
    )
    expected_ratio = 6.894745 / Q_(1, "psi").to("kPa").magnitude
    for field, geometric_field in [("EA", "A"), ("EI", "I"), ("GJ", "J")]:
        modulus = result["E"] if field != "GJ" else result["E"] / 2.4
        assert result[field] / (modulus * result[geometric_field]) == pytest.approx(
            expected_ratio, rel=1e-12, abs=1e-12
        )


@pytest.mark.xfail(strict=True, raises=AssertionError, reason="Existing measured path uses a rounded mass conversion")
def test_measured_mass_against_exact_boundary():
    expected = Q_(12000, "lb").to("kg").magnitude / Q_(25, "ft").to("m").magnitude
    assert measured_component()["MassPerUnitLength"] == pytest.approx(
        expected, rel=1e-9, abs=1e-12
    )


@pytest.mark.parametrize("field, geometric_field", [("EA", "A"), ("EI", "I"), ("GJ", "J")])
@pytest.mark.xfail(strict=True, raises=AssertionError, reason="Existing component stiffness factor differs from its converted E")
def test_measured_stiffness_consistent_with_its_modulus(field, geometric_field):
    result = measured_component()
    modulus = result["E"] if field != "GJ" else result["E"] / 2.4
    assert result[field] == pytest.approx(
        modulus * result[geometric_field], rel=1e-9, abs=1e-12
    )


@pytest.mark.parametrize("bottom_m, top_m, expected_kn", [
    (2.0, 6.0, 20.0), (-6.0, -2.0, 12.0), (-2.0, 2.0, 16.0),
    (0.0, 4.0, 20.0), (-4.0, 0.0, 12.0),
])
def test_service_weight_dry_wet_and_waterline_limits(bottom_m, top_m, expected_kn):
    """Illustrative 4 m section: dry 5 kN/m and wet 3 kN/m; zero contents."""
    calculation = TypicalRiserStackUpCalculations(SimpleNamespace(
        stack_up_table=[None], water_depth=30.0
    ))
    calculation.stack_up_properties_df = pd.DataFrame([{
        "component_bottom_elevation_above_mudline": bottom_m + 30.0,
        "component_top_elevation_above_mudline": top_m + 30.0,
        "dry_weight_per_unit_length": 5.0, "wet_weight_per_unit_length": 3.0,
        "internal_fluid_weight_per_unit_length": 0.0,
        "internal_fluid_weight_in_seawater_per_unit_length": 0.0,
        "stack_length": 4.0, "stack_in_service_weight": 0.0,
    }])
    calculation.evaluate_stack_in_service_weights()
    assert calculation.stack_up_properties_df.loc[0, "stack_in_service_weight"] == expected_kn


def test_tension_segment_additivity_and_prescribed_boundary():
    first = effective_tension(140.0, 12.0, 1.0)
    assert effective_tension(first, 8.0, 1.0) == effective_tension(140.0, 20.0, 1.0)
    assert effective_tension(140.0, 20.0, 0.4) == 132.0
    # An additional prescribed boundary is a caller input, not a computed load.
    assert effective_tension(240.0, 20.0, 1.0) == 220.0
    assert effective_tension(140.0, 0.0, 1.0) == 140.0
    assert effective_tension(140.0, -20.0, 1.0) == 160.0
    assert effective_tension(20.0, 20.0, 1.0) == 0.0
    assert effective_tension(20.0, 40.0, 1.0) == -20.0


@pytest.mark.parametrize("od_m, wall_m", [(0, 0.05), (-1, 0.05), (1.6, 0), (1.6, -0.05), (1.6, 0.8), (1.6, 0.9)])
def test_geometry_boundary_rejects_invalid_tubes(od_m, wall_m):
    with pytest.raises(ValueError):
        PipeSection(od_m, wall_m)


@pytest.mark.parametrize("value, unit", [(-1, "m"), (float("nan"), "m"), (float("inf"), "ft"), (True, "m"), (1, "unknown")])
def test_separate_explicit_length_boundary_validation(value, unit):
    with pytest.raises(ValueError):
        length_in_metres(value, unit)


@pytest.mark.parametrize("od_m, id_m, modulus_pa, poisson", [
    (1.2, 1.6, 180e9, 0.2), (1.6, 1.2, -180e9, 0.2),
    (float("nan"), 1.2, 180e9, 0.2), (1.6, 1.2, 180e9, -1.2),
])
@pytest.mark.xfail(strict=True, raises=pytest.fail.Exception, reason="Engineering input guard is absent from the existing scalar section API")
def test_section_api_input_guard_gap(od_m, id_m, modulus_pa, poisson):
    with pytest.raises(ValueError):
        section_properties(od_m, id_m, modulus_pa, poisson)


@pytest.mark.xfail(strict=True, raises=pytest.fail.Exception, reason="Existing geometry size checks do not reject infinite diameter")
def test_geometry_finite_input_guard_gap():
    with pytest.raises(ValueError):
        PipeSection(float("inf"), 0.05)
