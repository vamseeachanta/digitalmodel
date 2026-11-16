"""Illustrative example regressions for existing riser property methods.

A legacy engineering workbook prepares tube geometry, stiffness, measured
mass per length and an equivalent displaced diameter. Existing methods are
exercised here; no engineering kernel is introduced.

Methods: A = integral dA, I = integral y**2 dA, J = 2I; EA, EI and GJ use
G = E/[2(1+nu)]. A measured submerged mass difference equals water density
times displaced annular area, with a flooded bore excluded from displacement.
Bottom tension equals top tension less weight.
Geometry uses metres or explicitly consistent inch/psi inputs in the legacy
sizing interface; the component interface returns m, kg/m, kN and kN.m**2.
All test values are independently chosen illustrative examples.

Standards: API-1111; clause not stated in source. These geometry identities
establish no wall-thickness design compliance. Input gaps: qualified material
properties, fluid density, gravity, wet/dry assumptions and external solver
provenance. No sourced design defaults are introduced. Supplied soil tables,
current profiles, solver runs and design qualification remain outside these
regressions. Strict expected failures expose existing rounded conversions;
the reused methods are unchanged. Raw geometry kernels have no physical-input
validation; validation tests below exercise existing unit/record boundaries.
"""

import ast
import importlib.util
import logging
import math
from pathlib import Path
from types import SimpleNamespace

import pandas as pd
import pytest

from digitalmodel.common.units import length_in_metres
from digitalmodel.drilling_riser.adapter import normalize_riser_component_record
from digitalmodel.drilling_riser.section import section_properties, section_properties_imperial
from digitalmodel.drilling_riser.stackup import effective_tension
from digitalmodel.specialized.custom.PipeSizing import PipeSizing
from digitalmodel.units import Q_


def assert_annulus(result, radius_outer, radius_inner, modulus, nu):
    # Independent polar-coordinate integrals of area and y**2 dA.
    area = math.pi * (radius_outer**2 - radius_inner**2)
    inertia = math.pi * (radius_outer**4 - radius_inner**4) / 4
    shear = modulus / (2 * (1 + nu))
    for field, expected in {
        "area": area, "moment_of_inertia": inertia, "polar_moment": 2 * inertia,
        "shear_modulus": shear, "axial_stiffness": modulus * area,
        "bending_stiffness": modulus * inertia,
        "torsional_stiffness": shear * 2 * inertia,
    }.items():
        assert getattr(result, field) == pytest.approx(expected, rel=1e-12, abs=1e-12)


@pytest.mark.parametrize("inner_radius", [0.0, 0.4, 0.59999])
def test_existing_sections_against_area_integrals(inner_radius):
    result = section_properties(1.2, 2 * inner_radius, 180e9, 0.25)
    assert_annulus(result, 0.6, inner_radius, 180e9, 0.25)


def test_elastic_scaling_and_poisson_dependence():
    base = section_properties(1.2, 0.8, 180e9, 0.25)
    doubled = section_properties(1.2, 0.8, 360e9, 0.25)
    changed_nu = section_properties(1.2, 0.8, 180e9, 0.0)
    for field in ("axial_stiffness", "bending_stiffness", "torsional_stiffness"):
        assert getattr(doubled, field) == pytest.approx(2 * getattr(base, field))
    assert changed_nu.axial_stiffness == base.axial_stiffness
    assert changed_nu.torsional_stiffness == pytest.approx(1.25 * base.torsional_stiffness)


def test_imperial_geometry_boundary():
    # Exact inch definition expressed independently as a rational metre value.
    inch_m = 127 / 5000
    result = section_properties_imperial(12.0, 10.0, 1.0, 0.25)
    assert result.area == pytest.approx(
        math.pi * ((6 * inch_m)**2 - (5 * inch_m)**2), rel=1e-12
    )
    assert result.moment_of_inertia == pytest.approx(
        math.pi * ((6 * inch_m)**4 - (5 * inch_m)**4) / 4, rel=1e-12
    )
    assert length_in_metres(10, "ft") == pytest.approx(10 * 381 / 1250, rel=1e-15)


@pytest.fixture
def component():
    # Execute the actual tracked class with its real calculation dependencies,
    # excluding imports for unrelated solver builders. No methods are rewritten.
    path = (Path(__file__).resolve().parents[2] / "src/digitalmodel/subsea/"
            "vertical_riser/vertical_riser_components.py")
    # tests/drilling_riser is two levels below the repository root.
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    definition = next(n for n in tree.body if isinstance(n, ast.ClassDef)
                      and n.name == "VerticalRiser")
    namespace = {"Q_": Q_, "PipeSizing": PipeSizing, "math": math, "logging": logging}
    exec(compile(ast.Module(body=[definition], type_ignores=[]), str(path), "exec"), namespace)
    # The grade label is required by the existing interface; the arbitrary
    # SMYS and SMUS numbers below are illustrative inputs and are not the
    # properties of the labelled grade.
    material = {"Steel": {"E": 20e6, "Rho": 0.27, "Poissionsratio": 0.25,
                          "Grades": {"API 5L X80": {"SMYS": 50000, "SMUS": 60000}}},
                "SeaWater": {"Rho": 1000}}  # Explicit density basis: kg/m**3.
    obj = namespace["VerticalRiser"]({"Material": material})
    obj.fea_type = "test"
    obj.auxiliary_lines_properties = {"Ai": 0.0}
    obj.stack_up_table = pd.DataFrame({"Aux Lines Present": []})
    obj.riser_joints = pd.DataFrame([{
        "Pipe OD": 12.0, "Pipe WT": 1.0, "Length": 10.0,
        "Dry wt.": 2.0, "Wet wt.": 1.0, "Weight Method": "measured",
        "Component": "illustrative example", "Drag Diameter": float("nan"),
        "Lateral Drag Coefficient, test, Cd": 1.0,
        "Axial Drag Coefficient, test, Cd": 0.0,
        "Lateral Added mass, Ca": 1.0, "Axial Added mass, Ca": 0.0,
    }])
    return obj


def test_existing_thickness_input_path(component):
    sizing = PipeSizing({
        "Outer_Pipe": {"Geometry": {"Nominal_OD": 12.0, "Nominal_ID": None,
                                    "Design_WT": 1.0},
                       "Material": {"Material": "Steel", "Material_Grade": "API 5L X80"}},
        "Inner_Pipe": None, "Material": component.cfg["Material"],
    })
    result = sizing.get_pipe_system_properties()["section_properties"]["pipe"]
    area = math.pi * (6**2 - 5**2)
    inertia = math.pi * (6**4 - 5**4) / 4
    assert result["geometry"]["Nominal_ID"] == 10.0
    for key, expected in {"A": area, "I": inertia, "J": 2 * inertia,
                          "EA": 20e6 * area, "EI": 20e6 * inertia,
                          "GJ": 20e6 / (2 * 1.25) * 2 * inertia}.items():
        assert result[key] == pytest.approx(expected, rel=1e-12)


@pytest.mark.parametrize("wet_kips", [1.0, 2.0])
def test_measured_buoyancy_satisfies_displacement_balance(component, wet_kips):
    component.riser_joints.loc[0, "Wet wt."] = wet_kips
    component.get_fea_properties(0)
    result = component.pipe_properties[0]["section_properties"]["pipe"]
    # Archimedes balance, including the zero net buoyancy limit.
    displaced_area = math.pi / 4 * (result["OD"]**2 - result["ID"]**2)
    mass_difference = result["MassPerUnitLength"] - result["wet_weight_per_unit_length"]
    assert displaced_area * 1000 == pytest.approx(mass_difference, rel=1e-12, abs=1e-12)
    if wet_kips == 2.0:
        assert result["OD"] == pytest.approx(result["ID"], rel=1e-12)


def test_measured_component_against_independent_hand_values(component):
    component.get_fea_properties(0)
    result = component.pipe_properties[0]["section_properties"]["pipe"]
    inch_m, foot_m = 127 / 5000, 381 / 1250
    # Pin the existing public method's four-decimal mass convention, while
    # the strict xfail below separately exposes its exact-definition error.
    rounded_pound_kg = round(45359237 / 100000000, 4)
    mass_dry = 2000 * rounded_pound_kg / (10 * foot_m)
    mass_wet = 1000 * rounded_pound_kg / (10 * foot_m)
    outer_radius, bore_radius = 6 * inch_m, 5 * inch_m
    area = math.pi * (outer_radius**2 - bore_radius**2)
    inertia = math.pi / 4 * (outer_radius**4 - bore_radius**4)
    displaced_area = (mass_dry - mass_wet) / 1000
    diameter = math.sqrt((2 * bore_radius)**2 + 4 * displaced_area / math.pi)
    # This coefficient belongs to the existing public conversion method;
    # it is a behavior being characterized, not a qualified stress constant.
    # The exact-definition stiffness comparison remains a strict xfail.
    legacy_stress_factor_kpa_per_psi = 6.894745
    modulus_kpa = 20e6 * legacy_stress_factor_kpa_per_psi
    expected = {
        "ID": 2 * bore_radius, "OD": diameter,
        "MassPerUnitLength": mass_dry, "wet_weight_per_unit_length": mass_wet,
        "A": area, "I": inertia, "J": 2 * inertia,
        "EA": modulus_kpa * area, "EI": modulus_kpa * inertia,
        "GJ": modulus_kpa / (2 * 1.25) * 2 * inertia,
    }
    for field, value in expected.items():
        assert result[field] == pytest.approx(value, rel=1e-12, abs=1e-12)


def test_existing_buoyancy_rejects_negative_square_root(component):
    component.riser_joints.loc[0, "Wet wt."] = 3.0
    # Characterize arithmetic-domain rejection, not full physical validation.
    with pytest.raises(ValueError):
        component.get_fea_properties(0)


@pytest.mark.xfail(strict=True, raises=AssertionError,
                   reason="Existing measured-mass conversion rounds the pound factor")
def test_measured_mass_against_exact_unit_definition(component):
    component.get_fea_properties(0)
    result = component.pipe_properties[0]["section_properties"]["pipe"]
    # Exact international pound and foot definitions, independently expressed.
    expected_kg_m = (2 * 1000 * (45359237 / 100000000)) / (10 * (381 / 1250))
    assert result["MassPerUnitLength"] == pytest.approx(expected_kg_m, rel=1e-12)


@pytest.mark.xfail(strict=True, raises=AssertionError,
                   reason="Existing component stiffness uses a rounded stress conversion")
def test_component_stiffness_against_force_area_units(component):
    component.get_fea_properties(0)
    result = component.pipe_properties[0]["section_properties"]["pipe"]
    inch_m = 127 / 5000
    # psi -> kPa from exact standard gravity, pound and inch definitions.
    psi_kpa = (45359237 / 100000000) * (980665 / 100000) / inch_m**2 / 1000
    inertia_in4 = math.pi * (6**4 - 5**4) / 4
    expected = 20e6 * inertia_in4 * psi_kpa * inch_m**4
    assert result["EI"] == pytest.approx(expected, rel=1e-12)


@pytest.mark.parametrize("depth_fraction,expected", [(0.0, 1200.0), (0.5, 1050.0), (1.0, 900.0)])
def test_effective_tension_existing_method(depth_fraction, expected):
    assert effective_tension(1200.0, 300.0, depth_fraction) == expected


def test_segment_tension_chain_against_cumulative_weight():
    # Illustrative example, with independent closed-form cumulative weights.
    weights = [100.0, 200.0, 50.0]
    tension = 1200.0
    for index, weight in enumerate(weights):
        tension = effective_tension(tension, weight, 1.0)
        assert tension == 1200.0 - sum(weights[:index + 1])


@pytest.fixture
def stack_class():
    # Load the complete actual module, avoiding package-level builder imports.
    path = (Path(__file__).resolve().parents[2] / "src/digitalmodel/infrastructure/"
            "base_solvers/marine/typical_riser_stack_up_calculations.py")
    spec = importlib.util.spec_from_file_location("stack_calculation_under_test", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.TypicalRiserStackUpCalculations


def test_existing_stack_joint_lengths_and_cumulative_positions(stack_class):
    model = SimpleNamespace(stack_up_table=pd.DataFrame({
        "Component": ["illustrative a", "illustrative b"],
        "No. Of. Joints": pd.Series([2, 3], dtype=object),
        "Component Length": [10.0, 5.0],
    }))
    calc = stack_class(model)
    calc.stack_up_properties_df = pd.DataFrame({
        "component": ["", ""], "stack_length": [0.0, 0.0],
        "component_bottom_elevation_from_end_A": [0.0, 0.0],
    })
    calc.populate_elevation_from_endA_in_df()
    foot_m = 381 / 1250
    assert calc.stack_up_properties_df["stack_length"].tolist() == pytest.approx(
        [2 * 10 * foot_m, 3 * 5 * foot_m], rel=1e-12
    )
    assert calc.stack_up_properties_df["component_bottom_elevation_from_end_A"].tolist() == pytest.approx(
        [20 * foot_m, 35 * foot_m], rel=1e-12
    )


def test_existing_stack_weights_across_water_surface_and_tension_chain(stack_class):
    # Illustrative example: dry, half-submerged and submerged segments.
    # Weight intensities in kN/m; elevations and lengths in m.
    model = SimpleNamespace(
        stack_up_table=pd.DataFrame({"Component": ["illustrative a", "illustrative c", "illustrative b"]}),
        water_depth=1000.0, riser_type="illustrative", support_spring=True,
        cfg={"stack_up": {"overpull": {"illustrative": {
            "bottom_of_component": "illustrative a", "value": 0.0,
        }}}},
    )
    calc = stack_class(model)
    calc.stack_up_properties_df = pd.DataFrame({
        "component_bottom_elevation_above_mudline": [1100.0, 900.0, 800.0],
        "component_top_elevation_above_mudline": [1200.0, 1100.0, 900.0],
        "stack_length": [100.0, 200.0, 100.0],
        "dry_weight_per_unit_length": [10.0] * 3,
        "wet_weight_per_unit_length": [6.0] * 3,
        "internal_fluid_weight_per_unit_length": [2.0] * 3,
        "internal_fluid_weight_in_seawater_per_unit_length": [1.0] * 3,
        "stack_in_service_weight": [0.0] * 3,
        "component_bottom_effective_tension": [0.0] * 3,
        "component_top_effective_tension": [0.0] * 3,
    })
    calc.evaluate_stack_in_service_weights()
    # Length-integrated intensities, including the surface split.
    expected_weights = [100 * (10 + 2), 100 * (6 + 1) + 100 * (10 + 2), 100 * (6 + 1)]
    assert calc.stack_up_properties_df["stack_in_service_weight"].tolist() == expected_weights
    calc.evaluate_effective_tension()
    # Zero overpull at the first component bottom is the closed-form limit.
    assert calc.stack_up_properties_df["component_bottom_effective_tension"].tolist() == [
        0.0, -expected_weights[1], -sum(expected_weights[1:]),
    ]


@pytest.mark.xfail(strict=True, raises=AssertionError,
                   reason="existing overpull conversion returns N against kN stack weights")
def test_existing_overpull_against_exact_lbf_to_kn_conversion(stack_class):
    # Illustrative overpull assumed in lbf; stack weights and tensions in kN.
    model = SimpleNamespace(
        stack_up_table=pd.DataFrame({"Component": ["illustrative a", "illustrative b"]}),
        riser_type="illustrative", support_spring=True,
        cfg={"stack_up": {"overpull": {"illustrative": {
            "bottom_of_component": "illustrative a", "value": 1000.0,
        }}}},
    )
    calc = stack_class(model)
    calc.stack_up_properties_df = pd.DataFrame({
        "stack_in_service_weight": [0.0, 0.0],
        "component_bottom_effective_tension": [0.0, 0.0],
        "component_top_effective_tension": [0.0, 0.0],
    })
    calc.evaluate_effective_tension()
    expected_kn = 1000 * 0.45359237 * 9.80665 / 1000
    assert calc.stack_up_properties_df.loc[0, "component_bottom_effective_tension"] == pytest.approx(
        expected_kn, rel=1e-12
    )


def test_exact_shared_unit_conversions():
    assert Q_(12, "inch").to("m").magnitude == pytest.approx(12 * 127 / 5000, rel=1e-15)
    assert Q_(2, "pound").to("kg").magnitude == pytest.approx(2 * 45359237 / 100000000, rel=1e-15)


@pytest.mark.parametrize("invalid", [True, "1", None, -1, float("nan"), float("inf"), 10**400])
def test_existing_length_boundary_rejects_invalid_input(invalid):
    with pytest.raises(ValueError):
        length_in_metres(invalid, "ft")


def test_existing_length_boundary_rejects_ambiguous_unit():
    with pytest.raises(ValueError, match="unit"):
        length_in_metres(1, "unknown")


@pytest.mark.parametrize("invalid", [True, None, "bad", -1, float("nan"), float("inf")])
def test_existing_record_boundary_omits_invalid_weight(invalid):
    record = normalize_riser_component_record({"COMPONENT_ID": "illustrative example",
                                               "WEIGHT_AIR_KIPS": invalid})
    assert "weight_air_kn" not in record
