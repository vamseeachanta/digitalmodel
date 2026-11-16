"""Illustrative examples for existing tubular-component calculation methods.

Method and units
---------------
The drilling_riser.section functions compute circular annular area, diametral
and polar second moments, shear modulus and EA/EI/GJ. SI inputs are outer/inner
diameter in m, Young's modulus in Pa and dimensionless Poisson ratio. Area is
pi*(outer radius squared - inner radius squared); diametral second moment is
pi/4*(outer radius to the fourth - inner radius to the fourth); polar second
moment is twice the diametral value. Shear modulus is E/[2*(1+nu)]. Stiffness
is modulus times the applicable area or second moment: outputs are N and N*m2.
The imperial helper accepts inches/ksi and returns SI; the shared Q_ converter
supplies an independent dimensional boundary in the tests.

The existing measured model builder infers displaced diameter from linear
dry/submerged mass using annular displacement balance, including the flooded
bore area. Its input mass entries are interpreted as thousands of pounds and
water density as kg/m3, inferred from its formula. Returned linear mass is
kg/m despite weight-like keys. Legacy mass-per-length and EI use rounded
conversion factors; tests compare them with exact conversions at rel=2e-4.
Legacy EA/GJ and material-strength conversions are outside these neutral tests.
The direct SI section method's EA/EI/GJ are tested independently.

The existing effective_tension function is exercised as an arithmetic force
decrement with explicit net submerged force in kN; no pressure-corrected or
dynamic tension is reconstructed. Distributed-force conversion checks Q_ only,
not a soil-response law or a complete foundation-pressure adapter.

Source statement, limits and Input gaps
--------------------------------------
The originating calculation names API-1111; clause not stated in source.
Edition, applicability and standards compliance are not established here.
The scalar section functions lack domain checks. PipeSection supplies existing
diameter/thickness bounds, not finite/type validation. Modulus, Poisson ratio,
fluid density, gravity, measured mass, wet/dry assumptions and imposed forces
require caller provenance; no new calculation or default is introduced.
Imported solver arrays and supplied soil-response tables are not reconstructed.
Input allocation, plot preparation, the calculated-weight branch, auxiliary
lines, material strength and complete forward recalculation remain unverified.
All examples are independently selected illustrative examples; no private input
or response table is used. Numeric assertions retain rel=1e-9 and abs=1e-12,
except measured mass and EI conversions, which use rel=2e-4 for rounded factors.
"""

import math

import pandas as pd
import pytest

from digitalmodel.drilling_riser.section import (
    cross_sectional_area,
    moment_of_inertia,
    polar_moment_of_inertia,
    section_properties,
    section_properties_imperial,
    shear_modulus,
)
from digitalmodel.drilling_riser.stackup import effective_tension
from digitalmodel.units import Q_


def close_to(value):
    return pytest.approx(value, rel=1e-9, abs=1e-12)


@pytest.mark.parametrize("outer_m,inner_m", [(1.2, 1.0), (0.8, 0.6), (0.4, 0.0)])
def test_annular_geometry_from_radius_closed_forms(outer_m, inner_m):
    """Illustrative example: closed forms for circular area and inertia."""
    radius_outer, radius_inner = outer_m / 2.0, inner_m / 2.0
    area_m2 = math.pi * (radius_outer**2 - radius_inner**2)
    inertia_m4 = math.pi / 4.0 * (radius_outer**4 - radius_inner**4)
    assert cross_sectional_area(outer_m, inner_m) == close_to(area_m2)
    assert moment_of_inertia(outer_m, inner_m) == close_to(inertia_m4)
    assert polar_moment_of_inertia(outer_m, inner_m) == close_to(2.0 * inertia_m4)


def test_elastic_stiffness_from_illustrative_annulus():
    # Illustrative example: D=1.2 m, d=1 m gives A=0.11*pi,
    # I=0.016775*pi, and J=0.03355*pi. E=200 GPa, nu=0.25.
    result = section_properties(1.2, 1.0, 200e9, 0.25)
    assert result.area == close_to(0.11 * math.pi)
    assert result.moment_of_inertia == close_to(0.016775 * math.pi)
    assert result.polar_moment == close_to(0.03355 * math.pi)
    assert result.shear_modulus == close_to(80e9)
    assert result.axial_stiffness == close_to(22e9 * math.pi)
    assert result.bending_stiffness == close_to(3.355e9 * math.pi)
    assert result.torsional_stiffness == close_to(2.684e9 * math.pi)


def test_solid_circle_limit_and_zero_poisson_ratio():
    # Illustrative example: radius 0.2 m gives A=0.04*pi, I=0.0004*pi.
    result = section_properties(0.4, 0.0, 40e9, 0.0)
    assert result.area == close_to(0.04 * math.pi)
    assert result.moment_of_inertia == close_to(0.0004 * math.pi)
    assert result.torsional_stiffness == close_to(result.bending_stiffness)
    assert shear_modulus(40e9, 0.0) == close_to(20e9)


def test_diameter_scaling_has_second_and_fourth_powers():
    # Illustrative example: doubling all lengths quadruples A and gives 16*I.
    base = section_properties(0.8, 0.6, 180e9, 0.25)
    scaled = section_properties(1.6, 1.2, 180e9, 0.25)
    assert scaled.area == close_to(4.0 * base.area)
    assert scaled.bending_stiffness == close_to(16.0 * base.bending_stiffness)
    assert scaled.torsional_stiffness == close_to(16.0 * base.torsional_stiffness)


def test_imperial_section_helper_returns_si_properties():
    # Illustrative example: exact shared converter supplies the independent
    # boundary basis. The existing ksi factor has finite stored precision.
    outer_m = Q_(10.0, "in").to("m").magnitude
    inner_m = Q_(8.0, "in").to("m").magnitude
    modulus_pa = Q_(25_000.0, "ksi").to("Pa").magnitude
    area_m2 = math.pi * ((outer_m / 2.0)**2 - (inner_m / 2.0)**2)
    inertia_m4 = math.pi / 4.0 * ((outer_m / 2.0)**4 - (inner_m / 2.0)**4)
    result = section_properties_imperial(10.0, 8.0, 25_000.0, 0.25)
    assert result.area == close_to(area_m2)
    assert result.moment_of_inertia == close_to(inertia_m4)
    assert result.axial_stiffness == close_to(modulus_pa * area_m2)
    assert result.bending_stiffness == close_to(modulus_pa * inertia_m4)
    assert result.torsional_stiffness == close_to(modulus_pa / 2.5 * 2 * inertia_m4)


@pytest.mark.parametrize("outer_m,thickness_m", [
    (0.0, 0.1), (-1.2, 0.1), (1.2, 0.0), (1.2, -0.1),
    (1.2, 0.6), (1.2, 0.7),
])
def test_existing_pipe_section_rejects_out_of_range_geometry(outer_m, thickness_m):
    from digitalmodel.subsea.connectors.section_properties import PipeSection

    with pytest.raises(ValueError):
        PipeSection(outer_m, thickness_m)


def test_existing_pipe_section_derives_bore_from_wall():
    from digitalmodel.subsea.connectors.section_properties import PipeSection

    # Illustrative example: diameter 1.2 m, wall 0.1 m, bore 1.0 m.
    pipe = PipeSection(1.2, 0.1)
    assert pipe.inner_diameter == close_to(1.0)
    assert pipe.area == close_to(0.11 * math.pi)


def measured_properties(*, dry_klb, wet_klb, length_ft, bore_in, water_kg_m3):
    """Exercise the existing measured branch with an illustrative one-row model.

    The normal constructor is used. Attributes ordinarily set by the input
    reader are supplied explicitly; no input-reader, solver or plotting method
    is called. Steel density and strength fields are unused placeholders here.
    Each call uses a fresh mapping because the existing sizing code mutates it.
    """
    from digitalmodel.subsea.vertical_riser.vertical_riser_components import VerticalRiser

    model = VerticalRiser({"Material": {
        "Steel": {"E": 28e6, "Poissionsratio": 0.25, "Rho": 0.28,
                  # Key must match the grade name hardcoded in get_fea_properties; strength values are unused placeholders.
                  "Grades": {"API 5L X80": {"SMYS": 60_000., "SMUS": 80_000.}}},
        "SeaWater": {"Rho": water_kg_m3},
    }})
    model.fea_type = "Fatigue"
    model.riser_joints = pd.DataFrame([{
        "Pipe OD": bore_in + 1.0, "Pipe WT": 0.5, "Length": length_ft,
        "Dry wt.": dry_klb, "Wet wt.": wet_klb, "Weight Method": "measured",
        "Drag Diameter": float("nan"), "Component": "illustrative example",
        "Lateral Drag Coefficient, Fatigue, Cd": 1.0,
        "Axial Drag Coefficient, Fatigue, Cd": 0.0,
        "Lateral Added mass, Ca": 1.0, "Axial Added mass, Ca": 0.0,
    }])
    model.stack_up_table = pd.DataFrame(columns=["Aux Lines Present"])
    model.auxiliary_lines_properties = {"Ai": 0.0}
    model.get_fea_properties(0)
    return model.pipe_properties[-1]["section_properties"]["pipe"]


@pytest.mark.parametrize("bore_in,wet_klb", [(15.0, 2.0), (15.0, 0.0), (15.0, -1.0), (0.0, 2.0)])
def test_measured_buoyancy_closes_displaced_annulus_mass_balance(bore_in, wet_klb):
    # Illustrative example: 3 klb dry mass, 12 ft joint, 15 in flooded bore.
    result = measured_properties(dry_klb=3.0, wet_klb=wet_klb, length_ft=12.0,
                                 bore_in=bore_in, water_kg_m3=1000.0)
    bore_m = Q_(bore_in, "in").to("m").magnitude
    displaced_area_m2 = math.pi * ((result["buoyancy_diameter"] / 2.0)**2
                                  - (bore_m / 2.0)**2)
    assert displaced_area_m2 * 1000.0 == close_to(
        result["MassPerUnitLength"] - result["wet_weight_per_unit_length"])
    assert result["ID"] == close_to(bore_m)


def test_zero_mass_flooded_component_has_buoyancy_diameter_equal_to_bore():
    result = measured_properties(dry_klb=0.0, wet_klb=0.0, length_ft=12.0,
                                 bore_in=15.0, water_kg_m3=1000.0)
    assert result["MassPerUnitLength"] == 0.0
    assert result["wet_weight_per_unit_length"] == 0.0
    assert result["buoyancy_diameter"] == close_to(Q_(15.0, "in").to("m").magnitude)


def test_measured_mass_matches_exact_conversion():
    result = measured_properties(dry_klb=3.0, wet_klb=2.0, length_ft=12.0,
                                 bore_in=15.0, water_kg_m3=1000.0)
    exact_kg_m = (Q_(3000.0, "lb") / Q_(12.0, "ft")).to("kg/m").magnitude
    # The existing module uses the rounded lb->kg factor 0.4536; rel=2e-4 covers it.
    assert result["MassPerUnitLength"] == pytest.approx(exact_kg_m, rel=2e-4)


def test_measured_stiffness_matches_exact_conversion():
    result = measured_properties(dry_klb=3.0, wet_klb=2.0, length_ft=12.0,
                                 bore_in=15.0, water_kg_m3=1000.0)
    # Illustrative example: geometry in inch powers and E in psi; EI is in kN*m2.
    inertia_in4 = math.pi / 4.0 * (8.0**4 - 7.5**4)
    exact_kn_m2 = Q_(28e6, "psi").to("kPa").magnitude * inertia_in4 * 0.0254**4
    # The existing module uses the rounded psi->kPa factor 6.894745; rel=2e-4 covers it.
    assert result["EI"] == pytest.approx(exact_kn_m2, rel=2e-4)


@pytest.mark.parametrize("net_submerged_kn,bottom_kn", [(60.0, 340.0), (-60.0, 460.0), (0.0, 400.0)])
def test_existing_end_force_decrement_with_explicit_submerged_basis(net_submerged_kn, bottom_kn):
    # Illustrative example: 400 kN top force, selected net submerged weight.
    # This is end-force bookkeeping; no pressure-corrected tension is inferred.
    assert effective_tension(400.0, net_submerged_kn, 0.0) == close_to(400.0)
    assert effective_tension(400.0, net_submerged_kn, 1.0) == close_to(bottom_kn)
    assert effective_tension(400.0, net_submerged_kn, 0.5) == close_to((400.0 + bottom_kn) / 2.0)


def test_existing_exact_converter_preserves_distributed_force_dimensions():
    # Illustrative example: 100 lbf/in has 100*4.4482216152605/0.0254 N/m.
    # This checks the existing converter only; no p-y constitutive law is added.
    result = Q_(100.0, "lbf/in").to("N/m").magnitude
    assert result == close_to(100.0 * 4.4482216152605 / 0.0254)
