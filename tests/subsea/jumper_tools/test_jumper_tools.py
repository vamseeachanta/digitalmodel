# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations using synthetic inputs, all in SI units.

Geometry: Engineering Statics: Open and Interactive (2020), section 10.2.4.
Buoyancy: OpenStax University Physics Volume 1 (2016), section 14.4.
Catenary: Catenary Curve (2019), equations (1), (4), (9), (10).
Numerical tolerances below test floating-point arithmetic, not design acceptance.
"""

import math
import runpy
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.subsea.jumper_tools.jumper_section_properties import (
    SectionInputs,
    calculate_section_properties,
)
from digitalmodel.subsea.jumper_tools.jumper_catenary_config import (
    calculate_catenary_config,
)
from digitalmodel.subsea.jumper_tools.jumper_fe_model import (
    mesh_catenary,
    mesh_polyline,
)


@pytest.fixture
def section_inputs():
    """Synthetic values, deliberately simple; no material grade is implied."""
    return SectionInputs(
        outer_diameter_m=2.0,
        wall_thickness_m=0.5,
        coating_thickness_m=0.5,
        pipe_density_kg_m3=8.0,
        coating_density_kg_m3=2.0,
        contents_density_kg_m3=4.0,
        water_density_kg_m3=3.0,
        youngs_modulus_pa=100.0,
        gravity_m_s2=10.0,
    )


@pytest.mark.parametrize("coating", [0.0, 0.5, 1.0])
def test_section_hand_calculation(section_inputs, coating):
    """D=2, d=1: A=3*pi/4, I=15*pi/64, Z=I.

    Coated D=2+2c; coating area=pi*((1+c)^2-1).
    Pipe mass=6*pi; contents mass=pi; coating mass=2*A_coat.
    Displaced mass=3*pi*(1+c)^2; net weight=10*(mass-displacement).
    EA=75*pi, EI=1500*pi/64. These are direct area/volume integrals.
    """
    result = calculate_section_properties(
        replace(section_inputs, coating_thickness_m=coating)
    )
    coat_area = math.pi * ((1 + coating) ** 2 - 1)
    mass = 7 * math.pi + 2 * coat_area
    assert result.inner_diameter_m == 1.0
    assert result.pipe_area_m2 == pytest.approx(3 * math.pi / 4)
    assert result.bore_area_m2 == pytest.approx(math.pi / 4)
    assert result.coating_area_m2 == pytest.approx(coat_area)
    assert result.second_moment_m4 == pytest.approx(15 * math.pi / 64)
    assert result.section_modulus_m3 == pytest.approx(15 * math.pi / 64)
    assert result.dry_mass_kg_m == pytest.approx(6 * math.pi + 2 * coat_area)
    assert result.filled_mass_kg_m == pytest.approx(mass)
    assert result.submerged_weight_n_m == pytest.approx(
        10 * (mass - 3 * math.pi * (1 + coating) ** 2)
    )
    assert result.axial_stiffness_n == pytest.approx(75 * math.pi)
    assert result.bending_stiffness_n_m2 == pytest.approx(1500 * math.pi / 64)


def test_buoyant_section_preserves_negative_weight(section_inputs):
    """At water density 100, displaced mass exceeds filled mass."""
    result = calculate_section_properties(
        replace(section_inputs, water_density_kg_m3=100.0)
    )
    assert result.submerged_weight_n_m < 0


@pytest.mark.parametrize(
    "field,value",
    [
        ("outer_diameter_m", 0),
        ("outer_diameter_m", math.inf),
        ("wall_thickness_m", math.nan),
        ("wall_thickness_m", 1),
        ("coating_thickness_m", -1),
        ("pipe_density_kg_m3", 0),
        ("contents_density_kg_m3", -1),
        ("water_density_kg_m3", math.nan),
        ("youngs_modulus_pa", -1),
        ("gravity_m_s2", 0),
    ],
)
def test_invalid_section_inputs(section_inputs, field, value):
    with pytest.raises(ValueError):
        calculate_section_properties(replace(section_inputs, **{field: value}))


@pytest.mark.parametrize("rise", [0.0, 10.0, 30.0])
def test_catenary_hand_calculation(rise):
    """H=100, w=10 => a=10; length=50 (synthetic inputs).

    At rises 0,10,30: cosh(u)=1,2,4 and sinh(u)=0,sqrt(3),sqrt(15).
    Thus s=0,10*sqrt(3),10*sqrt(15); x=10*log(cosh(u)+sinh(u));
    V=10*s, T=100+10*rise; theta=atan(s/10) from horizontal.
    Remaining pipe lies flat, so touchdown x=50-s.
    """
    c = calculate_catenary_config(100.0, 10.0, rise, 50.0)
    cosine = 1 + rise / 10
    sine = math.sqrt(cosine * cosine - 1)
    suspended = 10 * sine
    assert c.parameter_m == 10
    assert c.suspended_length_m == pytest.approx(suspended)
    assert c.suspended_span_m == pytest.approx(10 * math.log(cosine + sine))
    assert c.grounded_length_m == pytest.approx(50 - suspended)
    assert c.top_tension_n == pytest.approx(100 + 10 * rise)
    assert c.top_vertical_force_n == pytest.approx(10 * suspended)
    assert c.hangoff_angle_from_horizontal_rad == pytest.approx(math.atan(sine))
    assert c.hangoff_angle_from_vertical_rad == pytest.approx(
        math.pi / 2 - math.atan(sine)
    )


@pytest.mark.parametrize(
    "args",
    [
        (0, 10, 10, 50),
        (100, 0, 10, 50),
        (100, -10, 10, 50),
        (100, 10, -1, 50),
        (100, 10, 10, 1),
        (100, 10, 10, math.inf),
        (math.nan, 10, 10, 50),
    ],
)
def test_invalid_catenary_inputs(args):
    with pytest.raises(ValueError):
        calculate_catenary_config(*args)


def test_nearly_flat_catenary_retains_span():
    """s^2=h*(h+2a) gives s approximately sqrt(20) for a=1e20,h=1e-19.

    Using acosh(1+h/a) directly would round its argument to one.
    """
    c = calculate_catenary_config(1e20, 1.0, 1e-19, 10.0)
    assert c.suspended_span_m == pytest.approx(math.sqrt(20))


@pytest.mark.parametrize("count", [1, 2, 4])
def test_polyline_mesh_hand_calculation(count):
    """Two perpendicular segments of lengths 3 and 4 give total length 7.

    Each segment has count elements; lengths are 3/count and 4/count.
    Shared corner appears once; connectivity uses consecutive one-based IDs.
    """
    mesh = mesh_polyline([(0, 0, 0), (3, 0, 0), (3, 4, 0)], [count, count])
    assert len(mesh.nodes_m) == 2 * count + 1
    assert mesh.nodes_m[count] == (3.0, 0.0, 0.0)
    assert mesh.nodes_m[-1] == (3.0, 4.0, 0.0)
    assert mesh.elements == tuple((i, i + 1) for i in range(1, 2 * count + 1))
    assert mesh.chord_lengths_m == pytest.approx(
        [3 / count] * count + [4 / count] * count
    )


def test_catenary_mesh_hand_calculation():
    """a=10,h=10,L=20: s=sqrt(300). Mid-arc s/2=sqrt(75).

    x_mid=10*asinh(sqrt(3)/2), z_mid=sqrt(175)-10;
    final x=20-sqrt(300)+10*log(2+sqrt(3)), z=10.
    """
    c = calculate_catenary_config(100, 10, 10, 20)
    mesh = mesh_catenary(c, 2)
    td = 20 - math.sqrt(300)
    assert mesh.nodes_m[0] == pytest.approx((td, 0, 0))
    assert mesh.nodes_m[1] == pytest.approx(
        (td + 10 * math.asinh(math.sqrt(3) / 2), 0, math.sqrt(175) - 10)
    )
    assert mesh.nodes_m[-1] == pytest.approx(
        (td + 10 * math.log(2 + math.sqrt(3)), 0, 10)
    )
    assert sum(mesh.chord_lengths_m) < c.suspended_length_m


@pytest.mark.parametrize("count", [0, -1, 1.5, True])
def test_invalid_mesh_counts(count):
    with pytest.raises(ValueError):
        mesh_polyline([(0, 0, 0), (1, 0, 0)], [count])


@pytest.mark.parametrize(
    "points,counts",
    [
        ([], []),
        ([(0, 0, 0)], []),
        ([(0, 0, 0), (0, 0, 0)], [1]),
        ([(0, 0), (1, 0, 0)], [1]),
        ([(0, 0, 0), (math.nan, 0, 0)], [1]),
        ([(0, 0, 0), (1, 0, 0)], []),
    ],
)
def test_invalid_mesh_geometry(points, counts):
    with pytest.raises(ValueError):
        mesh_polyline(points, counts)


def test_flat_catenary_cannot_be_meshed_as_suspended():
    with pytest.raises(ValueError):
        mesh_catenary(calculate_catenary_config(100, 10, 0, 20), 2)


def test_worked_example_matches_independent_hand_calculation():
    """S1-S2: w=27.5*pi, H=275*pi; C1: a=10, h=10 => T=550*pi.

    tan(theta)=sqrt(3) => theta=pi/3 from horizontal, pi/6 from vertical.
    The example returns incomplete-qualification statuses, not a design verdict.
    """
    root = Path(__file__).resolve().parents[3]
    example = runpy.run_path(str(root / "examples/subsea/jumper_tools/basic_usage.py"))
    result = example["worked_example"]()
    assert result["section"]["submerged_weight_n_m"] == pytest.approx(27.5 * math.pi)
    assert result["catenary"]["top_tension_n"] == pytest.approx(550 * math.pi)
    assert result["catenary"]["hangoff_angle_from_horizontal_rad"] == pytest.approx(
        math.pi / 3
    )
    assert result["catenary"]["hangoff_angle_from_vertical_rad"] == pytest.approx(
        math.pi / 6
    )
    assert result["spreadsheet_comparison"] == "NOT_EVALUATED"
    assert result["design_code_checks"] == "NOT_EVALUATED"
