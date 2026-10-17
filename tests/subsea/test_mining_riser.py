# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Analytical verification; all numbers are manufactured test inputs (basis S1).

Formula and numerical-tolerance provenance: docs/domains/risers/mining-lift-screening.md.
No experimental or commercial-solver result is represented by these fixtures.
"""

from dataclasses import replace
import json
from math import cos, pi, sin, sqrt, tan
from pathlib import Path

import numpy as np
import pytest
from scipy.optimize import brentq

from digitalmodel.subsea.mining_riser import (
    BundleSection,
    SlurryLayer,
    Tube,
    axial_modes,
    free_hanging_screen,
    slurry_density_profile,
)


def tube(**overrides):
    """S1: scaled artificial tube, with no engineering material implied."""
    values = dict(
        outer_diameter_m=2.0,
        wall_thickness_m=0.5,
        material_density_kg_m3=8.0,
        youngs_modulus_pa=800.0,
        contents_density_kg_m3=2.0,
        contents_inertia_fraction=1.0,
        carries_axial_load=True,
    )
    return Tube(**(values | overrides))


def section(**overrides):
    """S1: uniform, fully submerged artificial section."""
    values = dict(
        length_m=10.0,
        tubes=(tube(),),
        external_density_kg_m3=1.0,
        additional_mass_kg_m=0.0,
        additional_displaced_area_m2=0.0,
        axial_added_mass_kg_m=0.0,
    )
    return BundleSection(**(values | overrides))


def modes(sections, **overrides):
    values = dict(
        gravity_m_s2=10.0,
        lower_end_force_n=0.0,
        lower_end_mass_kg=0.0,
        elements_per_section=32,
        mode_count=3,
    )
    return axial_modes(sections, **(values | overrides))


def test_slurry_profile_uses_local_inventory_and_cumulative_coordinates():
    # R1 Eq. (6.3); S1 fractions represent local volume, never feed concentration.
    result = slurry_density_profile(
        [SlurryLayer(2.0, 1.0, 5.0, 0.0), SlurryLayer(3.0, 1.0, 5.0, 0.25)]
    )
    assert [(c.top_m, c.bottom_m) for c in result] == [(0.0, 2.0), (2.0, 5.0)]
    assert [c.density_kg_m3 for c in result] == [1.0, 2.0]
    assert (
        slurry_density_profile([SlurryLayer(1.0, 1.0, 5.0, 1.0)])[0].density_kg_m3
        == 5.0
    )


def test_tube_geometry_mass_and_axial_participation():
    main = tube()
    assert main.wall_area_m2 == pytest.approx(3 * pi / 4)
    assert main.bore_area_m2 == pytest.approx(pi / 4)
    assert main.displaced_area_m2 == pytest.approx(pi)
    assert main.mass_kg_m == pytest.approx(6.5 * pi)
    assert main.axial_stiffness_n == pytest.approx(600 * pi)
    auxiliary = tube(carries_axial_load=False, contents_inertia_fraction=0.0)
    bundle = section(tubes=(main, auxiliary))
    assert bundle.mass_kg_m == pytest.approx(13 * pi)
    assert bundle.axial_mass_kg_m == pytest.approx(12.5 * pi)
    assert bundle.axial_stiffness_n == pytest.approx(600 * pi)


def test_uniform_static_tension_and_exact_extension():
    # D2/D3: bottom force plus weight below; exact integral of linear tension.
    result = free_hanging_screen([section()], gravity_m_s2=10.0, lower_end_force_n=20.0)
    weight = 55 * pi
    assert result.coordinates_m == (0.0, 10.0)
    assert result.effective_tension_n == pytest.approx((20 + 10 * weight, 20))
    assert result.extension_m == pytest.approx(
        (20 * 10 + weight * 10**2 / 2) / (600 * pi)
    )
    assert not result.has_compression


def test_section_order_buoyancy_and_lower_force_are_explicit():
    top = section(length_m=2.0, additional_displaced_area_m2=pi)
    bottom = section(length_m=3.0, additional_mass_kg_m=pi)
    result = free_hanging_screen(
        [top, bottom], gravity_m_s2=10.0, lower_end_force_n=7.0
    )
    assert result.coordinates_m == (0.0, 2.0, 5.0)
    assert result.effective_tension_n == pytest.approx((7 + 285 * pi, 7 + 195 * pi, 7))


def test_internal_compression_is_detected_even_when_top_is_in_tension():
    lower = section(additional_displaced_area_m2=6 * pi)
    result = free_hanging_screen(
        [section(), lower], gravity_m_s2=10.0, lower_end_force_n=0.0
    )
    assert result.effective_tension_n[0] > 0
    assert result.has_compression
    with pytest.raises(ValueError, match="compression"):
        modes([section(), lower])


def test_modal_mesh_converges_to_fixed_free_bar_and_mode_shape():
    # D5: f_n=(2n-1)/(4L)*sqrt(EA/mu); S1 tolerances are numerical targets.
    s = section()
    exact = np.arange(1, 6, 2) / (4 * s.length_m) * sqrt(1200 / 13)
    coarse = modes([s], elements_per_section=8)
    fine = modes([s], elements_per_section=64)
    coarse_error = np.abs(np.array(coarse.frequencies_hz) / exact - 1)
    fine_error = np.abs(np.array(fine.frequencies_hz) / exact - 1)
    assert np.all(fine_error < coarse_error)
    assert fine.frequencies_hz == pytest.approx(exact, rel=0.001)
    assert fine.periods_s == pytest.approx(1 / np.array(fine.frequencies_hz))
    shape = np.array(fine.mode_shapes[0])
    assert shape[0] == 0.0
    assert shape == pytest.approx(
        np.sin(np.pi * np.array(fine.coordinates_m) / 20), abs=1e-9
    )


def test_lower_mass_matches_independent_continuum_characteristic_equation():
    # D6: cot(beta)=M*beta/(mu*L), with a synthetic mass ratio of one.
    s = section()
    mass = s.mass_kg_m * s.length_m
    beta = brentq(lambda b: 1 / tan(b) - b, 0.01, pi / 2 - 0.01)
    expected = beta / s.length_m * sqrt(1200 / 13) / (2 * pi)
    result = modes([s], lower_end_mass_kg=mass, elements_per_section=64, mode_count=1)
    assert result.frequencies_hz[0] == pytest.approx(expected, rel=0.0001)
    assert result.frequencies_hz[0] < modes([s]).frequencies_hz[0]


def test_auxiliary_mass_slows_modes_unless_it_also_transfers_axial_load():
    one = modes([section()]).frequencies_hz[0]
    sliding = modes(
        [section(tubes=(tube(), tube(carries_axial_load=False)))]
    ).frequencies_hz[0]
    latched = modes([section(tubes=(tube(), tube()))]).frequencies_hz[0]
    assert sliding == pytest.approx(one / sqrt(2))
    assert latched == pytest.approx(one)


def test_contents_inertia_is_independent_of_static_contents_weight():
    coupled = section()
    uncoupled = section(tubes=(tube(contents_inertia_fraction=0.0),))
    assert coupled.mass_kg_m == uncoupled.mass_kg_m
    assert modes([uncoupled]).frequencies_hz[0] > modes([coupled]).frequencies_hz[0]
    added = replace(coupled, axial_added_mass_kg_m=coupled.axial_mass_kg_m)
    assert modes([added]).frequencies_hz[0] == pytest.approx(
        modes([coupled]).frequencies_hz[0] / sqrt(2)
    )


def test_segment_split_preserves_static_integrals_and_modal_spectrum():
    s = section()
    split = [replace(s, length_m=5.0)] * 2
    a = free_hanging_screen([s], gravity_m_s2=10.0, lower_end_force_n=2.0)
    b = free_hanging_screen(split, gravity_m_s2=10.0, lower_end_force_n=2.0)
    assert a.extension_m == pytest.approx(b.extension_m)
    assert a.effective_tension_n[0] == pytest.approx(b.effective_tension_n[0])
    assert modes([s], elements_per_section=32).frequencies_hz == pytest.approx(
        modes(split, elements_per_section=16).frequencies_hz
    )


def test_nonuniform_sections_match_independent_interface_force_balance():
    # D7: analytical waves in each section, continuous displacement/axial force.
    top = section(length_m=2.0)
    bottom = section(
        length_m=3.0,
        tubes=(tube(youngs_modulus_pa=2 * tube().youngs_modulus_pa),),
    )
    travel_top = top.length_m * sqrt(top.axial_mass_kg_m / top.axial_stiffness_n)
    travel_bottom = bottom.length_m * sqrt(
        bottom.axial_mass_kg_m / bottom.axial_stiffness_n
    )
    impedance_ratio = sqrt(
        bottom.axial_stiffness_n
        * bottom.axial_mass_kg_m
        / (top.axial_stiffness_n * top.axial_mass_kg_m)
    )
    omega = brentq(
        lambda w: (
            cos(w * travel_top) * cos(w * travel_bottom)
            - impedance_ratio * sin(w * travel_top) * sin(w * travel_bottom)
        ),
        0.0,
        pi / (2 * max(travel_top, travel_bottom)),
    )
    result = modes([top, bottom], elements_per_section=64, mode_count=1)
    assert result.frequencies_hz[0] == pytest.approx(omega / (2 * pi), rel=0.0001)


@pytest.mark.parametrize(
    "field,value",
    [
        ("outer_diameter_m", 0),
        ("wall_thickness_m", 1.0),
        ("material_density_kg_m3", -1),
        ("youngs_modulus_pa", float("inf")),
        ("contents_density_kg_m3", float("nan")),
        ("contents_inertia_fraction", 1.01),
        ("carries_axial_load", "false"),
        ("outer_diameter_m", True),
    ],
)
def test_invalid_tube_rejected(field, value):
    with pytest.raises(ValueError):
        tube(**{field: value})


@pytest.mark.parametrize(
    "args",
    [
        (0, 1, 5, 0),
        (1, 0, 5, 0),
        (1, 1, -5, 0),
        (1, 1, 5, -0.1),
        (1, 1, 5, 1.1),
        (1, 1, 5, float("nan")),
    ],
)
def test_invalid_slurry_rejected(args):
    with pytest.raises(ValueError):
        SlurryLayer(*args)


@pytest.mark.parametrize(
    "overrides",
    [
        dict(length_m=0),
        dict(tubes=()),
        dict(tubes=(tube(carries_axial_load=False),)),
        dict(external_density_kg_m3=-1),
        dict(additional_mass_kg_m=-1),
        dict(additional_displaced_area_m2=-1),
        dict(axial_added_mass_kg_m=float("inf")),
    ],
)
def test_invalid_section_rejected(overrides):
    with pytest.raises(ValueError):
        section(**overrides)


@pytest.mark.parametrize(
    "overrides",
    [
        dict(gravity_m_s2=0),
        dict(lower_end_force_n=float("nan")),
        dict(lower_end_mass_kg=-1),
        dict(elements_per_section=0),
        dict(elements_per_section=True),
        dict(elements_per_section=1.5),
        dict(mode_count=0),
        dict(mode_count=33),
        dict(mode_count=True),
    ],
)
def test_invalid_modal_inputs_rejected(overrides):
    with pytest.raises(ValueError):
        modes([section()], **overrides)


def test_empty_profiles_and_strings_rejected():
    with pytest.raises(ValueError):
        slurry_density_profile([])
    with pytest.raises(ValueError):
        free_hanging_screen([], gravity_m_s2=10.0, lower_end_force_n=0.0)
    with pytest.raises(ValueError):
        modes([])


def test_public_synthetic_example_runs_through_density_static_and_modes():
    path = (
        Path(__file__).resolve().parents[2]
        / "examples/mining_lift_riser/synthetic.json"
    )
    example = json.loads(path.read_text())
    layers = [SlurryLayer(**row) for row in example["slurry_layers"]]
    profile = slurry_density_profile(layers)
    sections = [
        BundleSection(
            **example["section_properties"],
            length_m=layer.length_m,
            tubes=(
                Tube(
                    **example["tube_properties"],
                    contents_density_kg_m3=cell.density_kg_m3,
                ),
            ),
        )
        for layer, cell in zip(layers, profile)
    ]
    result = free_hanging_screen(sections, **example["static_inputs"])
    eigen = axial_modes(sections, **example["static_inputs"], **example["modal_inputs"])
    assert not result.has_compression
    assert len(eigen.frequencies_hz) == example["modal_inputs"]["mode_count"]
    assert all(f > 0 for f in eigen.frequencies_hz)
