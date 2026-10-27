# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Closed forms, domain rejection and public spudcan research comparisons."""

import math
from dataclasses import replace

import pytest

from digitalmodel.geotechnical.spudcan import (
    SandOverClay,
    clay_resistance_kpa,
    hu_clay_resistance_kpa,
    hu_peak_resistance_kpa,
    penetration_curve,
    punch_through_screen,
    sand_resistance_kpa,
)


def profile():
    # Hossain 2019, Tables 1–2, Site 3; no unit identity is retained.
    return SandOverClay(
        diameter_m=12.1, area_m2=114.7, sand_thickness_m=4.9,
        interface_su_kpa=22.13, clay_gradient_kpa_m=0.93,
        sand_submerged_weight_kn_m3=9.4, clay_submerged_weight_kn_m3=7.3,
        relative_density=0.637,
    )


def test_uniform_clay_closed_form():
    # Hu 2015 equation 5 at k=0, flat circular footing reference factor.
    assert clay_resistance_kpa(20.0, 0.0, 12.0) == pytest.approx(126.8)
    assert clay_resistance_kpa(20.0, 2.0, 12.0) == pytest.approx(140.24)


def test_uniform_sand_square_surrogate_closed_form():
    # Independent Hansen expression for B=L, c=0, d/B=0.25.
    phi = math.radians(30)
    nq = math.exp(math.pi * math.tan(phi)) * math.tan(math.pi / 4 + phi / 2)**2
    ng = 1.5 * (nq - 1) * math.tan(phi)
    dq = 1 + 2 * math.tan(phi) * (1 - math.sin(phi))**2 * 0.25
    expected = 10 * 3 * nq * (1 + math.tan(phi)) * dq + 0.5 * 10 * 12 * ng * 0.6
    assert sand_resistance_kpa(12.0, 3.0, 10.0, 30.0) == pytest.approx(expected)


def test_published_gulf_site3_peak_and_non_event():
    p = profile()
    peak_mn = hu_peak_resistance_kpa(p) * p.area_m2 / 1000
    # Hossain 2019 section 4.1(c): Hu prediction 46.6 MN, preload 35.6 MN.
    # 1% accommodates the conference-formulation and rounded-input differences.
    assert peak_mn == pytest.approx(46.6, rel=0.01)
    result = punch_through_screen(p, 35600)
    assert result.capacity_drop
    assert not result.preload_reaches_peak


def test_clay_branch_uses_interface_datum_and_sand_plug():
    p = profile()
    nc = 11 * 4.9 / 12.1 + 10.5
    assert hu_clay_resistance_kpa(p, 4.9) == pytest.approx(nc * 22.13 + 0.9 * 4.9 * 7.3)
    assert hu_clay_resistance_kpa(p, 10.0) == pytest.approx(
        nc * (22.13 + 0.93 * 5.1) + 0.9 * 4.9 * 7.3)


def test_curve_contains_peak_and_interface_without_grid_aliasing():
    p = profile()
    points = penetration_curve(p, [0.0, 1.0, 10.0])
    assert [pt.depth_m for pt in points] == sorted({0.0, 0.588, 1.0, 4.9, 10.0})
    assert points[0].load_kn == 0.0
    peak = next(pt for pt in points if pt.branch == 'peak')
    assert peak.load_kn == pytest.approx(hu_peak_resistance_kpa(p) * 114.7)
    assert next(pt for pt in points if pt.depth_m == 4.9).pressure_kpa == pytest.approx(
        hu_clay_resistance_kpa(p, 4.9))


def test_recovery_depth_is_force_equilibrium_not_a_dynamics_prediction():
    p = profile()
    r = punch_through_screen(p, 60000)
    assert r.preload_reaches_peak
    assert r.preload_equilibrium_depth_m > p.sand_thickness_m
    assert hu_clay_resistance_kpa(p, r.preload_equilibrium_depth_m) * p.area_m2 == pytest.approx(60000)
    assert hu_clay_resistance_kpa(p, r.peak_recovery_depth_m) == pytest.approx(hu_peak_resistance_kpa(p))


def test_no_recovery_is_explicit_for_zero_clay_gradient():
    r = punch_through_screen(replace(profile(), clay_gradient_kpa_m=0.0), 100000)
    assert r.preload_equilibrium_depth_m is None
    assert r.peak_recovery_depth_m is None


@pytest.mark.parametrize('name,value', [
    ('diameter_m', 0), ('area_m2', -1), ('interface_su_kpa', 0),
    ('clay_gradient_kpa_m', -1), ('relative_density', 0),
    ('relative_density', 1.1), ('sand_thickness_m', 0.1),
    ('sand_thickness_m', 20), ('base_cone_angle_deg', 7),
    ('sand_submerged_weight_kn_m3', float('nan')),
    ('clay_submerged_weight_kn_m3', float('inf')), ('diameter_m', True),
])
def test_invalid_profiles_rejected(name, value):
    with pytest.raises(ValueError):
        hu_peak_resistance_kpa(replace(profile(), **{name: value}))


@pytest.mark.parametrize('depths', [[], [-1, 2], [float('nan'), 2]])
def test_invalid_curve_grid_rejected(depths):
    with pytest.raises(ValueError):
        penetration_curve(profile(), depths)


def test_clay_branch_rejects_sand_depth():
    with pytest.raises(ValueError):
        hu_clay_resistance_kpa(profile(), 1)


@pytest.mark.parametrize('preload', [0, -1, float('nan'), True])
def test_invalid_preload_rejected(preload):
    with pytest.raises(ValueError):
        punch_through_screen(profile(), preload)


def test_published_gulf_site2_disagreement_is_exposed():
    # Hossain 2019 Site 2: observed final penetration 15.8–18.5 m at 53.4 MN,
    # with punch-through. The earlier Hu 2015 formulation does not reproduce it.
    p = SandOverClay(13.5, 143.6, 7.0, 17.87, 1.31, 9.4, 6.7, 0.649)
    result = punch_through_screen(p, 53400)
    assert result.capacity_drop
    assert not result.preload_reaches_peak  # a documented false negative
    assert result.preload_equilibrium_depth_m < 15.8
    # Even the clay-branch equilibrium at peak is shallower than the field range.
    assert result.peak_recovery_depth_m < 15.8


def test_increasing_connection_uses_its_first_equilibrium_not_the_interface():
    p = replace(profile(), sand_thickness_m=0.16 * 12.1,
                interface_su_kpa=200, clay_gradient_kpa_m=0)
    peak = hu_peak_resistance_kpa(p)
    interface = hu_clay_resistance_kpa(p, p.sand_thickness_m)
    assert interface > peak
    load = (peak + interface) / 2 * p.area_m2
    result = punch_through_screen(p, load)
    assert not result.capacity_drop
    expected = (0.12 * p.sand_thickness_m + p.sand_thickness_m) / 2
    assert result.preload_equilibrium_depth_m == pytest.approx(expected)
