# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Closed forms, domain rejection and public spudcan research comparisons."""

import math
from dataclasses import replace

import pytest

from digitalmodel.geotechnical.spudcan import (
    SandOverClay,
    UniformSand,
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


@pytest.mark.parametrize('depth', [0.0, 3.0, 12.0])
def test_uniform_circular_sand_closed_form_and_penetration_branch(depth):
    # Circular plates: 1961 paper, equations 3 and 5 (printed page 662).
    # Bearing factors: 1970 bulletin, equations 2 and 4. B is diameter.
    phi = math.radians(30)
    nq = math.exp(math.pi * math.tan(phi)) * math.tan(math.pi / 4 + phi / 2)**2
    ng = 1.5 * (nq - 1) * math.tan(phi)
    expected = 0.3 * 10 * 12 * ng + 1.2 * 10 * depth * (1 + 0.1 * depth / 12) * nq
    assert sand_resistance_kpa(12.0, depth, 10.0, 30.0) == pytest.approx(expected)
    sand = UniformSand(diameter_m=12.0, submerged_weight_kn_m3=10.0, friction_deg=30.0)
    point, = penetration_curve(sand, [depth])
    assert point.branch == 'uniform_sand_circular'
    assert point.pressure_kpa == pytest.approx(expected)
    assert point.load_kn == pytest.approx(expected * math.pi * 12**2 / 4)


@pytest.mark.parametrize('diameter,depth,gamma,friction', [
    (0, 0, 10, 30), (12, -1, 10, 30), (12, 13, 10, 30),
    (12, 0, 0, 30), (12, 0, 10, 0), (12, 0, 10, 50),
    (True, 0, 10, 30), (12, float('nan'), 10, 30),
])
def test_circular_sand_rejects_invalid_or_deep_inputs(diameter, depth, gamma, friction):
    with pytest.raises(ValueError):
        penetration_curve(UniformSand(diameter, gamma, friction), [depth])


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


# Independent reference arithmetic: Hossain 2019 Tables 1–2, Site 3 inputs;
# Hu 2015 equations 4–12 give qpeak = 405.061054 kPa from a separate
# 60-digit decimal evaluation (Taylor sin/cos; no production imports).
# Equations 13–14 give Nc = 14.95454545 and
# q(z) = 363.13709091 + 13.90772727 * (z - 4.9) kPa in clay.
# Loads below use the independently tabulated area 114.7 m2.
@pytest.mark.parametrize('depth,pressure,load,branch', [
    (0.0, 0.0, 0.0, 'illustrative_mobilization'),
    (0.294, 202.53052700, 23230.25144678, 'illustrative_mobilization'),
    (0.588, 405.06105400, 46460.50289356, 'peak'),
    (1.0, 401.05533211, 46001.04659270, 'illustrative_post_peak'),
    (2.744, 384.09907245, 44056.16361042, 'illustrative_post_peak'),
    (4.9, 363.13709091, 41651.82432727, 'clay'),
    (10.0, 434.06650000, 49787.42755000, 'clay'),
])
def test_layered_curve_matches_independent_pressure_and_load(depth, pressure, load, branch):
    points = penetration_curve(profile(), [depth])
    point = next(pt for pt in points if pt.depth_m == depth)
    assert point.branch == branch
    # Root solver stopping tolerance is 1e-8 relative pressure.
    assert point.pressure_kpa == pytest.approx(pressure, rel=3e-8, abs=1e-8)
    assert point.load_kn == pytest.approx(load, rel=3e-8, abs=1e-8)


def test_curve_contains_peak_and_interface_without_grid_aliasing():
    points = penetration_curve(profile(), [0.0, 1.0, 10.0])
    assert [pt.depth_m for pt in points] == [0.0, 0.588, 1.0, 4.9, 10.0]
    assert next(pt for pt in points if pt.branch == 'peak').load_kn == pytest.approx(
        46460.50289356, rel=3e-8)
    assert next(pt for pt in points if pt.depth_m == 4.9).pressure_kpa == pytest.approx(
        363.13709091)


def test_recovery_depth_is_force_equilibrium_not_a_dynamics_prediction():
    r = punch_through_screen(profile(), 60000)
    assert r.preload_reaches_peak
    # Independently solve z = 4.9 + (load / 114.7 - 363.13709091) / 13.90772727.
    assert r.peak_recovery_depth_m == pytest.approx(7.91443667, rel=3e-8)
    assert r.preload_equilibrium_depth_m == pytest.approx(16.40199848, rel=3e-8)
    assert r.interface_load_kn == pytest.approx(41651.82432727)


def test_sub_peak_equilibrium_uses_independent_mobilization_depth():
    # Linear mobilization assumption: z = (35600 / 46460.50289356) * 0.588.
    r = punch_through_screen(profile(), 35600)
    assert not r.preload_reaches_peak
    assert r.preload_equilibrium_depth_m == pytest.approx(0.45055044, rel=3e-8)


def test_no_recovery_is_explicit_for_zero_clay_gradient():
    p = replace(profile(), clay_gradient_kpa_m=0.0)
    r = punch_through_screen(p, 100000)
    # Independent Hu peak solve with k=0; clay is constant at 363.13709091 kPa.
    assert r.peak_load_kn == pytest.approx(44663.40176331, rel=3e-8)
    assert r.interface_load_kn == pytest.approx(41651.82432727)
    assert r.capacity_drop
    assert r.preload_reaches_peak
    assert r.preload_equilibrium_depth_m is None
    assert r.peak_recovery_depth_m is None
    points = penetration_curve(p, [4.9, 10.0, 20.0])
    for point in points:
        if point.depth_m >= 4.9:
            assert point.pressure_kpa == pytest.approx(363.13709091)
            assert point.load_kn == pytest.approx(41651.82432727)


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
    # Independent 60-digit Hu peak: 2201.77322196 kPa; equations 13–14:
    # qinterface = (11 * 0.16 + 10.5) * 200 + 0.9 * 1.936 * 7.3
    #            = 2464.71952 kPa. Mean load gives the midpoint of the
    # assumed rising connection: z = 0.56 * 1.936 = 1.08416 m.
    result = punch_through_screen(p, 267623.35875128)
    assert not result.capacity_drop
    assert result.preload_reaches_peak
    # The peak's 1e-8 relative pressure tolerance is amplified by the shallow
    # connection slope; 1e-7 m bounds that propagated root-solve error.
    assert result.preload_equilibrium_depth_m == pytest.approx(1.08416, abs=1e-7)
