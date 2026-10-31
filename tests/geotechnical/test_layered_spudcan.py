# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent hand calculations for layered circular-footing mechanisms.

Sand equations 1–3: Spudcan Penetration Analyses for Three Case Histories in
Sand Overlying Clay (2015), https://jack-up.citystgeorges.ac.uk/wp-content/
uploads/2025/09/Jackup-2015-11.pdf.
Clay equations 2 and 6: Theoretical Study on the Effect of a Weak Layer on the
Vertical Bearing Capacity of Spudcans in Clay (2015), same proceedings,
Jackup-2015-19.pdf. The clay reference factor is explicitly selected as 6.34
from the existing circular branch, rather than that paper's 6.05 surface factor.
All numerical expectations below are independent arithmetic, not model outputs.
"""

import importlib.util
import json
import math
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.geotechnical.spudcan import (
    LayeredProfile,
    SoilLayer,
    Spudcan,
    penetration_curve,
)


def clay(top, bottom, su, gamma=7.0):
    return SoilLayer('clay', top, bottom, gamma, su_kpa=su)


def point(curve, depth):
    return next(p for p in curve.points if p.depth_m == pytest.approx(depth))


def test_uniform_control_and_tip_datum():
    # qgross = 6.34*20 + 7*z; Q = q*pi*10^2/4. Tip = bearing plane + 1.
    curve = penetration_curve(LayeredProfile((clay(0, 20, 20),)),
                              Spudcan(10, tip_to_bearing_m=1), [1, 3, 11])
    assert [p.pressure_kpa for p in curve.points] == pytest.approx([126.8, 140.8, 196.8])
    assert point(curve, 3).load_kn == pytest.approx(3520 * math.pi)
    assert curve.post_peak_drop_kn == 0
    assert curve.punch_through_flag is None  # no supplied preload
    assert curve.events == ()


def test_stiff_over_soft_hand_calculation_and_exact_transition():
    # D=10, upper su=100, lower su=10, H=20, both gamma=0.
    # q = min(634, 63.4 + 3*100*(20-z)/10).
    # Cap changes at h=(634-63.4)/30=19.02, hence zpeak=0.98.
    # At z=10: q=363.4; at interface q=63.4; loss=570.6 kPa.
    profile = LayeredProfile((clay(0, 20, 100, 0), clay(20, 40, 10, 0)))
    curve = penetration_curve(profile, Spudcan(10), [0, 10, 30], preload_kn=634 * 25 * math.pi)
    assert point(curve, 0.98).pressure_kpa == pytest.approx(634)
    assert point(curve, 10).pressure_kpa == pytest.approx(363.4)
    assert point(curve, 20).pressure_kpa == pytest.approx(63.4)
    assert curve.peak.depth_m == pytest.approx(0.98)
    assert curve.post_peak_drop_kn == pytest.approx(14265 * math.pi)
    assert curve.events[0].trough.depth_m == 20
    assert curve.events[0].recovery_depth_m is None
    assert curve.punch_through_flag
    assert not penetration_curve(profile, Spudcan(10), [0, 30], preload_kn=100).punch_through_flag


@pytest.mark.parametrize('method,expected', [('load_spread', 136.8 * (17 / 15)**2),
                                            ('punching_shear', 126.8 + 24 / math.sqrt(3) + 10)])
def test_sand_over_clay_methods_match_hand_calculations(method, expected):
    # Sand H=3, D=10, gamma=10, phi=30, clay su=20, Ks=1.5.
    # z=1 => h=2, q0=10. Spread ratio=3:
    # Eq1: (1+4/30)^2*(126.8+10) = (17/15)^2*136.8.
    # Eq2: 126.8 + (4/10)*(20+20)*1.5*tan(30) + 10.
    profile = LayeredProfile((SoilLayer('sand', 0, 3, 10, friction_deg=30),
                              clay(3, 20, 20)), sand_method=method,
                             punching_coefficient=1.5 if method == 'punching_shear' else None)
    curve = penetration_curve(profile, Spudcan(10), [0, 1, 3, 10], preload_kn=30000)
    assert point(curve, 1).pressure_kpa == pytest.approx(expected)
    # At z=3: same circular clay reference plus sand effective overburden.
    assert point(curve, 3).pressure_kpa == pytest.approx(156.8)
    assert curve.post_peak_drop_kn > 0


def test_sand_is_limited_by_existing_circular_sand_branch():
    # Weak sand cap, strong underlying clay: independent circular-plate formula.
    phi = math.radians(30)
    nq = math.exp(math.pi * math.tan(phi)) * math.tan(math.pi / 4 + phi / 2)**2
    ng = 1.5 * (nq - 1) * math.tan(phi)
    profile = LayeredProfile((SoilLayer('sand', 0, 3, 10, friction_deg=30), clay(3, 20, 200)))
    curve = penetration_curve(profile, Spudcan(10), [0, 1, 3])
    assert point(curve, 1).pressure_kpa == pytest.approx(0.3 * 10 * 10 * ng + 1.2 * 10 * 1.01 * nq)
    assert point(curve, 1).branch == 'sand_general_shear'


def test_squeezing_is_bounded_and_transmitted_through_upper_clay():
    # D=10, strong/soft/strong, gamma=0; soft layer z=1..5, su=10.
    # z=3: T=2; Eq2 q=10*(5+0.33*10/2+1.2*3/10)=70.1.
    # z=4.9: raw squeeze=10*(5+33+0.588)=385.88, cap=6.34*30=190.2.
    # At z=0 lower support at z=1 = max(63.4,10*(5+.825+.12))=63.4;
    # upper punching = 63.4 + 3*100*1/10 = 93.4.
    profile = LayeredProfile((clay(0, 1, 100, 0), clay(1, 5, 10, 0), clay(5, 15, 30, 0)))
    curve = penetration_curve(profile, Spudcan(10), [0, 1, 3, 4.9, 5, 10], preload_kn=8000)
    assert point(curve, 0).pressure_kpa == pytest.approx(93.4)
    assert point(curve, 3).pressure_kpa == pytest.approx(70.1)
    assert point(curve, 4.9).pressure_kpa == pytest.approx(190.2)
    assert point(curve, 5).pressure_kpa == pytest.approx(190.2)
    assert curve.maximum.pressure_kpa == pytest.approx(190.2)
    assert curve.peak.pressure_kpa == pytest.approx(93.4)
    assert curve.events[0].recovery_depth_m is not None
    # Recovery is solved on the actual squeeze expression, not interpolated
    # between requested samples: 10*(5+3.3/(5-z)+.12*z)=93.4.
    # 0.12*z^2 - 4.94*z + 18.4 = 0; use its smaller root.
    expected = (4.94 - math.sqrt(4.94**2 - 4 * 0.12 * 18.4)) / 0.24
    assert curve.events[0].recovery_depth_m == pytest.approx(expected)


def test_equal_clay_strengths_have_no_artificial_drop():
    # Equal clay strengths imply identical support at every interface.
    profile = LayeredProfile((clay(0, 3, 20), clay(3, 20, 20)))
    curve = penetration_curve(profile, Spudcan(10), [0, 10], preload_kn=15000)
    assert not curve.punch_through_flag
    assert not curve.events


@pytest.mark.parametrize('method', ['load_spread', 'punching_shear'])
def test_grid_resolution_does_not_hide_sand_peak_or_recovery(method):
    # Independent stationary points for H=5, D=10, gamma_s=10, su=10:
    # Spread q=(4/3-z/15)^2*(63.4+10z), dq/dz=0 at z=2.44.
    # Punch q=63.4+2*(25-z^2)*1.5*tan(30)+10z;
    # dq/dz=0 at z=10/(6*tan(30)). Both reach q=113.4 at z=5.
    # Clay then recovers at z=5+(qpeak-113.4)/7.
    if method == 'load_spread':
        peak_depth = 2.44
        pressure = (4 / 3 - peak_depth / 15)**2 * (63.4 + 10 * peak_depth)
    else:
        peak_depth = 10 / (6 * math.tan(math.pi / 6))
        pressure = 63.4 + 3 * (25 - peak_depth**2) * math.tan(math.pi / 6) + 10 * peak_depth
    profile = LayeredProfile((SoilLayer('sand', 0, 5, 10, friction_deg=30), clay(5, 30, 10)),
                             sand_method=method,
                             punching_coefficient=1.5 if method == 'punching_shear' else None)
    for depths in ([0, 30], [i / 10 for i in range(301)]):
        curve = penetration_curve(profile, Spudcan(10), depths, preload_kn=30000)
        assert curve.peak.depth_m == pytest.approx(peak_depth)
        assert curve.peak.pressure_kpa == pytest.approx(pressure)
        assert curve.post_peak_drop_kn == pytest.approx((pressure - 113.4) * 25 * math.pi)
        assert curve.events[0].recovery_depth_m == pytest.approx(5 + (pressure - 113.4) / 7)


@pytest.mark.parametrize('depths', [[], [-1, 2], [float('nan')], [float('inf')], [True], [21]])
def test_invalid_depths(depths):
    with pytest.raises(ValueError):
        penetration_curve(LayeredProfile((clay(0, 20, 20),)), Spudcan(10), depths)


@pytest.mark.parametrize('spudcan', [Spudcan(0), Spudcan(True), Spudcan(10, -1),
                                    Spudcan(10, float('inf')), Spudcan(1e200)])
def test_invalid_geometry(spudcan):
    with pytest.raises(ValueError):
        penetration_curve(LayeredProfile((clay(0, 20, 20),)), spudcan, [0, 10])


def test_partial_engagement_is_rejected():
    with pytest.raises(ValueError, match='bearing'):
        penetration_curve(LayeredProfile((clay(0, 20, 20),)), Spudcan(10, 1), [0, 2])


@pytest.mark.parametrize('profile', [
    LayeredProfile(()),
    LayeredProfile((clay(1, 20, 20),)),
    LayeredProfile((clay(0, 2, 20), clay(3, 20, 30))),
    LayeredProfile((clay(0, 2, -1),)),
    LayeredProfile((replace(clay(0, 20, 20), kind='silt'),)),
    LayeredProfile((replace(clay(0, 20, 20), submerged_weight_kn_m3=float('nan')),)),
    LayeredProfile((replace(clay(0, 20, 20), friction_deg=30),)),
    LayeredProfile((SoilLayer('sand', 0, 3, 10, friction_deg=30),)),
    LayeredProfile((clay(0, 2, 20), SoilLayer('sand', 2, 3, 10, friction_deg=30))),
    LayeredProfile((SoilLayer('sand', 0, 3, 10, friction_deg=50), clay(3, 20, 20))),
    LayeredProfile((SoilLayer('sand', 0, 11, 10, friction_deg=30), clay(11, 20, 20))),
    LayeredProfile((SoilLayer('sand', 0, 3, 10, friction_deg=30), clay(3, 20, 20)), spread_ratio=2),
    LayeredProfile((clay(0, 20, 20),), sand_method='unknown'),
    LayeredProfile((SoilLayer('sand', 0, 3, 10, friction_deg=30), clay(3, 20, 20)),
                   sand_method='punching_shear'),
])
def test_invalid_profiles(profile):
    with pytest.raises(ValueError):
        penetration_curve(profile, Spudcan(10), [1, 2])


@pytest.mark.parametrize('preload', [-1, 0, True, float('nan'), float('inf')])
def test_invalid_preload(preload):
    with pytest.raises(ValueError):
        penetration_curve(LayeredProfile((clay(0, 20, 20),)), Spudcan(10), [0, 10], preload_kn=preload)


def test_example_outputs_are_portable_offline_and_replayable(tmp_path):
    path = Path('examples/domains/geotechnical/jackup_layered_penetration.py')
    spec = importlib.util.spec_from_file_location('layered_example', path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.write_examples(tmp_path)
    inputs = sorted(tmp_path.glob('*.input.json'))
    assert {p.stem for p in inputs} == {'stiff_over_soft.input', 'sand_over_soft.input', 'uniform_soft.input'}
    # Independent default-case peaks (D=12, area=36*pi):
    # Clay punch at mudline: 6.34*15+3*80*3/12=155.1 kPa.
    # Sand spread at mudline: (1+8/36)^2*6.34*20 kPa; it decreases
    # immediately and lies below the homogeneous circular sand cap.
    # Control maximum: 6.34*15+6*22.5=230.1 kPa at the evaluated bottom.
    expected = {
        'stiff_over_soft': (155.1 * 36 * math.pi, True),
        'sand_over_soft': ((11 / 9)**2 * 126.8 * 36 * math.pi, True),
        'uniform_soft': (230.1 * 36 * math.pi, False),
    }
    for source in inputs:
        payload = json.loads(source.read_text())
        assert payload['basis'] == 'assumed, based on previous experience'
        assert payload['parameter_ranges']
        profile, spudcan, depths, preload = module.from_input(payload)
        curve = penetration_curve(profile, spudcan, depths, preload_kn=preload)
        name = source.name.removesuffix('.input.json')
        assert (tmp_path / f'{name}.curve.csv').is_file()
        figure = json.loads((tmp_path / f'{name}.figure.json').read_text())
        assert figure['layout']['xaxis']['title']['text'] == 'Tip penetration below mudline (m)'
        assert any(t['name'] == 'Preload reaction' for t in figure['data'])
        summary = json.loads((tmp_path / f'{name}.summary.json').read_text())
        expected_peak, expected_flag = expected[name]
        assert curve.peak.load_kn == pytest.approx(expected_peak)
        assert summary['peak']['load_kn'] == pytest.approx(expected_peak)
        assert summary['punch_through_flag'] is expected_flag
        assert summary['standards'][0]['clause'] == 'TO CONFIRM'


def test_preload_support_is_separate_from_capacity_loss_flag():
    # q(z)=6.34*10+7*z, D=10. At preload pressure 100 kPa, z=36.6/7.
    # At pressure 1000 kPa there is no support before the 20 m profile bottom.
    profile = LayeredProfile((clay(0, 20, 10),))
    supported = penetration_curve(profile, Spudcan(10), [0, 20], preload_kn=2500 * math.pi)
    assert supported.preload_support_depth_m == pytest.approx(36.6 / 7)
    unsupported = penetration_curve(profile, Spudcan(10), [0, 20], preload_kn=25000 * math.pi)
    assert unsupported.preload_support_depth_m is None
    assert unsupported.punch_through_flag is False


def test_multiple_drops_remain_shielded_by_an_earlier_unreached_capacity():
    # D=10, gamma=0; successive interface supports calculated bottom-up:
    # z=18: 5*(5+.825+2.16)=39.925 kPa (squeezing);
    # z=14: 39.925+3*80*4/10=135.925 kPa (punching);
    # z=10: 10*(5+.825+1.2)=70.25 kPa (squeezing);
    # z=0: 70.25+3*80*10/10=310.25 kPa (punching).
    profile = LayeredProfile((clay(0, 10, 80, 0), clay(10, 14, 10, 0),
                              clay(14, 18, 80, 0), clay(18, 22, 5, 0), clay(22, 40, 100, 0)))
    curve = penetration_curve(profile, Spudcan(10), [0, 40], preload_kn=5000 * math.pi)
    assert curve.peak.pressure_kpa == pytest.approx(310.25)
    assert len(curve.events) == 2
    assert all(not event.preload_reaches_peak for event in curve.events)
    assert curve.preload_support_depth_m == 0
    assert not curve.punch_through_flag


def test_single_point_and_restricted_interval_do_not_extrapolate_events():
    profile = LayeredProfile((clay(0, 2, 80, 0), clay(2, 20, 10, 0)))
    curve = penetration_curve(profile, Spudcan(10), [5], preload_kn=1585 * math.pi)
    # 63.4 kPa * 25*pi is exactly the supplied preload at the only sample.
    assert [p.depth_m for p in curve.points] == [5]
    assert curve.preload_support_depth_m == 5
    assert curve.events == ()
    assert curve.evaluated_tip_interval_m == (5, 5)


def test_legacy_keyword_interface_is_preserved():
    from digitalmodel.geotechnical.spudcan import UniformSand
    # Independent circular-plate formula at phi=30, D=10, gamma=10, z=0.
    phi = math.radians(30)
    nq = math.exp(math.pi * math.tan(phi)) * math.tan(math.pi / 4 + phi / 2)**2
    expected = 0.3 * 10 * 10 * 1.5 * (nq - 1) * math.tan(phi)
    result = penetration_curve(UniformSand(10, 10, 30), depths_m=[0])
    assert isinstance(result, list)
    assert result[0].pressure_kpa == pytest.approx(expected)


def test_sand_general_shear_intersection_is_inserted_in_sparse_grid():
    # Independent cap intersection for H=5, D=10, gamma_s=10, su=150.
    # Circular sand: q=30*Ngamma+12*z*(1+.01*z)*Nq.
    # Spread: q=(4/3-z/15)^2*(951+10z). Solve their crossing by hand-equation
    # bisection without calling either production resistance function.
    phi = math.pi / 6
    nq = math.exp(math.pi * math.tan(phi)) * math.tan(math.pi / 4 + phi / 2)**2
    ng = 1.5 * (nq - 1) * math.tan(phi)
    low, high = 0, 5
    for _ in range(100):
        z = (low + high) / 2
        if 30 * ng + 12 * z * (1 + .01 * z) * nq < (4 / 3 - z / 15)**2 * (951 + 10 * z):
            low = z
        else:
            high = z
    expected_depth = (low + high) / 2
    expected_pressure = 30 * ng + 12 * expected_depth * (1 + .01 * expected_depth) * nq
    profile = LayeredProfile((SoilLayer('sand', 0, 5, 10, friction_deg=30), clay(5, 30, 150)))
    curve = penetration_curve(profile, Spudcan(10), [0, 30])
    assert curve.peak.depth_m == pytest.approx(expected_depth)
    assert curve.peak.pressure_kpa == pytest.approx(expected_pressure)
    assert curve.post_peak_drop_kn == pytest.approx((expected_pressure - 1001) * 25 * math.pi)


def test_tip_must_remain_inside_investigated_soil_even_with_bearing_offset():
    # Soil investigation ends at 20 m. A 20.5 m tip with a 1 m offset has
    # its bearing plane at 19.5 m, but the tip is outside the known profile.
    with pytest.raises(ValueError, match='profile'):
        penetration_curve(LayeredProfile((clay(0, 20, 20),)), Spudcan(10, 1), [20.5])
