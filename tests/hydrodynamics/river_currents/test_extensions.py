# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Analytical checks for synthetic river screening scenarios."""

import hashlib
import json
import math
from pathlib import Path

import pytest


def test_lateral_magnitude_and_discharge_conservation():
    from digitalmodel.hydrodynamics.river_currents.bank_effects import lateral_factor

    assert lateral_factor(0, 100, 1) == 0
    assert lateral_factor(50, 100, 1) == 1.5
    assert lateral_factor(25, 100, 1) == 1.125
    # Simpson integration is exact for the assumed quadratic distribution.
    assert (lateral_factor(0, 100, 1) + 4 * lateral_factor(50, 100, 1)
            + lateral_factor(100, 100, 1)) / 6 == 1


def test_confinement_and_corrected_shipped_profile():
    from digitalmodel.hydrodynamics.river_currents.bank_effects import (
        blockage_factor, corrected_profile,
    )
    from digitalmodel.hydrodynamics.river_currents import power_law_profile

    assert blockage_factor(100, 5, 20, 2.5) == pytest.approx(1 / 0.9)
    profile = corrected_profile(lambda z: power_law_profile(z, 5, 2), 1.5)
    assert profile(5) == 3
    assert profile(2.5) == pytest.approx(3 * 0.5 ** (1 / 6))


def test_bend_superelevation_and_outer_bank_bias():
    from digitalmodel.hydrodynamics.river_currents.bank_effects import (
        bend_factor, bend_superelevation,
    )

    # Constant-speed radial balance integrated from radius 150 to 250 m.
    assert bend_superelevation(2, 100, 200, 10) == pytest.approx(
        4 / 10 * math.log(250 / 150))
    assert bend_factor(0, 100, 0.2) == 0.8
    assert bend_factor(100, 100, 0.2) == 1.2


@pytest.mark.parametrize('function,args', [
    ('lateral_factor', (-1, 100, 1)),
    ('lateral_factor', (50, 100, 2)),
    ('blockage_factor', (20, 2, 20, 2)),
    ('blockage_factor', (20, 2, 21, 1)),
    ('bend_factor', (50, 100, -0.1)),
    ('bend_superelevation', (2, 100, 40, 10)),
    ('lateral_factor', (float('nan'), 100, 1)),
])
def test_bank_invalid(function, args):
    from digitalmodel.hydrodynamics.river_currents import bank_effects

    with pytest.raises(ValueError):
        getattr(bank_effects, function)(*args)


def test_weibull_known_quantile_series():
    from digitalmodel.hydrodynamics.river_currents.extremes import fit_weibull

    p = [(i + 0.5) / 10 for i in range(10)]
    series = [2 * math.sqrt(-math.log1p(-q)) for q in p]
    fit = fit_weibull(series)
    assert fit.shape == pytest.approx(2)
    assert fit.scale == pytest.approx(2)
    assert fit.return_level(10) == pytest.approx(2 * math.sqrt(math.log(10)))
    # Ten independent samples/year: F(x)^10 = 0.9, not F(x) = 0.99.
    assert fit.return_level(10, samples_per_year=10) == pytest.approx(
        2 * math.sqrt(-math.log(1 - 0.9 ** 0.1)))


def test_gumbel_known_annual_maxima_series():
    from digitalmodel.hydrodynamics.river_currents.extremes import fit_gumbel

    p = [(i + 0.5) / 10 for i in range(10)]
    series = [3 - 0.5 * math.log(-math.log(q)) for q in p]
    fit = fit_gumbel(series)
    assert fit.location == pytest.approx(3)
    assert fit.scale == pytest.approx(0.5)
    assert fit.return_level(10) == pytest.approx(3 - 0.5 * math.log(-math.log(0.9)))


def test_seasonal_flood_variation():
    from digitalmodel.hydrodynamics.river_currents.extremes import seasonal_summary

    assert seasonal_summary([1, 2, 3, 4], ['dry', 'dry', 'flood', 'flood']) == {
        'dry': {'count': 2, 'mean': 1.5, 'maximum': 2.0},
        'flood': {'count': 2, 'mean': 3.5, 'maximum': 4.0},
    }


@pytest.mark.parametrize('function,series', [
    ('fit_weibull', [0, 1, 2]), ('fit_weibull', [1, 1, 1]),
    ('fit_gumbel', [1, 2]), ('fit_gumbel', [1, float('inf'), 3]),
])
def test_extreme_invalid_series(function, series):
    from digitalmodel.hydrodynamics.river_currents import extremes

    with pytest.raises(ValueError):
        getattr(extremes, function)(series)


@pytest.mark.parametrize('period,rate', [(1, 1), (0, 1), (10, 0), (math.inf, 1)])
def test_extreme_invalid_return_period(period, rate):
    from digitalmodel.hydrodynamics.river_currents.extremes import fit_weibull

    with pytest.raises(ValueError):
        fit_weibull([1, 2, 3]).return_level(period, rate)


def _vessel():
    from digitalmodel.hydrodynamics.models import VesselProperties

    return VesselProperties('generic', 40, 10, 2, 500)


def test_shallow_correction_and_force_magnitudes():
    from digitalmodel.hydrodynamics.river_currents.vessel_forces import (
        current_load, shallow_drag_factor,
    )

    assert shallow_drag_factor(3, 2) == 9
    force = current_load(_vessel(), lambda z: 2, 3, water_density=1000)
    assert force['Fx_surge_N'] == pytest.approx(0.5 * 1000 * 4 * 20 * 0.55 * 9)
    assert force['Fy_sway_N'] == 0
    assert force['Mz_yaw_Nm'] == 0
    beam = current_load(_vessel(), lambda z: 2, 3, direction_deg=90,
                        water_density=1000)
    assert beam['Fx_surge_N'] == 0
    assert beam['Fy_sway_N'] == pytest.approx(0.5 * 1000 * 4 * 80 * 1.2 * 9)
    assert beam['Mz_yaw_Nm'] == pytest.approx(0.5 * 1000 * 4 * 80 * 40 * 0.4 * 9)


def test_depth_integrated_power_law_against_analytical_integral():
    from digitalmodel.hydrodynamics.river_currents import power_law_profile
    from digitalmodel.hydrodynamics.river_currents.vessel_forces import current_load

    force = current_load(_vessel(), lambda z: power_law_profile(z, 3, 2), 3,
                         water_density=1000, n_slices=1000)
    # Integral of u^2 over the occupied heights [1,3], divided by draft 2.
    mean_square = 4 * 3 / (2 * (1 + 2 / 6)) * (1 - (1 / 3) ** (1 + 2 / 6))
    analytical = 0.5 * 1000 * mean_square * 20 * 0.55 * 9
    assert force['Fx_surge_N'] == pytest.approx(analytical, rel=1e-6)
    uniform = current_load(_vessel(), lambda z: 2, 3, water_density=1000)
    assert force['Fx_surge_N'] < uniform['Fx_surge_N']


def test_signed_profile_relative_motion_and_oblique_loads():
    from digitalmodel.hydrodynamics.river_currents.vessel_forces import current_load

    positive = current_load(_vessel(), lambda z: 2, 3, direction_deg=30)
    reverse = current_load(_vessel(), lambda z: -2, 3, direction_deg=30)
    for key in ('Fx_surge_N', 'Fy_sway_N', 'Mz_yaw_Nm'):
        assert reverse[key] == pytest.approx(-positive[key])
    moving = current_load(_vessel(), lambda z: 1, 3, vessel_speed=3)
    stationary = current_load(_vessel(), lambda z: -2, 3)
    assert moving == stationary


def test_towing_pull_and_yaw_limit():
    from digitalmodel.hydrodynamics.river_currents.vessel_forces import towing_assessment

    load = {'Fx_surge_N': 3000, 'Fy_sway_N': 4000, 'Mz_yaw_Nm': 1000}
    r = towing_assessment(load, 10, efficiency=0.5, reserve_factor=1.2,
                          yaw_capacity_Nm=1000)
    assert r['required_bollard_pull_N'] == 12000
    assert r['go'] is False
    assert towing_assessment(load, 12000, efficiency=0.5, reserve_factor=1.2,
                             yaw_capacity_Nm=1000)['go'] is True
    assert towing_assessment(load, 12000, efficiency=0.5, reserve_factor=1.2,
                             yaw_capacity_Nm=999)['go'] is False


@pytest.mark.parametrize('kwargs', [
    {'water_depth': 2}, {'water_depth': 3, 'n_slices': 0},
    {'water_depth': 3, 'water_density': -1},
    {'water_depth': 3, 'direction_deg': math.nan},
])
def test_invalid_loading(kwargs):
    from digitalmodel.hydrodynamics.river_currents.vessel_forces import current_load

    with pytest.raises(ValueError):
        current_load(_vessel(), lambda z: 2, **kwargs)


def test_worked_examples_and_deterministic_report(tmp_path):
    from digitalmodel.hydrodynamics.river_currents.examples import build, compute

    basis_path = Path('examples/hydrodynamics/river_currents/basis.json')
    data = json.loads(basis_path.read_text())
    results = compute(data)
    bank = results['bank_and_extremes']
    assert bank['centre_factor'] == 1.5
    assert bank['confinement_factor'] == pytest.approx(1 / 0.9)
    assert bank['weibull_10_year_m_s'] == pytest.approx(2 * math.sqrt(math.log(10)))
    assert bank['gumbel_10_year_m_s'] == pytest.approx(3 - 0.5 * math.log(-math.log(0.9)))
    towing = results['towing']
    # Discharge 600 / (100*3) -> mean 2; m=6 -> surface 7/3.
    # Mid-channel 1.5 and channel blockage 20/300 -> combined surface 3.75.
    assert towing['surface_speed_m_s'] == pytest.approx(3.75)
    mean_square = 3.75 ** 2 * 3 / (2 * (1 + 2 / 6)) * (1 - (1 / 3) ** (1 + 2 / 6))
    force = 0.5 * 1000 * mean_square * 20 * 0.55 * 9
    assert towing['loads']['Fx_surge_N'] == pytest.approx(force, rel=1e-6)
    assert towing['assessment']['required_bollard_pull_N'] == pytest.approx(force * 1.2 / 0.8)
    assert towing['assessment']['go'] is False
    first, second = tmp_path / 'first', tmp_path / 'second'
    build(basis_path, first)
    build(basis_path, second)
    for p in first.iterdir():
        assert p.read_bytes() == (second / p.name).read_bytes()
        committed = Path('docs/domains/hydrodynamics/river_currents') / p.name
        assert p.read_bytes() == committed.read_bytes()
    manifest = json.loads((first / 'river-current-analysis.manifest.json').read_text())
    assert manifest['basis_sha256'] == hashlib.sha256(basis_path.read_bytes()).hexdigest()
    for name, digest in manifest['outputs'].items():
        assert hashlib.sha256((first / name).read_bytes()).hexdigest() == digest
    page = (first / 'river-current-analysis.html').read_text()
    assert page.index('Summary') < page.index('Revision history') < page.index('Bank effects')
    assert str(data['revision']) in page
    assert '<svg' in page and 'height above bed' in page
    assert 'src=' not in page and '<script' not in page


def test_towing_verdict_reverses_without_slot_multiplier():
    from digitalmodel.hydrodynamics.river_currents.examples import compute, render

    data = json.loads(Path('examples/hydrodynamics/river_currents/basis.json').read_text())
    results = compute(data)
    towing = results['towing']
    demand = towing['required_bollard_pull_without_slot_N']
    assert towing['shallow_drag_factor'] == 9
    assert demand == pytest.approx(towing['assessment']['required_bollard_pull_N'] / 9)
    assert demand / 1000 == pytest.approx(100.4, abs=0.05)
    assert demand < data['towing']['available_pull_N']
    assert towing['assessment']['go'] is False
    assert towing['loads']['Mz_yaw_Nm'] == 0
    summary = render(data, results).split('<h2>Summary</h2>')[1].split('<h2>Revision history</h2>')[0]
    assert 'slot multiplier of 9.0' in summary
    assert '100.4 kN' in summary
    assert 'verdict reverses' in summary


def test_revision_must_match_latest_record(tmp_path):
    from digitalmodel.hydrodynamics.river_currents.examples import build

    data = json.loads(Path('examples/hydrodynamics/river_currents/basis.json').read_text())
    data['revision'] = 'unrecorded'
    p = tmp_path / 'basis.json'
    p.write_text(json.dumps(data))
    with pytest.raises(ValueError, match='revision'):
        build(p, tmp_path / 'output')
