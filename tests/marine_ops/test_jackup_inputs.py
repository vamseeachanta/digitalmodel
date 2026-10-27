# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Frozen-input integrity and explicit missing-source failures."""

import hashlib
import json
import math
from datetime import datetime
from pathlib import Path

import pytest

from digitalmodel.marine_ops.jackup_inputs import (
    frozen_inputs,
    frozen_environment,
    gumbel_return_levels,
    observation_summary,
    query_public_inputs,
)


def test_observed_maxima_are_not_return_levels_and_missing_values_are_counted():
    records = [
        {'timestamp': datetime(2024, 1, 1), 'wave_height': 1, 'wind_speed': 2},
        {'timestamp': datetime(2024, 1, 2), 'wave_height': 99, 'wind_speed': None},
        {'timestamp': datetime(2024, 1, 3), 'wave_height': 3, 'wind_speed': 4},
    ]
    result = observation_summary(records)
    assert result['record_count'] == 3
    assert result['wave_height']['maximum'] == 3
    assert result['wave_height']['valid_count'] == 2
    assert result['wind_speed']['valid_count'] == 2
    assert result['return_period_years'] is None
    assert result['status'] == 'observed_sample_only'


@pytest.mark.parametrize('records', [[], [{'wave_height': None, 'wind_speed': None}],
                                     [{'wave_height': float('nan'), 'wind_speed': 99}]])
def test_empty_or_unusable_metocean_is_rejected(records):
    with pytest.raises(ValueError):
        observation_summary(records)


def test_missing_era5_is_not_replaced_by_another_source():
    with pytest.raises(ValueError, match='ERA5'):
        query_public_inputs(None, None, None, datetime(2024, 1, 1), datetime(2025, 1, 1),
                            {'lat': 28, 'lon': -95}, '42019')


def test_clients_are_consumed_with_date_and_location_and_zero_depth_is_rejected():
    class Ocean:
        def query_by_date(self, **kwargs):
            assert kwargs['start_date'] == datetime(2024, 1, 1)
            return iter([{'wave_height': 2.0, 'wind_speed': 10.0}])
    class Reanalysis:
        def query_by_date(self, **kwargs):
            assert kwargs['location'] == {'lat': 28, 'lon': -95}
            return iter([{'wave_height': 3.0, 'wind_speed_10m': 11.0}])
    class Depth:
        def get_depth(self, lat, lon):
            return 0.0
    args = (Ocean(), Reanalysis(), Depth(), datetime(2024, 1, 1), datetime(2025, 1, 1),
            {'lat': 28, 'lon': -95}, '42019')
    with pytest.raises(ValueError, match='depth'):
        query_public_inputs(*args)
    class ValidDepth:
        def get_depth(self, lat, lon):
            return 60.0
    result = query_public_inputs(args[0], args[1], ValidDepth(), *args[3:])
    assert result['era5']['wind_speed']['maximum'] == 11.0
    assert result['noaa']['wave_height']['maximum'] == 2.0
    assert result['water_depth_m'] == 60.0
    assert result['return_levels_status'] == 'not_evaluated'


def test_frozen_digest_is_verified_before_parsing(tmp_path):
    value = {'test': 1}
    raw = json.dumps(value).encode()
    (tmp_path / 'inputs.json').write_bytes(raw)
    (tmp_path / 'manifest.json').write_text(json.dumps({
        'files': {'inputs.json': hashlib.sha256(raw).hexdigest()}}))
    assert frozen_inputs(tmp_path) == value
    (tmp_path / 'inputs.json').write_text('{"test": 2}')
    with pytest.raises(ValueError, match='digest'):
        frozen_inputs(tmp_path)


def test_shipped_input_manifest_and_assumptions():
    root = Path(__file__).resolve().parents[2]
    data = frozen_inputs(root / 'examples' / 'data' / 'jackup_screening')
    assert data['qualification'] == 'generic_screening_only'
    for key in ('lightship_t', 'preload_per_leg_kn', 'spudcan_area_m2', 'leg_spacing_m'):
        assert data['unit'][key]['status'] == 'illustrative assumption'
        assert data['unit'][key]['basis']
    assert data['environment']['era5']['status'] == 'unavailable'
    assert data['environment']['gebco']['status'] == 'unavailable'
    assert data['environment']['return_period_years'] is None


def test_worked_example_runs_offline_and_keeps_environment_unqualified():
    import importlib.util
    root = Path(__file__).resolve().parents[2]
    script = root / 'examples' / 'domains' / 'geotechnical' / 'jackup_screening.py'
    spec = importlib.util.spec_from_file_location('jackup_example', script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    result = module.run_example()
    assert result['qualification'] == 'generic_screening_only'
    assert result['environment_status'] == 'illustrative_inputs_only'
    assert result['operating_envelope_status'] == 'synthetic_load_sensitivity_only'
    assert result['curve']
    assert result['static_envelope']
    assert result['punch_through']['capacity_drop']
    assert len(result['zero_environment_statics']['reactions_kn']) == 3
    assert result['environment']['depth']['status'] == 'illustrative assumption'
    assert result['environment']['era5']['status'] == 'illustrative assumption'
    assert len(result['environment_cases']) == 3
    assert all(case['actions']['horizontal_force_kn'] > 0 for case in result['environment_cases'])
    assert all(case['qualification'] == 'illustrative_only' for case in result['environment_cases'])


def test_gumbel_moments_and_return_quantile_independently():
    # NIST e-Handbook, Extreme Value Type I, maximum distribution and moments.
    result = gumbel_return_levels([2.0, 4.0, 6.0], [2, 50, 100])
    beta = 2 * math.sqrt(6) / math.pi  # sample standard deviation = 2
    mu = 4 - 0.5772156649015329 * beta
    assert result['scale'] == pytest.approx(beta)
    for period in (2, 50, 100):
        expected = mu - beta * math.log(-math.log(1 - 1 / period))
        assert result['return_levels'][str(period)] == pytest.approx(expected)
    assert result['qualification'] == 'illustrative_extrapolation'


@pytest.mark.parametrize('maxima,periods', [
    ([2, 3], [50]), ([2, 2, 2], [50]), ([2, float('nan'), 4], [50]),
    ([2, True, 4], [50]), ([2, -1, 4], [50]), ([2, 3, 4], [1]),
    ([2, 3, 4], [float('inf')]), ([2, 3, 4], []),
])
def test_invalid_extreme_fit_is_rejected(maxima, periods):
    with pytest.raises(ValueError):
        gumbel_return_levels(maxima, periods)


@pytest.mark.parametrize('corrupted_file', ['environment.json', 'ndbc_annual_maxima.txt'])
def test_environment_extract_integrity_and_replayed_extremes(tmp_path, corrupted_file):
    root = Path(__file__).resolve().parents[2] / 'examples/data/jackup_screening'
    result = frozen_environment(root)
    assert result['noaa']['year_range'] == [2005, 2024]
    assert result['noaa']['fit_years'] == [2005, 2006, 2007, 2008, 2013, 2016, 2017, 2018, 2021, 2022, 2024]
    wave = result['noaa']['wave_height']['return_levels']
    wind = result['noaa']['wind_speed']['return_levels']
    assert round(wave['50'], 1) == 8.3
    assert round(wave['100'], 1) == 8.9
    assert round(wind['50'], 1) == 25.6
    assert round(wind['100'], 1) == 27.0
    assert result['depth']['value'] == 60.0
    assert result['era5']['basis']
    for name in ('manifest.json', 'environment.json', 'ndbc_annual_maxima.txt'):
        (tmp_path / name).write_bytes((root / name).read_bytes())
    (tmp_path / corrupted_file).write_text('corrupted')
    with pytest.raises(ValueError, match='digest'):
        frozen_environment(tmp_path)


def test_example_consumes_existing_clients_and_depth_in_loads():
    import importlib.util
    from digitalmodel.data_systems.data_procurement.metocean.api_clients import (
        ERA5Client, GEBCOClient, NOAAClient,
    )
    from types import SimpleNamespace

    class Ocean(NOAAClient):
        def stream_lines(self, endpoint, **kwargs):
            yield '2024 01 01 00 00 MM 10 MM 2 MM MM MM MM'

    class Reanalysis(ERA5Client):
        def __init__(self):
            # Synthetic transport fixture; no authentication or network use.
            pass

        def _stream_era5_data(self, payload, parameters):
            assert '10m_u_component_of_wind' in payload['variable']
            yield {'timestamp': datetime(2024, 1, 1), 'wave_height': 3, 'wind_speed_10m': 12}

    class Depth(GEBCOClient):
        elevation = -60

        def get(self, endpoint, **kwargs):
            assert endpoint == 'mapserv'
            assert kwargs['params']['request'] == 'GetFeatureInfo'
            assert kwargs['params']['query_layers'] == 'GEBCO_LATEST_2'
            assert [float(v) for v in kwargs['params']['bbox'].split(',')] == pytest.approx(
                [27.8, -95.5, 28.0, -95.3])
            return SimpleNamespace(text=f'Elevation: {self.elevation} m')

    root = Path(__file__).resolve().parents[2]
    spec = importlib.util.spec_from_file_location(
        'jackup_example_live', root / 'examples/domains/geotechnical/jackup_screening.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    with Ocean() as ocean, Depth() as depth:
        clients = (ocean, Reanalysis(), depth)
        shallow = module.run_example(public_clients=clients)
        depth.elevation = -80
        deep = module.run_example(public_clients=clients)
    assert shallow['environment']['water_depth_m'] == 60
    assert shallow['environment']['era5']['wind_speed']['maximum'] == 12
    shallow_moment = shallow['environment_cases'][0]['actions']['moment_y_knm']
    deep_moment = deep['environment_cases'][0]['actions']['moment_y_knm']
    assert shallow_moment != deep_moment
    assert shallow['environment']['load_assumptions']['current_m_s']['status'] == 'illustrative assumption'


def test_unparseable_gebco_response_is_rejected_by_client_adapter():
    from types import SimpleNamespace
    from digitalmodel.data_systems.data_procurement.metocean.api_clients import GEBCOClient

    class NoResult(GEBCOClient):
        def get(self, endpoint, **kwargs):
            return SimpleNamespace(text='GetFeatureInfo results:\n\n  Search returned no results.\n')

    with NoResult() as depth:
        with pytest.raises(ValueError, match='positive.*depth'):
            query_public_inputs(object(), object(), depth,
                                datetime(2024, 1, 1), datetime(2025, 1, 1),
                                {'lat': 27.9, 'lon': -95.4}, '42019')


def test_source_retrieval_receipt_rejects_changed_bytes(tmp_path):
    import importlib.util

    root = Path(__file__).resolve().parents[2]
    spec = importlib.util.spec_from_file_location(
        'freeze_environment', root / 'examples/domains/geotechnical/freeze_jackup_environment.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    raw = b'1 2 3\n'
    (tmp_path / 'sample.txt').write_bytes(raw)
    (tmp_path / 'sample.txt.receipt.json').write_text(json.dumps({
        'http_status': 200, 'sha256': hashlib.sha256(raw).hexdigest()}))
    assert module.receipt(tmp_path, 'sample.txt')['http_status'] == 200
    (tmp_path / 'sample.txt').write_bytes(b'1 2 4\n')
    with pytest.raises(ValueError, match='source response digest mismatch'):
        module.receipt(tmp_path, 'sample.txt')
