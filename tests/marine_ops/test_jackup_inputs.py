# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Frozen-input integrity and explicit missing-source failures."""

import hashlib
import json
from datetime import datetime
from pathlib import Path

import pytest

from digitalmodel.marine_ops.jackup_inputs import (
    frozen_inputs,
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
    assert result['environment_status'] == 'not_evaluated'
    assert result['operating_envelope_status'] == 'synthetic_load_sensitivity_only'
    assert result['curve']
    assert result['static_envelope']
    assert result['punch_through']['capacity_drop']
    assert len(result['zero_environment_statics']['reactions_kn']) == 3
