# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Digest-checked jack-up inputs and strict metocean client consumption."""

from __future__ import annotations

import hashlib
import json
import math
from collections.abc import Iterable
from pathlib import Path


def frozen_inputs(directory: Path) -> dict:
    """Read a numerical input snapshot only after verifying its byte digest."""
    directory = Path(directory)
    manifest = json.loads((directory / 'manifest.json').read_text(encoding='utf-8'))
    raw = (directory / 'inputs.json').read_bytes()
    expected = manifest['files']['inputs.json']
    if hashlib.sha256(raw).hexdigest() != expected:
        raise ValueError('frozen input digest mismatch')
    return json.loads(raw)


def observation_summary(records: Iterable[dict], *, wind_key: str = 'wind_speed') -> dict:
    """Summarize measured sample maxima, rejecting common NDBC sentinels.

    Neither a station catalogue nor a sampled maximum supplies a return period.
    No joint extreme distribution, completeness qualification or extrapolation
    is inferred. Missing fields remain missing and each parameter has a count.
    """
    count = 0
    values: dict[str, list[float]] = {'wave_height': [], 'wind_speed': []}
    timestamps = []
    for record in records:
        count += 1
        if record.get('timestamp') is not None:
            timestamps.append(record['timestamp'].isoformat())
        for output_key, source_key in (('wave_height', 'wave_height'), ('wind_speed', wind_key)):
            value = record.get(source_key)
            if (isinstance(value, (int, float)) and not isinstance(value, bool)
                    and math.isfinite(value) and 0 <= value < 99):
                values[output_key].append(float(value))
    if not count or any(not samples for samples in values.values()):
        raise ValueError('metocean sample lacks usable wave or wind values')
    return {
        'status': 'observed_sample_only', 'return_period_years': None,
        'record_count': count,
        'first_timestamp': min(timestamps) if timestamps else None,
        'last_timestamp': max(timestamps) if timestamps else None,
        **{key: {'maximum': max(samples), 'valid_count': len(samples),
                 'missing_count': count - len(samples)} for key, samples in values.items()},
        'units': {'wave_height': 'm', 'wind_speed': 'm/s'},
        'coverage_qualification': 'not_evaluated',
    }


def query_public_inputs(noaa_client, era5_client, gebco_client, start_date, end_date,
                        location: dict, station_id: str) -> dict:
    """Consume the existing NOAA, ERA5 and GEBCO client interfaces.

    Clients must already be configured by the caller. This function reads no
    credentials and implements no automatic provider substitution. The existing
    GEBCO client returns zero on parse failure; zero is rejected for this offshore
    calculation. Returned sample maxima are not qualified design extremes.
    """
    if era5_client is None:
        raise ValueError('ERA5 input is unavailable; no automatic substitution is permitted')
    if noaa_client is None or gebco_client is None:
        raise ValueError('NOAA and GEBCO clients are required')
    if end_date <= start_date:
        raise ValueError('end date must follow start date')
    lat, lon = location['lat'], location['lon']
    if (not math.isfinite(lat) or not math.isfinite(lon)
            or not -90 <= lat <= 90 or not -180 <= lon <= 180):
        raise ValueError('location must have finite geographic coordinates')
    depth = gebco_client.get_depth(lat, lon)
    if isinstance(depth, bool) or not math.isfinite(depth) or depth <= 0:
        raise ValueError('a positive, qualified offshore water depth is required')
    noaa = observation_summary(noaa_client.query_by_date(
        start_date=start_date, end_date=end_date, station_id=station_id))
    era5 = observation_summary(era5_client.query_by_date(
        start_date=start_date, end_date=end_date, location=location,
        parameters=['wave_height', 'wind_speed_10m']), wind_key='wind_speed_10m')
    return {'water_depth_m': depth, 'noaa': noaa, 'era5': era5,
            'return_levels_status': 'not_evaluated'}
