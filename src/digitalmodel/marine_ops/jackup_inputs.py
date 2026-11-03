# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Digest-checked jack-up inputs and strict metocean client consumption."""

from __future__ import annotations

import hashlib
import json
import math
import statistics
from collections.abc import Iterable
from datetime import datetime
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
    GEBCO client rejects missing, malformed and nonfinite elevations; zero depth
    is also rejected for this offshore calculation. Returned sample maxima are
    not qualified design extremes.
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


def gumbel_return_levels(maxima: Iterable[float], return_periods=(50, 100)) -> dict:
    """Fit annual block maxima with the Gumbel maximum method of moments.

    NIST/SEMATECH e-Handbook, Extreme Value Type I Distribution, parameter
    estimation and percent point function:
    https://www.itl.nist.gov/div898/handbook/eda/section3/eda366g.htm
    beta = s sqrt(6)/pi; mu = mean - EulerGamma beta;
    x(T) = mu - beta log(-log(1-1/T)), for annual exceedance probability 1/T.
    Sample standard deviation uses n-1. Independence, stationarity, completeness
    and distribution suitability require external qualification. Marginal wind
    and wave return levels do not define a joint environmental return period.
    """
    values, periods = list(maxima), list(return_periods)
    if len(values) < 3 or not periods:
        raise ValueError('at least three annual maxima and a return period are required')
    for value in values:
        if isinstance(value, bool) or not isinstance(value, (int, float)) or not math.isfinite(value) or value < 0:
            raise ValueError('annual maxima must be finite nonnegative numbers')
    for period in periods:
        if isinstance(period, bool) or not isinstance(period, (int, float)) or not math.isfinite(period) or period <= 1:
            raise ValueError('return periods must be finite and greater than one year')
    scale = statistics.stdev(values) * math.sqrt(6) / math.pi
    if scale <= 0:
        raise ValueError('annual maxima must have nonzero variation')
    location = statistics.mean(values) - 0.5772156649015329 * scale
    return {
        'method': 'Gumbel maximum; method of moments; annual blocks',
        'source': 'https://www.itl.nist.gov/div898/handbook/eda/section3/eda366g.htm',
        'qualification': 'illustrative_extrapolation',
        'block_count': len(values), 'location': location, 'scale': scale,
        'return_levels': {str(period): location - scale * math.log(-math.log1p(-1 / period))
                          for period in periods},
    }


def frozen_environment(directory: Path) -> dict:
    """Verify the extract, replay the NOAA client and compute annual extremes.

    The extract retains the wind and wave maximum rows in each calendar year.
    Counts and valid-day coverage describe the full source archives, not this
    deliberately reduced extract. The 330-day screening threshold is an
    illustrative selection rule, not a source completeness acceptance criterion.
    ERA5 and depth assumptions are explicit metadata, never provider observations.
    """
    from digitalmodel.data_systems.data_procurement.metocean.api_clients import NOAAClient

    directory = Path(directory)
    manifest = json.loads((directory / 'manifest.json').read_text(encoding='utf-8'))
    verified = {}
    for name in ('environment.json', 'ndbc_annual_maxima.txt'):
        raw = (directory / name).read_bytes()
        if hashlib.sha256(raw).hexdigest() != manifest['files'][name]:
            raise ValueError('frozen environmental input digest mismatch')
        verified[name] = raw.decode('utf-8')
    metadata = json.loads(verified['environment.json'])

    class ExtractClient(NOAAClient):
        def stream_lines(self, endpoint, **kwargs):
            year = int(endpoint.rsplit('h', 1)[1].split('.')[0])
            for line in verified['ndbc_annual_maxima.txt'].splitlines():
                if line.split() and line.split()[0] == str(year):
                    yield line

    first, last = metadata['noaa']['year_range']
    with ExtractClient() as client:
        records = list(client.query_by_date(
            datetime(first, 1, 1), datetime(last, 12, 31, 23, 59),
            station_id=metadata['noaa']['station']))
    selected, excluded = [], []
    maxima: dict[str, list[float]] = {'wave_height': [], 'wind_speed': []}
    threshold = metadata['noaa']['minimum_valid_days']
    for coverage in metadata['noaa']['coverage']:
        year = coverage['year']
        summary = observation_summary(r for r in records if r['timestamp'].year == year)
        if min(coverage['wave_valid_days'], coverage['wind_valid_days']) < threshold:
            excluded.append(year)
            continue
        selected.append(year)
        for key in maxima:
            maxima[key].append(summary[key]['maximum'])
    return {
        **metadata,
        'noaa': {**metadata['noaa'], 'fit_years': selected, 'excluded_years': excluded,
                 **{key: gumbel_return_levels(values) for key, values in maxima.items()}},
        'return_levels_status': 'illustrative_gumbel_annual_maxima',
    }
