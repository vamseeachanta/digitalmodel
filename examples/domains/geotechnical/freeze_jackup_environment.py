# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Retrieve public evidence externally, then freeze selected observation facts.

Usage: python freeze_jackup_environment.py fetch EXTERNAL_DIRECTORY
       python freeze_jackup_environment.py freeze EXTERNAL_DIRECTORY OUTPUT

Retrieval uses curl, one request at a time, with no authentication. Existing
responses are reused only when their recorded byte digest matches. Source
archives and response bodies remain external. Only maximum observation rows,
counts, provenance and explicit assumptions are written to OUTPUT.
"""

import argparse
import gzip
import hashlib
import json
import math
import subprocess
from datetime import datetime, timezone
from pathlib import Path

from digitalmodel.marine_ops.jackup_inputs import frozen_inputs

NDBC = 'https://www.ndbc.noaa.gov/data/historical/stdmet/'
GEBCO = ('https://wms.gebco.net/mapserv?service=WMS&version=1.3.0'
         '&request=GetFeatureInfo&layers=GEBCO_LATEST_2&query_layers=GEBCO_LATEST_2'
         '&info_format=text/plain&i=50&j=50&width=101&height=101&crs=EPSG:4326'
         '&bbox=27.8,-95.5,28.0,-95.3&styles=&format=image/png')
EVIDENCE = {
    'gebco-feature.txt': GEBCO,
    'gebco-capabilities.xml': 'https://wms.gebco.net/mapserv?request=getcapabilities&service=wms&version=1.3.0',
    'era5-access.html': 'https://cds.climate.copernicus.eu/how-to-api',
}
INPUTS = Path(__file__).resolve().parents[2] / 'data/jackup_screening'


def digest(raw):
    """Return the SHA-256 of exact bytes."""
    return hashlib.sha256(raw).hexdigest()


def receipt(directory, name):
    """Read provenance only after checking the retained source bytes."""
    provenance = json.loads((directory / f'{name}.receipt.json').read_text(encoding='utf-8'))
    if digest((directory / name).read_bytes()) != provenance['sha256']:
        raise ValueError('source response digest mismatch')
    if provenance['http_status'] != 200:
        raise ValueError('source retrieval did not succeed')
    return provenance


def fetch(directory):
    """Fetch archives and public access evidence, resuming verified responses."""
    directory.mkdir(parents=True, exist_ok=True)
    sources = {f'42019h{year}.txt.gz': f'{NDBC}42019h{year}.txt.gz'
               for year in range(2005, 2025)}
    sources.update(EVIDENCE)
    for name, url in sources.items():
        if (directory / f'{name}.receipt.json').exists():
            receipt(directory, name)
            continue
        target = directory / name
        response = subprocess.run(
            ['curl', '-L', '--max-time', '45', '-sS', '-o', str(target),
             '-w', '%{http_code}\n%{url_effective}\n', url],
            capture_output=True, text=True, check=True,
        )
        status, effective_url = response.stdout.splitlines()
        provenance = {'source': url, 'effective_url': effective_url,
                      'retrieved_at': datetime.now(timezone.utc).isoformat(),
                      'http_status': int(status), 'sha256': digest(target.read_bytes())}
        (directory / f'{name}.receipt.json').write_text(
            json.dumps(provenance, indent=2, sort_keys=True) + '\n', encoding='utf-8')
        receipt(directory, name)


def usable(record, key):
    """Exclude missing values and the NDBC 99 sentinel before taking maxima."""
    value = record.get(key)
    return isinstance(value, (int, float)) and not isinstance(value, bool) and math.isfinite(value) and 0 <= value < 99


def assumption(value, unit, basis):
    """Label a sensitivity input without attributing it to a data provider."""
    return {'value': value, 'unit': unit, 'status': 'illustrative assumption', 'basis': basis}


def freeze(directory, output):
    """Reduce complete archives through the existing NOAA client date parser."""
    from digitalmodel.data_systems.data_procurement.metocean.api_clients import NOAAClient

    class ArchiveClient(NOAAClient):
        def stream_lines(self, endpoint, **kwargs):
            with gzip.open(directory / (endpoint.rsplit('/', 1)[1] + '.gz'),
                           'rt', encoding='ascii') as stream:
                yield from stream

    sources, coverage, extract = [], [], []
    for year in range(2005, 2025):
        name = f'42019h{year}.txt.gz'
        provenance = receipt(directory, name)
        if provenance['source'] != NDBC + name:
            raise ValueError('archive source does not match the requested station and year')
        sources.append(provenance)
        with ArchiveClient() as client:
            records = list(client.query_by_date(
                datetime(year, 1, 1), datetime(year, 12, 31, 23, 59), station_id='42019'))
        row = {'year': year, 'record_count': len(records)}
        selected = {}
        for label, key in (('wave', 'wave_height'), ('wind', 'wind_speed')):
            valid = [r for r in records if usable(r, key)]
            if not valid:
                raise ValueError('annual archive contains no usable observations')
            row[f'{label}_valid_count'] = len(valid)
            row[f'{label}_valid_days'] = len({r['timestamp'].date() for r in valid})
            maximum = max(valid, key=lambda r: r[key])
            selected[maximum['timestamp']] = maximum
        coverage.append(row)
        for timestamp, record in sorted(selected.items()):
            wave = str(record['wave_height']) if usable(record, 'wave_height') else 'MM'
            wind = str(record['wind_speed']) if usable(record, 'wind_speed') else 'MM'
            # Only selected date, wave and wind facts; unused stdmet fields are MM.
            extract.append(f'{timestamp:%Y %m %d %H %M} MM {wind} MM {wave} MM MM MM MM')
    access = {name: receipt(directory, name) for name in EVIDENCE}
    if any(access[name]['source'] != url for name, url in EVIDENCE.items()):
        raise ValueError('access evidence source mismatch')
    # The frozen response is expected to contain no numerical GEBCO result.
    # If the service changes, require qualification instead of retaining an assumption.
    if 'Search returned no results.' not in (directory / 'gebco-feature.txt').read_text(encoding='utf-8'):
        raise ValueError('GEBCO response changed; qualify the numerical lookup before freezing')
    original = frozen_inputs(INPUTS)
    observed = original['environment']['noaa']['summary']
    data = {
        'qualification': 'generic_screening_only',
        'location': {'lat': 27.9, 'lon': -95.4},
        'location_basis': 'Rounded public station coordinates; not the soil profile location.',
        'noaa': {
            'station': '42019', 'year_range': [2005, 2024], 'sources': sources,
            'coverage': coverage, 'minimum_valid_days': 330,
            'selection_basis': 'Illustrative threshold: both parameters observed on at least 330 distinct UTC days. Within-day gaps, station changes and storm censoring remain unqualified.',
            'extract_basis': 'First row attaining each annual wave or wind maximum; deduplicated by timestamp. Counts refer to complete source archives.',
            'reuse': 'Selected United States government observation facts only; no original archive is copied.',
        },
        'era5': {
            'status': 'illustrative assumption',
            'wave_height_m': observed['wave_height']['maximum'],
            'wind_speed_10m_m_s': observed['wind_speed']['maximum'],
            'basis': 'Original demonstrator NDBC 2024 sample maxima reused as a sensitivity scenario; these are not ERA5 estimates and have no assigned return period.',
            'unavailable_reason': 'Existing ERA5 client requires authenticated access; no authentication was used. Public API instructions confirm a personal access token is required.',
            'source': EVIDENCE['era5-access.html'], 'access_evidence': access['era5-access.html'],
        },
        'depth': assumption(60.0, 'm', 'Positive shelf-depth sensitivity chosen to exercise load transfer; the GEBCO numerical query returned no results. No measured depth is inferred.'),
        'gebco': {'status': 'unavailable', 'source': GEBCO,
                  'access_evidence': access['gebco-feature.txt'],
                  'capabilities_evidence': access['gebco-capabilities.xml'],
                  'reason': 'Current GetFeatureInfo query returned no numerical result; no map colour is interpreted as elevation.'},
        'load_assumptions': {
            'tp_s': assumption(12.0, 's', 'Illustrative period, independent of marginal wave and wind extremes.'),
            'current_m_s': assumption(0.5, 'm/s', 'Illustrative aligned current; no return period is assigned.'),
            'air_gap_m': assumption(10.0, 'm', 'Illustrative wind application height above water.'),
            'wind_area_m2': assumption(1000.0, 'm2', 'Illustrative effective drag area; no actual structure is represented.'),
            'submerged_area_m2': assumption(300.0, 'm2', 'Illustrative effective water drag area; no actual members are represented.'),
            'wind_drag_coefficient': assumption(1.2, '1', 'Illustrative sensitivity coefficient.'),
            'water_drag_coefficient': assumption(0.9, '1', 'Illustrative sensitivity coefficient.'),
        },
        'limits': [
            'Annual marginal Gumbel extrapolations are illustrative, without fit or stationarity qualification.',
            'Combining marginal wind and wave levels does not establish a joint return period.',
            'Within-day gaps and missing storm peaks may bias fitted extremes downward.',
            'ERA5 scenario, depth, current and load geometry remain illustrative assumptions.',
        ],
    }
    output.mkdir(parents=True, exist_ok=True)
    manifest = json.loads((INPUTS / 'manifest.json').read_text(encoding='utf-8'))
    payloads = {'environment.json': json.dumps(data, indent=2, sort_keys=True, allow_nan=False) + '\n',
                'ndbc_annual_maxima.txt': '\n'.join(extract) + '\n'}
    for name, content in payloads.items():
        raw = content.encode('utf-8')
        (output / name).write_bytes(raw)
        manifest['files'][name] = digest(raw)
    manifest['external_source_digests'].update({
        f'ndbc_{year}_archive': item['sha256'] for year, item in zip(range(2005, 2025), sources)})
    (output / 'manifest.json').write_text(json.dumps(manifest, indent=2, sort_keys=True) + '\n', encoding='utf-8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('mode', choices=('fetch', 'freeze'))
    parser.add_argument('external_directory', type=Path)
    parser.add_argument('output', type=Path, nargs='?')
    args = parser.parse_args()
    if args.mode == 'fetch':
        fetch(args.external_directory)
    elif args.output is None:
        parser.error('freeze requires an output directory')
    else:
        freeze(args.external_directory, args.output)
