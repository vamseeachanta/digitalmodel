#!/usr/bin/env python3
"""Probe public imagery access for issue #2538 without requiring Earth Engine auth."""
from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path

import requests

WORKDIR = Path('/mnt/local-analysis/ace2-gis-timelapse')
META = WORKDIR / 'outputs/11511-piping-rock/manifests/11511-piping-rock-aoi-metadata.json'
OUT = WORKDIR / 'outputs/11511-piping-rock/reports/imagery-access-probe.json'

STAC_URL = 'https://planetarycomputer.microsoft.com/api/stac/v1/search'
COLLECTIONS = {
    'naip': {'datetime': '2003-01-01/2026-12-31'},
    'landsat-c2-l2': {'datetime': '1984-01-01/2026-12-31'},
    'sentinel-2-l2a': {'datetime': '2015-01-01/2026-12-31'},
}


def stac_search(collection: str, bbox: list[float], dt: str) -> dict:
    payload = {
        'collections': [collection],
        'bbox': bbox,
        'datetime': dt,
        'limit': 10,
    }
    r = requests.post(STAC_URL, json=payload, timeout=45)
    info = {
        'http_status': r.status_code,
        'ok': r.ok,
        'url': STAC_URL,
        'collection': collection,
    }
    if not r.ok:
        info['error'] = r.text[:1000]
        return info
    data = r.json()
    features = data.get('features', [])
    years = sorted({(f.get('properties', {}).get('datetime') or '')[:4] for f in features if f.get('properties', {}).get('datetime')})
    sample = []
    for f in features[:3]:
        props = f.get('properties', {})
        assets = f.get('assets', {})
        sample.append({
            'id': f.get('id'),
            'datetime': props.get('datetime'),
            'eo_cloud_cover': props.get('eo:cloud_cover'),
            'asset_keys': sorted(list(assets.keys()))[:12],
        })
    info.update({'matched_returned': len(features), 'sample_years_returned': years, 'sample_items': sample})
    return info


def main() -> None:
    meta = json.loads(META.read_text())
    bbox = meta['aoi']['neighborhood']['bounds_wgs84']
    result = {
        'generated_at': datetime.now(timezone.utc).isoformat(),
        'address_slug': meta['slug'],
        'address': meta['address'],
        'bbox_used': bbox,
        'earth_engine': {},
        'planetary_computer_stac': {},
        'interpretation': [],
    }
    try:
        import ee
        try:
            ee.Initialize()
            result['earth_engine'] = {'initialize': 'ok', 'probe': ee.Number(1).getInfo()}
        except Exception as e:
            result['earth_engine'] = {'initialize': 'error', 'error_type': type(e).__name__, 'error': str(e)[:1000]}
    except Exception as e:
        result['earth_engine'] = {'import': 'error', 'error_type': type(e).__name__, 'error': str(e)[:1000]}

    for collection, cfg in COLLECTIONS.items():
        try:
            result['planetary_computer_stac'][collection] = stac_search(collection, bbox, cfg['datetime'])
        except Exception as e:
            result['planetary_computer_stac'][collection] = {'ok': False, 'error_type': type(e).__name__, 'error': str(e)[:1000]}

    if result['earth_engine'].get('initialize') == 'error':
        result['interpretation'].append('Earth Engine Python libraries are installed, but account authentication is missing; run earthengine authenticate or use public STAC fallback.')
    for collection, probe in result['planetary_computer_stac'].items():
        if probe.get('ok') and probe.get('matched_returned', 0) > 0:
            result['interpretation'].append(f'{collection}: public STAC returned sample items; viable for automated discovery/rendering with follow-up asset signing/download.')
        else:
            result['interpretation'].append(f'{collection}: no sample items returned or probe failed; document as blocker/fallback candidate.')
    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(result, indent=2))
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
