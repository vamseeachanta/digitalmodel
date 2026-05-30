#!/usr/bin/env python3
"""Prepare AOI buffers for issue #2538 multi-address timelapse workflow."""
from __future__ import annotations

import json
import math
from datetime import datetime, timezone
from pathlib import Path

import geopandas as gpd
from shapely.geometry import Point, mapping

WORKDIR = Path('/mnt/local-analysis/ace2-gis-timelapse')
MANIFEST = WORKDIR / 'addresses.json'
MILES_TO_METERS = 1609.344


def local_utm_epsg(lat: float, lon: float) -> int:
    zone = int(math.floor((lon + 180.0) / 6.0) + 1)
    return (32600 if lat >= 0 else 32700) + zone


def feature_collection(geometry, properties):
    return {
        'type': 'FeatureCollection',
        'features': [
            {
                'type': 'Feature',
                'geometry': mapping(geometry),
                'properties': properties,
            }
        ],
    }


def main() -> None:
    manifest = json.loads(MANIFEST.read_text())
    output_root = Path(manifest['output_root'])
    cache_root = Path(manifest['cache_root'])
    log_root = Path(manifest['log_root'])
    now = datetime.now(timezone.utc).isoformat()

    statuses = []
    for addr in manifest['addresses']:
        slug = addr['slug']
        lat = float(addr['latitude'])
        lon = float(addr['longitude'])
        utm = f"EPSG:{local_utm_epsg(lat, lon)}"
        out_dir = output_root / slug
        aoi_dir = out_dir / 'aoi'
        manifest_dir = out_dir / 'manifests'
        report_dir = out_dir / 'reports'
        frame_dirs = [out_dir / 'frames' / 'property', out_dir / 'frames' / 'neighborhood']
        media_dir = out_dir / 'media'
        cache_dir = cache_root / slug
        for d in [aoi_dir, manifest_dir, report_dir, media_dir, cache_dir, log_root, *frame_dirs]:
            d.mkdir(parents=True, exist_ok=True)

        gdf = gpd.GeoDataFrame(
            [{'slug': slug, 'label': addr['label'], 'address': addr['address']}],
            geometry=[Point(lon, lat)],
            crs='EPSG:4326',
        )
        metric = gdf.to_crs(utm)
        outputs = {}
        for scale, radius_key in [('property', 'property_radius_miles'), ('neighborhood', 'neighborhood_radius_miles')]:
            radius_miles = float(addr[radius_key])
            buffer_metric = metric.geometry.iloc[0].buffer(radius_miles * MILES_TO_METERS)
            buffer_wgs84 = gpd.GeoSeries([buffer_metric], crs=utm).to_crs('EPSG:4326').iloc[0]
            props = {
                'slug': slug,
                'label': addr['label'],
                'address': addr['address'],
                'scale': scale,
                'radius_miles': radius_miles,
                'radius_meters': radius_miles * MILES_TO_METERS,
                'center_latitude': lat,
                'center_longitude': lon,
                'source_crs': 'EPSG:4326',
                'buffer_crs': utm,
                'generated_at': now,
            }
            geojson_path = aoi_dir / f'{slug}-{scale}-aoi.geojson'
            geojson_path.write_text(json.dumps(feature_collection(buffer_wgs84, props), indent=2))
            outputs[scale] = {
                'geojson': str(geojson_path),
                'radius_miles': radius_miles,
                'radius_meters': radius_miles * MILES_TO_METERS,
                'bounds_wgs84': list(buffer_wgs84.bounds),
                'buffer_crs': utm,
            }

        point_path = aoi_dir / f'{slug}-point.geojson'
        point_path.write_text(json.dumps(feature_collection(Point(lon, lat), {
            'slug': slug,
            'label': addr['label'],
            'address': addr['address'],
            'latitude': lat,
            'longitude': lon,
            'geocode_source': addr.get('geocode_source', 'unknown'),
            'generated_at': now,
        }), indent=2))

        metadata = {
            'slug': slug,
            'label': addr['label'],
            'address': addr['address'],
            'latitude': lat,
            'longitude': lon,
            'geocode_source': addr.get('geocode_source', 'unknown'),
            'generated_at': now,
            'point_geojson': str(point_path),
            'aoi': outputs,
            'output_dirs': {
                'aoi': str(aoi_dir),
                'frames_property': str(frame_dirs[0]),
                'frames_neighborhood': str(frame_dirs[1]),
                'media': str(media_dir),
                'reports': str(report_dir),
                'manifests': str(manifest_dir),
                'cache': str(cache_dir),
            },
        }
        metadata_path = manifest_dir / f'{slug}-aoi-metadata.json'
        metadata_path.write_text(json.dumps(metadata, indent=2))
        statuses.append({'slug': slug, 'stage': 'aoi_prepared', 'status': 'complete', 'metadata': str(metadata_path)})

    batch_status = {
        'issue': 2538,
        'generated_at': now,
        'stage': 'aoi_preparation',
        'addresses_total': len(manifest['addresses']),
        'addresses': statuses,
    }
    (WORKDIR / 'batch-status.json').write_text(json.dumps(batch_status, indent=2))
    print(json.dumps(batch_status, indent=2))


if __name__ == '__main__':
    main()
