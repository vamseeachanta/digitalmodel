"""Area-of-interest (AOI) buffer generation for imagery timelapses.

Builds point, property-scale and neighborhood-scale AOI buffers around a
geocoded address and writes them as WGS84 GeoJSON plus a metadata manifest.
Buffers are computed in a local UTM projection so the radius is honoured in
metres, then reprojected back to WGS84.

Ported from the workspace-hub issue #2538 ``prepare_aoi.py`` script, with the
hard-coded working directory replaced by an :class:`ImageryManifest`.
"""

from __future__ import annotations

import json
import logging
import math
from datetime import datetime, timezone
from pathlib import Path

import geopandas as gpd
from shapely.geometry import Point, mapping

from digitalmodel.gis.imagery.manifest import AddressRecord, ImageryManifest

logger = logging.getLogger(__name__)

MILES_TO_METERS = 1609.344


def local_utm_epsg(lat: float, lon: float) -> int:
    """Return the EPSG code of the UTM zone containing ``(lat, lon)``."""
    zone = int(math.floor((lon + 180.0) / 6.0) + 1)
    return (32600 if lat >= 0 else 32700) + zone


def feature_collection(geometry, properties: dict) -> dict:
    """Wrap a single geometry + properties as a GeoJSON FeatureCollection."""
    return {
        "type": "FeatureCollection",
        "features": [
            {
                "type": "Feature",
                "geometry": mapping(geometry),
                "properties": properties,
            }
        ],
    }


def build_aoi(manifest: ImageryManifest, address: AddressRecord) -> dict:
    """Generate point + property/neighborhood AOI GeoJSON for one address.

    Returns the metadata dict (also written to ``manifests/<slug>-aoi-metadata.json``)
    describing the generated files and buffer bounds.
    """
    output_root = manifest.output_root
    cache_root = manifest.cache_root
    log_root = manifest.log_root
    now = datetime.now(timezone.utc).isoformat()

    slug = address.slug
    lat = float(address.latitude)
    lon = float(address.longitude)
    utm = f"EPSG:{local_utm_epsg(lat, lon)}"

    out_dir = output_root / slug
    aoi_dir = out_dir / "aoi"
    manifest_dir = out_dir / "manifests"
    report_dir = out_dir / "reports"
    frame_dirs = [out_dir / "frames" / "property", out_dir / "frames" / "neighborhood"]
    media_dir = out_dir / "media"
    cache_dir = cache_root / slug
    for d in [aoi_dir, manifest_dir, report_dir, media_dir, cache_dir, log_root, *frame_dirs]:
        d.mkdir(parents=True, exist_ok=True)

    gdf = gpd.GeoDataFrame(
        [{"slug": slug, "label": address.label, "address": address.address}],
        geometry=[Point(lon, lat)],
        crs="EPSG:4326",
    )
    metric = gdf.to_crs(utm)
    outputs: dict[str, dict] = {}
    radii = {
        "property": manifest.property_radius(address),
        "neighborhood": manifest.neighborhood_radius(address),
    }
    for scale, radius_miles in radii.items():
        buffer_metric = metric.geometry.iloc[0].buffer(radius_miles * MILES_TO_METERS)
        buffer_wgs84 = gpd.GeoSeries([buffer_metric], crs=utm).to_crs("EPSG:4326").iloc[0]
        props = {
            "slug": slug,
            "label": address.label,
            "address": address.address,
            "scale": scale,
            "radius_miles": radius_miles,
            "radius_meters": radius_miles * MILES_TO_METERS,
            "center_latitude": lat,
            "center_longitude": lon,
            "source_crs": "EPSG:4326",
            "buffer_crs": utm,
            "generated_at": now,
        }
        geojson_path = aoi_dir / f"{slug}-{scale}-aoi.geojson"
        geojson_path.write_text(json.dumps(feature_collection(buffer_wgs84, props), indent=2))
        outputs[scale] = {
            "geojson": str(geojson_path),
            "radius_miles": radius_miles,
            "radius_meters": radius_miles * MILES_TO_METERS,
            "bounds_wgs84": list(buffer_wgs84.bounds),
            "buffer_crs": utm,
        }

    point_path = aoi_dir / f"{slug}-point.geojson"
    point_path.write_text(
        json.dumps(
            feature_collection(
                Point(lon, lat),
                {
                    "slug": slug,
                    "label": address.label,
                    "address": address.address,
                    "latitude": lat,
                    "longitude": lon,
                    "geocode_source": address.geocode_source,
                    "generated_at": now,
                },
            ),
            indent=2,
        )
    )

    metadata = {
        "slug": slug,
        "label": address.label,
        "address": address.address,
        "latitude": lat,
        "longitude": lon,
        "geocode_source": address.geocode_source,
        "generated_at": now,
        "point_geojson": str(point_path),
        "aoi": outputs,
        "output_dirs": {
            "aoi": str(aoi_dir),
            "frames_property": str(frame_dirs[0]),
            "frames_neighborhood": str(frame_dirs[1]),
            "media": str(media_dir),
            "reports": str(report_dir),
            "manifests": str(manifest_dir),
            "cache": str(cache_dir),
        },
    }
    metadata_path = manifest_dir / f"{slug}-aoi-metadata.json"
    metadata_path.write_text(json.dumps(metadata, indent=2))
    logger.info("Prepared AOI for %s -> %s", slug, metadata_path)
    return metadata


def prepare_all(manifest: ImageryManifest) -> dict:
    """Build AOIs for every address in the manifest; write ``batch-status.json``."""
    now = datetime.now(timezone.utc).isoformat()
    statuses = []
    for address in manifest.addresses:
        metadata = build_aoi(manifest, address)
        statuses.append(
            {
                "slug": address.slug,
                "stage": "aoi_prepared",
                "status": "complete",
                "metadata": str(manifest.output_root / address.slug / "manifests" / f"{address.slug}-aoi-metadata.json"),
            }
        )
        _ = metadata
    batch_status = {
        "issue": manifest.created_for_issue,
        "generated_at": now,
        "stage": "aoi_preparation",
        "addresses_total": len(manifest.addresses),
        "addresses": statuses,
    }
    status_path = manifest.output_root / "batch-status.json"
    status_path.parent.mkdir(parents=True, exist_ok=True)
    status_path.write_text(json.dumps(batch_status, indent=2))
    return batch_status
