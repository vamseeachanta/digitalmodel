"""Offline tests for the gis.imagery AOI + manifest stages (no network)."""

from __future__ import annotations

import json

import pytest

from digitalmodel.gis.imagery import ImageryManifest
from digitalmodel.gis.imagery.aoi import build_aoi, local_utm_epsg, prepare_all

PIPING_ROCK = {
    "version": 1,
    "created_for_issue": 2538,
    "coordinate_crs": "EPSG:4326",
    "output_root": "outputs",
    "cache_root": "cache",
    "log_root": "logs",
    "defaults": {"property_radius_miles": 0.20, "neighborhood_radius_miles": 1.50},
    "addresses": [
        {
            "label": "11511 Piping Rock Dr",
            "slug": "11511-piping-rock",
            "address": "11511 Piping Rock Dr., Houston, TX 77077",
            "latitude": 29.7397219,
            "longitude": -95.5971637,
            "geocode_source": "issue_body_nominatim",
        }
    ],
}


def test_local_utm_epsg_houston_is_zone_15n():
    # Houston (~-95.6 lon, +29.7 lat) falls in UTM zone 15 North -> EPSG:32615.
    assert local_utm_epsg(29.7397219, -95.5971637) == 32615


def test_local_utm_epsg_southern_hemisphere():
    # Southern-hemisphere points use the 327xx band.
    assert local_utm_epsg(-33.86, 151.20) == 32756


def _write_manifest(tmp_path):
    manifest_path = tmp_path / "addresses.json"
    manifest_path.write_text(json.dumps(PIPING_ROCK))
    return manifest_path


def test_manifest_roots_resolved_relative_to_file(tmp_path):
    manifest = ImageryManifest.from_file(_write_manifest(tmp_path))
    # Relative roots in the manifest are resolved against the manifest's dir.
    assert manifest.output_root == (tmp_path / "outputs").resolve()
    assert manifest.created_for_issue == 2538


def test_manifest_radius_falls_back_to_defaults(tmp_path):
    manifest = ImageryManifest.from_file(_write_manifest(tmp_path))
    addr = manifest.addresses[0]
    assert manifest.property_radius(addr) == pytest.approx(0.20)
    assert manifest.neighborhood_radius(addr) == pytest.approx(1.50)


def test_build_aoi_writes_expected_geojson(tmp_path):
    manifest = ImageryManifest.from_file(_write_manifest(tmp_path))
    addr = manifest.addresses[0]
    meta = build_aoi(manifest, addr)

    slug = "11511-piping-rock"
    aoi_dir = manifest.output_root / slug / "aoi"
    for name in (f"{slug}-point.geojson", f"{slug}-property-aoi.geojson", f"{slug}-neighborhood-aoi.geojson"):
        assert (aoi_dir / name).exists(), name

    # Neighborhood bbox must be wider than the property bbox (1.5 mi vs 0.2 mi).
    prop = meta["aoi"]["property"]["bounds_wgs84"]
    nbhd = meta["aoi"]["neighborhood"]["bounds_wgs84"]
    prop_width = prop[2] - prop[0]
    nbhd_width = nbhd[2] - nbhd[0]
    assert nbhd_width > prop_width
    assert meta["aoi"]["neighborhood"]["buffer_crs"] == "EPSG:32615"


def test_prepare_all_writes_batch_status(tmp_path):
    manifest = ImageryManifest.from_file(_write_manifest(tmp_path))
    batch = prepare_all(manifest)
    assert batch["issue"] == 2538
    assert batch["addresses_total"] == 1
    assert batch["addresses"][0]["status"] == "complete"
    assert (manifest.output_root / "batch-status.json").exists()
