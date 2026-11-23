"""Imagery-source access probing for the timelapse pipeline.

Probes the Microsoft Planetary Computer STAC API for NAIP/Landsat/Sentinel-2
coverage of an AOI, and (optionally) checks whether Earth Engine is importable
and authenticated.  No credentials are required for the STAC probe; Earth Engine
is treated as an optional enhancement that degrades gracefully when unavailable.

Ported from the workspace-hub issue #2538 ``probe_imagery_access.py`` script.
"""

from __future__ import annotations

import json
import logging
from datetime import datetime, timezone
from pathlib import Path

import requests

logger = logging.getLogger(__name__)

STAC_URL = "https://planetarycomputer.microsoft.com/api/stac/v1/search"
COLLECTIONS = {
    "naip": {"datetime": "2003-01-01/2026-12-31"},
    "landsat-c2-l2": {"datetime": "1984-01-01/2026-12-31"},
    "sentinel-2-l2a": {"datetime": "2015-01-01/2026-12-31"},
}


def stac_search(collection: str, bbox: list[float], dt: str, limit: int = 10) -> dict:
    """Search one STAC collection over ``bbox``/``dt`` and summarise the result."""
    payload = {
        "collections": [collection],
        "bbox": bbox,
        "datetime": dt,
        "limit": limit,
    }
    r = requests.post(STAC_URL, json=payload, timeout=45)
    info: dict = {
        "http_status": r.status_code,
        "ok": r.ok,
        "url": STAC_URL,
        "collection": collection,
    }
    if not r.ok:
        info["error"] = r.text[:1000]
        return info
    data = r.json()
    features = data.get("features", [])
    years = sorted(
        {
            (f.get("properties", {}).get("datetime") or "")[:4]
            for f in features
            if f.get("properties", {}).get("datetime")
        }
    )
    sample = []
    for f in features[:3]:
        props = f.get("properties", {})
        assets = f.get("assets", {})
        sample.append(
            {
                "id": f.get("id"),
                "datetime": props.get("datetime"),
                "eo_cloud_cover": props.get("eo:cloud_cover"),
                "asset_keys": sorted(list(assets.keys()))[:12],
            }
        )
    info.update(
        {
            "matched_returned": len(features),
            "sample_years_returned": years,
            "sample_items": sample,
        }
    )
    return info


def probe_earth_engine() -> dict:
    """Report whether the Earth Engine SDK is importable and initialises."""
    try:
        import ee
    except Exception as e:  # noqa: BLE001 - report any import failure verbatim
        return {"import": "error", "error_type": type(e).__name__, "error": str(e)[:1000]}
    try:
        ee.Initialize()
        return {"initialize": "ok", "probe": ee.Number(1).getInfo()}
    except Exception as e:  # noqa: BLE001 - auth/init failures are expected & non-fatal
        return {"initialize": "error", "error_type": type(e).__name__, "error": str(e)[:1000]}


def probe_imagery_access(bbox: list[float], *, address: str | None = None, slug: str | None = None) -> dict:
    """Probe Earth Engine + Planetary Computer STAC coverage for ``bbox``."""
    result: dict = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "address_slug": slug,
        "address": address,
        "bbox_used": bbox,
        "earth_engine": probe_earth_engine(),
        "planetary_computer_stac": {},
        "interpretation": [],
    }
    for collection, cfg in COLLECTIONS.items():
        try:
            result["planetary_computer_stac"][collection] = stac_search(collection, bbox, cfg["datetime"])
        except Exception as e:  # noqa: BLE001 - record per-collection failures, keep going
            result["planetary_computer_stac"][collection] = {
                "ok": False,
                "error_type": type(e).__name__,
                "error": str(e)[:1000],
            }

    if result["earth_engine"].get("initialize") == "error":
        result["interpretation"].append(
            "Earth Engine Python libraries are installed, but account authentication is "
            "missing; run earthengine authenticate or use public STAC fallback."
        )
    for collection, probe in result["planetary_computer_stac"].items():
        if probe.get("ok") and probe.get("matched_returned", 0) > 0:
            result["interpretation"].append(
                f"{collection}: public STAC returned sample items; viable for automated "
                "discovery/rendering with follow-up asset signing/download."
            )
        else:
            result["interpretation"].append(
                f"{collection}: no sample items returned or probe failed; document as "
                "blocker/fallback candidate."
            )
    return result


def probe_from_metadata(metadata_path: str | Path) -> dict:
    """Probe imagery access using the neighborhood bbox from an AOI metadata file.

    Writes the probe report next to the address's ``reports/`` directory and
    returns the result dict.
    """
    metadata_path = Path(metadata_path)
    meta = json.loads(metadata_path.read_text())
    bbox = meta["aoi"]["neighborhood"]["bounds_wgs84"]
    result = probe_imagery_access(bbox, address=meta.get("address"), slug=meta.get("slug"))
    report_dir = Path(meta["output_dirs"]["reports"])
    report_dir.mkdir(parents=True, exist_ok=True)
    out = report_dir / "imagery-access-probe.json"
    out.write_text(json.dumps(result, indent=2))
    logger.info("Wrote imagery access probe -> %s", out)
    return result
