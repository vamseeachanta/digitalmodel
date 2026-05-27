"""Manifest models for the GIS imagery-timelapse pipeline.

Provides :class:`ImageryManifest`, a manifest describing one or more addresses
for which historical-imagery timelapse artifacts should be generated.  The
manifest decouples the pipeline from any hard-coded working directory: output,
cache and log roots are read from the manifest (and may be resolved relative to
the manifest file), so the same code runs for any address set in any workspace.

The schema mirrors the original ``addresses.yaml``/``addresses.json`` files
developed for workspace-hub issue #2538 (see
``ace-examples/gis-timelapse-2538`` for a worked example).
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import yaml
from pydantic import BaseModel, Field

DEFAULT_PROPERTY_RADIUS_MILES = 0.20
DEFAULT_NEIGHBORHOOD_RADIUS_MILES = 1.50


class ManifestDefaults(BaseModel):
    """Pipeline defaults applied to addresses that do not override them."""

    property_radius_miles: float = DEFAULT_PROPERTY_RADIUS_MILES
    neighborhood_radius_miles: float = DEFAULT_NEIGHBORHOOD_RADIUS_MILES
    requested_outputs: list[str] = Field(default_factory=list)
    sources_to_evaluate: list[str] = Field(default_factory=list)


class AddressRecord(BaseModel):
    """A single address to render, with optional per-address overrides."""

    label: str
    slug: str
    address: str
    latitude: float
    longitude: float
    geocode_source: str = "unknown"
    property_radius_miles: float | None = None
    neighborhood_radius_miles: float | None = None
    requested_outputs: list[str] = Field(default_factory=list)
    notes: str | None = None


class ImageryManifest(BaseModel):
    """Top-level manifest binding addresses to output/cache/log roots."""

    version: int = 1
    created_for_issue: int | None = None
    coordinate_crs: str = "EPSG:4326"
    output_root: Path
    cache_root: Path
    log_root: Path
    defaults: ManifestDefaults = Field(default_factory=ManifestDefaults)
    addresses: list[AddressRecord] = Field(default_factory=list)

    @classmethod
    def from_file(cls, path: str | Path) -> "ImageryManifest":
        """Load a manifest from a ``.yaml``/``.yml`` or ``.json`` file.

        Relative ``output_root``/``cache_root``/``log_root`` values are resolved
        relative to the manifest's parent directory so a manifest is portable.
        """
        path = Path(path)
        raw = path.read_text()
        if path.suffix.lower() in {".yaml", ".yml"}:
            data: dict[str, Any] = yaml.safe_load(raw)
        else:
            data = json.loads(raw)
        base_dir = path.parent
        for key in ("output_root", "cache_root", "log_root"):
            value = data.get(key)
            if value is not None and not Path(value).is_absolute():
                data[key] = str((base_dir / value).resolve())
        return cls.model_validate(data)

    def property_radius(self, address: AddressRecord) -> float:
        """Effective property-scale radius for ``address`` (miles)."""
        if address.property_radius_miles is not None:
            return address.property_radius_miles
        return self.defaults.property_radius_miles

    def neighborhood_radius(self, address: AddressRecord) -> float:
        """Effective neighborhood-scale radius for ``address`` (miles)."""
        if address.neighborhood_radius_miles is not None:
            return address.neighborhood_radius_miles
        return self.defaults.neighborhood_radius_miles
