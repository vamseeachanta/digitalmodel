# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Source-bearing tree registry without built-in equipment data.

API 17D and API 6A identify the equipment scope; verified editions and clause
locators are not available here. This registry is not a qualification check.
No licensed rating or temperature table is reproduced. Ratings, dimensions,
masses and valve counts are supplied from a public equipment source by callers.
There are deliberately no seeded catalogue entries pending public source data.
OneSubsea, Subsea trees (undated), provides product-family capability, but
not a complete model-specific envelope, mass and valve-count record:
https://www.onesubsea.slb.com/products-and-services/subsea-field-development/subsea-production-systems/subsea-trees
All geometry uses metres, mass uses kilograms, pressure uses pascals and
operating temperature uses degrees Celsius. Wet mass is an apparent mass.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from urllib.parse import urlsplit


def _positive(*values: float) -> None:
    if any(not math.isfinite(value) or value <= 0 for value in values):
        raise ValueError("values must be finite and positive")


@dataclass(frozen=True)
class TreeEntry:
    """Describe sourced equipment; the source is not independently verified."""

    key: str
    configuration: str
    orientation: str
    production_bore_m: float
    annulus_bore_m: float | None
    rwp_pa: float
    temperature_range_c: tuple[float, float]
    envelope_m: tuple[float, float, float]
    dry_mass_kg: float
    wet_mass_kg: float
    valve_count: int
    source: str

    def __post_init__(self) -> None:
        """Reject incomplete dimensions, invalid topology and missing sources."""
        if not self.key or self.configuration not in ("single-bore", "dual-bore"):
            raise ValueError("key and a supported configuration are required")
        if self.orientation not in ("horizontal", "vertical"):
            raise ValueError("unsupported orientation")
        if (self.annulus_bore_m is None) != (self.configuration == "single-bore"):
            raise ValueError("configuration and annulus bore disagree")
        if self.annulus_bore_m is not None:
            _positive(self.annulus_bore_m)
        if len(self.envelope_m) != 3 or len(self.temperature_range_c) != 2:
            raise ValueError("three envelope and two temperature values required")
        _positive(self.production_bore_m, self.rwp_pa, self.dry_mass_kg,
                  *self.envelope_m)
        if not math.isfinite(self.wet_mass_kg):
            raise ValueError("wet mass must be finite")
        if any(not math.isfinite(t) for t in self.temperature_range_c):
            raise ValueError("temperature limits must be finite")
        if self.temperature_range_c[0] > self.temperature_range_c[1]:
            raise ValueError("temperature limits are reversed")
        if type(self.valve_count) is not int or self.valve_count < 1:
            raise ValueError("valve_count must be a positive integer")
        locator = urlsplit(self.source)
        if locator.scheme != "https" or not locator.hostname or locator.username:
            raise ValueError("a public HTTPS source is required")


@dataclass(frozen=True)
class TreeCatalog:
    """Store immutable caller-sourced entries; duplicate keys are rejected."""

    entries: tuple[TreeEntry, ...] = ()

    def __post_init__(self) -> None:
        """Freeze the input collection and reject duplicate keys."""
        object.__setattr__(self, "entries", tuple(self.entries))
        if len({entry.key for entry in self.entries}) != len(self.entries):
            raise ValueError("duplicate tree key")

    def get(self, key: str) -> TreeEntry:
        """Return an entry by its stable key, raising KeyError if absent."""
        for entry in self.entries:
            if entry.key == key:
                return entry
        raise KeyError(key)

    def select(self, orientation: str, bore_m: float, pressure_pa: float) -> tuple[TreeEntry, ...]:
        """Select by orientation and minimum production bore and pressure."""
        _positive(bore_m, pressure_pa)
        if orientation not in ("horizontal", "vertical"):
            raise ValueError("unsupported orientation")
        return tuple(entry for entry in self.entries
                     if entry.orientation == orientation
                     and entry.production_bore_m >= bore_m
                     and entry.rwp_pa >= pressure_pa)
