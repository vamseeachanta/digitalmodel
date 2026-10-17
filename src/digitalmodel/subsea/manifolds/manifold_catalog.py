# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Source-bearing manifold registry with no built-in manufacturer entries.

API 17P (Design and Operation of Subsea Production Systems: Subsea Structures
and Manifolds) defines the equipment scope, not the data in this registry.
Verified edition and clause locators are unavailable here; this module does
not establish API qualification. All ratings, dimensions, mass and tie-in
methods require caller-supplied public equipment evidence. No standard rating
table is included. Public-source completeness remains an open data requirement.
OneSubsea, Subsea manifolds (undated), describes product-family capability,
but does not provide a complete header, footprint and mass record:
https://www.onesubsea.slb.com/products-and-services/subsea-field-development/subsea-production-systems/subsea-manifolds
Geometry uses metres, pressure uses pascals and mass uses kilograms. Wet mass
is apparent mass. The source locator is retained, not independently verified.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from urllib.parse import urlsplit


KINDS = ("gathering", "template", "multi-slot", "plem-integrated")


def _positive(*values: float) -> None:
    if any(not math.isfinite(value) or value <= 0 for value in values):
        raise ValueError("values must be finite and positive")


@dataclass(frozen=True)
class ManifoldEntry:
    """Record equipment attributes and their caller-supplied public source."""

    key: str
    kind: str
    slots: int
    header_bore_m: float
    rating_pa: float
    footprint_m: tuple[float, float]
    dry_mass_kg: float
    wet_mass_kg: float
    tie_in: str
    source: str

    def __post_init__(self) -> None:
        """Validate type, dimensions, counts and the evidence locator."""
        if not self.key or self.kind not in KINDS or not self.tie_in:
            raise ValueError("key, supported kind and tie-in are required")
        if type(self.slots) is not int or self.slots < 1:
            raise ValueError("slots must be a positive integer")
        if len(self.footprint_m) != 2:
            raise ValueError("two footprint dimensions required")
        _positive(self.header_bore_m, self.rating_pa, self.dry_mass_kg, *self.footprint_m)
        if not math.isfinite(self.wet_mass_kg):
            raise ValueError("wet mass must be finite")
        locator = urlsplit(self.source)
        if locator.scheme != "https" or not locator.hostname or locator.username:
            raise ValueError("a public HTTPS source is required")


@dataclass(frozen=True)
class ManifoldCatalog:
    """Store immutable entries by unique stable key, initially empty."""

    entries: tuple[ManifoldEntry, ...] = ()

    def __post_init__(self) -> None:
        """Freeze the collection and reject duplicate keys."""
        object.__setattr__(self, "entries", tuple(self.entries))
        if len({entry.key for entry in self.entries}) != len(self.entries):
            raise ValueError("duplicate manifold key")

    def get(self, key: str) -> ManifoldEntry:
        """Look up an entry by key, raising KeyError if absent."""
        for entry in self.entries:
            if entry.key == key:
                return entry
        raise KeyError(key)

    def select(self, kind: str, slots: int, pressure_pa: float) -> tuple[ManifoldEntry, ...]:
        """Filter by equipment kind and minimum slot count and rating."""
        if kind not in KINDS or type(slots) is not int or slots < 1:
            raise ValueError("supported kind and positive integer slots required")
        _positive(pressure_pa)
        return tuple(entry for entry in self.entries if entry.kind == kind
                     and entry.slots >= slots and entry.rating_pa >= pressure_pa)
