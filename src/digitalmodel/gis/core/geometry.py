"""Basic geometry primitives for the GIS module.

Provides GeoPoint and GeoBoundingBox dataclasses with coordinate operations,
distance calculations (haversine / euclidean), and GeoJSON-like serialization.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field

EARTH_RADIUS_METERS: float = 6_371_000.0


@dataclass
class GeoPoint:
    """A geographic point with optional elevation and arbitrary properties."""

    x: float
    y: float
    z: float | None = None
    properties: dict | None = field(default=None, repr=False)

    # ------------------------------------------------------------------
    # Convenience aliases
    # ------------------------------------------------------------------

    @property
    def longitude(self) -> float:
        return self.x

    @property
    def latitude(self) -> float:
        return self.y

    @property
    def elevation(self) -> float | None:
        return self.z

    # ------------------------------------------------------------------
    # Distance calculations
    # ------------------------------------------------------------------

    def distance_to(self, other: GeoPoint, method: str = "haversine") -> float:
        """Return the distance to *other* in the chosen metric.

        Parameters
        ----------
        other:
            Target point.
        method:
            ``"haversine"`` (default) treats *x* / *y* as lon / lat in
            degrees and returns the great-circle distance in **meters**
            on a spherical Earth (radius = 6 371 000 m).
            ``"euclidean"`` returns the straight-line distance in
            coordinate units (2-D when *z* is ``None`` on either point,
            3-D otherwise).
        """
        if method == "haversine":
            return self._haversine(other)
        if method == "euclidean":
            return self._euclidean(other)
        raise ValueError(f"Unknown distance method: {method!r}")

    def _haversine(self, other: GeoPoint) -> float:
        lat1 = math.radians(self.y)
        lat2 = math.radians(other.y)
        dlat = math.radians(other.y - self.y)
        dlon = math.radians(other.x - self.x)

        a = (
            math.sin(dlat / 2) ** 2
            + math.cos(lat1) * math.cos(lat2) * math.sin(dlon / 2) ** 2
        )
        c = 2 * math.asin(math.sqrt(a))
        return EARTH_RADIUS_METERS * c

    def _euclidean(self, other: GeoPoint) -> float:
        dx = other.x - self.x
        dy = other.y - self.y

        if self.z is not None and other.z is not None:
            dz = other.z - self.z
            return math.sqrt(dx * dx + dy * dy + dz * dz)

        return math.sqrt(dx * dx + dy * dy)

    # ------------------------------------------------------------------
    # Serialization
    # ------------------------------------------------------------------

    def to_dict(self) -> dict:
        """Return a GeoJSON-like ``Point`` dictionary."""
        coordinates: list[float] = [self.x, self.y]
        if self.z is not None:
            coordinates.append(self.z)

        result: dict = {
            "type": "Point",
            "coordinates": coordinates,
        }
        if self.properties:
            result["properties"] = self.properties
        return result

    @classmethod
    def from_dict(cls, d: dict) -> GeoPoint:
        """Construct a ``GeoPoint`` from a GeoJSON-like dictionary.

        Expects at minimum ``{"coordinates": [x, y]}`` or
        ``{"coordinates": [x, y, z]}``.
        """
        coords = d["coordinates"]
        x = float(coords[0])
        y = float(coords[1])
        z = float(coords[2]) if len(coords) > 2 else None
        properties = d.get("properties")
        return cls(x=x, y=y, z=z, properties=properties)


@dataclass
class GeoBoundingBox:
    """An axis-aligned bounding box defined by its corner coordinates."""

    min_x: float
    min_y: float
    max_x: float
    max_y: float

    # ------------------------------------------------------------------
    # Queries
    # ------------------------------------------------------------------

    def contains(self, point: GeoPoint) -> bool:
        """Return ``True`` if *point* lies inside (or on the edge of) the box."""
        return (
            self.min_x <= point.x <= self.max_x
            and self.min_y <= point.y <= self.max_y
        )

    def center(self) -> GeoPoint:
        """Return the centre of the bounding box as a ``GeoPoint``."""
        return GeoPoint(
            x=(self.min_x + self.max_x) / 2,
            y=(self.min_y + self.max_y) / 2,
        )

    # ------------------------------------------------------------------
    # Transformations
    # ------------------------------------------------------------------

    def expand(self, margin: float) -> GeoBoundingBox:
        """Return a new bounding box expanded by *margin* on every side."""
        return GeoBoundingBox(
            min_x=self.min_x - margin,
            min_y=self.min_y - margin,
            max_x=self.max_x + margin,
            max_y=self.max_y + margin,
        )

    def to_polygon_coords(self) -> list[tuple[float, float]]:
        """Return five coordinate pairs forming a closed ring (SW -> SE -> NE -> NW -> SW)."""
        return [
            (self.min_x, self.min_y),
            (self.max_x, self.min_y),
            (self.max_x, self.max_y),
            (self.min_x, self.max_y),
            (self.min_x, self.min_y),
        ]

    # ------------------------------------------------------------------
    # Construction helpers
    # ------------------------------------------------------------------

    @classmethod
    def from_points(cls, points: list[GeoPoint]) -> GeoBoundingBox:
        """Build the smallest bounding box that contains all *points*.

        Raises ``ValueError`` if *points* is empty.
        """
        if not points:
            raise ValueError("Cannot create a bounding box from an empty list of points")

        xs = [p.x for p in points]
        ys = [p.y for p in points]
        return cls(
            min_x=min(xs),
            min_y=min(ys),
            max_x=max(xs),
            max_y=max(ys),
        )
