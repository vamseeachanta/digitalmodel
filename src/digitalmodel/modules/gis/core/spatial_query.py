"""Spatial query operations for GIS data.

Provides :class:`SpatialQuery` with static methods for proximity searches,
point-in-polygon tests, bounding box operations, and layer filtering.
Uses only Phase 1 modules (no external spatial libraries required).
"""

from __future__ import annotations

from .geometry import GeoBoundingBox, GeoPoint


class SpatialQuery:
    """Static methods for spatial query operations on GeoPoint and FeatureLayer data."""

    @staticmethod
    def nearest_neighbors(
        origin: GeoPoint,
        points: list[GeoPoint],
        k: int = 1,
    ) -> list[tuple[GeoPoint, float]]:
        """Find the *k* nearest points to *origin* using haversine distance.

        Parameters
        ----------
        origin:
            The reference point to measure distances from.
        points:
            Candidate points to search.
        k:
            Number of nearest neighbours to return.

        Returns
        -------
        list[tuple[GeoPoint, float]]
            List of ``(point, distance_meters)`` pairs sorted by distance
            ascending.  If *k* exceeds the number of available points, all
            points are returned.
        """
        if not points:
            return []

        scored = [(p, origin.distance_to(p)) for p in points]
        scored.sort(key=lambda pair: pair[1])
        return scored[: min(k, len(scored))]

    @staticmethod
    def points_within_radius(
        origin: GeoPoint,
        points: list[GeoPoint],
        radius_m: float,
    ) -> list[tuple[GeoPoint, float]]:
        """Find all points within *radius_m* meters of *origin*.

        Parameters
        ----------
        origin:
            The reference point.
        points:
            Candidate points to search.
        radius_m:
            Search radius in meters.

        Returns
        -------
        list[tuple[GeoPoint, float]]
            List of ``(point, distance_meters)`` pairs sorted by distance
            ascending.
        """
        results: list[tuple[GeoPoint, float]] = []
        for p in points:
            d = origin.distance_to(p)
            if d <= radius_m:
                results.append((p, d))
        results.sort(key=lambda pair: pair[1])
        return results

    @staticmethod
    def points_in_bounding_box(
        points: list[GeoPoint],
        bbox: GeoBoundingBox,
    ) -> list[GeoPoint]:
        """Filter points contained within a bounding box.

        Parameters
        ----------
        points:
            Candidate points.
        bbox:
            The bounding box to test against.

        Returns
        -------
        list[GeoPoint]
            Points that lie inside (or on the edge of) *bbox*.
        """
        return [p for p in points if bbox.contains(p)]

    @staticmethod
    def point_in_polygon(
        point: GeoPoint,
        polygon_coords: list[tuple[float, float]],
    ) -> bool:
        """Test whether *point* lies inside a polygon using ray-casting.

        Parameters
        ----------
        point:
            The point to test.
        polygon_coords:
            List of ``(x, y)`` tuples defining the polygon boundary.
            The polygon may or may not be explicitly closed (first vertex
            repeated at the end).

        Returns
        -------
        bool
            ``True`` if the point is inside the polygon.
        """
        x, y = point.x, point.y
        n = len(polygon_coords)
        inside = False

        j = n - 1
        for i in range(n):
            xi, yi = polygon_coords[i]
            xj, yj = polygon_coords[j]

            if ((yi > y) != (yj > y)) and (
                x < (xj - xi) * (y - yi) / (yj - yi) + xi
            ):
                inside = not inside
            j = i

        return inside

    @staticmethod
    def bounding_box_overlap(
        bbox1: GeoBoundingBox,
        bbox2: GeoBoundingBox,
    ) -> bool:
        """Test whether two bounding boxes overlap.

        Parameters
        ----------
        bbox1:
            First bounding box.
        bbox2:
            Second bounding box.

        Returns
        -------
        bool
            ``True`` if the boxes overlap (including touching edges).
        """
        if bbox1.max_x < bbox2.min_x or bbox2.max_x < bbox1.min_x:
            return False
        if bbox1.max_y < bbox2.min_y or bbox2.max_y < bbox1.min_y:
            return False
        return True

    @staticmethod
    def filter_layer_by_proximity(
        layer: "FeatureLayer",
        origin: GeoPoint,
        radius_m: float,
    ) -> "FeatureLayer":
        """Filter a FeatureLayer to features within *radius_m* of *origin*.

        Parameters
        ----------
        layer:
            The source feature layer.
        origin:
            The reference point.
        radius_m:
            Search radius in meters.

        Returns
        -------
        FeatureLayer
            A new layer containing only features within the radius.
        """
        from ..layers.feature_layer import FeatureLayer  # noqa: F811

        import pandas as pd

        keep: list[bool] = []
        for _, row in layer.data.iterrows():
            pt = GeoPoint(
                x=float(row[layer.lon_col]),
                y=float(row[layer.lat_col]),
            )
            keep.append(origin.distance_to(pt) <= radius_m)

        mask = pd.Series(keep, index=layer.data.index)
        return layer.filter(mask)
