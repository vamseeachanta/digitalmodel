"""GeoJSON file handler for reading, writing, and converting GeoJSON data.

Uses standard json module to keep core dependencies minimal.
"""

from __future__ import annotations

import json
import logging
from pathlib import Path

import pandas as pd

logger = logging.getLogger(__name__)


class GeoJSONHandler:
    """Handles reading, writing, and conversion of GeoJSON data."""

    @staticmethod
    def read(file_path: str | Path) -> dict:
        """Read a GeoJSON file and return parsed dict.

        Args:
            file_path: Path to the GeoJSON file.

        Returns:
            Parsed GeoJSON dict (typically a FeatureCollection).

        Raises:
            FileNotFoundError: If the file does not exist.
            json.JSONDecodeError: If the file contains invalid JSON.
        """
        path = Path(file_path)
        if not path.exists():
            raise FileNotFoundError(f"GeoJSON file not found: {path}")

        with path.open("r", encoding="utf-8") as f:
            data = json.load(f)

        logger.info("Read GeoJSON file: %s", path)
        return data

    @staticmethod
    def write(data: dict, file_path: str | Path, indent: int = 2) -> Path:
        """Write a GeoJSON dict to file.

        Args:
            data: GeoJSON dict to write.
            file_path: Destination file path.
            indent: JSON indentation level.

        Returns:
            Path to the written file.
        """
        path = Path(file_path)
        path.parent.mkdir(parents=True, exist_ok=True)

        with path.open("w", encoding="utf-8") as f:
            json.dump(data, f, indent=indent)

        logger.info("Wrote GeoJSON file: %s", path)
        return path

    @staticmethod
    def features_to_dataframe(features: list[dict]) -> pd.DataFrame:
        """Convert GeoJSON features to a pandas DataFrame.

        Extracts geometry coordinates as latitude/longitude columns and
        flattens properties into columns. Handles Point geometries by
        extracting lon/lat directly, and Polygon geometries by computing
        the centroid.

        Args:
            features: List of GeoJSON Feature dicts.

        Returns:
            DataFrame with longitude, latitude, and property columns.
        """
        rows = []
        for feature in features:
            geometry = feature.get("geometry", {})
            properties = feature.get("properties", {}) or {}
            geom_type = geometry.get("type", "")
            coordinates = geometry.get("coordinates")

            longitude = None
            latitude = None

            if geom_type == "Point" and coordinates:
                longitude = coordinates[0]
                latitude = coordinates[1]
            elif geom_type == "Polygon" and coordinates:
                longitude, latitude = _polygon_centroid(coordinates[0])

            row = {
                "longitude": longitude,
                "latitude": latitude,
                **properties,
            }
            rows.append(row)

        return pd.DataFrame(rows)

    @staticmethod
    def dataframe_to_features(
        df: pd.DataFrame,
        lon_col: str = "longitude",
        lat_col: str = "latitude",
        properties_cols: list[str] | None = None,
    ) -> list[dict]:
        """Convert DataFrame rows to GeoJSON Feature dicts.

        Each row becomes a Point Feature with the specified longitude and
        latitude columns used for the geometry.

        Args:
            df: Source DataFrame.
            lon_col: Column name for longitude values.
            lat_col: Column name for latitude values.
            properties_cols: Column names to include as feature properties.
                If None, all columns except lon_col and lat_col are included.

        Returns:
            List of GeoJSON Feature dicts.
        """
        if properties_cols is None:
            properties_cols = [
                col for col in df.columns if col not in (lon_col, lat_col)
            ]

        features = []
        for _, row in df.iterrows():
            properties = {col: row[col] for col in properties_cols}
            feature = GeoJSONHandler.create_point_feature(
                longitude=row[lon_col],
                latitude=row[lat_col],
                properties=properties,
            )
            features.append(feature)

        return features

    @staticmethod
    def create_feature_collection(features: list[dict]) -> dict:
        """Wrap a list of features in a GeoJSON FeatureCollection.

        Args:
            features: List of GeoJSON Feature dicts.

        Returns:
            GeoJSON FeatureCollection dict.
        """
        return {
            "type": "FeatureCollection",
            "features": features,
        }

    @staticmethod
    def create_point_feature(
        longitude: float,
        latitude: float,
        properties: dict | None = None,
    ) -> dict:
        """Create a single GeoJSON Point Feature.

        Args:
            longitude: The longitude coordinate.
            latitude: The latitude coordinate.
            properties: Optional dict of feature properties.

        Returns:
            GeoJSON Feature dict with Point geometry.
        """
        return {
            "type": "Feature",
            "geometry": {
                "type": "Point",
                "coordinates": [longitude, latitude],
            },
            "properties": properties or {},
        }


def _polygon_centroid(ring: list[list[float]]) -> tuple[float, float]:
    """Compute the centroid of a polygon ring as the average of its vertices.

    Args:
        ring: List of [lon, lat, ...] coordinate arrays forming the
            outer ring of a polygon.

    Returns:
        Tuple of (longitude, latitude) representing the centroid.
    """
    if not ring:
        return 0.0, 0.0

    n = len(ring)
    lon_sum = sum(coord[0] for coord in ring)
    lat_sum = sum(coord[1] for coord in ring)
    return lon_sum / n, lat_sum / n
