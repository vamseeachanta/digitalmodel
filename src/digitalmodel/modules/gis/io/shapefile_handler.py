"""Shapefile handler for reading, writing, and converting Shapefile data.

Uses optional geopandas/fiona dependencies. All methods raise ImportError
with a helpful message when geopandas is not installed.
"""

from __future__ import annotations

import logging
from pathlib import Path

import pandas as pd

logger = logging.getLogger(__name__)

try:
    import geopandas as gpd
    from shapely.geometry import Point, mapping, shape

    HAS_GEOPANDAS = True
except ImportError:
    HAS_GEOPANDAS = False

_MISSING_MSG = (
    "geopandas is required for Shapefile support. "
    "Install it with: pip install geopandas"
)


def _require_geopandas() -> None:
    """Raise ImportError if geopandas is not available."""
    if not HAS_GEOPANDAS:
        raise ImportError(_MISSING_MSG)


class ShapefileHandler:
    """Read and write ESRI Shapefiles and convert between features and DataFrames."""

    @classmethod
    def read(cls, filepath: str | Path) -> list[dict]:
        """Read a Shapefile and return a list of feature dicts.

        Each dict contains ``geometry`` (GeoJSON-like dict with ``type``
        and ``coordinates``) and ``properties`` (attribute dict).

        Args:
            filepath: Path to the ``.shp`` file.

        Returns:
            List of feature dicts.

        Raises:
            ImportError: If geopandas is not installed.
            FileNotFoundError: If the file does not exist.
        """
        _require_geopandas()
        path = Path(filepath)
        if not path.exists():
            raise FileNotFoundError(f"Shapefile not found: {path}")

        gdf = gpd.read_file(path)
        logger.info("Read Shapefile: %s (%d features)", path, len(gdf))

        features: list[dict] = []
        for _, row in gdf.iterrows():
            geom = mapping(row.geometry) if row.geometry is not None else {}
            props = {
                col: row[col]
                for col in gdf.columns
                if col != gdf.geometry.name
            }
            features.append({"geometry": geom, "properties": props})

        return features

    @classmethod
    def write(
        cls,
        features: list[dict],
        filepath: str | Path,
        crs: str = "EPSG:4326",
    ) -> Path:
        """Write feature dicts to a Shapefile.

        Each feature dict must contain ``geometry`` (GeoJSON-like dict)
        and ``properties`` (attribute dict).

        Args:
            features: List of feature dicts.
            filepath: Destination ``.shp`` path.
            crs: Coordinate reference system string.

        Returns:
            Path to the written Shapefile.

        Raises:
            ImportError: If geopandas is not installed.
        """
        _require_geopandas()
        path = Path(filepath)
        path.parent.mkdir(parents=True, exist_ok=True)

        geometries = []
        rows: list[dict] = []
        for feature in features:
            geom_dict = feature.get("geometry", {})
            geometries.append(shape(geom_dict) if geom_dict else None)
            rows.append(feature.get("properties", {}) or {})

        gdf = gpd.GeoDataFrame(rows, geometry=geometries, crs=crs)
        gdf.to_file(str(path))

        logger.info("Wrote Shapefile: %s (%d features)", path, len(gdf))
        return path

    @classmethod
    def features_to_dataframe(cls, features: list[dict]) -> pd.DataFrame:
        """Convert feature dicts to a flat pandas DataFrame.

        Extracts the first coordinate pair from each geometry as
        ``longitude`` and ``latitude`` columns.  All entries from
        ``properties`` are flattened into columns.

        Args:
            features: List of feature dicts with ``geometry`` and
                ``properties`` keys.

        Returns:
            DataFrame with longitude, latitude, and property columns.
        """
        rows: list[dict] = []
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
                ring = coordinates[0]
                if ring:
                    longitude = sum(c[0] for c in ring) / len(ring)
                    latitude = sum(c[1] for c in ring) / len(ring)

            row = {
                "longitude": longitude,
                "latitude": latitude,
                **properties,
            }
            rows.append(row)

        return pd.DataFrame(rows)

    @classmethod
    def dataframe_to_features(
        cls,
        df: pd.DataFrame,
        lon_col: str = "longitude",
        lat_col: str = "latitude",
    ) -> list[dict]:
        """Convert a DataFrame to feature dicts with Point geometries.

        All columns except the coordinate columns are included as
        feature properties.

        Args:
            df: Source DataFrame.
            lon_col: Column name for longitude values.
            lat_col: Column name for latitude values.

        Returns:
            List of feature dicts.
        """
        property_cols = [
            col for col in df.columns if col not in (lon_col, lat_col)
        ]

        features: list[dict] = []
        for _, row in df.iterrows():
            properties = {col: row[col] for col in property_cols}
            feature = {
                "geometry": {
                    "type": "Point",
                    "coordinates": [float(row[lon_col]), float(row[lat_col])],
                },
                "properties": properties,
            }
            features.append(feature)

        return features
