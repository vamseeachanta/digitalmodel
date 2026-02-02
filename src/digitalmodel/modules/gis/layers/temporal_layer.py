"""Temporal vector layer for time-aware geospatial features.

Provides :class:`TemporalLayer`, a subclass of :class:`FeatureLayer` that adds
a time dimension to geospatial features.  Designed for tracking offshore
infrastructure lifecycle events such as installation, inspection, repair, and
decommissioning.
"""

from __future__ import annotations

import logging
from datetime import datetime, timedelta
from pathlib import Path

import pandas as pd

from digitalmodel.modules.gis.core.crs import CRSDefinition
from digitalmodel.modules.gis.io.geojson_handler import GeoJSONHandler
from digitalmodel.modules.gis.layers.feature_layer import FeatureLayer

logger = logging.getLogger(__name__)


class TemporalLayer(FeatureLayer):
    """A FeatureLayer with a time dimension for lifecycle tracking.

    Parameters
    ----------
    data:
        DataFrame containing coordinate and timestamp columns.
    crs:
        Coordinate reference system.  Defaults to WGS 84 when ``None``.
    name:
        Human-readable layer name.
    lon_col:
        Name of the column holding longitude values.
    lat_col:
        Name of the column holding latitude values.
    time_col:
        Name of the column holding timestamp values.
    """

    def __init__(
        self,
        data: pd.DataFrame,
        crs: CRSDefinition | None = None,
        name: str = "temporal",
        lon_col: str = "longitude",
        lat_col: str = "latitude",
        time_col: str = "timestamp",
    ) -> None:
        if crs is None:
            crs = CRSDefinition.wgs84()
        super().__init__(data=data, crs=crs, name=name, lon_col=lon_col, lat_col=lat_col)
        self._time_col = time_col
        if time_col in self._data.columns and not self._data.empty:
            self._data[time_col] = pd.to_datetime(self._data[time_col])

    # ------------------------------------------------------------------
    # Properties
    # ------------------------------------------------------------------

    @property
    def time_column(self) -> str:
        """Name of the time column."""
        return self._time_col

    @property
    def time_range(self) -> tuple[datetime, datetime]:
        """Earliest and latest timestamps as ``(min, max)``."""
        ts = self._data[self._time_col]
        return (ts.min().to_pydatetime(), ts.max().to_pydatetime())

    @property
    def duration(self) -> timedelta:
        """Duration between the earliest and latest timestamps."""
        ts = self._data[self._time_col]
        return (ts.max() - ts.min()).to_pytimedelta()

    # ------------------------------------------------------------------
    # Dunder overrides
    # ------------------------------------------------------------------

    def __repr__(self) -> str:
        return (
            f"TemporalLayer(name={self._name!r}, "
            f"features={len(self)}, "
            f"crs={self._crs.crs_type.value})"
        )

    # ------------------------------------------------------------------
    # Factory class methods
    # ------------------------------------------------------------------

    @classmethod
    def from_geojson(
        cls,
        file_path: str | Path,
        time_col: str = "timestamp",
        name: str = "temporal",
    ) -> TemporalLayer:
        """Load a TemporalLayer from a GeoJSON file.

        Parameters
        ----------
        file_path:
            Path to the GeoJSON file.
        time_col:
            Name of the property that holds timestamp values.
        name:
            Layer name.

        Returns
        -------
        TemporalLayer
            A new temporal layer with data parsed from the file.
        """
        geojson = GeoJSONHandler.read(file_path)
        features = geojson.get("features", [])
        df = GeoJSONHandler.features_to_dataframe(features)
        return cls(data=df, crs=CRSDefinition.wgs84(), name=name, time_col=time_col)

    # ------------------------------------------------------------------
    # Temporal query methods
    # ------------------------------------------------------------------

    def filter_by_time(
        self,
        start: datetime | str | None = None,
        end: datetime | str | None = None,
    ) -> TemporalLayer:
        """Filter features to a time window.

        Parameters
        ----------
        start:
            Lower bound (inclusive).  ``None`` means no lower bound.
        end:
            Upper bound (inclusive).  ``None`` means no upper bound.

        Returns
        -------
        TemporalLayer
            A new layer containing only features within the time window.
        """
        ts = self._data[self._time_col]
        mask = pd.Series(True, index=self._data.index)

        if start is not None:
            mask = mask & (ts >= pd.Timestamp(start))
        if end is not None:
            mask = mask & (ts <= pd.Timestamp(end))

        filtered = self._data.loc[mask].reset_index(drop=True)
        return TemporalLayer(
            data=filtered,
            crs=self._crs,
            name=self._name,
            lon_col=self._lon_col,
            lat_col=self._lat_col,
            time_col=self._time_col,
        )

    def snapshot_at(self, timestamp: datetime | str) -> TemporalLayer:
        """Get the latest state of each unique feature at or before *timestamp*.

        Groups features by ``name`` (falling back to ``feature_id`` if present)
        and selects the row with the maximum timestamp that does not exceed the
        given *timestamp*.

        Parameters
        ----------
        timestamp:
            The point in time to snapshot.

        Returns
        -------
        TemporalLayer
            A new layer with at most one row per unique feature.
        """
        ts = pd.Timestamp(timestamp)
        id_col = self._resolve_id_column()

        before = self._data[self._data[self._time_col] <= ts]
        if before.empty:
            empty_df = self._data.iloc[:0].copy()
            return TemporalLayer(
                data=empty_df,
                crs=self._crs,
                name=self._name,
                lon_col=self._lon_col,
                lat_col=self._lat_col,
                time_col=self._time_col,
            )

        idx = before.groupby(id_col)[self._time_col].idxmax()
        snapshot = before.loc[idx].reset_index(drop=True)
        return TemporalLayer(
            data=snapshot,
            crs=self._crs,
            name=self._name,
            lon_col=self._lon_col,
            lat_col=self._lat_col,
            time_col=self._time_col,
        )

    def time_series(self, feature_id: str, value_col: str) -> pd.DataFrame:
        """Extract a time series for a specific feature.

        Parameters
        ----------
        feature_id:
            The identifier of the feature (matched against ``name`` or
            ``feature_id`` column).
        value_col:
            The column whose values form the time series.

        Returns
        -------
        pd.DataFrame
            A two-column DataFrame (timestamp, value) sorted by time.
        """
        id_col = self._resolve_id_column()
        rows = self._data[self._data[id_col] == feature_id].copy()
        result = rows[[self._time_col, value_col]].sort_values(self._time_col)
        return result.reset_index(drop=True)

    def animate_frames(self, freq: str = "Y") -> list[TemporalLayer]:
        """Split into time-binned frames for animation.

        Parameters
        ----------
        freq:
            Pandas frequency string (``"Y"`` for yearly, ``"M"`` for monthly,
            ``"Q"`` for quarterly, etc.).

        Returns
        -------
        list[TemporalLayer]
            One layer per time bin, in chronological order.
        """
        ts = self._data[self._time_col]
        groups = self._data.groupby(ts.dt.to_period(freq))

        frames: list[TemporalLayer] = []
        for _period, group_df in sorted(groups, key=lambda x: x[0]):
            frame = TemporalLayer(
                data=group_df.reset_index(drop=True),
                crs=self._crs,
                name=self._name,
                lon_col=self._lon_col,
                lat_col=self._lat_col,
                time_col=self._time_col,
            )
            frames.append(frame)
        return frames

    # ------------------------------------------------------------------
    # Summary
    # ------------------------------------------------------------------

    def summary(self) -> dict:
        """Return a summary dictionary of this temporal layer.

        Returns
        -------
        dict
            Contains ``feature_count``, ``time_range``, ``duration_days``,
            and ``unique_features``.
        """
        id_col = self._resolve_id_column()
        return {
            "feature_count": len(self),
            "time_range": self.time_range,
            "duration_days": self.duration.days,
            "unique_features": sorted(self._data[id_col].unique().tolist()),
        }

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _resolve_id_column(self) -> str:
        """Determine which column identifies unique features."""
        if "name" in self._data.columns:
            return "name"
        if "feature_id" in self._data.columns:
            return "feature_id"
        raise ValueError(
            "Data must contain a 'name' or 'feature_id' column "
            "to identify unique features"
        )
