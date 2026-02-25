"""WellLayer: a FeatureLayer specialised for oil and gas well data.

Extends :class:`FeatureLayer` with well-specific filtering helpers such as
status lookup, water-depth range filtering, and a well-list serialisation
for downstream use by export integrations.
"""

from __future__ import annotations

from typing import Optional

import pandas as pd

from .feature_layer import FeatureLayer


class WellLayer(FeatureLayer):
    """A :class:`FeatureLayer` specialised for oil and gas well data.

    Parameters
    ----------
    data:
        DataFrame with at minimum longitude, latitude, and a well name column.
    name:
        Human-readable layer name.
    lon_col:
        Name of the longitude column (default ``"longitude"``).
    lat_col:
        Name of the latitude column (default ``"latitude"``).
    well_name_col:
        Name of the column holding well identifiers (default ``"well_name"``).
    depth_col:
        Name of the column holding water depth values in metres
        (default ``"water_depth_m"``).  Used by depth-range filtering.
    status_col:
        Name of the column holding well status strings
        (default ``"status"``).
    """

    def __init__(
        self,
        data: pd.DataFrame,
        name: str,
        lon_col: str = "longitude",
        lat_col: str = "latitude",
        well_name_col: str = "well_name",
        depth_col: str = "water_depth_m",
        status_col: str = "status",
    ) -> None:
        super().__init__(data=data, name=name, lon_col=lon_col, lat_col=lat_col)
        self._well_name_col = well_name_col
        self._depth_col = depth_col
        self._status_col = status_col

    # ------------------------------------------------------------------
    # Properties
    # ------------------------------------------------------------------

    @property
    def well_name_col(self) -> str:
        """Name of the well identifier column."""
        return self._well_name_col

    @property
    def depth_col(self) -> str:
        """Name of the water depth column."""
        return self._depth_col

    @property
    def status_col(self) -> str:
        """Name of the well status column."""
        return self._status_col

    # ------------------------------------------------------------------
    # Filters
    # ------------------------------------------------------------------

    def filter_by_status(self, status: str) -> WellLayer:
        """Return a new WellLayer containing only wells with the given status.

        Parameters
        ----------
        status:
            Well status string to filter by (e.g. ``"producing"``).

        Returns
        -------
        WellLayer

        Raises
        ------
        KeyError
            If the status column is not present in the data.
        """
        if self._status_col not in self._data.columns:
            raise KeyError(
                f"Status column '{self._status_col}' not found. "
                f"Available columns: {list(self._data.columns)}"
            )
        mask = self._data[self._status_col] == status
        filtered = self._data.loc[mask].reset_index(drop=True)
        return WellLayer(
            data=filtered,
            name=self._name,
            lon_col=self._lon_col,
            lat_col=self._lat_col,
            well_name_col=self._well_name_col,
            depth_col=self._depth_col,
            status_col=self._status_col,
        )

    def filter_by_depth_range(
        self,
        min_depth_m: Optional[float] = None,
        max_depth_m: Optional[float] = None,
    ) -> WellLayer:
        """Return wells within an optional water-depth range.

        Parameters
        ----------
        min_depth_m:
            Minimum water depth in metres (inclusive).  ``None`` means no
            lower bound.
        max_depth_m:
            Maximum water depth in metres (inclusive).  ``None`` means no
            upper bound.

        Returns
        -------
        WellLayer
        """
        mask = pd.Series([True] * len(self._data), index=self._data.index)

        if min_depth_m is not None and self._depth_col in self._data.columns:
            mask &= self._data[self._depth_col] >= min_depth_m

        if max_depth_m is not None and self._depth_col in self._data.columns:
            mask &= self._data[self._depth_col] <= max_depth_m

        filtered = self._data.loc[mask].reset_index(drop=True)
        return WellLayer(
            data=filtered,
            name=self._name,
            lon_col=self._lon_col,
            lat_col=self._lat_col,
            well_name_col=self._well_name_col,
            depth_col=self._depth_col,
            status_col=self._status_col,
        )

    # ------------------------------------------------------------------
    # Serialisation helpers
    # ------------------------------------------------------------------

    def to_well_list(self) -> list[dict]:
        """Return a list of dicts, one per well, with all column values.

        Returns
        -------
        list[dict]
            Each dict includes at minimum ``"longitude"`` and ``"latitude"``
            (or the configured column names) plus all other columns.
        """
        return self._data.to_dict(orient="records")

    def filter(self, mask: pd.Series) -> WellLayer:
        """Override parent filter to return a WellLayer instead of FeatureLayer.

        Parameters
        ----------
        mask:
            Boolean :class:`pandas.Series` aligned to this layer's index.

        Returns
        -------
        WellLayer
        """
        filtered_df = self._data.loc[mask].reset_index(drop=True)
        return WellLayer(
            data=filtered_df,
            name=self._name,
            lon_col=self._lon_col,
            lat_col=self._lat_col,
            well_name_col=self._well_name_col,
            depth_col=self._depth_col,
            status_col=self._status_col,
        )
