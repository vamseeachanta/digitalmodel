"""Interactive web map generation using Folium (optional dependency).

Provides :class:`FoliumMapBuilder`, a fluent builder that wraps the
`folium <https://python-visualization.github.io/folium/>`_ library to
create interactive Leaflet maps from GIS layer data.

Folium is an **optional** dependency.  If it is not installed, importing
this module will succeed but instantiating :class:`FoliumMapBuilder` will
raise :exc:`ImportError`.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from digitalmodel.modules.gis.layers.feature_layer import FeatureLayer

try:
    import folium
    import folium.plugins

    HAS_FOLIUM = True
except ImportError:  # pragma: no cover
    HAS_FOLIUM = False

logger = logging.getLogger(__name__)


class FoliumMapBuilder:
    """Fluent builder for interactive Leaflet maps backed by Folium.

    Parameters
    ----------
    center_lat:
        Latitude of the initial map center.
    center_lon:
        Longitude of the initial map center.
    zoom_start:
        Initial zoom level.
    tiles:
        Base tile layer name (e.g. ``"OpenStreetMap"``).

    Raises
    ------
    ImportError
        If the ``folium`` package is not installed.
    """

    def __init__(
        self,
        center_lat: float = 28.0,
        center_lon: float = -90.0,
        zoom_start: int = 7,
        tiles: str = "OpenStreetMap",
    ) -> None:
        if not HAS_FOLIUM:
            raise ImportError(
                "folium is required for FoliumMapBuilder. "
                "Install it with: pip install folium"
            )
        self._map: folium.Map = folium.Map(
            location=[center_lat, center_lon],
            zoom_start=zoom_start,
            tiles=tiles,
        )

    # ------------------------------------------------------------------
    # Marker helpers
    # ------------------------------------------------------------------

    def add_well_markers(
        self,
        layer: FeatureLayer,
        popup_columns: list[str] | None = None,
        color_column: str | None = None,
        color_map: dict[str, str] | None = None,
    ) -> FoliumMapBuilder:
        """Add point markers for each feature in a layer.

        Parameters
        ----------
        layer:
            A :class:`FeatureLayer` (or subclass such as ``WellLayer``)
            whose rows represent point features.
        popup_columns:
            Column names to display in the marker popup.  Defaults to all
            columns when ``None``.
        color_column:
            Column whose values determine the marker colour.
        color_map:
            Mapping of column values to CSS colour names / hex codes.  If
            *color_column* is set but *color_map* is ``None``, all markers
            use the default colour.

        Returns
        -------
        FoliumMapBuilder
            ``self``, for method chaining.
        """
        data = layer.data
        cols = popup_columns if popup_columns is not None else list(data.columns)

        for _, row in data.iterrows():
            lat = float(row[layer.lat_col])
            lon = float(row[layer.lon_col])

            popup_html = "<br>".join(
                f"<b>{c}</b>: {row[c]}" for c in cols if c in row.index
            )

            color = "blue"
            if color_column and color_map and color_column in row.index:
                color = color_map.get(str(row[color_column]), "blue")

            folium.Marker(
                location=[lat, lon],
                popup=folium.Popup(popup_html, max_width=300),
                icon=folium.Icon(color=color),
            ).add_to(self._map)

        return self

    # ------------------------------------------------------------------
    # Polyline / polygon helpers
    # ------------------------------------------------------------------

    def add_pipeline_route(
        self,
        coordinates: list[tuple[float, float]],
        name: str = "Pipeline",
        color: str = "red",
        weight: int = 3,
    ) -> FoliumMapBuilder:
        """Add a polyline representing a pipeline route.

        Parameters
        ----------
        coordinates:
            Ordered sequence of ``(latitude, longitude)`` tuples.  Folium
            uses latitude-first ordering.
        name:
            Tooltip label for the route.
        color:
            CSS colour for the line.
        weight:
            Line width in pixels.

        Returns
        -------
        FoliumMapBuilder
            ``self``, for method chaining.
        """
        folium.PolyLine(
            locations=coordinates,
            color=color,
            weight=weight,
            tooltip=name,
        ).add_to(self._map)
        return self

    def add_lease_block(
        self,
        coordinates: list[tuple[float, float]],
        name: str = "Lease Block",
        color: str = "blue",
        fill_opacity: float = 0.2,
    ) -> FoliumMapBuilder:
        """Add a polygon representing a lease block.

        Parameters
        ----------
        coordinates:
            Ordered sequence of ``(latitude, longitude)`` vertex tuples.
        name:
            Tooltip label for the block.
        color:
            CSS colour for the polygon border and fill.
        fill_opacity:
            Opacity of the polygon fill (0.0 -- 1.0).

        Returns
        -------
        FoliumMapBuilder
            ``self``, for method chaining.
        """
        folium.Polygon(
            locations=coordinates,
            color=color,
            fill=True,
            fill_color=color,
            fill_opacity=fill_opacity,
            tooltip=name,
        ).add_to(self._map)
        return self

    # ------------------------------------------------------------------
    # Heatmap
    # ------------------------------------------------------------------

    def add_heatmap(
        self,
        layer: FeatureLayer,
        weight_column: str | None = None,
    ) -> FoliumMapBuilder:
        """Add a heatmap layer from point features.

        Parameters
        ----------
        layer:
            A :class:`FeatureLayer` whose rows are point features.
        weight_column:
            Optional column to use as intensity weight.

        Returns
        -------
        FoliumMapBuilder
            ``self``, for method chaining.
        """
        data = layer.data
        heat_data: list[list[float]] = []

        for _, row in data.iterrows():
            lat = float(row[layer.lat_col])
            lon = float(row[layer.lon_col])
            if weight_column and weight_column in row.index:
                heat_data.append([lat, lon, float(row[weight_column])])
            else:
                heat_data.append([lat, lon])

        folium.plugins.HeatMap(heat_data).add_to(self._map)
        return self

    # ------------------------------------------------------------------
    # Output
    # ------------------------------------------------------------------

    def save(self, filepath: str | Path) -> Path:
        """Save the map as an HTML file.

        Parameters
        ----------
        filepath:
            Destination path for the HTML file.

        Returns
        -------
        Path
            The resolved output path.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        self._map.save(str(filepath))
        return filepath

    def get_map(self) -> Any:
        """Return the underlying :class:`folium.Map` object.

        Returns
        -------
        folium.Map
            The Folium map instance managed by this builder.
        """
        return self._map
