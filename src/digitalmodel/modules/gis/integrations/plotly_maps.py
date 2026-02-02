"""Interactive Plotly map visualizations for GIS layers.

Provides :class:`PlotlyMapBuilder`, a collection of classmethods that turn
:class:`~digitalmodel.modules.gis.layers.feature_layer.FeatureLayer` data into
interactive ``scatter_mapbox``, ``scatter_geo``, and dashboard figures.
"""

from __future__ import annotations

import math
from pathlib import Path

import plotly.graph_objects as go


class PlotlyMapBuilder:
    """Build interactive Plotly maps from GIS FeatureLayer data."""

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _auto_zoom(lons: list[float], lats: list[float]) -> float:
        """Estimate a reasonable mapbox zoom level from coordinate spread."""
        if not lons or not lats:
            return 2.0
        lon_range = max(lons) - min(lons)
        lat_range = max(lats) - min(lats)
        max_range = max(lon_range, lat_range, 1e-6)
        zoom = math.log2(360.0 / max_range) - 1
        return max(min(zoom, 18.0), 1.0)

    # ------------------------------------------------------------------
    # scatter_map
    # ------------------------------------------------------------------

    @classmethod
    def scatter_map(
        cls,
        layer,
        color_column: str | None = None,
        size_column: str | None = None,
        hover_columns: list[str] | None = None,
        title: str = "Map",
        mapbox_style: str = "open-street-map",
    ) -> go.Figure:
        """Create a Plotly scatter_mapbox from a FeatureLayer.

        Parameters
        ----------
        layer:
            A :class:`FeatureLayer` (or subclass) containing point data.
        color_column:
            Optional column name for colour-coding markers.
        size_column:
            Optional column name for scaling marker sizes.
        hover_columns:
            Columns to display on hover.  ``None`` means all columns.
        title:
            Figure title.
        mapbox_style:
            Mapbox base-map style name.

        Returns
        -------
        go.Figure
        """
        from digitalmodel.modules.gis.layers.feature_layer import FeatureLayer  # noqa: F811

        df = layer.data
        lons = df[layer.lon_col].tolist()
        lats = df[layer.lat_col].tolist()
        centroid = layer.centroid

        if hover_columns is None:
            hover_columns = list(df.columns)

        hover_text = []
        for _, row in df.iterrows():
            parts = [f"{c}: {row[c]}" for c in hover_columns if c in df.columns]
            hover_text.append("<br>".join(parts))

        if color_column and color_column in df.columns:
            unique_vals = df[color_column].unique()
            fig = go.Figure()
            for val in unique_vals:
                mask = df[color_column] == val
                sub = df[mask]
                sub_hover = [h for h, m in zip(hover_text, mask) if m]
                marker_kwargs: dict = {}
                if size_column and size_column in sub.columns:
                    marker_kwargs["size"] = sub[size_column].tolist()
                    marker_kwargs["sizemode"] = "area"
                    marker_kwargs["sizeref"] = 2.0 * max(sub[size_column].max(), 1) / (40.0**2)
                    marker_kwargs["sizemin"] = 4
                fig.add_trace(
                    go.Scattermapbox(
                        lon=sub[layer.lon_col].tolist(),
                        lat=sub[layer.lat_col].tolist(),
                        mode="markers",
                        marker=marker_kwargs if marker_kwargs else None,
                        name=str(val),
                        text=sub_hover,
                        hoverinfo="text",
                    )
                )
        else:
            marker_kwargs = {}
            if size_column and size_column in df.columns:
                marker_kwargs["size"] = df[size_column].tolist()
                marker_kwargs["sizemode"] = "area"
                marker_kwargs["sizeref"] = 2.0 * max(df[size_column].max(), 1) / (40.0**2)
                marker_kwargs["sizemin"] = 4
            fig = go.Figure(
                go.Scattermapbox(
                    lon=lons,
                    lat=lats,
                    mode="markers",
                    marker=marker_kwargs if marker_kwargs else None,
                    text=hover_text,
                    hoverinfo="text",
                )
            )

        zoom = cls._auto_zoom(lons, lats)
        fig.update_layout(
            title=title,
            mapbox=dict(
                style=mapbox_style,
                center=dict(lon=centroid.x, lat=centroid.y),
                zoom=zoom,
            ),
            margin=dict(l=0, r=0, t=40, b=0),
        )
        return fig

    # ------------------------------------------------------------------
    # pipeline_map
    # ------------------------------------------------------------------

    @classmethod
    def pipeline_map(
        cls,
        routes: list[dict],
        title: str = "Pipeline Routes",
        mapbox_style: str = "open-street-map",
    ) -> go.Figure:
        """Create a map with pipeline routes rendered as lines.

        Parameters
        ----------
        routes:
            List of dicts, each with ``'name'`` (str) and ``'coordinates'``
            (list of ``(lon, lat)`` tuples).
        title:
            Figure title.
        mapbox_style:
            Mapbox base-map style name.

        Returns
        -------
        go.Figure
        """
        fig = go.Figure()
        all_lons: list[float] = []
        all_lats: list[float] = []

        for route in routes:
            coords = route["coordinates"]
            lons = [c[0] for c in coords]
            lats = [c[1] for c in coords]
            all_lons.extend(lons)
            all_lats.extend(lats)
            fig.add_trace(
                go.Scattermapbox(
                    lon=lons,
                    lat=lats,
                    mode="lines",
                    name=route.get("name", "route"),
                )
            )

        center_lon = sum(all_lons) / len(all_lons) if all_lons else 0.0
        center_lat = sum(all_lats) / len(all_lats) if all_lats else 0.0
        zoom = cls._auto_zoom(all_lons, all_lats)

        fig.update_layout(
            title=title,
            mapbox=dict(
                style=mapbox_style,
                center=dict(lon=center_lon, lat=center_lat),
                zoom=zoom,
            ),
            margin=dict(l=0, r=0, t=40, b=0),
        )
        return fig

    # ------------------------------------------------------------------
    # well_dashboard
    # ------------------------------------------------------------------

    @classmethod
    def well_dashboard(
        cls,
        layer,
        title: str = "Well Dashboard",
        mapbox_style: str = "open-street-map",
    ) -> go.Figure:
        """Create a subplot figure: map + status bar chart + depth histogram.

        Parameters
        ----------
        layer:
            A :class:`FeatureLayer` (or subclass) with well data.
        title:
            Figure super-title.
        mapbox_style:
            Mapbox base-map style name.

        Returns
        -------
        go.Figure
        """
        from plotly.subplots import make_subplots

        df = layer.data
        lons = df[layer.lon_col].tolist()
        lats = df[layer.lat_col].tolist()
        centroid = layer.centroid

        fig = make_subplots(
            rows=2,
            cols=2,
            specs=[
                [{"type": "mapbox", "rowspan": 2}, {"type": "xy"}],
                [None, {"type": "xy"}],
            ],
            subplot_titles=["Well Locations", "Status Distribution", "Water Depth"],
            column_widths=[0.6, 0.4],
        )

        # Left panel: scatter_mapbox
        fig.add_trace(
            go.Scattermapbox(
                lon=lons,
                lat=lats,
                mode="markers",
                marker=dict(size=8),
                text=df["name"].tolist() if "name" in df.columns else None,
                hoverinfo="text",
                name="Wells",
            ),
            row=1,
            col=1,
        )

        # Top-right: bar chart of statuses
        if "status" in df.columns:
            status_counts = df["status"].value_counts()
            fig.add_trace(
                go.Bar(
                    x=status_counts.index.tolist(),
                    y=status_counts.values.tolist(),
                    name="Status",
                ),
                row=1,
                col=2,
            )

        # Bottom-right: histogram of water depths
        if "water_depth_m" in df.columns:
            fig.add_trace(
                go.Histogram(
                    x=df["water_depth_m"].tolist(),
                    name="Water Depth",
                ),
                row=2,
                col=2,
            )

        zoom = cls._auto_zoom(lons, lats)
        fig.update_layout(
            title_text=title,
            mapbox=dict(
                style=mapbox_style,
                center=dict(lon=centroid.x, lat=centroid.y),
                zoom=zoom,
            ),
            showlegend=False,
            height=600,
        )
        return fig

    # ------------------------------------------------------------------
    # save_html
    # ------------------------------------------------------------------

    @classmethod
    def save_html(cls, fig: go.Figure, filepath: str | Path) -> Path:
        """Save a Plotly Figure to an HTML file.

        Parameters
        ----------
        fig:
            The Plotly figure to export.
        filepath:
            Destination file path.

        Returns
        -------
        Path
            The resolved path of the written file.
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        fig.write_html(str(filepath))
        return filepath
