"""Tests for PlotlyMapBuilder -- interactive Plotly map visualizations."""

from __future__ import annotations

from pathlib import Path

import plotly.graph_objects as go
import pytest

from digitalmodel.modules.gis.integrations.plotly_maps import PlotlyMapBuilder
from digitalmodel.modules.gis.layers.well_layer import WellLayer


# ------------------------------------------------------------------
# scatter_map
# ------------------------------------------------------------------


class TestScatterMap:
    """Create a scatter_mapbox from a FeatureLayer."""

    def test_returns_figure(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.scatter_map(layer)
        assert isinstance(fig, go.Figure)

    def test_has_data_traces(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.scatter_map(layer)
        assert len(fig.data) >= 1

    def test_center_is_approximate(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.scatter_map(layer)
        center = fig.layout.mapbox.center
        # Centroid of the sample wells is roughly lon=-90.3, lat=28.2
        assert -92.0 < center.lon < -89.0
        assert 27.0 < center.lat < 29.0

    def test_custom_title(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.scatter_map(layer, title="My Wells")
        assert fig.layout.title.text == "My Wells"


# ------------------------------------------------------------------
# scatter_map with color_column
# ------------------------------------------------------------------


class TestScatterMapColored:
    """Scatter map with colour-coded markers."""

    def test_creates_separate_traces_per_color(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.scatter_map(layer, color_column="field")
        # 4 unique fields -> 4 traces
        assert len(fig.data) == 4

    def test_trace_names_match_column_values(
        self, sample_wells_geojson: Path
    ) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.scatter_map(layer, color_column="status")
        trace_names = {t.name for t in fig.data}
        assert "active" in trace_names
        assert "decommissioned" in trace_names


# ------------------------------------------------------------------
# pipeline_map
# ------------------------------------------------------------------


class TestPipelineMap:
    """Create a map with pipeline routes as lines."""

    @pytest.fixture
    def sample_routes(self) -> list[dict]:
        return [
            {
                "name": "Pipeline A",
                "coordinates": [(-90.5, 28.1), (-90.3, 28.2), (-90.0, 28.3)],
            },
            {
                "name": "Pipeline B",
                "coordinates": [(-91.0, 27.9), (-90.8, 28.0)],
            },
        ]

    def test_returns_figure(self, sample_routes: list[dict]) -> None:
        fig = PlotlyMapBuilder.pipeline_map(sample_routes)
        assert isinstance(fig, go.Figure)

    def test_has_line_traces(self, sample_routes: list[dict]) -> None:
        fig = PlotlyMapBuilder.pipeline_map(sample_routes)
        assert len(fig.data) == 2
        for trace in fig.data:
            assert trace.mode == "lines"

    def test_trace_names(self, sample_routes: list[dict]) -> None:
        fig = PlotlyMapBuilder.pipeline_map(sample_routes)
        names = [t.name for t in fig.data]
        assert "Pipeline A" in names
        assert "Pipeline B" in names


# ------------------------------------------------------------------
# well_dashboard
# ------------------------------------------------------------------


class TestWellDashboard:
    """Create a subplot dashboard: map + bar chart + histogram."""

    def test_returns_figure(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.well_dashboard(layer)
        assert isinstance(fig, go.Figure)

    def test_has_multiple_traces(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.well_dashboard(layer)
        # At least: scattermapbox + bar + histogram = 3
        assert len(fig.data) >= 3

    def test_contains_scattermapbox_trace(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.well_dashboard(layer)
        mapbox_traces = [t for t in fig.data if isinstance(t, go.Scattermapbox)]
        assert len(mapbox_traces) >= 1

    def test_contains_bar_trace(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.well_dashboard(layer)
        bar_traces = [t for t in fig.data if isinstance(t, go.Bar)]
        assert len(bar_traces) >= 1

    def test_contains_histogram_trace(self, sample_wells_geojson: Path) -> None:
        layer = WellLayer.from_geojson(sample_wells_geojson)
        fig = PlotlyMapBuilder.well_dashboard(layer)
        hist_traces = [t for t in fig.data if isinstance(t, go.Histogram)]
        assert len(hist_traces) >= 1


# ------------------------------------------------------------------
# save_html
# ------------------------------------------------------------------


class TestSaveHtml:
    """Save a Plotly Figure to an HTML file."""

    def test_creates_html_file(self, tmp_path: Path) -> None:
        fig = go.Figure(go.Scatter(x=[1, 2], y=[3, 4]))
        filepath = tmp_path / "test_map.html"
        result = PlotlyMapBuilder.save_html(fig, filepath)
        assert result == filepath
        assert filepath.exists()

    def test_file_contains_html(self, tmp_path: Path) -> None:
        fig = go.Figure(go.Scatter(x=[1, 2], y=[3, 4]))
        filepath = tmp_path / "test_map.html"
        PlotlyMapBuilder.save_html(fig, filepath)
        content = filepath.read_text()
        assert "<html>" in content.lower() or "plotly" in content.lower()

    def test_creates_parent_directories(self, tmp_path: Path) -> None:
        fig = go.Figure(go.Scatter(x=[1, 2], y=[3, 4]))
        filepath = tmp_path / "sub" / "dir" / "map.html"
        result = PlotlyMapBuilder.save_html(fig, filepath)
        assert result == filepath
        assert filepath.exists()
