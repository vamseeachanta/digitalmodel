"""Tests for FoliumMapBuilder -- interactive web map generation."""

from __future__ import annotations

from pathlib import Path
from unittest.mock import patch

import pandas as pd
import pytest

from digitalmodel.modules.gis.integrations.folium_maps import (
    FoliumMapBuilder,
    HAS_FOLIUM,
)

needs_folium = pytest.mark.skipif(not HAS_FOLIUM, reason="folium not installed")


# ------------------------------------------------------------------
# ImportError when folium is missing (always runs, no folium needed)
# ------------------------------------------------------------------


class TestImportErrorWhenMissing:
    """Ensure ImportError is raised when folium is unavailable."""

    def test_raises_import_error_without_folium(self) -> None:
        with patch(
            "digitalmodel.modules.gis.integrations.folium_maps.HAS_FOLIUM", False
        ):
            with pytest.raises(ImportError, match="folium is required"):
                FoliumMapBuilder()


# ------------------------------------------------------------------
# Fixtures (only usable when folium is installed)
# ------------------------------------------------------------------


@pytest.fixture
def builder() -> FoliumMapBuilder:
    """A default FoliumMapBuilder centred on the Gulf of Mexico."""
    return FoliumMapBuilder()


@pytest.fixture
def sample_layer():
    """A minimal FeatureLayer for testing markers and heatmaps."""
    from digitalmodel.modules.gis.core.crs import CRSDefinition
    from digitalmodel.modules.gis.layers.feature_layer import FeatureLayer

    df = pd.DataFrame(
        {
            "longitude": [-90.0, -91.0, -89.5],
            "latitude": [28.0, 27.5, 29.0],
            "name": ["Well-A", "Well-B", "Well-C"],
            "status": ["active", "inactive", "active"],
            "production_bpd": [1500.0, 0.0, 3200.0],
        }
    )
    return FeatureLayer.from_dataframe(df, crs=CRSDefinition.wgs84())


# ------------------------------------------------------------------
# __init__
# ------------------------------------------------------------------


@needs_folium
class TestInit:
    """Verify builder initialisation and map defaults."""

    def test_creates_folium_map(self, builder: FoliumMapBuilder) -> None:
        import folium

        m = builder.get_map()
        assert isinstance(m, folium.Map)

    def test_default_center(self, builder: FoliumMapBuilder) -> None:
        m = builder.get_map()
        assert m.location == [28.0, -90.0]

    def test_custom_center(self) -> None:
        b = FoliumMapBuilder(center_lat=30.0, center_lon=-85.0, zoom_start=10)
        m = b.get_map()
        assert m.location == [30.0, -85.0]


# ------------------------------------------------------------------
# add_well_markers
# ------------------------------------------------------------------


@needs_folium
class TestAddWellMarkers:
    """Verify marker creation from a FeatureLayer."""

    def test_returns_self_for_chaining(
        self, builder: FoliumMapBuilder, sample_layer
    ) -> None:
        result = builder.add_well_markers(sample_layer)
        assert result is builder

    def test_markers_added_to_map(
        self, builder: FoliumMapBuilder, sample_layer
    ) -> None:
        builder.add_well_markers(sample_layer)
        html = builder.get_map().get_root().render()
        assert "Well-A" in html

    def test_popup_columns_filter(
        self, builder: FoliumMapBuilder, sample_layer
    ) -> None:
        builder.add_well_markers(sample_layer, popup_columns=["name"])
        html = builder.get_map().get_root().render()
        assert "Well-A" in html

    def test_color_map_applied(
        self, builder: FoliumMapBuilder, sample_layer
    ) -> None:
        color_map = {"active": "green", "inactive": "red"}
        builder.add_well_markers(
            sample_layer, color_column="status", color_map=color_map
        )
        html = builder.get_map().get_root().render()
        assert "green" in html


# ------------------------------------------------------------------
# add_pipeline_route
# ------------------------------------------------------------------


@needs_folium
class TestAddPipelineRoute:
    """Verify polyline creation for a pipeline route."""

    def test_returns_self_for_chaining(
        self, builder: FoliumMapBuilder
    ) -> None:
        coords = [(28.0, -90.0), (28.5, -89.5)]
        result = builder.add_pipeline_route(coords)
        assert result is builder

    def test_pipeline_in_html(self, builder: FoliumMapBuilder) -> None:
        coords = [(28.0, -90.0), (28.5, -89.5), (29.0, -89.0)]
        builder.add_pipeline_route(coords, name="Test Pipeline", color="red")
        html = builder.get_map().get_root().render()
        assert "Test Pipeline" in html


# ------------------------------------------------------------------
# add_lease_block
# ------------------------------------------------------------------


@needs_folium
class TestAddLeaseBlock:
    """Verify polygon creation for a lease block."""

    def test_returns_self_for_chaining(
        self, builder: FoliumMapBuilder
    ) -> None:
        coords = [(28.0, -90.0), (28.0, -89.0), (29.0, -89.0), (29.0, -90.0)]
        result = builder.add_lease_block(coords)
        assert result is builder

    def test_lease_block_in_html(self, builder: FoliumMapBuilder) -> None:
        coords = [(28.0, -90.0), (28.0, -89.0), (29.0, -89.0), (29.0, -90.0)]
        builder.add_lease_block(coords, name="Block A-123", color="blue")
        html = builder.get_map().get_root().render()
        assert "Block A-123" in html


# ------------------------------------------------------------------
# add_heatmap
# ------------------------------------------------------------------


@needs_folium
class TestAddHeatmap:
    """Verify heatmap layer creation."""

    def test_returns_self_for_chaining(
        self, builder: FoliumMapBuilder, sample_layer
    ) -> None:
        result = builder.add_heatmap(sample_layer)
        assert result is builder

    def test_heatmap_with_weight_column(
        self, builder: FoliumMapBuilder, sample_layer
    ) -> None:
        builder.add_heatmap(sample_layer, weight_column="production_bpd")
        html = builder.get_map().get_root().render()
        assert "HeatMap" in html


# ------------------------------------------------------------------
# save
# ------------------------------------------------------------------


@needs_folium
class TestSave:
    """Verify HTML export to disk."""

    def test_creates_html_file(
        self, builder: FoliumMapBuilder, tmp_path: Path
    ) -> None:
        output = tmp_path / "map.html"
        result = builder.save(output)
        assert result == output
        assert output.exists()
        assert output.stat().st_size > 0

    def test_html_contains_map_div(
        self, builder: FoliumMapBuilder, tmp_path: Path
    ) -> None:
        output = tmp_path / "map.html"
        builder.save(output)
        content = output.read_text()
        assert "<div" in content
        assert "leaflet" in content.lower()

    def test_save_creates_parent_directories(
        self, builder: FoliumMapBuilder, tmp_path: Path
    ) -> None:
        output = tmp_path / "nested" / "dir" / "map.html"
        result = builder.save(output)
        assert result.exists()

    def test_saved_map_contains_markers(
        self, builder: FoliumMapBuilder, sample_layer, tmp_path: Path
    ) -> None:
        builder.add_well_markers(sample_layer)
        output = tmp_path / "wells_map.html"
        builder.save(output)
        content = output.read_text()
        assert "Well-A" in content


# ------------------------------------------------------------------
# Chaining
# ------------------------------------------------------------------


@needs_folium
class TestMethodChaining:
    """Verify fluent builder pattern works end-to-end."""

    def test_full_chain(
        self, sample_layer, tmp_path: Path
    ) -> None:
        pipeline_coords = [(28.0, -90.0), (28.5, -89.5)]
        lease_coords = [(28.0, -90.0), (28.0, -89.0), (29.0, -89.0), (29.0, -90.0)]

        output = tmp_path / "chained.html"
        result = (
            FoliumMapBuilder(center_lat=28.0, center_lon=-90.0)
            .add_well_markers(sample_layer)
            .add_pipeline_route(pipeline_coords, name="Export Line")
            .add_lease_block(lease_coords, name="OCS-G-12345")
            .add_heatmap(sample_layer, weight_column="production_bpd")
            .save(output)
        )
        assert result.exists()
        content = output.read_text()
        assert "Well-A" in content
        assert "Export Line" in content
        assert "OCS-G-12345" in content
