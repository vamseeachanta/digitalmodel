"""Tests for RasterLayer -- raster/grid data layer for bathymetry."""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.specialized.gis.core.crs import CRSDefinition, CRSType
from digitalmodel.specialized.gis.layers.raster_layer import RasterLayer


# ------------------------------------------------------------------
# Fixtures
# ------------------------------------------------------------------


@pytest.fixture
def bathymetry_data() -> np.ndarray:
    """A 4x5 synthetic bathymetry grid with depths in metres."""
    return np.array(
        [
            [-50.0, -100.0, -150.0, -200.0, -250.0],
            [-60.0, -120.0, -180.0, -220.0, -260.0],
            [-70.0, -130.0, -190.0, -230.0, -270.0],
            [-80.0, -140.0, -200.0, -240.0, -280.0],
        ],
        dtype=np.float64,
    )


@pytest.fixture
def gulf_bounds() -> tuple[float, float, float, float]:
    """Bounds covering a region in the Gulf of Mexico."""
    return (-91.0, 27.0, -89.0, 29.0)


@pytest.fixture
def raster_layer(
    bathymetry_data: np.ndarray, gulf_bounds: tuple
) -> RasterLayer:
    """A fully configured RasterLayer."""
    return RasterLayer(
        data=bathymetry_data,
        crs=CRSDefinition.wgs84(),
        name="test_bathymetry",
        bounds=gulf_bounds,
        nodata=-9999.0,
    )


# ------------------------------------------------------------------
# Construction
# ------------------------------------------------------------------


class TestConstruction:
    """Verify RasterLayer construction and defaults."""

    def test_basic_construction(self, bathymetry_data: np.ndarray) -> None:
        layer = RasterLayer(data=bathymetry_data)
        assert layer.data is bathymetry_data
        assert layer.name == "raster"

    def test_defaults_to_wgs84(self, bathymetry_data: np.ndarray) -> None:
        layer = RasterLayer(data=bathymetry_data)
        assert layer.crs.crs_type == CRSType.WGS84
        assert layer.crs.epsg_code == 4326

    def test_custom_crs(self, bathymetry_data: np.ndarray) -> None:
        utm_crs = CRSDefinition.utm_from_zone(16, "N")
        layer = RasterLayer(data=bathymetry_data, crs=utm_crs)
        assert layer.crs.crs_type == CRSType.UTM
        assert layer.crs.epsg_code == 32616


# ------------------------------------------------------------------
# Properties
# ------------------------------------------------------------------


class TestProperties:
    """Verify properties return expected values."""

    def test_shape(self, raster_layer: RasterLayer) -> None:
        assert raster_layer.shape == (4, 5)

    def test_bounds(
        self, raster_layer: RasterLayer, gulf_bounds: tuple
    ) -> None:
        assert raster_layer.bounds == gulf_bounds

    def test_name(self, raster_layer: RasterLayer) -> None:
        assert raster_layer.name == "test_bathymetry"

    def test_nodata(self, raster_layer: RasterLayer) -> None:
        assert raster_layer.nodata == -9999.0

    def test_resolution(self, raster_layer: RasterLayer) -> None:
        res = raster_layer.resolution
        assert res is not None
        x_res, y_res = res
        # bounds: (-91, 27) to (-89, 29), shape (4, 5)
        assert x_res == pytest.approx(2.0 / 5)
        assert y_res == pytest.approx(2.0 / 4)

    def test_resolution_none_without_bounds(
        self, bathymetry_data: np.ndarray
    ) -> None:
        layer = RasterLayer(data=bathymetry_data)
        assert layer.resolution is None


# ------------------------------------------------------------------
# sample_point
# ------------------------------------------------------------------


class TestSamplePoint:
    """Sample raster values at geographic coordinates."""

    def test_sample_in_bounds(self, raster_layer: RasterLayer) -> None:
        value = raster_layer.sample_point(-90.0, 28.0)
        assert value is not None
        assert isinstance(value, float)

    def test_sample_out_of_bounds(self, raster_layer: RasterLayer) -> None:
        value = raster_layer.sample_point(-95.0, 28.0)
        assert value is None

    def test_sample_returns_none_for_nodata(self) -> None:
        data = np.array([[1.0, -9999.0], [3.0, 4.0]], dtype=np.float64)
        layer = RasterLayer(
            data=data,
            bounds=(0.0, 0.0, 2.0, 2.0),
            nodata=-9999.0,
        )
        # Top-right cell (col=1, row=0) contains nodata
        value = layer.sample_point(1.5, 1.5)
        assert value is None

    def test_sample_without_bounds(
        self, bathymetry_data: np.ndarray
    ) -> None:
        layer = RasterLayer(data=bathymetry_data)
        value = layer.sample_point(0.0, 0.0)
        assert value is None


# ------------------------------------------------------------------
# to_xyz_points
# ------------------------------------------------------------------


class TestToXyzPoints:
    """Convert raster to xyz point list."""

    def test_returns_list_of_tuples(self, raster_layer: RasterLayer) -> None:
        points = raster_layer.to_xyz_points()
        assert isinstance(points, list)
        assert all(isinstance(p, tuple) and len(p) == 3 for p in points)

    def test_correct_count(self, raster_layer: RasterLayer) -> None:
        # No nodata values in the grid, so all 20 cells should be present
        points = raster_layer.to_xyz_points()
        assert len(points) == 20

    def test_skips_nodata_values(self) -> None:
        data = np.array([[1.0, -9999.0], [3.0, 4.0]], dtype=np.float64)
        layer = RasterLayer(
            data=data,
            bounds=(0.0, 0.0, 2.0, 2.0),
            nodata=-9999.0,
        )
        points = layer.to_xyz_points()
        assert len(points) == 3

    def test_empty_without_bounds(
        self, bathymetry_data: np.ndarray
    ) -> None:
        layer = RasterLayer(data=bathymetry_data)
        points = layer.to_xyz_points()
        assert points == []


# ------------------------------------------------------------------
# statistics
# ------------------------------------------------------------------


class TestStatistics:
    """Compute summary statistics."""

    def test_statistics_keys(self, raster_layer: RasterLayer) -> None:
        stats = raster_layer.statistics()
        assert "min" in stats
        assert "max" in stats
        assert "mean" in stats
        assert "std" in stats
        assert "nodata_count" in stats

    def test_statistics_values(self, raster_layer: RasterLayer) -> None:
        stats = raster_layer.statistics()
        assert stats["min"] == pytest.approx(-280.0)
        assert stats["max"] == pytest.approx(-50.0)
        assert stats["nodata_count"] == 0

    def test_statistics_with_nodata(self) -> None:
        data = np.array([[1.0, -9999.0], [3.0, 4.0]], dtype=np.float64)
        layer = RasterLayer(data=data, nodata=-9999.0)
        stats = layer.statistics()
        assert stats["nodata_count"] == 1
        assert stats["min"] == pytest.approx(1.0)
        assert stats["max"] == pytest.approx(4.0)
        assert stats["mean"] == pytest.approx(8.0 / 3)

    def test_statistics_all_nodata(self) -> None:
        data = np.full((2, 2), -9999.0, dtype=np.float64)
        layer = RasterLayer(data=data, nodata=-9999.0)
        stats = layer.statistics()
        assert stats["nodata_count"] == 4
        assert stats["min"] is None
        assert stats["max"] is None


# ------------------------------------------------------------------
# __repr__
# ------------------------------------------------------------------


class TestRepr:
    """Verify string representation."""

    def test_repr_contains_key_info(self, raster_layer: RasterLayer) -> None:
        text = repr(raster_layer)
        assert "RasterLayer" in text
        assert "test_bathymetry" in text
        assert "(4, 5)" in text
        assert "wgs84" in text


# ------------------------------------------------------------------
# GeoTIFF I/O (requires rasterio)
# ------------------------------------------------------------------


class TestGeotiffIO:
    """Round-trip GeoTIFF read/write via RasterLayer."""

    @pytest.fixture(autouse=True)
    def _require_rasterio(self) -> None:
        pytest.importorskip("rasterio")

    def test_to_geotiff_creates_file(
        self, raster_layer: RasterLayer, tmp_path: Path
    ) -> None:
        filepath = tmp_path / "export.tif"
        result = raster_layer.to_geotiff(filepath)
        assert result.exists()

    def test_from_geotiff_round_trip(
        self, raster_layer: RasterLayer, tmp_path: Path
    ) -> None:
        filepath = tmp_path / "round_trip.tif"
        raster_layer.to_geotiff(filepath)
        loaded = RasterLayer.from_geotiff(filepath, name="loaded")

        assert loaded.name == "loaded"
        assert loaded.shape == raster_layer.shape
        np.testing.assert_array_almost_equal(loaded.data, raster_layer.data)
