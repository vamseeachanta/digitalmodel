"""Tests for GeoTIFFHandler -- GeoTIFF read/write for bathymetry data."""

from __future__ import annotations

from pathlib import Path
from unittest.mock import patch

import numpy as np
import pytest


# ------------------------------------------------------------------
# Import and rasterio availability
# ------------------------------------------------------------------


class TestImportError:
    """Verify ImportError is raised when rasterio is not available."""

    def test_read_raises_import_error_without_rasterio(self, tmp_path: Path) -> None:
        from digitalmodel.specialized.gis.io import geotiff_handler

        with patch.object(geotiff_handler, "HAS_RASTERIO", False):
            with pytest.raises(ImportError, match="rasterio is required"):
                geotiff_handler.GeoTIFFHandler.read(tmp_path / "fake.tif")

    def test_write_raises_import_error_without_rasterio(self, tmp_path: Path) -> None:
        from digitalmodel.specialized.gis.io import geotiff_handler

        data = np.zeros((3, 3), dtype=np.float32)
        with patch.object(geotiff_handler, "HAS_RASTERIO", False):
            with pytest.raises(ImportError, match="rasterio is required"):
                geotiff_handler.GeoTIFFHandler.write(data, tmp_path / "fake.tif")

    def test_get_metadata_raises_import_error_without_rasterio(
        self, tmp_path: Path
    ) -> None:
        from digitalmodel.specialized.gis.io import geotiff_handler

        with patch.object(geotiff_handler, "HAS_RASTERIO", False):
            with pytest.raises(ImportError, match="rasterio is required"):
                geotiff_handler.GeoTIFFHandler.get_metadata(tmp_path / "fake.tif")


# ------------------------------------------------------------------
# Read / Write round-trip (requires rasterio)
# ------------------------------------------------------------------


class TestReadWrite:
    """Round-trip read/write tests requiring rasterio."""

    @pytest.fixture(autouse=True)
    def _require_rasterio(self) -> None:
        pytest.importorskip("rasterio")

    @pytest.fixture
    def sample_bathymetry(self) -> np.ndarray:
        """A small synthetic bathymetry grid (depth values)."""
        return np.array(
            [[-100.0, -200.0, -150.0], [-300.0, -250.0, -180.0]], dtype=np.float64
        )

    @pytest.fixture
    def sample_bounds(self) -> tuple[float, float, float, float]:
        return (-90.0, 27.0, -89.0, 28.0)

    def test_write_creates_file(
        self, sample_bathymetry: np.ndarray, tmp_path: Path, sample_bounds: tuple
    ) -> None:
        from digitalmodel.specialized.gis.io.geotiff_handler import GeoTIFFHandler

        filepath = tmp_path / "bathy.tif"
        result = GeoTIFFHandler.write(
            sample_bathymetry, filepath, bounds=sample_bounds
        )
        assert result.exists()
        assert result.stat().st_size > 0

    def test_read_returns_correct_keys(
        self, sample_bathymetry: np.ndarray, tmp_path: Path, sample_bounds: tuple
    ) -> None:
        from digitalmodel.specialized.gis.io.geotiff_handler import GeoTIFFHandler

        filepath = tmp_path / "bathy.tif"
        GeoTIFFHandler.write(sample_bathymetry, filepath, bounds=sample_bounds)
        result = GeoTIFFHandler.read(filepath)

        assert "data" in result
        assert "crs" in result
        assert "bounds" in result
        assert "resolution" in result
        assert "nodata" in result

    def test_round_trip_preserves_data(
        self, sample_bathymetry: np.ndarray, tmp_path: Path, sample_bounds: tuple
    ) -> None:
        from digitalmodel.specialized.gis.io.geotiff_handler import GeoTIFFHandler

        filepath = tmp_path / "bathy.tif"
        GeoTIFFHandler.write(sample_bathymetry, filepath, bounds=sample_bounds)
        result = GeoTIFFHandler.read(filepath)

        np.testing.assert_array_almost_equal(result["data"], sample_bathymetry)

    def test_round_trip_preserves_bounds(
        self, sample_bathymetry: np.ndarray, tmp_path: Path, sample_bounds: tuple
    ) -> None:
        from digitalmodel.specialized.gis.io.geotiff_handler import GeoTIFFHandler

        filepath = tmp_path / "bathy.tif"
        GeoTIFFHandler.write(sample_bathymetry, filepath, bounds=sample_bounds)
        result = GeoTIFFHandler.read(filepath)

        assert result["bounds"] == pytest.approx(sample_bounds, abs=1e-6)

    def test_write_with_nodata(
        self, tmp_path: Path, sample_bounds: tuple
    ) -> None:
        from digitalmodel.specialized.gis.io.geotiff_handler import GeoTIFFHandler

        data = np.array([[1.0, -9999.0], [3.0, 4.0]], dtype=np.float64)
        filepath = tmp_path / "nodata.tif"
        GeoTIFFHandler.write(data, filepath, bounds=sample_bounds, nodata=-9999.0)
        result = GeoTIFFHandler.read(filepath)

        assert result["nodata"] == -9999.0


# ------------------------------------------------------------------
# to_xyz (requires rasterio)
# ------------------------------------------------------------------


class TestToXyz:
    """Convert GeoTIFF to xyz point list."""

    @pytest.fixture(autouse=True)
    def _require_rasterio(self) -> None:
        pytest.importorskip("rasterio")

    def test_to_xyz_returns_points(self, tmp_path: Path) -> None:
        from digitalmodel.specialized.gis.io.geotiff_handler import GeoTIFFHandler

        data = np.array([[10.0, 20.0], [30.0, 40.0]], dtype=np.float64)
        bounds = (0.0, 0.0, 2.0, 2.0)
        filepath = tmp_path / "grid.tif"
        GeoTIFFHandler.write(data, filepath, bounds=bounds)

        points = GeoTIFFHandler.to_xyz(filepath)
        assert len(points) == 4
        assert all(len(p) == 3 for p in points)

    def test_to_xyz_skips_nodata(self, tmp_path: Path) -> None:
        from digitalmodel.specialized.gis.io.geotiff_handler import GeoTIFFHandler

        data = np.array([[10.0, -9999.0], [30.0, 40.0]], dtype=np.float64)
        bounds = (0.0, 0.0, 2.0, 2.0)
        filepath = tmp_path / "nodata_grid.tif"
        GeoTIFFHandler.write(data, filepath, bounds=bounds, nodata=-9999.0)

        points = GeoTIFFHandler.to_xyz(filepath)
        assert len(points) == 3


# ------------------------------------------------------------------
# get_metadata (requires rasterio)
# ------------------------------------------------------------------


class TestGetMetadata:
    """Retrieve metadata from a GeoTIFF file."""

    @pytest.fixture(autouse=True)
    def _require_rasterio(self) -> None:
        pytest.importorskip("rasterio")

    def test_metadata_keys(self, tmp_path: Path) -> None:
        from digitalmodel.specialized.gis.io.geotiff_handler import GeoTIFFHandler

        data = np.ones((4, 5), dtype=np.float32)
        bounds = (0.0, 0.0, 5.0, 4.0)
        filepath = tmp_path / "meta.tif"
        GeoTIFFHandler.write(data, filepath, bounds=bounds)

        meta = GeoTIFFHandler.get_metadata(filepath)
        assert meta["shape"] == (4, 5)
        assert "crs" in meta
        assert "bounds" in meta
        assert "resolution" in meta
        assert "nodata" in meta
        assert "dtype" in meta
