"""Shared fixtures for GIS module tests."""

from __future__ import annotations

from pathlib import Path

import pytest

TEST_DATA_DIR = Path(__file__).parent / "test_data"


@pytest.fixture
def test_data_dir() -> Path:
    """Path to the test_data directory."""
    return TEST_DATA_DIR


@pytest.fixture
def sample_wells_geojson(test_data_dir: Path) -> Path:
    """Path to sample_wells.geojson fixture."""
    return test_data_dir / "sample_wells.geojson"


@pytest.fixture
def sample_polygons_kml(test_data_dir: Path) -> Path:
    """Path to sample_polygons.kml fixture."""
    return test_data_dir / "sample_polygons.kml"


@pytest.fixture
def temporal_wells_geojson(test_data_dir: Path) -> Path:
    """Path to temporal_wells.geojson fixture."""
    return test_data_dir / "temporal_wells.geojson"


@pytest.fixture
def gulf_of_mexico_coords() -> dict:
    """Representative Gulf of Mexico coordinates for testing."""
    return {
        "longitude": -90.5,
        "latitude": 28.1,
        "utm_zone": 15,
        "hemisphere": "N",
        "epsg_utm": 32615,
    }
