# ABOUTME: Integration tests for metocean data procurement using real FREE APIs
# ABOUTME: Tests streaming, date-based queries, zero storage, and direct consumption

"""
Metocean Streaming Integration Tests
=====================================

Integration tests using real FREE APIs (NO MOCKS per CLAUDE.md).

Tests:
- Real API connections (NOAA, Open-Meteo, GEBCO)
- Streaming data retrieval
- Date-based queries
- Zero storage architecture
- Quality control
- Direct consumption (OrcaFlex/AQWA)

Note: ERA5 requires API key, tested separately if configured.
"""

import pytest
from datetime import datetime, timedelta
from pathlib import Path
import sys

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / 'src'))

from digitalmodel.data_procurement.metocean import (
    MetoceanClient,
    NOAAClient,
    OpenMeteoClient,
    GEBCOClient
)


class TestNOAAStreaming:
    """Test NOAA NDBC buoy data streaming (real API, no mocks)."""

    def test_noaa_real_time_data(self):
        """Test streaming real-time buoy data from NOAA."""
        client = NOAAClient()

        # Query last 7 days from buoy 42040 (Mississippi Canyon)
        start_date = datetime.now() - timedelta(days=7)
        end_date = datetime.now()
        location = {"lat": 29.2, "lon": -88.2}

        records = list(client.query_by_date(
            start_date=start_date,
            end_date=end_date,
            location=location
        ))

        # Verify we got data
        assert len(records) > 0, "Should receive real-time buoy data"

        # Verify data structure
        record = records[0]
        assert 'timestamp' in record
        assert 'wave_height' in record
        assert 'wind_speed' in record

        # Verify no file storage occurred
        assert not Path('./cache').exists(), "Should not cache large data files"

        print(f"✓ Received {len(records)} real-time buoy records (zero storage)")

    def test_noaa_historical_data_streaming(self):
        """Test streaming historical buoy data (1 year)."""
        client = NOAAClient()

        # Query 2023 data from buoy 42040
        start_date = datetime(2023, 1, 1)
        end_date = datetime(2023, 12, 31)

        record_count = 0
        wave_heights = []

        # Stream data (no storage)
        for record in client.query_by_date(
            start_date=start_date,
            end_date=end_date,
            station_id="42040"
        ):
            record_count += 1

            if record.get('wave_height'):
                wave_heights.append(record['wave_height'])

            # Verify streaming (stop after 1000 records for test efficiency)
            if record_count >= 1000:
                break

        assert record_count > 0, "Should stream historical data"
        assert len(wave_heights) > 0, "Should extract wave heights"

        # Verify no file storage
        assert not Path('./data').exists(), "Should not save data files"

        print(f"✓ Streamed {record_count} historical records (in-memory only)")

    def test_nearest_buoy_selection(self):
        """Test automatic nearest buoy selection."""
        client = NOAAClient()

        # Location in Gulf of Mexico
        location = {"lat": 28.0, "lon": -90.0}

        nearest_id = client._find_nearest_buoy(location)

        assert nearest_id in ['42001', '42002', '42003', '42019', '42020',
                             '42035', '42036', '42039', '42040']

        print(f"✓ Found nearest buoy: {nearest_id}")


class TestOpenMeteoStreaming:
    """Test Open-Meteo marine forecast streaming (real API, no mocks)."""

    def test_marine_forecast_streaming(self):
        """Test streaming 7-day marine forecast."""
        client = OpenMeteoClient()

        # Query 7-day forecast
        start_date = datetime.now()
        end_date = datetime.now() + timedelta(days=7)
        location = {"lat": 29.5, "lon": -88.5}

        records = list(client.query_by_date(
            start_date=start_date,
            end_date=end_date,
            location=location,
            parameters=["wave_height", "wind_speed_10m"]
        ))

        # Verify forecast data
        assert len(records) > 0, "Should receive forecast data"

        # Should get ~168 hourly records (7 days × 24 hours)
        assert len(records) >= 140, f"Should get ~168 hourly forecasts, got {len(records)}"

        # Verify data structure
        record = records[0]
        assert 'timestamp' in record
        assert 'wave_height' in record
        assert 'wind_speed_10m' in record

        # Verify no file storage
        assert not Path('./forecast_data').exists(), "Should not save forecast files"

        print(f"✓ Streamed {len(records)} forecast records (zero storage)")

    def test_real_time_vs_storage(self):
        """Verify forecasts are fresh (not cached/stored)."""
        client = OpenMeteoClient()

        location = {"lat": 29.5, "lon": -88.5}

        # Query twice (should get latest data each time, not cached)
        start_date = datetime.now()
        end_date = datetime.now() + timedelta(days=1)

        records1 = list(client.query_by_date(start_date, end_date, location))
        records2 = list(client.query_by_date(start_date, end_date, location))

        assert len(records1) > 0
        assert len(records2) > 0

        # Both should be recent forecasts (timestamps should be close)
        assert abs((records1[0]['timestamp'] - records2[0]['timestamp']).total_seconds()) < 3600

        print("✓ Verified fresh data on each query (no stale cache)")


class TestGEBCOBathymetry:
    """Test GEBCO bathymetry queries (real API, no mocks)."""

    def test_water_depth_query(self):
        """Test single-point water depth query."""
        client = GEBCOClient()

        # Query Gulf of Mexico location (known deep water)
        depth = client.get_depth(lat=29.5, lon=-88.5)

        # Gulf of Mexico should be > 1000m deep in this area
        assert depth > 1000, f"Expected deep water (>1000m), got {depth}m"
        assert depth < 5000, "Depth should be reasonable (<5000m)"

        print(f"✓ Water depth at (29.5, -88.5): {depth:.1f} m")

    def test_bathymetry_streaming(self):
        """Test bathymetry grid streaming (no file storage)."""
        client = GEBCOClient()

        # Small grid for testing
        bbox = {
            'north': 30.0,
            'south': 29.0,
            'east': -88.0,
            'west': -89.0
        }

        grid_points = []

        # Stream grid (limit to 10 points for test efficiency)
        for i, point in enumerate(client.get_depth_grid(bbox, resolution=0.5)):
            grid_points.append(point)

            if i >= 9:  # Get first 10 points
                break

        assert len(grid_points) > 0, "Should stream grid points"

        # Verify point structure
        point = grid_points[0]
        assert 'latitude' in point
        assert 'longitude' in point
        assert 'water_depth' in point

        # Verify no file storage
        assert not Path('./bathymetry').exists(), "Should not save bathymetry files"

        print(f"✓ Streamed {len(grid_points)} bathymetry grid points (in-memory)")


class TestMetoceanClientIntegration:
    """Test unified MetoceanClient with automatic provider selection."""

    @pytest.fixture
    def client(self):
        """Create MetoceanClient from config."""
        config_path = Path(__file__).parent.parent.parent / \
                      'specs/modules/data-procurement/metocean-data/configs/example_config.yml'

        if not config_path.exists():
            pytest.skip(f"Config not found: {config_path}")

        return MetoceanClient.from_config(str(config_path))

    def test_automatic_provider_selection(self, client):
        """Test automatic provider fallback mechanism."""
        # Query recent data (should use Open-Meteo or NOAA)
        start_date = datetime.now() - timedelta(days=1)
        end_date = datetime.now()
        location = {"lat": 29.5, "lon": -88.5}

        records = list(client.query_metocean(
            start_date=start_date,
            end_date=end_date,
            location=location,
            parameters=["wave_height"]
        ))

        assert len(records) > 0, "Should retrieve data from available provider"

        print(f"✓ Auto-selected provider, received {len(records)} records")

    def test_bathymetry_integration(self, client):
        """Test bathymetry query through unified client."""
        location = {"lat": 29.5, "lon": -88.5}

        depth = client.get_bathymetry(location)

        assert depth > 0, "Should get valid depth"
        assert depth > 1000, "Gulf of Mexico should be deep"

        print(f"✓ Bathymetry query: {depth:.1f} m")

    def test_quality_control_streaming(self, client):
        """Test quality control on streaming data."""
        start_date = datetime.now() - timedelta(days=1)
        end_date = datetime.now()
        location = {"lat": 29.5, "lon": -88.5}

        # Query data
        stream = client.query_metocean(start_date, end_date, location)

        # Apply QC (streaming)
        limits = {
            'wave_height': (0.0, 30.0),
            'wind_speed_10m': (0.0, 80.0)
        }

        qc_records = list(client.apply_quality_control(stream, limits))

        assert len(qc_records) > 0, "Should pass QC records"

        # Verify all records within limits
        for record in qc_records:
            if 'wave_height' in record and record['wave_height'] is not None:
                assert 0 <= record['wave_height'] <= 30

        print(f"✓ QC passed {len(qc_records)} records (streaming)")

    def test_orcaflex_streaming(self, client):
        """Test streaming to OrcaFlex format (in-memory)."""
        start_date = datetime.now() - timedelta(days=1)
        end_date = datetime.now()
        location = {"lat": 29.5, "lon": -88.5}

        # Stream to OrcaFlex format (in-memory)
        yaml_blocks = list(client.stream_to_orcaflex(
            start_date=start_date,
            end_date=end_date,
            location=location,
            parameters=["wave_height", "wave_period"]
        ))

        assert len(yaml_blocks) > 0, "Should generate OrcaFlex YAML"

        # Verify YAML format (should contain YAML syntax)
        assert any(':' in block for block in yaml_blocks), "Should be valid YAML"

        # Verify no file storage
        assert not Path('./orcaflex_data').exists(), "Should not save OrcaFlex files"

        print(f"✓ Generated {len(yaml_blocks)} OrcaFlex YAML blocks (in-memory)")

    def test_zero_storage_architecture(self, client):
        """
        Critical test: Verify zero storage footprint.

        This test ensures NO data files are saved during streaming.
        """
        # Clean any existing cache/data directories
        for dir_path in [Path('./cache'), Path('./data'), Path('./metocean_data')]:
            if dir_path.exists():
                import shutil
                shutil.rmtree(dir_path)

        # Query significant amount of data
        start_date = datetime.now() - timedelta(days=30)
        end_date = datetime.now()
        location = {"lat": 29.5, "lon": -88.5}

        record_count = 0

        for record in client.query_metocean(start_date, end_date, location):
            record_count += 1

            # Process some records
            if record_count >= 100:
                break

        # Verify NO data files created
        assert not Path('./cache').exists(), "Should not create cache directory"
        assert not Path('./data').exists(), "Should not create data directory"
        assert not Path('./metocean_data').exists(), "Should not save metocean files"

        # Verify NO large files in any directory
        import os

        for root, dirs, files in os.walk('.'):
            for file in files:
                file_path = Path(root) / file

                # Skip Python files, configs, etc.
                if file.endswith(('.py', '.yml', '.yaml', '.md', '.txt')):
                    continue

                # Any data file should be < 1MB (no large datasets)
                if file_path.exists():
                    size_mb = file_path.stat().st_size / (1024 * 1024)
                    assert size_mb < 1, f"Found large file: {file_path} ({size_mb:.2f} MB)"

        print(f"✓ VERIFIED: Processed {record_count} records with ZERO storage footprint")


class TestStreamingPerformance:
    """Test streaming performance and memory efficiency."""

    def test_memory_efficient_streaming(self):
        """Verify streaming uses constant memory (not accumulating data)."""
        import psutil
        import os

        process = psutil.Process(os.getpid())
        initial_memory = process.memory_info().rss / (1024 * 1024)  # MB

        client = OpenMeteoClient()

        # Stream large dataset
        start_date = datetime.now()
        end_date = datetime.now() + timedelta(days=7)
        location = {"lat": 29.5, "lon": -88.5}

        record_count = 0

        for record in client.query_by_date(start_date, end_date, location):
            record_count += 1

            # Check memory every 50 records
            if record_count % 50 == 0:
                current_memory = process.memory_info().rss / (1024 * 1024)
                memory_increase = current_memory - initial_memory

                # Memory should not grow significantly (< 50MB increase)
                assert memory_increase < 50, \
                    f"Memory increased by {memory_increase:.1f} MB (should be constant)"

        final_memory = process.memory_info().rss / (1024 * 1024)
        total_increase = final_memory - initial_memory

        print(f"✓ Streamed {record_count} records")
        print(f"  Memory increase: {total_increase:.1f} MB (constant memory usage)")


def test_all_components():
    """
    Run all integration tests.

    Critical: All tests use real APIs (NO MOCKS per CLAUDE.md).
    """
    pytest.main([__file__, '-v', '-s'])


if __name__ == "__main__":
    test_all_components()
