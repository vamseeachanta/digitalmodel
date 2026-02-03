# ABOUTME: Example demonstrating zero-storage streaming metocean data procurement
# ABOUTME: Date-based queries with in-memory processing and direct consumption

"""
Metocean Streaming Example
===========================

Demonstrates zero-storage metocean data procurement with:
- Date-based API queries (ERA5, NOAA, Open-Meteo)
- Streaming in-memory processing
- Direct consumption (no file saving)
- Quality control and statistics

Critical: NO data saving (data can be huge and infeasible)!

This example shows how to:
1. Query 20 years of wave data
2. Process in-memory without storing GB files
3. Calculate extreme statistics
4. Pass directly to OrcaFlex/AQWA
"""

import sys
from pathlib import Path
from datetime import datetime, timedelta

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

from digitalmodel.data_systems.data_procurement.metocean import MetoceanClient


def example_streaming_historical_data():
    """
    Example 1: Stream 20 years of historical wave data (NO file storage).

    Demonstrates:
    - Date-based query for large time range
    - Streaming processing without storing GB files
    - In-memory extreme value analysis
    """
    print("\n=== Example 1: Streaming Historical Data (Zero Storage) ===\n")

    # Initialize client from config
    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/metocean-data/configs/example_config.yml'

    client = MetoceanClient.from_config(str(config_path))

    # Query parameters
    start_date = datetime(2000, 1, 1)
    end_date = datetime(2020, 12, 31)
    location = {"lat": 29.5, "lon": -88.5}  # Gulf of Mexico
    parameters = ["wave_height", "wave_period", "wind_speed_10m"]

    print(f"Querying metocean data from {start_date} to {end_date}")
    print(f"Location: ({location['lat']}, {location['lon']})")
    print(f"Parameters: {parameters}")
    print(f"\nCritical: Data streamed on-demand, NOT saved to disk!\n")

    # Stream data (zero storage)
    record_count = 0
    wave_heights = []

    for record in client.query_metocean(
        start_date=start_date,
        end_date=end_date,
        location=location,
        parameters=parameters
    ):
        record_count += 1

        # Process in-memory (example: collect wave heights for statistics)
        if record.get('wave_height'):
            wave_heights.append(record['wave_height'])

        # Print progress every 10,000 records
        if record_count % 10000 == 0:
            print(f"Processed {record_count} records (in-memory, not saved)...")

    print(f"\nCompleted: Processed {record_count} records")
    print(f"Wave height data points: {len(wave_heights)}")
    print(f"\nStorage footprint: ZERO bytes (data discarded after processing)")

    # Calculate statistics (in-memory)
    if wave_heights:
        import numpy as np

        print(f"\nWave Height Statistics:")
        print(f"  Mean: {np.mean(wave_heights):.2f} m")
        print(f"  Max: {np.max(wave_heights):.2f} m")
        print(f"  Std: {np.std(wave_heights):.2f} m")
        print(f"  95th percentile: {np.percentile(wave_heights, 95):.2f} m")

    print("\nNote: To re-analyze data, query API again (no stored files)")


def example_extreme_value_analysis():
    """
    Example 2: Extreme value analysis on streaming data (in-memory).

    Demonstrates:
    - Streaming data for statistical analysis
    - In-memory extreme value distribution fitting
    - Calculate 100-year return values
    """
    print("\n=== Example 2: Extreme Value Analysis (Streaming) ===\n")

    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/metocean-data/configs/example_config.yml'

    client = MetoceanClient.from_config(str(config_path))

    # Query 10 years for extreme analysis
    start_date = datetime(2010, 1, 1)
    end_date = datetime(2020, 12, 31)
    location = {"lat": 29.5, "lon": -88.5}

    print("Calculating extreme wave heights (100-year return value)")
    print("Data streamed from API, processed in-memory\n")

    # Stream data and calculate extremes (in-memory)
    stream = client.query_metocean(
        start_date=start_date,
        end_date=end_date,
        location=location,
        parameters=["wave_height"]
    )

    # Calculate extreme values
    extremes = client.calculate_extremes(
        stream,
        parameter="wave_height",
        return_periods=[1, 10, 50, 100]
    )

    print("Extreme Wave Heights (Gumbel distribution):")
    for period, value in extremes.items():
        print(f"  {period:3d}-year return: {value:.2f} m")

    print("\nNote: Data discarded after analysis (zero storage)")


def example_direct_consumption():
    """
    Example 3: Direct consumption by OrcaFlex (NO intermediate files).

    Demonstrates:
    - Query data via API
    - Stream directly to OrcaFlex YAML format
    - No intermediate NetCDF/CSV files saved
    """
    print("\n=== Example 3: Direct Consumption (OrcaFlex) ===\n")

    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/metocean-data/configs/example_config.yml'

    client = MetoceanClient.from_config(str(config_path))

    # Query recent data
    start_date = datetime.now() - timedelta(days=30)
    end_date = datetime.now()
    location = {"lat": 29.5, "lon": -88.5}

    print(f"Streaming metocean data to OrcaFlex format")
    print(f"Date range: {start_date.date()} to {end_date.date()}")
    print(f"Output: In-memory YAML (no file saving)\n")

    # Stream to OrcaFlex format (in-memory)
    yaml_blocks = client.stream_to_orcaflex(
        start_date=start_date,
        end_date=end_date,
        location=location,
        parameters=["wave_height", "wave_period", "wave_direction"]
    )

    # Consume YAML blocks (example: count lines)
    block_count = 0

    for yaml_block in yaml_blocks:
        block_count += 1

        # In production, this would be consumed directly by OrcaFlex API
        # For this example, just count blocks
        if block_count <= 3:
            print(f"YAML Block {block_count}:")
            print(yaml_block[:200] + "..." if len(yaml_block) > 200 else yaml_block)
            print()

    print(f"Generated {block_count} OrcaFlex YAML blocks (in-memory)")
    print("Note: In production, pass directly to OrcaFlex API (no file I/O)")


def example_real_time_forecast():
    """
    Example 4: Real-time marine forecast (Open-Meteo, streaming).

    Demonstrates:
    - 7-day forecast retrieval
    - Real-time data streaming
    - Direct consumption without storage
    """
    print("\n=== Example 4: Real-Time Marine Forecast ===\n")

    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/metocean-data/configs/example_config.yml'

    client = MetoceanClient.from_config(str(config_path))

    # Query 7-day forecast
    start_date = datetime.now()
    end_date = datetime.now() + timedelta(days=7)
    location = {"lat": 29.5, "lon": -88.5}

    print(f"Retrieving 7-day marine forecast")
    print(f"Location: ({location['lat']}, {location['lon']})")
    print(f"Provider: Open-Meteo (FREE, unlimited)\n")

    # Stream forecast data
    forecast_count = 0

    for record in client.query_metocean(
        start_date=start_date,
        end_date=end_date,
        location=location,
        parameters=["wave_height", "wind_speed_10m"],
        provider="Open-Meteo"  # Force Open-Meteo for forecast
    ):
        forecast_count += 1

        # Print first few records
        if forecast_count <= 24:  # First 24 hours
            print(f"{record['timestamp']}: "
                  f"Hs={record.get('wave_height', 'N/A')}m, "
                  f"Wind={record.get('wind_speed_10m', 'N/A')}m/s")

    print(f"\nReceived {forecast_count} hourly forecast records (in-memory)")
    print("Note: Each API call gets latest forecast (no caching needed)")


def example_buoy_validation():
    """
    Example 5: NOAA buoy data for model validation (streaming).

    Demonstrates:
    - Real buoy measurements (NOAA NDBC)
    - Historical buoy data retrieval
    - Streaming comparison with model results
    """
    print("\n=== Example 5: NOAA Buoy Data (Real Measurements) ===\n")

    config_path = Path(__file__).parent.parent / \
                  'specs/modules/data-procurement/metocean-data/configs/example_config.yml'

    client = MetoceanClient.from_config(str(config_path))

    # Query buoy data
    start_date = datetime(2023, 1, 1)
    end_date = datetime(2023, 12, 31)
    location = {"lat": 29.2, "lon": -88.2}  # Near buoy 42040

    print(f"Retrieving NOAA buoy measurements")
    print(f"Location: ({location['lat']}, {location['lon']})")
    print(f"Nearest buoy: 42040 (Mississippi Canyon)\n")

    # Stream buoy data
    buoy_count = 0

    for record in client.query_metocean(
        start_date=start_date,
        end_date=end_date,
        location=location,
        provider="NOAA"  # Force NOAA buoy data
    ):
        buoy_count += 1

        # Print sample records
        if buoy_count <= 10:
            print(f"{record['timestamp']}: "
                  f"Hs={record.get('wave_height', 'N/A')}m, "
                  f"Tp={record.get('dominant_wave_period', 'N/A')}s, "
                  f"Wind={record.get('wind_speed', 'N/A')}m/s")

    print(f"\nReceived {buoy_count} buoy measurement records (in-memory)")
    print("Note: Real measurements from offshore buoy (validation-ready)")


def main():
    """
    Run all metocean streaming examples.

    Critical: All examples demonstrate ZERO STORAGE architecture!
    """
    print("=" * 70)
    print("Metocean Data Procurement - Zero Storage Examples")
    print("=" * 70)

    print("\nKey Principles:")
    print("  1. Date-based API queries for on-demand retrieval")
    print("  2. Streaming in-memory processing")
    print("  3. Direct consumption (OrcaFlex, AQWA, Python)")
    print("  4. NO data file storage (data can be huge and infeasible)")
    print("  5. Retrieve fresh data on every analysis\n")

    try:
        # Run examples
        example_streaming_historical_data()
        example_extreme_value_analysis()
        example_direct_consumption()
        example_real_time_forecast()
        example_buoy_validation()

        print("\n" + "=" * 70)
        print("All examples completed successfully!")
        print("=" * 70)

        print("\nNext Steps:")
        print("  1. Configure API keys in example_config.yml")
        print("  2. Run integration tests with real APIs")
        print("  3. Integrate with OrcaFlex/AQWA workflows")
        print("  4. Customize processing pipelines")

    except Exception as e:
        print(f"\nError running examples: {e}")
        print("\nTroubleshooting:")
        print("  - Check API keys are set in config")
        print("  - Verify internet connection")
        print("  - Check FREE API rate limits")
        print("  - Review logs for details")

        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
