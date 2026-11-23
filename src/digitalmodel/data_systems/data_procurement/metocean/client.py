# ABOUTME: Unified metocean data procurement client across multiple FREE APIs
# ABOUTME: Automatic provider selection, fallbacks, and streaming date-based queries

"""
Metocean Client
===============

Unified interface for metocean data procurement from multiple FREE APIs.

Features:
- Automatic provider selection (ERA5, NOAA, Open-Meteo, GEBCO)
- Fallback mechanism (if primary fails, try fallback)
- Provider-agnostic date-based queries
- Streaming architecture with zero storage
- Configuration-driven (YAML)

Example:
    # Initialize from config
    client = MetoceanClient.from_config("path/to/config.yml")

    # Stream metocean data (zero storage)
    for record in client.query_metocean(
        start_date=datetime(2020, 1, 1),
        end_date=datetime(2020, 12, 31),
        location={"lat": 29.5, "lon": -88.5},
        parameters=["wave_height", "wind_speed_10m"]
    ):
        # Process in-memory (not saved)
        analyze_metocean(record)

Configuration:
    Uses YAML config from specs/modules/data-procurement/metocean-data/configs/
    Supports primary API + fallbacks for resilience
"""

import logging
from typing import Dict, Any, Iterator, Optional, List
from datetime import datetime
from pathlib import Path

from ..common.config_loader import ConfigLoader
from ..common.stream_handler import StreamHandler
from .api_clients import ERA5Client, NOAAClient, OpenMeteoClient, GEBCOClient

logger = logging.getLogger(__name__)


class MetoceanClient:
    """
    Unified metocean data procurement client.

    Automatically selects appropriate API based on configuration and data type.
    Falls back to alternative providers if primary fails.

    Architecture:
    - Date-based queries for on-demand retrieval
    - Streaming processing (zero storage footprint)
    - Direct consumption (OrcaFlex, AQWA, Python objects)
    """

    def __init__(self, config: Dict[str, Any]):
        """
        Initialize metocean client from configuration.

        Args:
            config: Configuration dictionary (typically loaded from YAML)
        """
        self.config = config

        # Initialize API clients based on configuration
        self.clients: Dict[str, Any] = {}
        self._initialize_clients()

        # Initialize stream handler for processing
        self.stream_handler = StreamHandler()

        # Get provider configuration
        self.primary_provider = config['apis'].get('primary', 'ERA5')
        self.fallback_providers = config['apis'].get('fallback', ['NOAA', 'Open-Meteo'])

        logger.info(f"Initialized MetoceanClient (primary: {self.primary_provider}, "
                   f"fallbacks: {self.fallback_providers})")

    @classmethod
    def from_config(cls, config_path: str) -> 'MetoceanClient':
        """
        Create client from YAML configuration file.

        Args:
            config_path: Path to YAML config file

        Returns:
            Initialized MetoceanClient

        Example:
            client = MetoceanClient.from_config(
                "specs/modules/data-procurement/metocean-data/configs/example_config.yml"
            )
        """
        config_loader = ConfigLoader(config_path)

        if not config_loader.validate_schema():
            raise ValueError(f"Invalid configuration: {config_path}")

        return cls(config_loader.to_dict())

    def _initialize_clients(self) -> None:
        """Initialize API clients based on configuration."""
        apis = self.config.get('apis', {})

        # ERA5
        if 'ERA5' in apis and apis['ERA5'].get('enabled', False):
            api_config = apis['ERA5']
            auth_config = api_config.get('authentication', {})

            if auth_config.get('key'):
                self.clients['ERA5'] = ERA5Client(
                    api_key=auth_config['key']
                )
                logger.info("Initialized ERA5 client")

        # NOAA
        if 'NOAA' in apis and apis['NOAA'].get('enabled', True):
            self.clients['NOAA'] = NOAAClient()
            logger.info("Initialized NOAA client")

        # Open-Meteo
        if 'Open-Meteo' in apis and apis['Open-Meteo'].get('enabled', True):
            self.clients['Open-Meteo'] = OpenMeteoClient()
            logger.info("Initialized Open-Meteo client")

        # GEBCO
        if 'GenericRAO' not in apis or apis.get('GEBCO', {}).get('enabled', True):
            self.clients['GEBCO'] = GEBCOClient()
            logger.info("Initialized GEBCO client")

    def query_metocean(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float],
                      parameters: List[str] = None,
                      provider: Optional[str] = None,
                      **kwargs) -> Iterator[Dict[str, Any]]:
        """
        Query metocean data with automatic provider selection (streaming, zero storage).

        Args:
            start_date: Start date/time
            end_date: End date/time
            location: Geographic location {"lat": float, "lon": float}
            parameters: Metocean parameters to retrieve:
                - "wave_height" (significant wave height, m)
                - "wave_period" (peak period, s)
                - "wave_direction" (mean direction, degrees)
                - "wind_speed_10m" (wind at 10m, m/s)
                - "wind_speed_100m" (wind at 100m, m/s)
                - "current_speed" (surface current, m/s)
                - "current_direction" (current direction, degrees)
            provider: Specific provider to use (None = automatic selection)
            **kwargs: Additional provider-specific parameters

        Yields:
            Metocean data records (streaming, not stored)

        Critical: Data streamed on-demand, NO file storage!

        Example:
            for record in client.query_metocean(
                start_date=datetime(2023, 1, 1),
                end_date=datetime(2023, 12, 31),
                location={"lat": 29.5, "lon": -88.5},
                parameters=["wave_height", "wind_speed_10m"]
            ):
                print(f"{record['timestamp']}: Hs={record['wave_height']}m")
                # Data discarded after use (retrieve again if needed)
        """
        parameters = parameters or ["wave_height", "wind_speed_10m"]

        # Determine which provider to use
        providers_to_try = []

        if provider:
            providers_to_try = [provider]
        else:
            providers_to_try = [self.primary_provider] + self.fallback_providers

        # Try providers in sequence until one succeeds
        last_error = None

        for provider_name in providers_to_try:
            if provider_name not in self.clients:
                logger.warning(f"Provider {provider_name} not initialized, skipping")
                continue

            logger.info(f"Querying {provider_name} for {start_date} to {end_date}")

            try:
                client = self.clients[provider_name]

                # Query data (streaming)
                yield from client.query_by_date(
                    start_date=start_date,
                    end_date=end_date,
                    location=location,
                    parameters=parameters,
                    **kwargs
                )

                # Success - don't try fallbacks
                return

            except Exception as e:
                last_error = e
                logger.warning(f"{provider_name} failed: {e}, trying next provider")
                continue

        # All providers failed
        raise RuntimeError(f"All metocean providers failed. Last error: {last_error}")

    def get_bathymetry(self, location: Dict[str, float]) -> float:
        """
        Get water depth at location.

        Args:
            location: Geographic location {"lat": float, "lon": float}

        Returns:
            Water depth in meters

        Example:
            depth = client.get_bathymetry({"lat": 29.5, "lon": -88.5})
            print(f"Water depth: {depth} m")
        """
        if 'GEBCO' not in self.clients:
            raise RuntimeError("GEBCO client not initialized")

        return self.clients['GEBCO'].get_depth(location['lat'], location['lon'])

    def stream_to_orcaflex(self, start_date: datetime, end_date: datetime,
                          location: Dict[str, float],
                          parameters: List[str] = None,
                          output_path: Optional[str] = None) -> Iterator[str]:
        """
        Stream metocean data in OrcaFlex YAML format (in-memory).

        Args:
            start_date: Start date
            end_date: End date
            location: Location dict
            parameters: Metocean parameters
            output_path: Optional output file path (if provided, writes to file)

        Yields:
            OrcaFlex YAML blocks (streaming, in-memory)

        Note: If output_path is None, caller must handle YAML output
              If output_path provided, writes directly to OrcaFlex
        """
        # Stream metocean data
        stream = self.query_metocean(start_date, end_date, location, parameters)

        # Convert to OrcaFlex format
        orcaflex_stream = self.stream_handler.to_orcaflex(stream, output_format='yml')

        # Either yield for in-memory use or write to file
        if output_path:
            with open(output_path, 'w') as f:
                for yaml_block in orcaflex_stream:
                    f.write(yaml_block)
                    f.write('\n')
            logger.info(f"Wrote OrcaFlex YAML to {output_path}")
        else:
            yield from orcaflex_stream

    def stream_to_aqwa(self, start_date: datetime, end_date: datetime,
                      location: Dict[str, float],
                      parameters: List[str] = None) -> Iterator[Dict[str, Any]]:
        """
        Stream metocean data for AQWA consumption (in-memory).

        Args:
            start_date: Start date
            end_date: End date
            location: Location dict
            parameters: Metocean parameters

        Yields:
            AQWA-formatted data records (streaming, in-memory)
        """
        # Stream metocean data
        stream = self.query_metocean(start_date, end_date, location, parameters)

        # Convert to AQWA format (specific formatting depends on AQWA requirements)
        for record in stream:
            # Transform to AQWA format
            aqwa_record = {
                'time': record['timestamp'].isoformat(),
                'significant_wave_height': record.get('wave_height'),
                'peak_period': record.get('wave_period'),
                'wave_direction': record.get('wave_direction'),
                'wind_speed': record.get('wind_speed_10m'),
                'current_speed': record.get('current_speed')
            }

            yield aqwa_record

    def apply_quality_control(self, stream: Iterator[Dict[str, Any]],
                             limits: Optional[Dict[str, tuple]] = None) -> Iterator[Dict[str, Any]]:
        """
        Apply quality control to streaming data.

        Args:
            stream: Metocean data stream
            limits: Parameter limits {"param": (min, max)}

        Yields:
            Quality-controlled records (streaming)
        """
        limits = limits or self.config.get('quality_control', {}).get('limits', {})

        from ..common.stream_handler import QualityControl

        for record in stream:
            # Range checks
            valid = True

            for param, (min_val, max_val) in limits.items():
                if param in record:
                    value = record[param]
                    if value is not None and not QualityControl.range_check(value, min_val, max_val):
                        logger.debug(f"QC failed for {param}={value} (range: {min_val}-{max_val})")
                        valid = False
                        break

            if valid:
                yield record

    def calculate_extremes(self, stream: Iterator[Dict[str, Any]],
                          parameter: str,
                          return_periods: List[int] = [1, 10, 50, 100]) -> Dict[int, float]:
        """
        Calculate extreme values from streaming data (in-memory).

        Args:
            stream: Metocean data stream
            parameter: Parameter for extreme analysis (e.g., "wave_height")
            return_periods: Return periods in years

        Returns:
            Extreme values for each return period

        Note: Accumulates data in-memory for statistical analysis
              (avoids storing full dataset to disk)
        """
        # Collect values (in-memory)
        values = []

        for record in stream:
            value = record.get(parameter)
            if value is not None:
                values.append(value)

        # Fit extreme value distribution (Gumbel)
        import numpy as np
        from scipy import stats

        values_array = np.array(values)

        # Fit Gumbel distribution
        params = stats.gumbel_r.fit(values_array)

        # Calculate return values
        results = {}

        for return_period in return_periods:
            # Exceedance probability
            p = 1 - 1/return_period

            # Return value
            return_value = stats.gumbel_r.ppf(p, *params)
            results[return_period] = float(return_value)

        logger.info(f"Calculated {len(return_periods)} extreme values from {len(values)} records")

        return results
