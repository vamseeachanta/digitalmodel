# API Specification

This is the API specification for the spec detailed in @specs/modules/data-procurement/web-api-integration/spec.md

> Created: 2025-01-09
> Version: 1.0.0

## Internal API Endpoints

### Data Procurement Gateway API

These are the internal APIs that other DigitalModel modules will use to access external data sources.

## Python Module Interface

### Get Ocean Data

**Module:** `digitalmodel.data_procurement.api_gateway`
**Method:** `get_ocean_data(location, parameters, time_range, source=None)`

**Purpose:** Retrieve ocean data (waves, currents, tides) from configured APIs

**Parameters:**
- `location`: Dict with `latitude` and `longitude` keys
- `parameters`: List of requested parameters (`wave_height`, `wave_period`, `current_speed`, etc.)
- `time_range`: Dict with `start` and `end` datetime objects
- `source`: Optional API source preference (`noaa`, `openmeteo`, `stormglass`, or `auto`)

**Response:**
```python
{
    "status": "success",
    "source": "noaa",
    "data": {
        "wave_height": [1.2, 1.3, 1.4, ...],
        "wave_period": [8.5, 8.6, 8.7, ...],
        "timestamps": ["2025-01-09T00:00:00Z", ...]
    },
    "metadata": {
        "location": {"lat": 29.0, "lon": -94.0},
        "units": {"wave_height": "meters", "wave_period": "seconds"},
        "cache_hit": false,
        "response_time_ms": 1250
    }
}
```

**Errors:**
- `APIConnectionError`: Unable to connect to any configured API
- `RateLimitError`: API rate limit exceeded
- `InvalidParameterError`: Requested parameter not available
- `AuthenticationError`: Invalid or missing API credentials

### Get Weather Data

**Module:** `digitalmodel.data_procurement.api_gateway`
**Method:** `get_weather_data(location, parameters, time_range, source=None)`

**Purpose:** Retrieve weather data (wind, temperature, pressure) from configured APIs

**Parameters:**
- `location`: Dict with `latitude` and `longitude` keys
- `parameters`: List of requested parameters (`wind_speed`, `wind_direction`, `temperature`, etc.)
- `time_range`: Dict with `start` and `end` datetime objects
- `source`: Optional API source preference

**Response:** Similar structure to ocean data response

### Batch Data Request

**Module:** `digitalmodel.data_procurement.api_gateway`
**Method:** `batch_request(requests)`

**Purpose:** Process multiple data requests concurrently for improved performance

**Parameters:**
- `requests`: List of request dictionaries, each containing:
  - `type`: `ocean` or `weather`
  - `location`: Location dictionary
  - `parameters`: List of parameters
  - `time_range`: Time range dictionary

**Response:**
```python
{
    "status": "success",
    "results": [
        {"request_id": 0, "data": {...}},
        {"request_id": 1, "data": {...}},
        ...
    ],
    "metadata": {
        "total_requests": 5,
        "successful": 5,
        "failed": 0,
        "total_time_ms": 2500
    }
}
```

## CLI Interface

### Command Line Usage

```bash
# Get ocean data for a location
python -m digitalmodel.data_procurement.cli ocean \
    --lat 29.0 --lon -94.0 \
    --parameters wave_height wave_period \
    --start "2025-01-09T00:00:00" \
    --end "2025-01-10T00:00:00" \
    --output ocean_data.csv

# Get weather data
python -m digitalmodel.data_procurement.cli weather \
    --lat 29.0 --lon -94.0 \
    --parameters wind_speed wind_direction temperature \
    --hours 48 \
    --output weather_data.csv

# Batch request from YAML config
python -m digitalmodel.data_procurement.cli batch \
    --config batch_requests.yml \
    --output-dir ./results/
```

## Configuration API

### YAML Configuration Schema

```yaml
# data_procurement_config.yml
api_sources:
  noaa:
    enabled: true
    api_key: ${NOAA_API_KEY}  # From environment variable
    base_url: "https://api.tidesandcurrents.noaa.gov/api/prod/"
    rate_limit:
      requests_per_second: 5
      daily_limit: 10000
    
  openmeteo:
    enabled: true
    api_key: null  # No key required for free tier
    base_url: "https://marine-api.open-meteo.com/v1/"
    rate_limit:
      requests_per_second: 10
      daily_limit: 50000
    
  stormglass:
    enabled: true
    api_key: ${STORMGLASS_API_KEY}
    base_url: "https://api.stormglass.io/v2/"
    rate_limit:
      requests_per_second: 1
      daily_limit: 50  # Free tier limit

cache_config:
  enabled: true
  backend: "memory"  # or "redis", "file"
  ttl:
    ocean_data: 1800  # 30 minutes in seconds
    weather_data: 3600  # 1 hour
    historical_data: 86400  # 24 hours
  max_size_mb: 100

retry_config:
  max_attempts: 3
  initial_delay_seconds: 1
  max_delay_seconds: 30
  exponential_base: 2

fallback_strategy:
  order: ["noaa", "openmeteo", "stormglass"]
  use_cache_on_failure: true
  
logging:
  level: "INFO"
  file: "data_procurement.log"
  format: "json"
```

## Integration with Existing Modules

### OrcaFlex Module Integration

```python
from digitalmodel.data_procurement import api_gateway

class OrcaFlexAnalysis:
    def __init__(self, config):
        self.data_gateway = api_gateway.DataGateway(config["data_procurement"])
        
    def get_environmental_data(self, location, time_range):
        """Fetch environmental data for OrcaFlex simulation."""
        ocean_data = self.data_gateway.get_ocean_data(
            location=location,
            parameters=["wave_height", "wave_period", "current_speed"],
            time_range=time_range
        )
        
        weather_data = self.data_gateway.get_weather_data(
            location=location,
            parameters=["wind_speed", "wind_direction"],
            time_range=time_range
        )
        
        return self._format_for_orcaflex(ocean_data, weather_data)
```

### AQWA Module Integration

```python
from digitalmodel.data_procurement import api_gateway

def prepare_aqwa_environment(config_file):
    """Prepare environmental conditions for AQWA analysis."""
    gateway = api_gateway.DataGateway.from_config(config_file)
    
    # Get wave spectrum data
    wave_data = gateway.get_ocean_data(
        location=config["analysis_location"],
        parameters=["wave_height", "wave_period", "wave_direction"],
        time_range=config["time_range"]
    )
    
    return convert_to_aqwa_format(wave_data)
```

## Error Handling

All API methods follow consistent error handling patterns:

```python
try:
    data = gateway.get_ocean_data(...)
except APIConnectionError as e:
    logger.error(f"API connection failed: {e}")
    # Attempt fallback or use cached data
except RateLimitError as e:
    logger.warning(f"Rate limit reached: {e}")
    # Wait and retry or use alternative source
except AuthenticationError as e:
    logger.error(f"Authentication failed: {e}")
    # Check credentials and notify user
except Exception as e:
    logger.critical(f"Unexpected error: {e}")
    # Log for debugging and return safe default
```