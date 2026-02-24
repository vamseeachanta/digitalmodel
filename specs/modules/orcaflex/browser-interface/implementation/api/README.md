# OrcaFlex Browser REST API

## Overview
FastAPI-based REST API for browsing and analyzing OrcaFlex simulation data.

## Features
- 🚀 High-performance async API with FastAPI
- 📊 Real-time tension analysis
- 📈 Time series data access
- 🔍 Metadata-based filtering
- 📡 WebSocket support for live updates
- 📚 Auto-generated API documentation
- ⚡ In-memory caching for performance

## Installation

```bash
# Install dependencies
pip install -r requirements.txt

# Or install individually
pip install fastapi uvicorn pandas numpy
```

## Running the API

### Windows
```bash
# Use the batch file
run_api.bat

# Or run directly
python main.py
```

### Linux/Mac
```bash
# Run with uvicorn
uvicorn main:app --reload --host 0.0.0.0 --port 8000

# Or use Python
python main.py
```

## API Documentation

Once running, access interactive documentation at:
- **Swagger UI**: http://localhost:8000/docs
- **ReDoc**: http://localhost:8000/redoc

## API Endpoints

### Core Endpoints

#### 1. Get Files
```http
GET /api/files?pattern=dm_*_strut_dyn.csv&limit=50
```
Returns list of available CSV files with metadata.

#### 2. Analyze Files
```http
POST /api/analyze
{
  "file_pattern": "dm_*_strut_dyn.csv",
  "max_files": 20,
  "force_refresh": false
}
```
Performs tension analysis on selected files.

#### 3. Get Critical Case
```http
GET /api/critical
```
Returns the critical loading case (maximum tension).

#### 4. Get Time Series
```http
GET /api/timeseries/{fe_file}?strut_id=7&start_time=0&end_time=100
```
Returns time series data for specified FE file.

#### 5. Get Metadata Categories
```http
GET /api/metadata/categories
```
Returns available filter categories (LNG loading, tide levels, etc.).

#### 6. Browse with Filters
```http
POST /api/browse
{
  "lng_loading": ["15%"],
  "tide_levels": ["hwl"],
  "min_tension": 5000
}
```
Browse files with metadata filters.

#### 7. Get Statistics
```http
GET /api/stats
```
Returns overall statistics and cache status.

### WebSocket

#### Real-time Analysis Updates
```javascript
const ws = new WebSocket('ws://localhost:8000/ws/analysis');

ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  console.log('Update:', data);
};
```

## Testing

### Run Test Suite
```bash
# Test all endpoints
python test_api.py

# Test specific endpoint
python test_api.py --test analyze
```

### Using cURL

```bash
# Get files
curl http://localhost:8000/api/files

# Analyze
curl -X POST http://localhost:8000/api/analyze \
  -H "Content-Type: application/json" \
  -d '{"file_pattern": "*.csv"}'

# Get critical case
curl http://localhost:8000/api/critical
```

## Response Examples

### Analysis Response
```json
{
  "timestamp": "2025-08-13T12:00:00",
  "files_analyzed": 4,
  "struts_found": 8,
  "absolute_max": 8265.55,
  "absolute_min": -4541.68,
  "critical_case": {
    "value": 8265.55,
    "strut": "strut7",
    "fe_filename": "fsts_l015_hwl_ncl_240deg.sim",
    "metadata": {
      "lng_loading": "15% LNG",
      "tide_level": "HHWL",
      "environment_type": "Non-colinear",
      "direction": "240 degrees"
    }
  },
  "strut_analysis": [
    {
      "strut_id": "strut7",
      "min_tension": -4055.64,
      "max_tension": 8265.55,
      "range": 12321.19,
      "status": "CRITICAL"
    }
  ]
}
```

### Time Series Response
```json
{
  "fe_file": "fsts_l015_hwl_ncl_240deg.sim",
  "strut_id": "Strut7_Tension",
  "time_points": 100,
  "sample_rate": 0.1,
  "max_value": 8265.55,
  "min_value": 3617.47,
  "mean_value": 5500.68,
  "std_dev": 797.70,
  "max_time": 2451.0,
  "data": [
    {"time": 0.0, "value": 5556.59},
    {"time": 0.1, "value": 5557.23}
  ]
}
```

## Configuration

Edit `main.py` to configure:

```python
class Config:
    BASE_PATH = Path("D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr")
    CACHE_TTL = 300  # Cache timeout in seconds
    MAX_FILES = 100  # Maximum files to process
```

## Performance

- **Caching**: Results cached for 5 minutes by default
- **Parallel Processing**: Uses ThreadPoolExecutor for file loading
- **Async Operations**: Non-blocking I/O with FastAPI
- **Downsampling**: Time series data automatically downsampled

## Error Handling

All endpoints return appropriate HTTP status codes:
- `200`: Success
- `404`: Resource not found
- `422`: Validation error
- `500`: Internal server error

Error responses include details:
```json
{
  "detail": "No files found matching pattern"
}
```

## Deployment

### Production Setup
```bash
# Install production server
pip install gunicorn

# Run with multiple workers
gunicorn main:app -w 4 -k uvicorn.workers.UvicornWorker --bind 0.0.0.0:8000
```

### Docker
```dockerfile
FROM python:3.9-slim
WORKDIR /app
COPY requirements.txt .
RUN pip install -r requirements.txt
COPY . .
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
```

## Security

For production:
1. Configure CORS properly (not allow all origins)
2. Add authentication/authorization
3. Use HTTPS
4. Rate limiting
5. Input validation

## Next Steps

1. Add authentication with JWT
2. Implement Redis caching
3. Add GraphQL endpoint
4. Create OpenAPI client libraries
5. Add monitoring with Prometheus