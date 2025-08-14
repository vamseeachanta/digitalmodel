# OrcaFlex Browser Interface Specification (Revised)

## Overview
A comprehensive web-based interface for browsing, analyzing, and visualizing OrcaFlex CSV output files with focus on structural analysis results, particularly effective tension in mooring strut components. The system identifies critical loading conditions and provides interactive visualizations with professional PDF reporting capabilities.

## Actual Implementation Architecture

### Technology Stack
- **Backend**: Python with FastAPI framework
- **Frontend**: Single HTML file with Plotly.js for visualizations
- **PDF Generation**: Dual approach - ReportLab (simple) and matplotlib (rich with charts)
- **Data Processing**: Pandas with ThreadPoolExecutor for parallel processing
- **Real-time Updates**: WebSocket connections for live data streaming

## Core Functionality (As Implemented)

### 1. File Navigation and Discovery

#### 1.1 Directory Management
- **Base Path Configuration**: `D:/1522/ctr7/orcaflex/rev_a08/output/csv/03c_100yr`
- **Dynamic Folder Switching**: Server-side folder browsing with modal UI
- **Pattern Recognition**: `dm_*_strut_dyn.csv` files for strut analysis
- **Parallel Loading**: ThreadPoolExecutor with 4 workers for concurrent CSV reading

#### 1.2 File Processing Implementation
```python
class FileProcessor:
    def read_multiple_csvs_parallel(self, file_paths, max_workers=4):
        # Uses filename stem as unique key (critical fix)
        # Returns dictionary: {filename_stem: dataframe}
```

### 2. Data Analysis Engine (Actual Implementation)

#### 2.1 Effective Tension Analysis
- **Critical Value Found**: 8265.55 kN in Strut7 at 240° direction
- **Analysis Method**: Processes ALL rows (not just first row - critical bug fix)
- **Strut Coverage**: Analyzes 8 struts simultaneously
- **Performance**: ~2 seconds for 20 CSV files

#### 2.2 Metadata Extraction Pattern
**Actual Parsing Implementation**:
```python
Pattern: dm_fsts_l015_hwl_ncl_240deg_strut_dyn.csv

Extracted Components:
- lng: "fsts_l015" → Display: "15% LNG"
- tide: "hwl" → Display: "HHWL"
- env: "ncl" → Display: "Non-colinear"
- direction: "240deg" → Display: "240°"
```

**Metadata Categories**:
- **LNG Loading**: 15% (l015) or 95% (l095)
- **Tide Levels**: HWL, LWL, MWL
- **Environment**: Colinear (cl) or Non-colinear (ncl)
- **Directions**: Every 15° for non-colinear, limited set for colinear

### 3. REST API Endpoints (As Built)

#### Core Endpoints
- `GET /` - API root with documentation links
- `GET /health` - Health check endpoint
- `GET /docs` - Swagger UI documentation

#### Data Endpoints
- `GET /api/files` - List available CSV files with metadata
- `POST /api/analyze` - Analyze files for tension data (cached)
- `GET /api/critical` - Get critical loading case
- `GET /api/stats` - Overall statistics

#### Time Series Endpoints
- `GET /api/timeseries/{fe_file}` - Single strut time series
- `GET /api/v2/timeseries/struts/{fe_file}/all` - All 8 struts data
- `GET /api/v2/timeseries/statistics/{fe_file}/{parameter}` - Statistical analysis
- `POST /api/v2/timeseries/export/{fe_file}` - Export to CSV

#### Configuration Endpoints
- `GET /api/config/folders` - List available data folders
- `POST /api/config/folder` - Change active folder

#### Report Generation
- `POST /api/simple-reports/generate/{case_id}` - Fast text-only PDF (~0.35s)
- `POST /api/rich-reports/generate/{case_id}` - PDF with matplotlib charts (~2.14s)

#### WebSocket
- `WS /ws/analysis` - Real-time analysis updates (5-second intervals)

### 4. Frontend Implementation (plotly_visualization.html)

#### 4.1 UI Components
- **Header**: "Woodfibre LNG - Mooring Analysis"
- **Metadata Controls**:
  - LNG Loading: Radio buttons (15% / 95%)
  - Tide Level: Dropdown (HWL / LWL / MWL)
  - Environment: Radio buttons (Colinear / Non-colinear)
  - Wave Direction: Dynamic dropdown (filtered by environment)
- **Case Selector**: Dropdown with critical case auto-selected
- **Action Buttons**:
  - Load Case Data
  - Compare with Critical
  - Export Plots
  - Generate PDF Report (with choice dialog)

#### 4.2 Visualization Panels (6 Interactive Plots)
1. **Time Series Plot**: All 8 struts over simulation time
2. **Tension Heatmap**: 2D visualization of tension distribution
3. **Distribution Plot**: Histogram of tension values
4. **Comparison Plot**: Bar chart comparing struts
5. **Polar Plot**: Directional response visualization
6. **Wave Period Spectrum**: Converted from frequency (ocean engineering standard)

#### 4.3 Critical Features Implemented
- **Auto-load Critical Case**: Loads 8265.55 kN case on startup
- **Metadata Synchronization**: All controls update when case changes
- **Dynamic Direction Filtering**: Directions update based on environment selection
- **Pulsing Critical Badge**: Visual indicator for critical case
- **Folder Browser Modal**: Server-side browsing (avoids browser security)

### 5. PDF Generation System

#### 5.1 Simple PDF Generator
- **Technology**: ReportLab only
- **Performance**: ~0.35 seconds
- **Size**: ~4.6 KB
- **Content**: Tables, text, executive summary

#### 5.2 Rich PDF Generator
- **Technology**: ReportLab + matplotlib
- **Performance**: ~2.14 seconds
- **Size**: ~1.2 MB
- **Charts Included**:
  - Time series for all struts
  - Max/min tension bar chart
  - Polar directional response
  - Tension distribution heatmap

### 6. Critical Bug Fixes Applied

#### Bug 1: Incorrect Maximum Value
- **Issue**: Only reading first row (found 1653.77 instead of 8265.55)
- **Fix**: Use `df[col].max()` instead of `df[col].iloc[0]`

#### Bug 2: Dataframe Overwriting
- **Issue**: Using strut number as key caused overwrites
- **Fix**: Use unique filename stem as dictionary key

#### Bug 3: Metadata Mismatch
- **Issue**: "fsts_l015" not matching "l015" in UI
- **Fix**: Strip "fsts_" prefix for comparison

#### Bug 4: PDF Generation Hanging
- **Issue**: Plotly's kaleido dependency failures
- **Fix**: Dual approach with matplotlib for charts

### 7. Performance Metrics

| Operation | Time | Details |
|-----------|------|---------|
| Load 20 CSV files | ~2s | Parallel with 4 workers |
| Find critical case | ~0.5s | Cached after first run |
| Generate simple PDF | ~0.35s | Text only |
| Generate rich PDF | ~2.14s | With 4 matplotlib charts |
| Update plots | ~0.1s | Client-side Plotly rendering |
| WebSocket update | 5s | Configurable interval |
| Folder change | ~1s | Includes file re-scan |

### 8. Caching Strategy

```python
cache = {
    "analysis_results": None,  # Full analysis results
    "last_updated": None,       # Timestamp
    "file_list": None,          # Cached file listing
    "TTL": 300                  # 5 minutes
}
```

### 9. Data Flow

1. **Initial Load**:
   - Load cases from current folder
   - Find critical case (8265.55 kN)
   - Auto-select and load critical case
   - Synchronize metadata controls

2. **User Interaction**:
   - Select metadata filters
   - System finds matching cases
   - Load selected case data
   - Update all 6 visualizations

3. **PDF Generation**:
   - User chooses simple or rich
   - Backend generates PDF
   - Frontend downloads file

### 10. Configuration Files

#### requirements.txt
```
fastapi==0.104.1
uvicorn==0.24.0
pandas==2.1.3
numpy==1.24.3
plotly==5.18.0
reportlab==4.0.7
matplotlib==3.8.2
python-multipart==0.0.6
websockets==12.0
```

## Deployment Considerations

### Production Requirements
1. **Authentication**: Add JWT/OAuth2 for security
2. **HTTPS**: SSL certificates for production
3. **Scaling**: Consider Gunicorn with multiple workers
4. **Monitoring**: Add logging and metrics (Prometheus/Grafana)
5. **Database**: Consider PostgreSQL for analysis history

### Environment Variables
```bash
ORCAFLEX_BASE_PATH=/data/orcaflex/output
API_HOST=0.0.0.0
API_PORT=8001
CACHE_TTL=300
MAX_WORKERS=4
```

## Testing Coverage

### Unit Tests Required
- File processor CSV reading
- Metadata extraction accuracy
- Maximum value calculation
- PDF generation functionality

### Integration Tests
- API endpoint responses
- WebSocket connections
- Folder browsing operations
- Critical case identification

### Performance Tests
- Large dataset handling (100+ files)
- Concurrent user access
- Memory usage under load
- PDF generation stress test

## Future Enhancements (Not Yet Implemented)

1. **Database Integration**: Store analysis history
2. **User Management**: Multi-user support with preferences
3. **Batch Operations**: Process multiple folders
4. **Alert System**: Threshold-based notifications
5. **3D Visualization**: Three.js for spatial data
6. **Machine Learning**: Predict critical conditions
7. **Cloud Storage**: S3/Azure blob support
8. **Mobile App**: React Native companion app

## API Documentation

Full API documentation available at:
- Swagger UI: `http://localhost:8001/docs`
- ReDoc: `http://localhost:8001/redoc`

## Conclusion

This revised specification reflects the actual implementation of the OrcaFlex Browser Interface, including all features built, bugs fixed, and patterns established during development. The system successfully identifies the critical loading condition (8265.55 kN in Strut7) and provides comprehensive analysis tools with professional reporting capabilities.