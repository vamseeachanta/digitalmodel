# OrcaFlex Browser Interface - Development Steps Guide

## Table of Contents
1. [Project Overview](#project-overview)
2. [Development Journey](#development-journey)
3. [Key Implementation Decisions](#key-implementation-decisions)
4. [Critical Bug Fixes](#critical-bug-fixes)
5. [Architecture Patterns](#architecture-patterns)
6. [Step-by-Step Development Guide](#step-by-step-development-guide)
7. [Lessons Learned](#lessons-learned)
8. [Reusable Patterns](#reusable-patterns)

## Project Overview

### Goal
Build a web-based browser interface for OrcaFlex CSV output files to automate manual analysis process, finding and displaying the absolute maximum effective tension across all mooring struts.

### Critical Requirements
- Find absolute maximum tension: **8265.55 kN in Strut7**
- Parse metadata from filenames (LNG loading, tide levels, environment types, wave directions)
- Interactive visualizations with Plotly.js
- Professional PDF report generation
- Real-time data updates via WebSocket

## Development Journey

### Phase 1: Backend Core Implementation

#### 1.1 File Processing System
```python
# backend/file_processor.py
class FileProcessor:
    def __init__(self, base_path):
        self.base_path = Path(base_path)
    
    def read_multiple_csvs_parallel(self, file_paths, max_workers=4):
        """Parallel CSV reading for performance"""
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Critical fix: Use filename stem as key, not strut number
            futures = {executor.submit(self._read_csv, fp): fp for fp in file_paths}
```

**Key Learning**: Always use unique identifiers (filename stem) as dictionary keys when processing multiple files.

#### 1.2 OrcaFlex Analyzer
```python
# backend/orcaflex_analyzer.py
class OrcaFlexAnalyzer:
    def find_absolute_maximum(self, dataframes):
        # CRITICAL BUG FIX: Read ALL rows, not just first row
        for col in df.columns:
            if 'strut' in col.lower():
                max_val = float(df[col].max())  # Not df[col].iloc[0]!
```

**Critical Bug**: Initial implementation only checked first row (1653.77 kN) instead of all rows (8265.55 kN).

### Phase 2: API Development

#### 2.1 FastAPI Structure
```python
# api/main.py
app = FastAPI(
    title="OrcaFlex Browser API",
    docs_url="/docs",  # Swagger documentation
)

# Modular router pattern
app.include_router(timeseries_router)
app.include_router(report_router)
app.include_router(rich_pdf_router)
```

#### 2.2 Metadata Extraction Pattern
```python
# Pattern: dm_fsts_l015_hwl_ncl_240deg_strut_dyn.csv
metadata = {
    'lng_loading': extract_lng(filename),     # l015 -> 15% LNG
    'tide_level': extract_tide(filename),     # hwl -> HHWL
    'environment': extract_env(filename),     # ncl -> Non-colinear
    'direction': extract_direction(filename)  # 240deg -> 240°
}
```

### Phase 3: Frontend Visualization

#### 3.1 Technology Choice Decision
**Options Considered**:
1. React/Vue.js with separate frontend
2. **Plotly.js with embedded HTML (CHOSEN)**

**Why Plotly**: 
- Single-file deployment
- Rich interactive visualizations
- No build process required
- Direct integration with Python backend

#### 3.2 Critical UI Components

```javascript
// plotly_visualization.html
// Metadata synchronization for critical case
function updateMetadataFromCase(caseObj) {
    const components = caseObj.metadata.parsed_components;
    
    // Fix: Extract just l015/l095 from fsts_l015
    const lngValue = components.lng.replace('fsts_', '');
    
    // Fix: Populate directions BEFORE setting value
    updateEnvironmentDirections(components.env);
    document.getElementById('direction-selector').value = components.direction;
}
```

### Phase 4: PDF Generation Evolution

#### 4.1 Initial Attempt - Plotly Image Export
```python
# FAILED: Kaleido dependency issues
fig.write_image("chart.png")  # Hangs indefinitely
```

#### 4.2 Solution - Dual PDF Strategy
```python
# simple_pdf_endpoint.py - Fast, text-only (0.35s)
from reportlab.platypus import SimpleDocTemplate, Table

# rich_pdf_endpoint.py - With matplotlib charts (2.14s)
import matplotlib.pyplot as plt
matplotlib.use('Agg')  # Non-interactive backend

def create_time_series_chart():
    fig, ax = plt.subplots(figsize=(10, 6))
    # Generate charts without display
    plt.savefig(img_buffer, format='png', dpi=150)
```

## Critical Bug Fixes

### Bug 1: Wrong Maximum Value (1653.77 vs 8265.55)
**Problem**: Only reading first row of CSV
```python
# WRONG
max_tension = df[col].iloc[0]

# CORRECT
max_tension = float(df[col].max())
```

### Bug 2: Only 2 of 4 Dataframes Loading
**Problem**: Using strut number as dictionary key caused overwrites
```python
# WRONG
key = f"strut{strut_num}"  # Multiple files have same strut

# CORRECT
key = filepath.stem  # Unique filename without extension
```

### Bug 3: Metadata Selection Not Working
**Problem**: Mismatch between parsed values and UI values
```javascript
// WRONG
radio.checked = (radio.value === components.lng);  // 'l015' !== 'fsts_l015'

// CORRECT
const lngValue = components.lng.replace('fsts_', '');
radio.checked = (radio.value === lngValue);
```

### Bug 4: PDF Generation Hanging
**Problem**: Plotly's kaleido image export dependency issues
**Solution**: Create two endpoints - simple (ReportLab only) and rich (matplotlib charts)

## Architecture Patterns

### 1. Parallel Processing Pattern
```python
def process_files_parallel(files, max_workers=4):
    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = {executor.submit(process, f): f for f in files}
        results = {}
        for future in as_completed(futures):
            filepath = futures[future]
            results[filepath.stem] = future.result()
    return results
```

### 2. Caching Pattern
```python
cache = {
    "analysis_results": None,
    "last_updated": None,
    "file_list": None
}

if not force_refresh and cache["analysis_results"]:
    if (datetime.now() - cache["last_updated"]).seconds < CACHE_TTL:
        return cache["analysis_results"]
```

### 3. WebSocket Real-time Updates
```python
@app.websocket("/ws/analysis")
async def websocket_analysis(websocket: WebSocket):
    await websocket.accept()
    while True:
        await websocket.send_json({
            "type": "update",
            "data": get_latest_data()
        })
        await asyncio.sleep(5)
```

### 4. Modular Router Pattern
```python
# Each feature in separate file
router = APIRouter(prefix="/api/reports", tags=["Reports"])

@router.post("/generate/{case_id}")
async def generate_report(case_id: str):
    pass

# Main app includes all routers
app.include_router(router)
```

## Step-by-Step Development Guide

### Step 1: Setup Project Structure
```
browser-interface/
├── backend/
│   ├── file_processor.py       # CSV file handling
│   ├── orcaflex_analyzer.py    # Data analysis logic
│   └── metadata_extractor.py   # Filename parsing
├── api/
│   ├── main.py                 # FastAPI application
│   ├── timeseries_enhanced.py  # Time series endpoints
│   ├── simple_pdf_endpoint.py  # Fast PDF generation
│   ├── rich_pdf_endpoint.py    # PDF with charts
│   └── plotly_visualization.html # Frontend UI
└── requirements.txt
```

### Step 2: Implement Core Backend
1. Start with file reading functionality
2. Add metadata extraction from filenames
3. Implement analysis to find maximum tensions
4. **TEST THOROUGHLY** - Verify correct maximum value

### Step 3: Build REST API
1. Create FastAPI application
2. Add CORS middleware for browser access
3. Implement basic endpoints (/files, /analyze, /critical)
4. Add Swagger documentation (/docs)

### Step 4: Create Frontend Interface
1. Choose visualization library (Plotly recommended)
2. Build single HTML file with embedded JavaScript
3. Add metadata selection controls
4. Implement real-time plot updates

### Step 5: Add PDF Generation
1. Start with simple text-only PDF (ReportLab)
2. Add rich PDF with charts (matplotlib, not Plotly)
3. Provide user choice between fast/rich PDFs

### Step 6: Enhance with Advanced Features
1. WebSocket for real-time updates
2. Folder browsing capability
3. Wave period spectrum conversion
4. Comparative analysis tools

## Lessons Learned

### 1. Always Verify Data Accuracy
- User feedback: "you got to be less dumb than that"
- **Lesson**: Always check ALL rows in data, not just first row
- **Implementation**: Comprehensive testing with known values

### 2. Handle Browser Security Limitations
- **Problem**: Cannot directly browse local folders from browser
- **Solution**: Server-side folder API with modal dialog

### 3. Avoid Complex Dependencies
- **Problem**: Plotly's kaleido fails silently
- **Solution**: Use matplotlib for server-side chart generation

### 4. Metadata Synchronization is Critical
- **Problem**: UI controls not syncing with data
- **Solution**: Careful ordering of operations (populate options THEN set values)

### 5. Provide Performance Options
- **Implementation**: Dual PDF generation (fast 0.35s vs rich 2.14s)
- **User Experience**: Let users choose based on their needs

## Reusable Patterns

### 1. Filename Metadata Pattern
```python
def extract_metadata(filename):
    pattern = r'dm_(?P<prefix>\w+)_(?P<lng>l\d+)_(?P<tide>\w+)_(?P<env>\w+)_(?P<dir>\d+deg)'
    match = re.match(pattern, filename)
    return match.groupdict() if match else {}
```

### 2. Critical Value Finder Pattern
```python
def find_critical_value(dataframes):
    absolute_max = float('-inf')
    critical_info = {}
    
    for filename, df in dataframes.items():
        for col in df.columns:
            if 'relevant_column' in col.lower():
                max_val = float(df[col].max())
                if max_val > absolute_max:
                    absolute_max = max_val
                    critical_info = {
                        'value': max_val,
                        'source': filename,
                        'column': col,
                        'index': df[col].idxmax()
                    }
    return critical_info
```

### 3. Interactive Visualization Pattern
```javascript
function updatePlot(data) {
    const trace = {
        x: data.time,
        y: data.values,
        type: 'scatter',
        mode: 'lines',
        name: data.name
    };
    
    const layout = {
        title: data.title,
        xaxis: { title: 'Time (s)' },
        yaxis: { title: 'Value' },
        hovermode: 'closest'
    };
    
    Plotly.newPlot('plot-div', [trace], layout);
}
```

### 4. PDF Generation Pattern
```python
def generate_pdf_with_charts(data):
    # Use matplotlib for charts
    import matplotlib.pyplot as plt
    matplotlib.use('Agg')  # Non-interactive backend
    
    # Create charts
    fig, ax = plt.subplots()
    ax.plot(data['x'], data['y'])
    
    # Save to bytes
    img_buffer = io.BytesIO()
    plt.savefig(img_buffer, format='png', dpi=150)
    img_buffer.seek(0)
    
    # Add to PDF
    from reportlab.platypus import Image
    img = Image(img_buffer, width=6*inch, height=4*inch)
    story.append(img)
```

## Testing Checklist

- [ ] Verify correct maximum value (8265.55 kN)
- [ ] Test parallel file processing
- [ ] Confirm metadata extraction accuracy
- [ ] Check all UI controls synchronize properly
- [ ] Test PDF generation (both simple and rich)
- [ ] Verify WebSocket real-time updates
- [ ] Test folder browsing functionality
- [ ] Confirm wave spectrum calculations
- [ ] Load test with large datasets
- [ ] Cross-browser compatibility

## Performance Metrics

| Operation | Time | Notes |
|-----------|------|-------|
| Load 20 CSV files | ~2s | Parallel processing |
| Find critical case | ~0.5s | Cached after first run |
| Generate simple PDF | ~0.35s | Text only |
| Generate rich PDF | ~2.14s | With 4 charts |
| Update plots | ~0.1s | Client-side rendering |
| WebSocket update | 5s interval | Configurable |

## Deployment Considerations

1. **Configuration**: Use environment variables for paths
2. **Security**: Add authentication for production
3. **Scaling**: Consider Redis for distributed caching
4. **Monitoring**: Add logging and metrics collection
5. **Documentation**: Keep API docs updated via Swagger

## Future Enhancements

1. **Machine Learning**: Predict critical conditions
2. **3D Visualizations**: Add Three.js for spatial data
3. **Mobile Support**: Responsive design improvements
4. **Export Formats**: Add Excel, PowerBI integration
5. **Collaboration**: Multi-user real-time editing
6. **Automation**: Scheduled analysis and reports
7. **Cloud Storage**: S3/Azure blob integration
8. **Alert System**: Threshold-based notifications

---

*This guide represents the complete development journey from manual analysis to automated web-based system, capturing all critical decisions, bugs, fixes, and patterns for future reference.*