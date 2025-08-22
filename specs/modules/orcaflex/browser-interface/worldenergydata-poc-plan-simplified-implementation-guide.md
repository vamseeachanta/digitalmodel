# WorldEnergyData Dashboard - Simplified Implementation Guide

## Overview
A lightweight, SME-focused energy data visualization dashboard using simple, proven technologies.

## Core Philosophy
- **Simple over Complex**: SQLite + CSV instead of enterprise databases
- **SME-First**: Visualizations designed for Subject Matter Experts
- **Iterative Development**: Build incrementally with user approval at each phase
- **Minimal Infrastructure**: No Docker, no complex deployments initially

## Technology Stack

### Data Layer
- **Storage**: SQLite (single file database)
- **Input Format**: CSV files
- **Processing**: Python pandas for data manipulation

### Backend
- **Framework**: FastAPI (lightweight, simple)
- **Data Processing**: pandas, numpy
- **File Handling**: Python standard library

### Frontend
- **Framework**: React (or even simpler: vanilla JavaScript + Chart.js)
- **Visualization**: Chart.js or D3.js
- **Styling**: Simple CSS (no complex frameworks initially)

## Implementation Phases

### Phase 1: Data Foundation (Week 1)
**Goal**: Establish data pipeline and storage

#### Tasks:
1. **CSV Reader Module**
   ```python
   # Simple CSV to SQLite pipeline
   - Read CSV files from designated folder
   - Validate data structure
   - Insert into SQLite tables
   ```

2. **SQLite Schema**
   ```sql
   -- Simple schema for energy data
   CREATE TABLE energy_data (
       id INTEGER PRIMARY KEY,
       timestamp TEXT,
       region TEXT,
       energy_type TEXT,
       value REAL,
       unit TEXT
   );
   ```

3. **Basic Data Validation**
   - Check for required columns
   - Validate data types
   - Handle missing values

**Deliverables**:
- Working CSV import script
- SQLite database with sample data
- Data validation report

**Approval Gate**: Demo CSV import and data storage

---

### Phase 2: Basic Visualization (Week 2)
**Goal**: Create simple web interface with core visualizations

#### Tasks:
1. **Simple Web Server**
   ```python
   # FastAPI endpoints
   GET /api/data - Retrieve filtered data
   GET /api/summary - Get data summaries
   POST /api/upload - Upload new CSV
   ```

2. **Basic UI**
   - Single HTML page
   - Dropdown filters (region, date range, energy type)
   - One chart type (line chart for time series)

3. **Core Visualizations**
   - Time series plot
   - Data table view
   - Export to CSV button

**Deliverables**:
- Working web interface
- Interactive time series chart
- Data export functionality

**Approval Gate**: Demo basic visualization capabilities

---

### Phase 3: Enhanced Features (Week 3)
**Goal**: Add requested features based on user feedback

#### Potential Enhancements:
1. **Additional Chart Types**
   - Bar charts for comparisons
   - Pie charts for distributions
   - Stacked area charts

2. **Data Features**
   - Multiple CSV file support
   - Data aggregation options
   - Simple calculations (averages, totals)

3. **UI Improvements**
   - Better filtering options
   - Chart customization
   - Print-friendly views

**Deliverables**: Based on user priorities from Phase 2 feedback

**Approval Gate**: Feature prioritization meeting

---

### Phase 4: Production Readiness (If Needed)
**Goal**: Prepare for wider deployment

#### Tasks:
1. **Performance Optimization**
   - Index SQLite tables
   - Implement data caching
   - Optimize queries

2. **User Features**
   - Save chart configurations
   - Scheduled data imports
   - Email reports

3. **Documentation**
   - User guide for SMEs
   - Data format specifications
   - Troubleshooting guide

**Deliverables**: Production-ready application

---

## File Structure
```
worldenergydata/
├── data/
│   ├── input/           # CSV files to import
│   ├── processed/       # Archived CSVs
│   └── database.db      # SQLite database
├── backend/
│   ├── main.py         # FastAPI application
│   ├── csv_reader.py   # CSV processing
│   ├── database.py     # SQLite operations
│   └── api.py          # API endpoints
├── frontend/
│   ├── index.html      # Main page
│   ├── app.js          # Simple JavaScript
│   └── style.css       # Basic styling
└── docs/
    ├── user_guide.md
    └── data_format.md
```

## Sample Implementation

### CSV Reader (Phase 1)
```python
import pandas as pd
import sqlite3
from pathlib import Path

def import_csv_to_sqlite(csv_path, db_path):
    """Simple CSV to SQLite import"""
    # Read CSV
    df = pd.read_csv(csv_path)
    
    # Connect to SQLite
    conn = sqlite3.connect(db_path)
    
    # Write to database
    df.to_sql('energy_data', conn, if_exists='append', index=False)
    
    conn.close()
    print(f"Imported {len(df)} rows from {csv_path}")
```

### Simple API (Phase 2)
```python
from fastapi import FastAPI
import sqlite3
import pandas as pd

app = FastAPI()

@app.get("/api/data")
def get_data(region: str = None, start_date: str = None):
    """Retrieve filtered energy data"""
    conn = sqlite3.connect("data/database.db")
    
    query = "SELECT * FROM energy_data WHERE 1=1"
    if region:
        query += f" AND region = '{region}'"
    if start_date:
        query += f" AND timestamp >= '{start_date}'"
    
    df = pd.read_sql(query, conn)
    conn.close()
    
    return df.to_dict(orient='records')
```

### Basic Visualization (Phase 2)
```html
<!DOCTYPE html>
<html>
<head>
    <title>Energy Data Dashboard</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
</head>
<body>
    <h1>World Energy Data - SME Dashboard</h1>
    
    <select id="regionFilter">
        <option value="">All Regions</option>
        <option value="North America">North America</option>
        <option value="Europe">Europe</option>
        <option value="Asia">Asia</option>
    </select>
    
    <canvas id="chart"></canvas>
    
    <script>
        // Simple chart implementation
        fetch('/api/data')
            .then(response => response.json())
            .then(data => {
                new Chart(document.getElementById('chart'), {
                    type: 'line',
                    data: {
                        labels: data.map(d => d.timestamp),
                        datasets: [{
                            label: 'Energy Production',
                            data: data.map(d => d.value)
                        }]
                    }
                });
            });
    </script>
</body>
</html>
```

## Success Criteria

### Phase 1
- ✅ CSV files successfully imported to SQLite
- ✅ Data validation catches common errors
- ✅ SME can understand data structure

### Phase 2
- ✅ Web interface loads without errors
- ✅ Charts display data correctly
- ✅ SME can filter and export data

### Phase 3
- ✅ User-requested features implemented
- ✅ Performance acceptable for typical datasets
- ✅ SMEs find interface intuitive

## Risk Mitigation

### Technical Risks
- **Large Data Files**: Implement chunked reading for large CSVs
- **Browser Performance**: Limit data points displayed in charts
- **Data Quality**: Implement validation rules and error reporting

### User Adoption
- **Training**: Provide simple video tutorials
- **Documentation**: Clear, SME-focused guides
- **Support**: Designated point of contact for issues

## Development Guidelines

### Code Principles
1. **Readability over Cleverness**: Write code that SMEs can understand
2. **Explicit over Implicit**: Clear variable names and comments
3. **Simple over Complex**: Avoid over-engineering

### Testing Approach
- Manual testing for Phase 1-2
- Automated tests only when complexity justifies
- User acceptance testing at each phase

### Documentation Standards
- Screenshots for every feature
- Step-by-step guides
- Common troubleshooting scenarios

## Deployment Strategy

### Local Development
```bash
# Simple setup
pip install pandas fastapi sqlite3
python backend/main.py
# Open browser to http://localhost:8000
```

### Initial Deployment
- Single server deployment
- Manual updates initially
- Backup strategy for SQLite database

### Future Scaling (Only if Needed)
- Consider PostgreSQL migration
- Add caching layer
- Implement load balancing

## Timeline Summary

| Phase | Duration | Key Deliverable | Approval Required |
|-------|----------|-----------------|-------------------|
| Phase 1 | Week 1 | CSV → SQLite pipeline | Yes - Data flow |
| Phase 2 | Week 2 | Basic web dashboard | Yes - UI/UX |
| Phase 3 | Week 3 | Enhanced features | Yes - Feature set |
| Phase 4 | TBD | Production ready | Yes - Deployment |

## Next Steps

1. **Immediate Actions**:
   - Get sample CSV files from stakeholders
   - Define initial data schema
   - Set up development environment

2. **Phase 1 Start**:
   - Create CSV reader script
   - Design SQLite schema
   - Import sample data

3. **Stakeholder Engagement**:
   - Schedule Phase 1 demo
   - Gather visualization requirements
   - Identify power users for testing

## Conclusion

This simplified approach prioritizes:
- **Quick wins** over perfect architecture
- **User feedback** over assumed requirements  
- **Iterative improvement** over big-bang delivery
- **SME usability** over technical sophistication

The goal is to deliver value quickly, learn from users, and enhance based on real needs rather than anticipated requirements.