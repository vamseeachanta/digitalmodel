# Time Trace Display Specification

## Overview
Once an FE filename is selected (e.g., `fsts_l015_hwl_ncl_240deg.sim`), the system will display time series data in organized tabs for detailed analysis.

## Data Structure Expected

### From FE Simulation Files (.sim)
The selected FE file contains time series data for:
1. **Strut Forces**: Time history of forces/tensions in each strut
2. **Jacket Response**: Structural response data (displacements, accelerations, loads)
3. **Environmental Data**: Wave, wind, current time histories
4. **Vessel Motion**: 6DOF motion time series

## Tab Organization

### Tab 1: Strut Forces
```
[STRUT FORCES] - fsts_l015_hwl_ncl_240deg.sim
================================================
Time Range: 0 - 10800 sec (3 hours)
Sample Rate: 0.1 sec

Strut 7 - CRITICAL (Max: 8265.55 at t=2450.3s)
------------------------------------------------
Time(s)  Tension(kN)  |  Visual
0.0      1234.5       |  ████████░░░░░░░░░░░
0.1      1245.2       |  ████████░░░░░░░░░░░
...
2450.3   8265.55      |  ████████████████████ MAX
...

Summary Statistics:
- Mean: 3456.7 kN
- Std Dev: 1234.5 kN
- Max: 8265.55 kN at t=2450.3s
- Min: -1234.5 kN at t=567.8s
- Cycles above 5000kN: 45
```

### Tab 2: Jacket Data
```
[JACKET DATA] - fsts_l015_hwl_ncl_240deg.sim
================================================
Time Range: 0 - 10800 sec
Sample Rate: 0.1 sec

Node: TOP_DECK_CENTER
------------------------------------------------
Parameter    Max        Min        Mean     Units
Disp_X       2.34      -2.15      0.12     m
Disp_Y       1.87      -1.92      0.08     m
Disp_Z       0.45      -0.38      0.02     m
Accel_X      0.89      -0.92      0.01     m/s²
Accel_Y      0.76      -0.78      0.00     m/s²
```

## CLI Dashboard Implementation

### Enhanced Dashboard with Tabs
```python
class TimeTraceDisplay:
    """Display time traces from FE simulation files"""
    
    def __init__(self, fe_filename: str):
        self.fe_filename = fe_filename
        self.fe_path = self.find_fe_file(fe_filename)
        self.time_data = {}
        self.current_tab = "strut_forces"
        
    def load_fe_data(self):
        """Load time series data from FE file"""
        # Could be .sim file or associated CSV exports
        pass
        
    def display_strut_forces_tab(self):
        """Display strut forces time traces"""
        pass
        
    def display_jacket_data_tab(self):
        """Display jacket structural response"""
        pass
        
    def plot_ascii_chart(self, time, values, width=60):
        """Create ASCII plot for terminal display"""
        pass
```

### Navigation Controls
```
================================================================================
FE File: fsts_l015_hwl_ncl_240deg.sim
--------------------------------------------------------------------------------
[1] Overview  [2] Strut Forces*  [3] Jacket Data  [4] Environment  [5] Export

Press number to switch tabs, or Q to return to main dashboard
================================================================================
```

## Data Sources

### Option 1: Direct from .sim files
- Parse OrcaFlex .sim binary format
- Extract time series data directly

### Option 2: From exported CSV time series
- Look for associated CSV files with time data
- Pattern: `{fe_filename_stem}_timeseries.csv`
- More practical for implementation

### Option 3: From database
- Pre-processed time series stored in database
- Faster access for large datasets

## Display Features

### For CLI Dashboard
1. **ASCII Plots**: Simple time series visualization in terminal
2. **Statistics Panel**: Key metrics for selected time range
3. **Peak Detection**: Highlight maximum/minimum events
4. **Zoom Controls**: Focus on specific time ranges
5. **Export Options**: Save selected data to CSV

### For Web Dashboard (Future)
1. **Interactive Charts**: Plotly/D3.js time series plots
2. **Multi-trace Overlay**: Compare multiple struts
3. **Synchronized Cursors**: Link time across all tabs
4. **FFT Analysis**: Frequency domain views
5. **Animation Playback**: Time-stepped visualization

## Implementation Steps

### Phase 1: Basic Display
1. Detect when FE file is selected
2. Check for available time series data
3. Display basic statistics
4. Show simple ASCII plot

### Phase 2: Full Tab System
1. Implement tab navigation
2. Load full time series data
3. Add zoom/pan controls
4. Enable data export

### Phase 3: Advanced Analysis
1. Peak detection algorithms
2. Fatigue cycle counting
3. Statistical distributions
4. Correlation analysis

## File Patterns to Search

When FE file `fsts_l015_hwl_ncl_240deg.sim` is selected, search for:

1. **Time Series CSVs**:
   - `fsts_l015_hwl_ncl_240deg_strut_timeseries.csv`
   - `fsts_l015_hwl_ncl_240deg_jacket_timeseries.csv`
   - `fsts_l015_hwl_ncl_240deg_environment.csv`

2. **OrcaFlex Outputs**:
   - `fsts_l015_hwl_ncl_240deg.sim` (binary simulation)
   - `fsts_l015_hwl_ncl_240deg.txt` (text report)
   - `fsts_l015_hwl_ncl_240deg_results/` (results folder)

## Sample Data Structure

### Strut Forces Time Series CSV
```csv
Time,Strut1_Tension,Strut2_Tension,...,Strut8_Tension
0.0,1234.5,2345.6,...,3456.7
0.1,1235.1,2346.2,...,3457.3
...
```

### Jacket Response Time Series CSV
```csv
Time,Node,Disp_X,Disp_Y,Disp_Z,Accel_X,Accel_Y,Accel_Z
0.0,TOP_DECK,0.12,0.08,0.02,0.01,0.00,0.00
0.1,TOP_DECK,0.13,0.09,0.02,0.02,0.01,0.00
...
```

## User Workflow

1. **Select Critical Case**: Dashboard shows max tension
2. **Press Enter on FE File**: Opens time trace view
3. **Navigate Tabs**: Use number keys to switch views
4. **Analyze Time Series**: View when maximum occurred
5. **Export Data**: Save specific time ranges
6. **Return to Dashboard**: Press Q to go back

## Benefits

1. **Complete Context**: See not just max value but when/how it occurred
2. **Multi-Parameter View**: Correlate strut forces with jacket response
3. **Engineering Insight**: Understand dynamic behavior
4. **Quality Check**: Verify simulation ran correctly
5. **Report Generation**: Export key time windows for documentation