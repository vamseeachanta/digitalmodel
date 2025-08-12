# OrcaFlex Data Browser - POC Dashboard

## Overview

A complete proof-of-concept HTML dashboard for browsing OrcaFlex mooring line tension data with intelligent filename parsing and interactive visualizations.

## Features Implemented

### ✅ Core Data Processing (NOW.1-NOW.3)
- **Project Structure**: Complete module in `src/modules/orcaflex-browser/`
- **CSV File Reader**: Simulated data generator with realistic mooring line tensions
- **Filename Parser**: Regex-based parsing for FST configurations, tide levels, and headings
  - FST patterns: `FST1_F`, `FST1_E`, `FST2_F`, `FST2_E`
  - Tide levels: `HWL`, `MWL`, `LWL`
  - Headings: `000deg`, `045deg`, `090deg`, `135deg`, `180deg`, `225deg`, `270deg`, `315deg`

### ✅ Interactive Dashboard (NOW.4-NOW.6)
- **Complete HTML Dashboard**: Single-file implementation with embedded CSS and JavaScript
- **Filter Dropdowns**: Interactive selectors for FST configuration, tide level, and environmental heading
- **Plotly Visualizations**: Interactive time series plots with:
  - 16 mooring lines with distinct colors
  - Individual line toggle controls
  - Zoom, pan, and hover functionality
  - Responsive design

### ✅ Analytics and Insights (NOW.7-NOW.8)
- **Statistical Summary Table**: Real-time calculations showing:
  - Min/Max tensions for each line
  - Mean values and standard deviation
  - Utilization percentages with color-coded status
  - Safety status indicators (Normal/Warning/Critical)
- **Automated Insights**: 4 key engineering conclusions:
  1. **Critical Line Analysis**: Identifies highest utilized lines
  2. **FST Flooding Impact**: Analyzes effect of flooded vs intact configurations
  3. **Environmental Heading Effects**: Directional loading analysis
  4. **Tide Level Impact**: Water level effects on mooring tensions

## Quick Start

1. **Open the Dashboard**:
   ```
   Open: src/modules/orcaflex-browser/orcaflex-data-browser.html
   ```

2. **Select Parameters**:
   - Choose FST Configuration (FST1_F, FST1_E, FST2_F, FST2_E)
   - Select Tide Level (HWL, MWL, LWL)
   - Pick Environment Heading (000deg to 315deg in 45° increments)

3. **Load Data**:
   - Click "Load Data" button
   - View real-time generated mooring line tensions
   - Explore interactive visualizations

4. **Analyze Results**:
   - Use line toggle controls to show/hide specific mooring lines
   - Review statistical summary for utilization analysis
   - Read automated insights for engineering conclusions

## Technical Implementation

### Data Generation
- Simulates 1000 time steps (100 seconds at 0.1s intervals)
- 16 mooring lines with realistic tension patterns
- Complex wave interactions with multiple frequency components
- Environmental heading effects using directional loading factors
- Tide level impacts on mean tension levels

### Filename Pattern Recognition
```javascript
// Regex patterns for automatic parsing
const patterns = {
    fst: /FST[12]_[FE]/,      // FST1_F, FST1_E, FST2_F, FST2_E
    tide: /[HML]WL/,          // HWL, MWL, LWL
    heading: /\d{3}deg/       // 000deg, 045deg, etc.
};
```

### Engineering Calculations
- **Utilization**: `(Max Tension / Design Limit) × 100%`
- **Directional Factor**: `cos(heading - line_angle)`
- **Tide Factor**: HWL = 1.2, MWL = 1.0, LWL = 0.8
- **FST Flooding Impact**: ~20% increase in tensions for flooded configurations

## Browser Compatibility

- ✅ Chrome/Chromium (recommended)
- ✅ Firefox
- ✅ Safari
- ✅ Edge
- 📱 Mobile responsive design

## Dependencies

All dependencies are loaded via CDN:
- **Plotly.js**: Interactive visualizations
- **Papa Parse**: CSV parsing (for future real CSV support)

## Future Enhancements

- Real CSV file upload and processing
- Multiple case comparison (side-by-side plots)
- Export functionality (PNG, PDF, CSV)
- Pattern learning from actual OrcaFlex filenames
- Integration with existing orcaflex-dashboard module

## File Structure

```
src/modules/orcaflex-browser/
├── orcaflex-data-browser.html    # Complete POC dashboard
└── README.md                     # This documentation
```

## Usage Notes

This is a **Proof of Concept** dashboard demonstrating:
1. Pattern-based filename parsing for OrcaFlex data
2. Interactive visualization of mooring line tensions
3. Automated engineering insight generation
4. Responsive web-based interface

The simulated data represents realistic mooring line tension patterns based on:
- FST floating storage configurations
- Environmental heading effects
- Tide level variations
- Dynamic wave loading

Perfect for stakeholder demonstrations and development planning!