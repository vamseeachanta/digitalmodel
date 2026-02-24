# RAO Quality Assurance Reports

## Overview

Interactive HTML reports for Quality Assurance of Response Amplitude Operator (RAO) data, following **HTML_REPORTING_STANDARDS.md** compliance.

## Generated Reports

### Vessel Heave RAO QA Report
- **File:** `vessel_heave_rao_qa_report.html`
- **Generated:** 2025-10-05
- **Data Source:** `docs/modules/orcaflex/mooring/semi/rao-check/Vessel Heave_RAoStudy.csv`
- **Size:** 139 KB
- **Charts:** 5 interactive Plotly visualizations

## Report Features

✅ **Interactive Plotly Charts** (MANDATORY per HTML_REPORTING_STANDARDS.md):
1. **RAO vs Period** - All wave headings with interactive hover tooltips
2. **RAO vs Frequency** - Frequency domain response
3. **Polar Diagram** - Directional response for median period
4. **Heatmap** - RAO amplitude across period and heading
5. **Statistical Distribution** - Box plots by wave heading

✅ **Data Quality Metrics:**
- Total data points: 1,503
- Wave periods: 501 unique periods
- Period range: 250.0s to 300.0s
- Wave headings: 3 directions
- RAO amplitude range: -14.705 to 8.041 m/m
- Data completeness: 100%
- Outlier detection with statistical analysis

✅ **QA Status Dashboard:**
- Data completeness check
- Outliers detection
- Data points validation
- Wave headings verification

## Technology Stack

- **Visualization:** Plotly 2.26.0 (interactive JavaScript charts)
- **Data Processing:** pandas, numpy
- **Report Format:** HTML5 with embedded JSON data
- **Responsive Design:** Mobile and desktop compatible

## Usage

### View Report
Simply open the HTML file in any modern web browser:
```bash
# Windows
start docs/reports/rao_qa/vessel_heave_rao_qa_report.html

# Mac/Linux
open docs/reports/rao_qa/vessel_heave_rao_qa_report.html
```

### Generate New Reports
```bash
python scripts/generate_rao_qa_report.py
```

### Customize Report Generation
```python
from scripts.generate_rao_qa_report import RAOQAReportGenerator

# Initialize generator
generator = RAOQAReportGenerator(output_dir="docs/reports/rao_qa")

# Load RAO data
df = generator.load_rao_data("path/to/rao_data.csv")

# Generate interactive HTML report
report_file = generator.generate_html_report(df, report_name="custom_rao_report")
```

## Interactive Features

All charts support:
- **Hover tooltips** - Detailed data values on mouse hover
- **Zoom and pan** - Click and drag to zoom, double-click to reset
- **Legend toggling** - Click legend items to show/hide data series
- **Export options** - Download plots as PNG/SVG
- **Responsive layout** - Automatically adjusts to screen size

## Data Format

RAO CSV files must follow this structure:
- **Row 1:** Period values (T in seconds)
- **Row 2+:** RAO amplitude values for different wave headings

Example:
```csv
250.0,250.1,250.2,...,300.0
3.179,2.799,2.408,...,2.146
1.894,1.532,1.163,...,2.126
2.146,1.827,1.501,...,1.987
```

## Compliance

This report generator follows the **HTML_REPORTING_STANDARDS.md** requirements:

✅ **Interactive plots only** (no static matplotlib PNG/SVG)
✅ **HTML reports required** for all modules
✅ **CSV data with relative paths**
✅ **Responsive design** (mobile + desktop)
✅ **Professional styling** with modern CSS
✅ **Plotly.js CDN** for offline-capable reports

## Future Enhancements

- [ ] Multi-file comparison reports
- [ ] Export to PDF functionality
- [ ] Automated report scheduling
- [ ] Email notification on anomaly detection
- [ ] Integration with CI/CD pipeline
- [ ] Custom threshold configuration
- [ ] Historical trend analysis

## Support

For issues or feature requests, contact the Digital Model Team.

---

**Generated with Claude Code** | Digital Model Engineering
**Standards:** HTML_REPORTING_STANDARDS.md v1.0.0
