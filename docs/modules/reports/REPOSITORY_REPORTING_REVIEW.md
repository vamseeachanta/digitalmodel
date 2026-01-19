# Repository Reporting Standards Review

**Date:** 2025-10-05
**Reviewer:** Claude Code Analysis Agent
**Scope:** HTML Report Generation with Interactive Charts

## Executive Summary

**Current Status:** ⚠️ **Partially Compliant** - Repository has good foundation but needs standardization

### Key Findings

✅ **Strengths:**
- Excellent Plotly template exists (`plotly_report_template.py`)
- CSV data sources well-organized in `/data` directory
- JSON data available for structured data
- Some HTML reports already generated

❌ **Issues Identified:**
- OCIMF charts using matplotlib static images (NON-COMPLIANT)
- Mixed use of static vs interactive charts
- Some reports embed PNG images instead of interactive plots
- Data paths not always relative
- No standardized CSV-to-chart pipeline

## Detailed Analysis

### 1. Report Generation Files Found

**Compliant (Plotly/Interactive):**
- ✅ `modules/reporting/templates/plotly_report_template.py` - Excellent template
- ✅ Template includes responsive design, summary stats, multiple plot types

**Non-Compliant (Matplotlib/Static):**
- ❌ `src/digitalmodel/modules/marine_analysis/visualization/ocimf_charts.py`
  - Uses matplotlib 3D plots, seaborn heatmaps
  - Saves static PNG files
  - HTML report embeds PNG images instead of interactive plots
  - **VIOLATION:** Does not comply with interactive-only requirement

### 2. Data Sources Available

**CSV Files:** ✅ Well-organized
```
/data/fatigue/*.csv
/data/marine_engineering/mooring_components/*.csv
/data/marine_engineering/hydrodynamic/*.csv (100+ files)
/data/ocimf/ocimf_coefficients_sample.csv
```

**JSON Files:** ✅ Available
```
/data/fatigue/*.json
/examples/workflows/data/*.json
```

### 3. HTML Reports Status

**Existing Reports:**
- `docs/charts/phase2/ocimf/ocimf_extraction_report.html` - ❌ Uses static PNG images
- `docs/hydro_coefficients_extraction_report.html`
- `docs/charts/phase2/validation/phase2_validation_dashboard.html`

**Issues:**
- Reports reference static image files (PNG)
- Not using interactive Plotly charts embedded in HTML
- Data loaded from absolute paths in some cases

## Compliance Assessment

### HTML_REPORTING_STANDARDS.md Compliance

| Requirement | Status | Notes |
|------------|--------|-------|
| Interactive plots only | ❌ Partial | ocimf_charts.py uses matplotlib |
| HTML reports required | ✅ Yes | Reports exist but need improvement |
| CSV data with relative paths | ⚠️ Partial | Some hardcoded paths found |
| Plotly/Bokeh/Altair/D3 | ⚠️ Partial | Good template exists, needs wider adoption |
| Responsive design | ✅ Yes | Plotly template has responsive CSS |
| Summary statistics | ✅ Yes | Template includes stat cards |
| Hover tooltips | ❌ No | Static images don't have tooltips |
| Zoom/pan functionality | ❌ No | Static images lack interactivity |

**Overall Compliance:** 50% ⚠️

## Priority Action Items

### High Priority (MUST FIX)

1. **Convert OCIMF Charts to Plotly**
   - File: `src/digitalmodel/modules/marine_analysis/visualization/ocimf_charts.py`
   - Convert all matplotlib plots to Plotly
   - Use 3D surface plots from Plotly (not matplotlib)
   - Embed interactive charts directly in HTML

2. **Standardize CSV Data Loading**
   - Create utility functions for relative path resolution
   - Ensure all data loading uses relative paths
   - Document standard data directory structure

3. **Update HTML Report Generation**
   - Use PlotlyReportGenerator template consistently
   - Remove PNG image embedding
   - Embed interactive Plotly charts directly

### Medium Priority

4. **Create Data Integration Utilities**
   - CSV loader with automatic path resolution
   - JSON data loader
   - Data validation utilities

5. **Enhance PlotlyReportGenerator**
   - Add 3D plot support
   - Add polar plot support
   - Add heatmap support

6. **Documentation**
   - Create examples for common chart types
   - Document CSV data format standards
   - Create migration guide for existing reports

## Recommended Implementation Plan

### Phase 1: Fix Non-Compliant Code (Priority 1)

**Task 1.1:** Convert `ocimf_charts.py` to use Plotly
```python
# Replace matplotlib 3D plots with Plotly 3D surface
import plotly.graph_objects as go
import plotly.express as px

# Replace seaborn heatmaps with Plotly heatmaps
# Replace polar plots with Plotly polar charts
```

**Task 1.2:** Update HTML report generation
```python
# Use PlotlyReportGenerator for all reports
from modules.reporting.templates.plotly_report_template import PlotlyReportGenerator
```

### Phase 2: Standardize Data Loading

**Task 2.1:** Create data utilities
```python
# modules/reporting/utils/data_loader.py
def load_csv_relative(filename, data_type='processed'):
    """Load CSV with automatic relative path resolution"""

def load_json_relative(filename, data_type='processed'):
    """Load JSON with automatic relative path resolution"""
```

### Phase 3: Enhance Templates

**Task 3.1:** Add missing plot types to PlotlyReportGenerator
- 3D surface plots
- Polar diagrams
- Vector field plots
- Custom subplot layouts

### Phase 4: Documentation & Examples

**Task 4.1:** Create example reports
**Task 4.2:** Document data format standards
**Task 4.3:** Migration guide for existing code

## Code Examples

### ✅ CORRECT: Interactive Plotly Report

```python
from modules.reporting.templates.plotly_report_template import PlotlyReportGenerator
import pandas as pd

# Load CSV with relative path
df = pd.read_csv('../data/ocimf/ocimf_coefficients_sample.csv')

# Create report
report = PlotlyReportGenerator(
    title='OCIMF Analysis Report',
    module_name='Marine Analysis',
    repository_name='digitalmodel'
)

# Add interactive charts
report.add_3d_surface(df, 'heading', 'displacement', 'CXw',
                      title='CXw Coefficient 3D Surface')
report.add_polar_plot(df, 'heading', 'CXw', title='CXw Polar Diagram')

# Generate HTML with embedded interactive charts
report.generate_html('../reports/ocimf_analysis.html')
```

### ❌ INCORRECT: Static Matplotlib Charts

```python
# This is NON-COMPLIANT - DO NOT USE
import matplotlib.pyplot as plt

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.plot_surface(X, Y, Z)
plt.savefig('static_chart.png')  # ❌ Static image

# Then embedding in HTML
html = '<img src="static_chart.png">'  # ❌ Not interactive
```

## Success Metrics

**Target State (100% Compliance):**
- ✅ All visualizations use Plotly/Bokeh/Altair/D3
- ✅ Zero static PNG/SVG image embeds in reports
- ✅ All data loaded via relative paths
- ✅ All reports have hover tooltips
- ✅ All reports have zoom/pan functionality
- ✅ All reports are responsive (mobile/desktop)
- ✅ All CSV data in standardized directories

**Current State:**
- 50% compliant
- 1 major violation (ocimf_charts.py)
- Several reports need updating

**Estimated Effort:**
- High priority fixes: 4-6 hours
- Medium priority: 2-3 hours
- Documentation: 1-2 hours
- **Total: 7-11 hours**

## Next Steps

1. ✅ **IMMEDIATE:** Create updated OCIMF charts using Plotly
2. ✅ **TODAY:** Implement data loading utilities
3. ✅ **THIS WEEK:** Enhance PlotlyReportGenerator template
4. ✅ **THIS WEEK:** Update all existing reports to use interactive charts

## Conclusion

Repository has excellent foundation with PlotlyReportGenerator template but needs:
1. Migration of matplotlib code to Plotly
2. Standardization of data loading patterns
3. Consistent use of interactive charts across all modules

**Recommendation:** Proceed with phased implementation focusing on high-priority items first.

---

**Status:** ⚠️ Action Required
**Next Review:** After Phase 1 completion
