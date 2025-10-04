# HTML Reporting Standards

> **Version:** 1.0.0
> **Last Updated:** 2025-10-03
> **Scope:** All 26 repositories in workspace-hub

## Overview

All repositories and associated modules in workspace-hub **MUST** generate HTML reports with interactive visualizations. This standard ensures consistent, professional, and accessible data presentation across all projects.

## Core Requirements

### 1. **Interactive Plots Only**

**MANDATORY:** All plots in HTML reports must be interactive.

- ✅ **Allowed:** Plotly, Bokeh, Altair, D3.js
- ❌ **Not Allowed:** Static matplotlib PNG/SVG exports, seaborn static images

**Interactive features required:**
- Hover tooltips showing data values
- Zoom and pan capabilities
- Legend toggling
- Export options (PNG, SVG)
- Responsive design for different screen sizes

### 2. **HTML Reports Required**

**MANDATORY:** Every module must generate HTML reports.

**Report Types:**
- Analysis reports with visualizations
- Performance dashboards
- Data quality reports
- Test coverage reports with charts
- Model evaluation reports
- Monitoring dashboards

### 3. **CSV Data Import**

**MANDATORY:** Data must be imported directly from CSV files using relative paths.

**Requirements:**
- Use relative paths from report location
- No hardcoded absolute paths
- CSV files stored in standardized locations
- Automatic path resolution

**Standard Structure:**
```
module/
├── reports/
│   ├── index.html              # Main report
│   ├── analysis_report.html    # Analysis with plots
│   └── dashboard.html          # Interactive dashboard
├── data/
│   ├── raw/                    # Raw CSV data
│   ├── processed/              # Processed CSV data
│   └── results/                # Analysis results CSV
└── src/
    └── reporting/
        ├── generate_report.py  # Report generation
        └── plot_utils.py       # Plotting utilities
```

## Technology Standards

### Primary: Plotly (Python)

**Recommended for:** General-purpose interactive plotting

**Why Plotly:**
- Rich interactive features out of the box
- Easy CSV integration with pandas
- Professional appearance
- Good documentation
- Wide browser compatibility

**Installation:**
```bash
pip install plotly pandas
# or
uv pip install plotly pandas
```

**Example:**
```python
import plotly.express as px
import pandas as pd

# Import CSV with relative path
df = pd.read_csv('../data/processed/results.csv')

# Create interactive plot
fig = px.scatter(df, x='time', y='value',
                 color='category',
                 title='Interactive Analysis Results',
                 hover_data=['additional_info'])

# Save as HTML
fig.write_html('../reports/analysis_plot.html')
```

### Alternative: Bokeh (Python)

**Recommended for:** Complex dashboards, real-time updates

**Why Bokeh:**
- Excellent for interactive dashboards
- Server-side capabilities for dynamic data
- Good for large datasets
- Streaming data support

**Installation:**
```bash
pip install bokeh pandas
# or
uv pip install bokeh pandas
```

**Example:**
```python
from bokeh.plotting import figure, output_file, save
from bokeh.models import HoverTool
import pandas as pd

# Import CSV with relative path
df = pd.read_csv('../data/processed/results.csv')

# Create plot
p = figure(title='Interactive Dashboard')
p.circle(df['x'], df['y'], size=10, alpha=0.5)

# Add interactivity
hover = HoverTool(tooltips=[('Value', '@y')])
p.add_tools(hover)

# Save as HTML
output_file('../reports/dashboard.html')
save(p)
```

### Alternative: Altair (Python)

**Recommended for:** Declarative, grammar-of-graphics style plotting

**Why Altair:**
- Clean, declarative syntax
- Based on Vega-Lite
- Excellent for exploratory analysis
- Good statistical visualizations

**Installation:**
```bash
pip install altair pandas
# or
uv pip install altair pandas
```

**Example:**
```python
import altair as alt
import pandas as pd

# Import CSV with relative path
df = pd.read_csv('../data/processed/results.csv')

# Create interactive plot
chart = alt.Chart(df).mark_point().encode(
    x='time:T',
    y='value:Q',
    color='category:N',
    tooltip=['time', 'value', 'category']
).interactive()

# Save as HTML
chart.save('../reports/analysis.html')
```

### JavaScript: D3.js

**Recommended for:** Custom, highly interactive visualizations

**Why D3.js:**
- Maximum flexibility
- Beautiful custom visualizations
- Excellent for complex interactions
- Industry standard

**Installation:**
```html
<!-- Include in HTML -->
<script src="https://d3js.org/d3.v7.min.js"></script>
```

**Example:**
```javascript
// Load CSV with relative path
d3.csv('../data/processed/results.csv').then(data => {
    // Create interactive visualization
    const svg = d3.select('#plot')
        .append('svg')
        .attr('width', 800)
        .attr('height', 600);

    // Plot with interactions
    svg.selectAll('circle')
        .data(data)
        .enter()
        .append('circle')
        .attr('cx', d => xScale(d.time))
        .attr('cy', d => yScale(d.value))
        .on('mouseover', showTooltip)
        .on('mouseout', hideTooltip);
});
```

## Technology Selection Guide

| Use Case | Recommended Technology | Why |
|----------|----------------------|-----|
| **General Analysis Reports** | Plotly | Easy, feature-rich, pandas integration |
| **Statistical Visualizations** | Altair | Grammar of graphics, declarative |
| **Real-time Dashboards** | Bokeh | Server capabilities, streaming |
| **Custom Interactive Viz** | D3.js | Maximum flexibility |
| **Time Series Analysis** | Plotly | Excellent time series support |
| **Geospatial Data** | Plotly (Mapbox) | Built-in map support |
| **Large Datasets (>100k points)** | Bokeh or Plotly (webGL) | Performance optimizations |
| **Scientific/Engineering** | Plotly | 3D plots, contours, heatmaps |

## CSV Data Handling Standards

### 1. **Relative Path Convention**

**Always use relative paths from the report location:**

```python
# ✅ CORRECT: Relative path from report
df = pd.read_csv('../data/processed/results.csv')

# ❌ WRONG: Absolute path
df = pd.read_csv('/mnt/github/workspace-hub/repo/data/results.csv')

# ❌ WRONG: Hardcoded path
df = pd.read_csv('C:/Users/user/data/results.csv')
```

### 2. **Path Resolution Utility**

**Create reusable path resolver:**

```python
# src/reporting/path_utils.py
from pathlib import Path

def get_data_path(filename, data_type='processed'):
    """
    Get data file path relative to project root.

    Args:
        filename: CSV filename
        data_type: 'raw', 'processed', or 'results'

    Returns:
        Path object to data file
    """
    # Get project root (assuming standard structure)
    project_root = Path(__file__).parent.parent.parent
    return project_root / 'data' / data_type / filename

# Usage in report generation
import pandas as pd
from reporting.path_utils import get_data_path

df = pd.read_csv(get_data_path('analysis.csv'))
```

### 3. **CSV File Standards**

**Required CSV structure:**
- First row: Column headers (clear, descriptive names)
- Consistent delimiter (prefer comma)
- UTF-8 encoding
- ISO 8601 dates (YYYY-MM-DD or YYYY-MM-DD HH:MM:SS)
- No empty rows/columns
- Missing values: empty string or 'NA' (consistent)

**Example:**
```csv
timestamp,parameter,value,unit,status
2025-10-03 10:00:00,temperature,25.3,celsius,ok
2025-10-03 10:05:00,temperature,25.8,celsius,ok
2025-10-03 10:10:00,temperature,26.1,celsius,warning
```

## HTML Report Standards

### 1. **Report Structure**

**Standard HTML report template:**

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Analysis Report - [Module Name]</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background-color: #f5f5f5;
        }
        .report-header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 30px;
        }
        .plot-container {
            background: white;
            padding: 20px;
            border-radius: 10px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .summary-stats {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 15px;
            margin-bottom: 30px;
        }
        .stat-card {
            background: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .stat-value {
            font-size: 2em;
            font-weight: bold;
            color: #667eea;
        }
    </style>
</head>
<body>
    <div class="report-header">
        <h1>Analysis Report</h1>
        <p>Generated: <span id="timestamp"></span></p>
        <p>Module: [Module Name]</p>
        <p>Repository: [Repository Name]</p>
    </div>

    <div class="summary-stats">
        <div class="stat-card">
            <div class="stat-label">Total Records</div>
            <div class="stat-value">10,234</div>
        </div>
        <!-- More stat cards -->
    </div>

    <div class="plot-container">
        <h2>Interactive Analysis</h2>
        <div id="plot1"></div>
    </div>

    <div class="plot-container">
        <h2>Trend Analysis</h2>
        <div id="plot2"></div>
    </div>

    <script>
        // Set timestamp
        document.getElementById('timestamp').textContent =
            new Date().toLocaleString();

        // Load and render plots (Plotly, Bokeh, D3.js, etc.)
    </script>
</body>
</html>
```

### 2. **Report Sections Required**

Every HTML report must include:

1. **Header Section:**
   - Report title
   - Generation timestamp
   - Module/repository name
   - Author/generator info

2. **Summary Statistics:**
   - Key metrics at a glance
   - Data quality indicators
   - Record counts

3. **Interactive Visualizations:**
   - Minimum 2-3 plots per report
   - Each plot must be interactive
   - Clear titles and labels

4. **Data Table (optional):**
   - Interactive table with sorting/filtering
   - Show sample of data
   - Link to full CSV

5. **Footer:**
   - Data source information
   - Generation method
   - Contact/support info

### 3. **Responsive Design**

**Reports must work on all devices:**
- Desktop (1920px+)
- Laptop (1366px)
- Tablet (768px)
- Mobile (375px)

**Use responsive containers:**
```python
# Plotly responsive
fig.update_layout(
    autosize=True,
    margin=dict(l=20, r=20, t=40, b=20),
)

# Bokeh responsive
from bokeh.models import Div
p.sizing_mode = 'stretch_width'
```

## Agent Assignments

### Visualization Agent

**Added to agent registry:**
- **Agent:** `plotly-visualization-agent`
- **Platform:** Python/Plotly
- **Capabilities:**
  - Interactive plot generation (95)
  - CSV data handling (93)
  - HTML report creation (91)
  - Responsive design (88)
- **Best For:** Creating interactive HTML reports with Plotly

### Dashboard Agent

**Added to agent registry:**
- **Agent:** `bokeh-dashboard-agent`
- **Platform:** Python/Bokeh
- **Capabilities:**
  - Dashboard creation (94)
  - Real-time updates (90)
  - Complex layouts (92)
  - Large dataset handling (91)
- **Best For:** Complex interactive dashboards

### Data Analysis Reporter

**Added to agent registry:**
- **Agent:** `altair-analysis-agent`
- **Platform:** Python/Altair
- **Capabilities:**
  - Statistical visualizations (93)
  - Exploratory analysis (92)
  - Grammar of graphics (94)
  - Clean code generation (90)
- **Best For:** Statistical analysis reports

## Implementation Examples

### Example 1: Simple Analysis Report

```python
# generate_report.py
import plotly.express as px
import pandas as pd
from pathlib import Path

def generate_analysis_report(csv_file, output_file):
    """Generate interactive HTML report from CSV data."""

    # Import CSV with relative path
    df = pd.read_csv(csv_file)

    # Create interactive plot
    fig = px.line(df, x='timestamp', y='value',
                  color='parameter',
                  title='Parameter Trends Over Time',
                  labels={'value': 'Measurement Value',
                          'timestamp': 'Time'},
                  hover_data=['unit', 'status'])

    # Customize layout for professional appearance
    fig.update_layout(
        template='plotly_white',
        hovermode='x unified',
        height=600,
        font=dict(size=12),
    )

    # Save as standalone HTML
    fig.write_html(output_file,
                   include_plotlyjs='cdn',
                   config={'responsive': True})

    print(f"Report generated: {output_file}")

# Usage
if __name__ == '__main__':
    generate_analysis_report(
        csv_file='../data/processed/measurements.csv',
        output_file='../reports/analysis_report.html'
    )
```

### Example 2: Multi-Plot Dashboard

```python
# generate_dashboard.py
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import pandas as pd

def generate_dashboard(csv_file, output_file):
    """Generate multi-plot interactive dashboard."""

    # Import CSV
    df = pd.read_csv(csv_file)

    # Create subplots
    fig = make_subplots(
        rows=2, cols=2,
        subplot_titles=('Time Series', 'Distribution',
                       'Correlation', 'Summary'),
        specs=[[{'type': 'scatter'}, {'type': 'histogram'}],
               [{'type': 'scatter'}, {'type': 'table'}]]
    )

    # Add plots
    fig.add_trace(
        go.Scatter(x=df['time'], y=df['value'], mode='lines+markers',
                   name='Trend', hovertemplate='%{y:.2f}'),
        row=1, col=1
    )

    fig.add_trace(
        go.Histogram(x=df['value'], name='Distribution'),
        row=1, col=2
    )

    fig.add_trace(
        go.Scatter(x=df['param1'], y=df['param2'], mode='markers',
                   name='Correlation'),
        row=2, col=1
    )

    # Add summary table
    summary = df.describe().reset_index()
    fig.add_trace(
        go.Table(
            header=dict(values=list(summary.columns)),
            cells=dict(values=[summary[col] for col in summary.columns])
        ),
        row=2, col=2
    )

    # Update layout
    fig.update_layout(
        height=800,
        showlegend=True,
        title_text="Interactive Analysis Dashboard"
    )

    # Save
    fig.write_html(output_file, include_plotlyjs='cdn')

# Usage
generate_dashboard(
    csv_file='../data/processed/results.csv',
    output_file='../reports/dashboard.html'
)
```

### Example 3: Automated Report Generation

```python
# auto_report.py
"""Automated report generation for all CSV files in data directory."""

import plotly.express as px
import pandas as pd
from pathlib import Path
from datetime import datetime

def auto_generate_reports(data_dir='../data/processed',
                          report_dir='../reports'):
    """
    Automatically generate HTML reports for all CSV files.

    Args:
        data_dir: Directory containing CSV files
        report_dir: Directory to save HTML reports
    """

    data_path = Path(data_dir)
    report_path = Path(report_dir)
    report_path.mkdir(exist_ok=True)

    # Find all CSV files
    csv_files = list(data_path.glob('*.csv'))

    print(f"Found {len(csv_files)} CSV files")

    for csv_file in csv_files:
        print(f"Processing: {csv_file.name}")

        try:
            # Read CSV
            df = pd.read_csv(csv_file)

            # Detect columns
            numeric_cols = df.select_dtypes(include='number').columns

            if len(numeric_cols) >= 2:
                # Create interactive plot
                fig = px.scatter(df,
                                x=numeric_cols[0],
                                y=numeric_cols[1],
                                title=f"Analysis: {csv_file.stem}",
                                hover_data=df.columns.tolist())

                # Save report
                output_file = report_path / f"{csv_file.stem}_report.html"
                fig.write_html(output_file, include_plotlyjs='cdn')

                print(f"  ✓ Generated: {output_file.name}")
            else:
                print(f"  ⚠ Skipped: Not enough numeric columns")

        except Exception as e:
            print(f"  ✗ Error: {e}")

    # Generate index page
    generate_index_page(csv_files, report_path)

def generate_index_page(csv_files, report_dir):
    """Generate index.html listing all reports."""

    html = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Analysis Reports</title>
        <style>
            body {{ font-family: Arial; max-width: 800px; margin: 40px auto; }}
            .report-link {{ display: block; padding: 10px; margin: 5px 0;
                           background: #f0f0f0; border-radius: 5px; }}
        </style>
    </head>
    <body>
        <h1>Analysis Reports</h1>
        <p>Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
        <div class="reports">
    """

    for csv_file in csv_files:
        report_file = f"{csv_file.stem}_report.html"
        html += f'<a class="report-link" href="{report_file}">{csv_file.stem}</a>\n'

    html += """
        </div>
    </body>
    </html>
    """

    index_file = report_dir / 'index.html'
    index_file.write_text(html)
    print(f"\n✓ Generated index: {index_file}")

if __name__ == '__main__':
    auto_generate_reports()
```

## Best Practices

### 1. **Performance Optimization**

**For large datasets (>10,000 points):**
```python
# Use WebGL for better performance
fig = px.scatter(df, x='x', y='y', render_mode='webgl')

# Or use data aggregation
df_sample = df.sample(n=5000)  # Sample for visualization
```

### 2. **Accessibility**

**Make reports accessible:**
- Use descriptive alt text
- Ensure sufficient color contrast
- Provide keyboard navigation
- Include ARIA labels

```python
fig.update_layout(
    title={'text': 'Analysis Results',
           'font': {'size': 20}},
    font={'size': 14},  # Readable font size
)
```

### 3. **Version Control**

**Don't commit generated HTML reports:**

```gitignore
# .gitignore
reports/*.html
!reports/index.html  # Keep index template
```

**Do commit:**
- Report generation scripts
- CSV data (if small)
- Report templates
- Documentation

### 4. **Automated Generation**

**Add to CI/CD pipeline:**

```yaml
# .github/workflows/generate-reports.yml
name: Generate Reports

on:
  push:
    paths:
      - 'data/processed/**'

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-python@v4
        with:
          python-version: '3.11'
      - run: |
          pip install plotly pandas
          python src/reporting/auto_report.py
      - uses: actions/upload-artifact@v3
        with:
          name: reports
          path: reports/*.html
```

## Validation Checklist

Before deploying any module, ensure:

- [ ] All plots are interactive (no static images)
- [ ] HTML report is generated
- [ ] CSV data uses relative paths
- [ ] Report is responsive (tested on mobile/desktop)
- [ ] Hover tooltips show data values
- [ ] Zoom/pan functionality works
- [ ] Legend is interactive (click to toggle)
- [ ] Export options available
- [ ] Report loads in <3 seconds
- [ ] Works offline (embedded data or CDN fallback)

## Support and Resources

### Documentation
- **Plotly:** https://plotly.com/python/
- **Bokeh:** https://docs.bokeh.org/
- **Altair:** https://altair-viz.github.io/
- **D3.js:** https://d3js.org/

### Agent Assistance
- **Visualization tasks:** Use `plotly-visualization-agent`
- **Dashboard creation:** Use `bokeh-dashboard-agent`
- **Statistical reports:** Use `altair-analysis-agent`

### Templates
- **Report templates:** `modules/reporting/templates/`
- **Example scripts:** `modules/reporting/examples/`

---

**Remember:** Interactive HTML reports with CSV data imports are MANDATORY for all modules across all 26 repositories. Use the agent orchestrator to automatically select the best visualization agent for your reporting needs! 📊
