# AutoViz Automatic EDA Skill

> **Quick Reference Guide**

## Overview

Automatic exploratory data analysis and visualization with a single line of code. AutoViz generates comprehensive charts, detects patterns, identifies outliers, and exports to HTML/notebooks automatically.

**Category**: Data Analysis
**Version**: 1.0.0
**Platforms**: Python

## Quick Start

```bash
# Install
pip install autoviz
# or with all backends
pip install autoviz matplotlib seaborn plotly bokeh
```

```python
from autoviz import AutoViz_Class
import pandas as pd

# Load data
df = pd.read_csv("data.csv")

# One-line EDA
AV = AutoViz_Class()
df_analyzed = AV.AutoViz(
    filename="",
    dfte=df,
    depVar="target",  # Optional target variable
    chart_format="png",
    verbose=1
)
```

## When to Use

**Use AutoViz when:**
- Quick exploration of new datasets
- Initial EDA before deeper analysis
- Generating reports for stakeholders
- Finding correlations automatically
- Detecting outliers in data

**Avoid when:**
- Need custom visualization designs
- Building interactive dashboards
- Real-time streaming data
- Production visualization systems

## Key Features

| Feature | Description |
|---------|-------------|
| One-Line EDA | Complete analysis with single function call |
| Auto Charts | Automatically selects appropriate chart types |
| Correlation | Detects and visualizes feature relationships |
| Outliers | Identifies and highlights anomalies |
| Export | Saves to PNG, SVG, HTML, or notebooks |

## Core Operations

```python
from autoviz import AutoViz_Class

AV = AutoViz_Class()

# Basic usage
df_analyzed = AV.AutoViz(filename="data.csv", verbose=1)

# From DataFrame with target
df_analyzed = AV.AutoViz(
    filename="",
    dfte=df,
    depVar="target",
    chart_format="html",
    save_plot_dir="output/"
)

# Large dataset with sampling
df_analyzed = AV.AutoViz(
    filename="",
    dfte=large_df,
    max_rows_analyzed=50000,
    max_cols_analyzed=20
)
```

## Chart Formats

```python
# PNG for presentations
chart_format="png"

# HTML for web reports
chart_format="html"

# Jupyter notebooks
chart_format="server"

# Interactive Bokeh
chart_format="bokeh"

# Vector graphics
chart_format="svg"
```

## Files

```
autoviz/
  SKILL.md    # Full documentation (700+ lines)
  README.md   # This quick reference
```

## Related Skills

- **ydata-profiling** - Comprehensive data quality reports
- **polars** - High-performance DataFrame operations
- **streamlit** - Build data apps with visualizations
- **plotly** - Custom interactive visualizations

## Resources

- [GitHub](https://github.com/AutoViML/AutoViz)
- [PyPI](https://pypi.org/project/autoviz/)
- [Tutorial](https://towardsdatascience.com/autoviz-a-new-tool-for-automated-visualization-ec9c1744a6ad)

---

**See SKILL.md for complete examples, integration patterns, and troubleshooting.**
