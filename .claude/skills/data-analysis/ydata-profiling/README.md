# YData Profiling Data Quality Skill

> **Quick Reference Guide**

## Overview

Automated data quality reports with comprehensive variable analysis, missing value detection, correlations, and HTML report generation. Formerly known as pandas-profiling.

**Category**: Data Analysis
**Version**: 1.0.0
**Platforms**: Python

## Quick Start

```bash
# Install
pip install ydata-profiling
# or with all extras
pip install 'ydata-profiling[all]'
```

```python
from ydata_profiling import ProfileReport
import pandas as pd

# Load data
df = pd.read_csv("data.csv")

# Generate profile report
profile = ProfileReport(df, title="Data Quality Report")

# Save to HTML
profile.to_file("report.html")

# Display in Jupyter
profile.to_notebook_iframe()
```

## When to Use

**Use YData Profiling when:**
- Assessing data quality and completeness
- Exploring new datasets quickly
- Detecting missing value patterns
- Finding correlations between variables
- Creating shareable data documentation

**Avoid when:**
- Real-time analysis requirements
- Custom visualization needs
- Very large datasets (>10M rows)
- Production pipeline validation

## Key Features

| Feature | Description |
|---------|-------------|
| Variable Analysis | Automatic type detection and statistics |
| Missing Values | Patterns, correlations, and visualizations |
| Correlations | Pearson, Spearman, Kendall, Phi-K methods |
| Comparison | Compare multiple datasets side by side |
| HTML Reports | Publication-ready interactive reports |

## Core Operations

```python
from ydata_profiling import ProfileReport, compare

# Basic profile
profile = ProfileReport(df, title="Report")
profile.to_file("report.html")

# Minimal mode (fast)
profile = ProfileReport(df, minimal=True)

# Explorative mode (detailed)
profile = ProfileReport(df, explorative=True)

# Compare datasets
profile1 = ProfileReport(df_train, title="Train")
profile2 = ProfileReport(df_test, title="Test")
comparison = compare([profile1, profile2])
comparison.to_file("comparison.html")
```

## Profile Modes

```python
# Full analysis (slower, comprehensive)
profile = ProfileReport(
    df,
    explorative=True,
    correlations={
        "pearson": {"calculate": True},
        "spearman": {"calculate": True}
    }
)

# Minimal mode (faster, essential stats)
profile = ProfileReport(df, minimal=True)

# Custom configuration
profile = ProfileReport(
    df,
    correlations={"pearson": {"calculate": True}},
    missing_diagrams={"bar": True, "matrix": False}
)
```

## Files

```
ydata-profiling/
  SKILL.md    # Full documentation (900+ lines)
  README.md   # This quick reference
```

## Related Skills

- **autoviz** - One-line automatic EDA
- **pandas-data-processing** - DataFrame manipulation
- **great-tables** - Publication-quality tables
- **polars** - High-performance DataFrames

## Resources

- [Official Docs](https://docs.profiling.ydata.ai/)
- [GitHub](https://github.com/ydataai/ydata-profiling)
- [PyPI](https://pypi.org/project/ydata-profiling/)

---

**See SKILL.md for complete examples, comparison reports, and troubleshooting.**
