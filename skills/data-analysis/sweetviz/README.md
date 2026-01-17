# Sweetviz EDA Comparison Skill

> **Quick Reference Guide**

## Overview

Automated EDA comparison reports with target analysis, feature comparison between datasets, and HTML report generation. Excels at comparing train vs test data and analyzing features against target variables.

**Category**: Data Analysis
**Version**: 1.0.0
**Platforms**: Python

## Quick Start

```bash
# Install
pip install sweetviz
```

```python
import sweetviz as sv
import pandas as pd

# Load data
df = pd.read_csv("data.csv")

# Basic analysis
report = sv.analyze(df)
report.show_html("report.html")

# With target variable
report = sv.analyze(df, target_feat="target")
report.show_html("target_analysis.html")
```

## When to Use

**Use Sweetviz when:**
- Comparing train vs test datasets
- Analyzing features against a target variable
- Quick, comprehensive EDA reports
- Detecting data drift between datasets
- Feature selection for ML

**Avoid when:**
- Very large datasets (>1M rows)
- Custom visualization requirements
- Interactive dashboards needed

## Key Features

| Feature | Description |
|---------|-------------|
| Target Analysis | Shows how each feature relates to target |
| Dataset Comparison | Compare train/test or any two datasets |
| Intra-set Comparison | Compare subpopulations within a dataset |
| HTML Reports | Beautiful, interactive reports |
| Correlation Analysis | Pairwise feature correlations |

## Core Operations

### Single Dataset Analysis

```python
import sweetviz as sv

# Basic analysis
report = sv.analyze(df)

# With target variable
report = sv.analyze(df, target_feat="target")

# Save report
report.show_html("analysis.html")
```

### Dataset Comparison

```python
# Compare two datasets
comparison = sv.compare(
    source=[train_df, "Training Data"],
    compare=[test_df, "Test Data"],
    target_feat="target"
)
comparison.show_html("comparison.html")
```

### Intra-set Comparison

```python
# Compare subgroups within dataset
report = sv.compare_intra(
    source_df=df,
    condition_series=df["category"] == "Premium",
    names=["Premium", "Standard"],
    target_feat="churned"
)
```

### Feature Configuration

```python
# Skip columns, force types
config = sv.FeatureConfig(
    skip=["id", "email"],
    force_cat=["zip_code", "rating"]
)

report = sv.analyze(df, target_feat="target", feat_cfg=config)
```

## Comparison Methods

| Method | Use Case |
|--------|----------|
| `sv.analyze()` | Single dataset EDA |
| `sv.compare()` | Two different datasets |
| `sv.compare_intra()` | Subpopulations in one dataset |

## Files

```
sweetviz/
  SKILL.md    # Full documentation (900+ lines)
  README.md   # This quick reference
```

## Related Skills

- **ydata-profiling** - Alternative data profiling tool
- **autoviz** - One-line automatic EDA
- **pandas-data-processing** - DataFrame manipulation
- **polars** - High-performance DataFrames

## Resources

- [GitHub Repository](https://github.com/fbdesignpro/sweetviz)
- [PyPI](https://pypi.org/project/sweetviz/)
- [Tutorial Article](https://towardsdatascience.com/powerful-eda-exploratory-data-analysis-in-just-two-lines-of-code-using-sweetviz-6c943d32f34)

---

**See SKILL.md for complete examples, ML pipeline integration, and troubleshooting.**
