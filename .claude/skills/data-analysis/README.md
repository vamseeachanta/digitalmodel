# Data Analysis Skills Library

> Data processing, visualization, EDA, and dashboard patterns
> Version: 1.0.0 | Last Updated: 2026-01-17

## Overview

This library contains 8 production-ready skills for data analysis, visualization, and exploratory data analysis (EDA). Each skill covers a specific aspect of the data analysis workflow with patterns for performance, interactivity, and presentation. Skills follow the Anthropic Skills format with practical examples from real-world data projects.

## Quick Start

```bash
# Browse available skills
ls skills/data-analysis/

# Read a skill
cat skills/data-analysis/polars/SKILL.md

# Skills are documentation - implement patterns in your data workflows
```

## Available Skills

| Skill | Description | Key Features |
|-------|-------------|--------------|
| [polars](./polars/SKILL.md) | High-performance DataFrame library | Lazy evaluation, parallel processing, memory efficiency |
| [streamlit](./streamlit/SKILL.md) | Rapid data app development | Interactive widgets, caching, deployment |
| [dash](./dash/SKILL.md) | Production-ready dashboards | Plotly integration, callbacks, enterprise features |
| [autoviz](./autoviz/SKILL.md) | Automated visualization | One-line EDA, smart chart selection |
| [ydata-profiling](./ydata-profiling/SKILL.md) | Automated data profiling | HTML reports, correlations, quality metrics |
| [great-tables](./great-tables/SKILL.md) | Publication-quality tables | Styling, formatting, export options |
| [sweetviz](./sweetviz/SKILL.md) | EDA comparison reports | Target analysis, dataset comparison |
| [bsee-sodir-extraction](./bsee-sodir-extraction/SKILL.md) | Energy regulatory data extraction | BSEE, SODIR, production, HSE data |

## Skill Categories

### Data Processing
- **polars** - Blazing-fast DataFrame operations for large datasets

### Dashboard & Web Apps
- **streamlit** - Quick prototyping and data apps
- **dash** - Production dashboards with enterprise features

### Automated EDA
- **autoviz** - One-line automated visualizations
- **ydata-profiling** - Comprehensive data quality reports
- **sweetviz** - Visual comparison and target analysis

### Presentation & Reporting
- **great-tables** - Beautiful tables for reports and publications

### Domain-Specific Data
- **bsee-sodir-extraction** - BSEE/SODIR energy regulatory data extraction

## Skill Selection Guide

### Choose polars when:
- Working with datasets too large for pandas
- Need maximum performance for data transformations
- Processing data in memory-constrained environments
- Lazy evaluation and query optimization are valuable

### Choose streamlit when:
- Rapid prototyping of data applications
- Internal tools and demos
- Data scientists building apps (minimal frontend knowledge)
- Need quick iteration on interactive visualizations

### Choose dash when:
- Building production-grade dashboards
- Enterprise features required (authentication, scaling)
- Complex callback interactions between components
- Plotly ecosystem integration is desired

### Choose autoviz when:
- Quick initial data exploration
- Need automated chart type selection
- Time is limited for manual visualization
- Working with unfamiliar datasets

### Choose ydata-profiling when:
- Comprehensive data quality assessment needed
- Generating shareable HTML reports
- Identifying data issues (missing values, duplicates)
- Need correlation analysis and distribution insights

### Choose great-tables when:
- Creating publication-quality table output
- Need fine-grained control over table styling
- Generating tables for reports or presentations
- Export to multiple formats (HTML, LaTeX, PNG)

### Choose sweetviz when:
- Comparing two datasets (train/test, before/after)
- Target variable analysis for ML projects
- Visual EDA with minimal code
- Need side-by-side feature comparisons

## Quick Examples

### Polars High-Performance Processing
```python
import polars as pl

# Read large CSV with lazy evaluation
df = pl.scan_csv("large_data.csv")

# Chain operations efficiently
result = (
    df
    .filter(pl.col("date") >= "2025-01-01")
    .with_columns([
        (pl.col("revenue") - pl.col("cost")).alias("profit"),
        pl.col("category").cast(pl.Categorical)
    ])
    .group_by("category")
    .agg([
        pl.col("profit").sum().alias("total_profit"),
        pl.col("profit").mean().alias("avg_profit"),
        pl.count().alias("transactions")
    ])
    .sort("total_profit", descending=True)
    .collect()  # Execute the query
)

# Memory-efficient joins
orders = pl.scan_parquet("orders/*.parquet")
customers = pl.scan_parquet("customers.parquet")

enriched = (
    orders
    .join(customers, on="customer_id", how="left")
    .filter(pl.col("country") == "USA")
    .collect()
)
```

### Streamlit Data App
```python
import streamlit as st
import polars as pl
import plotly.express as px

st.set_page_config(page_title="Sales Dashboard", layout="wide")
st.title("Sales Analytics Dashboard")

# Sidebar filters
st.sidebar.header("Filters")
date_range = st.sidebar.date_input(
    "Date Range",
    value=(datetime(2025, 1, 1), datetime.now())
)
category = st.sidebar.multiselect(
    "Category",
    options=["Electronics", "Clothing", "Food", "Home"],
    default=["Electronics", "Clothing"]
)

# Load and filter data
@st.cache_data
def load_data():
    return pl.read_parquet("sales_data.parquet")

df = load_data()
filtered = df.filter(
    (pl.col("date").is_between(*date_range)) &
    (pl.col("category").is_in(category))
)

# Metrics row
col1, col2, col3, col4 = st.columns(4)
col1.metric("Total Revenue", f"${filtered['revenue'].sum():,.0f}")
col2.metric("Orders", f"{len(filtered):,}")
col3.metric("Avg Order Value", f"${filtered['revenue'].mean():,.2f}")
col4.metric("Unique Customers", f"{filtered['customer_id'].n_unique():,}")

# Charts
st.subheader("Revenue Trend")
daily = filtered.group_by("date").agg(pl.col("revenue").sum())
fig = px.line(daily.to_pandas(), x="date", y="revenue")
st.plotly_chart(fig, use_container_width=True)

# Data table
st.subheader("Recent Transactions")
st.dataframe(filtered.head(100).to_pandas())
```

### Dash Production Dashboard
```python
from dash import Dash, html, dcc, callback, Output, Input
import plotly.express as px
import polars as pl

app = Dash(__name__)

# Layout
app.layout = html.Div([
    html.H1("Analytics Dashboard"),

    html.Div([
        html.Label("Select Metric:"),
        dcc.Dropdown(
            id="metric-dropdown",
            options=[
                {"label": "Revenue", "value": "revenue"},
                {"label": "Profit", "value": "profit"},
                {"label": "Units Sold", "value": "units"}
            ],
            value="revenue"
        )
    ], style={"width": "300px"}),

    dcc.Graph(id="main-chart"),

    html.Div([
        dcc.Graph(id="pie-chart", style={"width": "50%", "display": "inline-block"}),
        dcc.Graph(id="bar-chart", style={"width": "50%", "display": "inline-block"})
    ])
])

@callback(
    Output("main-chart", "figure"),
    Output("pie-chart", "figure"),
    Output("bar-chart", "figure"),
    Input("metric-dropdown", "value")
)
def update_charts(metric):
    df = pl.read_parquet("data.parquet").to_pandas()

    # Time series
    main_fig = px.line(
        df.groupby("date")[metric].sum().reset_index(),
        x="date", y=metric,
        title=f"{metric.title()} Over Time"
    )

    # Category breakdown
    pie_fig = px.pie(
        df.groupby("category")[metric].sum().reset_index(),
        values=metric, names="category",
        title=f"{metric.title()} by Category"
    )

    # Regional bar chart
    bar_fig = px.bar(
        df.groupby("region")[metric].sum().reset_index(),
        x="region", y=metric,
        title=f"{metric.title()} by Region"
    )

    return main_fig, pie_fig, bar_fig

if __name__ == "__main__":
    app.run_server(debug=True)
```

### YData Profiling Report
```python
from ydata_profiling import ProfileReport
import polars as pl

# Load data
df = pl.read_csv("dataset.csv").to_pandas()

# Generate comprehensive report
profile = ProfileReport(
    df,
    title="Dataset Quality Report",
    explorative=True,
    correlations={
        "pearson": {"calculate": True},
        "spearman": {"calculate": True},
        "kendall": {"calculate": False}
    }
)

# Save as HTML
profile.to_file("data_quality_report.html")

# Get specific sections
overview = profile.get_description()
print(f"Variables: {overview['table']['n_var']}")
print(f"Observations: {overview['table']['n']}")
print(f"Missing cells: {overview['table']['n_cells_missing']}")
```

### Great Tables Publication Output
```python
from great_tables import GT, md, html
import polars as pl

# Prepare summary data
summary = (
    pl.read_parquet("sales.parquet")
    .group_by("product_category")
    .agg([
        pl.col("revenue").sum().alias("total_revenue"),
        pl.col("revenue").mean().alias("avg_revenue"),
        pl.col("units").sum().alias("total_units"),
        ((pl.col("revenue").sum() - pl.col("cost").sum()) /
         pl.col("revenue").sum() * 100).alias("margin_pct")
    ])
    .sort("total_revenue", descending=True)
)

# Create publication-quality table
table = (
    GT(summary.to_pandas())
    .tab_header(
        title=md("**Q1 2026 Sales Summary**"),
        subtitle="Performance by Product Category"
    )
    .fmt_currency(
        columns=["total_revenue", "avg_revenue"],
        currency="USD"
    )
    .fmt_number(
        columns="total_units",
        use_seps=True
    )
    .fmt_percent(
        columns="margin_pct",
        decimals=1
    )
    .cols_label(
        product_category="Category",
        total_revenue="Total Revenue",
        avg_revenue="Avg. Order",
        total_units="Units Sold",
        margin_pct="Margin %"
    )
    .tab_spanner(
        label="Revenue Metrics",
        columns=["total_revenue", "avg_revenue"]
    )
    .data_color(
        columns="margin_pct",
        palette=["#fee0d2", "#fc9272", "#de2d26"]
    )
    .tab_source_note("Data: Internal Sales Database")
)

# Export
table.save("sales_summary.html")
table.save("sales_summary.png")
```

### Sweetviz Comparison Report
```python
import sweetviz as sv
import polars as pl

# Load datasets
train_df = pl.read_csv("train.csv").to_pandas()
test_df = pl.read_csv("test.csv").to_pandas()

# Compare train vs test
comparison_report = sv.compare([train_df, "Training"], [test_df, "Test"])
comparison_report.show_html("train_test_comparison.html")

# Analyze with target variable
target_report = sv.analyze(
    train_df,
    target_feat="target_column",
    feat_cfg=sv.FeatureConfig(skip=["id", "timestamp"])
)
target_report.show_html("target_analysis.html")
```

## Integration Patterns

### Data Pipeline Architecture
```
Raw Data --> Ingestion --> Processing --> Analysis --> Visualization --> Reporting
    |            |             |             |              |              |
    +-- Files   +-- Validate  +-- polars   +-- EDA tools  +-- streamlit  +-- great-tables
    +-- APIs    +-- Clean     +-- Transform +-- Profiling  +-- dash       +-- Export
    +-- DB      +-- Schema    +-- Aggregate +-- Insights   +-- Charts     +-- Share
```

### Dashboard Development Flow
```
Requirements --> Prototype --> Iterate --> Deploy --> Monitor
      |              |            |           |           |
      +-- Metrics   +-- streamlit +-- Feedback +-- Cloud  +-- Usage
      +-- Users     +-- Mock data +-- Polish   +-- Auth   +-- Performance
      +-- Frequency +-- Layout    +-- Test     +-- Scale  +-- Alerts
```

### EDA Workflow
```
Load Data --> Profile --> Visualize --> Document --> Share
     |            |            |            |           |
     +-- Sample  +-- ydata    +-- autoviz  +-- Findings +-- HTML
     +-- Schema  +-- sweetviz +-- Custom   +-- Issues   +-- Notebook
     +-- Types   +-- Alerts   +-- Explore  +-- Next     +-- Slides
```

## Common Patterns Across Skills

### Caching for Performance
```python
import streamlit as st
from functools import lru_cache

@st.cache_data(ttl=3600)  # Streamlit caching
def load_and_process_data():
    return pl.read_parquet("data.parquet")

@lru_cache(maxsize=100)  # General Python caching
def expensive_calculation(params_tuple):
    return compute_metrics(params_tuple)
```

### Consistent Styling
```python
# Define color palette
COLORS = {
    "primary": "#1f77b4",
    "secondary": "#ff7f0e",
    "success": "#2ca02c",
    "danger": "#d62728",
    "neutral": "#7f7f7f"
}

# Plotly template
import plotly.io as pio
pio.templates["custom"] = go.layout.Template(
    layout=dict(
        colorway=list(COLORS.values()),
        font=dict(family="Inter, sans-serif")
    )
)
pio.templates.default = "custom"
```

### Error Handling for Data Loading
```python
def safe_load_data(path, fallback=None):
    """Load data with comprehensive error handling."""
    try:
        if path.endswith('.parquet'):
            return pl.read_parquet(path)
        elif path.endswith('.csv'):
            return pl.read_csv(path)
        else:
            raise ValueError(f"Unsupported format: {path}")
    except FileNotFoundError:
        logger.error(f"File not found: {path}")
        return fallback
    except Exception as e:
        logger.error(f"Failed to load {path}: {e}")
        return fallback
```

## Integration with Workspace-Hub

These skills power data analysis across the workspace-hub ecosystem:

```
workspace-hub/
├── data/
│   ├── pipelines/           # Uses: polars
│   │   ├── etl_pipeline.py
│   │   └── aggregations.py
│   ├── dashboards/          # Uses: streamlit, dash
│   │   ├── streamlit_app/
│   │   └── dash_app/
│   ├── reports/             # Uses: ydata-profiling, great-tables
│   │   ├── quality_reports/
│   │   └── summary_tables/
│   └── eda/                 # Uses: autoviz, sweetviz
│       └── exploration/
├── output/
│   ├── reports/
│   └── exports/
└── config/
    └── analysis_config.yaml
```

## Best Practices

### 1. Lazy Evaluation First
```python
# Prefer lazy operations, collect only when needed
result = (
    pl.scan_parquet("data/*.parquet")
    .filter(...)
    .group_by(...)
    .agg(...)
    .collect()  # Execute at the end
)
```

### 2. Progressive Disclosure in Dashboards
```python
# Start with summary, allow drill-down
st.header("Overview")
show_metrics()

with st.expander("Detailed Analysis"):
    show_detailed_charts()

with st.expander("Raw Data"):
    st.dataframe(df)
```

### 3. Reproducible Reports
```python
# Include metadata in reports
report_metadata = {
    "generated_at": datetime.now().isoformat(),
    "data_source": "sales_database",
    "date_range": f"{start_date} to {end_date}",
    "filters_applied": filters
}
```

### 4. Performance Monitoring
```python
import time

def timed_operation(name):
    def decorator(func):
        def wrapper(*args, **kwargs):
            start = time.time()
            result = func(*args, **kwargs)
            duration = time.time() - start
            logger.info(f"{name} completed in {duration:.2f}s")
            return result
        return wrapper
    return decorator

@timed_operation("Data aggregation")
def aggregate_sales():
    ...
```

## Testing Data Analysis Code

```python
import pytest
import polars as pl

def test_aggregation_logic():
    """Test aggregation produces expected results."""
    test_data = pl.DataFrame({
        "category": ["A", "A", "B"],
        "value": [100, 200, 150]
    })

    result = aggregate_by_category(test_data)

    assert result.filter(pl.col("category") == "A")["total"][0] == 300
    assert result.filter(pl.col("category") == "B")["total"][0] == 150

def test_dashboard_callback():
    """Test dashboard callback returns valid figures."""
    from dash.testing.composite import DashComposite

    # Test callback outputs are valid plotly figures
    main, pie, bar = update_charts("revenue")

    assert main.data is not None
    assert pie.data is not None
    assert bar.data is not None

def test_report_generation():
    """Test profiling report generates without errors."""
    df = create_test_dataframe()
    report = ProfileReport(df, minimal=True)

    assert report.get_description() is not None
```

## Related Resources

- [Polars Documentation](https://pola.rs/)
- [Streamlit Documentation](https://docs.streamlit.io/)
- [Plotly Dash Documentation](https://dash.plotly.com/)
- [YData Profiling Docs](https://docs.profiling.ydata.ai/)
- [Great Tables Documentation](https://posit-dev.github.io/great-tables/)
- [Sweetviz Documentation](https://github.com/fbdesignpro/sweetviz)

## Version History

- **1.0.0** (2026-01-17): Initial release with 7 data analysis skills

---

*These skills represent patterns refined across data platforms processing millions of records and serving dashboards to thousands of users.*
