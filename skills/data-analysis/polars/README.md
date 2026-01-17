# Polars High-Performance DataFrame Skill

> **Quick Reference Guide**

## Overview

Blazing-fast DataFrame library built in Rust with Python bindings. Features lazy evaluation, automatic parallelization, and memory-efficient operations for datasets of any size.

**Category**: Data Analysis
**Version**: 1.0.0
**Platforms**: Python, Rust

## Quick Start

```bash
# Install
pip install polars
# or with all extras
pip install 'polars[all]'
```

```python
import polars as pl

# Read data lazily (no computation yet)
lf = pl.scan_parquet("data/*.parquet")

# Chain transformations
result = (
    lf
    .filter(pl.col("date") >= "2025-01-01")
    .with_columns([
        (pl.col("revenue") - pl.col("cost")).alias("profit")
    ])
    .group_by("category")
    .agg([
        pl.col("profit").sum().alias("total_profit"),
        pl.count().alias("count")
    ])
    .sort("total_profit", descending=True)
    .collect()  # Execute the optimized query
)
```

## When to Use

**Use Polars when:**
- Working with large datasets (10GB+)
- Need maximum performance
- Memory is constrained
- Building ETL pipelines
- Complex aggregations needed

**Avoid when:**
- Small datasets (<100MB)
- Need pandas-specific libraries
- Extensive legacy pandas codebase

## Key Features

| Feature | Description |
|---------|-------------|
| Lazy Evaluation | Query optimization before execution |
| Parallel Processing | Automatic multi-core utilization |
| Memory Efficiency | Arrow-based, zero-copy operations |
| Expression API | Powerful, composable transformations |
| Streaming | Process larger-than-memory datasets |

## Core Operations

```python
# Filter and transform
df.filter(pl.col("x") > 0).with_columns([
    (pl.col("a") + pl.col("b")).alias("sum")
])

# Group and aggregate
df.group_by("category").agg([
    pl.col("value").sum(),
    pl.col("value").mean().alias("avg")
])

# Window functions
df.with_columns([
    pl.col("value").rolling_mean(7).over("group").alias("rolling_avg")
])

# Joins
df1.join(df2, on="id", how="left")
```

## Files

```
polars/
  SKILL.md    # Full documentation (900+ lines)
  README.md   # This quick reference
```

## Related Skills

- **pandas-data-processing** - Traditional DataFrame operations
- **streamlit** - Build data apps with Polars
- **dash** - Production dashboards

## Resources

- [Official Docs](https://docs.pola.rs/)
- [User Guide](https://docs.pola.rs/user-guide/)
- [GitHub](https://github.com/pola-rs/polars)

---

**See SKILL.md for complete examples, best practices, and troubleshooting.**
