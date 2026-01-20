# Great Tables Publication-Quality Tables Skill

> **Quick Reference Guide**

## Overview

Publication-quality tables in Python with rich styling, formatting, conditional formatting, and export to HTML/images. Inspired by R's gt package.

**Category**: Data Analysis
**Version**: 1.0.0
**Platforms**: Python

## Quick Start

```bash
# Install
pip install great_tables
# with pandas/polars
pip install great_tables pandas polars
```

```python
from great_tables import GT
import pandas as pd

# Create sample data
df = pd.DataFrame({
    "Product": ["Widget A", "Widget B", "Gadget X"],
    "Revenue": [150000, 220000, 180000],
    "Growth": [0.12, 0.25, 0.08]
})

# Create formatted table
table = (
    GT(df)
    .tab_header(
        title="Q4 2025 Sales",
        subtitle="Revenue and growth metrics"
    )
    .fmt_currency(columns="Revenue", currency="USD")
    .fmt_percent(columns="Growth", decimals=1)
)

# Save to HTML
table.save("report.html")
```

## When to Use

**Use Great Tables when:**
- Creating tables for reports and presentations
- Formatting data for publication
- Applying conditional formatting
- Building HTML reports with styled tables
- Exporting tables as images

**Avoid when:**
- Displaying >1000 rows
- Need interactive sorting/filtering
- Real-time data updates
- Complex user interactions

## Key Features

| Feature | Description |
|---------|-------------|
| Rich Formatting | Currency, percent, date, custom formats |
| Conditional Colors | Color scales and data-driven styling |
| Grouping | Row groups, column spanners |
| Annotations | Footnotes, source notes, headers |
| Export | HTML, PNG, PDF output |

## Core Operations

```python
from great_tables import GT

# Basic table
table = GT(df).tab_header(title="My Table")

# Formatting
table = (
    GT(df)
    .fmt_currency(columns="Price", currency="USD")
    .fmt_percent(columns="Growth", decimals=1)
    .fmt_integer(columns="Units", use_seps=True)
)

# Styling
from great_tables import style, loc
table = (
    GT(df)
    .tab_style(
        style=style.fill(color="#4a86e8"),
        locations=loc.column_labels()
    )
)

# Color scales
table = (
    GT(df)
    .data_color(
        columns="Score",
        palette=["red", "yellow", "green"],
        domain=[0, 100]
    )
)
```

## Export Options

```python
# HTML file
table.save("table.html")

# Get HTML string
html = table.as_raw_html()

# PNG image (requires webshot)
table.save("table.png")

# Display in Jupyter
table  # Just return the object
```

## Files

```
great-tables/
  SKILL.md    # Full documentation (900+ lines)
  README.md   # This quick reference
```

## Related Skills

- **pandas-data-processing** - DataFrame manipulation
- **polars** - High-performance DataFrames
- **streamlit** - Build data apps
- **ydata-profiling** - Data quality reports

## Resources

- [Official Docs](https://posit-dev.github.io/great-tables/)
- [GitHub](https://github.com/posit-dev/great-tables)
- [PyPI](https://pypi.org/project/great-tables/)
- [Gallery](https://posit-dev.github.io/great-tables/examples/)

---

**See SKILL.md for complete examples, styling options, and integration patterns.**
