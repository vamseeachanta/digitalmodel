# Dash Production Dashboard Skill

> **Quick Reference Guide**

## Overview

Build enterprise-grade interactive dashboards with Plotly Dash. Features reactive callbacks, professional layouts, and scalable deployment for production workloads.

**Category**: Data Analysis
**Version**: 1.0.0
**Platforms**: Python, Web

## Quick Start

```bash
# Install
pip install dash dash-bootstrap-components plotly pandas

# Run app
python app.py
```

```python
from dash import Dash, html, dcc, callback, Output, Input
import dash_bootstrap_components as dbc
import plotly.express as px
import pandas as pd

app = Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])

df = pd.DataFrame({"x": [1, 2, 3], "y": [4, 5, 6], "cat": ["A", "B", "A"]})

app.layout = dbc.Container([
    html.H1("Dashboard"),
    dcc.Dropdown(id="cat-filter", options=["A", "B"], value="A"),
    dcc.Graph(id="chart")
])

@callback(Output("chart", "figure"), Input("cat-filter", "value"))
def update_chart(cat):
    return px.line(df[df["cat"] == cat], x="x", y="y")

if __name__ == "__main__":
    app.run(debug=True)
```

## When to Use

**Use Dash when:**
- Building production dashboards
- Need complex callback interactions
- Enterprise features required
- Multi-user concurrent access
- Plotly ecosystem integration

**Avoid when:**
- Quick prototypes (use Streamlit)
- Simple static reports
- Single-user internal tools

## Key Components

| Component | Purpose |
|-----------|---------|
| `dcc.Graph` | Plotly charts |
| `dcc.Dropdown` | Selection input |
| `dcc.Slider` | Numeric input |
| `dbc.Card` | Bootstrap card |
| `dbc.Row/Col` | Grid layout |
| `@callback` | Reactive updates |

## Callback Pattern

```python
from dash import callback, Output, Input, State

@callback(
    Output("output-id", "children"),  # What to update
    Input("input-id", "value"),       # Triggers callback
    State("state-id", "value"),       # Doesn't trigger
    prevent_initial_call=True
)
def update_output(input_val, state_val):
    return f"Input: {input_val}, State: {state_val}"
```

## Multi-Page Apps

```python
# app.py
from dash import Dash, page_container
app = Dash(__name__, use_pages=True)
app.layout = html.Div([navbar, page_container])

# pages/home.py
from dash import register_page
register_page(__name__, path="/")
layout = html.H1("Home")
```

## Files

```
dash/
  SKILL.md    # Full documentation (950+ lines)
  README.md   # This quick reference
```

## Related Skills

- **plotly** - Visualization library
- **streamlit** - Rapid prototyping
- **polars** - Fast data processing

## Resources

- [Official Docs](https://dash.plotly.com/)
- [Bootstrap Components](https://dash-bootstrap-components.opensource.faculty.ai/)
- [AG Grid](https://dash.plotly.com/dash-ag-grid)

---

**See SKILL.md for complete examples, enterprise patterns, and deployment guides.**
