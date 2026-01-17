# Streamlit Data Application Skill

> **Quick Reference Guide**

## Overview

Build interactive data applications and dashboards with pure Python. Transform scripts into shareable web apps in minutes with widgets, charts, and layouts.

**Category**: Data Analysis
**Version**: 1.0.0
**Platforms**: Python, Web

## Quick Start

```bash
# Install
pip install streamlit plotly pandas

# Run app
streamlit run app.py
```

```python
import streamlit as st
import pandas as pd
import plotly.express as px

st.set_page_config(page_title="My App", layout="wide")
st.title("Sales Dashboard")

# Sidebar filters
category = st.sidebar.multiselect("Category", ["A", "B", "C"])

# Load data with caching
@st.cache_data
def load_data():
    return pd.read_csv("sales.csv")

df = load_data()

# Metrics
col1, col2 = st.columns(2)
col1.metric("Revenue", f"${df['revenue'].sum():,.0f}")
col2.metric("Orders", f"{len(df):,}")

# Chart
fig = px.line(df, x="date", y="revenue")
st.plotly_chart(fig, use_container_width=True)
```

## When to Use

**Use Streamlit when:**
- Rapid prototyping of data apps
- Internal tools and demos
- Interactive data exploration
- ML model interfaces
- Simple dashboards

**Avoid when:**
- Complex callback logic needed
- Enterprise authentication required
- High-traffic production use

## Key Components

| Component | Purpose |
|-----------|---------|
| `st.title()` | Page headers |
| `st.columns()` | Layout columns |
| `st.sidebar` | Filter sidebar |
| `st.selectbox()` | Selection widget |
| `st.plotly_chart()` | Plotly integration |
| `@st.cache_data` | Performance caching |
| `st.session_state` | State management |

## Common Patterns

```python
# Caching expensive operations
@st.cache_data(ttl=3600)
def load_data():
    return pd.read_parquet("data.parquet")

# Session state
if "counter" not in st.session_state:
    st.session_state.counter = 0

# Forms (batch inputs)
with st.form("my_form"):
    name = st.text_input("Name")
    submitted = st.form_submit_button("Submit")

# Tabs
tab1, tab2 = st.tabs(["Chart", "Data"])
with tab1:
    st.plotly_chart(fig)
```

## Files

```
streamlit/
  SKILL.md    # Full documentation (900+ lines)
  README.md   # This quick reference
```

## Related Skills

- **polars** - Fast data processing
- **dash** - Production dashboards
- **plotly** - Interactive charts

## Resources

- [Official Docs](https://docs.streamlit.io/)
- [Gallery](https://streamlit.io/gallery)
- [Cloud Deployment](https://streamlit.io/cloud)

---

**See SKILL.md for complete examples, deployment patterns, and best practices.**
