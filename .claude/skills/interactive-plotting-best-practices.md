# Interactive Plotting Best Practices

**Purpose**: Guidelines for creating effective interactive HTML visualizations with Plotly

## Core Principles

### 1. Clear Visual Hierarchy (All Plots)

**Labels Must Be Bold and Prominent**
- Subplot titles: Use `<b>` tags for bold text
- Axis labels: Include units and use bold formatting
- Font sizes: Titles 16px, axis labels 14px minimum

```python
# Good - Clear labels with units
fig.update_yaxes(
    title_text=f"<b>{dof_label} ({dof_unit})</b>",
    title_font=dict(size=14)
)

# Bad - Small, unclear labels
fig.update_yaxes(title_text="Amplitude")
```

### 2. Y-Axis Range Management

#### For Displacement RAO Plots

**Avoid Noise by Setting Minimum Ranges**
- **Displacement RAOs only**: Set minimum y-axis range to 0.2
- Prevents small RAO values from creating noisy, hard-to-read plots
- Calculate range dynamically based on data

```python
# Displacement RAO plots - use minimum range of 0.2
min_range = 0.2  # Specific to displacement RAOs
for dof_name in dofs:
    all_vals = get_all_displacement_rao_values(dof_name)
    max_val = np.max(all_vals)

    # Use larger of min_range or data max (with margin)
    y_max = max(min_range, max_val * 1.1)  # 10% margin
    y_ranges[dof_name] = [0, y_max]

# Apply ranges to subplots
fig.update_yaxes(range=y_ranges[dof_name], row=row, col=col)
```

**Why This Matters for RAO Plots**:
- Displacement RAO values < 0.2 are typically small responses
- Small max values (e.g., 0.01) make noise look significant
- Users can't distinguish signal from noise at tiny scales
- Consistent scales aid comparison across different headings

#### For Other Plot Types

Use data-driven ranges appropriate to the context:
- Time series: Use actual min/max with small margin
- Stress plots: Use design limits or code requirements
- Fatigue damage: Use log scale if needed

### 3. Subplot Organization (All Plots)

**Multi-Panel Layouts**
```python
# Create organized subplots
fig = make_subplots(
    rows=3, cols=2,
    subplot_titles=[f"<b>{dof}</b>" for dof in dofs],
    vertical_spacing=0.12,
    horizontal_spacing=0.10
)

# Update subplot title formatting
for annotation in fig['layout']['annotations'][:6]:
    annotation['font'] = dict(size=16)
```

### 4. Interactive Elements (All Plots)

**Hover Information**
```python
# Comprehensive hover template
hovertemplate=(
    f'<b>{source} {heading}°</b><br>'
    'Period: %{x:.2f}s<br>'
    f'{label}: %{{y:.4f}} {unit}<br>'
    '<extra></extra>'  # Remove trace info
)
```

**Dropdown Menus**
```python
# Create selection controls
updatemenus=[{
    'buttons': buttons,
    'direction': "down",
    'showactive': True,
    'x': 0.02,
    'y': 1.15,
    'bgcolor': "white",
    'bordercolor': "gray",
    'borderwidth': 1
}]
```

### 5. Data Export (All Plots)

**Always Include CSV Export**
```python
# Save data alongside HTML
output_csv_dir.mkdir(exist_ok=True)
df.to_csv(output_csv_dir / "data.csv", index=False)
```

**Include Export Button**
```python
config={
    'displayModeBar': True,
    'displaylogo': False,
    'toImageButtonOptions': {
        'format': 'png',
        'filename': 'plot_name',
        'height': 1000,
        'width': 1400,
        'scale': 2  # Higher resolution
    }
}
```

## Common Patterns

### Displacement RAO Plot Layout (6 DOFs)
```python
dofs = [
    ('surge', 'Surge', 'm/m'),
    ('sway', 'Sway', 'm/m'),
    ('heave', 'Heave', 'm/m'),
    ('roll', 'Roll', 'rad/m'),
    ('pitch', 'Pitch', 'rad/m'),
    ('yaw', 'Yaw', 'rad/m')
]

# 3 rows × 2 columns layout
for dof_idx, (dof_name, dof_label, dof_unit) in enumerate(dofs):
    row = (dof_idx // 2) + 1
    col = (dof_idx % 2) + 1
    # Add traces to subplot
```

### Comparison Plots
```python
# Source 1: solid line with markers
go.Scatter(
    mode='lines+markers',
    line=dict(color=color, width=2),
    marker=dict(size=6)
)

# Source 2: dashed line
go.Scatter(
    mode='lines',
    line=dict(color=color, width=2, dash='dash')
)
```

## Anti-Patterns to Avoid

### General (All Plots)
❌ **Unlabeled axes** or missing units
❌ **Tiny font sizes** (< 12px)
❌ **No data export** capability
❌ **Static matplotlib** instead of interactive Plotly
❌ **No hover information**
❌ **Cluttered legends** without grouping

### Displacement RAO Plots Specifically
❌ **Small y-axis ranges** (< 0.2 for displacement RAO plots)
❌ **Auto-scaled ranges** that make noise prominent

## Checklist

### All Interactive Plots
- [ ] All subplots have clear, bold titles
- [ ] Axis labels include units in bold
- [ ] Font sizes: titles 16px, labels 14px
- [ ] Hover templates show all relevant info
- [ ] CSV data exported to `/data/` directory
- [ ] Export button configured for high-res PNG
- [ ] Legend organized and readable
- [ ] Grid lines enabled for readability

### Displacement RAO Plots Specifically
- [ ] Y-axis ranges ≥ 0.2 for all DOFs
- [ ] Small responses don't dominate visual scale
- [ ] Consistent scales across similar plots

## File Organization

```
project_folder/
├── interactive_plot.html          # Main visualization
├── generate_plot.py               # Generation script
└── data/
    ├── source1_data.csv          # Raw data (source 1)
    └── source2_data.csv          # Raw data (source 2)
```

## References

- Plotly Documentation: https://plotly.com/python/
- HTML Reporting Standards: `docs/modules/standards/HTML_REPORTING_STANDARDS.md`
- Marine Engineering Examples: `docs/modules/orcawave/L01_aqwa_benchmark/`

---

**Last Updated**: 2026-01-06
**Related Standards**: HTML_REPORTING_STANDARDS.md
