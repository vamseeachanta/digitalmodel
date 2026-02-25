#!/usr/bin/env python
"""Generate interactive HTML report for 900kN env_case analysis.

Processes OrcaFlex simulation results matching pattern 900kN_env_case_*deg.sim
and generates an interactive HTML report with Plotly charts.
"""
import re
import OrcFxAPI
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from pathlib import Path

# X65 yield strength
X65_YIELD_MPa = 65 * 6.89476  # 448.2 MPa

# Utilization limit (0.90 for design margin)
UTIL_LIMIT = 0.90

# Find sim files matching pattern
runs_dir = Path(__file__).parent.parent / 'runs'
pattern = '900kN_env_case_*deg.sim'
sim_files = sorted(runs_dir.glob(pattern))

# Create output directory for rangegraphs
rangegraph_dir = Path(__file__).parent / 'rangegraphs_env_case'
rangegraph_dir.mkdir(exist_ok=True)

print(f'Processing {len(sim_files)} sim files matching pattern: {pattern}')

results = []

# Model state constants
MS_RESET = 0
MS_CALCULATING_STATICS = 1
MS_IN_STATIC_STATE = 2
MS_SIMULATING = 3
MS_SIMULATION_COMPLETE = 4

# Store rangegraph data for chart generation
rangegraph_data = []


def parse_filename(filename: str) -> tuple:
    """Parse filename to extract env_case and heading.

    Examples:
        900kN_env_case_A_0deg -> ('A', '0deg')
        900kN_env_case_C_50deg -> ('C', '50deg')
    """
    match = re.match(r'900kN_env_case_([A-Z])_(\d+deg)', filename)
    if match:
        return match.group(1), match.group(2)
    return None, None


for sim_file in sim_files:
    print(f'  Processing: {sim_file.name}')
    model = OrcFxAPI.Model(str(sim_file))
    pipeline = model['pipeline']

    # Check if model has dynamic results
    has_dynamic = model.state == MS_SIMULATION_COMPLETE

    period_static = OrcFxAPI.pnStaticState

    if has_dynamic:
        period_rg = OrcFxAPI.pnLatestWave
        end_a_dyn = pipeline.TimeHistory('Effective Tension', period_rg, objectExtra=OrcFxAPI.oeEndA)
        end_a_max = max(end_a_dyn)
        end_a_min = min(end_a_dyn)
    else:
        # Static-only: use static state for RangeGraph
        period_rg = period_static
        end_a_static = pipeline.TimeHistory('Effective Tension', period_static, objectExtra=OrcFxAPI.oeEndA)[0]
        end_a_max = end_a_static
        end_a_min = end_a_static
        print(f'    (Static-only results)')

    # Extract RangeGraphs for arc-length distributions
    rg_tension = pipeline.RangeGraph('Effective Tension', period_rg)
    rg_y = pipeline.RangeGraph('y', period_rg)
    rg_strain = pipeline.RangeGraph('Direct Tensile Strain', period_rg)
    rg_bend_stress = pipeline.RangeGraph('Max Bending Stress', period_rg)
    rg_tens_stress = pipeline.RangeGraph('Direct Tensile Stress', period_rg)

    # Use tension arc length as reference
    arc_length = np.array(rg_tension.X)

    # Interpolate y values to match tension arc length
    y_arc = np.array(rg_y.X)
    if len(y_arc) != len(arc_length):
        if has_dynamic:
            y_min_interp = np.interp(arc_length, y_arc, np.array(rg_y.Min))
            y_max_interp = np.interp(arc_length, y_arc, np.array(rg_y.Max))
        else:
            y_min_interp = np.interp(arc_length, y_arc, np.array(rg_y.Mean))
            y_max_interp = y_min_interp.copy()
    else:
        if has_dynamic:
            y_min_interp = np.array(rg_y.Min)
            y_max_interp = np.array(rg_y.Max)
        else:
            y_min_interp = np.array(rg_y.Mean)
            y_max_interp = y_min_interp.copy()

    # For static state, Min/Max contain invalid placeholder values - use Mean instead
    if has_dynamic:
        max_bend_stress_mpa = max(rg_bend_stress.Max) / 1000
        max_tens_stress_mpa = max(rg_tens_stress.Max) / 1000
    else:
        max_bend_stress_mpa = max(rg_bend_stress.Mean) / 1000
        max_tens_stress_mpa = max(rg_tens_stress.Mean) / 1000

    bend_util = max_bend_stress_mpa / X65_YIELD_MPa
    combined_util = (max_bend_stress_mpa + max_tens_stress_mpa) / X65_YIELD_MPa

    env_case, heading = parse_filename(sim_file.stem)

    # Save arc-length distributed CSV
    if has_dynamic:
        tension_min = np.array(rg_tension.Min) / 1000
        tension_max = np.array(rg_tension.Max) / 1000
        strain_min = np.array(rg_strain.Min) * 100
        strain_max = np.array(rg_strain.Max) * 100
        bend_min = np.array(rg_bend_stress.Min) / 1000
        bend_max = np.array(rg_bend_stress.Max) / 1000
        tens_min = np.array(rg_tens_stress.Min) / 1000
        tens_max = np.array(rg_tens_stress.Max) / 1000
    else:
        tension_min = np.array(rg_tension.Mean) / 1000
        tension_max = tension_min.copy()
        strain_min = np.array(rg_strain.Mean) * 100
        strain_max = strain_min.copy()
        bend_min = np.array(rg_bend_stress.Mean) / 1000
        bend_max = bend_min.copy()
        tens_min = np.array(rg_tens_stress.Mean) / 1000
        tens_max = tens_min.copy()

    rg_df = pd.DataFrame({
        'ArcLength_m': arc_length,
        'Tension_Min_kN': tension_min,
        'Tension_Max_kN': tension_max,
        'Tension_Mean_kN': np.array(rg_tension.Mean) / 1000,
        'Y_Min_m': y_min_interp,
        'Y_Max_m': y_max_interp,
        'Strain_Min_pct': strain_min,
        'Strain_Max_pct': strain_max,
        'BendStress_Min_MPa': bend_min,
        'BendStress_Max_MPa': bend_max,
        'TensStress_Min_MPa': tens_min,
        'TensStress_Max_MPa': tens_max,
    })

    # Calculate combined utilization along arc length
    rg_df['CombinedUtil'] = (rg_df['BendStress_Max_MPa'] + rg_df['TensStress_Max_MPa']) / X65_YIELD_MPa

    rg_csv_path = rangegraph_dir / f'{sim_file.stem}_rangegraph.csv'
    rg_df.to_csv(rg_csv_path, index=False)

    # Store data for chart generation
    rangegraph_data.append({
        'filename': sim_file.stem,
        'env_case': env_case,
        'heading': heading,
        'df': rg_df,
        'has_dynamic': has_dynamic,
    })

    # Calculate max strain
    if has_dynamic:
        max_strain_pct = max(rg_strain.Max) * 100
    else:
        max_strain_pct = max(rg_strain.Mean) * 100

    results.append({
        'EnvCase': env_case,
        'Heading': heading,
        'EndA_Max_kN': round(end_a_max, 1),
        'EndA_Min_kN': round(end_a_min, 1),
        'GlobalY_Max_m': round(max(y_max_interp), 2),
        'MaxStrain_pct': round(max_strain_pct, 3),
        'MaxBendStress_MPa': round(max_bend_stress_mpa, 1),
        'BendUtil': round(bend_util, 3),
        'CombinedUtil': round(combined_util, 3),
        'Status': 'FAIL' if combined_util > UTIL_LIMIT else 'OK',
        'HasDynamic': has_dynamic,
    })

print(f'Saved {len(rangegraph_data)} rangegraph CSVs to: {rangegraph_dir}')

df = pd.DataFrame(results)

# Chart styles by env_case and heading
styles = {
    ('A', '0deg'): {'color': '#1f77b4', 'dash': 'solid', 'symbol': 'circle', 'name': 'Case A - 0 deg'},
    ('A', '50deg'): {'color': '#1f77b4', 'dash': 'dash', 'symbol': 'diamond', 'name': 'Case A - 50 deg'},
    ('C', '0deg'): {'color': '#2ca02c', 'dash': 'solid', 'symbol': 'circle', 'name': 'Case C - 0 deg'},
    ('C', '50deg'): {'color': '#2ca02c', 'dash': 'dash', 'symbol': 'diamond', 'name': 'Case C - 50 deg'},
    ('D', '0deg'): {'color': '#d62728', 'dash': 'solid', 'symbol': 'circle', 'name': 'Case D - 0 deg'},
    ('D', '50deg'): {'color': '#d62728', 'dash': 'dash', 'symbol': 'diamond', 'name': 'Case D - 50 deg'},
}


def create_bar_chart(df, y_col, title, y_label, hline=None, hline_text=None, hline_color='red'):
    """Create grouped bar chart comparing cases and headings."""
    fig = go.Figure()

    headings = sorted(df['Heading'].unique())
    cases = sorted(df['EnvCase'].unique())

    heading_colors = {'0deg': '#4472C4', '50deg': '#ED7D31'}

    for heading in headings:
        subset = df[df['Heading'] == heading].sort_values('EnvCase')
        fig.add_trace(go.Bar(
            x=[f'Case {c}' for c in subset['EnvCase']],
            y=subset[y_col],
            name=f'{heading.replace("deg", " deg")}',
            marker_color=heading_colors.get(heading, '#888888'),
        ))

    if hline:
        fig.add_hline(y=hline, line_dash='dot', line_color=hline_color,
                      annotation_text=hline_text, annotation_position='right')

    fig.update_layout(
        title=dict(text=title, font=dict(size=14)),
        xaxis_title='Environment Case',
        yaxis_title=y_label,
        barmode='group',
        legend=dict(orientation='h', yanchor='top', y=-0.15, xanchor='center', x=0.5),
        height=400,
        margin=dict(l=60, r=40, t=50, b=80),
    )
    return fig


def create_comparison_chart(df, y_col, title, y_label, hline=None, hline_text=None, hline_color='red'):
    """Create scatter chart comparing all cases."""
    fig = go.Figure()

    for (env_case, heading), style in styles.items():
        subset = df[(df['EnvCase'] == env_case) & (df['Heading'] == heading)]
        if len(subset) == 0:
            continue
        fig.add_trace(go.Scatter(
            x=[f'Case {env_case}'],
            y=subset[y_col],
            mode='markers',
            name=style['name'],
            marker=dict(
                color=style['color'],
                size=15,
                symbol=style['symbol'],
            ),
        ))

    if hline:
        fig.add_hline(y=hline, line_dash='dot', line_color=hline_color,
                      annotation_text=hline_text, annotation_position='right')

    fig.update_layout(
        title=dict(text=title, font=dict(size=14)),
        xaxis_title='Environment Case',
        yaxis_title=y_label,
        legend=dict(orientation='h', yanchor='top', y=-0.15, xanchor='center', x=0.5),
        height=400,
        margin=dict(l=60, r=40, t=50, b=80),
    )
    return fig


# Create charts
fig1 = create_bar_chart(df, 'EndA_Max_kN', 'End A Max Tension by Case', 'Tension (kN)')
fig2 = create_bar_chart(df, 'CombinedUtil', 'Combined Utilization by Case', 'Utilization', UTIL_LIMIT, f'Limit ({UTIL_LIMIT})')
fig3 = create_bar_chart(df, 'MaxBendStress_MPa', 'Max Bending Stress by Case', 'Stress (MPa)', X65_YIELD_MPa * UTIL_LIMIT, f'Limit ({UTIL_LIMIT} x Yield)')
fig4 = create_bar_chart(df, 'MaxStrain_pct', 'Max Strain by Case', 'Strain (%)')
fig5 = create_bar_chart(df, 'GlobalY_Max_m', 'Global Y Max by Case', 'Y (m)')
fig6 = create_bar_chart(df, 'BendUtil', 'Bending Utilization by Case', 'Utilization', UTIL_LIMIT, f'Limit ({UTIL_LIMIT})')

# Create summary statistics
summary_data = []
for heading in sorted(df['Heading'].unique()):
    heading_df = df[df['Heading'] == heading]
    max_util = heading_df['CombinedUtil'].max()
    worst_case = heading_df.loc[heading_df['CombinedUtil'].idxmax()]
    summary_data.append({
        'Heading': heading.replace('deg', ' deg'),
        'Max Utilization': round(max_util, 3),
        'Status': 'FAIL' if max_util > UTIL_LIMIT else 'OK',
        'Worst Case': f"Case {worst_case['EnvCase']}"
    })

for env_case in sorted(df['EnvCase'].unique()):
    case_df = df[df['EnvCase'] == env_case]
    max_util = case_df['CombinedUtil'].max()
    worst_heading = case_df.loc[case_df['CombinedUtil'].idxmax()]
    summary_data.append({
        'Heading': f'Case {env_case}',
        'Max Utilization': round(max_util, 3),
        'Status': 'FAIL' if max_util > UTIL_LIMIT else 'OK',
        'Worst Case': worst_heading['Heading'].replace('deg', ' deg')
    })

summary_df = pd.DataFrame(summary_data)

# Full results table
table_html = df.to_html(index=False, classes='results-table', border=0)
table_html = table_html.replace('>FAIL<', ' class="fail">FAIL<').replace('>OK<', ' class="ok">OK<')

summary_table_html = summary_df.to_html(index=False, classes='results-table', border=0)
summary_table_html = summary_table_html.replace('>FAIL<', ' class="fail">FAIL<').replace('>OK<', ' class="ok">OK<')

# Build HTML
html_template = """<!DOCTYPE html>
<html>
<head>
    <title>900kN Env Case Analysis - Pipeline Installation</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }}
        h1 {{ text-align: center; color: #333; }}
        h2 {{ color: #333; border-bottom: 2px solid #333; padding-bottom: 5px; }}
        h3 {{ color: #555; margin-top: 20px; }}
        .subtitle {{ text-align: center; color: #666; margin-bottom: 20px; }}
        .section {{ max-width: 1400px; margin: 0 auto 30px auto; }}
        .chart-container {{ display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }}
        .chart {{ background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }}
        .results-table {{ width: 100%; border-collapse: collapse; margin: 10px 0; font-size: 12px; }}
        .results-table th, .results-table td {{ border: 1px solid #ddd; padding: 6px; text-align: center; }}
        .results-table th {{ background: #4472C4; color: white; }}
        .results-table tr:nth-child(even) {{ background: #f9f9f9; }}
        .info-box {{ background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; }}
        .legend-note {{ text-align: center; color: #666; font-size: 12px; margin: 10px 0; }}
        .fail {{ color: red; font-weight: bold; }}
        .ok {{ color: green; font-weight: bold; }}
        .analysis-note {{ background: #e8f4e8; border-left: 4px solid #28a745; padding: 10px 15px; margin: 10px 0; }}
        .util-limit-note {{ background: #f8d7da; border-left: 4px solid #dc3545; padding: 10px 15px; margin: 10px 0; font-weight: bold; }}
        .toc {{ background: white; padding: 15px; border-radius: 8px; margin-bottom: 20px; }}
        .toc ul {{ list-style: none; padding-left: 0; }}
        .toc li {{ margin: 5px 0; }}
        .toc a {{ color: #4472C4; text-decoration: none; }}
        .toc a:hover {{ text-decoration: underline; }}
        .case-legend {{ display: flex; justify-content: center; gap: 30px; margin: 15px 0; flex-wrap: wrap; }}
        .case-legend-item {{ display: flex; align-items: center; gap: 8px; }}
        .case-legend-color {{ width: 20px; height: 20px; border-radius: 4px; }}
    </style>
</head>
<body>
    <h1>900kN Environment Case Analysis</h1>
    <p class="subtitle">Pipeline Installation | X65 Steel (Yield = 448.2 MPa) | Utilization Limit = {util_limit}</p>

    <div class="section">
        <div class="toc">
            <strong>Contents</strong>
            <ul>
                <li><a href="#summary">1. Analysis Summary</a></li>
                <li><a href="#comparison">2. Case Comparison Charts</a></li>
                <li><a href="#arclength">3. Arc-Length Distributions</a></li>
                <li><a href="#full-results">4. Full Results Table</a></li>
            </ul>
        </div>

        <div class="util-limit-note">
            Design Utilization Limit: {util_limit} (shown as red dotted line in all charts)
        </div>

        <div class="case-legend">
            <div class="case-legend-item">
                <div class="case-legend-color" style="background: #4472C4;"></div>
                <span>0 deg heading</span>
            </div>
            <div class="case-legend-item">
                <div class="case-legend-color" style="background: #ED7D31;"></div>
                <span>50 deg heading</span>
            </div>
        </div>
    </div>

    <div class="section" id="summary">
        <h2>1. Analysis Summary</h2>
        <div class="analysis-note">
            This report compares 900 kN lay tension results across environment cases (A, C, D)
            and heading directions (0 deg, 50 deg). A total of {num_cases} simulation cases were analyzed.
        </div>

        <h3>1.1 Summary by Parameter</h3>
        <div class="info-box">
            {summary_table_html}
        </div>
    </div>

    <div class="section" id="comparison">
        <h2>2. Case Comparison Charts</h2>
        <p class="legend-note">Click legend items to toggle visibility. Double-click to isolate a trace.</p>
        <div class="chart-container">
            <div class="chart" id="chart1"></div>
            <div class="chart" id="chart2"></div>
            <div class="chart" id="chart3"></div>
            <div class="chart" id="chart4"></div>
            <div class="chart" id="chart5"></div>
            <div class="chart" id="chart6"></div>
        </div>
    </div>

    <div class="section" id="arclength">
        <h2>3. Arc-Length Distributions</h2>
        <p class="legend-note">
            <strong>By Heading:</strong>
            <a href="900kN_env_case_arclength_0deg.html">0 deg</a> |
            <a href="900kN_env_case_arclength_50deg.html">50 deg</a>
            <br>
            <strong>By Case:</strong>
            <a href="900kN_env_case_arclength_A.html">Case A</a> |
            <a href="900kN_env_case_arclength_C.html">Case C</a> |
            <a href="900kN_env_case_arclength_D.html">Case D</a>
        </p>
    </div>

    <div class="section" id="full-results">
        <h2>4. Full Results Table</h2>
        <div class="info-box">
            {table_html}
        </div>
    </div>

    <script>
        var chart1 = {chart1_json};
        var chart2 = {chart2_json};
        var chart3 = {chart3_json};
        var chart4 = {chart4_json};
        var chart5 = {chart5_json};
        var chart6 = {chart6_json};

        Plotly.newPlot('chart1', chart1.data, chart1.layout);
        Plotly.newPlot('chart2', chart2.data, chart2.layout);
        Plotly.newPlot('chart3', chart3.data, chart3.layout);
        Plotly.newPlot('chart4', chart4.data, chart4.layout);
        Plotly.newPlot('chart5', chart5.data, chart5.layout);
        Plotly.newPlot('chart6', chart6.data, chart6.layout);
    </script>
</body>
</html>
"""

html_content = html_template.format(
    util_limit=UTIL_LIMIT,
    num_cases=len(df),
    summary_table_html=summary_table_html,
    table_html=table_html,
    chart1_json=fig1.to_json(),
    chart2_json=fig2.to_json(),
    chart3_json=fig3.to_json(),
    chart4_json=fig4.to_json(),
    chart5_json=fig5.to_json(),
    chart6_json=fig6.to_json(),
)

output_html = Path(__file__).parent / '900kN_env_case_report.html'
with open(output_html, 'w', encoding='utf-8') as f:
    f.write(html_content)
print(f'Interactive HTML saved to: {output_html}')

# Save CSV
output_csv = Path(__file__).parent / '900kN_env_case_results.csv'
df.to_csv(output_csv, index=False)
print(f'CSV saved to: {output_csv}')

# ============================================================================
# ARC-LENGTH DISTRIBUTION CHARTS
# ============================================================================

# Color palettes
case_colors = {
    'A': '#1f77b4',  # blue
    'C': '#2ca02c',  # green
    'D': '#d62728',  # red
}

heading_colors = {
    '0deg': '#4472C4',   # blue
    '50deg': '#ED7D31',  # orange
}


def create_rangegraph_chart(data_list, y_col, title, y_label, xlim=None, hline=None, hline_text=None, color_by='case', hline_color='red'):
    """Create arc-length distribution chart."""
    fig = go.Figure()

    for item in data_list:
        rg_df = item['df']
        if color_by == 'case':
            color = case_colors.get(item['env_case'], '#888888')
            name = f"Case {item['env_case']}"
        else:
            color = heading_colors.get(item['heading'], '#888888')
            name = f"{item['heading'].replace('deg', ' deg')}"

        x = rg_df['ArcLength_m']

        # Check if this is a Min/Max envelope variable
        if y_col.endswith('_Max') and y_col.replace('_Max', '_Min') in rg_df.columns:
            y_min_col = y_col.replace('_Max', '_Min')
            # Add Max line
            fig.add_trace(go.Scatter(
                x=x, y=rg_df[y_col],
                mode='lines', name=f'{name} Max',
                line=dict(color=color, width=1.5),
                legendgroup=name,
            ))
            # Add Min line with dash
            fig.add_trace(go.Scatter(
                x=x, y=rg_df[y_min_col],
                mode='lines', name=f'{name} Min',
                line=dict(color=color, width=1, dash='dash'),
                legendgroup=name,
                showlegend=False,
            ))
        else:
            # Single line
            fig.add_trace(go.Scatter(
                x=x, y=rg_df[y_col],
                mode='lines', name=name,
                line=dict(color=color, width=1.5),
            ))

    if hline is not None:
        fig.add_hline(y=hline, line_dash='dot', line_color=hline_color,
                      annotation_text=hline_text, annotation_position='top right')

    layout_kwargs = dict(
        title=dict(text=title, font=dict(size=14)),
        xaxis_title='Arc Length (m)',
        yaxis_title=y_label,
        legend=dict(orientation='h', yanchor='top', y=-0.12, xanchor='center', x=0.5),
        height=400,
        margin=dict(l=60, r=40, t=50, b=80),
    )

    if xlim:
        layout_kwargs['xaxis'] = dict(range=[0, xlim])

    fig.update_layout(**layout_kwargs)
    return fig


def create_dual_view_charts(data_list, color_by='case'):
    """Create all chart types with full length and critical zone views."""
    charts = {}

    chart_configs = [
        ('tension', 'Tension_Max_kN', 'Effective Tension', 'Tension (kN)', None, None),
        ('bend_stress', 'BendStress_Max_MPa', 'Max Bending Stress', 'Stress (MPa)', X65_YIELD_MPa * UTIL_LIMIT, f'Limit ({UTIL_LIMIT} x Yield)'),
        ('strain', 'Strain_Max_pct', 'Direct Tensile Strain', 'Strain (%)', None, None),
        ('y_pos', 'Y_Max_m', 'Global Y Position', 'Y (m)', None, None),
        ('util', 'CombinedUtil', 'Combined Utilization', 'Utilization', UTIL_LIMIT, f'Limit ({UTIL_LIMIT})'),
    ]

    for key, y_col, title, y_label, hline, hline_text in chart_configs:
        # Full length view
        charts[f'{key}_full'] = create_rangegraph_chart(
            data_list, y_col, f'{title} - Full Length', y_label,
            xlim=None, hline=hline, hline_text=hline_text, color_by=color_by
        )
        # Critical zone view (0-300m)
        charts[f'{key}_critical'] = create_rangegraph_chart(
            data_list, y_col, f'{title} - Critical Zone (0-300m)', y_label,
            xlim=300, hline=hline, hline_text=hline_text, color_by=color_by
        )

    return charts


def generate_chart_page(charts, title, subtitle, output_path):
    """Generate HTML page with all charts."""
    chart_pairs = [
        ('tension_full', 'tension_critical', 'Effective Tension'),
        ('bend_stress_full', 'bend_stress_critical', 'Max Bending Stress'),
        ('strain_full', 'strain_critical', 'Direct Tensile Strain'),
        ('y_pos_full', 'y_pos_critical', 'Global Y Position'),
        ('util_full', 'util_critical', 'Combined Utilization'),
    ]

    chart_divs = ""
    chart_scripts = ""
    chart_data_vars = ""
    for i, (full_key, crit_key, label) in enumerate(chart_pairs):
        chart_divs += f'''
        <div class="chart-row">
            <h3>{label}</h3>
            <div class="chart-pair">
                <div class="chart" id="chart{i*2+1}"></div>
                <div class="chart" id="chart{i*2+2}"></div>
            </div>
        </div>'''
        full_json = charts[full_key].to_json()
        crit_json = charts[crit_key].to_json()
        chart_data_vars += f'''
        var chart{i*2+1}_data = {full_json};
        var chart{i*2+2}_data = {crit_json};'''
        chart_scripts += f'''
        Plotly.newPlot('chart{i*2+1}', chart{i*2+1}_data.data, chart{i*2+1}_data.layout);
        Plotly.newPlot('chart{i*2+2}', chart{i*2+2}_data.data, chart{i*2+2}_data.layout);'''

    html = f'''<!DOCTYPE html>
<html>
<head>
    <title>{title}</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }}
        h1 {{ text-align: center; color: #333; }}
        h2 {{ color: #333; border-bottom: 2px solid #333; padding-bottom: 5px; }}
        h3 {{ color: #555; margin: 15px 0 10px 0; }}
        .subtitle {{ text-align: center; color: #666; margin-bottom: 20px; }}
        .section {{ max-width: 1600px; margin: 0 auto 30px auto; }}
        .chart-row {{ background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px; }}
        .chart-pair {{ display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }}
        .chart {{ min-height: 400px; }}
        .nav {{ text-align: center; margin: 20px 0; }}
        .nav a {{ margin: 0 10px; color: #4472C4; text-decoration: none; }}
        .nav a:hover {{ text-decoration: underline; }}
    </style>
</head>
<body>
    <h1>{title}</h1>
    <p class="subtitle">{subtitle}</p>

    <div class="nav">
        <a href="900kN_env_case_report.html">Summary Report</a> |
        <strong>By Heading:</strong>
        <a href="900kN_env_case_arclength_0deg.html">0 deg</a>
        <a href="900kN_env_case_arclength_50deg.html">50 deg</a> |
        <strong>By Case:</strong>
        <a href="900kN_env_case_arclength_A.html">Case A</a>
        <a href="900kN_env_case_arclength_C.html">Case C</a>
        <a href="900kN_env_case_arclength_D.html">Case D</a>
    </div>

    <div class="section">
        {chart_divs}
    </div>

    <script>
        {chart_data_vars}
        {chart_scripts}
    </script>
</body>
</html>'''

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(html)


# Generate charts grouped by heading
print('Generating arc-length distribution charts...')
headings = sorted(set(d['heading'] for d in rangegraph_data))

for heading in headings:
    filtered = [d for d in rangegraph_data if d['heading'] == heading]
    if not filtered:
        continue

    charts = create_dual_view_charts(filtered, color_by='case')
    title = f'Arc-Length Distributions - {heading.replace("deg", " deg")} Heading'
    subtitle = f'Comparing all environment cases | 900 kN Lay Tension'
    output_path = Path(__file__).parent / f'900kN_env_case_arclength_{heading}.html'
    generate_chart_page(charts, title, subtitle, output_path)
    print(f'  Saved: {output_path.name}')

# Generate charts grouped by env_case
env_cases = sorted(set(d['env_case'] for d in rangegraph_data))

for env_case in env_cases:
    filtered = [d for d in rangegraph_data if d['env_case'] == env_case]
    if not filtered:
        continue

    charts = create_dual_view_charts(filtered, color_by='heading')
    title = f'Arc-Length Distributions - Case {env_case}'
    subtitle = f'Comparing all headings | 900 kN Lay Tension'
    output_path = Path(__file__).parent / f'900kN_env_case_arclength_{env_case}.html'
    generate_chart_page(charts, title, subtitle, output_path)
    print(f'  Saved: {output_path.name}')

print('Post-processing complete!')
